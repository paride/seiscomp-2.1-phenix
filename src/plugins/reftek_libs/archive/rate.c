#pragma ident "$Id: rate.c,v 1.1.1.1 2005/07/26 19:28:53 andres Exp $"
/* --------------------------------------------------------------------
 Program  : Any
 Task     : Archive Library API functions.
 File     : rate.c
 Purpose  : Sampling rate derivation routines.
 Host     : CC, GCC, Microsoft Visual C++ 5.x
 Target   : Solaris (Sparc and x86), Linux, Win32
 Author   : Robert Banfill (r.banfill@reftek.com)
 Company  : Refraction Technology, Inc.
            2626 Lombardy Lane, Suite 105
            Dallas, Texas  75220  USA
            (214) 353-0609 Voice, (214) 353-9659 Fax, info@reftek.com
 Copyright: (c) 1997 Refraction Technology, Inc. - All Rights Reserved.
 Notes    :
 $Revision: 1.1.1.1 $
 $Logfile : R:/cpu68000/rt422/struct/version.h_v  $
 Revised  :
  17 Aug 1998  ---- (RLB) First effort.

-------------------------------------------------------------------- */

#define _RATE_C
#include "archive.h"

#include "rate.h"

/* Constants ----------------------------------------------------------*/
#define MAX_STASH  16

/* Module globals -----------------------------------------------------*/
static BOOL stash_created = FALSE;
static LIST stash;

/* Local prototypes ---------------------------------------------------*/
NODE *LookupPacketList(STREAM * stream);
BOOL StashPacket(STASH_LIST * list, RF_HEADER * hdr, RF_PACKET * rfp);
UINT16 AnalyzePacketList(STASH_LIST * list);
BOOL WritePacketList(H_ARCHIVE harchive, STASH_LIST * list, STREAM * stream);

/*---------------------------------------------------------------------*/
BOOL DeriveRate(H_ARCHIVE harchive, STREAM * stream, RF_HEADER * hdr, RF_PACKET * rfp)
{
    CHAR string[32];
    STASH_LIST *list;
    NODE *node;

    ASSERT(stream != NULL);
    ASSERT(hdr != NULL);
    ASSERT(rfp != NULL);

    if (!ValidateHandle(harchive))
        return (FALSE);

    _archive[harchive].last_error = ARC_NO_ERROR;
    _archive_error = ARC_NO_ERROR;

    if (!stash_created) {
        stash_created = TRUE;
        CreateList(&stash);
    }

    /* Lookup the packet list for this stream, create if not found */
    if ((node = LookupPacketList(stream)) == NULL) {
        _archive[harchive].last_error = ARC_NO_MEMORY;
        return (FALSE);
    }
    list = (STASH_LIST *) node->data;

    /* Stash this packet */
    if (!StashPacket(list, hdr, rfp))
        return (FALSE);

    /* Rates from EH and ET's override everything else */
    if (hdr->type == EH || hdr->type == ET)
        stream->rate = DecodeRFRate(rfp);
    else
        stream->rate = AnalyzePacketList(list);

    /* If we've managed to derive the sampling rate... */
    if (stream->rate != VOID_RATE) {
        ArchiveLog(ARC_LOG_VERBOSE, "Sampling rate on %04X:%u = %usps at %s", stream->unit,
            stream->stream, stream->rate, FormatMSTime(string, hdr->time, 10));

        /* Write out the stashed packets */
        if (!WritePacketList(harchive, list, stream))
            return (FALSE);

        /* Cleanup */
        DestroyNode(node);
    }

    return (TRUE);
}

/*---------------------------------------------------------------------*/
BOOL WritePacketList(H_ARCHIVE harchive, STASH_LIST * list, STREAM * stream)
{
    STASH_PACKET *packet;
    NODE *node;

    ASSERT(list != NULL);
    ASSERT(stream != NULL);
    ASSERT(stream->rate != VOID_RATE);

    /* Get the first packet in the list */
    if ((node = FirstNode(&list->packets)) != NULL) {
        do {
            packet = (STASH_PACKET *) node->data;

            if (!WritePacket(harchive, stream, &packet->hdr, &packet->rfp))
                return (FALSE);

        } while ((node = DestroyNode(node)) != NULL);
    }

    return (TRUE);
}

/*---------------------------------------------------------------------*/
UINT16 AnalyzePacketList(STASH_LIST * list)
{
    INT16 channel;
    UINT16 n_samples, rate;
    REAL64 time;
    NODE *node;
    STASH_PACKET *packet;

    ASSERT(list != NULL);

    /* Walk the packet list trying to divine the sampling rate */

try_again:
    /* Get the first packet in the list */
    if ((node = FirstNode(&list->packets)) == NULL)
        return (VOID_RATE);

    rate = VOID_RATE;
    channel = VOID_UINT8;
    do {
        packet = (STASH_PACKET *) node->data;

        /* Have we stashed too many packets? */
        if (list->n_packets > MAX_STASH) {
            ArchiveLog(ARC_LOG_ERRORS, "Too many packets in stash list while deriving rate for %04X:%u",
                packet->hdr.unit, packet->hdr.stream);
            /* Toss first packet and try again */
            if ((node = FirstNode(&list->packets)) == NULL)
                return (VOID_RATE);
            DestroyNode(node);
            list->n_packets--;
            goto try_again;
        }

        /* Ignore anything other than DT's */
        if (packet->hdr.type == DT) {

            /* Save info from first packet */
            if (channel == VOID_UINT8 && packet->hdr.length > 0) {
                time = packet->hdr.time;
                channel = packet->hdr.channel;
                n_samples = packet->hdr.length;
            }
            /* Next packet from same channel */
            else if (channel == packet->hdr.channel) {
                /* Compute length of packet */
                time = packet->hdr.time - time;

                /* Guard against /0 */
                if (time > 0.0) {
                    /* Compute rate */
                    rate = (UINT16) (((REAL64) n_samples / time) + 0.5);
                    ArchiveLog(ARC_LOG_VERBOSE, "Derived sampling rate for %04X:%u, %u sps",
                        packet->hdr.unit, packet->hdr.stream, rate);
                }

                /* Is it a valid sampling rate? */
                if (!IsValidRate(rate)) {
                    /* No, start again */
                    ArchiveLog(ARC_LOG_ERRORS, "Derived invalid sampling rate for %04X:%u, %u sps",
                        packet->hdr.unit, packet->hdr.stream, rate);
                    /* Toss first packet and try again */
                    if ((node = FirstNode(&list->packets)) == NULL)
                        return (VOID_RATE);
                    DestroyNode(node);
                    list->n_packets--;
                    goto try_again;
                }

                /* We're done */
                return (rate);
            }
        }
    } while ((node = NextNode(node)) != NULL);

    return (VOID_RATE);
}

/*---------------------------------------------------------------------*/
BOOL StashPacket(STASH_LIST * list, RF_HEADER * hdr, RF_PACKET * rfp)
{
    STASH_PACKET packet;

    ASSERT(list != NULL);
    ASSERT(hdr != NULL);
    ASSERT(rfp != NULL);

    /* Copy data into one local buffer */
    memcpy(&packet.hdr, hdr, sizeof(RF_HEADER));
    memcpy(&packet.rfp, rfp, sizeof(RF_PACKET));

    /* Insert at tail */
    if (InsertNodeBefore(&list->packets.tail, &packet, sizeof(STASH_PACKET)) == NULL) {
        _archive_error = ARC_NO_MEMORY;
        return (FALSE);
    }

    list->n_packets++;

    return (TRUE);
}

/*---------------------------------------------------------------------*/
VOID DestroyStash(VOID)
{
    STASH_LIST *list;
    NODE *node;

    /* Walk the stashed streams list destroying packet lists */
    if ((node = FirstNode(&stash)) != NULL) {
        do {
            list = (STASH_LIST *) node->data;
            ASSERT(list != NULL);
            DestroyList(&list->packets);
        } while ((node = NextNode(node)) != NULL);
    }

    /* Destroy stashed streams list */
    DestroyList(&stash);

    return;
}

/*---------------------------------------------------------------------*/
NODE *LookupPacketList(STREAM * stream)
{
    STASH_LIST *listptr, list;
    NODE *node;

    ASSERT(stream != NULL);

    /* See if we have a packet list for this stream list already */
    if ((node = FirstNode(&stash)) != NULL) {
        do {
            listptr = (STASH_LIST *) node->data;
            if (listptr->unit == stream->unit && listptr->stream == stream->stream)
                /* Found it, we're outta here */
                return (node);
        } while ((node = NextNode(node)));
    }

    /* Didn't find it, create a new packet list for this stream */

    ArchiveLog(ARC_LOG_VERBOSE, "Determining sampling rate of stream %04X:%u",
        stream->unit, stream->stream);

    list.unit = stream->unit;
    list.stream = stream->stream;
    list.n_packets = 0;

    /* Add it to the list of stashed streams */
    node = &stash.tail;
    if ((node = InsertNodeBefore(node, &list, sizeof(STASH_LIST))) == NULL) {
        _archive_error = ARC_NO_MEMORY;
        return (NULL);
    }

    /* Point to new list */
    listptr = (STASH_LIST *) node->data;

    /* Init this list after the copy */
    if (!CreateList(&listptr->packets)) {
        _archive_error = ARC_NO_MEMORY;
        return (NULL);
    }

    return (node);
}

/*---------------------------------------------------------------------*/
BOOL IsValidRate(UINT16 rate)
{
    UINT16 i;

    for (i = 0; i < N_RATES; i++) {
        if (rate == valid_rate[i])
            return (TRUE);
    }

    return (FALSE);
}

/*---------------------------------------------------------------------*/
UINT16 DecodeRFRate(RF_PACKET * packet)
{
    CHAR ascii[5];
    INT16 type;
    UINT16 rate;

    ASSERT(packet != NULL);

    type = RFPacketType(packet);
    if (type != EH && type != ET)
        return (VOID_RATE);

    strncpy(ascii, packet->pac.eh.rate, 4);
    ascii[4] = '\0';

    rate = atoi(ascii);

    if (!IsValidRate(rate))
        rate = VOID_RATE;

    return (rate);
}

/* Revision History
 *
 * $Log: rate.c,v $
 * Revision 1.1.1.1  2005/07/26 19:28:53  andres
 * Imported sources
 *
 * Revision 1.2  2002/01/18 17:53:22  nobody
 * changed interpretation of unit ID from BCD to binary
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
