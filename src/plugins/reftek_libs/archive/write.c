#pragma ident "$Id: write.c,v 1.1.1.1 2005/07/26 19:28:53 andres Exp $"
/* --------------------------------------------------------------------
 Program  : Any
 Task     : Archive Library API functions.
 File     : write.c
 Purpose  : Packet input functions.
 Host     : CC, GCC, Microsoft Visual C++ 5.x, MCC68K 3.1
 Target   : Solaris (Sparc and x86), Linux, DOS, Win32, and RTOS
 Author      : Robert Banfill (r.banfill@reftek.com)
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

-----------------------------------------------------------------------*/

#define _WRITE_C
#include "archive.h"

/* Local prototypes ---------------------------------------------------*/

/*---------------------------------------------------------------------*/
BOOL ArchiveWritePacket( H_ARCHIVE harchive, RF_PACKET * packet )
{
    CHAR string[32];
    RF_HEADER hdr;
    STREAM *stream;

    ASSERT( packet != NULL );

    _archive[harchive].last_error = ARC_NO_ERROR;
    _archive_error = ARC_NO_ERROR;

    if ( !ValidateHandle( harchive ) ) {
        ArchiveLog( ARC_LOG_ERRORS, "%s", "ValidateHandle failed");
        return ( FALSE );
    }

    if ( _archive[harchive].access == ARC_CLOSED ) {
        return ( TRUE );
    }

    if ( _archive[harchive].access != ARC_WRITE ) {
        _archive[harchive].last_error = ARC_PERMISSION_DENIED;
        return ( FALSE );
    }

    /* Purge if needed */
    if( _archive[harchive].state.bytes > _archive[harchive].state.thres_bytes ) {
        if( ! PurgeOldestEvent( harchive ) ) {
            ArchiveLog( ARC_LOG_ERRORS, "%s", "PurgeOldestEvent failed");
            return( FALSE );
        }
    }

    /* Max size? */
    if( _archive[harchive].state.bytes > _archive[harchive].state.max_bytes ) {
        ArchiveLog( ARC_LOG_ERRORS, "%s", "Archive: reached maximum allowed size!");
        return( FALSE );
    }

    /* Decode the recording format packet header */
    RFDecodeHeader( packet, &hdr );

    /* Validate the the header information */
    if ( hdr.type == NO_PACKET || hdr.unit > RF_MAX_UNIT ) {
        ArchiveLog( ARC_LOG_ERRORS, "%s", "Archive: unrecognized packet" );
        _archive[harchive].last_error = ARC_BAD_PACKET;
        return ( TRUE );
    }

    /* If it's not an EH, DT, or ET, set stream to 0 (SOH) */
    if( ! IsDataPacket( hdr.type ) )
        hdr.stream = 0;

    MUTEX_LOCK(&_archive[harchive].mutex);

    /* Find or create the stream record */
    if ( ( stream = LookupStream( harchive, hdr.unit, hdr.stream ) ) == NULL ) {
        if ( ( stream = CreateStream( harchive, hdr.unit, hdr.stream ) ) == NULL ) {
            ArchiveLog( ARC_LOG_ERRORS, "%s", "Archive: CreateStream failed" );
            MUTEX_UNLOCK(&_archive[harchive].mutex);
            return ( FALSE );
        }
    }

    /* Record that this channel was seen */
    if ( hdr.type == DT )
        stream->chn_mask |= 0x00000001 << ( hdr.channel - 1 );

    /* If this is a EH, DT, or ET, and sampling rate for this stream is undefined... */
    if ( IsDataPacket( hdr.type ) && stream->rate == VOID_RATE ) {
        /* Stash packet(s) and derive the rate for this stream */
        if ( !DeriveRate( harchive, stream, &hdr, packet ) ) {
            ArchiveLog( ARC_LOG_ERRORS, "%s", "Archive: DeriveRate failed" );
            MUTEX_UNLOCK(&_archive[harchive].mutex);
            return ( FALSE );
        }
        /* Packets will be written once the rate is derived */
        MUTEX_UNLOCK(&_archive[harchive].mutex);
        return ( TRUE );
    }
    /* Let any incoming EH's and ET's override the current rate */
    else if( hdr.type == EH || hdr.type == ET ) {
        stream->rate = DecodeRFRate( packet );
        ArchiveLog( ARC_LOG_VERBOSE, "Archive: %04X:%u %u sps at %s", stream->unit, 
                    stream->stream, stream->rate, FormatMSTime( string, hdr.time, 10 ) );
    }

    /* Write the packet */
    if( !WritePacket( harchive, stream, &hdr, packet ) ) {
        ArchiveLog( ARC_LOG_ERRORS, "%s", "Archive: WritePacket failed" );
        MUTEX_UNLOCK(&_archive[harchive].mutex);
        return ( FALSE );
    }
    
    if( !ArchiveSync( harchive ) ) {
        ArchiveLog( ARC_LOG_ERRORS, "%s", "Archive: ArchiveSync failed" );
        MUTEX_UNLOCK(&_archive[harchive].mutex);
        return ( FALSE );
    }

    MUTEX_UNLOCK(&_archive[harchive].mutex);
    return ( TRUE );
}

/*---------------------------------------------------------------------*/
BOOL WritePacket( H_ARCHIVE harchive, STREAM * stream, RF_HEADER * hdr, RF_PACKET * packet )
{
    CHAR string[2][32];
    UINT32 written;
    TIME_RANGE range;

    ASSERT( stream != NULL );
    ASSERT( hdr != NULL );
    ASSERT( packet != NULL );
	/*ASSERT( stream->rate > 0.0 );*/

    /* Compute packet time range */
	range.earliest = hdr->time;
    if( hdr->type == DT ) 
        range.latest = range.earliest + ( (REAL64)hdr->length / (REAL64)stream->rate );
    else
        range.latest = range.earliest;

    ArchiveLog( ARC_LOG_MAXIMUM, "Archive: Write: %.2s %04X:%u:%u %04u %s %s",
                PacketCodes[hdr->type], hdr->unit, hdr->stream, hdr->channel,
				(stream->event.number < RF_MAX_EVENT_NO ? stream->event.number : 0),
                FormatMSTime( string[0], range.earliest, 10 ),
                (UndefinedTime( range.latest ) ? "Undefined" : FormatMSTime( string[1], range.latest, 10 )) );

	/* Validate event and sequence number only on open events... */
	if( stream->event.file != VOID_H_FILE ) {
		/* If this is SOH, check only for day break */
		if( hdr->stream == 0 ) {
			if( CompareYD( hdr->time, stream->event.time.earliest ) != 0 ) {
				ArchiveLog( ARC_LOG_VERBOSE, "Archive: day break in SOH data on %04X:%u", 
					stream->unit, stream->stream );
				/* If so, close the current file */
				if( ! CloseEventFileAndRename( harchive, stream ) ) {
					ArchiveLog( ARC_LOG_ERRORS, "%s", "Archive: CloseEventFileAndRename failed");
					return( FALSE );
				}
			}
		}
		/* Event number break? */
		else if( stream->event.number != hdr->evn_no ) {
			ArchiveLog( ARC_LOG_VERBOSE, "Archive: closing incomplete event %u on %04X:%u", 
				(stream->event.number < RF_MAX_EVENT_NO ? stream->event.number : 0), 
				stream->unit, stream->stream );
			/* If so, close the current file */
			if( ! CloseEventFileAndRename( harchive, stream ) ) {
				ArchiveLog( ARC_LOG_ERRORS, "%s", "Archive: CloseEventFileAndRename failed");
				return( FALSE );
			}
		}
		/* Sequence number break? */
		else if( stream->event.sequence != hdr->seq ) {
			ArchiveLog( ARC_LOG_VERBOSE, "Archive: sequence break in event %u on %04X:%u, closing file!", 
				(stream->event.number < RF_MAX_EVENT_NO ? stream->event.number : 0), 
				stream->unit, stream->stream );
			/* If so, close the current file */
			if( ! CloseEventFileAndRename( harchive, stream ) ) {
				ArchiveLog( ARC_LOG_ERRORS, "%s", "Archive: CloseEventFileAndRename failed");
				return( FALSE );
			}
		}
	}
	stream->event.number = hdr->evn_no;
	stream->event.sequence = ExpectedSeqNumber( hdr->seq );
	
    /* Update time ranges for event, stream, and archive */
	/* If the inbound data is the tail of an incomplete event, don't let the ET timestamp
	   make us think that we have more data than we really do! */
	if( hdr->type != ET ) {
		if ( !UpdateStreamTimeInfo( harchive, stream, &range ) ) {
			ArchiveLog( ARC_LOG_ERRORS, "%s", "Archive: UpdateStreamTimeInfo failed");
			return ( FALSE );
		}
	}

    /* Is a file is not open... */
    if ( stream->event.file == VOID_H_FILE ) {
        if( ! OpenEventFileForWrite( harchive, stream ) ) {
            ArchiveLog( ARC_LOG_ERRORS, "%s", "Archive: OpenEventFileForWrite failed");
            return( FALSE );
        }
    }

    /* Write the packet */
    written = RF_PACKET_SIZE;
    if ( !FileWrite( stream->event.file, packet, &written ) ) {
        _archive[harchive].last_error = ARC_FILE_IO_ERROR;
        ArchiveLog( ARC_LOG_ERRORS, "%s", "Archive: FileWrite failed");
        return ( FALSE );
    }

    /* Store disk usage */
    stream->bytes += written;
    _archive[harchive].state.bytes += written;

    /* Timers... */
    _archive[harchive].state.last_update = SystemMSTime(  );
    Timer48Start( &stream->event.last_io, FILE_IDLE_TIMEOUT );

    /* Is this an packet is an ET, end current event */
    if ( hdr->type == ET ) {
        ArchiveLog( ARC_LOG_VERBOSE, "Archive: end of event %u on %04X:%u", 
			(stream->event.number < RF_MAX_EVENT_NO ? stream->event.number : 0), 
			stream->unit, stream->stream );
        /* If so, close the current file */
        if( ! CloseEventFileAndRename( harchive, stream ) ) {
            ArchiveLog( ARC_LOG_ERRORS, "%s", "Archive: CloseEventFileAndRename failed");
            return( FALSE );
        }
    }

    /* Archive is dirty */
    _archive[harchive].dirty = TRUE;

    return ( TRUE );
}

/* Revision History
 *
 * $Log: write.c,v $
 * Revision 1.1.1.1  2005/07/26 19:28:53  andres
 * Imported sources
 *
 * Revision 1.3  2002/01/18 17:53:23  nobody
 * changed interpretation of unit ID from BCD to binary
 *
 * Revision 1.2  2001/07/23 18:48:26  nobody
 * Added purge thread
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
