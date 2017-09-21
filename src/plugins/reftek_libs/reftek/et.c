#pragma ident "$Id: et.c,v 1.1.1.1 2005/07/26 19:28:53 andres Exp $"
/*======================================================================
 *
 *  Decode a ET packet
 *
 *====================================================================*/
#include "private.h"

/* Offsets to the various pieces */

#define EVTN_OFF  16  /* event number           */
#define STRM_OFF  18  /* stream id              */
#define FRMT_OFF  23  /* data format descriptor */
#define SINT_OFF  88  /* sample rate            */
#define TTYP_OFF  92  /* trigger type           */
#define TRON_OFF  96  /* trigger time           */
#define TOFS_OFF 112  /* time of first sample   */
#define TOFF_OFF 128  /* detrigger time         */
#define TOLS_OFF 144  /* time of last sample    */

BOOL reftek_et(struct reftek_et *dest, UINT8 *src)
{
UINT16 srate, yr, da, hr, mn, sc, ms;

/* Load the common header */

    reftek_com(src, &dest->exp, &dest->unit, &dest->seqno, &dest->tstamp);

/* Load the record specific parts */

    dest->evtno  = (UINT16) utilBcdToUint32(src + EVTN_OFF, 4, 0); 
    dest->stream = (UINT16) utilBcdToUint32(src + STRM_OFF, 2, 0);

    switch (utilBcdToUint32(src + FRMT_OFF, 2, 0)) {
      case 16:
        dest->format = REFTEK_F16;
        break;

      case 32:
        dest->format = REFTEK_F32;
        break;

      case 120:
        dest->format = REFTEK_FC0;
        break;

      default:
        errno = EINVAL;
        return FALSE;
    }

    sscanf((char *) (src + SINT_OFF), "%hd", &srate);
    dest->sint = (REAL32) 1.0 / (REAL32) srate;

    if (memcmp(src + TTYP_OFF, "CON", 3) == 0) {
        dest->trgtype = REFTEK_TRGCON;
    } else if (memcmp(src + TTYP_OFF, "CRS", 3) == 0) {
        dest->trgtype = REFTEK_TRGCRS;
    } else if (memcmp(src + TTYP_OFF, "EVT", 3) == 0) {
        dest->trgtype = REFTEK_TRGEVT;
    } else if (memcmp(src + TTYP_OFF, "EXT", 3) == 0) {
        dest->trgtype = REFTEK_TRGEXT;
    } else if (memcmp(src + TTYP_OFF, "LVL", 3) == 0) {
        dest->trgtype = REFTEK_TRGLVL;
    } else if (memcmp(src + TTYP_OFF, "RAD", 3) == 0) {
        dest->trgtype = REFTEK_TRGRAD;
    } else if (memcmp(src + TTYP_OFF, "TIM", 3) == 0) {
        dest->trgtype = REFTEK_TRGTIM;
    } else {
        errno = EINVAL;
        return FALSE;
    }

    sscanf((char *) (src + TRON_OFF), "%4hd%3hd%2hd%2hd%2hd%3hd",
        &yr, &da, &hr, &mn, &sc, &ms
    );
    dest->on = util_ydhmsmtod(yr, da, hr, mn, sc, ms);

    sscanf((char *) (src + TOFS_OFF), "%4hd%3hd%2hd%2hd%2hd%3hd",
        &yr, &da, &hr, &mn, &sc, &ms
    );
    dest->tofs = util_ydhmsmtod(yr, da, hr, mn, sc, ms);

    sscanf((char *) (src + TOFF_OFF), "%4hd%3hd%2hd%2hd%2hd%3hd",
        &yr, &da, &hr, &mn, &sc, &ms
    );
    dest->off = util_ydhmsmtod(yr, da, hr, mn, sc, ms);

    sscanf((char *) (src + TOLS_OFF), "%4hd%3hd%2hd%2hd%2hd%3hd",
        &yr, &da, &hr, &mn, &sc, &ms
    );
    dest->tols = util_ydhmsmtod(yr, da, hr, mn, sc, ms);

    return TRUE;

}

/* Revision History
 *
 * $Log: et.c,v $
 * Revision 1.1.1.1  2005/07/26 19:28:53  andres
 * Imported sources
 *
 * Revision 1.3  2002/03/08 22:46:55  nobody
 * corrected offsets to detrigger time and last sample time
 *
 * Revision 1.2  2002/01/18 17:55:56  nobody
 * replaced WORD, BYTE, LONG, etc macros with size specific equivalents
 * changed interpretation of unit ID from BCD to binary
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
