#pragma ident "$Id: ad.c,v 1.1.1.1 2005/07/26 19:28:53 andres Exp $"
/*======================================================================
 *
 *  Decode an AD packet
 *
 *====================================================================*/
#include "private.h"

BOOL reftek_ad(struct reftek_ad *dest, UINT8 *src)
{
    reftek_com(src, &dest->exp, &dest->unit, &dest->seqno, &dest->tstamp);
    return TRUE;
}

/* Revision History
 *
 * $Log: ad.c,v $
 * Revision 1.1.1.1  2005/07/26 19:28:53  andres
 * Imported sources
 *
 * Revision 1.2  2002/01/18 17:55:55  nobody
 * replaced WORD, BYTE, LONG, etc macros with size specific equivalents
 * changed interpretation of unit ID from BCD to binary
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
