#pragma ident "$Id: rate.h,v 1.1.1.1 2005/07/26 19:28:53 andres Exp $"
/* --------------------------------------------------------------------
 Program  : Any
 Task     : Archive Library API functions.
 File     : rate.h
 Purpose  : Sampling derivation stuff.
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

#ifndef _RATE_H_INCLUDED_
#define _RATE_H_INCLUDED_

/* Includes -----------------------------------------------------------*/
#include "archive.h"

/*---------------------------------------------------------------------*/
typedef struct _STASH_PACKET {
    RF_HEADER hdr;
    RF_PACKET rfp;
} STASH_PACKET;

typedef struct _STASH_LIST {
    UINT16 unit;
    UINT8 stream;
    UINT16 n_packets;
    LIST packets;
} STASH_LIST;

/*---------------------------------------------------------------------*/
/* Table of all valid sampling rates on 72A series DAS's */

#define N_RATES 16
UINT16 valid_rate[N_RATES] = {
    1000,
    500,
    250,
    200,
    125,
    100,
    50,
    40,
    25,
    20,
    10,
    8,
    5,
    4,
    2,
    1
};

#endif

/* Revision History
 *
 * $Log: rate.h,v $
 * Revision 1.1.1.1  2005/07/26 19:28:53  andres
 * Imported sources
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
