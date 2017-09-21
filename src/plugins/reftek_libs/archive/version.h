#pragma ident "$Id: version.h,v 1.1.1.1 2005/07/26 19:28:53 andres Exp $"
/* --------------------------------------------------------------------
 Program  : Any
 Task     : Archive Library API functions.
 File     : version.h
 Purpose  : da' version label.
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
    2000-03-22 1.13 RLB Fixed rate determination bug in archive API(read.c).
    2002-01-16 1.15 DEC changed interpretation of unit ID from BCD to binary

-------------------------------------------------------------------- */

#ifndef _VERSION_LABEL_
#define _VERSION_LABEL_

#define ARC_VERSION "RefTek Archive API 1.15"
#define ARC_COPYRIGHT "Copyright (c) 1998-2002, Refraction Technology, Inc. - All Rights Reserved."

#endif

/* Revision History
 *
 * $Log: version.h,v $
 * Revision 1.1.1.1  2005/07/26 19:28:53  andres
 * Imported sources
 *
 * Revision 1.3  2002/01/18 17:53:22  nobody
 * changed interpretation of unit ID from BCD to binary
 *
 * Revision 1.2  2001/07/23 18:48:26  nobody
 * Added purge thread
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
