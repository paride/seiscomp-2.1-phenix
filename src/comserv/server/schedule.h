/*****************************************************************************
 * schedule.h
 *
 * (c) 2000 Andres Heinloo, GFZ Potsdam
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *
 * This code is derived from vixie cron sources, original copyright notice
 * follows.
 *
 *   Copyright 1988,1990,1993,1994 by Paul Vixie
 *   All rights reserved
 *
 *   Distribute freely, except: don't remove my name from the source or
 *   documentation (don't take credit for my work), mark your changes (don't
 *   get me blamed for your possible bugs), don't alter or remove this
 *   notice.  May be sold if buildable source is provided to buyer.  No
 *   warrantee of any kind, express or implied, is included with this
 *   software; use at your own risk, responsibility for damages (if any) to
 *   anyone resulting from the use of this software rests entirely with the
 *   user.
 *****************************************************************************/

#include "bitstring.h"

#define MINUTE_ERR    (-1)
#define HOUR_ERR      (-2)
#define DOM_ERR       (-3)
#define MONTH_ERR     (-4)
#define DOW_ERR       (-5)
#define EOI_ERR       (-6)

#define FIRST_MINUTE  0
#define LAST_MINUTE   59
#define MINUTE_COUNT  (LAST_MINUTE - FIRST_MINUTE + 1)

#define FIRST_HOUR    0
#define LAST_HOUR     23
#define HOUR_COUNT    (LAST_HOUR - FIRST_HOUR + 1)

#define FIRST_DOM     1
#define LAST_DOM      31
#define DOM_COUNT     (LAST_DOM - FIRST_DOM + 1)

#define FIRST_MONTH   1
#define LAST_MONTH    12
#define MONTH_COUNT   (LAST_MONTH - FIRST_MONTH + 1)

/* note on DOW: 0 and 7 are both Sunday, for compatibility reasons. */
#define FIRST_DOW     0
#define LAST_DOW      7
#define DOW_COUNT     (LAST_DOW - FIRST_DOW + 1)

typedef struct _schedule {
    bitstr_t bit_decl(minute, MINUTE_COUNT);
    bitstr_t bit_decl(hour,   HOUR_COUNT);
    bitstr_t bit_decl(dom,    DOM_COUNT);
    bitstr_t bit_decl(month,  MONTH_COUNT);
    bitstr_t bit_decl(dow,    DOW_COUNT);
    int flags;
} schedule;

int init_schedule(schedule *sch, const char *minute_hour_dom_month_dow);
int check_schedule(const schedule *sch, time_t sec);

