
/*
 * dispatch.c:
 *
 * Copyright (c) 2003 Guralp Systems Limited
 * Author James McKenzie, contact <software@guralp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

static char rcsid[] = "$Id: dispatch.c,v 1.1.1.1 2005/07/26 19:28:53 andres Exp $";

/*
 * $Log: dispatch.c,v $
 * Revision 1.1.1.1  2005/07/26 19:28:53  andres
 * Imported sources
 *
 * Revision 1.1  2003/03/27 18:07:18  alex
 * Initial revision
 *
 * Revision 1.7  2003/03/17 11:21:20  root
 * #
 *
 * Revision 1.6  2003/02/28 17:08:27  root
 * #
 *
 * Revision 1.5  2003/02/28 17:05:37  root
 * #
 *
 */

/* Mar 2004  - Modified by Reinoud Sleeman (ORFEUS/KNMI)  */
/*             for the SCREAM plugin in SeedLink          */


#include "project.h"
#include "plugin.h"
#include "map.h"
#include <math.h>

#define TRACE_STA_LEN   7
#define TRACE_CHAN_LEN  9
#define TRACE_NET_LEN   9
#define TRACE_LOC_LEN   3

typedef struct {
        int     pinno;          	/* Pin number */
        int     nsamp;          	/* Number of samples in packet */
        double  starttime;      	/* time of first sample in epoch seconds
                               		    (seconds since midnight 1/1/1970) */
        double  endtime;       	 	/* Time of last sample in epoch seconds */
        double  samprate;       	/* Sample rate; nominal */
        char    sta[TRACE_STA_LEN];     /* Site name */
        char    net[TRACE_NET_LEN];     /* Network name */
        char    chan[TRACE_CHAN_LEN];   /* Component/channel code */
        char    datatype[3];    	/* Data format code */
        char    quality[2];     	/* Data-quality field */
        char    pad[2];         	/* padding */
} TRACE_HEADER;


struct tm *gmt;

extern Map *rootmap;


#define MAXCHAN 1000

void
dispatch (gcf_block b, int recno)
{
  uint8_t *buf;
  TRACE_HEADER *t;
  int len;
  int chid;
  long tm_starttime;
  static double  prevend[MAXCHAN];
  char sta[6], chan[4];
  int usec, tqual;
  struct ptime p_ptime;
  double lsec;
  Map *mp;

/* We only work with UTC time */

  putenv ("TZ=UTC");

  if(gmt==NULL) gmt=(struct tm *) malloc(sizeof(struct tm));


/* We can't handle status information*/

  if (!b->sample_rate) {
    printf("No sample rate info ?\n");
    return;
  }

  buf = malloc (sizeof (TRACE_HEADER) + (len = ((b->samples) * sizeof (int))));

  if (!buf) fatal (("malloc failed"));

  t = (TRACE_HEADER *) buf;

  t->endtime = t->starttime = (double) b->estart;
  t->endtime += ((double) (b->samples - 1)) / ((double) b->sample_rate);
  t->nsamp = b->samples;
  if ( t->nsamp == 0 ) {
       printf("No samples received\n");
       return;
  }
  t->samprate = (double) b->sample_rate;
  strcpy (t->datatype, SCREAM2EW_DATATYPE);

  memcpy (t + 1, b->data, len);

  /* Send it off to the controlling SeedLink server */
  //sprintf ( sta, "----");
  //sprintf ( chan, "---");

   tm_starttime = (long) t->starttime;
   gmt = (struct tm *) gmtime ( &tm_starttime );
   p_ptime.year = gmt->tm_year+1900;
   p_ptime.yday = gmt->tm_yday+1;
   p_ptime.hour = gmt->tm_hour;
   p_ptime.minute = gmt->tm_min;
   p_ptime.second = gmt->tm_sec;
   lsec = (double) floor(t->starttime);
   p_ptime.usec = (int)((1000)*(t->starttime-lsec));

  for ( mp = rootmap; mp != NULL; mp=mp->next ) {

        if (strncmp (b->strid, mp->stream, strlen(mp->stream)) == 0 ) {
             printf("send [%s]-[%s] data using send_raw3\n", mp->station, mp->channel);
             send_raw3 ( mp->station, mp->channel, &p_ptime, usec, tqual, b->data, b->samples);
             //send_flush3 ( mp->station, mp->channel);
             chid=mp->id;
        }
  }

   printf ("%d: Got %d bytes for %s  %s  start: %04d/%02d/%02d %02d:%02d:%02d  %lf  end: %lf\n",
            recno,
	    len+sizeof(TRACE_HEADER), 
            b->sysid, 
            b->strid,  
            gmt->tm_year+1900, gmt->tm_mon+1, gmt->tm_mday,
            gmt->tm_hour, gmt->tm_min, gmt->tm_sec, 
            t->starttime, t->endtime);


   if ( (t->starttime - prevend[chid]) > (1.0/t->samprate) ) 
        printf("GAP detected of %lf seconds for %s\n", t->starttime - prevend[chid], b->sysid);

   prevend[chid] = t->endtime;

  free (buf);
}
