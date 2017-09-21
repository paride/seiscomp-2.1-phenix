/*===================================================================
 Name:  libfilt.c
 *=================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "xplot.h"
#include "proto.h"


float  fh, fl;
extern XY *xy;
extern int nxy;


/*-------------------------------------------------------------------
 *                         do_filter
 *------------------------------------------------------------------*/
void do_filter(buf)
char   *buf;
{
XY    *fXY;
int   nfXY;
int j;
float f1,f2;

    if (strstr(buf, "fh=") || strstr(buf, "fl="))
    {

      fXY = NULL;
      nfXY = nxy;
      malloc_xy(&fXY, nfXY, "fXY");

/* fill-up filtered data array */
      for (j=0; j<nxy; j++)
          { fXY[j].x = xy[j].x; fXY[j].y = xy[j].y; }

      if (strstr(buf, "fh=")) { fh = atof(&buf[3]); f1=fh; f2=0.0; }
      else                    { fl = atof(&buf[3]); f1=0.0; f2=fl; }

printf("filter: high=%.4f (low=%.4f)  %d samples\n",fh,fl,nxy);

      filter(fXY, nfXY, f1, f2);

/* Replace current data by filtered data */

      free_xy(xy, "xy"); xy = NULL; nxy = 0;
      xy = fXY; fXY = NULL; nxy = nfXY;
      display1(xy, nxy, PS);
    }
}
