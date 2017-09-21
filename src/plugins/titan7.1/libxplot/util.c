/*====================================================================
 	util.c			 13 May 1994
 *===================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <X11/Xlib.h>
#include "xplot.h"
#include "proto.h"

extern char   *char_1[], *char_2[], *char_3[], *char_4[];
extern int     logxy, logx, logy, prev_logx, prev_logy;


/*===============================================================*/
void filter(xy, nxy, fh, fl)
XY *xy;
int nxy;
float fh, fl;
{
int   i, ny, npadd, nt;
static float *ydata = NULL;
float dt, sv[10];
double mean;

    ny = nxy;
    npadd = 2000;
    nt = ny+npadd;
    if (!(ydata = (float *)malloc(nt*sizeof(float))))
    {
        fprintf(stderr, "xplot: filter: malloc failed for %d data\n",nt);
        exit(1);
    }
/*
printf("filter: allocating %d data\n", nt);
*/
    mean = 0.0;
    for (i=0;i<ny;i++) mean += xy[i].y;
    mean /= ny;
/*
printf("mean %.1f\n", mean);
*/
    for (i=0;i<npadd;i++) ydata[i] = mean;
    for (i=0;i<ny;i++) ydata[i+npadd] = xy[i].y;
    dt = 1.0;
    ciir( fh, fl, dt, ydata, nt, sv ); 
    for (i=0;i<ny;i++) xy[i].y = ydata[i+npadd];
    free(ydata);
}

/********************************************************************* 
 *    Routine - ciir                                                 *
 *                                                                   * 
 *    Programmer - Ken Muirhead, Shane Ingate                        *
 *                                                                   * 
 *    Date Written - 1987                                            * 
 *      Update to C, Shane Ingate, Aug 88                            *
 *                                                                   * 
 *      Function -                                                   * 
 *        Filter the seismic trace with a recursive filter           * 
 *                                                                   * 
 *     Inputs -                                                      * 
 *          fl is low frequency cutoff (Hz)                          *
 *          fu is high frequency cutoff  (Hz)                        *
 *          dt is time increment  (sec)                              *
 *          xbuf is the input array.  Filtered data returned in xbuf *
 *          npts is the number of points                             *
 *          sv are the recursion coefficients                        *
 *                                                                   *
 *          No filtering if fl .ge. fu                               *
 *          Bandpass filtering if fl .lt. fu                         *
 *          Low pass filter if fl .eq. 0 and fu is non-zero          *
 *          High pass filter if fu .eq. 0 and fl is non zero         *
 *                                                                   * 
 *                                                                   * 
 *********************************************************************/

void ciir( fl, fu, dt, xbuf, npts, sv )
float fl,fu,dt,*xbuf,*sv;
int npts;
{
float wtl,wtal,bl,gl,zl,xa,xb,xc;
float wth,wtah,bh,gh,zh,temp;
int i;

    if ( fl == fu ) return;
    if ( fu == 0. ) goto HIPASS;


/*     calculate low pass filter coefficients and perform low pass filter */
/*
printf("ciir: lowpass\n");
*/
    for (i=0;i<6;i++) sv[i] = 0.0;
    wtl = 6.2831852 * dt * fu;
    wtal = 0.5 * wtl;
    zl = exp ( -wtl );
    bl = 2.0 * exp ( -wtal ) * cos ( 1.732 * wtal );
    gl = ( zl - 1.0 ) * ( bl - zl - 1.0 );
    
    for (i = 0; i < npts; i++) {
           xa = zl * sv[0] + xbuf[i];
           xb = sv[2];
           xc = sv[0] - ( zl * sv[1] ) + ( bl * sv[2] );
           xbuf[i] = gl * sv[2];
           sv[0] = xa;
           sv[1] = xb;
           sv[2] = xc;
   }


/*  calculate high pass filter coefficients and perform high pass filter */

HIPASS:
    if ( fl == 0. ) return;
/*
printf("ciir: hipass\n");
*/
    for (i=0;i<6;i++) sv[i] = 0.0;
    wth = 6.2831852 * dt * fl;
    wtah = 0.5 * wth;
    zh = exp ( -wth );
    bh = 2.0 * exp ( -wtah ) * cos ( 1.732 * wtah );
    gh = ( 1.0 + zh ) * ( 1.0 + bh + zh ) / 8.0;

    for (i = 0; i < npts; i++) {
           temp = xbuf[i];
           xc =  bh * sv[5] - sv[3] - zh * sv[4] + temp;
           xbuf[i] = gh * ( xc + sv[4] - sv[5] - sv[5] );
           sv[3] = zh * ( sv[3] - temp ) + temp;
           sv[4] = sv[5];
           sv[5] = xc;
   }
}


/*===============================================================
    Minimum-maximum of x and y in an array of xy data, ignoring
    pen-up data.
 *=============================================================*/
void min_max_xy(xy, nxy, xmin, xmax, ymin, ymax, ymean)
XY *xy;
int nxy;
float *xmin, *xmax, *ymin, *ymax, *ymean;
{
int   i;
float x, y;
    *xmax = *ymax = -BIG;
    *xmin = *ymin =  BIG;
    *ymean = 0.0;
    for (i=0;i<nxy;i++)
    {
        x = xy[i].x; y = xy[i].y;
/*
if (!(nxy%1000))    printf("%.0f\n", y);
*/
        if (x < *xmin)            *xmin = x;
        if (x < BIG && x > *xmax) *xmax = x;
        if (y < *ymin)            *ymin = y;
        if (y < BIG && y > *ymax) *ymax = y;
        if (y < BIG) *ymean += y;
    }
    *ymean /= (float) nxy;
if (0)    printf("min_max_xy ymean=%.3f\n", *ymean);
}



/*===============================================================
    Converts an array into log base 10 of its values. Checks if
    the calculation is possible and if not, do not performs the
    conversion and return the proper value of logxy flag.
logxy flag:  1 convert x values
             2 convert y values
             3 convert both x and y values
 *=============================================================*/
void logxy_data(xy, nxy)
XY     *xy;
int    nxy;
{
int i;

if (0) printf("logxy: inp: lx %d prevx %d    ly %d prevy %d  data=%E\n",
                       logx, prev_logx,    logy, prev_logy, xy[0].x);
again:
    if (logx == prev_logx && logy == prev_logy)
    {
if (0) printf("logxy: out: lx %d prevx %d    ly %d prevy %d  data=%E\n",
                       logx, prev_logx,    logy, prev_logy, xy[0].x);
        return;
    }

    if (logx != prev_logx)
    {
        prev_logx = logx;
        if (logx == 1)
        {
            for (i=0;i<nxy;i++)
            {
                if (xy[i].x <= 0.0)
                {
                  printf("WARNING: null or negative value; ");
                  printf("x log scale not provided; ");
                  printf("logx %d --> ", logx);
                  prev_logx = logx = 0;
                  printf("%d\n", logx);
                  goto again;
                }
            }
            for (i=0;i<nxy;i++)
                xy[i].x = (float)log10(xy[i].x);
        }
        else if (logx == 0)
        {
            for (i=0;i<nxy;i++)
                xy[i].x = (float)pow(10.0,xy[i].x);
        }
        else
        {
            fprintf(stderr,"ERROR logxy: logx not 0/1\n");
            exit(1);
        }
    }
    if (logy != prev_logy)
    {
        prev_logy = logy;
        if (logy == 1)
        {
            for (i=0;i<nxy;i++)
            {
                if (xy[i].y <= 0.0)
                {
                  printf("WARNING: null or negative value; ");
                  printf("y log scale not provided; ");
                  printf("logy %d --> ", logy);
                  prev_logy = logy = 0;
                  printf( "%d\n", logy);
                  goto again;
                }
            }
            for (i=0;i<nxy;i++)
                xy[i].y = (float)log10(xy[i].y);
        }
        else if (logy == 0)
        {
            for (i=0;i<nxy;i++)
                xy[i].y = (float)pow(10.0,xy[i].y);
        }
        else
        {
            fprintf(stderr,"ERROR logxy: logy not 0/1\n");
            exit(1);
        }
    } 
}


/*===================================================================
  Sorts xy array according to x by the combsort, a souped up version of
  bubble sort (Lacey & Box, Byte 16, p315, 1991).
  Almost as fast as quicksort.
*===================================================================*/
void sortxy(xy, ndt)
XY *xy;
int ndt;
{
int i, j, isw, ngap;
float temp;

    ngap = ndt;

    while (1)
    {
        ngap = max((int)(ngap/1.3), 1);
        if (ngap == 9 || ngap == 10) ngap = 11;
        isw = 0;

        for (i=0; i<(ndt-ngap); ++i)
        {
            j = i + ngap;
            if (xy[i].x <= xy[j].x) continue;
            temp = xy[i].x;
            xy[i].x = xy[j].x;
            xy[j].x = temp;
            temp = xy[i].y;
            xy[i].y = xy[j].y;
            xy[j].y = temp;
            isw = 1;
        }
        if (isw == 1 || ngap > 1) continue;
        break;
    }
    return;
}


/*===============================================================
 * Look-up code returning a pointer to special character xy data
 *=============================================================*/
void lookup_char(c, pt)
char c;
char **pt;
{
int    i;
    i = -1;
    switch (c) {
      case '.': i =  0; break;
      case '-': i =  1; break;
      case '+': i =  2; break;
      case '=': i =  3; break;
      case ':': i =  4; break;
      case '_': i =  5; break;
      case '(': i =  6; break;
      case ')': i =  7; break;
      case ',': i =  8; break;
      case '[': i =  9; break;
      case ']': i = 10; break;
      case '/': i = 11; break;
      case '%': i = 12; break;
      case '&': i = 13; break;
      case ';': i = 14; break;
      case '<': i = 15; break;
      case '>': i = 16; break;
      case '@': i = 17; break;
      case '^': i = 18; break;
      case '|': i = 19; break;

      case '!': i = 20; break;
      case '"': i = 21; break;
      case '#': i = 22; break;
      case '$': i = 23; break;
      case '\'': i = 24; break;
      case '*': i = 25; break;
      case '?': i = 26; break;
      case '~': i = 27; break;
    }
    if (i < 0) *pt = NULL;
    else       *pt = char_4[i];
}

/*===============================================================
 * Look-up code return a pointer to greek letter xy data
 *=============================================================*/
void lookup_greek(s, idx, pt)
char *s;
int  *idx;
char **pt;
{
int    i, j;
char   greek[20];

    i = *idx;
    i++;
    j = 0;
    while (s[i] && s[i] != '\\') {
        if (j>=10) break;
        greek[j++] = s[i++]; 
    }
    greek[j] = 0;
    *idx = i;   

    i   = -1; 
    if      (!strncmp(greek, "al", 2)) i =  0;
    else if (!strncmp(greek, "be", 2)) i =  1;
    else if (!strncmp(greek, "ga", 2)) i =  2;
    else if (!strncmp(greek, "de", 2)) i =  3;
    else if (!strncmp(greek, "ep", 2)) i =  4;
    else if (!strncmp(greek, "ze", 2)) i =  5;
    else if (!strncmp(greek, "et", 2)) i =  6;
    else if (!strncmp(greek, "th", 2)) i =  7;
    else if (!strncmp(greek, "io", 2)) i =  8;
    else if (!strncmp(greek, "ka", 2)) i =  9;
    else if (!strncmp(greek, "la", 2)) i = 10;
    else if (!strncmp(greek, "mu", 2)) i = 11;
    else if (!strncmp(greek, "nu", 2)) i = 12;
    else if (!strncmp(greek, "xi", 2)) i = 13;
    else if (!strncmp(greek, "om", 2)) i = 14;
    else if (!strncmp(greek, "pi", 2)) i = 15;
    else if (!strncmp(greek, "rh", 2)) i = 16;
    else if (!strncmp(greek, "si", 2)) i = 17;
    else if (!strncmp(greek, "ta", 2)) i = 18;
    else if (!strncmp(greek, "ip", 2)) i = 19;
    else if (!strncmp(greek, "ph", 2)) i = 20;
    else if (!strncmp(greek, "ch", 2)) i = 21;
    else if (!strncmp(greek, "ps", 2)) i = 22;
    else if (!strncmp(greek, "om", 2)) i = 23;
    if (i < 0) *pt = NULL;
    else       *pt = char_3[i];
}


/*================================================================
    Routine for scaling intervals and providing tick marks for axes.
    Inputs:
xin[0],xin[1]  extremes of variable x in its own units.
xtin  suggested interval for ticks; may be over-ruled if there
      are too many or too few ticks on the interval.
    Outputs:
xout[0],xout[1]  adjusted extremes, made to be round numbers.
       The new interval always covers old one.
xtick  distance between tick marks in x units. If computed by nicer,
       this number is always a round number.
Calls no other routines
 *==============================================================*/
void nicer(xin, xtin, xout, xtick)
float *xin, xtin, *xout, *xtick;
{
static float     bias, plus;
static int       index;
static float     units;
static int       npanel;
float            r1;
static float divs[4]  = {0.1, 0.2, 0.5, 1.0};

    xout[0] = xin[0];
    xout[1] = xin[1];
    if (xout[1] == xout[0]) xout[1] = xout[1] + fabs(xout[1]) + 1.0;
    plus = 1000.0 + log10(xout[1] - xout[0]);
    index = 1.4969 + 2.857 * mod(plus, 1.0);
    units = divs[index - 1] * pow(10.0, (double)((int)plus - 1000));
    npanel = (xin[1] - xin[0]) / max(xtin, (0.001*units));
    if (2 <= npanel && npanel <= 41) units = xtin;
    r1 = max(fabs(xout[0]), fabs(xout[1])) / units + 1.0;
    bias = (1.0 + 1.e-7) * units * (double)((int)(r1));
    xout[0] -= mod((xout[0] + bias), units);
    xout[1] -= mod((xout[1] - bias), units);
    if (fabs(xout[0] / units) <= .01) xout[0] = 0.0;
    if (fabs(xout[1] / units) <= .01) xout[1] = 0.0;
    *xtick = units;
}
