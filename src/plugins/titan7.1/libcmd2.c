/*===================================================================
 Name:	libcmd.c
 *=================================================================*/
#include <stdio.h>
#include <signal.h>
#include <math.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include "proto.h"
#include "libxplot/xplot.h"
#include "libxplot/proto.h"


#ifdef ANSI_C
extern void   smoothTop      (int, Top*, int, XY**, int*);
static double getSmoothedDt  (double, Top*, int);
#else
extern void   smoothTop      ();
static double getSmoothedDt  ();
#endif

/* Frame */
extern int     Wind_width, Wind_height;
extern float   Xf[2], Yf[2];
extern int     Xfofs, Yfofs;
extern double  Xfact, Yfact;
extern int     logxy, logx, logy, prev_logx, prev_logy;
extern int     stdin_wanted;
extern int     dballoc;

extern int do_pick;

extern XY   *xy;    extern int    nxy;     /* current data */
extern XY   *fitXY; extern int    nfitxy;  /* fitted data */
extern XY   *sXY;   extern int    nsxy;    /* smoothed data */
extern XY   *dXY;   extern int    ndxy;    /* smoothing residual */

extern BOX    Box;
extern char   *Notes;
extern int    BegYear;
extern double BegDay;
extern double BegTime;
extern Tcoef time_coef;


/*-------------------------------------------------------------------
 *                         pick
 *------------------------------------------------------------------*/
void pick(dpy, win, gc, pixmap, x, y, flag, add_point)
Display  *dpy;
Window    win;
GC        gc;
Pixmap    pixmap;
int       x;
int       y;
int       flag;
int       add_point;
{
static XY     *picXY;
static int    npicxy;
static XY     *nXY;
static int    ndxy;
static int cx, cy;
float  xdat, ydat;
int    j, jj;
static int first = 1;

    if (flag)
    {
/* Beg pick mode */
        if (do_pick == FALSE)
        {
            do_pick = TRUE;
            first = TRUE;
            npicxy = 0;
            malloc_xy(&picXY, 1000+10, "picXY");
            printf("pick is set%s: use any button to draw\n",
                ((add_point==TRUE) ?
                      " (adding new data)" : " (creating new data)"));
            return;
        }

/* End Pick mode */

        else
        {
            do_pick = FALSE;
            printf("pick unset; %d data points\n", npicxy);
            if (npicxy < 1)
            {
                free_xy(picXY, "picXY");
                picXY=NULL;
                npicxy=0;
                return;
            }

            if (add_point) 
            {
                ndxy = nxy + npicxy;
                malloc_xy(&nXY, ndxy, "nXY");
                for (j=0; j<nxy;    j++)
                    { nXY[j].x = xy[j].x; nXY[j].y = xy[j].y; }
                for (jj=0; j<ndxy; j++, jj++)
                    { nXY[j].x = picXY[jj].x; nXY[j].y = picXY[jj].y; }
            }
            else
            {
                ndxy = npicxy;
                malloc_xy(&nXY, ndxy, "nXY");
                for (j=0; j<ndxy; j++)
                    { nXY[j].x = picXY[j].x; nXY[j].y = picXY[j].y; }
            }
/*
for (j=0; j<ndxy; j++) printf("==== %E %E\n", nXY[j].x, nXY[j].y);
*/
            sortxy(nXY, ndxy);
/*
for (j=0; j<ndxy; j++) printf("== %E %E\n", nXY[j].x, nXY[j].y);
*/
            free_xy(picXY, "picXY"); picXY = NULL; npicxy = 0;

/* Replace current data by new data nXY: free xy and set xy = nXY */

            free_xy(xy, "xy"); xy = NULL; nxy = 0;
            xy = nXY; nXY = NULL; nxy = ndxy;

            display1(xy, nxy, PS);

        }
        return;
    }
    if (do_pick == FALSE)
    {
        return;
    }
    if (first)
    {
        cx = x; cy = y;
        npicxy = 0;
        first = FALSE;
    }

    pix2dat(x, y, &xdat, &ydat);
    printf("    %3d  x %d->%E  y %d->%f\n", npicxy, x, xdat, y, ydat);
    XDrawLine(dpy, pixmap, gc,  cx, cy, x, y);
    cx = x; cy = y;
    picXY[npicxy].x = xdat;
    picXY[npicxy].y = ydat;
    ++npicxy;

    XClearWindow(dpy,win);
    XFlush(dpy);
}


/*-------------------------------------------------------------------
 *                         smo
 *------------------------------------------------------------------*/
void smo(buf)
char *buf;
{
Top    *top = NULL;
Top    *stop = NULL;
int    i;
int    hper;
int    sper;
int    diff = FALSE;
double ddt;

    hper = 3;
    sper = 10800;
    if (strstr(buf, "S")) diff = TRUE;
    if (strstr(buf, "s=") || (strstr(buf, "S="))) sper = atoi(&buf[2]);

    if (!(top = (Top *) malloc(nxy*sizeof(Top))))
    {
        fprintf(stderr, "ERROR: smo: malloc failed (top)\n");
        exit(1);
    }

/* Load raw data in top[] */

    for (i=0; i<nxy; i++)
    {
        top[i].sec  =  dbday2dbtime(xy[i].x, BegYear);
        top[i].imin = ((int) top[i].sec / 60) * 60;
        top[i].dt   = xy[i].y;
    }

/* Smoothed data are in sXY */

    smoothTop(sper, top, nxy, &sXY, &nsxy);

    if (!sXY)
    {
        printf("== smoothTop failed\n");
        free(top);
        return;
    }

/* Smoothed data array: convert Unix time to days since beg of year */
    for (i=0; i<nsxy; i++)
        sXY[i].x = dbtime2dbday((double)sXY[i].x, BegYear);
    display1(xy, nxy, PS);

    if (diff == FALSE)
    {
        free(top);
        return;
    }

    if (!(stop = (Top *) malloc(nsxy*sizeof(Top))))
    {
        fprintf(stderr, "ERROR: smo: malloc failed (stop)\n");
        exit(1);
    }
    for (i=0; i<nsxy; i++)
    {
        stop[i].sec  = dbday2dbtime(sXY[i].x, BegYear);
        stop[i].imin = ((int) stop[i].sec / 60) * 60;
        stop[i].dt   = sXY[i].y;
    }
/* Load raw data again in top[] */
    for (i=0; i<nxy; i++)
    {
        top[i].sec  =  dbday2dbtime(xy[i].x, BegYear);
        top[i].imin = ((int) top[i].sec / 60) * 60;
        top[i].dt   = xy[i].y;
    }

/* Alloc diff array */
    if (dXY)  { free_xy(dXY, "dXY");  dXY=NULL;  ndxy=0; }
    ndxy = nxy;
    malloc_xy(&dXY, ndxy+2, "dXY");
    for (i=0; i<ndxy; i++)
    {
        ddt = getSmoothedDt(top[i].sec, stop, nsxy);
        dXY[i].x = xy[i].x;
        dXY[i].y = xy[i].y - ddt;
    }
    preset2();
    display2(dXY, ndxy, PS);
    free(stop);
    free(top);
}


/*-------------------------------------------------------------------
 *                         select_data
 *------------------------------------------------------------------*/
void select_data(mode)
int mode;
{
int j, jj;
int x1, x2;
int y1, y2;
XY  *txy = NULL;    /* temp xy data */
int ntxy = 0;

    jj = 0;
    if ((Box.n < 1) || (Box.xi[0] == Box.xi[1]))
    {
      printf("NO SELECT BOX AVAILABLE; USE FIRST BUTTON 2 TO SELECT BOX.\n");
      Box.n = Box.xi[0] = Box.xi[1] = Box.yi[0] = Box.yi[1] = 0;
      return;
    }
    if (mode != EXTRACT && mode != CLEAN)
    {
      printf("select_data: unsupported mode %d\n", mode);
      return;
    }

/* Allocate memory */
    malloc_xy(&txy, nxy, "txy");

/* fill-up selxy array according to selected mode */

    for (j=0;j<nxy;j++)
    {
        txy[j].x = xy[j].x;
        txy[j].y = xy[j].y;
    }

    if (Box.xi[0] < Box.xi[1]) x1 = Box.xi[0], x2 = Box.xi[1];
    else                       x2 = Box.xi[0], x1 = Box.xi[1];
    if (Box.yi[0] < Box.yi[1]) y2 = Box.yi[0], y1 = Box.yi[1];
    else                       y1 = Box.yi[0], y2 = Box.yi[1];

    if (Box.n && x1 != x2)
    {
      pix2dat(x1, y1, &Box.xd[0], &Box.yd[0]);
      pix2dat(x2, y2, &Box.xd[1], &Box.yd[1]);

if (1) printf("select_data: nxy=%d  x1=%.3f x2=%.3f   y1=%.3f y2=%.3f\n",
          Box.n, Box.xd[0], Box.xd[1], Box.yd[0], Box.yd[1]);

      if (mode == EXTRACT) for (j=0,jj=0;j<nxy;j++)
      {
            if ( (xy[j].y >= Box.yd[0] && xy[j].y < Box.yd[1]) &&
                 (xy[j].x >= Box.xd[0] && xy[j].x < Box.xd[1]) )
            {
                txy[jj].x = xy[j].x;
                txy[jj].y = xy[j].y;
                jj++;
            }
      }
      else if (mode == CLEAN) for (j=0,jj=0;j<nxy;j++)
      {
            if ( (xy[j].y >= Box.yd[0] && xy[j].y < Box.yd[1]) &&
                 (xy[j].x >= Box.xd[0] && xy[j].x < Box.xd[1]) )
                 continue;
            {
                txy[jj].x = xy[j].x;
                txy[jj].y = xy[j].y;
                jj++;
            }
      }
      ntxy = jj;

/* Replace current data by new data nXY: free xy and set xy = nXY */

      free_xy(xy, "xy"); xy = NULL; nxy = 0;
      xy = txy; txy = NULL; nxy = ntxy;

      printf("== select: %d data\n", nxy);
      Box.on = FALSE;
      Box.n = 0;
      Box.xi[0] = Box.xi[1] = Box.yi[0] = Box.yi[1] = 0;

      display1(xy, nxy, PS);
    }
}


/*-------------------------------------------------------------------
 *                        write_data 
 *------------------------------------------------------------------*/
void write_data(xy, nxy)
XY *xy;
int nxy;
{
FILE *Fp = NULL;
char date[40], fname[50];
char station[8];
char *p;
int  lt;
int  i, j;

    if (xy == NULL)
    {
        printf("write_data:  nothing to write (null pointer)\n");
        return;
    }
    if (nxy == 0)
    {
        printf("write_data:  nothing to write\n");
        return;
    }

/* Get station name from Notes */

    if ((p=strstr(Notes, "sta")))
    {
        p += 4; j = 0;
        while (*p != ' ' && j < 7) station[j++] = *p++;
        station[j] = '\0';
    }

/* Write selected data to disk; watch for existing files */

    lt = dbday2dbtime(xy[0].x, BegYear);
    if (!TIME_OK(lt))
    {
        printf("write_data: bad start time: %E %d\n", xy[0].x, lt);
        return;
    }

    while (1)
    {
        time_asc4(date, (double) lt);
        sprintf(fname, "%.19s.%s.dt", date, station);
        Fp=fopen(fname, "r");
        if (Fp == NULL) break;
        Fp = NULL;
        lt -= 1;
    }
    if (!(Fp=fopen(fname, "w")))
    {
        fprintf(stderr,"write_data: can't open '%s'\n", fname);
        exit(1);
    }

    printf("== write_data: %s n=%d\n", fname, nxy);

    fprintf(Fp,"%s  time reset (fake)\n", date);

    for (i=0; i<nxy; i++)
    {
        time_asc4(date, dbday2dbtime(xy[i].x, BegYear)); 
        fprintf(Fp,"%s %+.4f\n",date, xy[i].y);
    }
    fclose(Fp); Fp = NULL;

    if (!sXY || !nsxy) return;

/* Write smoothed data to disk; watch for existing files */

    lt = dbday2dbtime(sXY[0].x, BegYear);
    if (!TIME_OK(lt))
    {
        printf("write_data: bad start time: %E %d\n", sXY[0].x, lt);
        return;
    }

    while (1)
    {
        time_asc4(date, (double) lt);
        sprintf(fname, "%.19s.%s.dft", date, station);
        Fp=fopen(fname, "r");
        if (Fp == NULL) break;
        Fp = NULL;
        lt -= 1;
    }
    if (!(Fp=fopen(fname, "w")))
    {
        fprintf(stderr,"write_data: can't open '%s'\n", fname);
        exit(1);
    }

    printf("== write_data: %s n=%d\n", fname, nsxy);

    fprintf(Fp,"%s  time reset (fake)\n", date);

    for (i=0; i<nsxy; i++)
    {
        time_asc4(date, dbday2dbtime(sXY[i].x, BegYear)); 
        fprintf(Fp,"%s %+.4f\n",date, sXY[i].y);
    }
    fclose(Fp); Fp = NULL;

    return;
}


/*================================================================*/
static double getSmoothedDt(uTime, stop, nstop)
double uTime;
Top    *stop;
int    nstop;
{
Top     top1, top2;
int     ltime;
double  dt;
int     i;

    ltime = uTime;

/* On cherche les tops lisses entourant l'heure non corrigee */
    top1.imin = 0;
    top1.dt   = 0.0;

/* First entry */
    top2.imin = stop[0].imin;
    top2.dt   = stop[0].dt;

    i = 0;
    while (top2.imin < ltime)
    {
        top1.imin = top2.imin;
        top1.dt   = top2.dt;

        top2.imin = stop[i].imin;
        top2.dt   = stop[i].dt;
        ++i;
        if (i >= nstop) break;
    }

    if (ltime > top2.imin)
        dt = top2.dt;
    else 
        dt = (ltime-top1.imin)*(top2.dt-top1.dt)/(top2.imin-top1.imin)+top1.dt;

if (0) printf("utime=%d top1=%d %.3f  top2=%d %.3f dt=%.4f\n",
           ltime, top1.imin,top1.dt,top2.imin,top2.dt, dt);

    return (dt);
}


/*================================================================*/
double calc_delta_t(time, tcoef)
double time;
Tcoef tcoef;
{
double delta;
double day, day_expn;
int    i;

/*
    if (time < (tcoef.beg_systime - 100.0) ||
        time > (tcoef.end_systime + 100.0))
    return (double) 0x1777;
*/
    day = (time - tcoef.beg_systime) / (double) DAY;
    delta=0.0;
    day_expn=1.0;
    for (i=0; i<tcoef.ncoef; i++)
    {
        delta += tcoef.coef[i] * day_expn;
        day_expn *= day;
    }
/*
    printf("==== calc_delta_t: %.3f %.3f ncoef=%d --> dt=%.4f\n",
           time, tcoef.beg_systime, tcoef.ncoef, delta);
*/
    return delta;
}

