/*===================================================================
 Name:	libcmd.c
 *=================================================================*/
#include <stdio.h>
#include <signal.h>
#include <math.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include "xplot.h"
#include "proto.h"


/* Frame */
extern int     Wind_width, Wind_height;
extern float   Xf[2], Yf[2];
extern int     Xfofs, Yfofs;
extern double  Xfact, Yfact;
extern int     logxy, logx, logy, prev_logx, prev_logy;
extern int     stdin_wanted;
extern int     dballoc;

extern XY   *xy;    extern int    nxy;     /* current data */
extern XY   *fitXY; extern int    nfitxy;  /* fitted data */
extern XY   *sXY;   extern int    nsxy;    /* smoothed data */
extern XY   *dXY;   extern int    ndxy;    /* smoothing residual */

extern unsigned int  W, H;
extern BOX    Box;
extern char   *Notes;
extern float  Xmargin, Ymargin;


/*-------------------------------------------------------------------
 *                        pix2dat 
 *------------------------------------------------------------------*/
void pix2dat(xpix, ypix, xdat, ydat)
int xpix;
int ypix;
float *xdat;
float *ydat;
{
    *xdat = (float)(xpix - Xfofs) / Xfact + Xf[0];
    *ydat = (float)(-ypix + Wind_height - Yfofs) / Yfact + Yf[0];
}

/*-------------------------------------------------------------------
 *                         show_zoom_lim
 *------------------------------------------------------------------*/
void show_zoom_lim(dpy, win, gc, pixmap, x, y)
Display  *dpy;
Window    win;
GC        gc;
Pixmap    pixmap;
int       x;
int       y;
{
int x0,y0,y1;

/* Check and reset box variables */

    if (Box.n > 1) { Box.n=Box.xi[0]=Box.xi[1]=Box.yi[0]=Box.yi[1]=0; }

    y0 = Yfofs;
    y1 = H * (1.0 - Ymargin);

/* First limit */

    if (Box.n == 0)
    {
        display1(xy, nxy, !PS);
        GetWinParms1(&dpy, &win, &gc, &pixmap);
        XDrawLine(dpy, pixmap, gc, x,y0,x,y1);
    }

    Box.xi[Box.n] = x;
    if (0) printf("== click %d  x=%d  y=%d\n",
               Box.n, Box.xi[Box.n], Box.yi[Box.n]);
    ++Box.n;

/* Display both limits */

    if (Box.xi[1])
    {
        display1(xy, nxy, !PS);
        GetWinParms1(&dpy, &win, &gc, &pixmap);
        x0 = Box.xi[0];
        XDrawLine(dpy, pixmap, gc, x0,y0,x0,y1);
        XDrawLine(dpy, pixmap, gc, x, y0,x, y1);
    }

    XClearWindow(dpy,win);
    XFlush(dpy);
}


/*-------------------------------------------------------------------
 *                         zoom2
 *------------------------------------------------------------------*/
void zoom2()
{
static int x0;
static double XF = 0.0;
static double X0 = 0.0;

    if (XF == 0.0) XF = Xfact;
    if (X0 == 0.0) X0 = Xf[0];
    if (x0 == 0 || Box.xi[1] > (W * (1.0 - Xmargin)))
    {
       x0 = Xfofs;
    }
    Box.xi[0] = x0;
    Box.xi[1] = x0 + 50;
    x0 += 50;

    Box.xd[0] = (float)(Box.xi[0] - Xfofs) / XF + X0;
    Box.xd[1] = (float)(Box.xi[1] - Xfofs) / XF + X0;

if (1) printf("== zoom2: x1=%d  xd1=%f\n", Box.xi[0],Box.xd[0]);
if (1) printf("== zoom2: x2=%d  xd2=%f\n", Box.xi[1],Box.xd[1]);

    Box.on = TRUE;
    display1(xy, nxy, PS);
    if (!dXY) return;
    else
    {
        preset2();
        display2(dXY, ndxy, PS);
    }
}


/*-------------------------------------------------------------------
 *                         do_zoom
 *------------------------------------------------------------------*/
void do_zoom()
{
    if (Box.n < 1)
    {
      printf("NO SELECT BOX AVAILABLE; USE FIRST BUTTON 2 TO SELECT BOX.\n");
      return;
    }
    Box.on = TRUE;

    pix2dat(Box.xi[0], Box.yi[0], &Box.xd[0], &Box.yd[0]);
    pix2dat(Box.xi[1], Box.yi[1], &Box.xd[1], &Box.yd[1]);

if (0) printf("== do_zoom: x1=%d  y1=%d  xd1=%f yd1=%f\n",
            Box.xi[0],Box.yi[0],Box.xd[0],Box.yd[0]);
if (0) printf("== do_zoom: x2=%d  y2=%d  xd2=%f yd2=%f\n",
            Box.xi[1],Box.yi[1],Box.xd[1],Box.yd[1]);

    display1(xy, nxy, PS);
    if (!dXY) return;
    else
    {
        preset2();
        display2(dXY, ndxy, PS);
    }
}

/*-------------------------------------------------------------------
 *                        get_zoom_data 
 *------------------------------------------------------------------*/
int get_zoom_data(xy, nxy, zxy, nzxy)
XY  *xy;
int nxy;
XY  **zxy;
int *nzxy;
{
int i1, i2, i, j;


if (0) printf("======== beg: *zxy %p nzxy %d\n", *zxy, *nzxy);
if (0) printf("== get_zoom_data: n=%d  x1=%.3f x2=%.3f  y1=%.3f y2=%.3f\n",
         Box.n, Box.xd[0], Box.xd[1], Box.yd[0], Box.yd[1]);
/* Find beg data index */
    for (j=0;j<nxy;j++) if (xy[j].x >= Box.xd[0]) break;
    i1 = j;
/* Find end data index */
    for (j=0;j<nxy;j++) if (xy[j].x >= Box.xd[1]) break;
    i2 = j;

/* check indexes */
    if (i1 < 0)   i1 = 0;
    if (i1 > nxy) i1 = nxy-1;
    if (i2 < 0)   i2 = 0;
    if (i2 > nxy) i2 = nxy-1;
    if (i1 > i2)  { i = i1; i1 = i2; i2 = i; }

    if (i1>=0 && i2 && (i2-i1) > 0)
    {
        *nzxy = i2 - i1;
        if (*zxy) { free_xy(*zxy, "get_zoom_data: zxy"); *zxy = NULL; }
        malloc_xy(zxy, (*nzxy)+2, "get_zoom_data: zxy");

        for (j=0;j<*nzxy;j++)
        {
            (*zxy)[j].x = xy[j+i1].x;
            (*zxy)[j].y = xy[j+i1].y;
        }

if (0) printf("======== end: *zxy %p nzxy %d\n", *zxy, *nzxy);
        return 1;
    }
/*
    *zxy = NULL; 
    *nzxy = 0;
*/
if (1) printf("======== end: get_zoom_data failed\n");
    return 0;
}

/*-------------------------------------------------------------------
 *                         edit
 *------------------------------------------------------------------*/
void edit(buf)
char *buf;
{
static XY  *nXY;
static int ndxy;
static XY  *pXY;
static int npxy;
int    j, jj;
char   line[255];
char   *token[10];
int    ntok;


    strcpy(line, &buf[1]);
    if (0) printf("%s\n", line);
    fflush(stdout);
    ntok = sparse(line, token, " ", 10);
    ntok = (ntok / 2) * 2;
    npxy = 0;
/* Check input */
    for (j=0; j<ntok; j+=2)
    {
        if (atof(token[j]) < 0.0 || atof(token[j]) > 800.0) break;
        if (atof(token[j+1]) > 100.0 || atof(token[j+1]) < -100.0) break;
        ++npxy;
        printf("%d %E %E\n", npxy, atof(token[j]), atof(token[j+1]));
        fflush(stdout);
    }

/* Load new data */
    malloc_xy(&pXY, npxy+2, "pXY");
    strcpy(line, &buf[1]);
    ntok = sparse(line, token, " ", 10);
    ntok = (ntok / 2) * 2;
    npxy = 0;
    for (j=0; j<ntok; j+=2)
    {
        if (atof(token[j]) < 0.0 || atof(token[j]) > 800.0) break;
        if (atof(token[j+1]) > 100.0 || atof(token[j+1]) < -100.0) break;
        pXY[npxy].x = atof(token[j]);
        pXY[npxy].y = atof(token[j+1]);
        ++npxy;
    }
    
    ndxy = nxy + npxy;
    malloc_xy(&nXY, ndxy, "nXY");
    for (j=0; j<nxy;    j++)
        { nXY[j].x = xy[j].x; nXY[j].y = xy[j].y; }
    for (jj=0; j<ndxy; j++, jj++)
        { nXY[j].x = pXY[jj].x; nXY[j].y = pXY[jj].y; }

    sortxy(nXY, ndxy);

    free_xy(pXY, "pXY"); pXY = NULL; npxy = 0;

/* Replace current data by new data nXY: free xy and set xy = nXY */

    free_xy(xy, "xy"); xy = NULL; nxy = 0;
    xy = nXY; nXY = NULL; nxy = ndxy;

    display1(xy, nxy, PS);
}



/*-------------------------------------------------------------------
 *                        malloc_xy
 *------------------------------------------------------------------*/
void malloc_xy(xy, nxy, name)
XY **xy;
int nxy;
char *name;
{
    if (*xy == NULL)
    {
      if (!(*xy = (XY *)malloc(nxy*sizeof(XY))))
      {
          fprintf(stderr,"ERROR: malloc_xy failed (%s) %d data\n",name, nxy);
          exit(1);
      }
      if (dballoc) printf("alloc_pointer %p %s %d data\n", *xy, name, nxy);
      return;
    }
    fprintf(stderr,"ERROR: malloc_xy: (%s) pointer not null\n", name);
    exit(1);
}

/*-------------------------------------------------------------------
 *                        free_xy
 *------------------------------------------------------------------*/
void free_xy(xy, name)
XY *xy;
char *name;
{
    if (xy == NULL)
    {
        fprintf(stderr,"ERROR: free_xy: %s null pointer\n", name);
        exit(1);
    }
    if (dballoc) printf("free_pointer %p %s\n", xy, name);
}

