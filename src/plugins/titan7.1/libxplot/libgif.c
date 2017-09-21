/*======================================================================
    libgif.c  GIF lib
 *=====================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <X11/Xlib.h>
#include "xplot.h"
#include "proto.h"

#define MAIN
#include "globals.h"

/*
struct internalpic
{
    int wide, high;
    int numcolors;
    int cmap [3][256];
    unsigned char *data;
};
*/


extern unsigned int  W, H;
int bw = 1;
XRectangle  xrect;
imageInfo   image;
char        gifname[255];

extern Display    *dpy;
extern int        scr;
extern Window     root;
extern Display    *hDisplay;
extern int        hScreen;
extern Window     hRoot;

/*-------------------------------------------------------------------
 *                         plot_GIF
 *------------------------------------------------------------------*/
void plot_GIF(dpy, win, gc)
Display      *dpy;
Window        win;
GC            gc;
{
static struct internalpic pic;
static XImage *ximage;
XWindowAttributes xwa;
int    dpy_depth;
FILE   *fpout;
long   pix;
int    i, j;
int    n, n0, n1;
unsigned char rr[256], gg[256], bb[256];
int    dbug = 0;

    hDisplay = dpy;
    if (!hDisplay) {
      fprintf(stderr, "plot_GIF, can't open X display\n");
      exit(1);
    }
    hScreen  = scr;
    hRoot    = root;
    hRoot    = win;

    if (!XGetWindowAttributes(hDisplay, hRoot, &xwa))
    {
    fprintf(stderr, " unable to get root window attributes\n");
      XCloseDisplay(hDisplay);
      exit(1);
    }
    image.depth    = xwa.depth;
    image.visual   = xwa.visual;
    image.colormap = xwa.colormap;

    if (dbug)
      fprintf(stderr, "plot_GIF: visual=%x depth=%d colormap=%x\n",
              (int) image.visual, (int) image.depth, (int) image.colormap);
/*
    TEST TEST
    getRectangle(&xrect);
*/
    xrect.x = 0;
    xrect.y = 0;
    xrect.width  = W;
    xrect.height = H;


    if (dbug)
      fprintf(stderr, "++++ plot_GIF: dpy %x window %x, box [%d,%d,%d,%d]\n",
              (int) dpy, (int) win, 0, 0, W, H);


    ximage = XGetImage(dpy,win,0,0,W,H,AllPlanes,ZPixmap);

    getImage(&xrect, &image, hRoot);
    ximage = image.ximage;


if (0)
{
    n1 = n0 = n = 0;
    for (i=0; i<W; i++)
    {
      for (j=0; j<H; j++)
      {
        pix = XGetPixel(ximage, i, j);
        if (pix == 0) n0++;
        else if (pix == 1) n1++;
        else n++;
        if (0 && pix != 1) printf("%d %d %ld\n", i, j, pix);
      }
    }
    printf("++++ n=%d n0=%d n1=%d\n", n,n0,n1);
}

if (0)
{
    /* draw a small filled square */
    for (i=10; i<50; i++)
      for (j=10; j<50; j++)
        XPutPixel(ximage, i, j, 15);

    XPutImage(dpy,win,gc,ximage,10,10,20,20,W,H);
}

    dpy_depth = DefaultDepth(dpy,DefaultScreen(dpy));
/*
    printf("++++ plot_GIF: dpy_depth=%d\n", dpy_depth);
    printf("++++ plot_GIF: blackpixel=%d whitepixel=%d\n",
            BlackPixel(dpy,DefaultScreen(dpy)),
            WhitePixel(dpy,DefaultScreen(dpy)));
*/

    if (dpy_depth == 16)
    {
      char *pc;
      short *ps;
      int   i;

        printf("  plot_GIF: Found 16-bit colors on this screen; ");
        printf("converting data_16 to data_8\n");
        pc = malloc(W*H);
        ps = (short*) ximage->data;
        for (i=0; i<W*H; i++)
        {
            if (ps[i] == 0) pc[i] = 0;   /* black pixel */
            else            pc[i] = 1;   /* white pixel */
        }
        pic.data = pc;
    }
    else
    {
        pic.data = ximage->data;
    }
    pic.wide = W;
    pic.high = H;


    if (!(fpout = fopen(gifname, "w")))
    {
          fprintf(stderr," can't open file %s\n",gifname);
          exit(1);
    }

    printf("== Plotting data to GIF file '%s'\n", gifname);

    pic.cmap[0][0] =   0; pic.cmap[1][0] =   0; pic.cmap[2][0] =   0;
    pic.cmap[0][1] = 255; pic.cmap[1][1] = 255; pic.cmap[2][1] = 255;


if (dbug)
{
int p;

    p=0;
    printf("---- out_map[%d] r=%d g=%d b=%d \n",
                p, pic.cmap[0][p], pic.cmap[1][p], pic.cmap[2][p]);
    p=1;
    printf("---- out_map[%d] r=%d g=%d b=%d \n",
                p, pic.cmap[0][p], pic.cmap[1][p], pic.cmap[2][p]);
}

if (0) printf("==== writing gif w=%d h=%d ncolors=%d\n",
               pic.wide, pic.high, pic.numcolors);

    rr[0] = gg[0] = bb[0] = 0;
    rr[1] = gg[1] = bb[1] = 255;
    
    WriteGIF(fpout, pic.data, 0, pic.wide, pic.high,
             rr, gg, bb,
             2, 0, NULL);

if (0)  printf("==== end write gif\n");

}

