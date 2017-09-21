/*===================================================================
 Name:	libxplot2.c
 *=================================================================*/
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>
#include <stdio.h>
#include <signal.h>
#include <math.h>
#include <stdlib.h>
#include "proto.h"
#include "libxplot/xplot.h"
#include "libxplot/proto.h"

#ifdef ANSI_C
void        display2(XY*, int, int);
void        preset2();
#else
void        display2();
void        preset2();
#endif


static Display      *dpy;
static int           scr;
static Window        win, root;
static Visual       *vis;
static GC            gc = (GC)0;
static Pixmap        pixmap;
static int           Dd;

extern char Name[64];
extern unsigned int  W, H;
extern int           Wind_width, Wind_height;
extern unsigned long colors[Ncolors];
extern int           widths[Nwidths];
extern BOX    Box;
extern char   *Notes;
extern int    BegYear;




/*-------------------------------------------------------------------
 *                         preset2
 *------------------------------------------------------------------*/
void preset2()
{
char  *display = getenv("DISPLAY");
static XSetWindowAttributes attrib;
static XSizeHints hints;
int    BorderWidth = 2;
unsigned long  eventmask;

   if (dpy)
   {
if (0) fprintf(stderr,"== preset2: close dpy=%d win=%d gc=%d pxm=%d\n",
    (int) dpy, (int) win, (int) gc, (int) pixmap);
       XCloseDisplay(dpy);
   }


   gc = (GC) 0;
   dpy = NULL;
   win = 0;
   pixmap = 0;

/*---open display---*/

   dpy  = XOpenDisplay(display); 
   scr  = DefaultScreen(dpy);
   root = DefaultRootWindow(dpy);
   vis  = DefaultVisual(dpy,scr);
   Dd   = DefaultDepth(dpy,scr);

   attrib.background_pixel  = WhitePixel(dpy, scr);
   attrib.override_redirect = False;

   hints.flags  = USPosition | USSize | PMinSize;
   hints.x = 5;
   hints.y = H;    
   hints.width = W;
   hints.height = H;
   hints.min_width  = 60;
   hints.min_height = 36;

   eventmask = ExposureMask |
               ButtonPressMask |
               KeyPressMask |
               MotionNotify |
               ConfigureNotify |
               PropertyChangeMask |
               ResizeRedirectMask |
               PointerMotionMask /* |
               StructureNotifyMask */;

   eventmask = 0;

/*---create window---*/

   win = XCreateSimpleWindow(dpy, root,
                             hints.x, hints.y,
                             hints.width, hints.height,
                             BorderWidth, colors[1], colors[0]);

   XSetNormalHints(dpy, win, &hints);
   XStoreName(dpy, win, Name);
   XSelectInput(dpy, win, eventmask);
   XMapWindow(dpy, win);
if (0) fprintf(stderr,"== preset2: end   dpy=%d win=%d gc=%d pxm=%d\n",
    (int) dpy, (int) win, (int) gc, (int) pixmap);
}


/*-------------------------------------------------------------------
 *                         display
 *------------------------------------------------------------------*/
void display2(xy, nxy, psflag)
XY   *xy;
int  nxy;
int  psflag;
{
int    width, type;
char   xlabel[255];
char   notes[255];
float  size;
static int pW, pH;

   if (!xy || !nxy) return;

   W = Wind_width; H = Wind_height;
   if (pW && (pW != W || pH != H))
       preset2();
   pW = W; pH = H;

/* create new pixmap & GC */
   if (gc)
   {
      XFreeGC(dpy, gc);
      XFreePixmap(dpy, pixmap);
   }
   pixmap = XCreatePixmap(dpy, root, W, H, Dd);
   gc = XCreateGC(dpy, pixmap, 0, (XGCValues *)0);

/* set pixmap background */

   XSetForeground(dpy, gc, colors[1]);
   XFillRectangle(dpy, pixmap, gc, 0, 0, W, H);

/* set new pixmap as window background */
   XSetWindowBackgroundPixmap(dpy, win, pixmap);

/* top the window but don't put keyboard or mouse focus into it. */
   XMapRaised(dpy, win);

   width = 0;
   width = widths[2];
   type = LineSolid;
   XSetForeground(dpy, gc, colors[0]);
   XSetLineAttributes(dpy, gc, width, type, CapButt, JoinBevel);


   sprintf(notes, "%s  Residual: observed - smooth \n", Notes);
   if (Box.on == FALSE)
   {
       draw_picture(dpy, gc, pixmap, xy, nxy, notes);
       draw_curve_sq1(dpy, gc, pixmap, xy, nxy);
   }
   else
   {
       XY *z = NULL; int  nz;
       if (get_zoom_data(xy, nxy, &z, &nz))
       {
           draw_picture(dpy, gc, pixmap, z, nz, notes);
           draw_curve_sq1(dpy, gc, pixmap, z, nz);
       }
       if (z) { free_xy(z, "dispaly2: z"); z = NULL; nz = 0; }
   }

   sprintf(xlabel, "Days since %d January 1st", BegYear);
   size = 0.015 * (float)W;
   if (size > 12.0) size = 12.0;
   draw_string(dpy, gc, pixmap, xlabel, size,(int)(0.33*W),(int)(0.05*H), 0);
   if (Box.on) draw_string(dpy, gc, pixmap, "ZOOM", 10.0, 10, 10, 0);

/* trigger exposure of background pixmap */
   XClearWindow(dpy,win);
   XFlush(dpy);
}


/*-------------------------------------------------------------------
 *                         GetWinParms1
 *------------------------------------------------------------------*/
void GetWinParms2(rdpy, rwin, rgc, rpixmap)
Display  **rdpy;
Window   *rwin;
GC       *rgc;
Pixmap   *rpixmap;
{
    *rdpy  = dpy;
    *rwin  = win;
    *rgc   = gc;
    *rpixmap = pixmap;
}

