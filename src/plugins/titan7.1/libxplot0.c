/*===================================================================
         libxplot0.c

    Do a 'man select' to learn about select.
    See also /usr/include/sys/time.h
 *=================================================================*/
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>    /* for XtMalloc */
#include <stdio.h>
#include <signal.h>
#include <math.h>
#include <stdlib.h>
#include <errno.h>
#include "titan.h"
#include "proto.h"
#include "libxplot/xplot.h"
#include "libxplot/proto.h"

#ifdef ANSI_C
static int process_command (char*);
static int draw_frame3     (Display*, GC, Pixmap, XY*, int, int);
static void get_seek_ofs   (char*);
static void rescale        (XY*, int);
static void do_postcript   (char*);
static void do_gif         (char*);
static void show_zoom_lim3 (Display*, Window, GC, Pixmap, int, int);
static void do_zoom3       ();
static int get_zoom_data3  (XY*, int, XY**, int*);
static void reset_zoom     ();

#else
static int process_command ();
static int draw_frame3     ();
static void get_seek_ofs   ();
static void rescale        ();
static void do_postcript   ();
static void do_gif         ();
static void show_zoom_lim3 ();
static void do_zoom3       ();
static int get_zoom_data3  ();
static void reset_zoom     ();
#endif

/* This stuff is in /usr/include/linux/time.h */
#ifndef FD_SET
#define FD_SET(n, p)    ((p)->fds_bits[0] |= (1 << ((n) % 32)))
#define FD_CLR(n, p)    ((p)->fds_bits[0] &= ~(1 << ((n) % 32)))
#define FD_ISSET(n, p)  ((p)->fds_bits[0] & (1 << ((n) % 32)))
#define FD_ZERO(p)      bzero((char *)(p), sizeof(*(p)))
#endif

#define STRSIZE 20

static void preset();
char *getenv();

static Display      *dpy;
int                  scr;
static Window        win;
Window               root;
static Visual       *vis;
static GC            gc = (GC)0;
static Pixmap        pixmap;
static int           Dd;

#define BUFLEN  1024

char   buf[BUFLEN];
int    sorted  = FALSE;
int    do_sort = FALSE;
char   Name[64] = "xplot";
char   Notes[200];
unsigned long colors[Ncolors];
unsigned int widths[Nwidths] = { 2, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
unsigned int  W, H;
int           Wind_width, Wind_height;
float         Xmargin, Ymargin;
extern int    WH;
float  ymin[3];
float  ymax[3];
float  ymean[3];

XY     *xy;         int    nxy;    /* Work data */
XY     xy0[MAX_XY]; int    nxy0;   /* Backup of original data */ 
XY     *dXY;        int    ndxy;   /* for compatibility */

BOX    Box;
int    timeWindowSecSave;

int  Seek_ofs;
int  Seek_time;
int  Chan;
char Tmascii[40];

typedef struct
{
    int on;
    int x;
    int y;
} PICK;

static PICK Pick;

static fd_set rset, tset;
int nf, nfds, cn, in;
struct timeval *timer = (struct timeval *)0;


/* Frame */
extern float   Xf[2], Yf[2];
extern int     Xfofs, Yfofs;
extern double  Xfact, Yfact;
extern int     stdin_wanted;

extern char    psname[];
extern char    gifname[];
extern float   fh, fl;
extern float   plot_scale;
extern float   timeWindowSec;
extern Trigparm trig;

extern int ttell(TITFILE *);
extern TITFILE *Fp_tit;
extern struct Channel Channel[NCHAN];
extern outData OutData[NCHAN];
extern struct option opt;
extern char   Station[8];
extern double SystTime;
extern int    Beg_file_ofs[NCHAN];
extern int    new_plot;
extern int    removeFile;


/* For compatibility */
/*==============================================================*/
void mainloop(data, ndt, notes)
XY     *data;
int    ndt;
char   *notes;
{
    return;
}

/*==============================================================*/
void plot_loop(chan)
int  chan;
{
XEvent xe;
unsigned long all = 0xffffffff;
float xl[2];
int   i, n1, n2;
int   comp;

   Chan = chan;
   time_asc4(Tmascii, OutData[Chan].start_time);
   Xmargin = 0.10;
   Ymargin = 0.10;
   sprintf(Notes, "title:%s station=%s  chan=%d", Tmascii, Station, Chan);

   reset_zoom();

   if (new_plot)
   {
if (1) printf("== plot_loop: NEW PLOT chan=%d nxy=%d duration=%.1f\n",
           Chan, nxy, timeWindowSec);

       preset();

       cn = ConnectionNumber(dpy);
       in = fileno(stdin);

       FD_ZERO(&rset);
       FD_SET(cn, &rset);
       FD_SET(in, &rset);
       nfds = (cn > in) ? cn + 1 : in + 1;

       new_plot = 0;
   }

/* Save input data into xy0 */

   for (i=0;i<nxy;i++)
   {
       xy0[i].x = xy[i].x;
       xy0[i].y = xy[i].y;
   }

/*
 * Compute and save input data actual min, max and mean values.
 */
   for (comp=0; comp<3; comp++)
   {
       n1 = comp*nxy/3;
       n2 = (nxy/3);
       min_max_xy(&xy[n1], n2, &xl[0], &xl[1], &ymin[comp], &ymax[comp],&ymean[comp]);
       if (0) printf("== plot_loop: comp %d min-max [%.4e %.4e] m=%.4e\n",
           comp, ymin[comp], ymax[comp], ymean[comp]);
   }
/*
 * Rescale data in Y range: subtract mean and add offsets.
 */
   rescale(xy, nxy);

/*
 * Put image to screen
 */
   display1(xy, nxy, 0);


   if (1 && Fp_tit)
   {
       printf("== plot_loop: sta=%s chan=%d nxy=%d  %s ....... ofs=%d\n",
              Station, Chan, nxy, Tmascii, ttell(Fp_tit));
   }

   while (1)
   {
again:
      tset = rset;
      nf = select(nfds, &tset, (fd_set *)0, (fd_set *)0, timer);
      if (nf < 0)
      {
         if (errno == EINTR) continue;
         fprintf(stderr, "== plot_loop: select failed. errno:%d\n", errno);
         exit(1);
      }
      if (nf > 0 && XNoOp(dpy));

/* Check stdin */

      if (FD_ISSET(in, &tset))
      {
          while (fgets(buf, BUFLEN, stdin))
          {
             if (!process_command(buf)) return;
             goto again;
          }
          if (feof(stdin) || ferror(stdin)) exit(1);
      }

/* Check window events */

      if (FD_ISSET(cn, &tset))
      {
         XNextEvent(dpy, &xe);
if (0 && xe.type != 6) printf("== plot_loop: any event: type %d\n",xe.type);
         XCheckMaskEvent(dpy, all, &xe);
         if (xe.type == MotionNotify)
         {
         }
         if (xe.type == ResizeRequest)
         {
            XResizeRequestEvent *xre = (XResizeRequestEvent *) &xe;
            Wind_width  = xre->width;
            Wind_height = xre->height;
            if (0) printf("ResizeRequest --> %u  %u\n",
                       Wind_width, Wind_height);
            display1(xy, nxy, 0);
         }
         if (xe.type == ConfigureNotify)
         {
            XConfigureEvent *xce = (XConfigureEvent *) &xe;
            int w = xce->width, h = xce->height;
            if (0) printf("ConfigureNotify --> W %d  H %d\n", w, h);
            display1(xy, nxy, 0);
         }

         if (xe.type == KeyPress)
         {
            char        tmp[STRSIZE];
            char        strbuf[STRSIZE];
            XKeyEvent   xkey;
            static int  cnt;
            int         i, count;

            xkey  = xe.xkey;
            count = XLookupString(&xkey, tmp, STRSIZE,
                        NULL /*&keysym*/, NULL /*&compose_status*/);

            if (0) for (i=0;i<count;i++)
            {
                printf("=== tmp %d\n", tmp[i]);
            }

            for (i=0;i<count;i++)
            {
                if (tmp[i] == '\r') { cnt = 0; continue; }
                if (tmp[i] == 127 || tmp[i] == 8)
                {
                  printf("%c", '\b');
                  fflush(stdout);
                  printf("%c", ' ');
                  fflush(stdout);
                  printf("%c", '\b');
                  fflush(stdout);
                  if (cnt > 0) --cnt;
                  strbuf[cnt] = 0;
                  if (0) count = 0;
                  goto end;
                }
            }
            for (i=0;i<count;i++)
            {
                if (tmp[i] == 'q')  exit(0);
                if (tmp[i] == 'x')  exit(0);
                if (tmp[i] == '\t' || tmp[i] == ' ')  return;
                if (tmp[i] == 'B')
                {
                    if (Fp_tit)
                    {
                      Seek_ofs = Beg_file_ofs[Chan] -
                                 (ttell(Fp_tit) - Beg_file_ofs[Chan]);
                      if (Seek_ofs < 0) Seek_ofs = 0;
                      printf("backward chan=%d offset=%d\n", Chan, Seek_ofs);
                    }
                    return;
                }
                if (tmp[i] == 'z')
                {
                  do_zoom3();
                  return;
                }

                if (tmp[i] == 'r')
                {
                  reset_zoom();
                  if (timeWindowSecSave>0) timeWindowSec = timeWindowSecSave;
                  return;
                }
                if (tmp[i] == '+')
                {
                  tmp[i] = '\0';
                  tmp[i] = '\r';
                  plot_scale /= 2.0;
                  rescale(xy, nxy);
                  display1(xy, nxy, 0);
                }
                if (tmp[i] == '-')
                {
                  tmp[i] = '\0';
                  tmp[i] = '\r';
                  plot_scale *= 2.0;
                  rescale(xy, nxy);
                  display1(xy, nxy, 0);
                }
                if (tmp[i] == 'X')
                {
                  removeFile = 1;
                  return;
                }

                if (tmp[i] == '\r') { cnt = 0; continue; }
                strbuf[cnt] = tmp[i];
                if (++cnt >= STRSIZE) cnt = 0;
                strbuf[cnt] = 0;
            }
            if (cnt && isprint(tmp[0]))
            { 
                 printf("%c", strbuf[cnt-1]); fflush(stdout);
            }
            if (tmp[0] == '\r')
            {
              if (!strlen(strbuf)) goto end;
              printf("\n");
              if (!process_command(strbuf)) return;
              strbuf[0] = 0; cnt = 0;
            }
            end:
            ;
         }


         if (xe.type == ButtonPress)
         {
            XButtonEvent *xbe = (XButtonEvent *) &xe;
            int x = xbe->x, y = xbe->y;

            if (xbe->button == 1)
            {
                show_zoom_lim3(dpy, win, gc, pixmap, x, y);
            }

            if (xbe->button == 2)
            {
                do_zoom3();
                return;
            }

            if (xbe->button == 3)
            {
                static FILE *Fp_pick = NULL;
                float xdat, ydat;
                char str[40];

                if (Fp_pick == NULL)
                {
                   char fname[100];
                   sprintf(fname, "%s.pick", Station);
                   printf("\n\tPICK TIME: appending to file '%s'\n\n",fname);
                   fcreat_append(fname, &Fp_pick);
                   fseek(Fp_pick, 0L, SEEK_END);
                }

                pix2dat(x, y, &xdat, &ydat);
                time_asc4(str, OutData[Chan].start_time + xdat);
                fprintf(Fp_pick, "%s\n", str);
                fflush(Fp_pick);
                Pick.on = TRUE;
                Pick.x  = x;
                display1(xy, nxy, !PS);

            }
         }
      }
   }
}



/*-------------------------------------------------------------------
 *                         preset
 *------------------------------------------------------------------*/
static void preset()
{
char  *display = getenv("DISPLAY");
XSetWindowAttributes attrib;
XSizeHints hints;
unsigned long  eventmask;
int    BorderWidth = 2;
static int first = 1;
int    i;

   if (!first)
   {
        XCloseDisplay(dpy);
   }

   Wind_width  = WH;
   Wind_height = WH * 0.6;

/*---open display---*/

   if ((dpy  = XOpenDisplay(display)) == NULL)
   {
      fprintf(stderr, "ERROR: plot_loop: preset: null display.\n");
      exit(1);
   }
   scr  = DefaultScreen(dpy);
   root = DefaultRootWindow(dpy);
   vis  = DefaultVisual(dpy,scr);
   Dd   = DefaultDepth(dpy,scr);

   W = Wind_width;
   H = Wind_height;
   gc = (GC) 0;

   attrib.background_pixel  = WhitePixel(dpy, scr);
   attrib.override_redirect = False;

   hints.flags  = USPosition | USSize | PMinSize;
   hints.x = 5;
   hints.y = 5;        
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

/*
   colors[0] = BlackPixel(dpy,scr);
   for (i=1; i<Ncolors; i++)
      colors[i] = WhitePixel(dpy,scr);
*/

{
   Colormap cmap = DefaultColormap(dpy, scr);
   XColor def_color, exc_color;

   for (i=0; i<Ncolors; i++)
      colors[i] = BlackPixel(dpy,scr);
   colors[BLACK] = BlackPixel(dpy,scr);
   colors[WHITE] = WhitePixel(dpy,scr);
   XAllocNamedColor(dpy, cmap, "red", &def_color, &exc_color);
   colors[RED]     = def_color.pixel;
   XAllocNamedColor(dpy, cmap, "green", &def_color, &exc_color);
   colors[GREEN]   = def_color.pixel;
   XAllocNamedColor(dpy, cmap, "blue", &def_color, &exc_color);
   colors[BLUE]    = def_color.pixel;
   XAllocNamedColor(dpy, cmap, "yellow", &def_color, &exc_color);
   colors[YELLOW]  = def_color.pixel;
   XAllocNamedColor(dpy, cmap, "magenta", &def_color, &exc_color);
   colors[MAGENTA] = def_color.pixel;
   XAllocNamedColor(dpy, cmap, "cyan", &def_color, &exc_color);
   colors[CYAN]    = def_color.pixel;
}


/*---create window---*/
/*
unsigned long  windowmask = CWBackPixel | CWBorderPixel;
int     depth = 8;
   win = XCreateWindow(dpy, root, 
                       hints.x, hints.y,
                       hints.width, hints.height,
                       BorderWidth,
                       depth,
                       InputOutput,
                       vis,
                       windowmask,
                       &attrib);
*/

   win = XCreateSimpleWindow(dpy, root,
                             hints.x, hints.y, 
                             hints.width, hints.height,
                             BorderWidth, colors[1], colors[0]);


   XSetNormalHints(dpy, win, &hints);
   XStoreName(dpy, win, Name);
   XSelectInput(dpy, win, eventmask);
   XMapWindow(dpy, win);
   first = 0;
}


/*-------------------------------------------------------------------
 *                         display1
 *------------------------------------------------------------------*/
void display1(xy, nxy, ps)
XY   *xy;
int  nxy;
int  ps;
{
int width, type;
char str[255];
float   size;
static int pW, pH;
int xp, yp1, i, j;
int comp;

if (0) printf("==== display1: xy=%p nxy=%d PS=%d\n", xy, nxy, ps);

   W = Wind_width; H = Wind_height;
   if (pW && (pW != W || pH != H))
   {
       if (0) printf("==== display1: W %d H %d is new; preset\n", W,H);
       preset();
   }
   pW = W; pH = H;
/*
printf("======== display1: W %d H %d\n", W,H);
            XResizeWindow(dpy, win, (u_int) W, (u_int) H);
*/

/* create new pixmap & GC */
   if (gc)
   {
      XFreeGC(dpy, gc);
      XFreePixmap(dpy, pixmap);
   }
   pixmap = XCreatePixmap(dpy, root, W, H, Dd);
   gc = XCreateGC(dpy, pixmap, 0, (XGCValues *)0);

/* set pixmap background */
/* set color: colors[1] = white */
   XSetForeground(dpy, gc, colors[BLACK]);
   XFillRectangle(dpy, pixmap, gc, 0, 0, W, H);

/* set new pixmap as window background */
   XSetWindowBackgroundPixmap(dpy, win, pixmap);

/* top the window but don't put keyboard or mouse focus into it. */
   XMapRaised(dpy, win);

   width = 0;
   width = widths[2];
   type = LineSolid;
   XSetForeground(dpy, gc, colors[WHITE]);
   XSetLineAttributes(dpy, gc, width, type, CapButt, JoinBevel);

/*
 * Draw frame and x scale
 */
   
   XSetForeground(dpy, gc, colors[YELLOW]);
   draw_frame3(dpy, gc, pixmap, xy, nxy, 0);

/*
 * Draw seismic traces
 */
   for (i=0; i<3; i++)
   {
       XSetForeground(dpy, gc, colors[GREEN]);
       draw_curve_line(dpy, gc, pixmap, &xy[i*nxy/3], (nxy/3));
   }

   XSetForeground(dpy, gc, colors[YELLOW]);

/*=========================
   if (1 || Box.on == FALSE)
   {
       draw_frame3(dpy, gc, pixmap, xy, nxy, 0);
       for (i=0; i<3; i++)
           draw_curve_line(dpy, gc, pixmap, &xy[i*nxy/3], (nxy/3));
   }
   else
   {
       XY *z = NULL; int nz = 0;

       if (get_zoom_data3(xy, nxy, &z, &nz))
       {
           draw_frame3(dpy, gc, pixmap, z, nz, 0);
           for (i=0; i<3; i++)
               draw_curve_line(dpy, gc, pixmap, &z[i*nz/3], (nz/3));
       }
       else
       {
           draw_frame3(dpy, gc, pixmap, xy, nxy, 0);
           for (i=0; i<3; i++)
               draw_curve_line(dpy, gc, pixmap, &xy[i*nxy/3], (nxy/3));
       }
       if (z) { free_xy(z, "display1: z"); z = NULL; nz = 0; }
       reset_zoom();
   }
===========================*/

   size = 0.010 * W;
   if (size > 12.0) size = 12.0;

/*
 * Draw pick index
 */
   if (Pick.on)
   {
       float xdat, ydat;
       char str[40];
       int y0 = Yfofs;
       int y1 = H * (1.0 - Ymargin);

       Pick.on = FALSE;
       pix2dat(Pick.x, Pick.y, &xdat, &ydat);
       time_asc4(str, OutData[Chan].start_time + xdat);

printf("+++ x=%d --> %.3f secs --> %s\n", Pick.x, xdat, str);

       XDrawLine(dpy, pixmap, gc, Pick.x,y0,Pick.x,y1);
       draw_string(dpy, gc,pixmap, str,size, Pick.x+5,(int)(0.30*y0),0);
   }

/*
 * Draw zoom limits
 */
   if (Box.xi[0] != 0)
   {
       int x0, x1;
       int y0 = Yfofs;
       int y1 = H * (1.0 - Ymargin);
       float xdat, ydat;
       char str[40];

       x0 = Box.xi[0];
       pix2dat(x0, 0, &xdat, &ydat);
       time_asc4(str, OutData[Chan].start_time + xdat);
       XDrawLine(dpy, pixmap, gc, x0,y0,x0,y1);
       draw_string(dpy, gc,pixmap, str,size, x0+5,(int)(0.30*y0),!PS);

       if (Box.xi[1] != 0)
       {
           x1 = Box.xi[1];
           pix2dat(x1, 0, &xdat, &ydat);
           time_asc4(str, OutData[Chan].start_time + xdat);
           XDrawLine(dpy, pixmap, gc, x1,y0,x1,y1);
           draw_string(dpy, gc,pixmap, str,size, x1+5,(int)(0.30*y0),!PS);
       }
   }

/*
 * Station
 */
   sprintf(str, "%s", Station);
   draw_string(dpy, gc,pixmap, str,size,     (int)(0.1*W), (int)(0.95*H),ps);

/*
 * Channel number & sample rate
 */
   sprintf(str, "chan %d srate %.5f\n", Chan, OutData[Chan].srate);
   draw_string(dpy, gc,pixmap, str, size,    (int)(0.18*W),(int)(0.95*H),ps);

/*
 * Startime string
 */
   draw_string(dpy, gc,pixmap, Tmascii,size, (int)(0.4*W),(int)(0.95*H),ps);

/*
 * File offset
 */
   if (Fp_tit != NULL)
     sprintf(str, "file offset %d to %d\n",Beg_file_ofs[Chan],ttell(Fp_tit));
   else
     sprintf(str, "file offset %d\n",Beg_file_ofs[Chan]);
   draw_string(dpy, gc,pixmap, str, size, (int)(0.4*W),(int)(0.92*H),ps);

/*
 * Y-range string
 */
   sprintf(str, "Full Scale +/- %.0f counts", plot_scale);
   draw_string(dpy, gc,pixmap, str, size,      (int)(0.7*W),(int)(0.95*H),ps);

/*
 * X units: secs
 */
   sprintf(str, "secs");
   draw_string(dpy, gc, pixmap, str, size,(int)(0.1*W), (int)(0.065*H),ps);

/*
 * Y units: counts
 */
   sprintf(str, "counts");
   draw_string(dpy, gc, pixmap, str, size,(int)(0.03*W), (int)(0.9*H),ps);
   
/*
 * Mean value string
 */
   sprintf(str, "mean");
   draw_string(dpy, gc, pixmap, str, size,(int)(0.92*W), (int)(0.2*H),ps);


   for (comp=0,j=2; comp<3; comp++,j-=2)
   {
/* Comp number */
       sprintf(str, "%d", comp);
       dat2pix(0., -j*plot_scale, &xp, &yp1);
       dat2pix(0.05*W, -j*plot_scale, &xp, &yp1);
       yp1 -= (int) (0.015*H);
       draw_string(dpy, gc, pixmap, str, size,(int)(0.07*W),yp1,ps);

/* mean value */
       sprintf(str, "%+.0f", ymean[comp]);
       dat2pix(0., -j*plot_scale, &xp, &yp1);
       draw_string(dpy, gc, pixmap, str, size,(int)(0.91*W),yp1,ps);

/* upper range */
       sprintf(str, "%+.0f", -plot_scale + ymean[comp]);
       dat2pix(0., -(j-1)*plot_scale, &xp, &yp1);
       draw_string(dpy, gc, pixmap, str, size,20,yp1,ps);


/* lower range */
       sprintf(str, "%+.0f", +plot_scale + ymean[comp]);
       dat2pix(0., -(j+1)*plot_scale, &xp, &yp1);
       draw_string(dpy, gc, pixmap, str, size,20,yp1-15,ps);
   }

/* trigger exposure of background pixmap */
   XClearWindow(dpy,win);
   XFlush(dpy);
}

/* Those are for compatibility only */

void preset2()
{
    return;
}

void display2(xy, nxy, psflag)
XY   *xy;
int  nxy;
int  psflag;
{
    return;
}


/*-------------------------------------------------------------------
 *                         GetWinParms1
 *------------------------------------------------------------------*/
void GetWinParms1(rdpy, rwin, rgc, rpixmap)
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


/*-------------------------------------------------------------------
 *                         process_command
 *------------------------------------------------------------------*/
static int process_command(buf)
char *buf;
{

/*
static int n;
    ++n;
fprintf(stderr, "process_command ---- %d (%d)\n", buf[0], n);
    if ( 1 && (buf[0] == 'q' || buf[0] == 'x'))
    {
        exit(0);
    }
*/
    if (buf[0] == '+')
    {
      plot_scale /= 2.0;
      rescale(xy, nxy);
      display1(xy, nxy, 0);
    }
    else if (buf[0] == '-')
    {
      plot_scale *= 2.0;
      rescale(xy, nxy);
      display1(xy, nxy, 0);
    }
    else if ((buf[0] == '\t') || (buf[0] == ' '))
    {
        return 0;
    }
    else if (!strncmp(buf, "d=", 2))
    {
        timeWindowSec = atof(&buf[2]);
        timeWindowSecSave = timeWindowSec;
    }
    else if (!strncmp(buf, "ps", 2))
    {
        do_postcript(buf);
    }
    else if (!strncmp(buf, "gi", 2))
    {
        do_gif(buf);
    }
    else if (!strncmp(buf, "o=", 2))
    {
        get_seek_ofs(&buf[2]);
    }
    else if (!strncmp(buf, "B", 1))
    {
        if (Fp_tit)
        {
            Seek_ofs = Beg_file_ofs[Chan] - 20000;
            Seek_ofs = Beg_file_ofs[Chan] - (ttell(Fp_tit) - Beg_file_ofs[Chan]);
            if (Seek_ofs < 0) Seek_ofs = 0;
            printf("backward chan=%d offset=%d\n", Chan, Seek_ofs);
        }
    }
    else if (!strncmp(buf, "t=", 2))
    {
      double utime;
        if (strchr(&buf[2], ':') ||
            strchr(&buf[2], '.'))
        {
          char    *token[10];
          int     ntok;
          char    s1[40];
          char    ttime[40];

            trim(buf);
            strncpy(ttime, &buf[2], strlen(&buf[2]));
            ntok = sparse(ttime, token, ":-.", 6);
            if (ntok > 2)
            {
                sprintf(ttime, "1970.01.01-00.00.00.000");
                strncpy(ttime, &buf[2], strlen(&buf[2]));
                sprintf(s1, "%s", ttime);

                str2utime1(s1, &utime);
                Seek_time = (int) utime;
                printf("    -> seek to %d %s\n", Seek_time, s1);
            }
        }
        else
        {
            Seek_time = atoi(&buf[2]);
            time_asc4(buf, (double) Seek_time);
            printf("    -> seek to %s\n", buf);
        }
    }
    else if (!strncmp(buf, "chan=", 5))
    {
        if (strstr(buf, "all"))
        {
            opt.chan = -1;
            if (1) printf("    -> displaying all channels\n");
        }
        else
        {
            opt.chan = atoi(&buf[5]);
            if (opt.chan < 0 || opt.chan > 3)
            {
                printf("ERROR: channel %d not supported\n", opt.chan);
                goto help;
            }
            if (1) printf("    -> displaying channel %d\n", opt.chan);
        }
    }
    else if (!strncmp(buf, "comp=", 5))
    {
        opt.comp = atoi(&buf[5]);
        if (opt.comp < 0 || opt.comp > 2)
        {
            printf("ERROR: component %d not supported\n", opt.comp);
            goto help;
        }
        if (1) printf("    -> trigger on component %d\n", opt.comp);
    }
    else goto help;

    return 1;


help:
printf("============================\n");
printf("  SPACE or TAB   go forward\n");
printf("  B              go backward\n");
printf("  +/-        change y sensitivity: + to increase, - to decrease.\n");
printf("  button 1   select zoom limit\n");
printf("  z          apply zoom\n");
printf("  r          reset zoom\n");
printf("  button 3   pick time; output file is STA.pick\n");
printf("  d=x  RET   set plot duration to x (seconds)\n");
printf("  t=x  RET   goto to time x. Format: yyyy.mm.dd[.HH][.MM][.SS]\n");
printf("  o=x  RET   goto to file offset x (nnnM or nnnK accepted\n");
printf(
    "  chan=x RET  plot channel x. \"ch=all\" for all (default).\n");
printf(
    "  comp=x RET  apply trigger on specified component; default: comp 0\n");
printf("  ps[=name]  RET write Postscript file to disk.\n");
printf("  gif[=name] RET write GIF file to disk.\n");
printf("  q or x    exit\n");
/* buf[0] = '\0'; */

    return 1;
}

/*-------------------------------------------------------------------
 *                        rescale 
 * Remove mean and add offset equal to plot_scale modulo 2.
 * ATTENTION: This is inside this module that the 3 traces are
 * switched vertically in order to put the component 0 in the
 * the upper frame and the component 2 in the lower frame.
 * The same sheme must be applied to the strings "comp num",
 * "mean value ... in display1().
 *------------------------------------------------------------------*/
static void rescale(xy_, nxy_)
XY *xy_;
int nxy_;
{
int   comp, n;

/*
 * Remove mean and add offset equal to plot_scale modulo 2.
 */
    for (comp=0; comp<3; comp++)
    {
        for (n=comp*nxy_/3; n<((comp+1)*nxy_/3); n++)
            xy_[n].y = xy0[n].y - ymean[comp] - (comp-1)*2*plot_scale;
    }
}

/*-------------------------------------------------------------------
 *                        do_postcript 
 *------------------------------------------------------------------*/
static void do_postcript(buf)
char *buf;
{
XY    *xy2 = NULL;
int   nxy2 = 0;
XY *z = NULL; int nz = 0;

    psname[0] = '\0';
    if (!strstr(buf, "ps")) return;
    trim(buf);

    if (!strcmp(buf, "ps")) sprintf(psname, "myplot.ps");
    else if (strstr(buf, "ps=")) sprintf(psname, "%s", &buf[3]);
    trim(psname);
    if (!strlen(psname)) { printf("missing file name\n"); return; }
    if (psname[0] == '-') { printf("bad file name\n"); return; }

    if (Box.on == FALSE)
    {
        plot_PS(dpy, gc, pixmap, xy, nxy, xy2, nxy2, Notes, 0);
        return;
    }

    plot_PS(dpy, gc, pixmap, z, nz, xy2, nxy2, Notes, 0);
    if (xy2) { free_xy(xy2, "do_postcript: xy2"); xy2=NULL; nxy2=0; }
    if (z)   { free_xy(z,   "do_postcript: z");   z = NULL; nz = 0; }
}


/*-------------------------------------------------------------------
 *                        do_gif
 *------------------------------------------------------------------*/
static void do_gif(buf)
char *buf;
{
    trim(buf);
    gifname[0] = '\0';
    if (!strcmp(buf, "gif")) sprintf(gifname, "myplot.gif");
    else if (strstr(buf, "gif=")) sprintf(gifname, "%s", &buf[4]);
    trim(gifname);
    if (!strlen(gifname)) { printf("missing file name\n"); return; }
    if (gifname[0] == '-') { printf("bad file name\n"); return; }
    plot_GIF(dpy, win, gc);
}


/*-------------------------------------------------------------------
 *                         get_seek_ofs
 *------------------------------------------------------------------*/
static void get_seek_ofs(buf)
char   *buf;
{
char n_asc[10];
int n;

    n = atoi(buf);
    sprintf(n_asc, "%s", buf);
    if (strstr(n_asc, "M") || strstr(n_asc, "m"))
    {
        n *= 1000000;
    }
    else if (strstr(n_asc, "K") || strstr(n_asc, "k"))
    {
        n *= 1000;
    }
    Seek_ofs = n;
    printf("file offset = %d\n", Seek_ofs);
}


/*===============================================================*/
static int draw_frame3(dpy, gc, pixmap, xy, nxy, ps)
Display  *dpy;
GC        gc;
Pixmap    pixmap;
XY      *xy;
int     nxy;
int     ps;
{
int     x0, y0, x1, y1;
float   size, k;
float   xl[2], yl[2];
float   xa[2], xi;
int     n, n1, n2;
static char fmt[20];
int     nxtick;
int     org[2];
int     log = 0;
static int debug = 0;
float ymean;
extern float Xtick;

/* Frame offset */

    Xfofs  = W * Xmargin;
    Yfofs  = H * Ymargin;

if (debug)
{
    printf("window size:   width=%3d height=%3d (pixels)\n", W, H);
    printf("frame offsets: Xfofs=%3d  Yfofs=%3d (pixels)\n", Xfofs,Yfofs);
}

/* min, max of data */

    min_max_xy(xy, nxy, &xl[0], &xl[1], &yl[0], &yl[1], &ymean);


if (plot_scale == 0.0)
{
    plot_scale = fabs(yl[1] - yl[0]) / 2.0 / 3.0;
    if (plot_scale < 100.0) plot_scale = 100.0 / 3.0;
}
else
{
    yl[1] = 3*plot_scale;
    yl[0] = -yl[1];
}

if (0) printf("== draw_frame: plot_scale %.3f yl=[%.1f %.1f]\n",
           plot_scale, yl[0], yl[1]);

/* expand x and y range  */

if (debug) printf("frame: x min-max [%.4e %.4e]\n", xl[0], xl[1]);
     expand(log, xl);
if (debug) printf("frame: expand: x [%.4e %.4e]\n",xl[0],xl[1]);

/* expand y min max */

if (debug) printf("frame: y min-max [%.4e %.4e]\n", yl[0], yl[1]);
    expand(log, yl);
if (debug) printf("frame: expand: y [%.4e %.4e]\n",yl[0],yl[1]);

/* Set frame range and calculate bin to pixel factor */

    Xf[0] = xl[0]; Xf[1] = xl[1];
    Yf[0] = yl[0]; Yf[1] = yl[1];
    Xfact = (W * (1.0-2.0*Xmargin)) / fabs(Xf[1] - Xf[0]);
    Yfact = (H * (1.0-2.0*Ymargin)) / fabs(Yf[1] - Yf[0]);

if (debug)
{
    printf("frame: Xf [%.4e %.4e] Xfact=%.4e\n",Xf[0],Xf[1],Xfact);
    printf("       Yf [%.4e %.4e] Yfact=%.4e\n",Yf[0],Yf[1],Yfact);
}

    x0 = Xfofs;
    y0 = Yfofs;
    x1 = W * (1.0 - Xmargin);
    y1 = H * (1.0 - Ymargin);

/*
 * 3 boxes: 1 for each component
 */

    {
    int xp, yp1, yp2;

        dat2pix(0., 3.0*plot_scale, &xp, &yp1);
        dat2pix(0., 1.0*plot_scale, &xp, &yp2);
        draw_box(dpy, gc, pixmap, x0+5,yp1,x1-5,yp2);

if (0)
{
    printf("frame: scale %.1f %.1f --> %d\n", plot_scale,3.0*plot_scale,yp1);
    printf("frame: scale %.1f %.1f --> %d\n", plot_scale,1.0*plot_scale,yp2);
}

        dat2pix(0.,  plot_scale, &xp, &yp1);
        dat2pix(0., -plot_scale, &xp, &yp2);
        draw_box(dpy, gc, pixmap, x0+5,yp1,x1-5,yp2);

        dat2pix(0., -1.0*plot_scale, &xp, &yp1);
        dat2pix(0., -3.0*plot_scale, &xp, &yp2);
        draw_box(dpy, gc, pixmap, x0+5,yp1,x1-5,yp2);
    }

/*
 * Main frame box
 */

    y0 = -y0 + H;
    y1 = -y1 + H;

if (debug)
{
   printf("frame:  x0=%4d x1=%4d  (%4d  %4d)\n", x0, x1, x0+x1, W);
   printf("frame:  y0=%4d y1=%4d  (%4d  %4d)\n", y0, y1, y0+y1, H);
}
    if (!ps)
    {
        draw_box(dpy, gc, pixmap, x0,y0,x1,y1);
    }
    else
    {
        draw_box_ps(x0,y0,x1,y1);
    }



/* Resize character size with regard of window size */

    k = 0.015;
    k = 0.011;
    size = k * (float)W;
    if (size > 12.0) size = 12.0;



/*===================== X axis =======================*/
    xa[0] = Xf[0];
    xa[1] = 1.6 *(Xf[1]-Xf[0]) + Xf[0];
    nicer(xa, 0.0, xa, &Xtick);
if (debug) printf("nicer out x [%.4e %.4e] Xtick=%.4e\n",
                        xa[0], xa[1], Xtick);
    xy_num_fmt(xl, Xtick, fmt);

    n1 = rint(Xf[0] / Xtick);
    n2 = rint(Xf[1] / Xtick + 1.0);
    nxtick = 0;

/* If log scale, find number of decades to plot */

    for (n=n1; n<n2; n++)
    {
        xi = (float)n*Xtick;
        if (xi < Xf[0] || xi > Xf[1]) continue;
        org[0] = (xi - Xf[0]) * Xfact + Xfofs;
        org[1] = Yfofs;
if (0) printf("axes: x_ticks: %d x=%f %f\n", n, xi-Xf[0], xi);

        if (!nxtick || (nxtick > 1 && (fabs(mod(xi,1.0)) <= 0.00001)))
        {
            if (!ps)
                draw_tick(dpy, gc, pixmap, X, xi, org, size, fmt, nxtick);
            else
                draw_tick_ps(dpy, gc, pixmap, X, xi, org, size, fmt, nxtick);
        }
    }

/*===================== Y axis =======================*/

    return 0;
}


/*===============================================================*/
static void show_zoom_lim3(dpy, win, gc, pixmap, x, y)
Display  *dpy;
Window    win;
GC        gc;
Pixmap    pixmap;
int       x;
int       y;
{

/* Check and reset box variables */

    if (Box.n > 1) reset_zoom();

    Box.xi[Box.n] = x;
    if (0) printf("== click %d  x=%d  y=%d\n",
               Box.n, Box.xi[Box.n], Box.yi[Box.n]);

    display1(xy, nxy, !PS);
    ++Box.n;
}


/*===============================================================*/
static void do_zoom3()
{
int beg_ofs, end_ofs;
    
    if (Box.n < 1)
    {
      printf("NO ZOOM LIMITS AVAILABLE; USE BUTTON 1 TO SELECT LIMITS.\n");
      return;
    }
    Box.on = TRUE;

    pix2dat(Box.xi[0], Box.yi[0], &Box.xd[0], &Box.yd[0]);
    pix2dat(Box.xi[1], Box.yi[1], &Box.xd[1], &Box.yd[1]);
if (0) printf("do_zoom3: x1=%d  xd1=%f\n", Box.xi[0],Box.xd[0]);
if (0) printf("do_zoom3: x2=%d  xd2=%f\n", Box.xi[1],Box.xd[1]);

    if (Box.xd[0] < 0.0) Box.xd[0] = 0.0;
    end_ofs = ttell(Fp_tit);
    beg_ofs = Beg_file_ofs[Chan];
    Seek_ofs = beg_ofs + (Box.xd[0] / timeWindowSec) * (end_ofs-beg_ofs);
    Seek_ofs -= 2000;

/* Save plot duration, but just once (do not save if multiple zoom */
    if (timeWindowSecSave == 0) timeWindowSecSave = timeWindowSec;
/* Set zoom plot duration */
    timeWindowSec = Box.xd[1] - Box.xd[0];

/*
    printf("do_zoom3: beg_ofs=%d end_ofs=%d   seek to %d duration %.2f\n",
        beg_ofs, end_ofs, Seek_ofs, timeWindowSec);
*/
    { Box.n=Box.xi[0]=Box.xi[1]=Box.yi[0]=Box.yi[1]=0; }
}


/*===============================================================*/
static int get_zoom_data3(xy, nxy, zxy, nzxy)
XY  *xy;
int nxy;
XY  **zxy;
int *nzxy;
{
int i1, i2, i, j;
int nxy1;
int comp, n;


if (1) printf("== get_zoom_data3: nxy=%d\n", nxy);
if (1) printf("== get_zoom_data3: n=%d  x1=%.3f x2=%.3f  y1=%.3f y2=%.3f\n",
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
if (1) printf("== get_zoom_data3: i1=%d i2=%d\n", i1, i2);

    nxy1  = i2 - i1;
    *nzxy = nxy1 * 3;
    if (*zxy) { free_xy(*zxy, "get_zoom_data3: zxy"); *zxy = NULL; }
    malloc_xy(zxy, (*nzxy)+2, "get_zoom_data3: zxy");

    n = 0;
    for (comp=0; comp<3; comp++)
        printf("++++ %d to %d\n", i1 + comp*nxy/3, nxy1+i1 + comp*nxy/3);
    for (comp=0; comp<3; comp++)
        for (j=0; j<nxy1; j++, n++)
        {
            (*zxy)[n].x = xy[j+i1 + comp*nxy/3].x;
            (*zxy)[n].y = xy[j+i1 + comp*nxy/3].y;
        }

if (1) printf("get_zoom_data3: end: *zxy %p nzxy %d\n", *zxy, *nzxy);
        return 1;
    }
printf("get_zoom_data3 failed: ix1=%.3f x2=%.3f\n", Box.xd[0], Box.xd[1]);
    return 0;
}

/*===============================================================*/
static void reset_zoom()
{
   Box.on=Box.n=Box.xi[0]=Box.xi[1]=Box.yi[0]=Box.yi[1]=0;
}
