/*===================================================================
         libxplot1.c
 *=================================================================*/
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>    /* for XtMalloc */
/*
#include <X11/Xmu/StdCmap.h>
#include <X11/Xresource.h>
*/
#include <stdio.h>
#include <signal.h>
#include <math.h>
#include <stdlib.h>
#include <errno.h>
#include "libxplot/xplot.h"
#include "libxplot/proto.h"


#ifdef ANSI_C
static int process_commmand(char*);
static void reset_plot(int);
static void do_postcript(char*);
static void do_gif(char*);
static void preset();
#else
static int process_commmand();
static void reset_plot();
static void do_postcript();
static void do_gif();
static void preset();
#endif

/* This stuff is in /usr/include/linux/time.h */
#ifndef FD_SET
#define FD_SET(n, p)    ((p)->fds_bits[0] |= (1 << ((n) % 32)))
#define FD_CLR(n, p)    ((p)->fds_bits[0] &= ~(1 << ((n) % 32)))
#define FD_ISSET(n, p)  ((p)->fds_bits[0] & (1 << ((n) % 32)))
#define FD_ZERO(p)      bzero((char *)(p), sizeof(*(p)))
#endif

#define STRSIZE 20

static Display    *dpy;
int               scr;
static Window     win;
Window            root;
static Visual     *vis;
static GC         gc = (GC) 0;
static Pixmap     pixmap;
static int        Dd;


#define BUFLEN 1024
char buf[BUFLEN];
int sorted  = FALSE;
int do_sort = FALSE;
int do_pick = FALSE;
char Name[64] = "xdft";
char *Notes;
unsigned long colors[Ncolors];
unsigned int widths[Nwidths] = { 2, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
unsigned int  W, H;
int           Wind_width, Wind_height;
extern float  Xmargin, Ymargin;
extern int    WH;
BOX        Box;
extern XRectangle xrect;


XY     *xy0;   int    nxy0;      /* backup of input data */
XY     *xy;    int    nxy;       /* current data, zoom... */
XY     *sXY;   int    nsxy;      /* smoothed data */
XY     *fitXY; int    nfitxy;    /* fitted data */
XY     *dXY;   int    ndxy;      /* smoothing residual */


/* Frame */
extern float   Xf[2], Yf[2];
extern int     Xfofs, Yfofs;
extern double  Xfact, Yfact;
extern int     logxy, logx, logy, prev_logx, prev_logy;
extern int     stdin_wanted;
extern char    psname[];
extern char    gifname[];
extern Display *hDisplay;
extern Window  hRoot;
extern int     BegYear;



/*==============================================================*
    Do a 'man select' to learn about select.
    See also /usr/include/sys/time.h
 *==============================================================*/
void mainloop(data, ndt, notes)
XY     *data;
int    ndt;
char   *notes;
{
int nf, nfds, cn, in;
struct timeval *timer = (struct timeval *)0;
fd_set rset, tset;
unsigned long all = 0xffffffff;
XEvent xe;
int j;

   nxy = nxy0 = ndt;
   xy0 = data;
   malloc_xy(&xy, nxy, "xy");
   for (j=0;j<nxy;j++)
   {
     xy[j].x = xy0[j].x;
     xy[j].y = xy0[j].y;
   }

   Notes = notes;

   preset();
   cn = ConnectionNumber(dpy);
   in = fileno(stdin);

/* Sort input data */

    if (sorted == FALSE)
    {
        for (j=1;j<nxy-1;j++) if (xy[j+1].x < xy[j].x)
        {
            printf("x[%d]=%.5f < x[%d]=%.5f ==> ",j+1,xy[j+1].x,j,xy[j].x);
            printf("sorting input data\n");
            break;
        }
        if (j == nxy-1) sorted = TRUE;
        else           do_sort = TRUE;
    }
    if (do_sort)
    {
        sortxy(xy, nxy);
    }

   display1(xy, nxy, PS);

   FD_ZERO(&rset);
   FD_SET(cn, &rset);
   FD_SET(in, &rset);
   nfds = (cn > in) ? cn + 1 : in + 1;


again:
   while (1)
   {
      tset = rset;
      nf = select(nfds, &tset, (fd_set *)0, (fd_set *)0, timer);
      if (nf < 0) {
         if (errno == EINTR) continue;
         fprintf(stderr, "plot: select failed. errno:%d\n", errno);
         exit(1);
      }
      if (nf > 0 && XNoOp(dpy));

/* Check stdin */

      if (FD_ISSET(in, &tset))
      {
/* printf("Please send command with X window activated ...\n"); */
          while (fgets(buf, BUFLEN, stdin))
          {
             if (0 && stdin_wanted == FALSE) goto again;
             if (!process_commmand(buf)) return;
             goto again;
          }
          if (feof(stdin) || ferror(stdin)) exit(1);
      }

/* Check window events */

      if (FD_ISSET(cn, &tset))
      {
         XNextEvent(dpy, &xe);
if (0 && xe.type != 6) printf("any event.... type %d\n", xe.type);
         XCheckMaskEvent(dpy, all, &xe);

/*
         if (xe.type == MotionNotify)
         {
            XMotionEvent *xme = (XMotionEvent *) &xme;

            int xx = xme->x_root, yy = xme->y_root;
            if (idx[0])
            {
              int a,b,c,d,e;
              Window       f,g; 
                display1(PS);
                XQueryPointer(dpy,win,&f,&g,&a,&b,&c,&d,&e);
                draw_box(idx[0],idy[0],c,d);
                XClearWindow(dpy,win);
                XFlush(dpy);
            }
         }
*/
         if (xe.type == ResizeRequest)
         {
            XResizeRequestEvent *xre = (XResizeRequestEvent *) &xe;
            Wind_width  = xre->width;
            Wind_height = xre->height;
            if (0) printf("ResizeRequest --> %u  %u\n",
                       Wind_width, Wind_height);
            display1(xy, nxy, PS);
         }
         if (xe.type == ConfigureNotify)
         {
            XConfigureEvent *xce = (XConfigureEvent *) &xe;
            int w = xce->width, h = xce->height;
            if (0) printf("ConfigureNotify --> W %d  H %d\n", w, h);
            display1(xy, nxy, PS);
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
                if (tmp[i] == '\r') { cnt = 0; continue; }
                strbuf[cnt] = tmp[i];
                if (++cnt >= STRSIZE) cnt = 0;
                strbuf[cnt] = 0;
            }
            if (cnt && isprint(tmp[0]))
            { 
                 printf("%c", strbuf[cnt-1]); fflush(stdout);
            }

            /* Process 1 char commands */

            if (strlen(strbuf) < 2)
            {
                switch (tmp[0])
                {

                  case '+':
                    printf("\n"); strbuf[0] = 0; cnt = 0;
                    Wind_width *= 1.1; Wind_height *= 1.1;
                    preset();
                    display1(xy, nxy, PS);
                    break;

                  case '-':
                    printf("\n"); strbuf[0] = 0; cnt = 0;
                    Wind_width /= 1.1; Wind_height /= 1.1;
                    preset();
                    display1(xy, nxy, PS);
                    break;

                  case 'z':
                    printf("\n"); strbuf[0] = 0; cnt = 0;
                    do_zoom();
                    break;

                  case 'd':
                    printf("\n"); strbuf[0] = 0; cnt = 0;
                    select_data(CLEAN);
                    break;

                  case 'e':
                    printf("\n"); strbuf[0] = 0; cnt = 0;
                    select_data(EXTRACT);
                    break;

                  case 'a':
                    printf("\n"); strbuf[0] = 0; cnt = 0;
                    pick(dpy, win, gc, pixmap, 0, 0, 1, 1);
                    break;

                  case 'c':
                    printf("\n"); strbuf[0] = 0; cnt = 0;
                    pick(dpy, win, gc, pixmap, 0, 0, 1, 0);
                    break;

                  case 'r':
                    printf("\n"); strbuf[0] = 0; cnt = 0;
                    reset_plot(0);
                    break;

                  case 'R':
                    printf("\n"); strbuf[0] = 0; cnt = 0;
                    reset_plot(1);
                    break;

                  case 'w':
                    printf("\n"); strbuf[0] = 0; cnt = 0;
                    write_data(xy, nxy);
                    break;

                  default:
                    break;
                }
            }
            /* end process 1 char commands */

            if (tmp[0] == '\r')
            {
              if (!strlen(strbuf)) goto end;
              printf("\n");
              if (!process_commmand(strbuf)) return;
              strbuf[0] = 0; cnt = 0;
            }
            end:
            ;
         }


         if (xe.type == ButtonPress)
         {
            XButtonEvent *xbe = (XButtonEvent *) &xe;
            int x = xbe->x, y = xbe->y;

            if (xbe->button == 2)
            {
                display1(xy, nxy, PS);
                getRectangle(&xrect);
                if (0) printf("%d %d  %d %d  %d %d\n",
                       xrect.x, xrect.y,
                       xrect.width, xrect.height,
                       xrect.x+xrect.width, xrect.y+xrect.height);
                Box.n = 2;
                Box.xi[0] = xrect.x;
                Box.yi[0] = xrect.y;
                Box.xi[1] = xrect.x+xrect.width;
                Box.yi[1] = xrect.y+xrect.height;
                if (1) printf("x0=%d y0=%d  x1=%d y1=%d\n",
                       Box.xi[0], Box.yi[0], Box.xi[1], Box.yi[1]);
                if (1) draw_box(dpy, gc, pixmap,
                         Box.xi[0], Box.yi[0],
                         Box.xi[1], Box.yi[1]);
    XClearWindow(dpy,win);
    XFlush(dpy);
            }
            else if (xbe->button == 3)
            {
                pick(dpy, win, gc, pixmap, x, y, 0, 0);
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

   if (first)
   {
       Wind_width  = 600;
       Wind_height = 360;
       if (WH)
       {
           Wind_width  = WH;
           Wind_height = WH * 0.6;
       }
   }
   else
   {
        XCloseDisplay(dpy);
   }
/*---open display---*/

   dpy  = XOpenDisplay(display); 
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


   colors[0] = BlackPixel(dpy,scr);
   for (i=1; i<Ncolors; i++)
      colors[i] = WhitePixel(dpy,scr);

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

   hDisplay = dpy;
   hRoot    = win;

   XSetNormalHints(dpy, win, &hints);
   XStoreName(dpy, win, Name);
   XSelectInput(dpy, win, eventmask);
   XMapWindow(dpy, win);
   first = 0;
}


/*-------------------------------------------------------------------
 *                         display1
 *------------------------------------------------------------------*/
void display1(xy, nxy, psflag)
XY   *xy;
int  nxy;
int  psflag;
{
int width, type;
char xlabel[255];
float   size;
static int pW, pH;

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


/* Display observed data */
/* Display smoothed data */

   if (Box.on == FALSE)
   {
       draw_picture(dpy, gc, pixmap, xy, nxy, Notes);
       draw_curve_sq1(dpy, gc, pixmap, xy, nxy);
       if (sXY)
       {
          draw_curve_line(dpy, gc, pixmap, sXY, nsxy);
          draw_curve_sq3(dpy, gc, pixmap, sXY, nsxy);
       }
   }
   else
   {
       XY *z = NULL; int nz = 0;

       if (get_zoom_data(xy, nxy, &z, &nz))
       {
           draw_picture(dpy, gc, pixmap, z, nz, Notes);
           draw_curve_sq1(dpy, gc, pixmap, z, nz);
       }
       if (z) { free_xy(z, "display1: z"); z = NULL; nz = 0; }
       if (sXY)
       {
           if (get_zoom_data(sXY, nsxy, &z, &nz))
           {
              draw_curve_line(dpy, gc, pixmap, z, nz);
              draw_curve_sq3(dpy, gc, pixmap, z, nz);
           }
           if (z) { free_xy(z, "display1: z"); z = NULL; nz = 0; }
       }
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
 *                         process_commmand
 *   commands:
 *
 *    a                        add_points
 *    A   A x1 y1 x2 y2 ....   add_points from stdin
 *    c                        create_new_series
 *    d                        delete
 *    e                        select
 *    f                        fit
 *    q                        exit
 *    p   ps[2] ps[2][=fname]  Postscript file
 *    r                        reset
 *    s   s[=t]                smooth
 *    S   S[=t]                smooth+residual
 *    R                        reset
 *    w                        write_to_disk
 *    x                        exit
 *    z                        zoom
 *    +                        resize
 *    -                        resize
 *------------------------------------------------------------------*/
int process_commmand(buf)
char *buf;
{
    switch (buf[0])
    {
        case 'x':
        case 'q':
            return 0;
            break;
        case 'z':
            do_zoom();
            break;
        case 'd':
            select_data(CLEAN);
            break;
        case 'e':
            select_data(EXTRACT);
            break;
        case 'a':
            pick(dpy, win, gc, pixmap, 0, 0, 1, 1);
            break;
        case 'c':
            pick(dpy, win, gc, pixmap, 0, 0, 1, 0);
            break;
        case '+':
            Wind_width *= 1.1; Wind_height *= 1.1;
            preset();
            display1(xy, nxy, PS);
            break;
        case '-':
            Wind_width /= 1.1; Wind_height /= 1.1;
            preset();
            display1(xy, nxy, PS);
            break;
        case 'p':
            do_postcript(buf);
            break;
        case 'g':
            do_gif(buf);
            break;
        case 'w':
            write_data(xy, nxy);
            break;
        case 'A':
            edit(buf);
            break;
        case 's':
            smo(buf);
            break;
        case 'S':
            smo(buf);
            break;
        case 'r':
            reset_plot(0);
            break;
        case 'R':
            reset_plot(1);
            break;
        default:
            {
printf("  NEW:\n");
printf("      SELECTION BOX: press button 2 to get the corner cursor.\n");
printf("                     Then move the mouse and press button 1 to\n");
printf("                     select the first corner, drag the mouse to\n");
printf("                     the second corner and release button 1.\n");
printf("      WINDOW SELECT: most command are active without [RET] when\n");
printf("                     the \"plot\" window is active [selected].\n");
printf("\n");
printf("  z [RET]   zoom data points in selection box\n");
printf("  d [RET]   remove data points in selection box\n");
printf("  e [RET]   extract data points in selection box\n");
printf("  c [RET]   toggle pick mode to create a new data series\n");
printf("  a [RET]   toggle pick mode to add new points to current data\n");
printf("  w [RET]   write data currently displayed to disk\n");
printf("  r [RET]   reset zoom\n");
printf("  R [RET]   reset all: unzoom, restart from original input\n");
printf("  s[=n] [RET]  smooth data; optional n is the period in seconds\n");
printf("  S[=n] [RET]  idem 's' plus plot of residual\n");
printf("  f[=n] [RET]  fit polynomial; optional n is the number of coefs\n");
printf("  ps[=name]  [RET] write Postscript plot to disk\n");
printf("  ps2[=name] [RET] write Postscript residual plot to disk\n");
/*
printf("  +/- [RET] resize window (resizing window frame also works)\n");
*/
printf("  x or q [RET]     EXIT\n");
buf[0] = '\0';
            }
}
    return 1;
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

/* Postcript for Residual; may be zoomed */

    if (strstr(buf, "ps2"))
    {
        char notes[255];

        if (!dXY || !ndxy)
        {
            fprintf(stderr,
                    "WARNING: do_postcript: no residuals file available\n");
            return;
        }
        if (!strcmp(buf, "ps2")) sprintf(psname, "myplot.ps");
        else if (strstr(buf, "ps2=")) sprintf(psname, "%s", &buf[4]);
        trim(psname);
        if (!strlen(psname)) { printf("missing file name\n"); return; }
        if (psname[0] == '-') { printf("bad file name\n"); return; }

        sprintf(notes, "%s  Residual: observed - smooth\n", Notes);

        if (Box.on == FALSE)
            plot_PS(dpy, gc, pixmap, dXY, ndxy, NULL, 0, notes, 1);
        else
        {
          if (get_zoom_data(dXY, ndxy, &z, &nz))
              plot_PS(dpy, gc, pixmap, z, nz, NULL, 0, notes, 1);
        }
        return;
    }

/* Postscript for observed dt and smoothed dt; not zoomed */

    if (!strcmp(buf, "ps")) sprintf(psname, "myplot.ps");
    else if (strstr(buf, "ps=")) sprintf(psname, "%s", &buf[3]);
    trim(psname);
    if (!strlen(psname)) { printf("missing file name\n"); return; }
    if (psname[0] == '-') { printf("bad file name\n"); return; }

    if (Box.on == FALSE)
    {
        if (sXY) { xy2 = sXY; nxy2 = nsxy; }
        plot_PS(dpy, gc, pixmap, xy, nxy, xy2, nxy2, Notes, 1);
        return;
    }

/* Postscript for observed dt and smoothed dt; both zoomed */
/* zoom true */

    get_zoom_data(xy, nxy, &z, &nz);
    if (sXY) get_zoom_data(sXY, nsxy, &xy2, &nxy2);

    plot_PS(dpy, gc, pixmap, z, nz, xy2, nxy2, Notes, 1);
    if (xy2) { free_xy(xy2, "do_postcript: xy2"); xy2=NULL; nxy2=0; }
    if (z)   { free_xy(z,   "do_postcript: z");   z = NULL; nz = 0; }
}

/*-------------------------------------------------------------------
 *                        do_gif
 *------------------------------------------------------------------*/
static void do_gif(buf)
char *buf;
{
    if (!strstr(buf, "gif")) return;
    trim(buf);
    gifname[0] = '\0';
    if (!strcmp(buf, "gif")) sprintf(gifname, "myplot.gif");
    else if (strstr(buf, "gif=")) sprintf(gifname, "%s", &buf[4]);
    trim(gifname);
    if (!strlen(gifname)) { printf("missing file name\n"); return; }
    if (psname[0] == '-') { printf("bad file name\n"); return; }

    plot_GIF(dpy, win, gc);
}

/*-------------------------------------------------------------------
 *                         reset_plot
 *------------------------------------------------------------------*/
static void reset_plot(all)
int all;
{
int j;

    Box.on = FALSE;           
    Box.n = Box.xi[0] = Box.xi[1] = Box.yi[0] = Box.yi[1] = 0;
    if ((logxy+2) % 2) logx = prev_logx = 1;
    if (logxy >= 2)    logy = prev_logy = 1;

    if (all)
    {
/* Free observed and smooth data */
       if (xy)  { free_xy(xy,  "xy");  xy=NULL;  nxy=0; }
       if (sXY) { free_xy(sXY, "sXY"); sXY=NULL; nsxy=0; }
       if (dXY) { free_xy(dXY, "dXY"); dXY=NULL; ndxy=0; }

/* Reload initial observed data into xy */
       nxy = nxy0;
       malloc_xy(&xy, nxy, "xy");
       for (j=0;j<nxy;j++)
       {
           xy[j].x = xy0[j].x;
           xy[j].y = xy0[j].y;
       }
    }

    if (0) printf("== reset%s %d input data\n", (all ? " all:" : ""), nxy);

    if (fitXY) { free_xy(fitXY, "fitXY"); fitXY=NULL; nfitxy=0; }

/* Replot unzoomed data */
    display1(xy, nxy, PS);

/* Replot unzoomed residual, if any */
    preset2();
    display2(dXY, ndxy, PS);

}

