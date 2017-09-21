/*====================================================================
        libplot.c

 Relationships between data values and pixel values

    bin data range:   xval0-xvalN  --> nice frame range: Xf[0]-Xf[1]
    pixels range:     xpix0-xpixN
    bin to pixel conversion fact: Xfact = (xpixN-xpix0) / (Xf[1]-Xf[0])
    
        xpix[i] = (xval[i]-xval0) * Xfact + Xfofs

    Conversely
        xval[i] = (xpix[i] - Xfofs) / Xfact + xval0

    There is a first order relationship between xval[i] and i:
        i = A.xval[i] + B

        i = (N / (Xf[1]-Xf[0])) * (xval[i] - Xf[0])
    We use this equation into the zoom mode.

 *===================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <X11/Xlib.h>
#include "xplot.h"
#include "proto.h"

int              logxy, logx, logy, prev_logx, prev_logy;
float            Xf[2], Yf[2], Xtick, Ytick;
int              Xfofs, Yfofs;
double           Xfact, Yfact;
int              stdin_wanted = 1;
float            Xmargin, Ymargin;

extern int       Wind_width, Wind_height;
extern unsigned int  W, H;
extern char     *char_1[], *char_2[], *char_3[], *char_4[];
    

/*===============================================================*/
void plot(xy, nxy, lgxy, notes, stdin_flag)
XY      *xy;
int     nxy;
int     lgxy;
char    *notes;
int     stdin_flag;
{

    logxy = lgxy;
    if (stdin_flag == 1) stdin_wanted = 0;

    logx = logy = 0;
    prev_logx = prev_logy = 0;
    if ((logxy+2) % 2) logx = 1;
    if (logxy >= 2)    logy = 1;
    logxy_data(xy, nxy);

    mainloop(xy, nxy, notes);
}

/*===============================================================*/
int draw_picture(dpy, gc, pixmap, xy, nxy, notes)
Display  *dpy;
GC       gc;
Pixmap   pixmap;
XY       *xy;
int      nxy;
char     *notes;
{
    if (draw_frame(dpy, gc, pixmap, xy, nxy, 0)) return 1;
    draw_notes(dpy, gc, pixmap, notes);
    draw_title(dpy, gc, pixmap, notes, 0);
    return 0;
}


/*===============================================================*/
int draw_frame(dpy, gc, pixmap, xy, nxy, ps)
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
int     nxtick, nytick;
int     org[2];
int     log = 0;
static int debug = 0;
float   ymean;

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

/* Draw frame */

    x0 = Xfofs;
    y0 = Yfofs;
    x1 = W * (1.0 - Xmargin);
    y1 = H * (1.0 - Ymargin);
    y0 = -y0 + H;
    y1 = -y1 + H;

if (debug)
{
   printf("frame:  x0=%4d x1=%4d  (%4d  %4d)\n", x0, x1, x0+x1, W);
   printf("frame:  y0=%4d y1=%4d  (%4d  %4d)\n", y0, y1, y0+y1, H);
}
    if (!ps)
        draw_box(dpy, gc, pixmap, x0,y0,x1,y1);
    else
        draw_box_ps(x0,y0,x1,y1);

/* Resize character size with regard of window size */

    k = 0.017;
    k = 0.015;
    size = k * (float)W;
    if (size > 12.0) size = 12.0;

    min_max_xy(xy, nxy, &xl[0], &xl[1], &yl[0], &yl[1], &ymean);

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

    if (logx) for (n=n1; n<n2; n++) {
        xi = (float)n*Xtick;
        if (xi < Xf[0] || xi > Xf[1]) continue;
        if (fabs(mod(xi,1.0)) <= 0.00001) ++nxtick;
    }

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
    xa[0] = Yf[0];
    xa[1] = 1.6 *(Yf[1]-Yf[0]) + Yf[0];
    nicer(xa, 0.0, xa, &Ytick);
if (debug) printf("nicer out y [%.4e %.4e] Ytick=%.4e\n",
                        xa[0], xa[1], Ytick);
    xy_num_fmt(yl, Ytick, fmt);

    n1 = rint(Yf[0] / Ytick);
    n2 = rint(Yf[1] / Ytick + 1.0);
    nytick = 0;

/* If log scale, find number of decades to plot */

    if (logy) for (n=n1; n<n2; n++) {
        xi = (float)n*Ytick;
        if (xi < Yf[0] || xi > Yf[1]) continue;
        if (fabs(mod(xi,1.0)) <= 0.00001) ++nytick;
    }

    for (n=n1; n<n2; n++)
    {
        xi = (float)n*Ytick;
        if (xi < Yf[0] || xi > Yf[1]) continue;
if (0) printf("axes: y_ticks: %d y=%f %f\n", n, xi-Yf[0], xi);
        org[0] = Xfofs;
        org[1] = (xi - Yf[0]) * Yfact + Yfofs;

        if (!nytick || (nytick > 1 && (fabs(mod(xi,1.0)) <= 0.00001)))
        {
            if (!ps)
                draw_tick(dpy, gc, pixmap, Y, xi, org, size, fmt, nytick);
            else
                draw_tick_ps(dpy, gc, pixmap, Y, xi, org, size, fmt, nytick);
        }
    }

/* If log scale and data covers less then 2 decades, */
/* then re-logxy data and return flag to restart plot from beg */

    if ((logx && nxtick < 2) || (logy && nytick < 2))
    {
        if (logx && nxtick < 2) logx = 0;
        if (logy && nytick < 2) logy = 0;
        logxy_data(xy, nxy);
        return 1;
    }

    return 0;
}


/*===============================================================*/
void draw_tick(dpy, gc, pixmap, axis, x, org, size, fmt, ntick)
Display  *dpy;
GC        gc;
Pixmap    pixmap;
int       axis;
float     x;
int       org[];
float     size;
char     *fmt;
int       ntick;
{
int   tick_h;
char  str[20];
char  exp[8];
int   npix, x0, y0, x1, y1, x2, y2;
float offset;
int   j, k;
static int n;

    str[0] = '\0';
    exp[0] = '\0';
    offset = 0.0;
    x1 = 0;
    y1 = 0;
    if (ntick == 0)
    {
        sprintf(str, fmt, x);
    }
    else if ((logx && axis == X) || (logy && axis == Y))
    {
        sprintf(str, "10");
        sprintf(exp,  "%d", (int)ten(pow(10.0,x)));
        offset = 1.2;
        if (axis == Y) offset += 0.5;
    }

/* Find string length */

    npix = (int)(size * (float)(strlen(str)+offset));

/* If x axis and string too long, skip 1 tick every 2 tick */

    if (axis == X)
    {
        if (0)  printf("npix  %d tick %d\n", npix, (int)(Xtick*Xfact));
        if (npix > (int)(1.2*Xtick*Xfact) && ((++n+2)%2)) return;
    }

/* Calculate strings coordinates in pixel units */

    if (axis == X) {
        x1 = org[0] - (int)(0.4 * npix);
        y1 = org[1] - (int)(2.0 * size);
    }
    else if (axis == Y) {
        x1 = org[0] - (int)(0.9 * npix);
        y1 = org[1] - (int)(0.5 * size);
    }
    x2 = x1 + (int)(0.65 * size * (float)strlen(str));
    y2 = y1 + (int)(0.5 * size);

if (0) printf("---- draw_tick: %s size=%.2f xpix0=%d ypix0=%d\n",str, size, x1, y1);
    draw_string(dpy, gc, pixmap, str,     size, x1, y1, 0);
    draw_string(dpy, gc, pixmap, exp, 0.7*size, x2, y2, 0); 

    x0 =  org[0];
    y0 = -org[1]+H;
    if (axis == X)
    {
        tick_h = 0.025 * (H * (1.0-2.0*Ymargin));
        XDrawLine(dpy, pixmap, gc,  x0, y0, x0, y0-tick_h);
        k = (Yf[1]-Yf[0])*Yfact / 5;
        for (j=0;j<k;j++)
        {
            XDrawLine(dpy, pixmap, gc, x0, y0, x0, y0-1);
            y0 -= 5;
        }
    }
    else
    {
        tick_h = 0.025 * (W * (1.0-2.0*Xmargin));
        XDrawLine(dpy, pixmap, gc, x0, y0, x0+tick_h, y0);
        k  = (Xf[1]-Xf[0])*Xfact / 5;
        for (j=0;j<k;j++)
        {
           XDrawLine(dpy, pixmap, gc, x0, y0, x0+1, y0);
           x0 += 5;
        }
    }
}


/*===============================================================*
    input:
           lim   min-max range
           tick  interval between ticks
    output:
           fmt   format (string)
 *===============================================================*/
void xy_num_fmt(lim, tick, fmt)
float *lim, tick;
char  *fmt;
{
double v;
double d1, d2;
int i, size, ntix, nfld, ndp;
static int debug = 0;

/* find number of significant figures in tick value */

    size = ten(max(fabs(lim[1]), fabs(lim[0])));
/*
    v    = (int)((fabs((double)tick)/pow(10.0, size-5)  + 0.5));
*/
    d1 = fabs((double)tick);
    d2 = pow(10.0, (double)(size-5));
    v =  (double) ( (int) (d1/d2 + 0.5) );
if (debug) printf("num_fmt: d1=%f d2=%f v=%f\n", d1, d2, v);

    ntix = size - 5;
    for (i=0; i<5; i++)
    {
      if (mod((int)(v / pow(10.0, (double)i) + 0.5), 10.0) != 0.0) break;
      ntix = ntix + 1;
    }
    i = max(size, -ntix);
    nfld = 3 + max(i, size-ntix);

    if (nfld <= 7) {
        ndp = max(0, -ntix);
        sprintf(fmt, "%c%1d.%1df", '%', nfld, ndp);
    }
    else {
        ndp  = size - ntix + 1;
        nfld = 7 + ndp;
        sprintf(fmt, "%c.%1dE", '%', ndp);
    }
if (debug) printf("num_fmt: size=%d v=%f nfld=%d ndp=%d fmt=%s\n",
    size,v,nfld,ndp,fmt);
}


/*===============================================================*/
void draw_curve_line(dpy, gc, pixmap, xy, nxy)
Display  *dpy;
GC       gc;
Pixmap   pixmap;
XY       *xy;
int      nxy;
{
int    i, x, y, cx, cy;
int    xm, xM;

    cx = cy = 0;
    xm = Xfofs;
    xM = (int)(W*(1.0-Xmargin));
/*
dat2pix(xy[nxy-1].x, xy[nxy-1].y, &x, &y);
printf("== xd=%.3f --> x=%d (%d %d)\n", xy[nxy-1].x, x, xm, xM);
*/
    for (i=0;i<nxy;i++)
    {
        if (xy[i].x >= BIG) {cx=cy=0; continue;}
        dat2pix(xy[i].x, xy[i].y, &x, &y);

/* Discard data outside of x range */

        if(x < xm)
        {
/*            printf("== draw_curve %d < %d\n", x, xm); */
            cx=cy=0; continue;
        }
        if(x > xM)
        {
/*            printf("== draw_curve %d > %d\n", x, xM); */
            cx=cy=0; continue;
        }

        if (i == 0 || cx==0 ||cy==0 ) { cx = x; cy = y; }
        XDrawLine(dpy, pixmap, gc,  cx, cy, x, y);
        cx = x; cy = y;
    }
}

/*===============================================================*/
void draw_curve_sq1(dpy, gc, pixmap, xy, nxy)
Display  *dpy;
GC       gc;
Pixmap   pixmap;
XY       *xy;
int      nxy;
{
int    i, x, y;
int    xm, xM;

    xm = Xfofs;
    xM = (int)(W*(1.0-Xmargin));

    for (i=0;i<nxy;i++)
    {
        if (xy[i].x >= BIG) continue;
        dat2pix(xy[i].x, xy[i].y, &x, &y);

/* Discard data outside of x range */

        if(x < xm) continue;
        if(x > xM) continue;
        draw_box(dpy, gc, pixmap,x,y,x+1,y+1);
    }
}


/*===============================================================*/
void draw_curve_sq3(dpy, gc, pixmap, xy, nxy)
Display  *dpy;
GC       gc;
Pixmap   pixmap;
XY       *xy;
int      nxy;
{
int    i, x, y;
int    xm, xM;

    xm = Xfofs;
    xM = (int)(W*(1.0-Xmargin));

    for (i=0;i<nxy;i++)
    {
        if (xy[i].x >= BIG) continue;
        dat2pix(xy[i].x, xy[i].y, &x, &y);

/* Discard data outside of x range */

        if(x < xm) continue;
        if(x > xM) continue;
        draw_box(dpy, gc, pixmap,x-1,y-1,x+2,y+2);
    }
}


/*===============================================================*/
void dat2pix(xdat, ydat, xpix, ypix)
float xdat;
float ydat;
int *xpix;
int *ypix;
{
        *xpix =    (int) ((xdat - Xf[0]) * Xfact) + Xfofs;
        *ypix = - ((int) ((ydat - Yf[0]) * Yfact) + Yfofs) + H;
}


/*===============================================================*/
void draw_box(dpy,gc,pixmap,x0,y0,x1,y1)
Display  *dpy;
GC        gc;
Pixmap    pixmap;
int x0,y0,x1,y1;
{
        XDrawLine(dpy, pixmap, gc, x0,y0, x1,y0);
        XDrawLine(dpy, pixmap, gc, x1,y0, x1,y1);
        XDrawLine(dpy, pixmap, gc, x1,y1, x0,y1);
        XDrawLine(dpy, pixmap, gc, x0,y1, x0,y0);
}


/*===============================================================
  title parser
 *=============================================================*/
void draw_title(dpy, gc, pixmap, s, ps)
Display  *dpy;
GC        gc;
Pixmap    pixmap;
char     *s;
int       ps;
{
int i = 0;
float size;

    size = ((size=0.015 * (float)W) > 12.0) ? 12.0 : size ;

    while (strncmp(&s[i], "title:", 6))
    {
        if (!s[i]) break;
        ++i;
    }
    if (!strncmp(&s[i], "title:", 6))
    {
        draw_string(dpy, gc, pixmap, &s[i+6], size, (int) (0.14*W), (int) (0.95*H), ps);
    }
}

/*===============================================================
  Notes parser
 *=============================================================*/
void draw_notes(dpy, gc, pixmap, s)
Display  *dpy;
GC        gc;
Pixmap    pixmap;
char     *s;
{
static char *str = NULL;
static int first = 1;
static int note_num;
int i, j, l;
static char notes[MAXNOTES][NOTELEN+1];

    if (first) { str = s; first = 0; }
    else goto draw;

    if (str == NULL) return;
    i = 0;
    l = sizeof("note:") - 1;

    for (note_num=0; note_num < MAXNOTES; note_num++) {
        
        while (strncmp(&str[i], "note:", l)) {
            if (!str[i]) break;
            ++i;
        }

        i += l;
        for (j=0;j<(NOTELEN);j++,i++) { 
            if (!str[i] || !strncmp(&str[i], "note:", l)) break;
            notes[note_num][j] = str[i];
        }
        notes[note_num][j] = 0;
    }
draw:
    for (i=0;i<note_num;i++)
        draw_note(dpy, gc, pixmap, notes[i]);
}


/*===============================================================*/
void draw_note(dpy, gc, pixmap, note)
Display  *dpy;
GC        gc;
Pixmap    pixmap;
char  *note;
{
int   i, n;
float x, y, size;
int   xpix, ypix;
char  *s;
float xy[2];
XY  xy_[2];

    s = note;
    if (!strlen(s)) return;

/* Strip off leading spaces */
    i = 0;
    while (s[i] == ' ') ++i;

/* Get note coordinates and size */
    for (n=0;n<3;n++) {
        if (n == 0) if (sscanf(&s[i], "%f", &x) != 1) return;
        if (n == 1) if (sscanf(&s[i], "%f", &y) != 1) return;
        if (n == 2) if (sscanf(&s[i], "%f", &size) != 1) return;
        while (s[i] && s[++i] != ' ');
        while (s[i] && s[++i] == ' ');
    }
    if (!strlen(&s[i])) return;

/* Convert coords values according to logxy */
    xy[0] = x;
    xy[1] = y;
    xy_[0].x = x;
    xy_[0].y = y;

 printf("draw_note: %E %E\n", xy_[0].x,xy_[0].y);
    logxy_data(xy_, 1);
 printf("draw_note: %E %E\n", xy_[0].x,xy_[0].y);


/* Express coords into pix values */
    xpix = (xy[0]-Xf[0])*Xfact + Xfofs;
    ypix = (xy[1]-Yf[0])*Yfact + Yfofs;
/*
 printf("draw_note: '%s' at %d %d\n", &s[i], xpix, ypix);
*/
    draw_string(dpy, gc, pixmap, &s[i], size, xpix, ypix, 0);
}

/*===============================================================*/
void check_str_len(bin_val, nbin_val, k, len)
float *bin_val;
int   nbin_val;
float *k;
int   len;
{
char  val[100];
int   i, n, npix;
float k1 = *k;

    for (i=0;i<nbin_val;i++) {
        sprintf(val, "%.1f", bin_val[i]);
        n = strlen(val);
        while (1) {
            npix = (float)(n+2)*k1*(float)W;
            if (npix >= len) k1 *= 0.9;
            else break;
        }
    }
    *k = k1;
}


/*===============================================================*/
void draw_string(dpy, gc, pixmap, s, size, start_x, y, ps)
Display  *dpy;
GC        gc;
Pixmap    pixmap;
char     *s;
float     size;
int       start_x;
int       y;
int       ps;
{
int    i, j, k, nc, len;
int    x, spacing;
float  w_fact, h_fact;
char   *pc, *data, c;
int    nxy;
char   xx, xmin, xmax;
static int debug = 0;

    spacing = size * 0.8;
/*
    h_fact  = size / CHAR_H_NPIX;
    w_fact  = h_fact * 0.8;
*/
    h_fact  = (2.0*size) / CHAR_H_NPIX;
    w_fact  = h_fact * 0.8;

    len = strlen(s);

if (debug > 0) printf("string: '%s' %d size=%4.1f\n", s,len,size);

    nc = 0;
    pc = NULL;
    x  = start_x;
    for (i=0;i<len;i++) {
        c = s[i];
        if      (c == ' ')             { x+=spacing; continue; }
        else if (c >= '0' && c <= '9') { pc = char_1[c-'0']; }
        else if (c >= 'a' && c <= 'z') { pc = char_1[c-'a'+10]; }
        else if (c >= 'A' && c <= 'Z') { pc = char_2[c-'A']; }
        else if (c == '\\')    { lookup_greek(s, &i, &pc); }
        else                   { lookup_char(c, &pc); }
        if (pc == NULL)        { x+=spacing; continue; }

        data = pc;
        nxy = data[0];
        ++data;
        xmax = -127;
        xmin =  127;
        for (k=0,j=0;k<nxy;k++,j+=2) {
            xx = data[j];
            if (xx < xmin)             xmin = xx;
            if (xx < 127 && xx > xmax) xmax = xx;
        }

/*        x = start_x + nc++ * spacing; */

if (debug > 1) printf("string: '%c' %3d %3d %d %d\n",c,x,y,xmin,xmax);

        if (!ps)
            draw_char(dpy, gc, pixmap, pc, w_fact, h_fact, x, y);
        else
            draw_char_ps(pc, w_fact, h_fact, x, y);
        x += (int)(size*0.4) + (int)(size*0.008*(xmax-xmin));
    }
}

/*===============================================================*/
void draw_char(dpy, gc, pixmap, data, w_fact, h_fact, xofs, yofs)
Display  *dpy;
GC       gc;
Pixmap   pixmap;
char     *data;
float    w_fact;
float    h_fact;
int      xofs;
int      yofs;
{
int     nxy, i, j, x, y, cx, cy;
float   it_fact;
XY      xy[1000];

    cx = cy = 0;
    it_fact = 0.5;

/* First value in array is num of data; get it and increment pointer */

    nxy = data[0];
    ++data;

    for (i=0,j=0;i<nxy;i++,j+=2)
    {
        if (data[j] == 127 && data[j+1] == 127)
        {
            xy[i].x = BIG;
            xy[i].y = BIG;
            continue;
        }
        xy[i].x = (float)data[j+0] * w_fact;
        xy[i].y = (float)data[j+1] * h_fact;
    }

    for (i=0,j=0;i<nxy;i++,j+=2)
    {
        if (xy[i+0].x >= BIG) {cx=cy=0; continue;}
        x = ((int)(xy[i].x*it_fact) + xofs);
        y =-((int)(xy[i].y*it_fact) + yofs) + H;
/*
printf("%f %f %d %d\n", xy[i].x, xy[i].y, x, y);
*/
        if (i == 0 || cx==0 ||cy==0 ) { cx = x; cy = y; }
        XDrawLine(dpy, pixmap, gc,  cx, cy, x, y);
        cx = x; cy = y;
    }
}


/*===============================================================
 *  Expand scale. Calls no other routines
 *  log    is  0  for linear scales  1  for log scales.
 *  range  on input: min-max of data, on output: range to be used.
 *  logout revised specification for log scales 
 *         if lower limit or least data value is nonpositive.
 *  Upper and lower limits equal or almost so: expand by+-10%
 *===============================================================*/
int expand(log, range)
int   log;
float *range;
{
static float xmarg, dxl, rxl;
int logout;


    if (range[1] - range[0] <= (3.0e-6*(range[1]+range[0]))) {
	range[1] += 0.1 * fabs(range[1]);
	range[0] -= 0.1 * fabs(range[0]);
	if (range[0] == range[1]) range[1] = 1.0;
    }

    xmarg = (float).05;

/*  Linear axis expanded */

    if (log == 0 || range[0] <= 0.0) {
        logout = 0;
        dxl = xmarg * (range[1] - range[0]);
        range[0] -= dxl;
        range[1] += dxl;
        return logout;
    }

/*  Log axes expanded */

    logout = 1;
    rxl = pow((double) (range[1] / range[0]), (double) xmarg);
    range[0] /= rxl;
    range[1] *= rxl;
    return logout;
}

