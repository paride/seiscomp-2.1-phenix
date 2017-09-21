/*======================================================================
    libps.c  PostScript lib
 *=====================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <X11/Xlib.h>
#include "xplot.h"
#include "proto.h"

#ifdef ANSI_C
static void ragbag(int);
static void remark(char*);
static void plot_ps(float, float, int);
static void newpen(int);
static void draw_curve_ps_line(XY*, int);
static void draw_curve_ps_sq1(XY*, int);
#else
static void ragbag();
static void remark();
static void plot_ps();
static void newpen();
static void draw_curve_ps_line();
static void draw_curve_ps_sq1();
#endif


#define mvor -3
#define iup   3
#define idn   2
#define kolor 1

extern char     *char_1[], *char_2[], *char_3[], *char_4[];
extern int      logxy, logx, logy, prev_logx, prev_logy;
extern int      Wind_width, Wind_height;
extern unsigned int  W, H;
extern int      Xfofs, Yfofs;
extern float    Xf[2], Yf[2], Xtick, Ytick;
extern double   Xfact, Yfact;
extern int      BegYear;
extern float    Xmargin, Ymargin;

static FILE     *F_ps;
char            psname[255];



/*===============================================================*/
void plot_PS(dpy, gc, pixmap, xy1, nxy1, xy2, nxy2, notes, mode)
Display  *dpy;
GC        gc;
Pixmap    pixmap;
XY *xy1;
int nxy1;
XY *xy2;
int nxy2;
char *notes;
int mode;
{
float  size;
char   xlabel[255];

/* Open PS file */

    if (!(F_ps = fopen(psname, "w")))
    {
        fprintf(stderr,"ERROR: plot_PS: can't open file %s\n", psname);
        exit(1);
    }
    if (mode != 0 && mode != 1)
    {
        fprintf(stderr,"ERROR: plot_PS: unsupported mode: %d \n", mode);
        exit(1);
    }

    ragbag(2);
/* Set PS origin coordinates  */
    plot_ps(0., 125., mvor);
    newpen(0);

    remark("Plotting frame\n");
    draw_frame(dpy, gc, pixmap, xy1, nxy1, 1);
    remark("Plotting data series 1\n");
    printf("== Plotting data series 1 to PS file '%s'\n", psname);
    if (mode == 0)
    {
        draw_curve_ps_line(xy1, nxy1);
    }
    else
    {
        draw_curve_ps_sq1(xy1, nxy1);
    }
    if (nxy2)
    {
        remark("Plotting data series 2\n");
        printf("== Plotting data series 2 to PS file '%s'\n", psname);
        draw_curve_ps_line(xy2, nxy2);
    }
    draw_title(dpy, gc, pixmap, notes, 1);

    xlabel[0] = '\0';
    size = 0.015 * (float)W;
    if (size > 12.0) size = 12.0;
    draw_string(dpy,gc,pixmap, xlabel, size, (int)(0.33*W), (int)(0.05*H),1);

/* Finish and close PS file */
    ragbag(-1);
}

/*=========================================================*/
static void ragbag(k)
int k;
{

    if (k == 2)
    {

/* Start output ragbag(k=2, ?) from "command plot" */

/*
    fprintf(F_ps,"%c! PostScript file\n",'%');
*/
    fprintf(F_ps,"%c!PS\n",'%');
    fprintf(F_ps,"/k {currentpoint stroke moveto newpath moveto} def\n");
    fprintf(F_ps,"/l {lineto} def\n");
    fprintf(F_ps,"/m {newpath moveto} def\n");
    fprintf(F_ps,"/n {lineto currentpoint stroke moveto} def\n");
    fprintf(F_ps,"/f {closepath fill 0 0 moveto 0 setgray} def\n");
    fprintf(F_ps,"0.072 0.072 scale %c Coords are 1/1000 th inch\n",'%');
    fprintf(F_ps,"2 setlinejoin 4 setlinewidth 1 setlinecap\n");
    fprintf(F_ps,"%c Uncomment next 2 lines for light blue background\n",'%');
    fprintf(F_ps,"%c 0.67 0.2 1.0 sethsbcolor %c Light blue\n",'%','%');
    fprintf(F_ps,"%c 0 0 m 9000 0 l 9000 11000 l 0 11000 l closepath fill\n",'%');
    }
    else if (k < 0)
    {
/* Finish */
        fprintf(F_ps,"currentpoint n\n");
        fprintf(F_ps,"showpage\n");
        fprintf(F_ps,"%c!PS-Adobe-2.0 EPSF-1.2\n",'%');
        fprintf(F_ps,"%c%cBoundingBox:    18    18   382   454\n",'%','%');
        fclose(F_ps);
    }
}

/*=========================================================*/
static void remark(str)
char *str;
{
    fprintf(F_ps,"%c%s", '%', str);
}


/*=========================================================*/
static void plot_ps(x, y, pen)
float x, y;
int pen;
{
static int moves = 0;
int ix, iy;

    ix = x * 15.;
    iy = y * 15.;
if (0) printf("== %d %d pen %d\n", ix, iy, pen);

    if (pen == idn)
    {
        if (moves <= 1000)
        {
           fprintf(F_ps," %d %d l\n", ix, iy);
           ++moves;
        }
        else
        {
           fprintf(F_ps," %d %d n\n", ix, iy);   
           moves = 0;
        }
    }
    else if (pen == iup)
    {
        if (moves > 0)
        {
           fprintf(F_ps," %d %d k\n", ix, iy);
           moves = 0;
        }
        else
           fprintf(F_ps," %d %d m\n", ix, iy);
    }
    else if (pen < 0)
    {
       fprintf(F_ps," %d %d translate 0 0 moveto\n", ix, iy);
    }
    else if (pen == 0)
    {
        fprintf(F_ps," %d %d moveto currentpoint translate showpage\n", ix, iy);
    }
    else
    {
        printf("== plot_ps: Unintelligible pen command\n");
    }
}

/*=========================================================*/
static void newpen(pen)
int pen;
{
static int lastpen = -9;
int wide;
    
    if (pen == lastpen) return;

    wide = pen / 1000;
    wide = 4;
    fprintf(F_ps," currentpoint k\n");
    fprintf(F_ps," %d setlinewidth\n",wide);
    fprintf(F_ps,"   0.000   0.000   0.000 sethsbcolor\n");
    lastpen = pen;
}


/*===============================================================*/
static void draw_curve_ps_line(xy, nxy)
XY     *xy;
int    nxy;
{
int    i;
float  xx, yy;

    xx = (xy[0].x - Xf[0]) * Xfact + Xfofs;
    yy = (xy[0].y - Yf[0]) * Yfact + Yfofs;
    plot_ps(xx, yy, iup);
    for (i=0;i<nxy;i++)
    {
        if (xy[i].x >= BIG)
        {
            xx = (xy[i+1].x - Xf[0]) * Xfact + Xfofs;
            yy = (xy[i+1].y - Yf[0]) * Yfact + Yfofs;
            plot_ps(xx, yy, iup);
            continue;
        }
        xx = (xy[i].x - Xf[0]) * Xfact + Xfofs;
        yy = (xy[i].y - Yf[0]) * Yfact + Yfofs;
        plot_ps(xx, yy, idn);
    }
    fflush(F_ps);
}


/*===============================================================*/
static void draw_curve_ps_sq1(xy, nxy)
XY     *xy;
int    nxy;
{
int    i;
int    x, y;
float  xx, yy;

    xx = (xy[0].x - Xf[0]) * Xfact + Xfofs;
    yy = (xy[0].y - Yf[0]) * Yfact + Yfofs;
    plot_ps(xx, yy, iup);
    for (i=0;i<nxy;i++)
    {
        if (xy[i].x >= BIG)
        {
            xx = (xy[i+1].x - Xf[0]) * Xfact + Xfofs;
            yy = (xy[i+1].y - Yf[0]) * Yfact + Yfofs;
            plot_ps(xx, yy, iup);
            continue;
        }
        xx = (xy[i].x - Xf[0]) * Xfact + Xfofs;
        yy = (xy[i].y - Yf[0]) * Yfact + Yfofs;
        x = (int)xx, y = (int)yy;
        draw_box_ps(x+1, y+1, x-1, y-1);
    }
    fflush(F_ps);
}

/*===============================================================*/
void draw_box_ps(x0,y0,x1,y1)
int x0;
int y0;
int x1;
int y1;
{
float xx0 = x0;
float yy0 = y0;
float xx1 = x1;
float yy1 = y1;

    plot_ps(xx0, yy0, iup);
    plot_ps(xx0, yy1, idn);
    plot_ps(xx1, yy1, idn);
    plot_ps(xx1, yy0, idn);
    plot_ps(xx0, yy0, idn);
}


/*===============================================================*/
void draw_char_ps(data, w_fact, h_fact, xofs, yofs)
char   *data;
float  w_fact, h_fact;
int    xofs, yofs;
{
int     nxy, i, j;
float   it_fact;
XY      xy[1000];

    it_fact = 0.5;

/* First value in array is num of data; get it and increment pointer */

    nxy = data[0];
    ++data;

    for (i=0,j=0;i<nxy;i++,j+=2)
    {
        if (data[j] == 127 && data[j+1] == 127)
        {
            xy[i].x = (float)data[j+2+0] * w_fact * it_fact + xofs;
            xy[i].y = (float)data[j+2+1] * h_fact * it_fact + yofs;
            plot_ps(xy[i].x, xy[i].y, iup);
            continue;
        }
        xy[i].x = (float)data[j+0] * w_fact * it_fact + xofs;
        xy[i].y = (float)data[j+1] * h_fact * it_fact + yofs;
        plot_ps(xy[i].x, xy[i].y, idn);
    }
}

/*===============================================================*/
void draw_tick_ps(dpy, gc, pixmap, axis, x, org, size, fmt, ntick)
Display  *dpy;
GC        gc;
Pixmap    pixmap;
int    axis;
float  x;
int    org[];
float  size;
char  *fmt;
int    ntick;
{
float tick_h;
char  str[20];
char  exp[8];
int   npix, x1, y1, x2, y2;
float xx0, yy0;
float offset;
int   j, k;
static int n;

    str[0] = '\0';
    exp[0] = '\0';
    offset = 0.0;
    x1 = y1 = 0;
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

    npix = (int)(size * (float)(strlen(str)+offset));

/* If x axis and string too long, skip 1 tick every 2 tick */

    if (axis == X)
    {
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

    draw_string(dpy, gc, pixmap, str, size, x1, y1, 1);
    draw_string(dpy, gc, pixmap, exp, 0.7*size, x2, y2, 1); 

    xx0 = org[0];
    yy0 = org[1];
    if (axis == X)
    {
        tick_h = 0.025 * ((float)H * (1.0-2.0*Ymargin));

        plot_ps(xx0, yy0, iup);
        plot_ps(xx0, (yy0+tick_h), idn);
        k = (Yf[1]-Yf[0])*Yfact / 5 + 1;

/* Horizontal dash lines */
        for (j=0;j<k;j++)
        {
            plot_ps(xx0, yy0, iup);
            plot_ps(xx0, yy0+0.01, idn);
            yy0 += 5.0;
        }
    }
    else
    {
        tick_h = 0.025 * ((float)W * (1.0-2.0*Xmargin));
        plot_ps(xx0, yy0, iup);
        plot_ps(xx0+tick_h, yy0, idn);

/* Vertical dash lines */
        k  = (Xf[1]-Xf[0])*Xfact / 5 + 1;
        for (j=0;j<k;j++)
        {
            plot_ps(xx0, yy0, iup);
            plot_ps(xx0+0.01, yy0, idn);
            xx0 += 5.0;
        }
    }
}

