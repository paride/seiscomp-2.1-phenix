#ifndef _xplot_h
#define _xplot_h

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <ctype.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>    /* for XtMalloc */

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif
#ifndef SEEK_SET
#define SEEK_SET 0
#endif
#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif
#ifndef SEEK_END
#define SEEK_END 2
#endif
#ifndef TIME_OK
#define TIME_OK(t)   (((int) t > 631152000 && (int) t < 2000000000) ? 1:0)
#endif

#define   BLACK   0     /* rgb:0/0/0           */
#define   WHITE   1     /* rgb:ffff/ffff/ffff  */
#define   RED     2     /* rgb:ffff/0/0        */
#define   GREEN   3     /* rgb:0/ffff/0        */
#define   BLUE    4     /* rgb:0/0/ffff        */
#define   YELLOW  5     /* rgb:ffff/ffff/0     */
#define   MAGENTA 6     /* rgb:ffff/0/ffff     */
#define   CYAN    7     /* rgb:0/ffff/ffff     */
#define Ncolors 13
#define Nwidths 10
#define PS      1
#define EXTRACT 1
#define CLEAN   2

#define CHAR_H_NPIX (float) 100.0    /* max num of pix in char fonts */
#define MAXNOTES  10
#define NOTELEN   60
#define X 1
#define Y 2
#define BIG       3.e38
#define rnd(a,b) (( a/b - (int)(a/b) ) > 0.5 ? 1 : 0)
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))
#define mod(a,b) ((double)(a - ((long)((double)a / (double)b)) * (double)b))
#define ten(x) ((int)(500.001 + log10(x)) - 500)
#define leap_year(i) ((i % 4 == 0 && i % 100 != 0) || i % 400 == 0)

typedef struct
{
    int xi[2];
    int yi[2];
    float xd[2];
    float yd[2];
    int n;
    int on;
} BOX;

typedef struct {
        double  sec;
        int     imin;
        float   dt;
} Top;

typedef struct
{
    float x;
    float y;
} XY;

#define MAX_XY 30000

#define D (double) 86400.0
#define DAY 86400.0


#endif
