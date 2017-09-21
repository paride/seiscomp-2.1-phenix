/*====================================================================
 *         globals.h
 *==================================================================*/
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <values.h>
#include <malloc.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>


#ifndef MAIN
#define WHERE extern
#else
#define WHERE
#endif


#define TRUE            1
#define FALSE           0
#define ON              1
#define MAXCOLORS       256
#define BW              1

typedef int            boolean;   /* boolean data type */

struct internalpic {
    int wide, high;
    int numcolors;
    int cmap[3][MAXCOLORS];
    unsigned char *data;
};
typedef struct internalpic InternalPic;

typedef unsigned int  word;
typedef unsigned char byte;
typedef unsigned long dw;

#define MAX_CELLS  256*256

typedef struct {
  XImage   *ximage;
  Visual   *visual;
  int       depth;
  Colormap  colormap;
  word      numcells;
  word      red[MAX_CELLS],
            green[MAX_CELLS],
            blue[MAX_CELLS];
  byte      used[MAX_CELLS];
} imageInfo;


WHERE  int          DEBUG;        /* print debugging info */
WHERE  int          numcols;      /* # of desired colors in picture */
WHERE  int          ColorMapSize;
WHERE  int          ColorMap[3*MAXCOLORS];
WHERE  byte         r[MAXCOLORS],g[MAXCOLORS],b[MAXCOLORS];
WHERE  int          gifcolor[MAXCOLORS];
WHERE  int          iWIDE, iHIGH; /* size of theImage */
WHERE  int          eWIDE, eHIGH; /* size of expanded image */
WHERE  InternalPic  origpic;
WHERE  FILE         *fpout;
WHERE  char         *outgiffname;



#define CENTERX(f,x,str) ((x)-XTextWidth(f,str,strlen(str))/2)
#define CENTERY(f,y) ((y)-((f->ascent+f->descent)/2)+f->ascent)

WHERE Display       *theDisp;
WHERE int           theScreen;
WHERE Colormap      theCmap;
WHERE Window        rootW, mainW;
WHERE GC            theGC;
WHERE Font          mfont;
WHERE XFontStruct   *mfinfo;
WHERE Visual        *theVisual;
WHERE XImage        *theImage;
WHERE XImage        *expImage;
WHERE unsigned long *pixels;

