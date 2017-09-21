/*======================================================================
    Program xdft.c

    Author: J.-F. Fels, OMP, Toulouse

    Purposes
    - Display the delta_t variation of a Titan file on X window
    - Clean the observed data by removing aberrant points and
      write the result back to disk.
    - An alternate method is to extract selected "clean" data 
      segments which are written to disk.
    - smooth the observed data.

*======================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "titan.h"
#include "proto.h"
#include "libxplot/xplot.h"
#include "libxplot/proto.h"

#ifdef ANSI_C
extern int read_top  (char*, Top**);
#else
extern int read_top  ();
#endif


char       Station[5];
int        verb;
int        BegYear;
double     BegTime;
double     EndTime;
double     BegDay;
Tcoef      time_coef;
int        ncoef;
int        dballoc;

int          WH = 0;
extern float Xmargin, Ymargin;


/*==================================================================*/
int main(argc, argv)
int    argc;
char **argv;
{
static XY *xy = NULL;
int       nxy;
int      i;
int      disable_stdin;
char     title[255];
Top      *top;
int      add_day = 0;

    ncoef = 3;
    WH = 500;
    Xmargin = 0.15;
    Ymargin = 0.20;

    if (argc < 3)
    {
help:
printf("\n");
printf("                    TIME DRIFT VIEWER   xdft Rev %s\n", REVISION);
printf("                       J.-F. Fels, OMP Toulouse\n"); 
printf("\n");
printf("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
printf("NEW NEW NEW  The way to select section of image has changed.\n");
printf("             See internal menu options in xdft window.\n");
printf("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
printf("\n");
printf("Usage:\n");
printf("    xdft sta filename [size=n]\n");
printf("\n");
printf("Input:\n");
printf("    - ascii  --> .dt file or .dft file\n");
printf("    - binary --> .top file\n");
printf("\n");
printf("Output:\n");
printf("    - nothing, or selected data (.dt), or smoothed data (.dft).\n");
printf("    - PostScript or GIF files.\n");
printf("\n");
printf("On xdft window, enter \"h\" to get help.\n");
printf("\n");
printf("Examples:\n");
printf("    xdft STA 1998.07.01-13.46.04.XXX.dt\n");
printf("    xdft al1 ../derives/AL1.tot size=600   --> window width = 600 pixel\n");
printf("\n");
exit(1);
    }

    for (i=3; i<argc; i++)
    {
        if      (!strncmp(argv[i],"size=",5)) WH      = atoi(&argv[i][5]);
        else if (!strncmp(argv[i],"-dba", 4)) dballoc = atoi(&argv[i][1]);
        else if (!strncmp(argv[i],"+",    1)) add_day = atoi(&argv[i][1]);
        else if (!strncmp(argv[i],"nc=",  3)) ncoef = atol(&argv[i][3]);
        else
        {
            fprintf(stderr,"\n\txdft: unsupported option '%s'\n\n", argv[i]);
            goto help;
        }
    }

    sprintf(Station, argv[1]);
    ucase(Station);

/* Open and read delta time file */

    nxy = read_top(argv[2], &top);

    if (nxy == 0)
    {
        printf("xdft: no valid data in %s\n", argv[2]);
        exit(1);
    }

/* Check first term */

    if (!TIME_OK(top[0].imin) || !TIME_OK(top[0].sec))
    {
        printf("xdft: unexpected start time: imin=%d sec=%d\n",
                (int) top[i].imin, (int) top[0].sec);
        exit(1);
    }

/* Find beg, end time */

    BegTime = 1.0E35;
    EndTime = -1.0E35;
    for (i=0; i<nxy; i++)
    {
        if (top[i].sec > EndTime) EndTime = top[i].sec;
        if (top[i].sec < BegTime) BegTime = top[i].sec;
if(0)        printf("%d %.3f %.3f\n", i, top[i].sec, top[i].dt);
    }

/* Get current year */
    BegYear = getyear(BegTime);

/* Calculate beg Julian day (double) */
    BegDay =  dbtime2dbday(BegTime,BegYear);

/* Allocate memory for xy array */
    malloc_xy(&xy, nxy+1, "plot: xy");

/* Fill xy array; x units are Julian days since beg of year */
    for (i=0; i<nxy; i++)
    {
        xy[i].x = dbtime2dbday((double)top[i].sec,BegYear) +(double)add_day;
        xy[i].y = top[i].dt;
if(0)        printf("%d x=%.3f y=%.3f\n", i, xy[i].x, xy[i].y);
    }
    
    free(top);
    top = NULL;

/* Start xplot */

    disable_stdin = 1;
    sprintf(title, "title: sta %s %s", Station, argv[2]);
    plot(xy, nxy, 0, title, disable_stdin);
    free_xy(xy, "plot: xy");
    return 0;
}

