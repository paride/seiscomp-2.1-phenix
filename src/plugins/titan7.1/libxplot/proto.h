/*=========================================================================
        proto.h
 *========================================================================*/
/*
#ifndef _proto_h
#define _proto_h
*/

#include "xplot.h"

#ifdef ANSI_C

/* libplot.c */
void plot       (XY*, int, int, char*, int);
int draw_picture(Display*, GC, Pixmap, XY*, int, char*);
int draw_frame  (Display*, GC, Pixmap, XY*, int, int);
void draw_tick  (Display*, GC, Pixmap, int, float, int[], float, char*, int);
void draw_curve_line (Display*, GC, Pixmap, XY*, int);
void draw_curve_sq1  (Display*, GC, Pixmap, XY*, int);
void draw_curve_sq3  (Display*, GC, Pixmap, XY*, int);
void draw_box   (Display*, GC, Pixmap, int, int, int, int);
void draw_notes (Display*, GC, Pixmap, char*);
void draw_note  (Display*, GC, Pixmap, char*);
void draw_title (Display*, GC, Pixmap, char*, int);
void draw_string(Display*, GC, Pixmap, char*, float, int, int, int);
void draw_char  (Display*, GC, Pixmap, char*, float, float, int, int);
void dat2pix    (float, float, int *, int *);
void xy_num_fmt (float *, float, char*);
int expand      (int, float*);


/* libps.c */
void plot_PS      (Display*, GC, Pixmap, XY*, int, XY*, int, char*, int);
void draw_box_ps  (int,int,int,int);
void draw_char_ps (char *, float, float, int, int);
void draw_tick_ps (Display*, GC, Pixmap, int, float, int[], float, char*, int);

/* libxplot1.c */
void mainloop        (XY *, int, char*);
void display1        (XY *, int, int);
void GetWinParms1    (Display**, Window*, GC*, Pixmap*);

/* libxplot2.c */
void preset2         ();
void display2        (XY *, int, int);
void GetWinParms2    (Display**, Window*, GC*, Pixmap*);

/* libcmd.c */
void pix2dat           (int, int, float *, float *);
void pick              (Display*, Window, GC, Pixmap, int, int, int, int);
void show_zoom_lim     (Display*, Window, GC, Pixmap, int, int);
void do_zoom           ();
void zoom2             ();
int  get_zoom_data     (XY *, int, XY **, int*);
void edit              (char *);
void smo               (char *);
void fit               (Display*, Window, GC, Pixmap, char*);
void select_data       (int);
void write_data        (XY *, int);
void malloc_xy         (XY **, int,  char*);
void free_xy           (XY *, char*);

/* libutil.c */
int    getyear       (double);
double dbtime2dbday  (double, int);
double dbtime2dbday_ (double);
double dbday2dbtime  (double, int);
int  str2utime1      (char *, double *);
int  str2utime2      (char *, char *, double *);
int  str2utime3      (char *, double *);
int  str2utime4      (char *, double *);
void get_dbltime1    (char *, char *, double *);
void get_dbltime2    (char *, double *);
void get_dbltime3    (char *, double *);
void get_dbltime4    (char *, double *);
void get_secs        (double, double *);
void time_asc0       (char *, char *, double);
void time_asc1       (char *, double);
void time_asc2       (char *, double);
void time_asc3       (char *, double);
void time_asc4       (char *, double);
long open_Frd        (char *, FILE **);
long open_Frw        (char *, FILE **);
long open_Frd_noex   (char *, FILE **);
long open_Fapp       (char *, FILE **);
void open_Fwr        (char *, FILE **);
long fcreat_append   (char *, FILE **);
int  getline         (FILE *, char *);
int  sparse          (char *, char **, char *, int);
int  trim            (char *);
void ucase           (char *);
void lcase           (char *);
void sort_table      (char *, int);

/* libfilt.c */
void do_filter       (char*);

/* util.c */
void filter          (XY*, int, float, float);
void ciir            (float, float, float, float*, int, float*);
void sortxy          (XY *, int);
void min_max_xy      (XY *, int, float*, float*, float*, float*, float*);
void logxy_data      (XY *, int);
void lookup_char     (char, char**);
void lookup_greek    (char*, int*, char**);
void nicer           (float*, float, float*, float*);


/* libgif.c */
void plot_GIF(Display*, Window, GC);

/* get.c */
int getImage();
int getRectangle(XRectangle*);

/* xvgifwr.c */
int WriteGIF();

#else

/* libplot.c */
void plot       ();
void draw_curve_line ();
void draw_curve_sq1  ();
void draw_curve_sq3  ();
int draw_frame  ();
void draw_tick  ();
void draw_curve ();
void draw_box   ();
void draw_notes ();
void draw_note  ();
void draw_title ();
void draw_string();
void draw_char  ();
void dat2pix    ();
void xy_num_fmt ();
int expand      ();

/* libps.c */
void plot_PS      ();
void draw_box_ps  ();
void draw_char_ps ();
void draw_tick_ps ();

/* libxplot1.c */
void mainloop        ();
void display1        ();
void GetWinParms1    ();

/* libxplot2.c */
void preset2         ();
void display2        ();
void GetWinParms2    ();

/* libcmd.c */
void pix2dat           ();
void pick              ();
void show_zoom_lim     ();
void do_zoom           ();
void zoom2             ();
int  get_zoom_data     ();
void edit              ();
void smo               ();
void fit               ();
void select_data       ();
void write_data        ();
void malloc_xy         ();
void free_xy           ();
double get_smoothed_dt ();
double calc_delta_t    ();


/* libutil.c */
int    getyear       ();
double dbtime2dbday  ();
double dbtime2dbday_ ();
double dbday2dbtime  ();
int  str2utime1      ();
int  str2utime2      ();
int  str2utime3      ();
int  str2utime4      ();
void get_dbltime1    ();
void get_dbltime2    ();
void get_dbltime3    ();
void get_dbltime4    ();
void get_secs        ();
void time_asc0       ();
void time_asc1       ();
void time_asc2       ();
void time_asc3       ();
void time_asc4       ();
long open_Frd        ();
long open_Frw        ();
long open_Frd_noex   ();
long open_Fapp       ();
void open_Fwr        ();
long fcreat_append   ();
int  getline         ();
int  sparse          ();
int  trim            ();
void ucase           ();
void lcase           ();
void sort_table      ();

/* libfilt.c */
void do_filter       ();

/* util.c */
void filter          ();
void ciir            ();
void sortxy          ();
void min_max_xy      ();
void logxy_data      ();
void lookup_char     ();
void lookup_greek    ();
void nicer           ();

/* get.c */
int getImage();
int getRectangle();

/* xvgifwr.c */
int WriteGIF();

#endif
/* #endif */
