/***************************************************************************** 
 * slqplot.h
 *
 * Plot seismic traces
 *
 * (c) 1998, 2003 Andres Heinloo, GFZ Potsdam
 *
 * Modified: 2003.6.11 - Ported to be a SeedLink client, Chad
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

#ifndef SLQPLOT_H
#define SLQPLOT_H

#include <time.h>
#include <libslink.h>

#define SLQP_VERSION     "1.0 (2005.212)"

/* array sizes */

#define NCHANS  10
#define NPLOTS  10

/* display parameters */

#define XDISPSIZE  7800
#define YDISPSIZE  5000

#define XPLSIZE    6750
#define XPLOFFS    700
#define XPAVERAGE  500
#define YPLSIZE    4500
#define YPLOFFS    475 

#define FNTNAME_NORMAL  "HersheySerif"
#define FNTNAME_BOLD    "HersheySerif-Bold"
#define FNTSIZE         75

/* data format parameters */

#define PACKET_SIZE  512
#define HEADER_SIZE  64
#define MAX_SAMPLES  ((PACKET_SIZE - HEADER_SIZE) * 2)
#define DEFMAG       1000

#define SCHNAME  4
#define SLOCNAME 3
#define SSTNAME  6
#define FILNAME  32

#define DESCLINES   2
#define DESCLINELEN 100

#define FL_SHOW_PACKETS    0x01
#define FL_INTERACTIVE     0x02
#define FL_COMPLETE_PAGES  0x04
#define FL_COLOURED_TRACES 0x08

#define N(arg) do { if((arg) < 0) { perror(#arg); exit(1); } } while(0)
#define P(arg) do { if((arg) == NULL) { perror(#arg); exit(1); } } while(0)

struct ms_hdrdata
  {
    int packet;
    char station_id[6];
    char channel_id[4];
    struct tm hdrtime;
    int tenth_millisec;
    int time_correction;
    int nsamples;
    double sample_rate;
    int32_t data_diff;
    int data_consistency;
  };

struct c_channel_filter_struct
  {
     char filname[FILNAME];
     int nband;
     int fmag;
     float w1[5];
     float w2[3];
  };
   
struct c_channel_struct 
  {
    char chname[SCHNAME];
    char location[SLOCNAME];
    int mag;
    struct c_channel_filter_struct filter;
  };

extern int chcount, plcount, c_traces, c_tracelen, c_scroll_step;
extern unsigned int c_flags;
extern int wwsscount;
extern int c_verbosity;
extern char station_name[SSTNAME];
extern struct c_channel_struct c_channel[NCHANS];
extern char c_description[DESCLINES][DESCLINELEN];
extern const char *state_file;

void qpsetup(int argc, char **argv, SLCD *slconn);
void restore_state(const char *state_file, int *curseq);
void save_state(const char *state_file);
void redisplay(int *curseq);
void display(int c, struct ms_hdrdata *head, int32_t *data, int *curseq);
int  prm_plot(void *dest, char *src, int n);
void create_plotters(void);
void destroy_plotters(void);
void flush_plotters(void);
void closepage(void);
void newpage(const char *page_id);
void moveto(double x, double y);
void lineto(double x, double y);
void htext(double x, double y, int h, int v, const char *fname, int fsize,
  const char *s);
void vtext(double x, double y, int h, int v, const char *fname, int fsize,
  const char *s);
void setcolor(const char *name);
void sifil(int, float[], int,float[],float[],int,int,int);

#endif /* SLQPLOT_H */

