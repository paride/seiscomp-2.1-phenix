/*                                                           */
/* esstf_aux.h - declarations for esstf_aux.c and            */
/*               esstf_plugin.c                              */
/*                                                           */
/*               please edit esstf_config.h                  */
/*                                                           */
/* Version: 2002.119    -   Mathias Hoffmann                 */
/*                          AWI Bremerhaven                  */
/*                                                           */
/*      tested with:                                         */
/*      Solaris 2.6 (sparc) &  Linux 2.4.4 (intel)           */

#ifndef esstf_aux_h
#define esstf_aux_h

#include <string.h>
#include <stdio.h>
#include <time.h>
#include "timedef.h"
#include "plugin.h"

/* make your changes here: */
#include "esstf_config.h" 

/*-----------------------------------------------------------------------*/

/* the digital byte (4th byte esstf data) */
enum {
  CHANNEL_0 = 1,
  /* bit 1..3 not used */
  TIME_SLOW = 16,
  TIME_FAST = 32,
  ERROR_SAMPLE = 64,
  TRIGGER = 128
};

/* time structure; data obtained from header or DCF code*/
typedef struct {
  int status;          /* 1 = time valid; -1 = time invalid */
  int year;	       /* 0004 - 0005 year                  */
  int month;	       /* 0006 - 0007 month                 */
  int day;	       /* 0008 - 0009 day                   */
  int hour;	       /* 0010 - 0011 hour                  */
  int minute;	       /* 0012 - 0013 minute                */
  int second;	       /* 0014 - 0015 second                */
  int nosc;	       /* 0016 - 0019 num. of samp. until next second change */
} TIME;



/* ESSTF Datablock definition */
typedef struct {
  unsigned char datablock[2048];  /* 0001 - 2048 complete datablock */

  /* esstf header information (48 byte ascii header) */
  unsigned int channel;		/* 0001 - 0003 channel number */
  TIME ASCII_time;		/* time structure from ASCII header */
  TIME DCF_time;		/* time structure from extr_DCF() */
  unsigned int nob;		/* 0020 - 0023 number of bytes in this block */
  unsigned char free[25];	/* 0024 - 0048 free for future use */
  
  /* valid time for send_raw */
  TIME *datablock_time;		/* points to DCF or ASCII time */
  int datablock_time_status;    /* -1=no timeinfo; 1=DCF 2=ASCII  */
  INT_TIME it;                  /* timedef.h */
  
  /* 32 bit signed int for send_raw */
  int si32_data[500];    	/* 500*4=2000 bytes data from extr_data() */

  char *station, *component;    /* station name and 3D-component */
} DATABLOCK;

/*-----------------------------------------------------------------------*/


/* "public" functions */
/*-----------------------------------------------------------------------*/
int read_esstf_datablock(DATABLOCK *DB, FILE *esstf_file);
void init_datablock(DATABLOCK *DB);
void show_datablock_info(DATABLOCK *DB);
int send_datablock(DATABLOCK *DB);
void dump_esstf_data(DATABLOCK *DB, int channel, int info);
void diag_message(char *message);
int send_datablock_last(void);


/* "private" functions */
/*-----------------------------------------------------------------------*/
int extr_header(DATABLOCK *DB);
int extr_DCFtime(DATABLOCK *DB);
int set_time(DATABLOCK *DB);
int extr_data(DATABLOCK *DB);
void set_station_code(DATABLOCK *DB);




#endif /* esstf_aux_h */
