/*                                                           */
/* esstf_config.h - edit this file according to your         */
/*                  esstf data stream definitions            */
/*                  and machine type                         */
/*                                                           */
/*                                                           */
/* Version: 2002.119   -    Mathias Hoffmann                 */
/*                          AWI Bremerhaven                  */
/*                                                           */
/*      tested with:                                         */
/*      Solaris 2.6 (sparc) &  Linux 2.4.4 (intel)           */
/*                                                           */

#ifndef esstf_config_h
#define esstf_config_h

#define ESSTF_PLUGIN_VERSION "2002.119"

/* select your machine type */
#define __ESSTF_INTEL__ 
/* #define __ESSTF_SPARC__ */

/*-----------------------------------------------------------------------*/
#define DATABLOCK_SIZE 2048  /* esstf datablock size */
#define CHANNELS 28          /* number of channels in esstf data stream  */
#define SAMPLERATE 62.5      /* samplerate for ALL channels */

/*-----------------------------------------------------------------------*/
/* station name, starting with esstf_channel 0 */
/* set NULL if the channel is present, but you don't want to */
/* proccess it */
static char *station_code[] =
      {"VNA1", "VNA1", "VNA1", /* 0-2 */
       "NULL", "NULL", "NULL", "NULL", /* 3-6*/
       "VNA3", "VNA3", "VNA3", /* 7-9 */
       "VNA2", "VNA2", "VNA2", /* 10-12 */
       "WATZ", "WATZ", "WATZ", /* 13-15 */
       "WATZ", "WATZ", "WATZ", "WATZ", "WATZ", /* 16-20 */
       "WATZ", "WATZ", "WATZ", "WATZ", "WATZ", "WATZ", "WATZ" /* 21-27 */ 
       };

/* 3D component, starting with esstf_channel 0 */
/* uses the same layout like station_code */
static char *station_component[] =
       {"N", "Z", "E", /* 0-2 */
        "NULL", "NULL", "NULL", "NULL", /* 3-6*/
	"N", "Z", "E", /* 7-9 */
	"N", "Z", "E", /* 10-12 */
	"A1", "A2", "A3", /* 13-15 */
	"B1", "B2", "B3", "B4", "B5", /* 16-20 */ 
	"C1", "C2", "C3", "C4", "C5", "C6", "C7" /* 21-27 */ 
       };

/*-----------------------------------------------------------------------*/


#endif /* esstf_config_h */
