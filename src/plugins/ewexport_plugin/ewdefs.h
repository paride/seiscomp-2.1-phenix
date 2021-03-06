
/* The following defines are dependant on site specific Earthworm configuration,
 * These value are from a Earthworm v6.2 installation.
 */

#ifndef EWDEFS_H
#define EWDEFS_H


/**** From the earthworm_global.d of an operational system ****/

#define TYPE_HEARTBEAT 3
#define TYPE_TRACEBUF  20



/**** From imp_exp_gen.h ****/

#define MAX_ALIVE_STR  256

#define STX 2     /* Start Transmission: used to frame beginning of message */
#define ETX 3     /* End Transmission: used to frame end of message */
#define ESC 27    /* Escape: used to 'cloak' unfortunate binary bit patterns which look like sacred characters */

/* Define States for Socket Message Receival */
#define SEARCHING_FOR_MESSAGE_START 0
#define EXPECTING_MESSAGE_START 1
#define ASSEMBLING_MESSAGE 2

/* Define Buffer Size for Socket Receiving Buffer */
#define INBUFFERSIZE 100



/**** From transport.h ****/

typedef struct {             /******** description of message *********/
  unsigned char    type;     /* message is of this type               */
  unsigned char    mod;      /* was created by this module id         */
  unsigned char    instid;   /* at this installation                  */
} MSG_LOGO;



/**** From trace_buf.h ****/

#define TRACE_STA_LEN   7
#define TRACE_CHAN_LEN  9
#define TRACE_NET_LEN   9
#define TRACE_LOC_LEN   3

typedef struct {
        int     pinno;          /* Pin number */
        int     nsamp;          /* Number of samples in packet */
        double  starttime;      /* time of first sample in epoch seconds
                                   (seconds since midnight 1/1/1970) */
        double  endtime;        /* Time of last sample in epoch seconds */
        double  samprate;       /* Sample rate; nominal */
        char    sta[TRACE_STA_LEN];         /* Site name */
        char    net[TRACE_NET_LEN];         /* Network name */
        char    chan[TRACE_CHAN_LEN];        /* Component/channel code */
        char    datatype[3];    /* Data format code */
        char    quality[2];     /* Data-quality field */
        char    pad[2];         /* padding */
} TRACE_HEADER;

#endif
