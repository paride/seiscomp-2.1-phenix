#ifndef _sismalp_h
#define _sismalp_h

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <ctype.h>
#include <string.h>
#include <limits.h>

#define SISMALP_REV "15.22"

#include <sys/fcntl.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#define m_alloc(n) malloc(n)
#define m_free(pt) free(pt)
#define SISMDIR "."
#define NETDIR  "/home/fels/sismnet"
#define EVNDIR  "/home/fels/sismnet/events"
#define TIMEDB  "timedb"

#define EVNLST   "events.lst"
#define EVNTBL   "events.tbl"
#define LOGNAME  "litho.log"
#define TMINFO   "litho.tim"

union four_bytes
{
    int data;
    char c[4];
};

#define MAX(a,b) (((a)>(b))?(a):(b))
#define MIN(a,b) (((a)<(b))?(a):(b))
#define leap_year(i) ((i % 4 == 0 && i % 100 != 0) || i % 400 == 0)
#define CRLF(fp)  fprintf(fp, "%c%c", 0x0d,0x0a);
#define LINELEN  256

#define Z         0
#define N         1
#define E         2

/* HADES 310 data descriptor */
#define PLEN_OFS  16
#define MAGN_OFS  18
#define FDEL_OFS  20
#define POST_OFS  25
#define PRE_OFS   27
#define FREQ_OFS  31
#define GAIN_OFS  33
#define STA_OFS   37
#define LTA_OFS   40
#define RAT_OFS   43

struct acqparm {
    float extpulse_len;
    float filtdelay;
    long  npre_evn;
    long  npost_evn;
    float freq;
    char  gain;
    long  sta;
    long  lta;
    char  ratio;
};

/* SISMALP format */
#define BLKLEN  512        /* datablock length: 512 shorts or 1024 bytes */
#define DSC1LEN  62        /* Descriptor 1st line: max 62 chars          */
#define DSC2LEN  65        /* Descriptor 2nd line: max 65 chars          */

/* SISMALP descriptor */
/*
"ofs     8     14    20    26       35         46           "
"        |     |     |     |        |          |            " 
"    E119  1008  1159     2     25.  19.01.1995 14:35:19.070"
"    E118  1008   221     2     25.  19.01.1995 07:43:32.760"
*/
#define STA_NC   8
#define NDT_NC   6
#define BLN_NC   6
#define NBL_NC   6
#define FRQ_NC   8
#define DAT_NC  10
#define TIM_NC  12

/*
#define STAOFF   0
#define NDTOFF   9
#define BLNOFF  15
#define NBLOFF  21
#define FRQOFF  27
*/
#define STAOFF   0
#define NDTOFF   8
#define BLNOFF  14
#define NBLOFF  20
#define FRQOFF  26
#define DATOFF  36
#define TIMOFF  47

struct desc {
    char sta[STA_NC+2];
    long ndt;
    long bln;
    long nbl;
    float frq;
    char date[DAT_NC+2];
    char time[TIM_NC+2];
};

/*
Obsolete
#define cln_desc(dsc) \
dsc[NDTOFF-1] = ' ', dsc[BLNOFF-1] = ' ', dsc[NBLOFF-1] = ' ', \
dsc[FRQOFF-1] = ' ', dsc[DATOFF-1] = ' ', dsc[TIMOFF-1] = ' ';
*/

/*
Characters string parsed with sscanf: 
    sscanf doesn't like string lenght specified by arg, such as
    sscanf(str, "%*s", LEN, station);
*/
/*
#define rd_desc(str, ds) \
sscanf(str, "%8s %ld %ld %ld %f %10s %12s", \
ds.sta, &ds.ndt, &ds.bln, &ds.nbl, &ds.frq, &ds.date, &ds.time);
*/

/*
Characters string formatted with sprintf:
    "%8s" will produce a field of 8 chars, taking what is in the string.
    "%8.4s" will produce a field of 8 chars and 4 (or less) chars
     are written right justified".
    "%4.8s" will produce a field of 8 chars and 4 (or less) chars
     are written left justified".
*/
#define wr_desc(str, ds) \
sprintf(str,"        "); \
strncpy(&str[4], ds.sta, strlen(ds.sta)); \
sprintf(&str[8], "%*ld%*ld%*ld%*.3f  %*s %*s", \
NDT_NC, ds.ndt, BLN_NC, ds.bln, NBL_NC, ds.nbl, \
FRQ_NC, ds.frq, DAT_NC, ds.date, TIM_NC, ds.time);


#define NOCORR  (double) 77777.7
#define NODRIFT (double) 99999.9

#ifndef TRUE
#define TRUE (1)
#endif
#ifndef FALSE
#define FALSE (0)
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

struct TimeSta
{
    long time;
    long sta;
};

#define sizeofTimeSta   sizeof(struct TimeSta)

#endif
