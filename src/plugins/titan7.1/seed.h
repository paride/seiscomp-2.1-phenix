/*==================================================================
 *
 *  seed.h
 *
 *================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <pwd.h>
#include <dirent.h>
#include <time.h>

#define MKSEED_REV   "10.9"
#define SEED_REV     2.3

#define ORGANIZATION "My organisation"
#define NETWORK      "ZZ"
#define LABEL        "NOLABEL"
#define STATIONSDIR  "stations"
#define SENSORSDIR   "ana_filters"
#define FILTERSDIR   "dig_filters"
#define SENSORLIST   "sensor_list"
#define VOLFNAME     "seedvol"
#define ERRFNAME     "seedvol.log"
#define REQFNAME     "tobeseeded"


#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif


#define LRECL      4096
#define PRECL     (LRECL * 8)
#define LREC_LEN    "12"             /* Logical record length: 1**12=4096 */
#define FSDH_SIZE         48       /* Fixed Section Data Header size */
#define DATA_REC_HD_SIZE  64       /* Data Header size */
#define BYTES_IN_DREC  (LRECL - DATA_REC_HD_SIZE)


/* data format */

#define STEIM1     1
#define IEEEFLOAT  2
#define INT_4      3

#define isaleap(y) ((((y)%100!=0) && ((y)%4==0)) || ((y)%400==0))

/* Times outside 1990.01.01-00:00:00 - 2036.07.18-13:20:00 are bad */
#define END_OF_WORLD   (double) 2100000000
#ifndef TIME_OK
#define TIME_OK(t) (((int) t>631152000 && (int) t<(int) END_OF_WORLD) ? 1:0)
#endif

#define append_linklist_element(new, head, tail) \
        new->next = NULL; \
        if (head != NULL) tail->next = new; \
        else head = new; \
        tail = new;

struct lrec
{
    char rec[5000];
    struct lrec *next;
};

struct s_b074
{
    char   *b74;
    struct s_b074 *next;
};

struct timehead
{
    char            *b070;
    struct s_b074   *b074_head;
    struct s_b074   *b074_tail;
    int              logrec;
    struct timehead *next;
};

struct data_segment {
    char    *fname;
    char    sta[5];
    char    cha[4];
    double  begtime;
    double  endtime;
    double  srate;
    int     nsamples;
    double  uncorrBegTime;
    double  filtdelay;
    double  observ_dt;
    int     beg_rec;
    int     nrec;
    struct data_segment *next;
};

struct inp_cmt {
    char    station[5];
    char    channel[4];
    int     beg;
    int     end;
    char   *comment;
    int     code;
    struct inp_cmt *next;
};

struct sta_cmt {
    char  *comment;
    int    code;
    struct sta_cmt *next;
};


/*  Constants  */

#ifndef PI
#define PI (double) 3.14159265358979
#endif
#define TWOPI (double) 2.0 * PI
#define FIR_NORM_TOL   0.02
#define MAXTOKENS     30
#define NCOMP          3

/*  Filter types  */

#define UNDEF_FILT  0
#define ANALOG      1
#define FIR_SYM_1   2
#define IIR_PZ      3
#define COMB        4
#define IIR_POLY    5
#define FIR_ASYM    6
#define FIR_SYM_2   7
#define LAPLACE     8

/*  Enumerated data  */

enum {
/*     0-8 already taken: filter type (see above) */
UNDEF_FILT_,
ANALOG_,
FIR_SYM_1_,
IIR_PZ_,
COMB_,
IIR_POLY_,
FIR_ASYM_,
FIR_SYM_2_,
LAPLACE_,
/*        Sensors key words list */
STS1,
STS2,
STS2_L,
CMG5,
CMG3T,
CMG3ESP,
CMG40,
CMG40T,
ES_T,   /* EPISENSOR */
L22,
L4C,
HS10,
GS13,
LE3D_5S,
LE3D_20S,
STS1POS,
STS2POS,
SDJ_S2A,
/*        Acquisiton systems key words list */
TITAN3,
TITAN6,
MINITITAN3,
MINITITAN6,
REFTEK,
QUANTERRA,
MARS2000,
MARSLIGHT,
/*        units */
ACC,
DIS,
VEL,
};

#ifndef UNKNOWN
#define UNKNOWN        0x1777
#endif


/*  Structure templates  */

struct loc {
    double  lat;        /* latitude,  decimal degress, + => N */
    double  lon;        /* longitude, decimal degress, + => E */
    int     elev;       /* elevation, meters                  */
    int     depth;      /* depth, meters                      */
    int     greg;       /* geographic region number           */
    int     sreg;       /* seismic region number              */
    char    *desc;      /* description string                 */
};

struct firCoef {
    int     type;     /* coef type: FIR_SYM_1 FIR_SYM_2 COMB FIR_ASYM */
    int     decim;    /* decimation factor (1 => no decimation)       */
    int     ncoef;    /* num of coefs                                 */
    double  *coef;    /* array of coefs                               */
};


/*================================================================*
 *  "Standard filter" definitions.
 *  The structure template is generic so that it will work for all
 *  types of filters (as defined below).  Not all fields are relevant
 *  for all types of filters.
 *================================================================*/
struct filter {
    int      type;  /* filter type ANALOG  FIR  IIR  COMB  IIR_POLY */
    int      ncoef; /* num of coefs (if type FIR | COMB | IIR_POLY) */
    int      nzero; /* num of zeros (if type ANALOG | IIR)          */
    int      npole; /* num of poles (if type ANALOG | IIR)          */
    int      decim; /* decimation factor (1 => no decimation)       */
    double   sint;  /* sample interval                              */
    double   norm;  /* normalization factor                         */
    double   gain;  /* filter gain                                  */
    double   A0;    /* SEED normalization factor                    */
    double   fn;    /* normalization frequency                      */
    double  *coef;  /* array of coefs (if FIR | COMB | IIR_POLY)    */
    double  *zero;  /* array of zeros (if ANALOG | IIR)             */
    double  *pole;  /* array of poles (if ANALOG | IIR)             */
};

struct sensor {
    char           label[10];   /* sensor label                      */
    char          *type;        /* sensor type (STS1, CMG3, etc...)  */
    char          *id;          /* seismometer serial number         */
    char          *sname;       /* analog sensor filter file name    */
    struct filter *sensor;      /* analog filter                     */
    double         sensor_G;    /* sensor gain                       */
    char          *fname;       /* analog filter filter file name    */
    struct filter *filter;      /* analog filter                     */
    double         filter_G;    /* filter gain                       */
    int            units;       /* units: volts/{ACC | DIS | VEL}    */
    float          g_err;       /* estimated error in nominal G      */
    float          delta_lat;   /* delta lat decimal degress, + => N */
    float          delta_lon;   /* delta lon decimal degress, + => E */
    int            delta_elev;  /* delta elev,  meters, + => UP      */
    int            delta_depth; /* delta depth, meters, + => DOWN    */
    float          azimuth;
    float          dip;
};

struct ampli {
    char           label[10]; /* ampli label                      */
    double         ampli_G;  /* filter gain                       */
    char          *fname;     /* analog filter filter file name    */
    struct filter *filter;    /* analog filter                     */
};


struct dig_filt {
    int            decim;     /* filter decim factor               */
    char          *filename;  /* filter file name                  */
    struct filter *filter;    /* filter                            */
};

struct filt_cascade {
    char              label[10];     /* cascade label              */
    char             *filename;      /* cascade filename           */
    int               nstage;        /* num of dig filt in array   */
    struct dig_filt  *dig_filt;      /* array of digital filters   */
};

struct adc {
    char                label[10];     /* digitizer label               */
    double              fact;          /* digitizer conversion constant */
    double              sint;          /* sample interval, seconds      */
    int                 dataformat;    /* data format: 1|2|3            */
    struct filt_cascade *filt_cascade; /* cascade of digital filter     */
};

struct channel {
    char             name[4];          /* channel name                  */
    char             network[3];       /* network name                  */
    double           beg_utime;        /*       */
    double           end_utime;        /*       */
    int              bps;              /* bytes per sample              */
    char             sensor_label[10]; /* sensor label                  */
    struct sensor   *sensor;           /* sensor                        */
    char             ampli_label[10];  /* amplifier-filter label        */
    struct ampli    *ampli;            /* amplifier-filter              */
    char             adc_label[10];    /* digitizer label               */
    struct adc      *adc;              /* digitizer                     */
    char             casc_label[10];   /* dig filter cascade label      */
    int              ndig_filt;        /* number of digital filters     */
    struct dig_filt *dig_filt;         /* array of digital filters      */
    double           reported_sint;    /* reported sample interval, sec */
    double           sint;             /* sample interval, seconds      */
    double           ampli_nom_gain;   /* amplifier nominal gain        */
    double           gain_corr;        /* overall magnif correction     */
    double           gain_err;         /* overall magnif  error         */
    double           gain;             /* overall magnification         */
};

struct station {
    char              name[6];    /* station name; format STA         */
    char              network[3]; /* network name                     */
/*    int               beg; */   /* begin valid time:  YYYYDDD       */
/*    int               end; */   /* end valid time:    YYYYDDD       */
    double            beg_utime;  /*          */
    double            end_utime;  /*          */
    int               nadc;       /* number of digitizers             */
    int               nsensor;    /* number of sensors                */
    int               nampli;     /* number of amplifier-filter       */
    int               nchannel;   /* number of channels               */
    struct loc        loc;        /* station location info            */
    char             *sys;        /* acquisition system name          */
    struct sensor   **sensor;     /* array of sensor                  */
    struct ampli    **ampli;      /* array of amplifier-filter        */ 
    struct adc      **adc;        /* array of digitizer               */
    struct channel  **channel;    /* array of channels                */
    int               lrec_num;   /* station blk logical record number*/
    struct station   *next;
};



struct seed_time                   /* time from input stream */
{
    unsigned short int  year;
    unsigned short int  day;
    char                hour;
    char                minute;
    char                second;
    char                unused;
    unsigned short int  fracsec;
};

/*  SEED data record structure */

struct seed_data_hdr               /* fixed data header */
{
    char               station[5];             /* station name */
    char               location[2];            /* station location */
    char               channel[3];             /* channel name */
    char               network[2];             /* network name */
    struct seed_time   time;                   /* time */
    unsigned short int nsamples;               /* number samples */
    short int          sample_rate;            /* sample rate factor */
    short int          sample_rate_multiplier; /* sample rate multiplier */
    char               activity_flags;         /* activity flags */
    char               io_flags;               /* i/o flags */
    char               data_quality_flags;     /* data quality flags */
    char               number_blockettes;     /* # blockettes which follow */
    long int           number_time_corrections;/* # .0001s time corrections*/
    unsigned short int beg_of_data;            /* beginning of data */
    unsigned short int beg_first_blk;          /* beginning 1st blkt */
};

struct data_blk_hdr       /* data record blockette header */
{
  unsigned short int type;          /* blockette type */
  unsigned short int next_blk_byte; /* start of next blockette byte number */
};

struct data_blk_100       /* blockette 100 */
{
    struct data_blk_hdr hdr;        /* blockette header */
    float      sample_rate;         /* actual sample rate */
    unsigned   char flags;          /* flags */
    unsigned   char reserved[3];    /* reserved bytes */
};

/*
struct data_blk_1000
{
    struct data_blk_hdr hdr;
    unsigned char   encoding_fmt;
    unsigned char   word_order;
    unsigned char   rec_length;
    unsigned char   reserved;
};
*/

struct data_blk_1000      /* blockette 1000 */
{
    unsigned short int type;
    unsigned short int next_blk_byte;
    unsigned char  encoding_fmt;
    unsigned char  word_order;
    unsigned char  rec_length;
    unsigned char  reserved;
};

struct data_blk_1001      /* blockette 1001 */
{
    unsigned short int type;
    unsigned short int next_blk_byte;
    unsigned char  timing_quality;
    unsigned char  micro_sec;
    unsigned char  reserved;
    unsigned char  frame_count;
};

