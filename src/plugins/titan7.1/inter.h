#ifndef _inter_def_h
#define _inter_def_h


#define leap_year(i) ((i % 4 == 0 && i % 100 != 0) || i % 400 == 0)

#define CRYSTAL_5323_22       0
#define AD_7710               1
#define HI7190                2
#define CRYSTAL_5321_22       3
#define BASIC_SAMPRATE1      31.25
#define BASIC_SAMPRATE2      20.0

/*
typedef unsigned char uchar;
*/


#define CONTINUOUS 1
#define TRIGGER    2

#define PRIMAIRE   1
#define SECONDAIRE 2

#define CMD_FRAME_TYPE  1
#define INFO_FRAME_TYPE 2


/* parametre d'une voie */

typedef struct
{
    int     chan_type;     /* channel type: primary or decimated          */
    int     nchan;         /* chan number: 0 1 = primary, 2 3 decimated   */
    int     on_off;        /* channel enabled/disabled                    */
    float   srate;         /* sample rate for primary channels only       */
    int     decim;         /* for decimated (or secondary) channels only  */
    float   sta;           /* trigger short term average                  */
    float   lta;           /* trigger long term average                   */
    float   ratio;         /* trigger ratio                               */
    int     absOffset;     /* data absolute offset: 1=yes, 0=no           */
} TCHAN;
#define MAXCHAN 16


#define NB_ECHAN_PROPA_AD       1.5
#define NB_ECHAN_PROPA_CRYSTAL  29.0
#define NB_ECHAN_PROPA_HI7190   1.5

/* External time pulse */

typedef struct
{
    int duration;
    int duration_test;
    int pulse_per_min;
} PULSE;

typedef struct
{
    float lat;
    float lon;
    float elev;
} COORD;


/* Station information and configuration */

typedef struct
{
   char    trames_digitizer[385];

   TCHAN   chan[MAXCHAN];            /* cmd frames 0,1,2,3 */

   int     pre_event;                /* cmd frame   5 */
   int     post_event;               /*               */
   int     min_duration;             /*               */
   int     gain;                     /*               */

   char    station_id[10];           /* cmd frame   7 */

   PULSE   pulse;                    /* cmd frame  15 */
   int     dig_last_time_setting;    /*               */

   char    owner[10];                /* info frame 16 */
   int     hardware_sernum;          /*               */

   char    software_version[12];     /* info frame 17 */

   float   battery_voltage;          /* info frame 18 */
   int     nb_trigger;               /*               */
   int     media_nbytes;             /*               */
   int     lost_flush_batt;          /*               */

   int     Ram_nbytes;               /* info frame 19 */
   int     Ram_ncycles;              /*               */
   int     lost_flush_media;         /*               */
   int     flush_status;             /*               */

   int     ncomp;                    /* info frame 20 */
   int     adc_type;                 /*               */
   float   adc_delay_nsamp;          /*               */
   float   basic_srate;              /*               */
   int     time_keeping_chip;        /*               */
   int     record_mode;              /*               */
   int     storage_media;            /*               */
   int     flash_memory;             /*               */
   int     modem_link;               /*               */
   int     parallel_link;            /*               */

   int     circ_mem_curr_pointer;    /* info frame 32 */
   int     circ_mem_nb_cycles;       /*               */

   double  dig_time;                 /* info frame 33 */
   double  ext_time;                 /*               */
   double  delta_t;                  /*               */
   int     time_out;                 /*               */

   int     pc_time_of_last_dig_time; /* info frame 34 */

   int     backup_total_space;       /* info frame 35 */
   int     backup_free_space;        /*               */

   char    version_pc[13];           /* info frame 36 */

   COORD   coord;                    /* frames MISC   */

} STA_INFO;

#endif /* _inter_def_h */
