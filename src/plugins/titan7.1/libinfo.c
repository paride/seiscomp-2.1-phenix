/*======================================================================*
    libinfo.c
    J.-F. Fels
 *======================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h> 
#include <time.h>
#include "titan.h"
#include "inter.h"
#include "proto.h"


/* Info frames: 32 from 0 to 31.                         */
/* Frames 0-15 correspond to the                         */
/* last 0-15 commands sent to the station.               */
/* All these frames: 0-20 are described in doctitan.doc. */

/* Digitizer last commands: 0 to 15 */
#define CMD_00      0  /* PARAM_CHAN_0 */
#define CMD_01      1  /* PARAM_CHAN_1 */
#define CMD_02      2  /* PARAM_CHAN_2 */
#define CMD_03      3  /* PARAM_CHAN_3 */
#define CMD_04      4  /* SENSOR_FILT  */
#define CMD_05      5  /* TRIGGER */
#define CMD_07      7  /* SEND_STA_ID */
#define CMD_15     15  /* SET_TIME */

/* Info frames */
#define INFO_FRAME_16      16
#define INFO_FRAME_17      17
#define INFO_FRAME_18      18
#define INFO_FRAME_19      19
#define INFO_FRAME_20      20

/* Type des voies */
#define PRIMARY             1
#define SECONDARY           2
#define LESS_THAN           0
#define GREATER_THAN        1


#ifdef ANSI_C
static void imprime_infos  ();
static void info_frame_20  (char*);
static void cmd_frame_0123 (char*);
static void cmd_frame_04   (char*);
static void cmd_frame_05   (char*);
static void cmd_frame_07   (char*);
static void cmd_frame_15   (char*);
static void info_frame_16  (char*);
static void info_frame_17  (char*);
static void info_frame_18  (char*);
static void info_frame_19  (char*);
static int diff_infos      ();
static float  dspw2real    (char, char, char);
static char *bin2hexstr    (char*, int);
#else
static void imprime_infos  ();
static void info_frame_20  ();
static void cmd_frame_0123 ();
static void cmd_frame_04   ();
static void cmd_frame_05   ();
static void cmd_frame_07   ();
static void cmd_frame_15   ();
static void info_frame_16  ();
static void info_frame_17  ();
static void info_frame_18  ();
static void info_frame_19  ();
static int  diff_infos     ();
static float dspw2real     ();
static char *bin2hexstr    ();
#endif


STA_INFO       STA_infos;
FILE           *f_log;
static char    messg[512];
static int     prim_srate_code[2] = {-1,-1};
static float   prim_srate;
static int     debug = 0;
static int     do_print = FALSE;

extern TITFILE *Fp_tit;
extern double  SystTime;
extern char    Station[8];
extern struct  option opt;


/*=====================================================================*
    decode_infos
        Support for command frames    0 1 2 3 5 7 15
                for inform. frames    16 17 18 19 20
 *=====================================================================*/
void decode_infos(buf, print_flag)
char *buf;
int  print_flag;
{
int  j;


    if (print_flag)    do_print = TRUE;
    if (f_log == NULL) f_log = stdout;

if (debug)
    for (j=0; j<48; j++) printf("%2d  %s\n", j, bin2hexstr(&buf[12*j],12));

    memcpy(STA_infos.trames_digitizer, buf, 32*12);


/* INFO_FRAME_20 */   info_frame_20(&buf[INFO_FRAME_20 * 12]); 
/* CMD_00 to 03 */    cmd_frame_0123(buf);
/* CMD_04 */          cmd_frame_04(&buf[CMD_04 * 12]);
/* CMD_05 */          cmd_frame_05(&buf[CMD_05 * 12]);
/* CMD_07 */          cmd_frame_07(&buf[CMD_07 * 12]);
/* CMD_15 */          cmd_frame_15(&buf[CMD_15 * 12]);
/* INFO_FRAME_16 */   info_frame_16(&buf[INFO_FRAME_16 * 12]);
/* INFO_FRAME_17 */   info_frame_17(&buf[INFO_FRAME_17 * 12]);
/* INFO_FRAME_18 */   info_frame_18(&buf[INFO_FRAME_18 * 12]);
/* INFO_FRAME_19 */   info_frame_19(&buf[INFO_FRAME_19 * 12]);

    imprime_infos();
}


/*=====================================================================*
                       INFO_FRAME_20
 *=====================================================================*/
static void info_frame_20(frame)
char *frame;
{
char           media_byte;

/*====== Summer 1999 update (from Agecodagis) =======

FRAME 20  :  Configuration 1 of the recorder 
  I0: nb of channels 
  I1: type of A/D digitizer
        b210  
          000 Crystal 5323/22
          001 Analog Device AD7710
          010 Harris HI7190
          011 Crystal 5321/22
          other not till assigned
        b7
          0 no Auxilliary channels
          1 16*16 bits Auxilliary channels
  I2: No info
  I3: Sampling frequencies set
        b2
          0 250 Hz
          1 160 Hz 
  I4: Time keeper
          0 none
          1 Installed
  I5: Type of recording
        b0
          0 continuous
          1 on trigger STA/LTA 
        b321 (if media byte is not 0)
          000 DAT
          001 SCSI DISK 
  I6: Flash memory
          0 no Flash
          x x mega bytes Flash (usually 8) 
  I7: Media storage capacity
          0 no one 
          2 2 Gb DAT/DISK  DRIVE
  I8: No info
  I9: Parallel link
          0 no parallel link 
          1 parallel link
 ===================================================================*/


if (0) printf("info_frame_20: %s\n", bin2hexstr(frame,12));

    STA_infos.ncomp             = frame[0];
    STA_infos.adc_type          =(frame[1] & 0x07);
    if      (STA_infos.adc_type == CRYSTAL_5323_22)
                          STA_infos.adc_delay_nsamp = NB_ECHAN_PROPA_CRYSTAL;
    else if (STA_infos.adc_type == AD_7710)
                          STA_infos.adc_delay_nsamp = NB_ECHAN_PROPA_AD;
    else if (STA_infos.adc_type == HI7190)
                          STA_infos.adc_delay_nsamp = NB_ECHAN_PROPA_HI7190;
    else if (STA_infos.adc_type == CRYSTAL_5321_22)
                          STA_infos.adc_delay_nsamp = NB_ECHAN_PROPA_CRYSTAL;
    else                  STA_infos.adc_delay_nsamp = -7777.0;
    if      (((frame[3] & 0x4) >> 2) == 0)
        STA_infos.basic_srate = BASIC_SAMPRATE1;
    else if (((frame[3] & 0x4) >> 2) == 1)
        STA_infos.basic_srate = BASIC_SAMPRATE2;
    STA_infos.time_keeping_chip = frame[4];
    if ((int) (frame[5] & 0x1) == 0)
        STA_infos.record_mode   = CONTINUOUS;
    else
        STA_infos.record_mode   = TRIGGER;
    media_byte = frame[7];
    if (media_byte != 0)
        STA_infos.storage_media = (frame[5] & 0xE) >> 1;
    else
        STA_infos.storage_media = -1;
    STA_infos.flash_memory      = (int) frame[6];

/* Normal user doesn't have to know about the following */

    STA_infos.modem_link        = (int) frame[8];
    STA_infos.parallel_link     = (int) frame[9];
}


/*=====================================================================*
                CMD_00
                CMD_01
                CMD_02
                CMD_03
 *=====================================================================*/
static void cmd_frame_0123(buf)
char *buf;
{
char frame[14];
char           temp;
int            chan;
/*
 * Attention: STA_infos.basic_srate comes from frame 20 : see info_frame_20()
 */

/* ATTENTION decode_infos */ if (0) STA_infos.basic_srate = 0.;

    if ((STA_infos.basic_srate != BASIC_SAMPRATE1) &&
        (STA_infos.basic_srate != BASIC_SAMPRATE2))
    {
        sprintf(messg,
          "cmd_frame_0123: can't find basic_srate (see info_frame %d)",
           INFO_FRAME_20);
        fprintf(f_log,"FATAL ERROR\n");
        exit(0);
    }

    for (chan=0; chan<4; chan++)
    {
        memcpy(frame, &buf[chan*12], 12);

if (0) printf("-- decode_infos : chan %d : %s\n",
              chan,bin2hexstr(frame, 12));

/*
                   0  1  2  3  4  5  6  7  8  9 10
-- decode_infos : 00 08 BC 01 05 19 02 F0 01 00 00 A4
-- decode_infos : 00 08 BC 01 05 19 02 F0 00 00 01 54
-- decode_infos : 04 4B 9B 52 07 31 07 F0 00 00 02 A4
-- decode_infos : 01 16 6E 1C E8 EA 05 06 00 00 03 54
*/

/* CHANNEL TYPE, SRATE, DECIM */

        if (chan == 0 || chan == 1)
        {
            STA_infos.chan[chan].chan_type = PRIMARY;
            temp = frame[6] & 0xF;
            prim_srate_code[chan] = 1 << temp;
            STA_infos.chan[chan].decim = 0;
            STA_infos.chan[chan].srate =
                  STA_infos.basic_srate * prim_srate_code[chan];
            prim_srate = STA_infos.chan[chan].srate;
if (0) printf("-- decode_infos : prim_srate_code=%d f=%.5f\n",
            prim_srate_code[chan], STA_infos.chan[chan].srate);
        }
        else if (chan == 2 || chan == 3)
        {
            STA_infos.chan[chan].chan_type = SECONDARY;
            temp = frame[6] & 0xF;
            if (prim_srate_code[chan-2] < 0)
            {
              fprintf(stderr,
                 "decode_infos chan %d: can't figure out prim srate\n",chan);
              exit(0);
            }
            STA_infos.chan[chan].decim = (1 << (int) temp);
            STA_infos.chan[chan].srate =
                  (STA_infos.basic_srate * prim_srate_code[chan-2]) /
                  (float) (1 << temp);
if (0) printf("-- decode_infos : prim_srate_code=%d f=%.5f\n",
            prim_srate_code[chan], STA_infos.chan[chan].srate);
        }

/*==== LTA, STA ====*/

      STA_infos.chan[chan].lta =
          dspw2real(frame[0],frame[1],frame[2]) / STA_infos.chan[chan].srate;
      STA_infos.chan[chan].sta =
          dspw2real(frame[3],frame[4],frame[5]) / STA_infos.chan[chan].srate;

/*====  RATIO ====*/

      STA_infos.chan[chan].ratio = (float) (int) frame[7] / 2;

/*==== ON-OFF ====*/

      if ((frame[8] & 0xFF) == 0xFF)
          STA_infos.chan[chan].on_off = 0;
      else
          STA_infos.chan[chan].on_off = (char) frame[8] & 0xFF;

/*==== DATA OFFSET SUPPRESSED */
/*
 * absOffset = 0 means that offset has been subtracted from samples
 *                 and that offset correction is needed to get absolute value.
 * absOffset = 1 means that samples are absolute values and
 *                 that no offset correction is needed.
 */
      STA_infos.chan[chan].absOffset = (int) frame[9] & 0xFF;


if (0) printf("-- decode_infos : ch %d type %d on %d f=%.5f sta=%d \
lta=%d rat=%.2f d=%d ofs=%d\n",
          chan,
          STA_infos.chan[chan].chan_type,
          STA_infos.chan[chan].on_off,
          STA_infos.chan[chan].srate,
    (int) STA_infos.chan[chan].sta,
    (int) STA_infos.chan[chan].lta,
          STA_infos.chan[chan].ratio,
          STA_infos.chan[chan].decim,
          STA_infos.chan[chan].absOffset);
    }
}


/*=====================================================================*
                        CMD_04
 *=====================================================================*/
static void cmd_frame_04(frame)
char *frame;
{
int i;

    for (i=0; i<9; i++) if (frame[i] != 0)
    {
       fprintf(f_log,"WARNING: Sensor Compensator filter OBSOLETE\n");
       fprintf(f_log,"         frame num 4: bytes 0 to 8 should be all 0\n");
       fprintf(f_log,"         %s\n", bin2hexstr((char*) frame,12));
       break;
    }
}


/*=====================================================================*
                         CMD_05
 *=====================================================================*/
static void cmd_frame_05(frame)
char *frame;
{

    STA_infos.pre_event    = (uchar) frame[0] * 256 + (uchar) frame[1];
    STA_infos.post_event   = (uchar) frame[2] * 256 + (uchar) frame[3];
    STA_infos.min_duration = (uchar) frame[5] * 256 + (uchar) frame[6];
    STA_infos.gain         = (int) pow(2,(int) frame[4]);
    if (debug) fprintf(f_log,
        "-- decode_infos : pre_event=%d post_event=%d min=%d gain=%d\n",
         STA_infos.pre_event,
         STA_infos.post_event,
         STA_infos.min_duration,
         STA_infos.gain);
}


/*=====================================================================*
                          CMD_07
 *=====================================================================*/
static void cmd_frame_07(frame)
char *frame;
{
    sprintf(STA_infos.station_id, "%.8s", frame);
}

/*=====================================================================*
                          CMD_15
 *=====================================================================*/
static void cmd_frame_15(frame)
char *frame;
{
char  tmasc[100];

    STA_infos.dig_last_time_setting =
              bytes2int4(frame[0],frame[1],frame[2],frame[3]);
    STA_infos.pulse.duration      = bytes2int4(0,0,frame[4],frame[5]);
    STA_infos.pulse.duration_test = (int) frame[6];
    STA_infos.pulse.pulse_per_min       = (int) frame[7];
    if (debug)
    {
        strftime(tmasc,40,"%Y/%m/%d-%H:%M:%S",
                 gmtime((time_t*) &STA_infos.dig_last_time_setting));
        fprintf(f_log,
            "-- decode_infos : digitizer last time setting %d -> %s\n",
             STA_infos.dig_last_time_setting, tmasc);
        fprintf(f_log,
            "-- decode_infos : ext_pulse_dur=%d test=%d n_per_min=%d\n",
             STA_infos.pulse.duration,
             STA_infos.pulse.duration_test,
             STA_infos.pulse.pulse_per_min);
    }
}

/*=====================================================================*
                         INFO_FRAME_16
 *=====================================================================*/
static void info_frame_16(frame)
char *frame;
{
int i;

if (0) printf("%s\n", bin2hexstr((char*) frame,12));

    for(i=0; i<9; i++) STA_infos.owner[i] = frame[i];
    STA_infos.owner[i] = '\0';
    STA_infos.hardware_sernum = frame[9];
}

/*=====================================================================*
                         INFO_FRAME_17
 *=====================================================================*/
static void info_frame_17(frame)
char *frame;
{
int i;
    for(i=0; i<10; i++) STA_infos.software_version[i] = frame[i];
    STA_infos.software_version[i] = '\0';
}

/*=====================================================================*
                         INFO_FRAME_18
 *=====================================================================*/
static void info_frame_18(frame)
char *frame;
{
    STA_infos.nb_trigger   = bytes2int4(0,0,frame[0],frame[1]);
    STA_infos.media_nbytes = 
                   bytes2int4(frame[2],frame[3],frame[4],frame[5]);
/* Units are 32768 Bytes. Convert to MBytes */
    STA_infos.media_nbytes =
         (int) ((double) STA_infos.media_nbytes * (double) 3.2768e-02);
    STA_infos.battery_voltage = (float) ((uchar)frame[6]) * 20.163 / 256.0;
    STA_infos.lost_flush_batt = bytes2int4(0,0,frame[7],frame[8]);
    if (debug ) fprintf(f_log,
        "-- decode_infos : batt %.2f nb_trigger %d\n",
         STA_infos.battery_voltage,
         STA_infos.nb_trigger);
}

/*=====================================================================*
                          INFO_FRAME_19
 *=====================================================================*/
static void info_frame_19(frame)
char *frame;
{
/*=======================================================
    Here we use RAM for flash memory and Media for storage device,
    usually a hard drive.

    b0,b1 (MSB 1st)    number of bad RAM flush caused by media problem.

    b2                 number of RAM cycles     (Valid for trigger mode only)
    b3,b4,b5 (MSB 1st) number of bytes in RAM
    b6                 RAM flush operations
                           0  Media is off
                           1  Turning Media on
                           2  going to end of data
                           3  writing data
                           4  going to begining of data
                           5  writing index
                           6  turning Media off
    b7                 trigger event in process (here ignored)
                           0  off
                           1  on
    b8                 SCSI problems            (here ignored)
==========================================================*/

if (0) printf("-- decode_infos : %s\n", bin2hexstr(frame, 12));

    STA_infos.lost_flush_media = bytes2int4(0,0,frame[0],frame[1]);
    if (STA_infos.record_mode == TRIGGER)
    {
        STA_infos.Ram_ncycles = (int) frame[2];
    }
    else
    {
        STA_infos.Ram_ncycles = -1;
    }
    STA_infos.Ram_nbytes = bytes2int4(0,frame[3],frame[4],frame[5]);
    STA_infos.flush_status = frame[6];
}


/*======================================================================*
 * imprime_infos
 *            Imprime le contenu de la structure information TINFO
 *    input:  structure STA.infos
 *    output: ecriture sur le file pointer f_log (global)
 *======================================================================*/
static void imprime_infos()
{
int    chan;
char   tmasc[100];

  if (do_print == TRUE || opt.info.all == TRUE || opt.info.diff == TRUE)
  {
    if ((opt.info.diff == TRUE) && (diff_infos() == 0)) return;

    fprintf(f_log,"\n");
    fprintf(f_log,"SYSTEM CONFIGURATION\n");

    fprintf(f_log,"digitizer chip model                    : ");
    switch (STA_infos.adc_type)
    {
      case CRYSTAL_5323_22   : fprintf(f_log,"CRYSTAL_5323_22\n");  break;
      case AD_7710           : fprintf(f_log,"AD_7710\n");          break;
      case HI7190            : fprintf(f_log,"HI7190\n");           break;
      case CRYSTAL_5321_22   : fprintf(f_log,"CRYSTAL_5321_22\n");  break;
      default                : fprintf(f_log,"UNKNOWN\n");
    }
    fprintf(f_log,"number of components                    : %d\n",
        STA_infos.ncomp);
    fprintf(f_log,"digitizer time delay                    : %.1f samples\n",
        STA_infos.adc_delay_nsamp);
    fprintf(f_log,"basic sample rate                       : %.3f\n",
        STA_infos.basic_srate);
    fprintf(f_log,"independant time-keeping device         : %s\n",
        (STA_infos.time_keeping_chip == 1 ? "yes" : "no"));
    fprintf(f_log,"recording mode                          : %s\n",
        (STA_infos.record_mode   == CONTINUOUS ? "continuous" : "trigger"));
    fprintf(f_log,"RAM   memory                            : %s\n",
        (STA_infos.flash_memory  == 0 ? "no" : "yes"));

    fprintf(f_log,"storage media                           : ");
    switch(STA_infos.storage_media)
    {
        case 0 : fprintf(f_log,"DAT\n");       break;
        case 1 : fprintf(f_log,"SCSI DISK\n"); break;
        default: fprintf(f_log,"NONE\n");      break;
    }

   fprintf(f_log, "owner                                   : %s\n",
           STA_infos.owner);
   fprintf(f_log, "equipment serial number                 : %d\n",
           STA_infos.hardware_sernum);
   fprintf(f_log, "acquisition software version            : %s\n",
           STA_infos.software_version);

   fprintf(f_log,"Channels\n");

   for (chan=0; chan<4; chan++)
   {
     fprintf(f_log,"ch %d on=%d %9.5f Hz sta=%.1f lta=%.1f rat=%.1f ofs=%d\n",
            chan,
            STA_infos.chan[chan].on_off,
            STA_infos.chan[chan].srate,
            STA_infos.chan[chan].sta,
            STA_infos.chan[chan].lta,
            STA_infos.chan[chan].ratio,
            STA_infos.chan[chan].absOffset);
   }

   fprintf(f_log,"event detection: pre-evn %d post-evn %d min %d    ",
           STA_infos.pre_event,
           STA_infos.post_event,
           STA_infos.min_duration);
   fprintf(f_log,"AD converter gain: %d\n", STA_infos.gain);
   if ((STA_infos.adc_type == HI7190) &&
       (STA_infos.gain != 1))
   {
     fprintf(f_log,
        "!!! WARNING: gain should be 1 for HARRIS 7190 digitizer\n");
   }

   fprintf(f_log,
           "external time pulse: %d per minute, valid if %s %d ms\n",
           STA_infos.pulse.pulse_per_min,
           ((STA_infos.pulse.duration_test == LESS_THAN) ? "<" : ">"),
           STA_infos.pulse.duration);



   sprintf(tmasc,"????.??.??-??:??:??");
   if (STA_infos.dig_last_time_setting > 0)
       strftime(tmasc,40,"%Y.%m.%d-%H:%M:%S",
                gmtime((time_t*) &STA_infos.dig_last_time_setting));
   fprintf(f_log,
       "digitizer last time setting             : %.19s\n", tmasc);

  }

/*
 * End of configuration. The following are variables
 */

  time_asc4(tmasc, SystTime);

  if (opt.info.all || opt.info.media)
  {
      fprintf(f_log,
          "%s %.19s %d RAM=%d B Media=%d MB flush_stat=%d\n",
          Station, tmasc, (int) SystTime,
          STA_infos.Ram_nbytes,
          STA_infos.media_nbytes,
          STA_infos.flush_status);
  }

  if (opt.info.all || opt.info.batt)
  {
      fprintf(f_log,
          "%s %.19s %d Batt=%.2f lost_flush(batt)=%d lost_flush(media)=%d\n",
          Station, tmasc, (int) SystTime,
          STA_infos.battery_voltage,
          STA_infos.lost_flush_batt,
          STA_infos.lost_flush_media);
  }


  if (opt.info.all || opt.info.coord)
  {
      static double prev_lat, prev_lon, prev_elev;

      if ((STA_infos.coord.lat < 0.0) ||
          (STA_infos.coord.lat > 360.0) ||
          (STA_infos.coord.elev > 10000.0) ||
          (STA_infos.coord.elev < -1000.0))

      {
          ;
      }
      else if ((prev_lat == STA_infos.coord.lat) &&
          (prev_lon == STA_infos.coord.lon) &&
          (prev_elev == STA_infos.coord.elev))
      {
          ;
      }
      else
      {
        fprintf(f_log,
          "%s %.19s %d Coord: lat=%.3f lon=%.3f elev=%.3f\n",
          Station, tmasc, (int) SystTime,
          STA_infos.coord.lat,
          STA_infos.coord.lon,
          STA_infos.coord.elev);
      }
      prev_lat = STA_infos.coord.lat;
      prev_lon = STA_infos.coord.lon;
      prev_elev = STA_infos.coord.elev;
  }
}

/*======================================================================*
 * diff_infos
 *    compare current STA.infos with previous
 *======================================================================*/
static int diff_infos()
{
static STA_INFO prev;
int    chan;
int    n = 0;

   if (strcmp(STA_infos.owner, prev.owner))                        ++n;
   if (STA_infos.hardware_sernum       != prev.hardware_sernum)    ++n;
   if (STA_infos.basic_srate           != prev.basic_srate)        ++n;

   for (chan=0; chan<4; chan++)
   {
    if (STA_infos.chan[chan].on_off      != prev.chan[chan].on_off)      ++n;
    if (STA_infos.chan[chan].srate       != prev.chan[chan].srate)       ++n;
    if (STA_infos.chan[chan].sta         != prev.chan[chan].sta)         ++n;
    if (STA_infos.chan[chan].lta         != prev.chan[chan].lta)         ++n;
    if (STA_infos.chan[chan].ratio       != prev.chan[chan].ratio)       ++n;
    if (STA_infos.chan[chan].absOffset   != prev.chan[chan].absOffset) ++n;
   }
   if (STA_infos.pre_event             != prev.pre_event)             ++n;
   if (STA_infos.post_event            != prev.post_event)            ++n;
   if (STA_infos.min_duration          != prev.min_duration)          ++n;
   if (STA_infos.pulse.pulse_per_min   != prev.pulse.pulse_per_min)   ++n;
   if (STA_infos.pulse.duration        != prev.pulse.duration)        ++n;
   if (STA_infos.pulse.duration_test   != prev.pulse.duration_test)   ++n;
   if (STA_infos.dig_last_time_setting != prev.dig_last_time_setting) ++n;
   
/*   if (n) printf("\n  NEW CONFIG: n=%d\n", n); */
   memcpy(&prev, &STA_infos, sizeof(STA_INFO));
   return n;
}

/*=====================================================================*
   - construct int number from 3 bytes
   - right shift by 8
   - left  shift by 8
   - divide by 2**23        : dblnum = n / (1 << 23)
   - resulting float number : -1 / log(1 - dblnum)
 *=====================================================================*/
float dspw2real(b1,b2,b3)
char b1,b2,b3;
{
double dbln;
int n; 

      dbln = 0.0; 
      n = bytes2int4(0,b1,b2,b3) << 8;
if (0) fprintf(stderr,"-- dspw2real : w=%d\n", n); 
      n = n >> 8;
      dbln = -1.0 / (log(1.0 - ((float) n / (double) 0x800000))); 
      return (float) dbln;
} 


/*====================================================================*
 *  bin2hexstr
 *====================================================================*/
char *bin2hexstr(buf, n)
char *buf;
int  n;
{
static char str[1023];
int i;
  
  if (n > 1020)
  {
      fprintf(stderr, "bin2hexstr : n_char too big (MAX 1020)\n");
      exit (0);
  }
  str[0] = '\0';
  for (i=0; i<n; i++)
  {
    sprintf(&str[strlen(str)], "%02X ", 0xFF & buf[i]);
  }
  return (str);
}


