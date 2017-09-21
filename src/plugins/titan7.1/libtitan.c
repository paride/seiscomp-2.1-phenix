/*======================================================================
    Program libtitan.c

    Library for Titan to Sismalp converter

    Author: J.-F. Fels, OMP, Toulouse

*======================================================================*/
#include "titan.h"
#include "proto.h"
#include "inter.h"

/* Prototypes */

#ifdef ANSI_C
static void   checkOutDataTime();
static void   pr_frame        ();
static void   printInfo       (FILE *);
static double GetDbExtraTcorr (double, int);
#else
static void   checkOutDataTime();
static void   pr_frame        ();
static void   printInfo       ();
static double GetDbExtraTcorr ();
#endif


static int stationDbParms = 0;
int     nnew, nduplic;  /* New and duplicate sismalp trace counts */
char    Station[8];     /* Station name from TITAN info   */
double  BasicSamprate;
struct StationParm  *staparms_head;
struct StationParm  *staparms_tail;


extern TITFILE *Fp_tit;
extern FILE    *Fp_log;
extern Paths   db_paths;
extern char    dtfname[PATHLEN];
extern struct  option opt;
extern struct  flags  flags;
extern int     inp_data[][NCOMP][NINP];
extern struct  acq acq;
extern struct  Channel Channel[];
extern outData OutData[];
extern char    Info[MAX_INFO_FRAME][12];
extern struct  dft_list *dft_head;
extern struct  xtc_list *xtc_head;
extern int     foundDftFiles;
extern STA_INFO STA_infos;
extern double  SystTime;
extern double  FirstSystTime;
extern double  LastSystTime;
extern double  ExtPulse;
extern double  ExtraTcorr;
extern TimeReset ResetTime;
extern double  Observ_dt;
extern int     totOutSamples;
extern int     DataOffsetCorr;



/*================================================================*
    processTimeFrame

 The time frames are found every 128 primary data samples
 Upon completion of this module, the flag "TimeFrame" is set
 to tell the module processDataFrame() that it has several
 task to do:
    check number of samples
    check the data continuity
    store the current data.
 The short data blocks are discarded in the present module.
 *================================================================*/
int processTimeFrame(frame)
char *frame;
{
int   n;
long             systtime, exttime;
int              systmsec, extmsec;
int              time_out_flag;
static double    prev_ExtPulse;
int              chan;
int              ninp;
double           tmp1,tmp2;
static double    prev_ExtraTcorr = 0.0001;

/* Time cannot be processed before info frames have been found */
 
    if (acq.AdcDelay == (double) UNKNOWN)
    {
        fprintf(Fp_log, "  ERROR: processTimeFrame: can't find adc delay\n");
        return -1;
    }

    systtime = bytes2int4(frame[0],frame[1],frame[2],frame[3]);
    exttime  = bytes2int4(frame[4],frame[5],frame[6],frame[7]);
    n        = bytes2int4(0,frame[8],frame[9],frame[10]);

    systmsec = n         & 0X3FF;
    extmsec  = (n >> 10) & 0X3FF;
    time_out_flag = (n >> 23) & 0X01;


/*
printf("-- %.19s", asctime(gmtime(&systtime)));
printf("-- %.19s\n", asctime(gmtime(&exttime)));
printf("==== SystTime %.3f %.3f %.3f %.3f\n", SystTime,tmp1,tmp2,tmp1+tmp2);
printf("==== systime= %.3f uncorrected_time=%.3f n=%d sps=%.4f\n",
   SystTime, Channel[chan].uncorrected_time, ninp, Channel[chan].srate);
time_asc4(s1, ExtPulse);
printf("-- %.3f %s\n", ExtPulse, s1);
*/

/* Calculate systeme time (double) */
    tmp1 = systtime;
    tmp2 = systmsec;
    tmp2 = (BasicSamprate == BASIC_SAMPRATE1) ? tmp2/1000. : tmp2/640.;
    SystTime = tmp1 + tmp2;

/* Calculate external pulse time (double) */
    if (exttime != 0)
    {
        tmp1 = exttime;
        tmp2 = extmsec;
        tmp2 = (BasicSamprate == BASIC_SAMPRATE1) ? tmp2/1000. : tmp2/640.;
        ExtPulse = tmp1 + tmp2;
        get_secs(ExtPulse, &Observ_dt);
        if (Observ_dt > 29.0) Observ_dt -= 60.0;
    }

    ExtraTcorr = 0.0;
    if (opt.do_time == FALSE)
    {

    /*
     * Load just once dft structure list from disk ascii file
     */
        if (foundDftFiles < 0 && dft_head == NULL)
        {
            readDftFiles();
            if (opt.use_database == TRUE) readXtcorrFile();
        }

    /*
     * EXTRA TIME CORRECTION cvtit database
     * EXTRA TIME CORRECTION IS APPLIED IN GetOutputDataTime()
     */
        if (opt.use_database == TRUE)
            ExtraTcorr = GetDbExtraTcorr(SystTime, 0);
        if (1)
        {
            if (ExtraTcorr != prev_ExtraTcorr)
            {
                printf("  ==== processTimeFrame: extra_tcorr=%.1f   @ ofs=%d\n",
                        ExtraTcorr, ttell(Fp_tit));
            }
            prev_ExtraTcorr = ExtraTcorr;
        }
    }

/*
 * Check for system time setting by time-out
 */
    if (flags.first_time_frame == FALSE && ExtPulse != prev_ExtPulse)
    {
      if (time_out_flag)
      {
/*
          char s1[40], s2[40];
          if (TIME_OK(SystTime)) time_asc4(s1, SystTime);
          else                   sprintf(s1,"UNKNOWN");
          if (TIME_OK((double) exttime)) time_asc4(s2, (double) exttime);
          else                   sprintf(s2,"UNKNOWN");
          fprintf(Fp_log,"  ===========================================\n");
          fprintf(Fp_log,"  WARNING: processTimeFrame: time set by time-out\n");
          fprintf(Fp_log,"  ext time %.19s   syst time %.19s\n", s1, s2);
          fprintf(Fp_log,"  TIME MAY BE WRONG\n");
          fprintf(Fp_log,"  ===========================================\n");
*/
          flags.treset_time_out = TRUE;
      }
    }
    prev_ExtPulse = ExtPulse;

/*
 * Set and save uncorrected start time of input data.
 * SystTime is the time of the last sample read in the last data frame.
 * Then:  Start_time = system_time - (n_samples - 1) * delta.
 * Eventually discard short data blocks and recalculate start time.
 */

    for (chan=0; chan<NCHAN; chan++) if (Channel[chan].numcomp)
    {

        ninp = (Channel[chan].ninp == 0) ? 0 : (Channel[chan].ninp-1);
        Channel[chan].uncorrected_time =
            SystTime - (double) ninp / Channel[chan].srate;
        Channel[chan].start_time = (double) UNKNOWN;

        if (1 && opt.discard_short_blk)
        {
            int expected_ninp = PRIM_NUM_DATA / Channel[chan].decim;

            if (Channel[chan].ninp < expected_ninp)
            {
               fprintf(Fp_log,"  WARNING: processTimeFrame: chan %d: ",
                              chan);
               fprintf(Fp_log,"SHORT DATA BLOCK DISCARDED  ofs %d\n",
                              ttell(Fp_tit));
               fprintf(Fp_log,"           Expecting %d samples, got %d\n",
                              expected_ninp, Channel[chan].ninp);
               if (opt.verb > 0)
               {
                   fprintf(Fp_log,"           uncorrected_time %.4f ",
                                  Channel[chan].uncorrected_time);
                   fprintf(Fp_log,"expected: %d samples, got %d\n",
                                  expected_ninp, Channel[chan].ninp);
               }

           /*
            * Set num of input sample to zero
            */
               Channel[chan].ninp = 0;

           /*
            * Set start time to the time of the first sample
            * of the next data block, not received yet.
            */
               Channel[chan].uncorrected_time =
                   SystTime + (double) 1 / Channel[chan].srate;

               if (opt.verb > 0)
                   fprintf(Fp_log,"       new uncorrected_time %.4f\n",
                                  Channel[chan].uncorrected_time);
            }
        }
    }

/*
 * If we have got the info frames and if this is
 * the first time frame, do some printing
 */
    if (flags.first_time_frame == TRUE && 
        flags.first_info_frame == FALSE)
    {
        /* FirstSystTime is used by xtit, not cvtit */
        FirstSystTime = SystTime;
        if (opt.verb > 0)
        {
            printInfo(Fp_log);
            decode_infos(&Info[0][0], TRUE);
            fprintf(Fp_log, "\n");
        }
        flags.first_time_frame = FALSE;
    }
    LastSystTime = SystTime;
    flags.TimeFrame = TRUE;
    return 0;
}

/*==================================================================*
 * EXTRA TIME CORRECTION TYPE 1 from cvtit database
 * There are to way to set the extra_time correction:
 *   1- The dft file in the extra_tcorr file correspond to an
 *      existing file in the dft database dir.
 *   2- Same condition than previous but in addition, the time
 *      must fall between the time limits of the dft file.*
 *
 * For the scheme 1, the extra time correction is applied to the
 * whole titan file.
 * For the scheme 2, it may happen that the extra time correction
 * will be applied for some segments of the titan file.
 * 
 * The present code uses the scheme 1.
 *==================================================================*/
static double GetDbExtraTcorr(time, print)
double time;
int    print;
{
struct dft_list *pdft = NULL;
struct xtc_list *pxtc = NULL;
double corr;

    corr = 0.0;

    /* loop thru all dft structures list */

    if (foundDftFiles) for (pdft=dft_head; pdft!=NULL; pdft=pdft->next)
    {

        /* loop thru all extra time corr structures list */

        for (pxtc=xtc_head; pxtc!=NULL; pxtc=pxtc->next)
        {

            /* if filenames match, get extra time corr */

            if (strstr(pdft->dft_name, pxtc->dft_name))
            {
/* Scheme 1 */
               if (FALSE)
               {
                   corr = pxtc->tcorr;
               }
/* Scheme 2 */
               else if ((int)time >= pdft->beg_systime &&
                        (int)time <  pdft->end_systime)
               {
                   corr = pxtc->tcorr;
               }
            }
        }
        if (print)
        {
            printf("++++ GetDbExtraTcorr: %d %d %d %.1f\n",
                   (int) time, pdft->beg_systime, pdft->end_systime, corr);
        }
    }
    return corr;
}


/*==================================================================*/
static void printInfo(Fp)
FILE * Fp;
{
char        time[40];
double      estim_dt = 0.0;
int         chan;
char        *dftfile = NULL;

    if (Fp == NULL) return;

/* Get estimated delta time */

    estim_dt = get_smoothed_dt(SystTime, Station, &dftfile);

    fprintf(Fp,"\n");
    fprintf(Fp,"station '%s' \n", Station);

/* ======== System time */
    time_asc4(time, SystTime);
    fprintf(Fp,"first system time              %s\n", time);

/* ======== External pulse time */
    if (ExtPulse == (double) UNKNOWN)
        sprintf(time, "??.??.??-??:??:??.???");
    else 
        time_asc4(time, ExtPulse);
    fprintf(Fp,"time of first external pulse   %s\n", time);

/* ======== System time reset */
    if (ResetTime.curr == (double) UNKNOWN)
    {
        sprintf(time, "??.??.??-??:??:??.???");
        fprintf(Fp,"system time reset              %s\n", time);
    }
    else 
    {
        time_asc4(time, ResetTime.curr);
        fprintf(Fp,"system time reset              %s (%s)\n",
            time, (ResetTime.reboot ? "reboot" : "synchro"));
    }

/* ======== Observed delta time */
    fprintf(Fp,"external delta time   observed ");
    if (Observ_dt == (double) UNKNOWN)
        fprintf(Fp,"?????? ");
    else
        fprintf(Fp,"%+.3f ", Observ_dt);

/* ======== Estimated delta time */
    if (estim_dt == (double) UNKNOWN)
        fprintf(Fp,"estim ??????\n");
    else
        fprintf(Fp,"estim %.3f", estim_dt);
    fprintf(Fp,"\n");
    if (dftfile)
        fprintf(Fp,"drift file %s\n", dftfile);

/* ======== Channel information */
    for (chan=0; chan<NCHAN; chan++) if (Channel[chan].numcomp)
    {
      fprintf(Fp,"channel %d      sample rate     %.8f sps\n",
          chan, Channel[chan].srate);
      fprintf(Fp,"               adc delay       %.3f\n",
          Channel[chan].adcdelay);
      fprintf(Fp,"               filter delay    %.3f\n",
          Channel[chan].filtdelay);
    }

    fprintf(Fp,"======================================================\n");
}


/*================================================================*/
int processDataFrame(frame)
char *frame;
{
int        compress;
int        delta[NCOMP][MAX_COMPRESSION];
int        i;
int        comp, ninp;
int        time_jump;
int        ncomp, current_ncomp;
double     samprate;
int        chan;
int        data;

    if (get_chan_parms(frame, &samprate, &chan) < 0)
    {
        return -1;
    }

    if (chan >= NCHAN) return 0;
    if (chan == 13) return 0;

/* Get compress flag */

    compress  = (int)(frame[10] & 0X0F);

/* Get number of components */

    ncomp = Channel[chan].numcomp;
    current_ncomp = ((frame[10] >> 4) & 0X01) ? 1 : 3;
    if (current_ncomp != ncomp)
    {
     fprintf(Fp_log,"  WARNING: processDataFrame: current_ncomp=%d ncomp=%d ???\n",
         current_ncomp, ncomp);
     Channel[chan].numcomp = current_ncomp;
     return 0;
/*
     if (compress != 1 && compress != 2 && compress != 3 &&
         compress != 6 && compress != 8)
         return 0;
*/
    }


/*
 * We just got a time frame. Things to do:
 *     - check compress flag is 1
 *     - check that number of data is not greater than 128
 *     - check data continuity and set time_jump flag
 *     - run output_data(): save and write current input data
 */
    if (flags.TimeFrame)
    {
/*
 * Check compress value
 */
        if (ncomp > 1 && compress != 1)
        {
          fprintf(Fp_log,"WARNING: processDataFrame: uncompressed data ");
          fprintf(Fp_log,"expected; found compress = %d\n", compress);
          return 0;
        }
        if (Channel[chan].ninp > PRIM_NUM_DATA)
        {
           fprintf(Fp_log,"  WARNING: processDataFrame: chan %d : ",chan);
           fprintf(Fp_log,"too many input data: %d; ", Channel[chan].ninp);
           fprintf(Fp_log,"max is %d ofs %d\n",PRIM_NUM_DATA,ttell(Fp_tit));
           Channel[chan].ninp = PRIM_NUM_DATA;
           return -1;
        }

/* Test */
if (0)  for (comp=0; comp<ncomp; comp++)
        {
          for (i=0; i<Channel[chan].ninp; i++)
          {
            if ((fabs)(inp_data[chan][comp][i]) > 8.38e+06)
            {
              int val = inp_data[chan][comp][i];
              printf("  chan=%d comp=%d sample=%d (%06X) @ ofs %d\n",
                  chan,comp,val,val,ttell(Fp_tit));
              break;
            }
          }
        }

/*
 * Check data continuity
 */
        time_jump = data_continuity(flags.first_data_frame);


if (0)
{
int ch;

    for (ch=0; ch<NCHAN; ch++) if (Channel[ch].numcomp)
    {
       if (opt.chan >= 0 && (ch != opt.chan)) continue;
       if (Channel[ch].nout == 0) continue;

       printf("==== chan=%d n=%d/%d out= %d uncorr_time %.4f start %.4f\n",
              ch, Channel[ch].ninp,
              PRIM_NUM_DATA / Channel[ch].decim,
              Channel[ch].nout,
              Channel[ch].uncorrected_time,
              OutData[ch].uncorrected_time);
    }
}


/*
 * Check output series origine time
 */
        if (time_jump == 0)
        {
            checkOutDataTime();
        }

/* Go write pending input data */

        output_data(!LAST, time_jump);

        flags.TimeFrame = FALSE;
        flags.first_data_frame = FALSE;

    }  /* end if (flags.TimeFrame) */



/*========  DECOMPRESS DATA FRAME ========*/

    decompress(frame, compress, delta, ncomp);

    ninp = Channel[chan].ninp;

/*
 *  1 component
 */
    if (ncomp == 1)
    {
/*
printf("++++ ncomp=1 ninp=%d  compress=%d\n", ninp, compress);
*/
        comp = 0;
        if (ninp == 0)
        {
            if (flags.first_data_frame == FALSE && compress != 0)
            {
              fprintf(Fp_log,"===== chan=%d ncomp=%d ", chan, ncomp);
              fprintf(Fp_log,"ninp=0 compress=%d ?????\n", compress);
              return -1;
            }
            inp_data[chan][comp][0] = delta[comp][0];
            Channel[chan].ninp = 1;
        }
        else
        {
            if (compress == 0)
            {
/*
printf("===== ncomp=1 ninp=%d  compress=0\n", ninp);
*/
                  inp_data[chan][comp][ninp] =
                  inp_data[chan][comp][ninp-1] + delta[comp][0];
                Channel[chan].ninp += 1;
            }
            else if (compress > 0 && compress < 9)
            {
                for (i=0; i<NCOMP*compress; i++)
                {
                    inp_data[chan][comp][ninp] = 
                      inp_data[chan][comp][ninp-1] + delta[comp][i];
                    ++ninp;
                    if (ninp > PRIM_NUM_DATA)
                    {
         fprintf(Fp_log,"  WARNING: processDataFrame: chan %d : too many ",chan);
         fprintf(Fp_log,"input data: %d; ", ninp);
         fprintf(Fp_log,"max is %d ofs %d\n",PRIM_NUM_DATA,ttell(Fp_tit));
         fprintf(Fp_log,"  THIS SHOULD HAVE BEEN CAUGHT BEFORE\n");
         return -1;
                    }
                }
                Channel[chan].ninp += (NCOMP*compress);
            }
        }
        return 0;
    }  /* end if (ncomp == 1) */

/*
 *  3 components
 */
    else
    {
        if (ninp == 0)
        {
            if (flags.first_data_frame == FALSE && compress != 1)
            {
              fprintf(Fp_log,"  processDataFrame: chan=%d ncomp=%d ",chan,ncomp);
              fprintf(Fp_log,"ninp=0 compress=%d ?????  ", compress);
              fprintf(Fp_log,"@ ofs %d\n", ttell(Fp_tit));
            }

            for (comp=0; comp<ncomp; comp++)
            {
                inp_data[chan][comp][0] =
                   delta[comp][0] +
                   Channel[chan].offset[comp];
            }
            Channel[chan].ninp = 1;
        }

        else if (compress > 0 && compress < 9)
        {
          for (i=0; i<compress; i++)
          {
            for (comp=0; comp<ncomp; comp++)
            {
/*
                inp_data[chan][comp][ninp] =
                   inp_data[chan][comp][ninp-1] +
                   delta[comp][i];
*/
/*
 * Check that samples stay within 24 bits range.
 * This bug should be corrected by Agecodagis someday.
 */
                data = inp_data[chan][comp][ninp-1] + delta[comp][i];
                if (data >  0X7FFFFF) data =  0X7FFFFF;
                if (data < -0X7FFFFF) data = -0X7FFFFF;
                inp_data[chan][comp][ninp] = data;
            }
            ++ninp;
          }
          Channel[chan].ninp += compress;
        }
        else
        {
          fprintf(Fp_log,"  processDataFrame: chan=%d ncomp=%d ",chan,ncomp);
          fprintf(Fp_log,"ninp=0 compress=%d ?????  ", compress);
          fprintf(Fp_log,"@ ofs %d\n", ttell(Fp_tit));
        }

    }  /* end (ncomp == 3) */

    return 0;
}


/*===================================================================*/
static void checkOutDataTime()
{
    int ch;
    double n;

    for (ch=0; ch<NCHAN; ch++) if (Channel[ch].numcomp)
    {
      if (opt.chan >= 0 && (ch != opt.chan)) continue;

      if ((Channel[ch].ninp == 0) ||
          (OutData[ch].nsamples == 0) ||
          (OutData[ch].uncorrected_time == 0.0)) continue;

      n = (Channel[ch].uncorrected_time -
           OutData[ch].uncorrected_time) * Channel[ch].srate;
      if (fabs(n - (double) Channel[ch].nout) > 0.001)

      {
          fprintf(Fp_log,"\n  ERROR: checkOutDataTime: ");
          fprintf(Fp_log,"TIMING ERROR FOR OUTPUT DATA SERIES ");
          fprintf(Fp_log,"CHANNEL %d\n", ch);
          fprintf(Fp_log,"  THIS SHOULD NEVER HAPPEND!!!\n");
          fprintf(Fp_log,"  Please mail to Jean-Francois.Fels@cnes.fr\n");
          fprintf(Fp_log, "\n");
  
          fprintf(Fp_log,"  chan=%d ninp=%d/%d\n",
                         ch, Channel[ch].ninp,
                         PRIM_NUM_DATA / Channel[ch].decim);
          fprintf(Fp_log,"  current time          %.4f\n",
                         Channel[ch].uncorrected_time);
          fprintf(Fp_log,"  out series start time %.4f\n",
                         OutData[ch].uncorrected_time);
          fprintf(Fp_log,"  expected num samples %.1f\n",
                         n);
          fprintf(Fp_log,"  got                  %d\n",
                         Channel[ch].nout);
          fprintf(Fp_log, "\n");
          exit(1);
      }
    }
}

/*===================================================================*
    Decode 32 (MAX_INFO_FRAME) 12 bytes info frames.
    Station name: read the 3 first chars of the field, discard others.
 *===================================================================*/
int processInfoFrames(frame, caller)
char *frame;
char *caller;
{
static char info_frame[MAX_INFO_FRAME][12];
static int info_frame_num;
int    sync;
int    frame_type;
int    chan;
int    i, l;
int    ltime;
int    new_config;
int    new_time_reset;
int    time_reset_flag;
static int prev_time_reset_flag = -1;
 

/* Inits */

    if (frame[10] == 0)
    {
        memset((char*) info_frame, 0, (MAX_INFO_FRAME*12));
        info_frame_num  = 0;
    }
    new_config = FALSE;

    GET_SYNC;
    GET_FRAME_TYPE;
    if (frame[10] < 0 || frame[10] > LAST_INFO_FRAME)
    {
      info_frame_num  = 0;

      fprintf(Fp_log,"  WARNING: processInfoFrames (%s) wrong frame number\n",
            caller);
      fprintf(Fp_log,"     frame %2d num %2d ",
            info_frame_num, frame[10]);
      fprintf(Fp_log,"sync %02X frame type %d  @ ofs %d\n",
            sync, frame_type, ttell(Fp_tit));
      return FALSE;
/*      flags.first_info_frame = TRUE; */
    }

    memcpy(&info_frame[(int) frame[10]], frame, 12);

    if (++info_frame_num <= LAST_INFO_FRAME) return 2;

/*
 * HERE WE HAVE GOT ALL INFO FRAMES.
 * --------------------------------
 * Check for changes in:
 *    - acq system config --> INFO_FRAME_20
 *    - digitizer config ---> CMD_00 to CMD_03
 *    - station id ---------> CMD_07
 *    - time_reset ---------> CMD_15
 * If station configuration is found to have changed, return false.
 * A new time reset is not considered as a configuration change.
 */

/*
 * Save info frames into Info[]
 */
    for (i=0; i<MAX_INFO_FRAME; i++)
    {
        if (flags.first_info_frame || Info[i][10] < 0)
        {
             memcpy(Info[i], info_frame[i], 11);
        }
    }


/*==============================================================
 *  INFO_FRAME_20    Acquisition system configuration
 *=============================================================*/

    for (i=0; i<10; i++)
    {
        if (i==2 || i>7) continue;
        if (info_frame[INFO_FRAME_20][i] != Info[INFO_FRAME_20][i])
        {
          new_config = TRUE;
          break;
        }
    }
    if (new_config)
    {
        fprintf(Fp_log,"  WARNING: processInfoFrames: new acq config: ");
        pr_frame(info_frame[INFO_FRAME_20]);
        fprintf(Fp_log,"                                         old: ");
        pr_frame(Info[INFO_FRAME_20]);
    }



/*==============================================================
 *  CMD_00 to CMD_03    Digitizer parameters for channels 0 to 3
 ==============================================================*/

/* Check for channels on_off flag changes */

    for (chan=0; chan<4; chan++)
    {
/*
printf("  processInfoFrames: chan %d on-off %d\n",
        info_frame[chan][10], info_frame[chan][8]);
*/
        if (info_frame[chan][8] != Info[chan][8])
        {
          acq.on_off[chan] = info_frame[chan][8];
          fprintf(Fp_log,
            "  WARNING: processInfoFrames: new on_off flag: %d chan %d\n",
             info_frame[chan][8], chan);
          new_config = TRUE;
        }
    }

/* Check for channels absOffset changes */

    for (chan=0; chan<4; chan++)
    {
        if ((info_frame[chan][9] & 0xFF) != (Info[chan][9] & 0xFF))
        {
          STA_infos.chan[chan].absOffset = (int) Info[chan][9] & 0xFF;
          fprintf(Fp_log,
          "  WARNING: processInfoFrames: new absOffset flag: %d chan %d\n",
             (info_frame[chan][9] & 0xFF), chan);
          new_config = TRUE;
        }
    }


/*==============================================================
 *  CMD_07    Station identificator  (last command received)
 *=============================================================*/

    if (strncmp(Info[CMD_07], info_frame[CMD_07], 3))
    {
        fprintf(Fp_log,"  WARNING: processInfoFrames: new station ");
        fprintf(Fp_log,"'%.3s' old '%.3s'\n",
                        info_frame[CMD_07], Info[CMD_07]);
        new_config = TRUE;
    }



/*==============================================================
 *  CMD_15    Time setting    (last command received)
 *=============================================================*/

 /*  Check for reboot: byte 8 */

    time_reset_flag = info_frame[CMD_15][8];

    if (prev_time_reset_flag < 0)
    {
        if (0) printf("------------ time_reset_flag=%d\n", time_reset_flag);
        prev_time_reset_flag = time_reset_flag;
    }

    if (time_reset_flag != prev_time_reset_flag)
    {
        if (0) printf("++++++++++++ time_reset_flag=%d\n", time_reset_flag);
        prev_time_reset_flag = time_reset_flag;
    }


/* Check for new system time setting: compare bytes 0 to 4 */

    new_time_reset = FALSE;
    for (i=0; i<4; i++)
    {
        if (info_frame[CMD_15][i] != Info[CMD_15][i])
        {
          new_time_reset = TRUE;
          break;
        }
    }

    if (new_time_reset)
    {

    /* Save time setting frame into Info[CMD_15] */
        memcpy(Info[CMD_15], info_frame[CMD_15], 11);

    /* If realistic, save new time reset and save type of time setting */
        ltime = bytes2int4(Info[CMD_15][0], Info[CMD_15][1],
                           Info[CMD_15][2], Info[CMD_15][3]);
        if (ltime > 1000000)
        {
            ResetTime.curr = (double) ltime;
            ResetTime.reboot = time_reset_flag;
        }
    }


/* NEW NEW NEW Print full info, eventually decimated */

    if (opt.info.on && flags.first_info_frame == FALSE)
    {
        static  int icnt;
        if (!(icnt % opt.info.decim))
        {
            decode_infos(&info_frame[0][0], TRUE);
        }
        ++icnt;
    }


/*===================================================
 * If station configuration has changed, return false
 *===================================================*/

    if (new_config)
    {
      flags.first_info_frame = TRUE;
      return FALSE;
    }





/* Upon initialization, store some of the parameters */

    if (flags.first_info_frame == TRUE)
    {

      acq.NumChan = Info[INFO_FRAME_20][0];
      if (acq.NumChan < 1 || acq.NumChan > 6)
      {
          fprintf(Fp_log,"  WARNING: processInfoFrames: ");
          fprintf(Fp_log,"unsupported number of channels: %d\n",acq.NumChan);
      }

      acq.AdcType = Info[INFO_FRAME_20][1];
      if      (acq.AdcType == CRYSTAL_5323_22)
          acq.AdcDelay = (double) CRYSTAL_DEL;
      else if (acq.AdcType == CRYSTAL_5321_22)
          acq.AdcDelay = (double) CRYSTAL_DEL;
      else if (acq.AdcType == AD_7710)
          acq.AdcDelay = (double) AD_7710_DEL;
      else if (acq.AdcType == HI7190)
          acq.AdcDelay = (double) HI7190_DEL;
      else
      {
          fprintf(Fp_log,"  WARNING: processInfoFrames: ");
          fprintf(Fp_log,"unsupported ADC type\n");
          return FALSE;
      }

/*
 * Store channels on_off flags
 */
      for (chan=0; chan<4; chan++)
      {
          acq.on_off[chan] = Info[chan][8];
      }

/*
 * Store channel offset correction
 */

      for (chan=0; chan<4; chan++)
      {
          STA_infos.chan[chan].absOffset = (int) Info[chan][9] & 0xFF;
      }

/* Store digitizer id, i.e. owner code
 * This name is stored on 9 bytes at most
 */
      i=0;
      while(Info[INFO_FRAME_16][i] == ' ') i++;
      l=9-i;
      strncpy(acq.Id, (char*)&Info[INFO_FRAME_16][i], l);
      acq.Id[l]='\0';
      i=0;
      while ((*(acq.Id+i) != ' ') && i<l)
      {
        if ((*(acq.Id+i) == '.') || (*(acq.Id+i) == '-')) *(acq.Id+i) = '_';
        if (isalnum(*(acq.Id+i)) == FALSE)                *(acq.Id+i) = '\0';
        i++;
      }
      *(acq.Id+i) = '\0';

/*
 * Station name; SISMALP: 4 char max.
 */
      if (strlen(opt.station) != 0)
          sprintf(Station, "%s", opt.station);
      else
          sprintf(Station, "%.8s", Info[CMD_07]);
      trim(Station);
      check_staname(Station);
      if (opt.do_sismalp) Station[4] = '\0';
/*
      ucase(Station);
*/

/*
 * System reboot time
 */
      ltime = bytes2int4(Info[CMD_15][0], Info[CMD_15][1],
                         Info[CMD_15][2], Info[CMD_15][3]);
      if (ltime > 1000000)
      {
          ResetTime.curr = (double) ltime;
          ResetTime.reboot = time_reset_flag;
      }

/*
 * Get station parameters
 */
      if (strlen(opt.station))
      {
          if (stationDbParms == FALSE) ReadDbStation(Station);
      }

/*
 * Print config
 */
      if (opt.verb > 1 && opt.verb <= 2)
      {
        fprintf(Fp_log,"  =========================\n");
        fprintf(Fp_log,"  ACQUISITION CONFIGURATION\n");
        fprintf(Fp_log,"  Station       '%s'\n", Station);
        fprintf(Fp_log,"  Owner:        '%.9s'\n",  Info[INFO_FRAME_16]);
        fprintf(Fp_log,"  Soft Version  '%.10s'\n", Info[INFO_FRAME_17]);

        fprintf(Fp_log,"  Num of Chan:   %d\n",
              Info[INFO_FRAME_20][0]);
        fprintf(Fp_log,"  ADC:           %s\n",
            ((Info[INFO_FRAME_20][1]) ? "Analog Device" : "Crystal"));
        fprintf(Fp_log,"  Basic srate:   %.4f\n", BasicSamprate);
        fprintf(Fp_log,"  Recording:     %s\n",
            ((Info[INFO_FRAME_20][5]) ? "triggered" : "continuous"));
        fprintf(Fp_log,"  Flash Memory:  %d MB\n",
              Info[INFO_FRAME_20][6]);
        fprintf(Fp_log,"  =========================\n");
      }
    }

/* If we couln't get the ADC delay, initialization is not completed. */
/* Do not reset 'first_info_frame' and return false.                 */
/* We'll try next time         */

    if (acq.AdcDelay == (double) UNKNOWN) 
    {
      flags.first_info_frame = TRUE;
      return FALSE;
    }

/* Init done; return OK */

/*
    fprintf(Fp_log,"  processInfoFrames (%s) SUCCES\n", caller);
*/
    flags.first_info_frame = FALSE;
    return TRUE;
}


/*================================================================*
    Check station name
    Accepted chars: alphanumerical plus '_', '-', '.'
    A leading '-' is replaced by 'X'.
 *================================================================*/
void check_staname(str)
char *str;
{
int i;

  if (strlen(str) != 0)
  {
      for (i=0; i<strlen(str); i++)
      {
        if ( isalnum(str[i]) == FALSE &&
             str[i] != '_' &&
             str[i] != '-' &&
             str[i] != '.'
           )
          str[i] = 'X';
      }
      if (str[0] == '-') str[0] = 'X';
      return;
  }

  fprintf(Fp_log,"  WARNING: check_staname: empty ");
  fprintf(Fp_log,"station name; set to 'XXX'\n");
  sprintf(str, "XXX");

/*
  for (i=0; i<STANAMELEN; i++)
  {
    if (!isalnum(str[i]))
    {
      if (strlen(str) == 0)
      {
        fprintf(Fp_log,"  WARNING: check_staname: wrong char in ");
        fprintf(Fp_log,"station name; replaced by 'X'\n");
      }
      str[i] = 'X';
    }
  }
*/
}


/*================================================================*
    Process station coordinates from titanGPS

Evolution dans le format TITAN.
De nouvelles trames peuvent se trouver au milieu des donnees.
Elles servent a donner la position GPS.
Elles n'existeront que quand la station aura ete connectee a un 
TITANGPS lors de l'enregistrement.
Leur periode actuelle de repetition est de 30 secondes mais pourra changer.
Ne pas en tenir compte de cette periodicite dans les softs.

MISCELLANEOUS FRAMES

These frames are used to multiplex miscellaneous informations with data.
The frame's structure is the following:

     MF1  MF2  MF3  MF4  MF5  MF6  MF7  MF8  MF9  IT  NU SYN

MF1..MF9 : Miscellaneous informations
IT       : Information type: 0..255 and give the meaning of MF1 ..MF9 bytes
NU       : Null byte. Set to 0

IT=0: GPS LATITUDE AND LONGITUDE
      MF1,MF2,MF3,MF4: LATITUDE, Long msb first.
            LATITUDE (+-PI/2) = MF1..MF4*10-8
      MF5,MF6,MF7,MF8: LONGITUDE, Long msb first.
            LONGITUDE (+-PI/2) = MF5..MF8*10-8
      MF9 null

IT=1: GPS ELEVATION
      MF1,MF2,MF3,MF4: ELEVATION, Long msb first.
            ELEVATION (meters = MF1..MF4*10-2
      MF5,MF6,MF7,MF8 null
      MF9 null

SYN  = synchro frame:
          b3=0
          b210 bits : type = 110  miscellaneous frames

 *================================================================*/
void processMiscFrames(frame, last)
char *frame;
int last;
{
int    w[2];
char   str[40];
static double lat, lon, elev;
static double prev_lat, prev_lon, prev_elev;
static double minute[2], sec[2];
static FILE *Fpcoord;
static int found_frames0_1;

/*
 * Last call: close file and return; nothing to do.
 */

    if (last)
    {
        if (Fpcoord) { fclose(Fpcoord); Fpcoord = NULL; }
        return;
    }
 
    w[0] = bytes2int4(frame[0],frame[1],frame[2],frame[3]);
    w[1] = bytes2int4(frame[4],frame[5],frame[6],frame[7]);

/*
 * Process 1st frame: latitude and longitude
 */
    if (frame[9] == 0)
    {
        lat = (double)w[0] * 1.e-8 * 180.0 / 3.14159;
        lon = (double)w[1] * 1.e-8 * 180.0 / 3.14159;
        minute[0] = (lat - (int) lat) * 60.0;
        sec[0] = (minute[0] - (int) minute[0]) * 60.0;
        minute[1] = (lon - (int) lon) * 60.0;
        sec[1] = (minute[1] - (int) minute[1]) * 60.0;
        found_frames0_1 = FALSE;
    }
/*
 * Process 2nd frame: elevation
 */
    else if (frame[9] == 1)
    {
        elev = (double)w[0]*0.01;
        found_frames0_1 = TRUE;

        STA_infos.coord.lat = lat;
        STA_infos.coord.lon = lon;
        STA_infos.coord.elev = elev;

if (0)
{      printf("==== processMiscFrames: lat=%.4f (%2d %2d %2d), ",
                 lat, (int) lat, (int) minute[0],  (int) sec[0]);
       printf(" lon=%.4f (%2d %2d %2d), ",
                 lon, (int) lon, (int) minute[1],  (int) sec[1]);
       printf(" %d m ofs=%d\n", (int) elev, ttell(Fp_tit));
}
    }

    if (opt.do_coord == FALSE) return;

    if (found_frames0_1 == FALSE) return;

/*
 * Open .pos coordinates file: create or append to existing file
 */

    if (Fpcoord == NULL)
    {
        char fname[255];

        time_asc4(str, SystTime);
        sprintf(fname,"%.19s.%s.pos", str, Station);

        fprintf(Fp_log,"  processMiscFrames: opening '%s'\n", fname);

        fcreat_append(fname, &Fpcoord); fseek(Fpcoord, 0L, SEEK_END);

        fprintf(Fpcoord,
       "#    date-time          Utime         lat       lon      elev\n");
        fprintf(Fpcoord,
       "#------------------   ---------   ---------- --------- ---------\n");
    }

    if ((lat < 0.0) || (lat > 360.0) ||
        (elev > 10000.0) || (elev < -1000.0))
    {
        return;
    }

/*
 * If no position change, discard and return;
 */
    if ((prev_lat == lat) && (prev_lon == lon) && (prev_elev == elev))
    {
        return;
    }
    time_asc4(str, SystTime);
    fprintf(Fpcoord, "%.19s   %d    %9.5f %9.5f %9.5f stat=%d\n",
            str, (int)SystTime, lat, lon, elev, STA_infos.flush_status);
    fflush(Fpcoord);

    prev_lat = lat;
    prev_lon = lon;
    prev_elev = elev;
}


/*================================================================*/
void processOffsetFrame(frame, firstTime)
char *frame;
int  firstTime;
{
#define RELATIVE 0
#define ABSOLUTE 1
int     w[NCOMP];
int     offsetType;
int     chan, comp;
int     corrType;
int     ofs[NCOMP];
char    typeStr[16];
int     dbug = 0;

/* Get channel and type of offset frame */

    chan = (frame[9] >> 4) & 0XF;
    offsetType = (frame[10] >> 5) & 0X1;

    w[0] = bytes2int4(0,frame[0],frame[1],frame[2]);
    w[1] = bytes2int4(0,frame[3],frame[4],frame[5]);
    w[2] = bytes2int4(0,frame[6],frame[7],frame[8]);

    for (comp=0; comp<NCOMP; comp++)
    {
        ofs[comp] = (w[comp] & 0XFFFFFF);
        ofs[comp] <<= 8;
        ofs[comp] >>= 8;
    }

/* Set correction type */

    corrType = DataOffsetCorr;

/* If offset correction specified, overwrite correction type */

    if      (opt.do_offset == NO_OFS)  corrType = NO_OFS;
    else if (opt.do_offset == REL_OFS) corrType = REL_OFS;
    else if (opt.do_offset == ABS_OFS) corrType = ABS_OFS;

    if      (corrType == NO_OFS)  sprintf(typeStr, "NONE");
    else if (corrType == REL_OFS) sprintf(typeStr, "RELATIVE");
    else if (corrType == ABS_OFS) sprintf(typeStr, "ABSOLUTE");

    if (offsetType == RELATIVE)
    {
      for (comp=0; comp<NCOMP; comp++)
          Channel[chan].rel_ofs[comp] = ofs[comp];
    }
    else if (offsetType == ABSOLUTE)
    {
      for (comp=0; comp<NCOMP; comp++)
          Channel[chan].abs_ofs[comp] = ofs[comp];
    }

    if (firstTime)
    {

      if (dbug || opt.verb>0)
      {
          static int printmsg = 1;
          if (printmsg)
          {
              printmsg = 0;
              printf("  processOffsetFrame: first time: ");
              printf("requested correction %s\n", typeStr);
          }
      }
      
      if (offsetType == RELATIVE)
      {
        if (dbug)
        {
          printf("    chan %d rel ofs ", chan);
          for (comp=0; comp<NCOMP; comp++)
              printf("%+7d ", Channel[chan].rel_ofs[comp]);
          printf("\n");
        }
      }
      else if (offsetType == ABSOLUTE)
      {
        if (dbug)
        {
          printf("    chan %d abs ofs ", chan);
          for (comp=0; comp<NCOMP; comp++)
              printf("%+7d ", Channel[chan].abs_ofs[comp]);
          printf("\n");
        }

        if      (corrType == NO_OFS)  for (comp=0; comp<NCOMP; comp++)
            Channel[chan].offset[comp] = 0;
        else if (corrType == REL_OFS) for (comp=0; comp<NCOMP; comp++)
            Channel[chan].offset[comp] = 0;
        else if (corrType == ABS_OFS) for (comp=0; comp<NCOMP; comp++)
            Channel[chan].offset[comp] = Channel[chan].abs_ofs[comp] -
                                         Channel[chan].rel_ofs[comp];

        if (dbug)
        {
          printf("    chan %d offset  ", chan);
          for (comp=0; comp<NCOMP; comp++)
              printf("%+7d ", Channel[chan].offset[comp]);
          printf("\n");
        }
      }
      return;
    }


    if (offsetType == ABSOLUTE)
    {
        if      (corrType == NO_OFS) for (comp=0; comp<NCOMP; comp++)
            Channel[chan].offset[comp] = 0;
        else if (corrType == REL_OFS) for (comp=0; comp<NCOMP; comp++)
            Channel[chan].offset[comp] += Channel[chan].rel_ofs[comp];
        else if (corrType == ABS_OFS) for (comp=0; comp<NCOMP; comp++)
            Channel[chan].offset[comp] = Channel[chan].abs_ofs[comp];
/*
        printf("  processOffsetFrame:\n");
        printf("    %s corr chan %d; offset ",typeStr,chan);
        for (comp=0; comp<NCOMP; comp++)
            printf("%+7d ", Channel[chan].offset[comp]);
        printf(" samp %6d %3d\n", Channel[chan].nout, Channel[chan].ninp);
*/
    }
}


/*==================================================================*
    flushInputData
    Flush the input data buffers to prevent overflow.
    Data are just discarded.
 *==================================================================*/
void flushInputData(last, time_jump)
int last, time_jump;
{
int chan;
    for (chan=0; chan<NCHAN; chan++) if (Channel[chan].numcomp)
    {
      Channel[chan].tot_inp += Channel[chan].ninp;
      totOutSamples += Channel[chan].ninp;
      Channel[chan].ninp = 0;
      if (opt.verb)
      {
          if (time_jump)
             printf("  flushInputData: time jump; chan %d total %7d %7d\n",
              chan, Channel[chan].tot_inp, totOutSamples);
          if (last == LAST)
             printf("  flushInputData: error; chan %d total %7d %7d\n",
              chan, Channel[chan].tot_inp, totOutSamples);
      }
    }
}


/*================================================================*
    get_chan_parms()
    This is a very messy function where we try to handle any weird
    situation coming from changes in the configuration.
    Many updated where done.

    Get sample rate from byte 9 and 10 of data frame.
    - Byte 9:  Channel type and secondary channel samp_rate code
        - bit 7654 = channel (triplet) type
        - bit 3210 = samp. rate code, two'complement
              samprate = 31.25 * 2^exp
    - Byte 10  Primary channel samp_rate code
        - bit 65   = samp. rate code.

    Return values:
        sampfreq    sample rate for this data frame
        cha         channel (triplet) type
        return flag   TRUE       OK
                      FALSE      unsupported channel
                     -1          upon sample rate change
 *================================================================*/
int get_chan_parms(frame, sampfreq, cha)
char       *frame;
double     *sampfreq;
int        *cha;
{
int          srcode1;            /* Current prim sample rate code */
int          srcode2;            /* Current secd sample rate code */
double       factor1, factor2;   /* Basic sample rate factors     */
int          ncomp;
double       prev_primsrate;

/* Get channel type (also called triplet type or triplet ID:
       0 -> sensor 1, primary
       1 -> sensor 2, primary
       2 -> sensor 1, secondary
       3 -> sensor 2, secondary
*/
    *cha = (short) (frame[9] >> 4) & 0XF;
    if (*cha < 0 || *cha > 3) return 0;

    ncomp = ((frame[10] >> 4) & 0X01) ? 1 : 3;

/* Get bits 65 of 'Taux' byte */
    srcode1 = (frame[10] >> 5) & 0X03;

/* Get bits 3210 of 'Fech' byte and check two's complement */
    srcode2 = (frame[9] & 0x0F);
    if (srcode2 > 0X7) srcode2 -= 0x10;


if (0) printf("get_chan_parms: cha %d n %d rate code %d %d\n",
           *cha, ncomp, srcode1, srcode2);


/* PRIMARY FREQUENCY */

    prev_primsrate = acq.PrimSrate;
    if (srcode1 == srcode2)
    {

/* First time: store channel params */
      if (Channel[*cha].numcomp == 0)
      {
        Channel[*cha].ratecode = srcode1;

        if (srcode1 > 0) factor1 = (double) (1 << srcode1);
        else             factor1 = 1.0 / (double) (1 << -srcode1);
        acq.PrimSrate = BasicSamprate * factor1;

if (opt.srate != 0)
{
   double ratio;
   ratio = opt.srate / acq.PrimSrate;
printf("===== prim: acq.PrimSrate %.2f --> ", acq.PrimSrate);
   acq.PrimSrate *= ratio;
printf("%.2f\n", acq.PrimSrate);
}

        Channel[*cha].srate = acq.PrimSrate;
        Channel[*cha].decim = 1;
        Channel[*cha].filtdelay = 0.0;
        Channel[*cha].adcdelay = acq.AdcDelay / acq.PrimSrate;
        Channel[*cha].numcomp = ncomp;

        if (opt.verb > 1 && opt.verb <= 2)
            fprintf(Fp_log,
                "  get_chan_parms: chan %d ncomp %d Prim freq: %.4f\n",
                *cha, ncomp, Channel[*cha].srate);
      }


/* Modif JFF 10 dec 97 */
      if (ncomp != Channel[*cha].numcomp)
      {
          fprintf(Fp_log,
                  "\n  WARNING: get_chan_parms: num comp has changed; ");
          fprintf(Fp_log,"was %d  now %d\n", Channel[*cha].numcomp, ncomp);
          return -1;
      }


      if (prev_primsrate != 0.0 && prev_primsrate != acq.PrimSrate)
      {
          fprintf(Fp_log,"\n  WARNING: get_chan_parms: primary ");
          fprintf(Fp_log,"sample frequency has changed: \n");
          fprintf(Fp_log,"           was %.3f  now %.3f\n",
                  prev_primsrate, samprate(frame));
          return -1;
      }

      if (Channel[*cha].srate != (double) UNKNOWN &&
          Channel[*cha].ratecode != srcode1)
      {
          fprintf(Fp_log,"\n  WARNING: get_chan_parms: primary ");
          fprintf(Fp_log,"sample frequency has changed: \n");
          fprintf(Fp_log,"           was %.3f  now %.3f\n",
              Channel[*cha].srate, samprate(frame));
          return -1;
      }

/* IF TWO SENSORS, PARAMS ARE ASSUMED TO BE IDENTICAL ??? !!! */
      if (*cha == 1)
      {
          if (Channel[1].srate == (double) UNKNOWN)
              memcpy(&Channel[1], &Channel[0], sizeof (struct Channel));
      }

      *sampfreq = Channel[*cha].srate;
      return TRUE;
    }


/* SECONDARY FREQUENCY */

    if (Channel[*cha].numcomp == 0)
    {
    /* First time: store channel params */
        Channel[*cha].ratecode = srcode2;

        if (srcode1 > 0) factor1 = (double) (1 << srcode1);
        else             factor1 = 1.0 / (double) (1 << -srcode1);
        acq.PrimSrate = BasicSamprate * factor1;
// acq.PrimSrate = 40.0 * factor1;

        if (srcode2 > 0) factor2 = (double) (1 << srcode2);
        else             factor2 = 1.0 / (double) (1 << -srcode2);
        Channel[*cha].srate = BasicSamprate * factor2;

if (opt.srate != 0)
{
   double ratio;
   ratio = opt.srate / acq.PrimSrate;
printf("===== secd: acq.PrimSrate %.2f --> ", acq.PrimSrate);
   acq.PrimSrate *= ratio;
printf("%.2f --> ", acq.PrimSrate);
   Channel[*cha].srate  *= ratio;
printf("srate %.2f\n", Channel[*cha].srate);
}

        Channel[*cha].decim = (short) (factor1 / factor2);

        if (Channel[*cha].decim <= 0)
        {
          fprintf(Fp_log,"\n  ERROR: get_chan_parms: wrong decim factor\n");
          Channel[*cha].decim = 1;
          return -1;
        }

/*
                  THIS DOESN'T WORK. SEE BELOW

        Channel[*cha].filtdelay =
                ((double) FIR_FILT_NCOEF - 1.0) / 2.0 *
                ((double) Channel[*cha].decim - 1.0) / acq.PrimSrate;
*/

        Channel[*cha].filtdelay =
                ((double) FIR_FILT_NCOEF + 1.0) / 2.0 *
                ((double) Channel[*cha].decim - 1.0) / acq.PrimSrate;

        Channel[*cha].adcdelay = acq.AdcDelay / acq.PrimSrate;
        Channel[*cha].numcomp = ncomp;

        if (opt.verb > 1 && opt.verb <= 2)
        {
          fprintf(Fp_log,"  get_chan_parms: chan %d ncomp %d ",
               *cha,
               ncomp);
          fprintf(Fp_log,"Secd freq: %.3f (Prim %.3f)\n",
               Channel[*cha].srate,
               acq.PrimSrate);
          fprintf(Fp_log,"                  decim: %d filt_delay: %.3f\n",
               Channel[*cha].decim,
               Channel[*cha].filtdelay);
        }
    }

    if (Channel[*cha].srate != (double) UNKNOWN &&
        Channel[*cha].ratecode != srcode2)
    {
      fprintf(Fp_log,"\n  WARNING: get_chan_parms: secondary ");
      fprintf(Fp_log,"sample frequency has changed: \n");
      fprintf(Fp_log,"           samp rate code was %d  now %d\n",
          Channel[*cha].ratecode, srcode2);
      return -1;
    }

    *sampfreq = Channel[*cha].srate;
    return TRUE;
}


/*================================================================*/
double samprate(frame)
char       *frame;
{
int          srcode1;
int          srcode2;
double       factor;
double       freq;

/* Get bits 65 of 'Taux' byte */
    srcode1 = (frame[10] >> 5) & 0X03;

/* Get bits 3210 of 'Fech' byte and check two's complement */
    srcode2 = (frame[9] & 0x0F);
    if (srcode2 > 0X7) srcode2 -= 0x10;

/* PRIMARY FREQUENCY */

    if (srcode1 < 0)
    {
        factor = (double) (1 << -srcode1);
        freq = BasicSamprate / factor;
    }
    else
    {
        factor = (double) (1 << srcode1);
        freq = BasicSamprate * factor;
    }

/* Here is the test to discriminate primary and secd. channels */

    if (srcode1 == srcode2) return freq;


/* SAVE SECONDARY FREQUENCY */

    if (srcode2 > 0)
    {
        factor = (double) (1 << srcode2);
        freq  = BasicSamprate * factor;
    }
    else
    {
        factor = (double) (1 << -srcode2);
        freq  = BasicSamprate / factor;
    }

    return freq;
}



/*================================================================*
 *    Check input data continuity between two consecutive
 *    time frames.
 *================================================================*/
int data_continuity(first)
int    first;
{
static  double prev_systime;
double  expected_diff;
double  diff;
int     time_jump;
int     chan;

    time_jump = FALSE;
    diff = SystTime - prev_systime;

    if (!first)
    {
      for (chan=0; chan<NCHAN; chan++)
      {
        if (acq.on_off[chan] == 1 && Channel[chan].numcomp)
        {
          expected_diff = Channel[chan].ninp / Channel[chan].srate;
/*
printf("  data_continuity: chan=%d %.3f n=%d sps=%.4f diff=%.4f exp=%.4f\n",
    chan, SystTime, Channel[chan].ninp,Channel[chan].srate,
    diff, expected_diff);
*/
          if (fabs(diff - expected_diff) > .008)
          {
            time_jump = TRUE;
            if ((diff <= expected_diff) && (opt.verb > 0))
            {
              fprintf(Fp_log,"  WARNING: data_continuity: chan %d OVERLAP: ",
                              chan);
              fprintf(Fp_log,"%.3f secs ", -(diff-expected_diff));
              fprintf(Fp_log,"(%d smp) ",
                      (int) (-(diff-expected_diff) * Channel[chan].srate));
              fprintf(Fp_log,"@ ofs %d\n", ttell(Fp_tit));
            }
            else if (opt.verb > 0)
            {
              fprintf(Fp_log,"  WARNING: data_continuity: chan %d GAP:     ",
                              chan);
              fprintf(Fp_log,"%.3f secs ", (diff-expected_diff));
              fprintf(Fp_log,"(%.1f smp) ",
                      /*(int)*/ ((diff-expected_diff) * Channel[chan].srate));
              fprintf(Fp_log,"@ ofs %d\n", ttell(Fp_tit));
            }
          }
        }
      }
    }

    prev_systime = SystTime;
    return (time_jump);
}



/*================================================================*/
void decompress(frame, compress, delta, ncomp)
char   *frame;
int     compress;
int     delta[NCOMP][MAX_COMPRESSION];
int     ncomp;
{
int    w[NCOMP];
int    mask;
int    shift;
int    shift_f;
int    ww;
int    i, comp;

    w[0] = bytes2int4(0,frame[0],frame[1],frame[2]);
    w[1] = bytes2int4(0,frame[3],frame[4],frame[5]);
    w[2] = bytes2int4(0,frame[6],frame[7],frame[8]);

    if (compress == 0)
    {
        delta[0][0] = (w[0] & 0XFFFFFF);
        delta[0][0] <<= 8;
        delta[0][0] >>= 8;
        return;
    }

    switch (compress)
    {
      case 1: mask = 0XFFFFFF;  shift =  1;  shift_f =  8; break;
      case 2: mask =    0XFFF;  shift = 12;  shift_f = 20; break;
      case 3: mask =     0XFF;  shift =  8;  shift_f = 24; break;
      case 4: mask =     0X3F;  shift =  6;  shift_f = 26; break;
      case 6: mask =      0XF;  shift =  4;  shift_f = 28; break;
      case 8: mask =      0X7;  shift =  3;  shift_f = 29; break;
      default:
          fprintf(Fp_log,"  WARNING: decompress: ");
          fprintf(Fp_log,"compression rate %d unsupported\n", compress);
          return;
    }

    if (ncomp == 1)
    {
      for (comp=0; comp<NCOMP; comp++)
      {
        ww = w[comp];
        for (i=comp*compress; i<(comp+1)*compress; i++)
        {
            delta[0][i] = (ww & mask);
            ww = (ww >> shift);
            delta[0][i] <<= shift_f;
            delta[0][i] >>= shift_f;
        }
      }
      return;
    }

    for (comp=0; comp<NCOMP; comp++)
    {
        ww = w[comp];
        for (i=0; i<compress; i++)
        {
             delta[comp][i] = (ww & mask);
             ww = (ww >> shift);
             delta[comp][i] <<= shift_f;
             delta[comp][i] >>= shift_f;
        }
    }
}


/*==================================================================*/
void clearOutputParms(chan)
int chan;
{
        memset(&OutData[chan], 0, sizeof(outData));
}



/*==================================================================* 
    saveOutputParms
    The purpose of this function is to store parameters relative to
    the beginning of data blocks. These parameters will be used later,
    when the end of data blocks is reached and the output headers
    are built.
 *==================================================================*/
void saveOutputParms(chan)
int chan;
{

/*
 * Variable OutData[chan].start_time is the corrected UTC time.
 * It will be set in the output data modules, in close_data_files()
 */

/*
    printf("==== saveOutputParms: chan %d n=%d uncorrected_time %.4f\n",
            chan, Channel[chan].ninp, Channel[chan].uncorrected_time);
*/
    OutData[chan].uncorrected_time = Channel[chan].uncorrected_time;
    OutData[chan].resettime        = ResetTime.curr;
    OutData[chan].reboot           = ResetTime.reboot;
    OutData[chan].extpulse         = ExtPulse;
    OutData[chan].observ_dt        = Observ_dt;
    OutData[chan].extra_tcorr      = ExtraTcorr;
    OutData[chan].adcdelay         = Channel[chan].adcdelay;
    OutData[chan].filtdelay        = Channel[chan].filtdelay;
    OutData[chan].numcomp          = Channel[chan].numcomp;
    OutData[chan].srate            = Channel[chan].srate;
    OutData[chan].decim            = Channel[chan].decim;
}


/*===================================================================*/
TITFILE *open_frd(fn)
char  *fn;
{
FILE *fp;
    if ((fp = fopen(fn, "rb")) == NULL) {
        fprintf(stderr, "\nERROR: open_rd: can't open %s\n", fn);
        exit(1);
    }
    return ((TITFILE *) fp);
}

/*==================================================================*/
void log_time(flag, init)
int flag;
int *init;
{
static double lmin =  2000000000.0;
static double lmax = -2000000000.0;
double        days;
char          beg[40], end[40];
int           chan;

    if (opt.do_time == FALSE) return;

    if (*init == TRUE)
    {
        *init = FALSE;
        lmin =  2000000000.0;
        lmax = -2000000000.0;
    }

    
    if (flag == !LAST )
    {
        if (!TIME_OK(SystTime))
            return;
        if (SystTime > lmax) lmax = SystTime;
        if (SystTime < lmin) lmin = SystTime;
        return;
    }


    if (flag == LAST)
    {
        if (TIME_OK(lmin)) time_asc4(beg, lmin);
        else
        {
          fprintf(Fp_log,"  WARNING: log_time: can't find min time\n");
          return;
        }
        if (TIME_OK(lmax)) time_asc4(end, lmax);
        else
        {
          fprintf(Fp_log,"  WARNING: log_time: can't find max time\n");
          return;
        }
    }
    else
    {
        fprintf(Fp_log,"ERROR: log_time: wrong flag: %d\n",flag);
        exit(1);
    }
    days = (lmax - lmin) / 86400.0;
    fprintf(Fp_log,"  log_time: %s %s %s ", Station, beg, end);
    if (days < 0.1) fprintf(Fp_log,"(%d secs) \n", (short) (lmax-lmin));
    else            fprintf(Fp_log,"(%.2f days) \n", days);

    for (chan=0; chan<NCHAN; chan++)
    {
        if (Channel[chan].numcomp)
        {
            fprintf(Fp_log,
                "  log_time: channel %d  %d comp %7.3f Hz %7d samples\n",
                chan,
                Channel[chan].numcomp,
                Channel[chan].srate,
                Channel[chan].tot_inp);
        }
    }
    fprintf(Fp_log,"  log_time: total samples %d\n", totOutSamples);
    fprintf(Fp_log,"\n");
}


/*================================================================*
    write_delta_time()
  Called by process_titan().
  Observed delta_t are stores and written to disk as follows:

     UTIME.STA.dt file:
     ascii file containing 2 values: date-time string of the system 
     time upon reception of an external time pulse and the delta-t
     whose value is forced to fall inside a +/- 30 sec interval.
     The file contains also the system time reset(s).
     NOTE THAT THE .dt IS NOT SORTED, in order to keep track of
     eventual system time going back.
     Example:
     1998.04.09-15:21:08.000 time reset (reboot)
     1998.04.09-15:24:00.000 new time reset
     1998.04.09-16:33:19.835 +19.8350

 *================================================================*/
void write_delta_time(last)
int   last;
{
static FILE    *Fp_dt;
char            str[40], str2[40];
static int      ndelta;
static double   prev_ExtPulse;
static double   prev_Observ_dt;
static char     path[255];

    if (opt.do_ah == TRUE)
    {
        if (opt.output_delta_t == FALSE) return;
    }
    else
    {
        if (opt.do_time == FALSE) return;
    }

/*
printf("dt  path  '%s'\n", db_paths.timedt);
printf("dft path  '%s'\n", db_paths.timedft);
*/     
    if (db_paths.timedt == NULL || !strlen(db_paths.timedt))
    {
        sprintf(path, ".");
    }
    else
    {
        if (!isdir(db_paths.timedt))
        {
            fprintf(stderr,"ERROR: write_delta_time: can't find dir %s\n",
                db_paths.timedt);
            exit(1);
        }
        sprintf(path, "%s/%s", db_paths.timedt, Station);
        if (!isdir(path))
        {
           printf("  write_delta_time: creating %s\n", path);
           if (mkdir(path, 0755) < 0)
           {
             fprintf(stderr,"ERROR: write_delta_time: can't create dir %s\n",
             path);
             exit(1);
           }
        }
    }
if (0) printf("++++ dt path  '%s'\n", path);

/* Open .dt delta_time file: create or append to existing file */

    if (Fp_dt == NULL)
    {
        time_asc4(str, SystTime);
        sprintf(dtfname,"%s/%.19s.%s.dt", path, str, Station);

        fprintf(Fp_log,"  write_delta_time: opening '%s'\n", dtfname);

        open_Fwr(dtfname, &Fp_dt);
/*
        fcreat_append(dtfname, &Fp_dt); fseek(Fp_dt, 0L, SEEK_END);
*/
    }

/* ECRITURE: 1ere remise a l'heure du fichier */

    if (ResetTime.prev == (double)UNKNOWN &&
        ResetTime.curr != (double)UNKNOWN)
    {
        ResetTime.prev = ResetTime.curr;

        time_asc4(str, ResetTime.curr);
        fprintf(Fp_dt, "%s time reset (%s)\n",
            str, (ResetTime.reboot ? "reboot" : "synchro"));
        fflush(Fp_dt);
    }

/* ECRITURE: heure interne de reception d'un top minute */

    if (last == !LAST)
    {
        if (TIME_OK(ExtPulse) && (ExtPulse != prev_ExtPulse))
        {
            time_asc4(str, ExtPulse);

            if (ResetTime.curr == (double)UNKNOWN)
            {
                ResetTime.prev = ResetTime.curr = ExtPulse;
                fprintf(Fp_dt, "%s  time reset (fake)\n", str);
                printf("%s  time reset (fake)\n", str);

                fflush(Fp_dt);
            }

            (Observ_dt == (double) UNKNOWN) ?
                sprintf(str2, "??????") :
                sprintf(str2, "%+.4f", Observ_dt);
            fprintf(Fp_dt, "%s %s\n", str, str2);
            fflush(Fp_dt);

            ++ndelta;
        }

/* ECRITURE: nouvelle remise a l'heure */

        if (ResetTime.curr != ResetTime.prev)
        {
            time_asc4(str, ResetTime.curr);
            fprintf(Fp_dt, "%s new time reset (%s)\n",
                str, (ResetTime.reboot ? "reboot" : "synchro"));
            fflush(Fp_dt);
        }
        prev_ExtPulse = ExtPulse;
        prev_Observ_dt = Observ_dt;
        ResetTime.prev = ResetTime.curr;
        return;
    }

/* Last call : close files */

    else if (last == LAST)
    {
        if (Fp_dt == NULL)
           fprintf(Fp_log,"\n\twrite_delta_time: WARNING: null pointer");
        else
        {
            fclose(Fp_dt);
            Fp_dt = NULL;
        }

        if (ndelta == 0)
        {
          fprintf(Fp_log,"\n\tWARNING: write_delta_time: ");
          fprintf(Fp_log,"no valid external pulse found\n\n");
          return;
        }
        return;
    }

    else
    {
        fprintf(Fp_log,"ERROR: write_delta_time: wrong 'last' flag value\n");
        exit(1);
    }
}


/*================================================================*
    get_output_name
        input    utime (double)
        output   name string
                 name length minus chan-comp field

  Returned name is based on the date-time.
  If option "daydir", output data are stored into directories
  named by Julian day. Dirname is included in the returned string.

  Examples:

      1998.05.16-02.18.42.XXX.0-0        returned len = 19 char
      R136/1998.05.16-02.18.41.XXX.2-1   returned len = 24 char

 *================================================================*/
int get_output_name(dbltime, name)
double dbltime;
char   *name;
{
char        str[50];
char        date[40];
int         ierr=0;
struct tm  *tms;
time_t      time;

    time = (time_t)dbltime;
    if (opt.daydir == TRUE)
    {
        tms  = gmtime(&time);
        sprintf(str,"R%03d",(tms->tm_yday+1));    
    
        if (!isdir(str))
        {
            ierr = mkdir(str,S_IREAD|S_IWRITE|S_IEXEC);
        }
        if (ierr == -1)
        {
            fprintf(stderr,"ERROR: day dirname: cannot create ");
            fprintf(stderr,"%s directory\n", str);
            exit(1);
        }
        time_asc4(date, dbltime);
        sprintf(name,"%s/%.19s.%s", str, date, Station);
        return(strlen(str)+1+19);
    }
    else
    {
        time_asc4(date, dbltime);
        sprintf(name, "%.19s.%s", date, Station);
        return(19);
    }
}


/*===================================================================*/
static void pr_frame(frame)
char *frame;
{
int i;

    for (i=0; i<12; i++)
    {
        if (!(i%4)) fprintf(Fp_log," ");
        fprintf(Fp_log,"%02X", frame[i] & 0XFF);
    }
    fprintf(Fp_log,"\n");
    return;
}


/*===================================================================*/
int HasStatDbParms()
{
    return(stationDbParms);
}


/*===================================================================*
  Database Station parameters
  lat lon elev (metres)
  Z_azim Z_dip
  N_azim N_dip
  E_azim E_dip

Examples:
---------
coordinates  42.76010 1.189400 100.0 
chan 0 comp 0 STS2 Z 0.00 -90.00
chan 0 comp 1 STS2 N 0.00  0.00
chan 0 comp 2 STS2 E 90.00  0.00
chan 1 comp 0 CMG5 Z 0.00 -90.00
chan 1 comp 1 CMG5 N 0.00  0.00
chan 1 comp 2 CMG5 E 90.00  0.00

begin_station PYAD 1998.10.27-14.35.00 2001.08.04-00.00.00
coordinates 45.20 05.73 250 0
chan 0 comp 0 CMG5 Z 0.00 -90.00
chan 0 comp 1 CMG5 N 0.00 0.00
chan 0 comp 2 CMG5 E 90.00 0.00
end_station

struct sismchan
{
    int  on;
    char sensor[8];
    char compname[3];
    double azim;
    double dip;
};

typedef struct
{
    char    station[9];
    double  begtime;
    double  endtime;
    double  lat;
    double  lon;
    double  elev;
    double  depth;
    struct  sismchan sismchan[4][3];
} StationParm;


 *===================================================================*/
void ReadDbStation(station)
char *station;
{
FILE     *Fp_sta;
char     stafname[PATHLEN];
char     line[255];
int      nline;
char     *token[10];
int      ntok;
int      ch, cp;
int      found_beg_station;
int      found_end_station;
struct   StationParm *ps = NULL;
char     str[40];

    if (opt.use_database == TRUE)
    {
        if (!strlen(db_paths.stations))
        {
            fprintf(Fp_log, "  ReadDbStation: no station file for sta %s\n",
                    Station);
            return;
        }
        sprintf(stafname, "%s/%s", db_paths.stations, station);
    }
    else
    {
        sprintf(stafname, "./%s", station);
    }

if (0) printf("==== ReadDbStation: opening file %s\n", stafname);

    if ((Fp_sta = fopen(stafname, "r")) == NULL)
    {
        fprintf(Fp_log,
            "  ReadDbStation: no station file for sta %s\n", Station);
        return;
    }


    found_beg_station = FALSE;
    found_end_station = FALSE;
    nline = 0;
    while (getline(Fp_sta, line))
    {
       ++nline;
       trim(line);
       ntok = sparse(line, token, " \t", 10);


/* NEW FORMAT
 * begin_station PYAD 1998.10.27-14.35.00 2001.08.04-00.00.00
 * coordinates 45.20 05.73 250 0
 * chan 0 comp 0 CMG5 Z 0.00 -90.00
 * chan 0 comp 1 CMG5 N 0.00 0.00
 * chan 0 comp 2 CMG5 E 90.00 0.00
 * end_station
 */
       if (!strcmp(token[0],"begin_station"))
       {

          found_beg_station = TRUE;
          if (ntok != 4)                    goto help;

          ps = (struct StationParm *)
              mem_alloc(sizeof(struct StationParm), "ReadDbStation");
          append_linklist_element(ps, staparms_head, staparms_tail);

/* Check station name */

          if (strcmp(token[1], station))
          {
              fprintf(stderr,"\n");
              fprintf(stderr,
                  "  ERROR: ReadDbStation: station name doesn't match\n");
              fprintf(stderr,"\n");
              exit(-1);
          }

/* start time */

          if (!strcmp(token[2], "0"));
          else
          {
             sprintf(str, "1970.01.01-00.00.00.000");
             strncpy(str, token[2], strlen(token[2]));
             str2utime1(str, &(ps->begtime));
/*
printf("-------- %s -> %s -> %.1f\n", token[2], str, ps->begtime);
*/
          }

/* end time */

          if (!strcmp(token[3], "0"));
          else if (!strcmp(token[3], "present")) ps->endtime = END_OF_WORLD;
          else
          {
             sprintf(str, "1970.01.01-00.00.00.000");
             strncpy(str, token[3], strlen(token[3]));
             str2utime4(str, &ps->endtime);
          }


          while (getline(Fp_sta, line))
          {
            ++nline;
            trim(line);
            ntok = sparse(line, token, " \t", 10);

            if (!strncmp(token[0],"coord", 5))
            {
               if (ntok != 5)                    goto help;

               ps->lat   = atof(token[1]);
               ps->lon   = atof(token[2]);
               ps->elev  = atof(token[3]);
               ps->depth = atof(token[4]);
            }

            if (!strncmp(token[0],"chan", 4))
            {
               if (ntok != 8)                    goto help;
               ch = atoi(token[1]);
               if (ch < 0 || ch > 1)             goto help;
               cp = atoi(token[3]);
               if (cp < 0 || cp > 2)             goto help;

               if (ps == NULL)
               {
                  ps = (struct StationParm *)
                      mem_alloc(sizeof(struct StationParm), "ReadDbStation");
                  append_linklist_element(ps,
                                          staparms_head, staparms_tail);
               }

               ps->sismchan[ch][cp].on = 1;
               sprintf(ps->sismchan[ch][cp].sensor, "%.8s", token[4]);
               sprintf(ps->sismchan[ch][cp].compname, "%.1s", token[5]);
               ps->sismchan[ch][cp].azim = atof(token[6]);
               ps->sismchan[ch][cp].dip  = atof(token[7]);

        /* Copy chan info to secondary channel-components */
               for (cp=0; cp<3; cp++) if (ps->sismchan[ch][cp].on)
               {
                   memcpy(&ps->sismchan[ch+2][cp],
                          &ps->sismchan[ch][cp],
                          sizeof(struct sismchan));
               }
            }
            if (!strcmp(token[0],"end_station"))
            {
                ps = NULL;
                found_end_station = TRUE;
                break;
            }
          }
       }

       if (found_beg_station == TRUE && found_end_station == FALSE)
       {
          fprintf(stderr,"\n");
          fprintf(stderr,
              "  ERROR: ReadDbStation: error syntax in file '%s'\n",
              stafname);
          fprintf(stderr,"  Check 'begin_station' 'end_station'\n");
          fprintf(stderr,"\n");
          exit(-1);
       }

       if (found_beg_station == TRUE)
       {
          found_beg_station = FALSE;
          found_end_station = FALSE;
       }

/*
 * END NEW FORMAT
 */


       if (!strncmp(token[0],"coord", 5))
       {
          if (ntok != 5)                    goto help;

          if (ps == NULL)
          {
             ps = (struct StationParm *)
                 mem_alloc(sizeof(struct StationParm), "ReadDbStation");
             append_linklist_element(ps, staparms_head, staparms_tail);
          }

          ps->lat   = atof(token[1]);
          ps->lon   = atof(token[2]);
          ps->elev  = atof(token[3]);
          ps->depth = atof(token[4]);
       }

       if (!strncmp(token[0],"chan", 4))
       {
          if (ntok != 8)                    goto help;
          ch = atoi(token[1]);
          if (ch < 0 || ch > 1)             goto help;
          cp = atoi(token[3]);
          if (cp < 0 || cp > 2)             goto help;

          if (ps == NULL)
          {
             ps = (struct StationParm *)
                 mem_alloc(sizeof(struct StationParm), "ReadDbStation");
             append_linklist_element(ps, staparms_head, staparms_tail);
          }

          ps->sismchan[ch][cp].on = 1;
          sprintf(ps->sismchan[ch][cp].sensor, "%.8s", token[4]);
          sprintf(ps->sismchan[ch][cp].compname, "%.1s", token[5]);
          ps->sismchan[ch][cp].azim = atof(token[6]);
          ps->sismchan[ch][cp].dip  = atof(token[7]);

        /* Copy chan info to secondary channel-components */
          for (cp=0; cp<3; cp++) if (ps->sismchan[ch][cp].on)
          {
              memcpy(&ps->sismchan[ch+2][cp],
                     &ps->sismchan[ch][cp],
                     sizeof(struct sismchan));
          }
       }
    }
    fclose(Fp_sta); Fp_sta = NULL;


    if (nline <= 0)
    {
        fprintf(Fp_log,"  ReadDbStation: empty station file %s\n", stafname);
        return;
    }

/*
 * Print database station info.
 */
    printf("\n");
    printf("  Station database params (from file: %s)\n", stafname);
    printf("\n");
    for (ps=staparms_head; ps!=NULL; ps=ps->next)
    {
       printf("      station %s begtime=%.1f endtime=%.1f\n",
               station, ps->begtime, ps->endtime);
       printf("      lat=%.4f lon=%.4f elev=%.4f depth=%.4f\n",
               ps->lat,
               ps->lon,
               ps->elev,
               ps->depth);

       for (ch=0; ch<4; ch++) for (cp=0; cp<3; cp++)
       {
         if (ps->sismchan[ch][cp].on)
           printf("      chan=%d comp=%d  %s %s %.4f %.4f\n",
               ch,
               cp,
               ps->sismchan[ch][cp].sensor,
               ps->sismchan[ch][cp].compname,
               ps->sismchan[ch][cp].azim,
               ps->sismchan[ch][cp].dip);
       }
       printf("\n");
    }
/*
 * Set flag
 */
    stationDbParms = 1;
    return;

help:

    fprintf(stderr,"\n");
    fprintf(stderr,"  ERROR: ReadDbStation: error syntax in file '%s'\n",
                    stafname);
    fprintf(stderr,"\n");
    exit(-1);
}

