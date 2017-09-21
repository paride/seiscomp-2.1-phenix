/*======================================================================
    readtitan.c

    Library for Titan converter

    Author: J.-F. Fels, OMP, Toulouse

Algorithm

process_titan:
    open input file
    get_station_name
    while (1)
        initilizations
        synchronize_on_frame
        while (read_read_frame)
            processDataFrame
            processTimeFrame
            processInfoFrames
            processOffsetFrame
            processMiscFrames
        end while
    end while

*======================================================================*/
#include "titan.h"
#include "proto.h"

/* Prototypes */

#ifdef ANSI_C
static int   readTitanLoop      ();
static void  print_byte_cnt     ();
static void  init               ();
#else
static int   readTitanLoop      ();
static void  print_byte_cnt     ();
static void  init               ();
#endif



TITFILE        *Fp_tit;                      /* TITAN file pointer */
char           dtfname[PATHLEN];             /* delta_t file name  */
int            inp_data[NCHAN][NCOMP][NINP]; /* Input data array */

double         SystTime;      /* System time of the more recent sample */
double         FirstSystTime; /* First System time */
double         LastSystTime;  /* Last System time */
double         ExtPulse;      /* System time of the external pulse    */
double         Observ_dt;     /* Time offset between system and external time */
double         ExtraTcorr;    /* Extra time correction */
TimeReset      ResetTime;     /* System time reset */
struct flags   flags;

struct data_list *list_head = NULL;
struct data_list *list_tail = NULL;
struct dft_list  *dft_head = NULL;
struct dft_list  *dft_tail = NULL;
int              foundDftFiles = -1;
struct xtc_list  *xtc_head = NULL;
struct xtc_list  *xtc_tail = NULL;

struct         acq acq;
struct         Channel Channel[NCHAN];
outData        OutData[NCHAN];
char           Info[32][12];
int            NumEstimDeltaTime;
int            NumFitEntries;
int            totOutSamples;
int            DataOffsetCorr;
Titseg         TitSegment[2048];
int            NTitSegment;
int            TitSegmentNum;

static int    titfile_ofs;
static char   frame[20];

extern struct option opt;
extern FILE   *Fp_log;
extern int    byteswap;
extern Event  evn;
extern Paths  paths;
extern char   Station[8];



/*==================================================================*/
void process_titan(path)
char *path;
{
int i;
char s1[40], s2[40];


    if ((Fp_tit = topenGuess(path)) == NULL)
    {
        fprintf(Fp_log,"process_titan: error opening file %s\n",path);
        return;
    }

/*
 * Optional titan file segmentation. When done, return, we are all done.
 * There are 3 schemes to segment the input titan:
 *    1- opt.offset_list  --> according to an ASCII list of offset
 *    2- opt.index_list   --> according to a titan binary index file
 *    3- opt.event_list   --> according to an event time ASCII list
                              (see Event2TitanSeg() in output_data()).
 */
    if (opt.titseg)
    {
        if (strlen(opt.offset_list))
        {
            read_offset_list();
            writeTitseg();
            return;
        }
        else if (strlen(opt.index_list))
        {
            readTitanIndex();
            writeTitseg();
            return;
        }
    }

    flags.treset_time_out  = FALSE;
    flags.missing_estim_dt = FALSE;
    flags.init_time_log    = TRUE;
    for (i=0; i<NCHAN; i++) Channel[i].tot_inp = 0;
    evn.cur_event = (evn.num_events) ? 0 : -1;
    evn.done      = FALSE;

/*
 * Set begining titan file offset
 */
    titfile_ofs = opt.beg_offset;

    fprintf(Fp_log,
        "  ============= process_titan: MAIN LOOP =============\n");

    while (1)
    {
        if (readTitanLoop() == FALSE)
        {
            break;
        }
    }

/*
 * Process remaining input data
 */
    output_data(LAST, 0);

/*
 * Write titan segment files
 */
    if (opt.titseg && (strlen(opt.event_list) || opt.tjump_seg))
    {
        if (0) for (i=0; i<NTitSegment; i++)
        {
            printf("segm %d beg=%d end=%d\n",
                   i, TitSegment[i].beg_offset, TitSegment[i].end_offset);
        }

        writeTitseg();
    }

/*
 * Process last misc frames
 */
    processMiscFrames(frame, LAST);

/*
 * Log last time
 */
    log_time(LAST, &flags.init_time_log);

/*
 * Close input titan file
 */
    tclose(Fp_tit);
    Fp_tit = NULL;

/*
 * Write last delta time
 */
    write_delta_time(LAST);

/*
 * Print events info
 */
    if ((evn.evn_time != NULL) && (evn.num_events > 0))
    {
        printMissingEvents();
    }

    time_asc4(s1, FirstSystTime);
    time_asc4(s2, LastSystTime);
    fprintf(Fp_log,"  System Time: from %.19s to %.19s\n", s1, s2);
}


/*==================================================================*/
static int readTitanLoop()
{
int   sync, frame_type;

    init();

    if (evn.cur_event >= evn.num_events)
    {
        fprintf(Fp_log,"  End readTitanLoop: last event found; ofs=%d\n",
             ttell(Fp_tit));
        return 0;
    }
    if ((titfile_ofs = sync_on_frame(titfile_ofs)) < 0)
    {
        fprintf(Fp_log,"  End readTitanLoop: sync_on_frame failed; ofs=%d\n",
             ttell(Fp_tit));
        return 0;
    }
    if (teof(Fp_tit))
    {
        fprintf(Fp_log,"  End readTitanLoop: EOF at file ofs %d\n",
             ttell(Fp_tit));
        return 0;
    }
    if (opt.end_offset > 0 && (ttell(Fp_tit) >= opt.end_offset))
    {
        fprintf(Fp_log,"  End readTitanLoop: end offset reached; ofs=%d\n",
             ttell(Fp_tit));
        return 0;
    }

    fprintf(Fp_log,"  readTitanLoop: frame synchro @ ofs %d\n",
        ttell(Fp_tit));

    while (tread(frame, 1, 12, Fp_tit) == 12)
    {
        GET_SYNC;
        if (!SYNC_OK)
        {
            titfile_ofs = ttell(Fp_tit);
            check_sync(frame);
            output_data(LAST, 0);
            log_time(LAST, &flags.init_time_log);
            return 1;
        }

        GET_FRAME_TYPE;
        switch (frame_type)
        {

case DATA_PRE_TRIG:
case DATA_POST_TRIG:
            titfile_ofs = ttell(Fp_tit);
            if (processDataFrame(frame) == -1)
            {
              fprintf(Fp_log,"  readTitanLoop: processDataFrame failed at ");
              fprintf(Fp_log,"file ofs: %d\n", ttell(Fp_tit));
              output_data(LAST, 0);
              log_time(LAST, &flags.init_time_log);
              return 1;
            }

            if (evn.done == TRUE)
            {
               evn.done = FALSE;
            }
            if (evn.cur_event >= evn.num_events)
            {
              output_data(LAST, 0);
              fprintf(Fp_log,"  End readTitanLoop: last event found; ofs=%d\n",
                   ttell(Fp_tit));
              return 0;
            }
            if (opt.titseg && strlen(opt.event_list))
            {
                if (NTitSegment >= evn.num_events)
                {
                  output_data(LAST, 0);
                  fprintf(Fp_log,"  End readTitanLoop: last segment found; ofs=%d\n",
                       ttell(Fp_tit));
                  return 0;
                }
            }
            break;

case TIME: 
            processTimeFrame(frame);
            log_time(!LAST, &flags.init_time_log);
            write_delta_time(!LAST);
            if (opt.end_offset > 0 && (ttell(Fp_tit) >= opt.end_offset))
            {
              output_data(LAST, 0);
              return 1;
            }
            break;

case OFFSET:
            processOffsetFrame(frame, 0);
            break;

case INFO:
            if (processInfoFrames(frame, "readTitanLoop") == FALSE)
            {
              titfile_ofs = ttell(Fp_tit);
              fprintf(Fp_log,"  readTitanLoop: WRONG INFO  at ");
              fprintf(Fp_log,"file ofs %d\n", titfile_ofs);
              output_data(LAST, 0);
              return 1;
            }
            print_byte_cnt();
            break;

case MISC:
            processMiscFrames(frame, !LAST);
            break;

case TIME_CORRECTED:
case FILLING:
            break;

default:
            fprintf(Fp_log,"  WARNING: readTitanLoop: ");
            fprintf(Fp_log,"frame type %d not supported ",frame_type);
            fprintf(Fp_log,"@ file ofs %d\n", ttell(Fp_tit));
            output_data(LAST, 0);
            return 1;

        }  /* end switch */

    }  /* end while */

    if (teof(Fp_tit))
    {
        fprintf(Fp_log,"  End readTitanLoop: EOF at file ofs %d\n", ttell(Fp_tit));
        return 0;
    }
    fprintf(Fp_log,"  WARNING: End readTitanLoop: ??? file ofs %d\n", ttell(Fp_tit));
    return 0;
}


/*==================================================================*
    output_data
    input args:
        last:        flag telling that end of data is detected.
        time_jump:   flag telling a discontinuity occured in the data.

    This module is called by processDataFrame() and by readTitanLoop().
    If last is not set, the module is called upon reading of a time
    frame and whe should have got 128 primary mux data triplets and
    optionally 128 /decim secondary mux data triplets in the
    inp_data[][][] array.

    The process is dispatched to different modules according to the
    general options and the optional output data format
 *==================================================================*/
void output_data(last, time_jump)
int last;
int time_jump;
{


  if (opt.titseg && strlen(opt.event_list))
  {
      flushInputData(last, time_jump);
      Event2TitanSeg(last, time_jump);
      return;
  }

  if (opt.titseg && opt.tjump_seg)
  {
      flushInputData(last, time_jump);
      Tjump2TitanSeg(last, time_jump);
      return;
  }

  return;
}


/*================================================================*/
static void init()
{
int i, chan;

    acq.AdcDelay      = (double) UNKNOWN;

    for (chan=0; chan<NCHAN; chan++)
    {
        Channel[chan].new           = TRUE;
        Channel[chan].numcomp       = 0;
        Channel[chan].ninp          = 0;
        Channel[chan].nout          = 0;
        Channel[chan].srate         = (double) UNKNOWN;
        Channel[chan].decim         = (short)  UNKNOWN;
        Channel[chan].adcdelay      = (double) UNKNOWN;
        Channel[chan].filtdelay     = (double) UNKNOWN;

        clearOutputParms(chan);
    }

    memset(Info, 0, (32*12));
    for (i=0; i<32; i++) Info[i][10] = -1;
    flags.first_info_frame     = TRUE;
    flags.first_data_frame     = TRUE;
    flags.first_time_frame     = TRUE;
    flags.TimeFrame            = FALSE;
    SystTime             = 0.0;
    ExtPulse             = (double) UNKNOWN;
    Observ_dt            = (double) UNKNOWN;
    ResetTime.curr       = (double) UNKNOWN;
    ResetTime.prev       = (double) UNKNOWN;
    ResetTime.reboot     = -1;
    DataOffsetCorr       = ABS_OFS;
}


/*==================================================================*/
static void print_byte_cnt()
{
static int  kkk;
char str[40];

  if (++kkk > 80000)
  {
    printf("  process_titan: ................   %7.2f MBytes read ",
              (double)(ttell(Fp_tit)) / 1000000.0);
    time_asc4(str, SystTime);
    printf("%.19s\n", str);
    kkk = 0;
  }
}

