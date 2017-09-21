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
#include "libxplot/proto.h"
#include "libxplot/xplot.h"

/* Prototypes */

#ifdef ANSI_C
static int   readTitanLoop      (void);
static void  print_byte_cnt     (void);
static void  init               (void);
static void  free_dft_list      (void);
#else
static int   readTitanLoop      ();
static void  print_byte_cnt     ();
static void  init               ();
static void  free_dft_list      ();
#endif


TITFILE   *Fp_tit;                      /* TITAN file pointer */
char      dtfname[PATHLEN];             /* delta_t file name  */
int       inp_data[NCHAN][NCOMP][NINP]; /* Input data array */

double    SystTime;       /* System time of the more recent sample */
double    FirstSystTime;  /* First System time */
double    LastSystTime;   /* Last System time */
double    ExtPulse;       /* System time of the external pulse    */
double    Observ_dt;      /* Time offset between system and external time */
double    ExtraTcorr;     /* Extra time correction */
TimeReset ResetTime;      /* System time reset */

struct data_list *list_head = NULL;
struct data_list *list_tail = NULL;
struct dft_list  *dft_head = NULL;
struct dft_list  *dft_tail = NULL;
int              foundDftFiles = -1;
struct xtc_list  *xtc_head = NULL;
struct xtc_list  *xtc_tail = NULL;

struct    acq acq;
struct    Channel Channel[NCHAN];
outData   OutData[NCHAN];
char      Info[32][12];
int       NumEstimDeltaTime;
int       NumFitEntries;
int       totOutSamples;
int       DataOffsetCorr;

static int    titfile_ofs;
static  char  frame[20];

struct flags flags;
extern struct option opt;
extern FILE  *Fp_log;
extern int    byteswap;
extern Event  evn;
extern Paths  paths;
extern Tcoef *time_coef;
extern char   Station[8];


extern int datalist;
extern int dbug;
extern XY  *xy;
extern float *trigdata;
extern int nxy;
extern int Seek_ofs;

/*==================================================================*/
void process_titan(path)
char *path;
{
int i;

    if ((Fp_tit = topenGuess(path)) == NULL)
    {
        fprintf(Fp_log,"process_titan: error opening file %s\n",path);
        return;
    }

    flags.treset_time_out  = FALSE;
    flags.missing_estim_dt = FALSE;
    flags.init_time_log = TRUE;
    for (i=0; i<NCHAN; i++) Channel[i].tot_inp = 0;
    evn.cur_event = (evn.num_events)? 0 : -1;
    evn.done      = FALSE;
    Seek_ofs = -1;

/*
 * Set begining titan file offset
 */
    titfile_ofs = opt.beg_offset;

    fprintf(Fp_log,
        "  ============= process_titan: MAIN LOOP =============\n");

again:
    while (1)
    {
        if (readTitanLoop() == 0) break;
    }


/*
 * Process remaining input data
 */
    output_data(LAST, 0);
/*
 * Process last misc frames
 */
    processMiscFrames(frame, LAST);

    if (Seek_ofs >= 0)
    {
        titfile_ofs = Seek_ofs;
        Seek_ofs = -1;
printf("process_titan: seek to ofs=%d\n", titfile_ofs);
        tseek(Fp_tit, titfile_ofs, SEEK_SET);
        goto again;
    }

    if (0 && teof(Fp_tit) ||
        (opt.end_offset > 0 && (ttell(Fp_tit) >= opt.end_offset)))
    {
       char line[100];
       fprintf(Fp_log,"\n");
       printf("End of file reached. Exit ? (y or n) ");
       while ((line[0] = getc(stdin)) != EOF)
       {
           if (line[0]!='y' && line[0]!='n') continue;
           break;
       }
       if (line[0]=='n')
       {
           titfile_ofs = ttell(Fp_tit) - 100000;
           if (titfile_ofs < 0) titfile_ofs = 0;
           fprintf(Fp_log,
               "process_titan: GOING BACKWARD 100000 bytes, to ofs=%d\n",
               titfile_ofs);
           tseek(Fp_tit, titfile_ofs, SEEK_SET);
           fprintf(Fp_log,"\n");
           goto again;
       }
    }
/*
    if (opt.end_offset > 0 && (ttell(Fp_tit) >= opt.end_offset))
    {
       fprintf(Fp_log,"  END OFFSET REACHED\n");
    }
*/
    tclose(Fp_tit);
    Fp_tit = NULL;
    if (xy) { free(xy); xy = NULL; nxy = 0; }
    if (trigdata) { free((char *) trigdata); trigdata = NULL; }
/*
    return;
*/
    log_time(LAST, &flags.init_time_log);

    free_dft_list();
}


/*==================================================================*/
static int readTitanLoop()
{
int   sync, frame_type;

    init();
    if (evn.cur_event == evn.num_events)                         return 0;
    if ((titfile_ofs = sync_on_frame(titfile_ofs)) < 0)          return 0;
    if (teof(Fp_tit))                                            return 0;
    if (opt.end_offset > 0 && (ttell(Fp_tit) >= opt.end_offset)) return 0;

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
            if (Seek_ofs >= 0)
            {
              titfile_ofs = Seek_ofs;
              Seek_ofs = -1;
/*            printf("process_titan: seek to ofs=%d\n", titfile_ofs); */
              return 1;
            }
            if (evn.done == TRUE)
            {
               evn.done = FALSE;
            }
            if (evn.cur_event >= evn.num_events)
            {
              output_data(LAST, 0);
              return 1;
            }
            break;

case TIME: 
            processTimeFrame(frame);
            log_time(!LAST, &flags.init_time_log);
            if (opt.end_offset > 0 && (ttell(Fp_tit) >= opt.end_offset))
            {
              output_data(LAST, 0);
              return 1;
            }
            break;

case OFFSET:
            if (opt.do_offset != NO_OFS)
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
        fprintf(Fp_log,"  readTitanLoop: EOF at ");
        fprintf(Fp_log,"file ofs %d\n", ttell(Fp_tit));
        return 0;
    }
    return 0;
}


/*==================================================================*/
static void print_byte_cnt()
{
static int kkk;
char str[40];

  if (++kkk > 80000)
  {
    printf("  process_titan: ................   %.6f MBytes read ",
              (double)(ttell(Fp_tit)) / 1000000.0);
    time_asc1(str, SystTime);
    printf("%.17s\n", str);
    kkk = 0;
  }
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
    if (xy) { free(xy); xy = NULL; nxy = 0; }
    if (trigdata) { free((char *) trigdata); trigdata = NULL; }
}


/*================================================================*
 * free the data info list after a call to process_titan
 *================================================================*/
void free_data_list()
{
struct data_list *pl,*plf;
 
if (0) printf("  ==== running free_data_list\n");

    for (pl=list_head; pl!=NULL;)
    {
        free(pl->header);
        plf=pl;
        pl=pl->next;
        free(plf);
    }
    list_head=NULL;
    list_tail=NULL;
}


/*================================================================*/
void free_dft_list()
{
struct dft_list *pl,*plf;
 
if (0) printf("  ==== running free_dft_list\n");

    for (pl=dft_head; pl!=NULL;)
    {
        plf=pl;
        pl=pl->next;
        free(plf);
    }
    dft_head=NULL;
    dft_tail=NULL;
}

