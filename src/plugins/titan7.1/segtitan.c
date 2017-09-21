/*======================================================================
    segtitan.c

    Author: J.-F. Fels, OMP, Toulouse

    contents:
void  Event2TitanSeg     ();
void  writeTitseg        ();
void  read_offset_list   ();
void  readTitanIndex     ();
void  printMissingEvents ();
*======================================================================*/
#include "titan.h"
#include "proto.h"

/* Prototypes */

#ifdef ANSI_C
#else
#endif

extern TITFILE   *Fp_tit;       /* TITAN file pointer */
extern double    SystTime;      /* System time of the more recent sample */
extern double    FirstSystTime; /* First System time */
extern double    LastSystTime;  /* Last System time */
extern double    Observ_dt;
extern double    ExtraTcorr;
extern struct    acq acq;
extern struct    Channel Channel[NCHAN];
extern struct    option opt;
extern FILE      *Fp_log;
extern Event     evn;
extern char      Station[8];
extern Titseg    TitSegment[2048];
extern int       NTitSegment;




/*================================================================*
    printMissingEvents
       Output file is in current working directory
 *================================================================*/
void printMissingEvents()
{
    int   i;
    int   missingEvents;
    FILE  *Fp;
    char  arg[255];
    char  fname[255];
    char  date[50];
    char  *token[4];
    int   ntok;

/*
 * Check for missing events
 */
    missingEvents = FALSE;
    for (i=0; i<evn.num_events; i++)
    {
/*
        printf("===== event %d %d found=%d ofs=%d\n",
                i, evn.evn_time[i], evn.found[i], evn.file_offset[i]);
*/
        if (evn.found[i] == 0)
        {
            if ( (evn.evn_time[i] > (int) FirstSystTime) &&
                 (evn.evn_time[i] < (int) LastSystTime) )
            {
                missingEvents = TRUE;
            }
        }
    }

    if (missingEvents)
    {
        sprintf(arg, "%s", opt.event_list);
        ntok = sparse(arg, token, "./ ", 4);
        if (ntok < 1)
        {
          fprintf(stderr,
          "ERROR: printMissingEvents: can't get event file name from '%s'\n",
                  opt.event_list);
          exit(1);
        
        }
        sprintf(fname, "%s/%s.missing", getcwd(NULL,0), token[ntok-1]);
        printf("\n");
        printf("  WARNING: Some events in '%s' were not found.\n",
               opt.event_list);
        printf("           Missing events are in current directory:\n");
        printf("           %s\n", fname);
        printf("\n");

/*
        if (!(Fp = fopen(fname, "w")))
        {
          fprintf(stderr,
              "ERROR: printMissingEvents: can't open file '%s'\n", fname);
          exit(1);
        }
*/
        fcreat_append(fname, &Fp);

        for (i=0; i<evn.num_events; i++) if (evn.found[i] == 0)
        {
            if ( (evn.evn_time[i] > (int) FirstSystTime) &&
                 (evn.evn_time[i] < (int) LastSystTime) )
            {
                strftime(date,40,"%Y.%m.%d-%H.%M.%S",
                         gmtime((time_t*)&(evn.evn_time[i])));
                fprintf(Fp, "%s %d\n", date, evn.evn_duration[i]);
            }
        }
        fclose(Fp); Fp = NULL;
    }
}


/*==================================================================*
    Event2TitanSeg
    Call by output_data(), every 128 primary samples.
    Take as input an events table (from the ascii events file)
    and look in the titan file for the events time. When a time is found,
    the following fields of the titan segments structures are filled:
        - utime
        - beg_offset
        - end_offset
    and the number of segments is incremented. 

    The events time above are corrected for 
    FIR filters delays, delta_t + ExtraTcorr

 *
 * The size of the time comparison window must be large enough to
 * catch the data time. How the data time increment or how often this
 * module is called? It depends of the primary samprate.
 * Set time comparison window as regard of the the primary samprate:
 *
    if      (acq.PrimSrate == 250.0)   tint = 0.6;
    else if (acq.PrimSrate == 160.0)   tint = 1.0; 
    else if (acq.PrimSrate == 125.0)   tint = 1.2; 
    else if (acq.PrimSrate ==  80.0)   tint = 2.0; 
    else if (acq.PrimSrate == 62.50)   tint = 2.4;
    else if (acq.PrimSrate ==  40.0)   tint = 4.0; 
    else if (acq.PrimSrate == 31.25)   tint = 4.8;
    else if (acq.PrimSrate ==  20.0)   tint = 8.0; 
    else                               tint = 10.0;
 *
 *==================================================================*/
void Event2TitanSeg(flag, time_jump)
int flag, time_jump;
{
double        dataTime;
double        begEvnTime;
static double endEvnTime;
static int    seek_to;
double        tint;
static int    event_in_process;
char          str[40];
int           i;
double        delta_t;
int           chan;
int           dbug = 0;
static int    first = 1;

char *dftfile;
double estim_dt;
static double prev_delta_t;
static double prev_SystTime;

/*
 * Check for segments starting before begining of the titan file
 * and ending inside.
 * If found, set begining time to start time of the titan file.
 */
    if (first)
    {
        for (i=0; i<evn.num_events; i++)
        {
            begEvnTime = (double) evn.evn_time[i];
            endEvnTime = (double) evn.evn_time[i] +
                         (double) evn.evn_duration[i];
            if (endEvnTime > FirstSystTime && begEvnTime < FirstSystTime)
            {
                evn.evn_time[i] = FirstSystTime;
            }
        }
        first = 0;
    }

/*
 * Time comparison window
 */
    tint = 12.0;

/*
 * We need a channel number to compute the corrected time.
 * We use the first channel present.
 */
    for (chan=0; chan<4; chan++) if (Channel[0].numcomp >= 0) break;
    if (chan == 4)
    {
      fprintf(stderr,"ERROR: Event2TitanSeg: no valid channel found!\n");
      exit(1);
    }

/*
 * Case no time correction
 */
    if (opt.tcorr_mode == NOCORRECTION)
    {
      dataTime = SystTime - Channel[chan].adcdelay - Channel[chan].filtdelay;
    }


/*
 * Compute current corrected time, using observed delta_t or
 * estimated delta_t.
 * Since estimated delta_t is calculation is highly time consuming,
 * it is computed only when delta_t changes.
 */
    else
    {
       estim_dt = (double) UNKNOWN;
       if (Observ_dt != (double) UNKNOWN) delta_t = Observ_dt;
       else                               delta_t = 0.0;

       if (delta_t != prev_delta_t)
       {
          estim_dt = GetEstimDt(SystTime, Station, &dftfile);
if (0) printf("delta_t %.4f estim_dt %.4f prev at %.3f secs\n",
              delta_t, estim_dt, SystTime-prev_SystTime);
       }
       prev_delta_t = delta_t;
       prev_SystTime = SystTime;

       if (estim_dt != (double) UNKNOWN)
       {
         dataTime = SystTime
                    - Channel[chan].adcdelay - Channel[chan].filtdelay
                    - estim_dt + ExtraTcorr;
       }
       else
       {
         dataTime = SystTime
                    - Channel[chan].adcdelay - Channel[chan].filtdelay
                    - delta_t + ExtraTcorr;
       }
    }

    for (i=evn.cur_event; i<evn.num_events; i++)
    {
        begEvnTime = (double) evn.evn_time[i];

/*======================*/
/* Look for beg segment */
/*======================*/

        if (begEvnTime > (dataTime-tint) && begEvnTime < (dataTime+tint))
        {

if (dbug)
    printf("++++ t=%d   begEvnTime=%d   evn=%d i=%d   ofs=%d\n",
        (int) dataTime, (int) begEvnTime, evn.cur_event, i, ttell(Fp_tit));

          if (dataTime > begEvnTime)
          {
            if (event_in_process == FALSE)
            {
                event_in_process = TRUE;
                if (i != evn.cur_event)
                {
                    if (dbug)
                       printf("  Event2TitanSeg: event %d not found\n", i-1);
                    evn.cur_event = i;
                }

                evn.found[i] = TRUE;
                endEvnTime = (double) evn.evn_time[i] +
                             (double) evn.evn_duration[i];
                seek_to = ttell(Fp_tit);
                TitSegment[NTitSegment].utime = begEvnTime;
                TitSegment[NTitSegment].beg_offset = ttell(Fp_tit);
                time_asc4(str, dataTime);
                printf("  ==== beg segment: %d %d %s ofs=%d ",
                    NTitSegment, i, str, TitSegment[NTitSegment].beg_offset);
                time_asc4(str, begEvnTime);
                printf(" (%.19s)\n", str);
            }
          }
        }

/*======================*/
/* Look for end segment */
/*======================*/

        if ((endEvnTime > (dataTime-tint) && endEvnTime < (dataTime+tint)) ||
             flag == LAST)
        {

if (dbug)
    printf("++++ t=%d   endEvnTime=%d   evn=%d i=%d   ofs=%d\n",
        (int) dataTime, (int) endEvnTime, evn.cur_event, i, ttell(Fp_tit));

          if (dataTime > endEvnTime || flag == LAST)
          {
            if (event_in_process == TRUE)
            {
                event_in_process = FALSE;
                TitSegment[NTitSegment].end_offset = ttell(Fp_tit);
                time_asc4(str, dataTime);
                printf("  ==== end segment: %d %d %s ofs=%d\n",
                    NTitSegment, i, str, TitSegment[NTitSegment].end_offset);

if (dbug)
    printf("==== end event; seg %d seek to %d\n",
        evn.cur_event, seek_to);

            /* Come back to segment beg offset */
                tseek(Fp_tit, seek_to, SEEK_SET);

                ++NTitSegment;
                ++evn.cur_event;

            /* Event finished. Exit for (num_events) loop */
                break;
            }
          }
        }
    }
}

/*==================================================================*
    Tjump2TitanSeg
    Call by output_data(), every 128 primary samples.
    Build TitSegment[] table according to time discontinuities
    occuring in the input Titan file.
    Then, a call to writeTitseg() segment the input Titan file
    and write continuous Titan files to disk.
 *==================================================================*/
void Tjump2TitanSeg(last, time_jump)
int last;
int time_jump;
{
static int  prev_offset;
static int  prev_time;

    if (prev_offset == 0)
    {
        prev_offset = opt.beg_offset;
        prev_time = FirstSystTime;
    }

    if (time_jump)
    {

      TitSegment[NTitSegment].beg_offset = prev_offset;
      TitSegment[NTitSegment].end_offset = ttell(Fp_tit)-12;
      TitSegment[NTitSegment].utime = prev_time;

      printf("           tj %8d %8d %d\n",
             TitSegment[NTitSegment].beg_offset,
             TitSegment[NTitSegment].end_offset,
             TitSegment[NTitSegment].utime);

      ++NTitSegment;
      prev_offset = ttell(Fp_tit)-12;
      prev_time   = SystTime;
    }
    if (last)
    {
      if (prev_offset   == TitSegment[NTitSegment-1].beg_offset)
          return;
      
      TitSegment[NTitSegment].beg_offset = prev_offset;
      TitSegment[NTitSegment].end_offset = ttell(Fp_tit);
      TitSegment[NTitSegment].utime = prev_time;

      if (NTitSegment == 0)
          TitSegment[NTitSegment].utime = FirstSystTime;
      else
          TitSegment[NTitSegment].utime = SystTime;

      printf("         last %8d %8d %d\n",
             TitSegment[NTitSegment].beg_offset,
             TitSegment[NTitSegment].end_offset,
             TitSegment[NTitSegment].utime);

      ++NTitSegment;
    }
}



/*==================================================================*/
void read_offset_list()
{
FILE    *Fpofs = NULL;
char    *token[4];
int      ntok;
int      nofs;
int      linenum;
char     line[255];

    open_Frd(opt.offset_list, &Fpofs);

    nofs = 0;
    linenum = 0;
    while (getline(Fpofs, line))
    {
        ++linenum;
        if (nofs > 2048)
        {
            printf("read_offset_list: too many input; max is 2048\n");
            break;
        }
        trim(line);
if (0) printf("---- %s\n", line);
        ntok = sparse(line, token, " ", 4);
        if (ntok < 2)
        {
            printf("read_offset_list: line %d: need 2 values; skipped.\n",
                   linenum);
            continue;
        }
        else
        {
            TitSegment[nofs].beg_offset = atoi(token[0]);
            TitSegment[nofs].end_offset = atoi(token[1]);
if (0) printf("++++ %d %d\n",
                     TitSegment[nofs].beg_offset,
                     TitSegment[nofs].end_offset);
            if (TitSegment[nofs].end_offset < TitSegment[nofs].beg_offset)
            {
                printf("read_offset_list: line %d: ", linenum);
                printf("second value < first value; skipped.\n");
                continue;
            }
        }
        ++nofs;
    }
    NTitSegment = nofs;

    if (nofs == 0)
    {
       fclose(Fpofs);
       fprintf(stderr,"\n  ERROR: read_offset_list: no offset values ");
       fprintf(stderr,"found in %s\n\n",opt.offset_list);
       evn.num_events = 0;
       return;
    }
}

typedef struct {
    int     time;
    int     chan;
    int     index_type;
    int     index_size;
    int     beg_ofs;
    int     beg_ofs_ncycle;
    int     end_ofs;
    int     end_ofs_ncycle;
    int     info_flag;
    int     alert;
    int     duration;
    int     max_chan1;
    int     max_chan2;
    int     max_chan3;
} TIT_INDEX;

/*==================================================================*/
void readTitanIndex()
{
#define    INDEX_SIZE  32
FILE       *Fpndx = NULL;
char       str[INDEX_SIZE+10];
int        nread;
TIT_INDEX  tindex;
struct tm  *ttm1;
int        yr;
time_t     cdate;
char       date[40];

    time(&cdate);
    open_Frd(opt.index_list, &Fpndx);

    while ( 1 )
    {
        nread = fread(str,1,INDEX_SIZE,Fpndx);
        if (nread <= 0 || nread < INDEX_SIZE) break;

        tindex.index_type     = str[5];

        if ((tindex.index_type != 0) && (tindex.index_type != 1))
            continue;

        tindex.time           = bytes2int4(str[0],str[1],str[2],str[3]);

/* Discard un-initialized entries */

        if ((tindex.time == (int) 0XFFFFFFFF) &&
            ((uchar) str[5] == (uchar) 0XFF))
            continue;

/* Discard entries with wrong time */

        ttm1=gmtime((time_t *) &tindex.time);
        strftime(date,32,"%Y",ttm1);
        sscanf(date,"%d",&yr);
        if (yr < 1993)
            continue;
        if (tindex.time > cdate)
            continue;

        tindex.beg_ofs        = bytes2int4(0,str[13],str[14],str[15]);
        tindex.beg_ofs_ncycle = (uchar) str[12];
        tindex.end_ofs        = bytes2int4(0,str[9],str[10],str[11]);
        tindex.end_ofs_ncycle = (uchar) str[8];

        TitSegment[NTitSegment].utime = tindex.time;
        TitSegment[NTitSegment].beg_offset = tindex.beg_ofs;
        TitSegment[NTitSegment].end_offset = tindex.end_ofs; 
        ++NTitSegment;

        strftime(date,40,"%Y.%m.%d-%H.%M.%S", gmtime((time_t*)&tindex.time));
    }

    fclose(Fpndx);
}


/*==================================================================*/
void writeTitseg()
{
#define BUFFLEN 100000
char buffer[BUFFLEN+10000];
FILE   *Fpseg;
char   segname[255];
int    NN, n;
int    i, j;
int    nout;
int    nbytes;
int    nread;
char   str[40];
struct stat fs;
int    dbug = 0;
char   sta[10];

    Fpseg = NULL;

    if (NTitSegment == 0)
    {
        fprintf(Fp_log,"\n  WARNING writeTitseg: no titan segment found\n");
        fprintf(Fp_log,"\n");
        return;
    }
    else
    {
        fprintf(Fp_log,"\n  writeTitseg: %d titan segments:\n", NTitSegment);
    }

    if      (strlen(Station))     sprintf(sta, "%s", Station);
    else if (strlen(opt.station)) sprintf(sta, "%s", opt.station);
    else sta[10] = '\0';

    for (i=0; i<NTitSegment; i++)
    {

/* Compute number of output bytes; make multiple of 12 */
        nbytes = TitSegment[i].end_offset - TitSegment[i].beg_offset;
        nbytes = (nbytes / 12) * 12;

        if (nbytes <= 0) continue;

        if (Fpseg) { fclose(Fpseg); Fpseg = NULL; }

        if (TitSegment[i].utime != 0)
        {
            time_asc4(str, (double) TitSegment[i].utime);
            if (strlen(sta))
            {
                if (opt.tjump_seg)
                  sprintf(segname, "d.%.19s.%s", str, sta);
                else
                  sprintf(segname, "%.19s.%s.tit", str, sta);
            }
            else
            {
                sprintf(segname, "data.%.19s.tit", str);
            }
        }
        else
        {
            sprintf(segname, "data.%d.tit", i);
        }
        if (!(Fpseg = fopen(segname, "w")))
        {
            fprintf(stderr,"ERROR: writeTitseg: can't open file '%s'\n",
                    segname);
            exit(1);
        }

        tseek(Fp_tit, TitSegment[i].beg_offset, SEEK_SET);

if (dbug) printf("==== writeTitseg: file '%s'; nbytes=%d ofs=%d\n",
           segname, nbytes, TitSegment[i].beg_offset);

/* Read-write by packets "BUFFLEN" long */

        NN = nbytes / BUFFLEN;
        n = nbytes % BUFFLEN;
        nout = 0;

/* Read-write NN packets */
        for (j=0; j<NN; j++)
        {
            nread = tread(buffer, 1, BUFFLEN, Fp_tit);
            if (fwrite(buffer, 1, nread, Fpseg) != nread)
            {
              fprintf(stderr,"ERROR: writeTitseg: write error\n"); exit(1);
            }
            nout += nread;
            if (dbug) printf("  writing %s  %d\n", segname, nread);
        }

/* Read-write remaning bytes */
        nread = tread(buffer, 1, n, Fp_tit);
        if (fwrite(buffer, 1, nread, Fpseg) != nread)
        {
          fprintf(stderr,"ERROR: writeTitseg: write error\n"); exit(1);
        }
        nout += nread;
        if (dbug) printf("  writing %s  %d\n", segname, nread);

        fclose(Fpseg); Fpseg = NULL;

/* Check output file */
        if (stat(segname, &fs) != 0)
        {
            fprintf(stderr,"ERROR: writeTitseg: can't stat '%s'\n",segname);
            exit(1);
        }
        if (fs.st_size == 0)
        {
            printf("  writeTitseg: empty file '%s'; removed\n",segname);
            unlink(segname);
        }

        fprintf(Fp_log,"  file '%s' written; %d bytes; ", segname, nout);
/*
        if (Fp_tit)
            fprintf(Fp_log,"titan offset=%d", ttell(Fp_tit));
        else
            fprintf(Fp_log,"titan offset=???");
*/
        fprintf(Fp_log,"\n");

    }  /* end for i<NTitSegment */

    fprintf(Fp_log,"\n");
    return;
}


