/*====================================================================
                      libtrigger.c

Data flow:
                        process_titan                      (readtitanx.c)
                           |     |
              -------------      |
             |                   |
     processDataFrame            |                         (libtitan.c)
             |                   |
              --------------     |
                            |    |
                          output_data
                            |    |
                      ------      ----
                     |                |
              storeInputData     send_to_plot
                                      |
                                  ----------
                                 |          |
                              trigger       |
                                            |
                                         plot_loop         (libxplot1.c)


new modif: selcomp ~line 400

 *===================================================================*/
#include "titan.h"
#include "proto.h"
#include "libxplot/xplot.h"
#include "libxplot/proto.h"


#define MAX_DATANUM 4000000
#define BUFSIZE     1000000

/* Prototypes, private */
#ifdef ANSI_C
extern void plot_loop           (int);
static void  output_event       (int, int);
static void  send_to_plot       (int);
static void  storeInputData     (int);
static int   min_max_dec        (int, int*, int*, int);
static void  trigger            (int, double, int, double);
static void  sta_lta     (float*, int, double, int, double, Trigparm, int*);
static void  dtrend             (float*, int);
#else
extern void plot_loop           ();
static void  output_event       ();
static void  send_to_plot       ();
static void  storeInputData     ();
static int   min_max_dec        ();
static void  trigger            ();
static void  sta_lta            ();
static void  dtrend             ();
#endif

time_t       logtm;
char        *logtm_asc;
int         *out_data[NCHAN][NCOMP];     /* Output data array */
char         tqf;                        /* Time quality factor     */

extern int    inp_data[][NCOMP][NINP];   /*  Input data array */
extern outData OutData[NCHAN];
extern struct Channel Channel[NCHAN];
extern struct option opt;
extern Event  evn;
extern Paths  paths;
extern FILE  *Fp_log;
extern int    nduplic, nnew;
extern int    totOutSamples;
extern int    byteswap;
extern char   Station[8];
extern double SystTime;
extern double FirstSystTime;
extern TITFILE *Fp_tit;
extern float  timeWindowSec;

int    timeWindowNsamp;
int    Beg_file_ofs[NCHAN];

extern Trigparm trig;
extern int opt_decim;

typedef struct
{
  int time;
  int fileofs;
} Idxtbl;

#define MAX_FIDX 2000
Idxtbl idxtbl[MAX_FIDX+2];
int    nfidx;

extern int Seek_ofs;
extern int Seek_time;


/*==================================================================*/
void output_data(last, time_jump)
int last;
int time_jump;
{
int   chan;
int   i;
int   ltime;

/*============================================
 * Store titan file indexes into table 'idxtbl'
 *============================================*/

    ltime = (int) SystTime / 1000.0;
    ltime *= 1000;

    for (i=0; i<1000; i++)
    {
        if (ltime == idxtbl[i].time) break;
        if (idxtbl[i].time <= 0)
        {
if (0)      printf("== found empty slot at i=%d\n", i);
            break;
        }
    }
    if (Fp_tit) if (ltime != idxtbl[i].time)
    {
        idxtbl[i].time    = ltime;
        idxtbl[i].fileofs = ttell(Fp_tit);
if (0) printf("== i=%d saving time=%d file_ofs=%d\n",
        nfidx, ltime, ttell(Fp_tit));
        ++nfidx;
        if (nfidx >= MAX_FIDX)
        {
            printf("output_data: too many file indexes (max %d); reset\n",
                    MAX_FIDX);
            for (i=0; i<MAX_FIDX; i++)
                idxtbl[i].time = 0;
            nfidx = 0;
        }
    }

if (0) for (i=0; i<nfidx; i++) printf("     %d\n", idxtbl[i].time);




  if (evn.evn_time != NULL)
  {
     output_event(last, time_jump);
     return;
  }



  for (chan=0; chan<NCHAN; chan++) if (Channel[chan].numcomp)
  {

      if (opt.chan >= 0 && (chan != opt.chan))
      {
          Channel[chan].ninp = 0;
          continue;
      }

/*
 * Check data duration to plot
 */
      if (timeWindowSec <= 0)
      {
          printf("  output_data: plot duration set to 1000 secs\n");
          timeWindowSec = 1000.0;
      }
      timeWindowNsamp = timeWindowSec * Channel[chan].srate;
      if (timeWindowNsamp < 128)         timeWindowNsamp = 128;
      if (timeWindowNsamp > MAX_DATANUM) timeWindowNsamp = MAX_DATANUM;
      if (trig.sta != 0.0)
      {
          if (timeWindowNsamp > (int)(0.75*BUFSIZE))
          {
            printf("  WARNING: output_data: num of data=%d;",timeWindowNsamp);
            printf(" for trigger max is %d\n", (int)(0.75*BUFSIZE));
            timeWindowSec = (int)(0.75*BUFSIZE) / Channel[chan].srate;
            printf("  Duration set to %.1f\n", timeWindowSec);
          }
      }

      if (0) {
          static int first = TRUE;
          if (first)
          {
            first = FALSE;
            printf("  output_data: chan=%d srate=%.5f timeWindowNsamp=%d\n",
                    chan, Channel[chan].srate, timeWindowNsamp);
          }
      }



/*========== Beginning of process or re-initialization ==============*/

    if (Channel[chan].new == TRUE && Channel[chan].ninp > 0)
    {
        saveOutputParms(chan);
        Channel[chan].new = FALSE;
        if (Fp_tit) Beg_file_ofs[chan] = ttell(Fp_tit);
    }



/*====== Case: discontinuous input data or data count reached =======*/

    if (time_jump == TRUE || (Channel[chan].nout >= timeWindowNsamp))
    {
        OutData[chan].nsamples = Channel[chan].nout;
        totOutSamples += OutData[chan].nsamples;

if (0) printf("++++ output_data: send to plot: file_ofs beg %d end %d\n",
           Beg_file_ofs[chan], ttell(Fp_tit));

        if (time_jump == TRUE)
        {
            printf("  output_data: TIME JUMP\n");
        }
        else if (Channel[chan].nout >= timeWindowNsamp)
        {
            if (0) printf("++++ output_data: nsamp reached\n");
        }
        else
        {
            printf("++++ output_data: ????\n");
        }

        send_to_plot(chan);

        if (Fp_tit) Beg_file_ofs[chan] = ttell(Fp_tit);

        Channel[chan].nout = 0;

        saveOutputParms(chan);
        storeInputData(chan);
        Channel[chan].nout += Channel[chan].ninp;
        Channel[chan].ninp = 0;
    }


/*==================  Case: continuous input data ===================*/

    if (time_jump == FALSE && last == FALSE)
    {
        storeInputData(chan);
        Channel[chan].nout += Channel[chan].ninp;
        Channel[chan].ninp = 0;
    }




/*================== Case: end of input data ======================*/

    if (last == TRUE)
    {
        storeInputData(chan);
        Channel[chan].nout += Channel[chan].ninp;
        Channel[chan].ninp = 0;

        OutData[chan].nsamples = Channel[chan].nout;
        totOutSamples += OutData[chan].nsamples;

    printf("  output_data: END OF DATA REACHED\n");

        send_to_plot(chan);
    }
  }
  return;
}



int    dballoc;
int    WH = 800;
int    new_plot;
float  *trigdata;

extern float plot_scale;
extern XY *xy;
extern int nxy;



/*===================================================================*/
static void send_to_plot(chan)
int     chan;
{
int    i, j;
char   *dftfile;
double startime;
int    comp, ncomp, selcomp;
int    nout;
char   str[40];
int    found_time;
int    decim, ndec;
double srate;

/*
 * First, seek to specified time if needed
 */
    if (Seek_time > 0)
    {
        found_time = 0;
        for (i=0; i<nfidx; i++)
        {
            if (Seek_time > (idxtbl[i].time-1000) &&
                Seek_time < (idxtbl[i].time+1000))
            {
                found_time = TRUE;
                break;
            }
        }
        if (found_time)
        {
            Seek_time = 0;
            Seek_ofs = idxtbl[i].fileofs;
            time_asc1(str, (double) idxtbl[i].time);
if (0)      printf("== send_to_plot: seek to time %s file_ofs %d\n",
                    str, Seek_ofs);

        }
        else return;
    }

/*
 * If too few data, nothing to do. return
 */
    if (OutData[chan].nsamples < 128)
    {
       fprintf(Fp_log, "  WARNING: send_to_plot: ");
       fprintf(Fp_log, " only %d samples. Discarded\n",
           OutData[chan].nsamples);
       return;
    }

/*
 * Get data series corrected start time
 */
    startime = GetOutputDataTime(opt.tcorr_mode, Station, chan, &dftfile);
    OutData[chan].start_time = startime;

    ncomp = OutData[chan].numcomp;
    nout  = OutData[chan].nsamples;
    srate = OutData[chan].srate;

/*
 * Clear num samples in structure OutData.
 * This prevents the checking on output data timing to fail
 */
    OutData[chan].nsamples = 0;

if (0) printf("== send_to_plot: ncomp=%d nout=%d d=%.0f\n",
            ncomp, nout, timeWindowSec);


/*================================
 *         Run trigger
 ================================*/

    if (trig.sta != 0.0)
    {
        trigger(chan, srate, nout, startime);

        if (trig.plot == FALSE) return;
    }


/*
 * If data series longer than 10000 samples, do a min-max decimation.
 * Attention: actual decim factor is twice smaller than specified factor.
 */

    if (nout >= 10000)
    {
       decim = nout / 1000;
       decim = (decim / 10) * 10;
       for (comp=0; comp<NCOMP; comp++)
       {
         ndec =
         min_max_dec(nout, out_data[chan][comp],out_data[chan][comp], decim);
       }
       srate = srate / ((double) decim / 2.0);
       nout = ndec;
    }

/*
 * If not already done, allocate memory for xy data
 */
    if (xy == NULL)
    {
        new_plot = 1;
        if (!(xy = (XY *)m_alloc((MAX_XY)*NCOMP*sizeof(XY))))
        {
            fprintf(stderr, "ERROR m_alloc failed for xy\n");
            exit(1);
        }
    }
/*
 * Fill xy data array: 3 xy series concatenated, 1 for each component.
 */
    if ((opt.comp >= 0) && (opt.comp < 3)) selcomp = opt.comp;
    else                                   selcomp = -1;
    nxy = 0;
    for (comp=0; comp<NCOMP; comp++)
    {
      if ((nxy+nout) > (MAX_XY))
      {
          printf("send_to_plot: too many data: %d; max is %d\n",
               nxy+nout, MAX_XY);
          exit(1);
      }
      if (selcomp < 0) for (j=0; j<nout; j++, nxy++)
      {
        xy[nxy].x = (float) j / srate;
        xy[nxy].y = (float) out_data[chan][comp][j];
      }
      else for (j=0; j<nout; j++, nxy++)
      {
        xy[nxy].x = (float) j / srate;
        if (comp == selcomp) xy[nxy].y = (float) out_data[chan][comp][j];
        else                 xy[nxy].y = 0.0;
      }
    }


if (0) printf("== send_to_plot: chan=%d comp=%d n=%d, d=%.1f new=%d\n",
        chan, comp, nxy, timeWindowSec, new_plot);

    plot_loop(chan);
}


/*==================================================================*
    storeInputData()
    Add input data into output data array.
 *==================================================================*/
static void storeInputData(chan)
int  chan;
{
int  comp, j;
int  n_inp, n_out;

    n_inp = Channel[chan].ninp;
    n_out = Channel[chan].nout;
    if ((n_out + n_inp) > (int) (MAX_DATANUM+1000))
    {
      fprintf(Fp_log,"  ERROR: storeInputData: too many samples; ");
      fprintf(Fp_log,"max allowed is %d\n",(int)(MAX_DATANUM+1000));
      exit(1);
    }

/*
    printf("store: chan=%d ninp=%d nout=%d\n", chan, n_inp, n_out);
*/

/* Allocate memory for output data */

    if (out_data[chan][0] == NULL)
    {
        for (comp=0; comp<NCOMP; comp++)
        {
          if (!(out_data[chan][comp] =
                 (int*)m_alloc((MAX_DATANUM+1000)*sizeof(int))))
          {
              fprintf(stderr, "ERROR m_alloc failed for out_data[]\n");
              exit(1);
          }
        }
    }

    for (comp=0; comp<NCOMP; comp++)
    {
        for (j=0; j<n_inp; j++)
        {
            out_data[chan][comp][n_out+j] = inp_data[chan][comp][j];
        }
    }
    return;
}


/*==================================================================*/
static void output_event(flag, time_jump)
int flag;
int time_jump;
{
static int     event_in_process;
static int     found_event[NCHAN];
static double  evntime;
static double  duration;
double         current_time;
int            i;
char           s1[40], s2[40];
int            chan;
extern double  SystTime;

  for (chan=0; chan<NCHAN; chan++) if (Channel[chan].numcomp)
  {
    if (opt.chan >= 0 && (chan != opt.chan))
    {
        Channel[chan].ninp = 0;
        continue;
    }

    current_time = SystTime -Channel[chan].adcdelay -Channel[chan].filtdelay;

    if (found_event[chan] == TRUE)
    {
        storeInputData(chan);
        Channel[chan].nout += Channel[chan].ninp;
        Channel[chan].ninp = 0;

        if (current_time >= (evntime + duration) || 
           time_jump || flag == LAST ||
           (Channel[chan].nout >= (BLKLEN * MAXBLK)))
        {
            found_event[chan] = FALSE;

            OutData[chan].nsamples = Channel[chan].nout;
            totOutSamples += OutData[chan].nsamples;

            if (time_jump) printf("==== TIME JUMP\n");
            time_asc1(s1, current_time);
            printf("= end event #%3d chan %d %s %5d samples tot %d\n",
                evn.cur_event, chan, s1, Channel[chan].nout, totOutSamples);

            send_to_plot(chan);

            Channel[chan].nout = 0;
        }
    }

/*
 * Loop thru all avents in list
 */

    else for (i=0; i<evn.num_events; i++)
    {
        if (event_in_process == FALSE)
        {
            evntime = (double) evn.evn_time[i];
        }
/*
 * Do we find an event close enough to the current time ?
 */
        if (evntime > (current_time-12.0) &&
            evntime < (current_time+12.0))
        {
          found_event[chan] = TRUE;

/* Raise process flag and set duration */

          if (event_in_process == FALSE)
          {
            duration = (evn.evn_duration[i] > 0) ?
               (double) evn.evn_duration[i] : (double) opt.evn_duration;
            event_in_process  = TRUE;
          }

          if (i != evn.cur_event)
          {
            if (evn.evn_time[i] < evn.evn_time[evn.cur_event])
            {
              time_asc4(s1, evn.evn_time[i]);
              time_asc4(s2, evn.evn_time[evn.cur_event]);
              fprintf(Fp_log,"  WARNING: ouput_event: %.19s (%d) ", s1, i);
              fprintf(Fp_log,"before %.19s (%d)\n", s2, evn.cur_event);
            }
            else if (1)
            {
              double diff;
              time_asc4(s1, evn.evn_time[evn.cur_event]);
              time_asc4(s2, evn.evn_time[evn.cur_event-1]);
              fprintf(Fp_log,"  WARNING: ouput_event: %.19s (%d) ",s1,i-1);
              fprintf(Fp_log,"too close to %.19s (%d)\n",s2,evn.cur_event-1);

              diff = evn.evn_time[evn.cur_event]
                   - evn.evn_time[evn.cur_event-1];
              fprintf(Fp_log,"           time difference: %.1f secs\n",diff);

            }
            fprintf(Fp_log,"           event index %d skipped\n", i-1);
            evn.cur_event=i;
          }

          time_asc1(s1, current_time);
          printf("= beg event #%3d chan %d %s duration %.1f secs\n",
                evn.cur_event, chan, s1, duration);

          if (Fp_tit) Beg_file_ofs[chan] = ttell(Fp_tit);

          saveOutputParms(chan);
          storeInputData(chan);
          Channel[chan].nout = Channel[chan].ninp;
          Channel[chan].ninp = 0;

          break;
        }
    }

    Channel[chan].ninp = 0;
  }

  if (event_in_process)
  {
    for (chan=0; chan<NCHAN; chan++) if (Channel[chan].numcomp)
    {
      if (opt.chan >= 0 && (chan != opt.chan)) continue;
      if (found_event[chan]) break;
    }
    if (chan == NCHAN)
    {
        event_in_process = FALSE;
        evn.done=TRUE;
        ++evn.cur_event;
    }
  }
}


/*=====================================================================
 *  min_max_dec
 *  decimate an int array src, length npts, by a factor of decim.
 *  Return in int array dest (may be same as src)
 *
 *====================================================================*/
static int min_max_dec(npts, src, dest, decim)
int npts, *src;
int *dest, decim;
{
    int i, nout;
    int imin, imax;

    if (decim <= 1) {
        for (i=0; i<npts; i++) dest[i] = src[i];
        nout = npts;
        return nout;
    }

/* Do the decimation */

    imax = imin = src[0];
    nout = 0;
    for (i=1; i<npts; i++)
    {
        if (src[i] < imin) imin = src[i];
        if (src[i] > imax) imax = src[i];
        if ( (i % decim) == 0 )
        {
            dest[nout++] = imin;
            dest[nout++] = imax;
            imin = imax  = src[i];
        }
    }

    dest[nout++] = imin;
    dest[nout++] = imax;
    return nout;
}




/*====================================================================*/
static void sta_lta(data, ndata, startime, file_ofs, sint, trig, new)
float    *data;
int      ndata;
double   startime;
double   sint;
int      file_ofs;
Trigparm trig;
int      *new;
{
int    nc1, nc2;
double sum1, sum2;
double sta, lta;
double max_sta, max_lta;
int    i, j;
int    trig_on;
static double dtime;
static double prev_dtime;
double t1, t2;
static double lta_upon_trig;
static double sta_lta_upon_trig;
char   str[40];
FILE   *Fp;
static FILE *Fp_trig;
char   trig_fname[PATHLEN];
static double *fbuf1;
static double *fbuf2;
static float *obuf1;
static float *obuf2;
static int dbug = 0;

    t1      = 0.0;
    trig_on = 0;
    nc1 = (double) trig.sta / sint;
    nc2 = (double) trig.lta / sint;

    if (ndata > BUFSIZE)
    {
        fprintf(stderr, "ERROR: sta_lta: too many data: %d; max is %d\n",
            ndata, BUFSIZE);
        exit(1);
    }
    if (dbug)
    {
        if (obuf1 == NULL)
        {
            if (!(obuf1= (float *)m_alloc((BUFSIZE)*sizeof(float))))
            {
                fprintf(stderr,"ERROR: sta_lta: m_alloc failed for obuf1\n");
                exit(1);
            }
            if (!(obuf2= (float *)m_alloc((BUFSIZE)*sizeof(float))))
            {
                fprintf(stderr,"ERROR: sta_lta: m_alloc failed for obuf2\n");
                exit(1);
            }
        }
    }
    if (fbuf1 == NULL)
    {
        if (!(fbuf1= (double*)m_alloc((BUFSIZE)*sizeof(double))))
        {
            fprintf(stderr,"ERROR: sta_lta: m_alloc failed for fbuf1\n");
            exit(1);
        }
        if (!(fbuf2= (double*)m_alloc((BUFSIZE)*sizeof(double))))
        {
            fprintf(stderr,"ERROR: sta_lta: m_alloc failed for fbuf2\n");
            exit(1);
        }
    }

    if (*new)
    {
        double mean;

        *new = 0;
        mean = 0.0;
        for (i=0; i<nc2; i++)
        {
            if (data[i] > 0.0) mean += data[i];
            else               mean -= data[i];
        }
        mean /= nc2;

        time_asc4(str, FirstSystTime);
        sprintf(trig_fname,"%.19s.%s.trig", str, Station);
        open_Fwr(trig_fname, &Fp_trig);

if (0) printf("**** sta_lta: new series: ndata=%d first systime %.19s\n",
                  ndata, str);
if (0) printf("**** sint=%f sta=%.1f lta=%.1f ratio=%.1f (n1=%d n2=%d)\n", 
                  sint, trig.sta, trig.lta, trig.ratio, nc1, nc2);

        for (i=0; i<nc1; i++) fbuf1[i] = mean;
        for (i=0; i<nc2; i++) fbuf2[i] = mean;
    }


/* load "fbuf1" and "fbuf2" at starting index "nc1", "nc2" respectively */

    for (i=0; i<ndata; i++)
    {
        if (data[i] > 0.0) { fbuf1[nc1 + i] = fbuf2[nc2 + i] = data[i]; }
        else               { fbuf1[nc1 + i] = fbuf2[nc2 + i] = -data[i]; }
    }


/* For ndata, compute STA from fbuf1 and LTA from fbuf2 */
/* Then find if trigger-on or trigger-off criteria are met */

    sum1 = 0.0;
    sum2 = 0.0;
    max_sta = 0.0;
    max_lta = 0.0;
    for (j=0; j<ndata; j++)
    {
        if (j == 0) for (i=0; i<nc1; i++) sum1 += fbuf1[i+j];
        else  sum1 = sum1 - fbuf1[j-1] + fbuf1[j+nc1-1];
        sta = (float)(sum1 / (double) nc1);
        if (sta > max_sta) max_sta = sta;
        if (dbug) obuf1[j] = sta;

        if (j == 0) for (i=0; i<nc2; i++) sum2 += fbuf2[i+j];
        else  sum2 = sum2 - fbuf2[j-1] + fbuf2[j+nc2-1];
        lta = (float)(sum2 / (double) nc2);
        if (lta > max_lta) max_lta = lta;
        if (dbug) obuf2[j] = lta;


    /* Look for trigger-on criteria */

        if ((trig_on == 0) &&
            ((sta/lta) > trig.ratio) && (j > 30))
        {
            trig_on = 1;
    max_sta = 0.0;
    max_lta = 0.0;
            t1 = (double) j * sint;
            dtime = startime + t1;
            lta_upon_trig = lta;
            sta_lta_upon_trig = sta / lta;
            time_asc4(str, dtime);
            if (0) printf("---- trigger on t=%.2f j=%d\n", t1, j);
        }

    /* Look for trigger off criteria */

        if ((trig_on == 1) && 
            ((sta/lta) < trig.ratio) && (lta < (lta_upon_trig*2.0)))
        {
            trig_on = 0;
            time_asc4(str, dtime);
            t2 = (double) j * sint;
            if ((t2-t1) > 0.8)
            {
              if ((dtime-prev_dtime) > 10.0)
              {
                printf("==== trigger %.19s (%.1f) dur=%.1f ofs=%d\n",
                       str, (double) j * sint, t2-t1, file_ofs); 
                fprintf(Fp_trig, "%.19s 0 0   utime=%d ofs=%d sta=%s ",
                        str, (int) dtime, file_ofs, Station);
                fprintf(Fp_trig, "sta/lta=%.2f dur=%.1f\n",
                        (max_sta/max_lta), t2-t1);
                fflush(Fp_trig);
              }
              prev_dtime = dtime;
            }
          /*
            if ((t2-t1) > 0.1) 
            {
                printf("---- trigger %.19s (%.1f) dur=%.1f ofs=%d\n",
                       str, (double) j * sint, t2-t1, file_ofs);
            }
          */
        }
    }

/* End of data reached */

    if (trig_on)
    {
        trig_on = 0;
        time_asc4(str, dtime);
        t2 = (double) j * sint;
        printf("==== trigger %.19s (%.1f) dur=%.1f ofs=%d\n",
               str, (double) j * sint, t2-t1, file_ofs); 
        fprintf(Fp_trig, "%.19s 0 0   utime=%d ofs=%d sta=%s ",
                str, (int) dtime, file_ofs, Station);
        fprintf(Fp_trig, "sta/lta=%.2f dur=%.1f\n",
                (max_sta/max_lta), t2-t1);
        fflush(Fp_trig);
    }

/* shift unprocessed data to beginning of filbuf[] */

    for (i=0; i<nc1; i++) fbuf1[i] = fbuf1[i+j];
    for (i=0; i<nc2; i++) fbuf2[i] = fbuf2[i+j];

    if (dbug)
    {
        Fp = fopen("sta.dat", "w");
        fwrite(obuf1, sizeof(float), ndata, Fp);
        fclose(Fp);

        Fp = fopen("lta.dat", "w");
        fwrite(obuf2, sizeof(float), ndata, Fp);
        fclose(Fp);
    }
}


/*================================================================*/
static void dtrend(x, l)
float *x;
int l;
{
float s, s11, s12, s22;
float a, b, v;
float w;
int i;

    s = l;
    s11 = 12./(s*(s*s-1.));
    s12 = 6./(s*(s-1.));
    s22 = (2.*(2.*s+1.))/(s*(s-1.));
    v = 0.;
    w = 0.;
    for (i=0; i<l; i++) {
        v += x[i];
        w += x[i] * (float) (i+1);
    }
    a = s22*v - s12*w;
    b = s11*w - s12*v;
    for (i=0; i<l; i++) {
        x[i] = x[i] - a - b * (float) (i+1);;
    }
}


/*==================================================================*
 * We apply 2 processes to the input data: remove trend and bandpass
 * filtering.
 * Highpass filtering generates a transcient signal whose duration (settle 
 * time) is in the order of the period of the corner period. The transcient 
 * effect is removed by copying the "n" last samples of the input series
 * at the beginning of the array holding the input data.
 * Here the array holding the data send to the trigger module is "trigdata".
 * The settle time of the filter is "ntemp" and is computed as 4 times
 * the highpass corner period times the sample rate.
 * The trigger process takes the sample number "ntemp" as first sample.
 *==================================================================*/
static void trigger(chan, srate, nout, startime)
int     chan;
double  srate;
int     nout;
double  startime;
{
static int new_series;
static float *temp;
static int   ntemp;
double sint;
int    beg_ofs;
float  flow, fhigh, sv[10];
int    comp;
int    j;
static int first = TRUE;


    if ((opt.comp >= 0) && (opt.comp < 3)) comp = opt.comp;
    else                                   comp = 0;
    sint    = 1.0/srate;
    beg_ofs = Beg_file_ofs[chan];

/*
 * band pass filter: default 0.05 to 20 hz
 */
    flow = 0.05;
    fhigh = 20.0;
    if (trig.fl != 0.0)
    {
        flow = trig.fl;
        fhigh = trig.fh;
    }
    if (first == TRUE)
    {
        first = FALSE;
        fprintf(Fp_log, "\n");
        fprintf(Fp_log, "    Trigger parameters:\n");
        fprintf(Fp_log, "\tSTA %.1f sec  lta %.1f sec  ratio %.1f\n",
            trig.sta, trig.lta, trig.ratio);
        fprintf(Fp_log, "\tbandpass filter : flow %.1f Hz  fhigh %.1f Hz\n",
            flow, fhigh);
        fprintf(Fp_log, "\n");
    }

    if (trigdata == NULL)
    {
        new_series = 1;
        if (!(trigdata = (float *)m_alloc((BUFSIZE)*sizeof(float))))
        {
            fprintf(stderr, "ERROR: trigger: m_alloc failed for trigdata\n");
            exit(1);
        }
    }
    if (temp == NULL)
    {
        ntemp = rint(4.0 / flow * srate);
        if (!(temp = (float *)m_alloc((ntemp+100)*sizeof(float))))
        {
            fprintf(stderr, "ERROR: trigger: m_alloc failed for temp\n");
            exit(1);
        }
    }
    if ((nout+ntemp) > BUFSIZE)
    {
        fprintf(stderr,"ERROR: trigger: too many data: %d; max is %d\n",
            (nout+ntemp), BUFSIZE);
        exit(1);
    }

    if (new_series)
    {
        memset(trigdata, 0, ntemp);
/*
 * Copy input data into "trigdata"
 */
        for (j=0; j<nout; j++)
        {
            trigdata[ntemp+j] = (float) out_data[chan][comp][j];
        }
/*
 * Remove trend
 */
        dtrend(&trigdata[ntemp], nout);
        
if (0) printf("===== ntemp=%d nout=%d (ntemp-nout=%d)\n",
           ntemp, nout, ntemp-nout);

/*
 * load "ntemp" input data into trigdata at starting index 0
 */
        if (ntemp < nout)
        {
           for (j=0; j<ntemp; j++) trigdata[j] = trigdata[ntemp+j];
        }
/*
 * else load "nout" input data into trigdata at starting index ntemp-nout
 */
        else
        {
           for (j=0; j<nout; j++) trigdata[ntemp-nout+j] = trigdata[ntemp+j];
        }
    }

    else
    {
/*
 * load "nout" out_data into trigdata at starting index ntemp
 */
        for (j=0; j<nout; j++)
            trigdata[ntemp+j] = (float) out_data[chan][comp][j];
    }

/*
 * save the "ntemp" last data of out_data
 */
    for (j=0; j<ntemp; j++)
        temp[j] = (float) out_data[chan][comp][nout-ntemp+j];

/*
 * remove trend
 */
    if (1) dtrend(trigdata, nout+ntemp);

/*
 * band pass
 */
    if (1) ciir(flow, fhigh, (float)sint, trigdata, nout+ntemp, sv);

/*
 * If plot set, we plot the filtered data instead of raw data.
 * Then, copy back filtered data into out_data, starting at index ntemp
 */

    if (trig.plot == TRUE) for (j=0; j<nout; j++)
        out_data[chan][comp][j] = (int) trigdata[ntemp+j];

    if (0)
    {
      FILE *Fp;
        Fp = fopen("f.dat", "w");
        if (1) fwrite(&trigdata[0], sizeof(float), ntemp+nout, Fp);
        if (0) fwrite(&trigdata[ntemp], sizeof(float), nout, Fp);
        fclose(Fp);
    }

/*======================================
 *         Run sta/lta trigger
 *=====================================*/

    sta_lta(&trigdata[ntemp],nout,startime,beg_ofs, sint, trig, &new_series);

/*
 * copy the "ntemp" data from "temp" into "trigdata" for next run
 */
    for (j=0; j<ntemp; j++) trigdata[j] = temp[j];

    return;
}

