/*======================================================================
    libout.c
    Library for Titan to binary data converter
    Author: J.-F. Fels, OMP, Toulouse

    Output data formats involved: SAC, AH, SEGY and BINARY.
    Other formats: SISMALP, MINISEED, ASCII are in separate source files.
*======================================================================*/
#include "titan.h"
#include "proto.h"
#include "sac.h"


/* Prototypes */

#ifdef ANSI_C
void         OutputData       (int, int);
static void  openDataFiles    (int);
static void  closeDataFiles   (int);
static void  writeData        (int);
static int   outputTimespan   (int);
static void  deleteDataFiles  (int, int);
static long  maxLong          (int*, int);
static long  minLong          (int*, int);
static float maxFloat         (float *, int);
static float minFloat         (float *, int);
#else
void         OutputData       ();
static void  openDataFiles    ();
static void  closeDataFiles   ();
static void  writeData        ();
static int   outputTimespan   ();
static void  deleteDataFiles  ();
static long  maxLong          ();
static long  minLong          ();
static float maxFloat         ();
static float minFloat         ();
#endif



extern TITFILE *Fp_tit;
extern int     inp_data[NCHAN][NCOMP][NINP]; /* Input data array */
extern struct  Channel Channel[NCHAN];
extern outData OutData[NCHAN];
extern struct  option opt;
extern FILE    *Fp_log;
extern Event   evn;
extern struct  data_list *list_head;
extern struct  data_list *list_tail;
extern struct  acq acq;
extern int     totOutSamples;
extern char    Station[8];
extern int     byteswap;
extern char    tqf;
extern int     nodata;
extern double  SystTime;
extern double  Observ_dt;
extern double  ExtraTcorr;
extern int     foundDftFiles;
extern Titseg  TitSegment[2048];
extern int     NTitSegment;
extern int     TitSegmentNum;

FILE  *Fp_head[NCHAN][NCOMP];
FILE  *Fp_data[NCHAN][NCOMP];
char   fname[NCHAN][NCOMP][PATHLEN];

/* Min-max variables */
static int   long_max[NCHAN][NCOMP];
static int   long_min[NCHAN][NCOMP];

static float fmax[NCHAN][NCOMP];
static float fmin[NCHAN][NCOMP];
static float float_max[NCHAN][NCOMP];
static float float_min[NCHAN][NCOMP];


/* For Segy */
int          imax[NCHAN][NCOMP];
int          imin[NCHAN][NCOMP];

/* Form AH */
extern int *Fsamp[NCHAN][NCOMP];


/*==================================================================*
    OutputData: output data formats involved: SAC, AH, SEGY and BINARY.

    Algorithm:

      if Beginning of process or re-initialization
          open output files

      then, 3 cases:

      if no time jump
          write data out. That's all

      if time jump
          write headers
          close current output files
          open new output files
          write data

      if end of input titan file
          write remaining data
          write headers
          close current output files

 *==================================================================*/
void OutputData(last, time_jump)
int last;
int time_jump;
{
char          str[40];
int           chan;
int           found_tspan_break;

  if (evn.evn_time != NULL)
  {
     flushInputData(last, time_jump);
     Event2TitanSeg(last, time_jump);
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
printf("== OutputData chan=%d n=%d new=%d\n",
    chan, Channel[chan].ninp, Channel[chan].new);
*/

/*========== Beginning of process or re-initialization ==============*/

    if (Channel[chan].new == TRUE && Channel[chan].ninp > 0)
    {
        openDataFiles(chan);
        saveOutputParms(chan);
        Channel[chan].new = FALSE;
    }

/*==================  Case: segmented ouput data  ===================*/

    found_tspan_break = FALSE;
    if (opt.timespan != 0.0)
    {
      found_tspan_break = outputTimespan(chan);
      if (found_tspan_break == TRUE) continue;
    }

/*==================  Case: continuous input data ===================*/

    if (time_jump==FALSE && last==FALSE && found_tspan_break==FALSE)
    {
        writeData(chan);
        Channel[chan].nout += Channel[chan].ninp;
        Channel[chan].ninp = 0;
    }

/*================== Case: discontinuous input data =================*/

    if (time_jump == TRUE)
    {
        OutData[chan].nsamples = Channel[chan].nout;
        totOutSamples += OutData[chan].nsamples;

        time_asc4(str, OutData[chan].uncorrected_time);
        fprintf(Fp_log,
            "  %s: chan %d %5d samples %s tot %d\n",
            Station, chan, Channel[chan].nout, str, totOutSamples);

        closeDataFiles(chan);

        openDataFiles(chan);
        saveOutputParms(chan);
        writeData(chan);
        Channel[chan].nout = Channel[chan].ninp;
        Channel[chan].ninp = 0;
    }

/*================== Case: end of input data ======================*/

    if (last == TRUE)
    {
        writeData(chan);
        Channel[chan].nout += Channel[chan].ninp;
        Channel[chan].ninp = 0;

        OutData[chan].nsamples = Channel[chan].nout;
        totOutSamples += OutData[chan].nsamples;

        time_asc4(str, OutData[chan].uncorrected_time);
        fprintf(Fp_log,
            "  %s: chan %d %5d samples %s tot %d\n",
            Station, chan, Channel[chan].nout, str, totOutSamples);

        closeDataFiles(chan);
    }
  }
  return;
}


/*==================================================================*
    openDataFiles
    Output files name formats:
        data:    yyyy.mm.dd-hh.mn.ss.STA.C.c.sac .segy .data .ah
        headers: yyyy.mm.dd-hh.mn.ss.STA.C.c.info
 *==================================================================*/
static void openDataFiles(chan)
int chan;
{
int    comp;
char   name[PATHLEN];
char   prefix[PATHLEN];
int    nc;

    if (NTitSegment != 0)
    {
        double evntime = TitSegment[TitSegmentNum].utime;
        nc = get_output_name(evntime, prefix);
    }
    else
    {
        nc = get_output_name(Channel[chan].uncorrected_time, prefix);
    }

    sprintf(prefix, "%s.%d", prefix, chan);

/*
   Check if new name is different from previous. If identical,
   issue a warning. This may happend if the start time differ 
   by less than 1 second.
*/
    if (!strncmp(prefix, fname[chan][0], nc))
    {
      fprintf(Fp_log,"  WARNING: open_data: file name already in use:\n");
      fprintf(Fp_log,"      previous: %.25s (overwritten)\n",fname[chan][0]);
      fprintf(Fp_log,"      current : %.25s\n",prefix);
    }

    if (nodata == TRUE) return;

    for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
        if (opt.comp >= 0 && (comp != opt.comp)) continue;

        sprintf(fname[chan][comp], "%s-%d", prefix, comp);

/* Initialize min-max */
long_max[chan][comp] = imax[chan][comp] = LONG_MIN;
long_min[chan][comp] = imin[chan][comp] = LONG_MAX;
float_max[chan][comp] = fmax[chan][comp] = (float) LONG_MIN;
float_min[chan][comp] = fmin[chan][comp] = (float) LONG_MAX;

        if (Fp_head[chan][comp] != NULL)
        {
          fprintf(Fp_log,"  WARNING: openDataFiles: file pointer ");
          fprintf(Fp_log,"Fp_head[chan][%d] not null\n", comp);
          fclose(Fp_head[chan][comp]);    /* ModifOC */
          Fp_head[chan][comp]=NULL;
        }
        if (Fp_data[chan][comp] != NULL)
        {
          fprintf(Fp_log,"  WARNING: openDataFiles: file pointer ");
          fprintf(Fp_log,"Fp_data[chan][%d] not null\n", comp);
          if (opt.do_ah)
          {

              xdr_destroy(&((AHFILE*)Fp_data[chan][comp])->xdr);
              fclose(((AHFILE*)Fp_data[chan][comp])->fp); /* Modif OC */
              free(Fp_data[chan][comp]);
              Fp_data[chan][comp]=NULL;
          }
          else
          {
              fclose(Fp_data[chan][comp]);    /* ModifOC */
              Fp_data[chan][comp]=NULL;
          }
        }

/* open info files */

        if (opt.do_ah && opt.noinfo == TRUE);
        else
        {
            sprintf(name, "%s.info", fname[chan][comp]);
            open_Fwr(name, &Fp_head[chan][comp]);
        }

/* open data files */

        if (opt.do_sac)
        {
            sprintf(name, "%s.SAC", fname[chan][comp]);
            open_Fwr(name, &Fp_data[chan][comp]);
        }
        else if (opt.do_segy)
        {
            sprintf(name, "%s.SEGY", fname[chan][comp]);
            open_Fwr(name, &Fp_data[chan][comp]);
        }
        else if (opt.do_ah)
        {
          AHFILE *ahfp;
            ahfp=(AHFILE*)mem_alloc(sizeof(AHFILE),"open_data_file");
            sprintf(name, "%s.ahz", fname[chan][comp]);
            ahfp->fp=fopen(name,"w");
            xdrstdio_create(&ahfp->xdr,ahfp->fp,XDR_ENCODE);
            Fp_data[chan][comp]=(FILE*)ahfp;
        }
        else if (opt.do_bindata)
        {
            sprintf(name, "%s.data", fname[chan][comp]);
            open_Fwr(name, &Fp_data[chan][comp]);
        }
        else
        {
            fprintf(Fp_log,"  ERROR: unsupported format\n");
            exit(1);
        }
        if (opt.do_sac)
        {
            struct sac sach;  /* SAC header structure */

            if (!byteswap && opt.sacsun) sach = sac_nullswap;
            else                         sach = sac_null;
            fwrite(&sach, sizeof(struct sac), 1, Fp_data[chan][comp]);
        }
        else if (opt.do_segy)
        {
            char segy_hd[250];

            fwrite(segy_hd, 240, 1, Fp_data[chan][comp]);
        }

    }
}


/*==================================================================*/
static void closeDataFiles(chan)
int chan;
{
int    comp;
struct data_list *plist;
char   *dftfile = NULL;
double startime;
ahhed  ahd;

    if (nodata == TRUE) return;

    if (OutData[chan].nsamples <= 64)
    {
        deleteDataFiles(chan, 64);
        return;
    }

    startime = GetOutputDataTime(opt.tcorr_mode, Station, chan, &dftfile);

    if (dftfile) sprintf(OutData[chan].dftfile, "%s", dftfile);
    OutData[chan].start_time = startime;


    if (opt.noinfo == FALSE) for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
        if (opt.comp >= 0 && (comp != opt.comp)) continue;
        wrt_info(chan, comp, startime, Fp_head[chan][comp]);
    }

/* Save data parameters into linked list */

    for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
        if (opt.comp >= 0 && (comp != opt.comp)) continue;

if (0)
{
        if (opt.do_sac)
          printf("  %s %5d samples min %.1f max %.1f\n",
                    fname[chan][comp], OutData[chan].nsamples,
                    fmin[chan][comp], fmax[chan][comp]);
        else
          printf("  %s %5d samples min %d max %d\n",
                    fname[chan][comp], OutData[chan].nsamples,
                    imin[chan][comp], imax[chan][comp]);
}

        plist = (struct data_list*)
              mem_alloc(sizeof(struct data_list), "closeDataFiles");
        append_linklist_element(plist, list_head, list_tail);
  
        plist->header = (outData *)
              mem_alloc(sizeof(outData), "closeDataFiles");
        memcpy(plist->header, &OutData[chan], sizeof(outData));
        sprintf(plist->station, "%s", Station);
        plist->chan = chan;
        plist->comp = comp;
        plist->data_fname =
             mem_alloc(strlen(fname[chan][comp])+2, "closeDataFiles");
        sprintf(plist->data_fname, "%s", fname[chan][comp]);
    }


/* Rewind SAC files and write header */

    if (opt.do_sac) for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
        if (opt.comp >= 0 && (comp != opt.comp)) continue;
        rewind(Fp_data[chan][comp]);
        write_sac_head(chan, comp, startime);
    }

/* Rewind SEGY files and write header */

    if (opt.do_segy) for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
        if (opt.comp >= 0 && (comp != opt.comp)) continue;
        rewind(Fp_data[chan][comp]);
        write_segy_head(chan, comp, startime);
    }



    if (opt.do_ah)
    {
        get_null_head(&ahd);
        ahd.record.type  = INTZ;
        ahd.record.ndata = OutData[chan].nsamples;
        for (comp=0; comp<Channel[chan].numcomp; comp++)
        {
          if (opt.comp >= 0 && (comp != opt.comp)) continue;
/* Write AH header */
          write_ahz_head(chan, comp, startime);
/* Writing AHZ data stored into Fsamp[][] */
          xdr_putdata(
              &ahd,
              (char *) Fsamp[chan][comp],
              &((AHFILE *) Fp_data[chan][comp])->xdr);
        }
    }

/*
 * Reset all variables in structure OutData.
 */

    clearOutputParms(chan);

/*
 * Close header and data files. Check for eventual null pointer
 */

    for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
      if (opt.comp >= 0 && (comp != opt.comp)) continue;

      if (0 && comp == 0) printf("  closing %.19s for chan %d  %d samples\n",
          fname[chan][comp], chan, OutData[chan].nsamples);


      if (Fp_head[chan][comp] == NULL)
      {
          if (opt.noinfo == FALSE)
            fprintf(Fp_log,"  WARNING: closeDataFiles: null hd pointer\n");
      }
      else
      {
          fclose(Fp_head[chan][comp]);
          Fp_head[chan][comp] = NULL;
      }



      if (Fp_data[chan][comp] == NULL)
          fprintf(Fp_log,"  WARNING: closeDataFiles: null data pointer\n");
      else
      {
          if (opt.do_ah)
          {
              /* xdr_destroy(&((AHFILE*)Fp_data[chan][comp])->xdr);*/
              fclose(((AHFILE*)Fp_data[chan][comp])->fp);
              free(Fp_data[chan][comp]);
          }
          else
          {
              fclose(Fp_data[chan][comp]);
          }
          Fp_data[chan][comp] = NULL;
      }
    }

}


/*==================================================================*/
static void writeData(chan)
int chan;
{
float  fsamp[NINP];
int    lsamp[NINP];
int    comp;
int    i;

    if (nodata == TRUE) return;
    if (Channel[chan].ninp == 0) return;

    if (Channel[chan].ninp > PRIM_NUM_DATA)
    {
        fprintf(Fp_log,"  WARNING: writeData: chan %d : ",chan);
        fprintf(Fp_log,"too many input data: %d; ", Channel[chan].ninp);
        fprintf(Fp_log,"max is %d\n", PRIM_NUM_DATA);
        Channel[chan].ninp = PRIM_NUM_DATA;
    }

/*
 * Copy input data and compute data min-max.
 *
 * If not SAC, copy input data into lsamp[] and
 * compute min-max into imin, imax.
 *
 * If SAC, copy input data into fsamp[] and
 * compute min-max into fmain, fmax.
 */

    if (opt.do_sac == FALSE) for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
      if (opt.comp >= 0 && (comp != opt.comp)) continue;

      for (i=0; i<Channel[chan].ninp; i++)
          lsamp[i] = inp_data[chan][comp][i];

      long_max[chan][comp] = maxLong(lsamp, Channel[chan].ninp);
      if (long_max[chan][comp] > imax[chan][comp])
         imax[chan][comp] = long_max[chan][comp];

      long_min[chan][comp] = minLong(lsamp, Channel[chan].ninp);
      if (long_min[chan][comp] < imin[chan][comp])
         imin[chan][comp] = long_min[chan][comp];
    }

    else for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
      if (opt.comp >= 0 && (comp != opt.comp)) continue;

/*==========
  Test for post decimation: replace data by square waves.
      {
      static int nn;
          if ((nn%2) == 0) for (i=0; i<Channel[chan].ninp; i++)
                              fsamp[i] = inp_data[chan][comp][i] = 10000;
          else             for (i=0; i<Channel[chan].ninp; i++)
                              fsamp[i] = inp_data[chan][comp][i] = 0;
          ++nn;
      }
===========*/

      for (i=0; i<Channel[chan].ninp; i++)
          fsamp[i] = (float) inp_data[chan][comp][i];

      float_max[chan][comp] = maxFloat(fsamp, Channel[chan].ninp);
      if (float_max[chan][comp] > fmax[chan][comp])
         fmax[chan][comp] = float_max[chan][comp];

      float_min[chan][comp] = minFloat(fsamp, Channel[chan].ninp);
      if (float_min[chan][comp] < fmin[chan][comp])
         fmin[chan][comp] = float_min[chan][comp];
    }

/*
 * Write to disk
 */

/*
 * AH
 */
    if (opt.do_ah)
    {
        write_ahz_data(chan);
        return;
    }
/*
 * SEGY
 */
    if (opt.do_segy) for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
      if (opt.comp >= 0 && (comp != opt.comp)) continue;

      for (i=0; i<Channel[chan].ninp; i++)
          lsamp[i] = inp_data[chan][comp][i];
      fwrite(lsamp, sizeof(int), Channel[chan].ninp, Fp_data[chan][comp]);
    }
/*
 * Other formats: BIN, SAC
 */
    else for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
      if (opt.comp >= 0 && (comp != opt.comp)) continue;

      for (i=0; i<Channel[chan].ninp; i++)
          fsamp[i] = (float) inp_data[chan][comp][i];
      if (opt.do_sac && opt.sacsun && !byteswap)
          swap_4byte_array(fsamp,Channel[chan].ninp);
      fwrite(fsamp, sizeof(float), Channel[chan].ninp, Fp_data[chan][comp]);
    }
}


/*==================================================================*
    Extract timespan
 *==================================================================*/
static int outputTimespan(chan)
int chan;
{
int       ltime;
double    dbtime, dbtime2;
int       found_tspan_break;
int       i, n1, n2;
char      str[40];
int       comp;
double    sint;

    sint = 1.0 / Channel[chan].srate;

    found_tspan_break = FALSE;

    for (n1=0; n1<Channel[chan].ninp; n1++)
    {
      dbtime = Channel[chan].uncorrected_time + ((double) n1 * sint);
      ltime = dbtime;
      if ( !(ltime % (int) opt.timespan) )
      {
        dbtime2 = ltime;
        if (dbtime >= dbtime2 && dbtime < (dbtime2+sint))
        {
          found_tspan_break = TRUE;
          break;
        }
      }
    }

    if (found_tspan_break == TRUE)
    {
      n2 = Channel[chan].ninp;
      Channel[chan].ninp = n1;

      writeData(chan);
      Channel[chan].nout += Channel[chan].ninp;
      Channel[chan].ninp = 0;

      OutData[chan].nsamples = Channel[chan].nout;
      totOutSamples += OutData[chan].nsamples;

      time_asc4(str, OutData[chan].uncorrected_time);
      fprintf(Fp_log,
          "  %s: chan %d %5d samples %s tot %d\n",
          Station, chan, Channel[chan].nout, str, totOutSamples);

      closeDataFiles(chan);


/* Update num of samples, start time and shift data samples */
/* to index 0 */

      Channel[chan].ninp     = n2 - n1;
      Channel[chan].start_time   += ((double) n1 * sint);
      Channel[chan].uncorrected_time += ((double) n1 * sint);
      for (comp=0; comp<Channel[chan].numcomp; comp++)
      {
        for (i=0; i<Channel[chan].ninp; i++)
          inp_data[chan][comp][i] = inp_data[chan][comp][i+n1];
      }
/*
fprintf(Fp_log,"  timespan break:  copy %d data from data[%d]\n", i, n1);
time_asc4(str,  Channel[chan].uncorrected_time);
fprintf(Fp_log,"  timespan break: chan %d %s writing %d samples\n",
            chan, str, Channel[chan].ninp);
*/
      openDataFiles(chan);
      saveOutputParms(chan);
      writeData(chan);
      Channel[chan].nout = Channel[chan].ninp;
      Channel[chan].ninp = 0;
      return TRUE;
    }
    return FALSE;
}


/*===================================================================*/
void wrt_info(chan, comp, startime, Fp_head)
int chan;
int comp;
double startime;
FILE *Fp_head;
{
char   str[40];
char   cmt[40];

    fprintf(Fp_head,"cvtit Revision %s\n", REVISION);
    fprintf(Fp_head,"%s    station\n", Station);
    fprintf(Fp_head,"%d      channel\n", chan);
    fprintf(Fp_head,"%d      comp\n", comp);
    fprintf(Fp_head,"%8.8f   sample rate\n",
        OutData[chan].srate);

    time_asc4(str, startime);
    if      (tqf == '1') sprintf(cmt,"corrected by estimated dt");
    else if (tqf == '2') sprintf(cmt,"corrected by observed dt");
    else if (tqf == '?') sprintf(cmt,"no dt correction applied");
    fprintf(Fp_head,"%s   start time (%s)\n", str, cmt);

    if (OutData[chan].uncorrected_time == (double) UNKNOWN)
    {
        sprintf(str, "??????");
        fprintf(Fp_head,"%s   uncorrected time\n", str);
    }
    else
    {
        time_asc4(str, OutData[chan].uncorrected_time);
        fprintf(Fp_head,"%s   uncorrected time\n", str);
    }

    if (OutData[chan].resettime == (double) UNKNOWN)
    {
        sprintf(str, "??????");
        fprintf(Fp_head,"%s   system reset time  ", str);
    }
    else
    {
        double diff = OutData[chan].uncorrected_time-OutData[chan].resettime;
        time_asc4(str, OutData[chan].resettime);
        fprintf(Fp_head,"%s   system reset time    ", str);
        if (OutData[chan].uncorrected_time == (double) UNKNOWN);
        else
        {
          fprintf(Fp_head," (%.2f days old, %s)",
              diff/86400.0, (OutData[chan].reboot ? "reboot" : "synchro"));
        }
    }
    fprintf(Fp_head,"\n");



    if (OutData[chan].extpulse == (double) UNKNOWN)
    {
        sprintf(str, "??????");
        fprintf(Fp_head,"%s   external pulse time  ", str);
    }
    else
    {
        double diff = OutData[chan].uncorrected_time-OutData[chan].extpulse; 
        time_asc4(str, OutData[chan].extpulse);
        fprintf(Fp_head,"%s   external pulse time  ", str);
        if (fabs(diff) > 3600.0)
        {
            diff /= 86400.0;
            fprintf(Fp_head," (%.2f days old)", diff);
        }
        else
        {
            fprintf(Fp_head," (%d secs old)", (int) diff);
        }

    }
    fprintf(Fp_head,"\n");



    (OutData[chan].observ_dt == (double) UNKNOWN) ?
         sprintf(str, "??????") :
         sprintf(str, "%+.4f", OutData[chan].observ_dt);
    fprintf(Fp_head,"%s  observed dt\n", str);

    (OutData[chan].estim_dt == (double) UNKNOWN) ?
         sprintf(str, "??????") :
         sprintf(str, "%+.4f", OutData[chan].estim_dt);
    fprintf(Fp_head,"%s  estimated dt", str);
    if (strlen(OutData[chan].dftfile))
        fprintf(Fp_head," (%s)", OutData[chan].dftfile);
    fprintf(Fp_head,"\n");

    fprintf(Fp_head,"%+.4f  adc delay\n",
        OutData[chan].adcdelay);
    fprintf(Fp_head,"%+.4f  filter delay\n",
        OutData[chan].filtdelay);
    fprintf(Fp_head,"%.4f extra time correction\n",
        OutData[chan].extra_tcorr);
    fprintf(Fp_head,"%7d  num of samples\n",
        OutData[chan].nsamples);
}

/*==================================================================*/
static void deleteDataFiles(chan, imin)
int chan, imin;
{
int    comp;
char   name[PATHLEN];

    fprintf(Fp_log,"  WARNING: deleteDataFiles: nsamp less than %d ! ",imin);
    fprintf(Fp_log,"Output files removed\n");
    for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
      if (Fp_head[chan][comp] != NULL)
      {
        fclose(Fp_head[chan][comp]); Fp_head[chan][comp] = NULL;
        sprintf(name, "%s.info", fname[chan][comp]);
        unlink(name);
      }
      if (Fp_data[chan][comp] != NULL)
      {
        fclose(Fp_data[chan][comp]); Fp_data[chan][comp] = NULL;
        if      (opt.do_sac)     sprintf(name, "%s.SAC", fname[chan][comp]);
        else if (opt.do_segy)    sprintf(name, "%s.SEGY",fname[chan][comp]);
        else if (opt.do_ah)      sprintf(name, "%s.ahz", fname[chan][comp]);
        else if (opt.do_bindata) sprintf(name, "%s.data",fname[chan][comp]);
        unlink(name);
      }
    }
}

/*================================================================*/
static long maxLong (long_pntr, n)
int *long_pntr;
int n;
{
register int *ptr, *end, imax=LONG_MIN;


    end = long_pntr + n;

    for (ptr=long_pntr; ptr < end; ptr++)
    {
        if (*ptr > imax) imax = *ptr;
    }
    return (imax);
}

/*================================================================*/
static long minLong (long_pntr, n)
int *long_pntr;
int n;
{
register int *ptr, *end, imin=LONG_MAX;

    end = long_pntr + n;

    for (ptr=long_pntr; ptr < end; ptr++)
    {
        if (*ptr < imin) imin = *ptr;
    }
    return (imin);
}

/*================================================================*/
static float maxFloat(pntr, n)
float *pntr;
int n;
{
register float *ptr, *end, fmax=(float)LONG_MIN;


    end = pntr + n;

    for (ptr=pntr; ptr < end; ptr++)
    {
        if (*ptr > fmax) fmax = *ptr;
    }
    return (fmax);
}

/*================================================================*/
static float minFloat(pntr, n)
float *pntr;
int n;
{
register float *ptr, *end, fmin=(float)LONG_MAX;

    end = pntr + n;

    for (ptr=pntr; ptr < end; ptr++)
    {
        if (*ptr < fmin) fmin = *ptr;
    }
    return (fmin);
}

