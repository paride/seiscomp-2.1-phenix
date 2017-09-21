/*======================================================================
    libseedtit.c

    Library for Titan to binary data converter

    Author: J.-F. Fels, OMP, Toulouse

*======================================================================*/
#include "titan.h"
#include "seed.h"
#include "proto.h"
#include "proto_seed.h"


/* Prototypes, private */

#ifdef ANSI_C
static void  openDataFiles    (int);
static void  closeDataFiles   (int);
static void  writeData        (int);
static void  wrtInfo_         (int, int, double, FILE*);
static void  deleteDataFiles  (int, int);
static int   seed_data        (struct data_segment*, FILE*, FILE*);
static int   writeRecords (FILE*, float*, char*, int*, struct data_segment*);
#else
static void  openDataFiles    ();
static void  closeDataFiles   ();
static void  writeData        ();
static void  wrtInfo_         ();
static void  deleteDataFiles  ();
static int   seed_data    ();
static int   writeRecords     ();
#endif



extern TITFILE *Fp_tit;
extern FILE    *Fp_err;        /* file ptr to output SEED log file */ 
extern FILE    *Fp_log;
extern int     inp_data[NCHAN][NCOMP][NINP]; /* Input data array */
extern struct  Channel Channel[NCHAN];
extern outData OutData[NCHAN];
extern struct  option opt;
extern Paths   db_paths;
extern Event   evn;
extern struct  data_list *list_head;
extern struct  data_list *list_tail;
extern struct  acq acq;
extern int     totOutSamples;
extern char    Station[8];
extern int     byteswap;
extern char    tqf;            /* Time quality factor */
extern int     nodata;
extern double  SystTime;
extern double  Observ_dt;
extern double  ExtraTcorr;
extern struct StationParm *staparms_head;
extern char    netname[];
extern char    seed_scalefactor[];
extern char    Network[3];

int     SeedDataformat;
double  SeedDataScaleFactor;


static FILE  *Fp_data[NCHAN][NCOMP];
static char   fname[NCHAN][NCOMP][PATHLEN];
static FILE  *Fp_hdr;
static double EvnTime;
static int    seed_data_record[5000];

extern struct data_segment *dsegm_head;
extern struct data_segment *dsegm_tail;


/*==================================================================*
    output_miniseed(). Algorithm

    if Beginning of process or re-initialization

      open output files

    Then, 3 cases:

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

    All the Seed stuff is called from closeDataFiles(). 
    - seed_data() -> Steim_comp() or writeRecords()

    SPECIAL CASE:  RAP data and seed output (not miniseed):
    - mean value is removed from data.
    - then, data are converted into float values with regard to ground
      acceleration.
    - the conversion constante is calculated in seed_data() by calling
      get_channel_gain().

 *==================================================================*/
void output_miniseed(last, time_jump)
int last;
int time_jump;
{
char          str[40];
int           chan;

  if (!strcmp(netname, "RAP") && opt.do_seed)
  {
      SeedDataformat = IEEEFLOAT;
  }
  else
  {
      SeedDataformat = STEIM1;
  }

  for (chan=0; chan<NCHAN; chan++) if (Channel[chan].numcomp)
  {
      if (opt.chan >= 0 && (chan != opt.chan))
      {
          Channel[chan].ninp = 0;
          continue;
      }

/*========== Beginning of process or re-initialization ==============*/

    if (Channel[chan].new == TRUE && Channel[chan].ninp > 0)
    {
        openDataFiles(chan);
        saveOutputParms(chan);
        Channel[chan].new = FALSE;
    }


/*==================  Case: continuous input data ===================*/

    if (time_jump == 0 && last == FALSE)
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
        fprintf(Fp_log,"  %s: chan %d %5d samples %s tot %d\n",
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
        fprintf(Fp_log,"  %s: chan %d %5d samples %s tot %d\n",
            Station, chan, Channel[chan].nout, str, totOutSamples);
        closeDataFiles(chan);
    }
  }
  return;
}


/*==================================================================*
    Names format:
        data:    yyyy.mm.dd-hh.mn.ss.STA.C.c.data
        headers: yyyy.mm.dd-hh.mn.ss.STA.C.c.info
 *==================================================================*/
static void openDataFiles(chan)
int chan;
{
int    comp;
char   name[PATHLEN];
char   prefix[PATHLEN];
char   date[40];
int    nc;

    if (EvnTime != 0.0)
        nc = get_output_name(EvnTime, prefix);
    else
        nc = get_output_name(Channel[chan].uncorrected_time, prefix);

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

/* open info files */

    if (Fp_hdr == NULL)
    {
        time_asc4(date, Channel[chan].uncorrected_time);
        sprintf(name,"%.19s.%s.info", date, Station);
        open_Fwr(name, &Fp_hdr);
    }

/* open data files */

    for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
        if (opt.comp >= 0 && (comp != opt.comp)) continue;

        sprintf(fname[chan][comp], "%s-%d", prefix, comp);

        if (Fp_data[chan][comp] != NULL)
        {
          fprintf(Fp_log,"  WARNING: openDataFiles: file pointer ");
          fprintf(Fp_log,"Fp_data[chan][%d] not null\n", comp);
          fclose(Fp_data[chan][comp]);
          Fp_data[chan][comp]=NULL;
        }

        sprintf(name, "%s.data", fname[chan][comp]);
        open_Fwr(name, &Fp_data[chan][comp]);
/*
printf("==== openDataFiles: %s -> %p\n", name, Fp_data[chan][comp]);
*/
    }
}


/*==================================================================*/
static void closeDataFiles(chan)
int chan;
{
char     seedFname[PATHLEN];
int      comp;
struct   data_list *plist;
struct   data_segment *segm;
char     *dftfile = NULL;
double   startime;
char     cp[2];
char     date[40];
char     sensor[8];
FILE     *Fpmseed;


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
        wrtInfo_(chan, comp, startime, Fp_hdr);
    }



/* Save data parameters into linked list */

    for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
      if (opt.comp >= 0 && (comp != opt.comp)) continue;

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


/*
 * For each component fill in data descriptor and call the miniseed writer
 */

    for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
        if (opt.comp >= 0 && (comp != opt.comp)) continue;

/*
 * Find sensor type from auxiliary station file info.
 */
        sensor[0] = '\0';
        if (HasStatDbParms())
        {
           struct StationParm *ps = NULL;

if (0)
{
  printf("  ==== libseed: closeDataFiles: found database params ");
  printf("for %s chan %d comp %d\n", Station, chan, comp);
}

           for (ps=staparms_head; ps!=NULL; ps=ps->next)
              if (startime >= ps->begtime && startime <= ps->endtime) break;
           if (ps == NULL) for (ps=staparms_head; ps!=NULL; ps=ps->next)
              if (ps->begtime == 0 && ps->endtime == 0) break;
           if (ps == NULL)
           {
              fprintf(stderr,
                 "\tERROR closeDataFiles: can't find station parameters\n");
              fprintf(stderr,"\n");
              exit(1);
           }

           if (ps->sismchan[chan][comp].on)
               sprintf(sensor, "%s", ps->sismchan[chan][comp].sensor);
        }
        else
        {
            fprintf(stderr, "\n  ERROR: closeDataFiles: optional ");
            fprintf(stderr,"station parameters file info is required\n");
            fprintf(stderr,"  for seed format. See program help.\n\n");
            exit(1);
        }
        if (!strlen(sensor))
        {
            fprintf(stderr, "\n  ERROR: miniseed: closeDataFiles: ");
            fprintf(stderr,"can't find sensor for channel %d\n", chan);
            fprintf(stderr,
                "  Please check station parameters file %s/%s\n\n",
                db_paths.stations, Station);
            exit(1);
        }



/* Log data segment beg infos into "segm" structure */
/* Segments info will be used to build the time headers */

        segm =
          (struct data_segment *) mem_alloc(sizeof(struct data_segment),"");
        append_linklist_element(segm, dsegm_head, dsegm_tail);
        sprintf(segm->sta,   "%.4s", Station);
/*
 * Figure out SEED channel, from sensor, gain, comp, srate, orient_tol.
 */
        segm->srate         = OutData[chan].srate;
        sprintf(cp, "%1d", comp);
        seed_channel_map(sensor, '\0', cp, segm->srate, 0.0, segm->cha);

        segm->nsamples      = OutData[chan].nsamples;
/*
 * We want the corrected time for the time headers
 */
        segm->begtime = OutData[chan].start_time;
        segm->endtime = segm->begtime +
                        (double) (segm->nsamples-1) / segm->srate;
/*
 * To keep consistency with SAC and other formats file naming
 * the miniseed filename is buid with uncorrected time
 */
        segm->uncorrBegTime = OutData[chan].uncorrected_time;
        segm->filtdelay     = OutData[chan].adcdelay + OutData[chan].filtdelay;
        segm->observ_dt     = OutData[chan].observ_dt;

        time_asc4(date, segm->uncorrBegTime);
        sprintf(seedFname,"%.19s.%s.%s.mseed", date, segm->sta, segm->cha);
        segm->fname = mem_alloc(strlen(seedFname)+2, "");
        sprintf(segm->fname, "%s", seedFname);

if (0)
{
printf("==== close_data: segm: %s %s %s %.3f %.3f\n",
    segm->fname,
    segm->sta, segm->cha,
    segm->begtime, segm->endtime);
printf("==== n=%d %E %.3f del=%.3f\n",
    segm->nsamples, segm->srate,
    segm->uncorrBegTime, segm->filtdelay);
}


/*
 * Open miniseed output file
 */
        if (!(Fpmseed = fopen(segm->fname, "w")))
        {
            fprintf(Fp_log,"ERROR: seed_data: can't open %s\n", segm->fname);
            exit(1);
        }

        rewind(Fp_data[chan][comp]);

/*
 * Miniseed writer
 */
        segm->nrec = seed_data(segm, Fp_data[chan][comp], Fpmseed);


/*
 * Close miniseed output file
 */
        fclose(Fpmseed); Fpmseed = NULL;

/*
 * Close and remove data files. Check for eventual null pointer
 */
        if (0 && comp == 0)
            printf("  closing %.19s for chan %d  %d samples\n",
                   fname[chan][comp], chan, OutData[chan].nsamples);

        if (Fp_data[chan][comp] == NULL)
        {
            fprintf(Fp_log,"  WARNING: closeDataFiles: null data pointer\n");
        }
        else
        {
            char name[PATHLEN];
            sprintf(name, "%s.data", fname[chan][comp]);
            fclose(Fp_data[chan][comp]); Fp_data[chan][comp] = NULL;
            unlink(name);
        }
    }

/*
 * Reset all variables in structure OutData.
 */
    clearOutputParms(chan);
}


/*==================================================================*
    seed_data
    Data timing: we use only the uncorrected time here.
    WE WANT THE ESTIMATED DELTA_T, NOT OBSERVED DELTA_T
 *==================================================================*/
static int seed_data(segm, Fp_datin, Fpmseed)
struct data_segment *segm;  /* data descriptor                   */
FILE     *Fp_datin;         /* file pointer to input data files  */
FILE     *Fpmseed;          /* file pointer to output data files  */
{
float    *fdata = NULL;
int      ndata;
char     seed_hd[DATA_REC_HD_SIZE];  /* data header; 64 bytes */
struct   seed_data_hdr  *fsdh;       /* FSDH */
int      req_samples;
int      nrec;
int      i;

    if (segm->nsamples == 0)
    {
        fprintf(stderr, "ERROR: seed_data: no data to process !\n");
        exit(1);
    }

if (0)
    printf("==== seed_data: %s %s %6d uncorr=%.4f nom_srate=%.3f\n",
            segm->sta, segm->cha,
            segm->nsamples, segm->uncorrBegTime, segm->srate);

    req_samples = segm->nsamples;

    memset(seed_hd, 0, DATA_REC_HD_SIZE);
    sprintf(seed_hd, "      D ");
    fsdh = (struct seed_data_hdr *) &seed_hd[8];

/* Set up FSDH (Fixed Section of Data Header)  */

    sprintf(fsdh->station, "     ");
    strncpy(fsdh->station, segm->sta, strlen(segm->sta));
    strncpy(fsdh->channel, segm->cha, strlen(segm->cha));
    strncpy(fsdh->location, "  ", 2);
    strncpy(fsdh->network, "  ", 2);
    strncpy(fsdh->network, NETWORK, strlen(NETWORK));
    strncpy(fsdh->network, Network, strlen(Network));
    fsdh->nsamples                = 0;
    fsdh->activity_flags          = 0;
    fsdh->io_flags                = 0;
    fsdh->data_quality_flags      = 0;
    fsdh->number_time_corrections = 0;
    fsdh->number_blockettes       = 0;
    fsdh->beg_of_data             = DATA_REC_HD_SIZE;

/* Set sample rate. If we can't encode it, exit */
    if (seed_srate(segm->srate, fsdh) != 0)
    {
        fprintf(stderr,
            "ERROR: seed_data: seed_srate: can't code sample rate %.5f\n",
            segm->srate);
        exit(1);
    }

/* Set time in FSDH */
    dbt_to_tstruct(segm->uncorrBegTime, &fsdh->time);

/*
 * Allocate memory for data buffers and read data.
 */
    fdata = (float *) mem_alloc((req_samples+1) * sizeof(float),"seed_data");
    ndata = fread(fdata, sizeof(float), req_samples, Fp_datin);
    
    if (ndata != req_samples)
    {
        fprintf(stderr, "ERROR: seed_data: read error\n");
        exit(1);
    }


/*
 * RAP network and SEED output file:
 * Convert data series from counts to acceleration units.
 * We need first to get the overall channel sensitivity (V/MS-2)
 * and then compute the conversion factor to G or cm/S2 or microG.
 */

    if (!strcmp(netname,"RAP") && opt.do_seed && SeedDataformat == IEEEFLOAT)
    {
        double sensit = 0.0;
        double mean   = 0.0;
        double multiplier = 0.0;
        double begtime = segm->uncorrBegTime;
        double endtime = segm->uncorrBegTime +
                        (double) (segm->nsamples-1) / segm->srate;


        get_channel_gain(segm->sta, segm->cha, begtime, endtime, &sensit);
        if (sensit == 0.0)
        {
            fprintf(stderr, "ERROR: seed_data: get_channel_gain failed\n");
            exit(1);
        }

    /*
     * Get scale factor to convert counts to disp/S2 units
     * Format of seed_scalefactor[]:   "scale = 0.01 * M/S2"
     * Keywords "scale" and "M/S2" are mandatory.
     * Token number 2 means
     *   0.01     -> cm/S2
     *   9.81     -> G
     *   9.81e-9  -> microG
     */
        if (strlen(seed_scalefactor))
        {
            char line[100];
            char *token[10];
            int  ntok;

            sprintf(line, "%.100s", seed_scalefactor);
            ucase(line);
            ntok = sparse(line, token, " =*\t", 10);

        /* Check mantory kwords */

            if (!strstr(token[0], "SCALE"))
            {
               fprintf(stderr,
                 "ERROR: seed_data: keyword 'scale' not found\n");
               exit(1);
            }
            if (strstr(token[2], "M/S2") == NULL)
            {
               fprintf(stderr,
                 "ERROR: seed_data: unsupported units '%s'\n", token[2]);
               exit(1);

            }

        /* Get multiplier */

            multiplier = atof(token[1]);

            SeedDataScaleFactor = 1.0 / (multiplier * sensit);

        /* Convert data series from counts to acceleration units */

if (0)  fprintf(Fp_log,
            "  seed_data: RAP: %s %s  multiplier=%E scale_factor=%E\n",
            segm->sta, segm->cha, multiplier, SeedDataScaleFactor);

            for (i=0; i<ndata; i++) fdata[i] = fdata[i]*SeedDataScaleFactor;
        }
        else
        {
            SeedDataScaleFactor = 0.0;
        }

/*
 * Compute and remove mean
 */
        for (i=0; i<ndata; i++) mean += fdata[i];
        mean /= (double) ndata;
        for (i=0; i<ndata; i++) fdata[i] -= mean;

if (0)  fprintf(Fp_log,
            "  seed_data: RAP: %s %s  mean=%E over %d samples\n",
            segm->sta, segm->cha, mean, ndata);

    }  /* end if RAP and SeedDataformat == IEEEFLOAT */



/*
 * Now build uncompressed or compressed data records and write to disk.
 */

    if (SeedDataformat == STEIM1)
    {
        nrec = Steim_comp(Fpmseed, fdata, seed_hd, seed_data_record, segm);
    }

    else
    {
        nrec = writeRecords(Fpmseed, fdata, seed_hd, seed_data_record, segm);
    }

    free(fdata);
    fdata = NULL;

    return nrec;
}


/*==================================================================*
    writeRecords
    Write uncompressed seed data records
 *==================================================================*/
static int writeRecords(Fpmseed, fdata, seed_hd, seed_data_record, segm)
FILE     *Fpmseed;
float    *fdata;
char     *seed_hd;
int      *seed_data_record;
struct data_segment *segm;
{
struct  seed_data_hdr *fsdh;
double  nom_sint;
double  dt;
int     nrec, rec;
int     nsamples;
int     samplesPerRecord;
char    tmp[10];
double  dbltime;
char    s2[24];
int     samplesOut, nnn;
float   *pf;
int     *pi;
int     i;
extern double GetEstimDt_(double);
int     foundDftFile;

    fsdh = (struct seed_data_hdr *) &seed_hd[8];

/*
 * Look for a .dft file matching InitialUncorrTime and station name.
 * If .dft file is found, get timeDrift samples (see array of
 * structures 'TimeDrift'.
 * This array will be used by GetEstimDt_(utime)
 * to compute the estimated delta_t.
 * Don't forget to free the array of TimeDrift: see below call
 * freeTimeDrift().
 */

    foundDftFile = loadDftFile(segm->uncorrBegTime, Station);

    nom_sint = 1.0 / segm->srate;
    nsamples = segm->nsamples;
    samplesPerRecord = BYTES_IN_DREC / 4;

if (1)
{
    char s2[24];
    time_asc(&(fsdh->time), s2);
    printf("  writeRecords: %.6s %.4s %.2s %.3s %6d %s (uncorr) it=%.5f\n",
            seed_hd,
            fsdh->station,
            fsdh->network,
            fsdh->channel,
            nsamples,
            s2,
            nom_sint);
}


    if ((nsamples % samplesPerRecord) == 0)
        nrec = (nsamples / samplesPerRecord);
    else
        nrec = (nsamples / samplesPerRecord) + 1;

    samplesOut = 0;
    for (nnn=0,rec=0; rec<nrec; rec++)
    {
        nnn += samplesPerRecord;
        if (nnn > nsamples) fsdh->nsamples = nsamples % samplesPerRecord;
        else                fsdh->nsamples = samplesPerRecord;

/*
 * Calculate data record begtime and time correction and write into header.
 */
        dbltime = segm->uncorrBegTime
                  - segm->filtdelay + samplesOut * nom_sint;

/*
 * Get and check time correction
 */
        dt = GetEstimDt_(dbltime);

        if (dt == (double) UNKNOWN)
        {
            dbltime -= segm->observ_dt;
        }
        else
        {
           fsdh->number_time_corrections = (int) (-dt * 10000.0);
        }
        dbt_to_asc(dbltime, s2);
        asc_to_dbt(s2, &(fsdh->time), &dbltime);

if (0) printf("==== writeRecords: dt=%ld observ_dt=%.4f\n",
               fsdh->number_time_corrections, segm->observ_dt);

        memcpy(seed_data_record, seed_hd, DATA_REC_HD_SIZE);
        sprintf(tmp, "%06d", rec+1);              /* relative number */
        memcpy(seed_data_record, tmp, strlen(tmp));

/*
 * Add extra header blockettes 1000 & 1001, starting at FSDH+48.
 * Encoding format: 4 -> IEEE floating point
 *                  3 -> 32 bits integers
 * Then copy data into data record.
 */

        if (SeedDataformat == IEEEFLOAT)
        {
            blk_1000_1001(((char*) seed_data_record + FSDH_SIZE), 4);
            pf = (float*) &seed_data_record[DATA_REC_HD_SIZE/4];
            for (i=0; i<fsdh->nsamples; i++) pf[i] = fdata[samplesOut+i];
        }
        else if (SeedDataformat == INT_4)
        {
            blk_1000_1001(((char*) seed_data_record + FSDH_SIZE), 3);
            pi = (int*) &seed_data_record[DATA_REC_HD_SIZE/4];
            for (i=0; i<fsdh->nsamples; i++) pi[i] = fdata[samplesOut+i];
        }
        else
        {
            fprintf(stderr,
                "ERROR: Seed writeRecords: unsupported seed data format\n");
            exit(1);
        }

/* Update number of samples */

        samplesOut += fsdh->nsamples;

/* Write data record */

        if (fwrite(seed_data_record, 1, LRECL, Fpmseed) != LRECL)
        {
          fprintf(stderr, "ERROR Seed writeRecords: write failed\n");
          exit(1);
        }

if (0)
{
   /* Compute corrected time and convert to string */
           tstruct_to_dbt(&(fsdh->time), &dbltime);
           dbltime += (double) fsdh->number_time_corrections / 10000.0;
           dbt_to_asc(dbltime, s2);

   printf("  writeRecords: %.6s %.4s %.2s %.3s %6d %s tc=%d tot=%d\n",
           seed_hd,
           fsdh->station,
           fsdh->network,
           fsdh->channel,
           fsdh->nsamples,
           s2,
           (int) fsdh->number_time_corrections,
           samplesOut);
}

/* Set next record FDSH beg time */

        tstruct_to_dbt(&(fsdh->time), &dbltime);
        dbltime += (double) fsdh->nsamples * nom_sint;
        dbt_to_tstruct(dbltime, &fsdh->time);
    }

/*
 * Free memory allocated for timeDrift.
 */
    freeTimeDrift();

    if (0 && foundDftFile == 0)
    {
       fprintf(stderr,"\n");
       fprintf(stderr,
   "  =================================================================\n");
       fprintf(stderr,
   "  WARNING: writeRecords: couldn't find a valid estimated time drift\n");
       fprintf(stderr,"  required for seed format.\n");
       fprintf(stderr,"  Time was corrected by observed delta_t: %.4f\n",
           segm->observ_dt);
       fprintf(stderr,
   "  =================================================================\n");
       fprintf(stderr,"\n");
    }


    return nrec;
}



/*==================================================================*/
static void writeData(chan)
int chan;
{
float  fsamp[NINP];
int    comp;
int    i;

    if (nodata == TRUE) return;
    if (Channel[chan].ninp == 0) return;

    for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
      if (opt.comp >= 0 && (comp != opt.comp)) continue;

      for (i=0; i<Channel[chan].ninp; i++)
          fsamp[i] = (float) inp_data[chan][comp][i];
      fwrite(fsamp, sizeof(float), Channel[chan].ninp, Fp_data[chan][comp]);
    }
}


/*===================================================================*/
static void wrtInfo_(chan, comp, startime, Fp_head)
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
          Channel[chan].srate);

      time_asc4(str, startime);
      if      (tqf == '1') sprintf(cmt,"corrected by estimated dt");
      else if (tqf == '2') sprintf(cmt,"corrected by observed dt");
      else if (tqf == '?') sprintf(cmt,"no dt correction applied");
      fprintf(Fp_head,"%s   start time (%s)\n", str, cmt);
      time_asc4(str, OutData[chan].uncorrected_time);
      fprintf(Fp_head,"%s   uncorrected time\n", str);


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
        fprintf(Fp_head," (%.2f days old, %s)",
            diff/86400.0, (OutData[chan].reboot ? "reboot" : "synchro"));
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
          Channel[chan].adcdelay);
      fprintf(Fp_head,"%+.4f  filter delay\n",
          Channel[chan].filtdelay);
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
/*
      if (Fp_head[chan][comp] != NULL)
      {
        fclose(Fp_head[chan][comp]); Fp_head[chan][comp] = NULL;
        sprintf(name, "%s.info", fname[chan][comp]);
        unlink(name);
      }
*/
      if (Fp_data[chan][comp] != NULL)
      {
        fclose(Fp_data[chan][comp]); Fp_data[chan][comp] = NULL;
        sprintf(name, "%s.data",fname[chan][comp]);
        unlink(name);
      }
    }
}

