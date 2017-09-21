/*====================================================================
         libsism.c            11 Jan 1995

 Updates
    21 Jul 1997. JFF. Byte swapping on Sismalp data performed if the
        program is run on a "3210" machine (typically SUN).
    22 Jul 1997. JFF. Bug fixed for cases where number of input data
        was zero. "Case: discontinuous input data" swapped with
        "Case: continuous input data".
    20 Feb 1998. JFF. opt.do_shift removed. Data are now in gain ranging
        format.
     3 March 1998. JFF. Max number of 512-Byte blocks was 58. Now 400.
 *===================================================================*/
#include "titan.h"
#include "proto.h"


/* Prototypes, private */
#ifdef ANSI_C
static void  openSismalp      (int, char*,char*,FILE**,FILE**,FILE**,int*);
static void  closeSismalp     (FILE**, FILE**, FILE**);
static void  writeSismalp     (int, FILE*, FILE*,  FILE*, int*);
static void  writeData        (FILE *, int, int, int, int);
static void  storeInputData   (int);
static int   getNumtra        (long, FILE *, char *);
static int   getDescLen       (FILE *);
static void  openOutputTrace  (char*,char*,char*,FILE**,FILE**,FILE**,int*);
static int   isNewTrace       (FILE *, struct desc, char *);
static void  make_events_dirs      (Paths, long, char *, char *, char *);
static void  make_evn_trace_name   (Event, char *, char *, char *);
static void  InitCodedSignal  (int, int);
static short CodedSignal      (int);
static void  getDataParm      (int*, int);
#else
static void  openSismalp      ();
static void  closeSismalp     ();
static void  writeSismalp     ();
static void  writeData        ();
static void  storeInputData   ();
static int   getNumtra        ();
static int   getDescLen       ();
static void  openOutputTrace  ();
static int   isNewTrace       ();
static void  make_events_dirs      ();
static void  make_evn_trace_name   ();
static void  InitCodedSignal  ();
static short CodedSignal      ();
static void  getDataParm      ();
#endif



int           *out_data[NCHAN][NCOMP];    /* Output data array */

extern TITFILE *Fp_tit;
extern int     inp_data[][NCOMP][NINP];   /*  Input data array */
extern outData OutData[NCHAN];
extern struct  Channel Channel[NCHAN];
extern struct  option opt;
extern Event   evn;
extern struct  data_list *list_head;
extern struct  data_list *list_tail;
extern Paths   db_paths;
extern FILE    *Fp_log;
extern int     nduplic, nnew;
extern int     totOutSamples;
extern int     byteswap;
extern char    Station[8];
extern int     nodata;
extern double  SystTime;
extern double  Observ_dt;
extern double  ExtraTcorr;
extern char    tqf;                        /* Time quality factor     */



static short          CodedNrBits, CodedNrGainBits;
static unsigned short SignalKMax;
static short          SignBit, SignificantBits;
static short          NrBits, NrGainBits;
static int            SignalMax;
static int            AdcMax;
static int            Smax;
static int            Smin;
static int            Sofs;

static FILE  *Fpndx;     /* SISMALP file pointers          */
static FILE  *Fpsis;
static FILE  *Fphdr;
static char  ndxFname[255];   /* SISMALP desc file names .ndx   */
static char  sisFname[255];   /* SISMALP data file names .sis   */



/*==================================================================*/
void output_sismalp(last, time_jump)
int last;
int time_jump;
{
static int  blkofs;
char        str[40];
int         chan;

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

/*========== Beginning of process or re-initialization ==============*/

    if (Channel[chan].new == TRUE && Channel[chan].ninp > 0)
    {
        closeSismalp(&Fpndx, &Fpsis, &Fphdr);
        openSismalp(chan, ndxFname, sisFname, &Fpndx, &Fpsis, &Fphdr, &blkofs);
        saveOutputParms(chan);
        Channel[chan].new = FALSE;
    }



/*================== Case: discontinuous input data =================*/

    if (time_jump == TRUE || (Channel[chan].nout >= (BLKLEN * MAXBLK)))
    {
        OutData[chan].nsamples = Channel[chan].nout;
        totOutSamples += OutData[chan].nsamples;

        time_asc4(str, OutData[chan].uncorrected_time);
        fprintf(Fp_log,"  %s: chan %d %5d samples %s tot %d ofs=%d\n",
            Station, chan, Channel[chan].nout, str, totOutSamples, ttell(Fp_tit));

        writeSismalp(chan, Fpndx, Fpsis, Fphdr, &blkofs);

        saveOutputParms(chan);

        Channel[chan].nout = 0;
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

        time_asc4(str, OutData[chan].uncorrected_time);
        fprintf(Fp_log,"  %s: chan %d %5d samples %s tot %d ofs=%d\n",
            Station, chan, Channel[chan].nout, str, totOutSamples, ttell(Fp_tit));

        writeSismalp(chan, Fpndx, Fpsis, Fphdr, &blkofs);
    }
  }

  if (last == TRUE)
  {
      closeSismalp(&Fpndx, &Fpsis, &Fphdr);
  }

  return;
}


/*==================================================================*/
static void openSismalp(chan, ndx, sis, Fp_ndx, Fp_sis, Fp_hdr, blkofs)
int  chan;
char *ndx;
char *sis;
FILE **Fp_ndx;
FILE **Fp_sis;
FILE **Fp_hdr;
int  *blkofs;
{
char     str[40];
char     yymmdd[10], hhmn[6];
char     name[10];
char     dirname[255];
double   begtime;
char     hdr[255];

    if (nodata == TRUE) return;
    if (*Fp_ndx != NULL)
    {
        return;
    }
    if (*Fp_sis != NULL)
    {
      fprintf(Fp_log,"  WARNING: openSismalp file pointer ");
      fprintf(Fp_log,"Fp_sis not null\n");
    }
    if (*Fp_hdr != NULL)
    {
      fprintf(Fp_log,"  WARNING: openSismalp file pointer ");
      fprintf(Fp_log,"Fp_hdr not null\n");
    }

    if (evn.evn_time != NULL && opt.use_database == TRUE)
    {
        if (evn.Fp_sism_evntbl == NULL)
        {
            fcreat_append(db_paths.events_tbl, &evn.Fp_sism_evntbl);
        }

/* corrected time to CHECK */
        begtime = Channel[chan].start_time;
        time_asc4(str, begtime);
        make_events_dirs(db_paths, (long) begtime, dirname, yymmdd, hhmn);
        make_evn_trace_name(evn, yymmdd, hhmn, name);
        sprintf(ndx, "%s/%s.ndx", dirname, name);
        sprintf(sis, "%s/%s.sis", dirname, name);
    }
    else
    {
        sprintf(ndx, "%s.ndx", Station);
        sprintf(sis, "%s.sis", Station);
        sprintf(hdr, "%s.info", Station);
    }
    openOutputTrace(ndx, sis, hdr, Fp_ndx, Fp_sis, Fp_hdr, blkofs);

if (0)
  fprintf(Fp_log," +++++ openSismalp: %p %p %p\n",*Fp_ndx,*Fp_sis,*Fp_hdr);

/*
    init_coded_signal(12,4);
*/
    InitCodedSignal(12,4);

}


/*==================================================================*/
static void closeSismalp(Fpndx, Fpsis, Fphdr)
FILE **Fpndx, **Fpsis, **Fphdr;
{

if (0)
  fprintf(Fp_log," +++++ closeSismalp: %p %p %p\n",*Fpndx,*Fpsis,*Fphdr);

      if (*Fpndx != NULL)
          { fclose(*Fpndx); *Fpndx = NULL; }

      if (*Fpsis != NULL)
          { fclose(*Fpsis); *Fpsis = NULL; }

      if (*Fphdr != NULL)
          { fclose(*Fphdr); *Fphdr = NULL; }
}


/*==================================================================*/
static void writeSismalp(chan, Fpndx, Fpsis, Fphdr, blkofs)
int     chan;
FILE   *Fpndx;
FILE   *Fpsis;
FILE   *Fphdr;
int    *blkofs;
{
char        *dftfile = NULL;
struct data_list *plist;
double      startime;
int         comp, ncomp;
int         npadd;
struct desc dsc;
char        *desc1;
char        *desc2;
char        syst_id[20];
int         nblk_out;
float       freq;
int         n_out;

    if (nodata == TRUE) return;

    if (OutData[chan].nsamples <= 64) return;

    startime = GetOutputDataTime(opt.tcorr_mode, Station, chan, &dftfile);

    if (dftfile) sprintf(OutData[chan].dftfile, "%s", dftfile);
    OutData[chan].start_time = startime;

/* Write info file */

    if (opt.noinfo == FALSE) for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
        if (opt.comp >= 0 && (comp != opt.comp)) continue;
        wrt_info(chan, comp, startime, Fphdr);
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
        plist->data_fname = mem_alloc(strlen(sisFname)+2, "closeDataFiles");
        sprintf(plist->data_fname, "%s", sisFname);
    }


    ncomp    = OutData[chan].numcomp;
    n_out    = OutData[chan].nsamples;
    freq     = OutData[chan].srate;
    npadd    = (n_out%BLKLEN) ? (BLKLEN - (n_out%BLKLEN)) : 0;
    nblk_out = (n_out+npadd) / BLKLEN;


    for (comp=0; comp<ncomp; comp++)
    {
        if (opt.comp >= 0 && (comp != opt.comp)) continue;

/* SISMALP descriptors */

        sprintf(syst_id, "TIT%02d%1d", chan,comp);
        desc1 = sismalp_desc1(n_out, nblk_out,*blkofs, freq, startime, &dsc);
        desc2 = sismalp_desc2(syst_id, comp, ncomp,
                              out_data[chan][comp],n_out, 0);

/* If input trace has not been previously extracted, write it out */

        if (isNewTrace(Fpndx, dsc, desc2) == TRUE)
        {
            ++nnew;
            if (opt.verb > 0) fprintf(Fp_log, "%s\n", desc1);

            fprintf(Fpndx, "%s", desc1); CRLF(Fpndx);
            fprintf(Fpndx, "%s", desc2); CRLF(Fpndx);

            writeData(Fpsis, chan, comp, n_out, npadd);

            *blkofs += nblk_out;
        }
        else
        {
            ++nduplic;
        }
    }

    clearOutputParms(chan);
}


/*===================================================================*/
static void writeData(Fpsis, chan, comp, nout, npadd)
FILE   *Fpsis;
int     chan;
int     comp;
int     nout;
int     npadd;
{
int       i, j, jj;
int       NN;
short     block[BLKLEN+8];
double    mean;

/*
 * Compute and remove mean value
 */
    mean = 0;
    for (i=0; i<nout; i++) mean += out_data[chan][comp][i];
    mean /= (double) nout;
    for (i=0; i<nout; i++) out_data[chan][comp][i] -= (int) mean;

if (0) printf("writeData: out_data[%d][%d] %d to %d mean=%.1f\n",
       chan, comp, 0, nout, mean);

    j = 0;
    NN = nout / BLKLEN;
    for (i=0; i<NN; i++)
    {
      for (jj=0; jj<BLKLEN; jj++)
      {
         if (opt.gain_range == FALSE) block[jj] = out_data[chan][comp][j+jj];
         else           block[jj] = CodedSignal(out_data[chan][comp][j+jj]);
      }
      if (byteswap) swap_2byte_array(block, BLKLEN);
      fwrite(block, sizeof(short), BLKLEN, Fpsis);
      j += BLKLEN;
    }
    NN = nout % BLKLEN;
    for (jj=0; jj<NN; jj++)
    {
      if (opt.gain_range == FALSE) block[jj] = out_data[chan][comp][j+jj];
      else            block[jj] = CodedSignal(out_data[chan][comp][j+jj]);
    }
    if (byteswap) swap_2byte_array(block, NN);
    fwrite(block, sizeof(short), NN, Fpsis);
    j += NN;

    if (npadd)
    {
        memset(block, 0, BLKLEN*sizeof(short));
        fwrite(block, sizeof(short), npadd, Fpsis);
    }
}


/*==================================================================*
    storeInputData()
    Add input data into output data array, converting
    to short and ajusting amplitude by shifting for SISMALP output.  
 *==================================================================*/
static void storeInputData(chan)
int        chan;
{
int comp, j;
int n_inp, n_out;

    n_inp = Channel[chan].ninp;
    n_out = Channel[chan].nout;
    if ((n_out + n_inp) > (int) (SISMALP_MAXOUT+1000))
    {
      fprintf(Fp_log,"  ERROR: storeInputData: too many samples; ");
      fprintf(Fp_log,"max allowed is %d\n",(int)(SISMALP_MAXOUT+1000));
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
                 (int *)m_alloc((SISMALP_MAXOUT+1000)*sizeof(int))))
          {
              fprintf(stderr, "ERROR m_alloc failed for out_data[]\n");
              exit(1);
          }
        }
    }

    for (comp=0; comp<NCOMP; comp++)
    {
        if (opt.comp >= 0 && (comp != opt.comp)) continue;

if (0)
 printf("storeInputData: copy %d data into out_data[%d][%d] from %d to %d\n",
    n_inp, chan, comp, n_out, n_out+n_inp);

        for (j=0; j<n_inp; j++)
        {
            out_data[chan][comp][n_out+j] = inp_data[chan][comp][j];
        }
    }
    return;
}



/*===================================================================*/
void rd_desc(s, d)
char *s;
struct desc *d;
{
char tmp[10];

    sscanf(s, "%8s", d->sta);
    sprintf(tmp, "%6.6s", &s[NDTOFF]); d->ndt = atoi(tmp);
    sprintf(tmp, "%6.6s", &s[BLNOFF]); d->bln = atoi(tmp);
    sprintf(tmp, "%6.6s", &s[NBLOFF]); d->nbl = atoi(tmp);
    sprintf(tmp, "%10.10s", &s[FRQOFF]); d->frq = atof(tmp);
    sscanf(&s[DATOFF], "%10s %12s",d->date,d->time);
}


/*===================================================================*/
static int getNumtra(filen, Fp, fname)
long filen;
FILE *Fp;
char *fname;
{
int desc_len;
char line1[DSC1LEN], line2[DSC2LEN];
char temp[10];
float rev = 0.0;

    if (!fgets(line1, DSC1LEN, Fp)) return 0;
    if (!fgets(line2, DSC2LEN, Fp)) return 0;
    if (strlen(line2) > 23)
    {
        sprintf(temp, "%5.5s", &line2[19]);
        if (isdigit(temp[0]) && isdigit(temp[1]))
            rev = atof(temp);
    }
    desc_len = ftell(Fp);
    rewind(Fp);
    if (rev >= 15.0)
    {
        if (desc_len != 125 || (filen%125) != 0)
        {
            fprintf(stderr, "ERROR: getNumtra() can't find descriptor ");
            fprintf(stderr, "lenght for %s\n", fname);
            fprintf(stderr," dsclen=%d filen=%ld\n", desc_len,filen);
            exit(1);
        } 
        return (int) (filen / desc_len);
    }

    if      ((filen%(77*79))   == 0)  desc_len = getDescLen(Fp);
    else if ((filen%(123*125)) == 0)  desc_len = getDescLen(Fp);
    else if ((filen%77)        == 0)  desc_len = 77;
    else if ((filen%79)        == 0)  desc_len = 79;
    else desc_len = getDescLen(Fp);

    if (desc_len == 0)
    {
        fprintf(stderr, "ERROR: getNumtra() can't find descriptor ");
        fprintf(stderr, "lenght for %s\n", fname);
        exit(1);
    } 
    return (int) (filen / desc_len);
}

/*===================================================================*/
static int getDescLen(Fp)
FILE *Fp;
{
char line1[DSC1LEN], line2[DSC2LEN];
long pos;
char temp[10];
float rev;

    if (!fgets(line1, DSC1LEN, Fp)) return 0;
    if (!fgets(line2, DSC2LEN, Fp)) return 0;
    sprintf(temp, "%5.5s", &line2[19]);
    rev = atof(temp);
    pos = ftell(Fp);
/*
printf("========== rev %s %.2f pos %d\n", temp, rev, pos);
*/
    rewind(Fp);
    if (pos ==  77 || pos ==  79) return pos;
    if (pos == 123 || pos == 125) return pos;
    else                          return 0;
}

/*===================================================================*/
static void openOutputTrace(ndx, sis, hdr, Fpndx, Fpsis, Fphdr, blkofs)
char   *ndx;
char   *sis;
char   *hdr;
FILE  **Fpndx;
FILE  **Fpsis;
FILE  **Fphdr;
int    *blkofs;
{
struct desc dsc;
char        desc1[DSC1LEN+1];
char        desc2[DSC2LEN+1];
long   filen;
int    i, ntraces;

    filen = fcreat_append(ndx, Fpndx);
/* Do not seek to end for index file */

    fcreat_append(sis, Fpsis); fseek(*Fpsis, 0L, SEEK_END);
    if (opt.noinfo == FALSE)
    {
        fcreat_append(hdr, Fphdr); fseek(*Fphdr, 0L, SEEK_END);
    }

    if (filen)
    {
        ntraces = getNumtra(filen, *Fpndx, ndx);

/* Get last descriptor of file to which we append */
        for (i=0; ;i++)
        {
            if (!fgets(desc1, DSC1LEN, *Fpndx)) break;
            if (!fgets(desc2, DSC2LEN, *Fpndx)) break;
        }

        if (ntraces != i)
        {
            fprintf(stderr,"ERROR: openOutputTrace: %s ", ndx);
            fprintf(stderr,"ntraces=%d i=%d\n", ntraces, i);
            exit(1);
        }

/* Calculate next starting block from last block number of */
/* file to which we append */
        rd_desc(desc1, &dsc);

        *blkofs = (long) dsc.bln + (long) dsc.nbl;
    }
    else
    {
        *blkofs = 1;
    }
}


/*==================================================================*
    compare channel: desc2[1,2] and component: desc2[3]
 *==================================================================*/
static int isNewTrace(Fpndx, dsc1, desc12)
FILE        *Fpndx;
struct desc  dsc1;
char        *desc12;
{
struct desc dsc2;
char        desc21[DSC1LEN];
char        desc22[DSC2LEN];
int         i;

/* Check that input trace has not been previously extracted */

    rewind(Fpndx);
    for (i=0; ;i++)
    {
        if (!fgets(desc21, DSC1LEN, Fpndx)) break;
        if (!fgets(desc22, DSC2LEN, Fpndx)) break;
        memset(&dsc2, 0, sizeof(struct desc));
        rd_desc(desc21, &dsc2);
        if (!strncmp(dsc1.sta,  dsc2.sta,   4) &&
            !strncmp(dsc1.time, dsc2.time, 12) &&
            !strncmp(dsc1.date, dsc2.date, 10) &&
            !strncmp(&desc12[10], &desc22[10], 1))
        {
            fseek(Fpndx, 0L, SEEK_END);
            return FALSE;
        }
    }
    fseek(Fpndx, 0L, SEEK_END);
    return TRUE;
}

/*==================================================================*/
char *sismalp_desc1(ndt, nbl, bln, f, startime, dsc)
int ndt;
int nbl;
int bln;
float f;
double startime;
struct desc *dsc;
{
static char desc1[DSC1LEN];
char        datasc[20], timasc[20];

    if (strlen(Station) < 1 || strlen(Station) > 7)
    {
      fprintf(stderr,"sismalp_desc1: wrong station name '%s'\n",Station);
      exit(1);
    }
    memset(dsc, 0, sizeof(struct desc));
    strncpy(dsc->sta, Station, strlen(Station));
    dsc->ndt = ndt;
    dsc->nbl = nbl;
    dsc->bln = bln;
    dsc->frq = f;
    time_asc0(datasc, timasc, startime);
    strcpy(dsc->date, datasc);
    strcpy(dsc->time, timasc);
    sprintf(desc1,"        ");
    strncpy(&desc1[4], dsc->sta, strlen(dsc->sta));
    sprintf(&desc1[8],"%*d%*d%*d%*.4f  %*s %*s",
         NDT_NC, ndt,
         BLN_NC, bln,
         NBL_NC, nbl,
         FRQ_NC, f,
         DAT_NC, datasc,
         TIM_NC, timasc);
    return (desc1);
}


/*==================================================================*/
/* Set descriptor line 2 params */
/*
                    rev    FS     max   min   ofs   pevn ch ga
------- - - -- - - ----- ------ ----- ----- ----- ------==----
SISMALP 3 Z 10 0 . 15.02   -512   -43    38     2   1024 1   3
TIT000? 3 Z 10 0   15.02 000000 00000 00000 00000 000000 0 000
TIT0002 1 Z 12 4   15.00 -00000 -1111  1111    11
TIT0002 1 Z 12 4   15.00 -00000 -1111  1111    11
TIT0002 1 Z 12 4   15.00        20479 21504  2076
TIT0002 1 Z 16 0   15.00        32767-32768  3188
TIT0002 1 Z 12 4   15.22  -9216 20479 21504  2049
*/
/*==================================================================*/
char *sismalp_desc2(syst_id, comp, ncomp, data, ndt, prevn)
char  *syst_id;
int    comp;
int    ncomp;
int   *data;
int    ndt;
int    prevn;
{
static char desc2[DSC2LEN] =
"TIT000? 3 Z 16 0   15.00 000000 00000 00000 00000 000000 0 000";
static char *CP = "ZNE";

        getDataParm(data, ndt);

        sprintf(desc2, "%6.6s%c %d %c %2d %1d",
            syst_id,tqf,ncomp,CP[comp],CodedNrBits,CodedNrGainBits);
        sprintf(&desc2[16], "   %5.5s", SISMALP_REV); 
        sprintf(&desc2[24], " ");
        sprintf(&desc2[25], "      ");
        if (AdcMax) sprintf(&desc2[25], "%6d", CodedSignal(AdcMax));
        else        sprintf(&desc2[25], "      ");
        sprintf(&desc2[31], "%6d", CodedSignal(Smax));
        sprintf(&desc2[37], "%6d", CodedSignal(Smin));
        sprintf(&desc2[43], "%6d", CodedSignal(Sofs));

/* pre-event, number of samples */
        if (prevn > 0) sprintf(&desc2[49], " %6d", prevn);
        else           sprintf(&desc2[49], "       ");
/* channel num */
        sprintf(&desc2[56], "  ");
/* gain (analog) */
        sprintf(&desc2[58], "   0");
    return (desc2);
}


/*===================================================================*/
void make_events_dirs(paths, evntime, dirname, yymmdd, hhmn)
Paths paths;
long evntime;
char *dirname;
char *yymmdd;
char *hhmn;
{
char    str[40];
char    yymm[5], dd[3];

    time_asc3(str, (double) evntime);
    sprintf(yymmdd, "%.6s", &str[0]);
    sprintf(hhmn,   "%.5s", &str[7]);
    sprintf(yymm,   "%.4s", &str[0]);
    sprintf(dd,     "%.2s", &str[4]);
    if (!isdir(paths.events))
    {
      fprintf(stderr,"ERROR: make_events_dirs: directory \"%s\" missing\n",
              paths.events);
      exit(1);
    }
    sprintf(dirname, "%s/%s", paths.events, yymm);
    if (!isdir(dirname))
    {
        printf("=== make_events_dirs: creating %s\n", dirname);
        if (mkdir(dirname, 0755) < 0)
        {
            fprintf(stderr,"ERROR: make_events_dirs: can't create dir %s\n",
                    dirname);
            exit(1);
        }
    }
    sprintf(dirname, "%s/%s/%s", paths.events, yymm, dd);
    if (!isdir(dirname))
    {
        printf("=== make_events_dirs: creating %s\n", dirname);
        if (mkdir(dirname, 0755) < 0)
        {
            fprintf(stderr,"ERROR: make_events_dirs: can't create dir %s\n",
                dirname);
            exit(1);
        }
    }
}


/*===================================================================*
    Sismalp event names:  YYMMDDnn.ndx, YYMMDDnn.sis
    Main event table: NETBASE/events/events.tbl
            format: YYMMDDnn hh:mm
    The event table file pointer is passed in 'evn.Fp_sism_evntbl'  

    The monthly event tables needed by Sismalp:
            NETBASE/events/YYMM/events.tbl
    are created from the main table above by calling the
    fonction sism_month_events_tbl().
 *===================================================================*/
void make_evn_trace_name(evn, yymmdd, hhmn, name)
Event evn;
char *yymmdd;
char *hhmn;
char *name;
{
int     found_day, found_time;
int     sism_event_num = 0;
char    line[255];
char    num_asc[4];

    found_day = FALSE;
    found_time = FALSE;
    rewind(evn.Fp_sism_evntbl);
    while (getline(evn.Fp_sism_evntbl, line))
    {
        if (line[0] == '=') continue;
        trim(line);
        if (!strncmp(yymmdd, line, 6))
        {
            found_day = TRUE;
            sprintf(num_asc, "%.2s", &line[6]);
            if (!strncmp(hhmn, &line[9], 5))
            {
                found_time = TRUE;
                break;
            }
        }
    }

    if (found_day == TRUE && found_time == TRUE)
    {
/* Name already exist */
        sprintf(name, "%.8s", line);
    }
    else
    {
/* Make new name */
        if      (found_day  == FALSE) sism_event_num = 1;
        else if (found_time == FALSE) sism_event_num = atoi(num_asc)+1;   
        sprintf(name, "%.6s%02d", yymmdd,sism_event_num);

/* Log new name and time in ouput event list */
        fseek(evn.Fp_sism_evntbl, 0L, SEEK_END);
        fprintf(evn.Fp_sism_evntbl, "%s %.5s", name, hhmn);
        CRLF(evn.Fp_sism_evntbl);
    }
}


/*===================================================================*
    sism_month_events_tbl:
    Create Sismalp events table in events/YYMM directories.
    The events/YYMM/events.tbl files are needed by PICKEV.
    Format:  95012203 13:54
 *===================================================================*/
void sism_month_events_tbl(paths)
Paths paths;
{
FILE *Fpndx;
FILE *Fp;
char dir_yymm[255];
char path[255];
char fname[255];
char ndx[255];
int  yy, mm, dd;
static struct desc dsc;
static char        desc1[DSC1LEN+1];

    for (yy=90; yy<100; yy++)
    {
      for (mm=1; mm<13; mm++)
      {
        sprintf(dir_yymm, "%s/%02d%02d", paths.events, yy, mm);
        if (isdir(dir_yymm))
        {
/* Create events/YYMM/events.tbl file */
          sprintf(fname, "%s/events.tbl", dir_yymm);
          open_Fwr(fname, &Fp);
          for (dd=1; dd<32; dd++)
          {
            sprintf(path, "%s/%02d", dir_yymm, dd);
            if (isdir(path)) while (read_dir(path, fname, ".ndx"))
            {
              sprintf(ndx, "%s/%s", path, fname);
              open_Frd(ndx, &Fpndx);

/* Get the time of the first trace */

              if (fgets(desc1, DSC1LEN, Fpndx))
              {
                  rd_desc(desc1, &dsc);

/* Write events/YYMM/events.tbl file */

                  fprintf(Fp, "%.8s %.5s", fname, dsc.time);
                  CRLF(Fp);
              }
              fclose(Fpndx);
            }
          }
          fclose(Fp);
        }
      }
    }
}









/*==================================================================*/
static void getDataParm(data, nout)
int *data, nout;
{
int i, sum;

    Smin = INT_MAX;
    Smax = INT_MIN;
    sum = 0;
    for (i=0; i<nout; i++)
    {
        if (data[i] < Smin) Smin = data[i];
        if (data[i] > Smax) Smax = data[i];
        sum += data[i];
    }
    Sofs = sum / nout;
    if (Smin < SHRT_MIN) Smin = SHRT_MIN;
    if (Smin > SHRT_MAX) Smin = SHRT_MAX;
    if (Smax < SHRT_MIN) Smax = SHRT_MIN;
    if (Smax > SHRT_MAX) Smax = SHRT_MAX;
    if (Sofs < SHRT_MIN) Sofs = SHRT_MIN;
    if (Sofs > SHRT_MAX) Sofs = SHRT_MAX;
/*
printf("n=%d min=%d max=%d ofs=%d\n", nout,Smin,Smax,Sofs);
*/
}


/*==================================================================*
 Codage d'un entier N bit en format gain variable
 sur 2 octets. Code ecrit par Julien Frechet
 *==================================================================*/

/*===================================================================*
    SignalLMax is the number of bit of the larger integer which can be
    converted in gain_ranging format, with regard of NB, the number of
    bit of the mantisse.
    SignalMax is the absolute maximum value of the larger integer to
    be converted, corresponding to SignalLMax. Then the condition 
    -SignalMax < Iampl < SignalMax must be satisfied.
 *===================================================================*/
static void InitCodedSignal(NB, NGB)
int NB, NGB;
{
unsigned short SignalLMax;

  CodedNrBits     = NB;
  CodedNrGainBits = NGB;

  if (CodedNrGainBits <= 0) return;
  SignalLMax = CodedNrBits + (1 << CodedNrGainBits) - 1;
  if (SignalLMax > 32) SignalLMax = 32;
  SignalKMax = SignalLMax - CodedNrBits + 1;
  SignalMax = 1 << (SignalLMax-1);
/*
printf("SignalLMax=%d SignalKMax=%d\n", SignalLMax, SignalKMax);
printf("SignalMax=%d\n", SignalMax);
*/
}

/*===================================================================*/
static short CodedSignal(IAmpl)
int IAmpl;
{
unsigned short gain;
long I;

    if (CodedNrGainBits <= 0) return IAmpl;
    I = ((unsigned long)abs(IAmpl)) >> (CodedNrBits - 1);
    gain = 0;
    while (((unsigned long)I) >> gain > 0 && gain < SignalKMax) gain++;
    return ((((unsigned long)(IAmpl + (1 << (CodedNrBits + gain - 1)))) >>
	     gain) + (gain << CodedNrBits));
}

/*===================================================================*/
int TrueSignal(si)
int si;
{
int sl;

    if (NrGainBits <= 0) return si;
    sl = (unsigned short)si;
    return (((sl & SignificantBits) - SignBit) << (((unsigned long)sl) >> NrBits));
}


/*===================================================================*/
void InitTrueSignal(NB, NGB)
int NB, NGB;
{
    NrBits = NB;
    NrGainBits = NGB;
    if (NrGainBits <= 0) return;
    SignBit = 1 << (NrBits - 1);
    SignificantBits = SignBit * 2 - 1;
}

/*
 * codage d'un entier N bit en format reel flottant
 * sur 2 octets. Code ecrit par Julien Frechet
 */

/*
void         init_coded_signal  (int, int);
short        coded_signal       (int);

void init_coded_signal(NB,NGB)
int NB,NGB;
{
int SignalLMax;
CodedNrBits=NB;
CodedNrGainBits=NGB;
  if (CodedNrGainBits>0)
  {
    SignalLMax=CodedNrBits+1<<CodedNrGainBits-1;
    if (SignalLMax>32) SignalLMax=32;
    SignalKMax=SignalLMax-CodedNrBits+1;
  }
}

short coded_signal(IAmpl)
int IAmpl;
{
int gain;
int I;
int j;
I=abs(IAmpl)>>(CodedNrBits-1);
gain=0;
while ((I>>gain>0) && (gain<SignalKMax)) gain++;
return (IAmpl+(1<<(CodedNrBits+gain-1)))>>gain+(gain<<CodedNrBits);
}
*/

/*
BAD
return (IAmpl+(1<<(CodedNrBits+gain-1)))>>gain+(gain<<CodedNrBits);
*/

/*
OK
return ((IAmpl+(1<<(CodedNrBits+gain-1)))>>gain)+(gain<<CodedNrBits);
*/

