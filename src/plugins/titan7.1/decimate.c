/*======================================================================
    decimate.c
*======================================================================*/
#include "titan.h"
#include "proto.h"
#include "sac.h"

/* Prototypes */

#ifdef ANSI_C
static void decimSection(int, int, int);
static void output_sac(struct data_list*, char*, double, int, int, double, double);
#else
static void decimSection();
static void output_sac();
#endif

double Fir128[128] =
{
6.48280351701978e-06,
3.72205896607926e-05,
8.98496417786153e-05,
1.10533719089916e-04,
3.93352193668697e-05,
-7.90333925956026e-05,
-9.36472622904975e-05,
4.93729119979858e-05,
1.56337974043305e-04,
1.85868880752521e-05,
-2.08380423918903e-04,
-1.37438441714037e-04,
2.16648196242478e-04,
2.99514323120658e-04,
-1.45590744592744e-04,
-4.76082776735382e-04,
-3.32698211583437e-05,
6.17654631673971e-04,
3.30160685039974e-04,
-6.58963094341583e-04,
-7.26077108016007e-04,
5.30044129423443e-04,
1.16530232898799e-03,
-1.72359414088347e-04,
-1.55378269433754e-03,
-4.41142726800615e-04,
1.76720689927821e-03,
1.28734958864396e-03,
-1.66800428339703e-03,
-2.27857026986125e-03,
1.13157832437519e-03,
3.25779796129575e-03,
-7.81650580046248e-05,
-4.00706930829052e-03,
-1.49476932424387e-03,
4.27043475390424e-03,
3.48571517500197e-03,
-3.79053844413802e-03,
-5.67397789070195e-03,
2.35508144452788e-03,
7.72105373692439e-03,
1.52847726354260e-04,
-9.19105931085419e-03,
-3.70839742462824e-03,
9.58873497303221e-03,
8.10975936591685e-03,
-8.40979610278303e-03,
-1.29598292544294e-02,
5.19311603227714e-03,
1.76648326165321e-02,
4.40392924832595e-04,
-2.14419006440696e-02,
-8.78636520190397e-03,
2.33098747811209e-02,
2.01395429088721e-02,
-2.19918112223609e-02,
-3.50829765147762e-02,
1.54952347689814e-02,
5.54329331284308e-02,
6.40753526506196e-04,
-8.83568208393635e-02,
-4.38867760749041e-02,
1.82967003246948e-01,
4.09454307143526e-01,
4.09454307143526e-01,
1.82967003246948e-01,
-4.38867760749041e-02,
-8.83568208393635e-02,
6.40753526506196e-04,
5.54329331284308e-02,
1.54952347689814e-02,
-3.50829765147762e-02,
-2.19918112223609e-02,
2.01395429088721e-02,
2.33098747811209e-02,
-8.78636520190397e-03,
-2.14419006440696e-02,
4.40392924832595e-04,
1.76648326165321e-02,
5.19311603227714e-03,
-1.29598292544294e-02,
-8.40979610278303e-03,
8.10975936591685e-03,
9.58873497303221e-03,
-3.70839742462824e-03,
-9.19105931085419e-03,
1.52847726354260e-04,
7.72105373692439e-03,
2.35508144452788e-03,
-5.67397789070195e-03,
-3.79053844413802e-03,
3.48571517500197e-03,
4.27043475390424e-03,
-1.49476932424387e-03,
-4.00706930829052e-03,
-7.81650580046248e-05,
3.25779796129575e-03,
1.13157832437519e-03,
-2.27857026986125e-03,
-1.66800428339703e-03,
1.28734958864396e-03,
1.76720689927821e-03,
-4.41142726800615e-04,
-1.55378269433754e-03,
-1.72359414088347e-04,
1.16530232898799e-03,
5.30044129423443e-04,
-7.26077108016007e-04,
-6.58963094341583e-04,
3.30160685039974e-04,
6.17654631673971e-04,
-3.32698211583437e-05,
-4.76082776735382e-04,
-1.45590744592744e-04,
2.99514323120658e-04,
2.16648196242478e-04,
-1.37438441714037e-04,
-2.08380423918903e-04,
1.85868880752521e-05,
1.56337974043305e-04,
4.93729119979858e-05,
-9.36472622904975e-05,
-7.90333925956026e-05,
3.93352193668697e-05,
1.10533719089916e-04,
8.98496417786153e-05,
3.72205896607926e-05,
6.48280351701978e-06
};

#define BUFLEN 10000

extern struct option opt;
extern outData OutData[NCHAN];
extern FILE   *Fp_data[NCHAN][NCOMP];

static FILE    *Fp_inp, *Fp_out;
static char    ifname[511], ofname[511];
static float   inpbuf[BUFLEN*2];
static double  filtbuf[BUFLEN+1000];
static float   outbuf[BUFLEN];
static double  *fir;
static int     ncoef, npast;
static int     nOutData;
static int     sacHeaderRemoved;
static int debug = 0;

/*===================================================================*/
int postDecimate(data)
struct data_list *data;
{
struct  stat fstat;
int     nInpData, n_remaining;
int     n, sectlen1, sectlen2, nsection1, nsection2;
int     decim, curDecim;
int     decPass, ndecPass;
int     len;
double  startime, srate, filtdelay;
int     chan, comp;
int     minInpData;

  decim = opt.postdecim.decim_fact;
  if (decim <= 0) return 0;

  if      (opt.do_sac);
  else if (opt.do_bindata);
  else return 0;

  sectlen1 = 10000;
  sectlen2 = 128;
  sectlen2 = 32;
  minInpData = sectlen2 * decim / 2;

  Fp_inp = NULL;
  Fp_out = NULL;

  fir = Fir128;
  ncoef = 128;
  npast = ncoef - 1;

/* Compute number of decimation passes */

  ndecPass = 0;
  for (n=1; n<decim; n*=2) ++ndecPass;

if (debug)
{
printf("\n");
printf("==== decim: decimation by %d, %d decimation passes\n",
        decim, ndecPass);
}

  filtdelay = 0.0;
  startime = data->header->start_time;
  srate    = data->header->srate;
  chan     = data->chan;
  comp     = data->comp;

if (debug) printf("==== decim: time %.4f srate %f\n", startime, srate);

  curDecim = 2;
  sacHeaderRemoved = FALSE;


  for (decPass=0; decPass<ndecPass; ++decPass, curDecim*=2)
  {

/* Make input-output filenames */

    if (decPass == 0)
    {
        if (opt.do_sac) sprintf(ifname, "%s.SAC",  data->data_fname);
        else            sprintf(ifname, "%s.data", data->data_fname);
    }
    else
    {
        sprintf(ifname, "%s", ofname);
    }

    len = strlen(data->data_fname);
    strncpy(ofname, data->data_fname, len-4);
    sprintf(&ofname[len-4],
            ".%d-%d.1:%d.data", chan, comp, curDecim);
if (debug) 
{
printf("\n");
printf("======== pass %d decim %d\n", decPass, curDecim);
printf("==== decim: opening input %s\n", ifname);
printf("==== decim: opening output %s\n", ofname);
}

/* Open input-output file */

    if (!(Fp_inp = fopen(ifname, "r")))
    {
        fprintf(stderr,"ERROR: decim: can't open file '%s'\n", ifname);
        exit(1);
    }

    if (!(Fp_out = fopen(ofname, "w")))
    {
        fprintf(stderr,"ERROR: decim: can't open file '%s'\n", ofname);
        exit(1);
    }

    stat(ifname, &fstat);
    if ((len = fstat.st_size) == 0)
        nInpData = 0;
    else if (opt.do_sac)
        nInpData = (len - (int)sizeof(struct sac)) / (int)sizeof(float);
    else
        nInpData = len / (int)sizeof(float);

if (debug) printf("==== decim: input file %s nsamp=%d\n", ifname, nInpData);
    if (nInpData < minInpData)
    {
        printf("  WARNING: pass=%d dec=%d input series %d too short; min is %d\n",
                decPass, curDecim, nInpData, minInpData);
        return 0;
    }

/* Compute number of sections */

    nsection1   = nInpData / sectlen1;
    n_remaining = nInpData % sectlen1;

if (debug) printf("==== decim: section1: ndata=%d nsection1=%d remain=%d\n",
        nInpData, nsection1, n_remaining);

    nOutData = 0;
/*
 * Process sections 1
 */
    decimSection(nsection1, sectlen1, 2);


if (debug) printf("==== end section1 pass %d : decimated data: total %d\n",
                   decPass, nOutData);


/*
 * Process remaining input data by reducing the section length
 */
    nInpData = n_remaining;
    nsection2   = nInpData / sectlen2;
    n_remaining = nInpData % sectlen2;

if (debug) printf("==== decim: section2: ndata=%d nsection2=%d remain=%d\n",
        nInpData, nsection2, n_remaining);

/*
 * Process sections 2
 */
    decimSection(nsection2, sectlen2, 2);

    fclose(Fp_inp); Fp_inp = NULL;
    fclose(Fp_out); Fp_out = NULL;

    if (decPass != 0)
        unlink(ifname);

/*
    filtdelay += (double) ((ncoef/2.0) + 0.5) / srate;
    startime = startime - (double) ((ncoef/2.0) + 0.5) / srate;
*/
    filtdelay += (double) (ncoef-1) / 2.0 / srate;
    startime = startime - (double) (ncoef-1) / 2.0 / srate;
    srate /= (double) 2.0;

if (debug) printf(
 "==== end section2 pass %d : decimated data: total %d time %.4f srate %f\n",
 decPass, nOutData, startime, srate);

  } /* end for ndecPass */


if (debug) printf("==== end decim: %d in file %s srate=%f delay=%f\n",
        nOutData, ofname, srate, filtdelay);


/*
 * SAC output
 */
  if (opt.do_sac && nOutData > 0)
  {
      output_sac(data, ofname, startime, -curDecim/2, nOutData, srate, filtdelay);
  }

  return 0;
}


/*===================================================================*/
static void decimSection(nsection, sectlen, firDecim)
int nsection;
int sectlen;
int firDecim;
{
int n, ndec;
int i, j;
double sum;

    for (n=0; n<nsection; n++)
    {
        if (opt.do_sac && sacHeaderRemoved == FALSE)
        {
            fread(inpbuf, sizeof(struct sac), 1, Fp_inp);
            sacHeaderRemoved = TRUE;
        }
        if (fread(inpbuf, sizeof(float), sectlen, Fp_inp) != sectlen)
        {
          fprintf(stderr, "ERROR: decimSection: error reading '%s'\n",ifname);
          exit(1);
        }

    /*
     * First time: copy the first data of the input series (inpbuf[0])
     * to the "npast" first data of the filter buffer (filtbuf[0,npast]).
     * This will prevent the occurence of a large transcient a the
     * beginning of the decimated data series.
     */
        if (nOutData == 0) for (i=0; i<npast; i++) filtbuf[i] = inpbuf[0];

    /*
     * Load "sectlen" data
     */

        for (i=npast,j=0; j<sectlen; j++,i++) filtbuf[i] = inpbuf[j];

    /*
     * Apply Fir filter; keep 1 sample over firDecim.
     */

        for (ndec = 0, i=0; i<sectlen; ndec++, i+=firDecim)
        {
            sum = 0;
            for (j=0; j<ncoef; j++) sum += fir[j] * filtbuf[i+j];
            outbuf[ndec] = sum;
        }

    /*
     * Shift last input data to beg of filter buffer
     */
        for (i=sectlen, j=0; i<(npast+sectlen); i++,j++)
            filtbuf[j] = filtbuf[i];

    /* write decim data to disk */

        if (fwrite(outbuf, sizeof(float), ndec, Fp_out) != ndec)
        {
            fprintf(stderr, "ERROR: decimSection: error writing '%s'\n", 
                             ofname);
            exit(1);
        }
        nOutData += ndec;

    }  /* end for nsection */
}


/*===================================================================*/
static void output_sac(data, datafname, startime, fact, nsamp, srate, delay)
struct data_list *data;
char   *datafname;
double startime;
int    fact;
int    nsamp;
double srate;
double delay;
{
int    chan, comp;
FILE   *Fp_inp;
FILE   *Fp_head;
struct stat fstat;
char   ofname[511];
int    i, n, len;
float  ibuf[BUFLEN*2];


    chan     = data->chan;
    comp     = data->comp;

/*
 * Copy undecimated input header[0|1] struct into OutData[0|1].
 */
    memcpy(&OutData[chan], data->header, sizeof(outData));
/*
 * Set decimated series params into OutData
 */
    OutData[chan].nsamples = nsamp;
    OutData[chan].srate    = srate;
    OutData[chan].filtdelay = delay;
    OutData[chan].uncorrected_time = (double) UNKNOWN;

/*
 * Open decimated input series.
 * It will be copied later into output SAC file.
 */
    if (!(Fp_inp = fopen(datafname, "r")))
    {
        fprintf(stderr,"ERROR: decim: can't open file '%s'\n", datafname);
        exit(1);
    }

    stat(datafname, &fstat);
    if (nsamp != (int) fstat.st_size / sizeof(float))
    {
       fprintf(stderr,"ERROR: decim: sac: wrong output sample number:\n");
       fprintf(stderr,"       arg: %d  file: %d\n",
               nsamp, (int) fstat.st_size / sizeof(float));
       exit(1);
    }

/*
 * Open output SAC file
 */
    len = strlen(data->data_fname);
    strncpy(ofname, data->data_fname, len-4);
    if (fact < 0)
       sprintf(&ofname[len-4], ".%d-%d.1:%d.SAC", chan, comp, -fact);
    else 
       sprintf(&ofname[len-4], ".%d-%d.i%d.SAC", chan, comp, fact);

    if (!(Fp_data[chan][comp] = fopen(ofname, "w")))
    {
        fprintf(stderr,"ERROR: decim: can't open file '%s'\n", ofname);
        exit(1);
    }

printf("==== decim: SAC: writing %s n=%d\n", ofname, nsamp);

/*
 * Write SAC header
 */
    write_sac_head(chan, comp, startime);

/*
 * Now copy decimated data into SAC data section 
 */
    for (i=0; i<(nsamp/10000); i++)
    {
      if (fread(ibuf, sizeof(float), 10000, Fp_inp) != 10000)
      {
          fprintf(stderr,"ERROR: decim: sac: error reading file '%s'\n",
                  datafname);
          exit(1);
      }
      if (fwrite(ibuf, sizeof(float), 10000, Fp_data[chan][comp]) != 10000)
      {
          fprintf(stderr,"ERROR: decim: sac: error writing file '%s'\n",
                  ofname);
          exit(1);
      }
    }
    n = nsamp%10000;
    if (fread(ibuf, sizeof(float), n, Fp_inp) != n)
    {
        fprintf(stderr,"ERROR: decim: sac: error reading file '%s'\n",
                datafname);
        exit(1);
    }
    if (fwrite(ibuf, sizeof(float), n, Fp_data[chan][comp]) != n)
    {
        fprintf(stderr,"ERROR: decim: sac: error writing file '%s'\n",
                ofname);
        exit(1);
    }

/*
 * Write info file
 */
    if (fact < 0)
       sprintf(&ofname[len-4], ".%d-%d.1:%d.info", chan, comp, -fact);
    else 
       sprintf(&ofname[len-4], ".%d-%d.i%d.info", chan, comp, fact);
    open_Fwr(ofname, &Fp_head);
    wrt_info(chan, comp, startime, Fp_head);

/*
 * Close files and delete decimated .data file
 */
    fclose(Fp_inp); Fp_inp = NULL;
    fclose(Fp_data[chan][comp]); Fp_data[chan][comp] = NULL;
    fclose(Fp_head); Fp_head = NULL;

/* printf("==== sac: deleting %s\n", datafname); */
    unlink(datafname);

}

