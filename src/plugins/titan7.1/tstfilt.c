/*====================================================================
    tstfilt.c

 *===================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/types.h>
#include "sac.h"
#include "coef.c"


struct seed_time                   /* time from input stream */
{
    unsigned short int  year;
    unsigned short int  day;
    char                hour;
    char                minute;
    char                second;
    char                unused;
    unsigned short int  fracsec;
};

/*
#ifdef ANSI_C
#else
static int firFilter();
#endif
*/

static void resampleSection(int, int, int, int, int);
static void decimSection(int, int, int);
static int  firFilter(double*, int, double*, float*, int, int);
static void newSacHeader(double, int, double);
static void asc_to_dbt(char*, struct seed_time*, double *);
static void tstruct_to_dbt(struct seed_time*, double*);
static int  sparse(char*, char**, char*, int);


#define BUFLEN 10000

static FILE    *Fp_inp, *Fp_out;
static char    ifname[511], ofname[511];
static float   *inpbuf;
static float   *outbuf;
static double  *filtbuf;
static float   *firbuf;
static double  *fir;
static int     ncoef;
static int     nOutData;
static struct  sac sacH;


/*===================================================================*/
int main(argc, argv)
int     argc;
char    *argv[];
{
int     nInpData, n_remaining;
int     sectlen1, sectlen2, nsection1, nsection2;
int     ipfact, dcfact, firDecim;
double  startime, srate, filtdelay;
int     len;
char    str[256];
struct  seed_time stime;
char    ratio[256];
double  fratio;
char    *token[3];
int     ntok;


    fir  = Fir119;
    ncoef = 119;
    fir  = Fir128;
    ncoef = 128;
    sacH = sac_nullswap;
    sacH = sac_null;
    Fp_inp = NULL;
    Fp_out = NULL;
    ipfact = 16;
    dcfact = 1;
    firDecim  = 1;
    fratio = ipfact / dcfact;
    sectlen1 = 10000;
    sectlen2 = 100;
    srate    = 1;
    filtdelay = 0.0;
    nOutData = 0;


    if (argc < 2)
    {
        printf("Usage: tstfilt sacfile [ratio]  (ex: 16:5)\n");
        exit(1);
    }

    sprintf(ifname, "%s", argv[1]);

/*
 * Get and check resampling ratio
 */
    if (argc > 2)
    {
        sprintf(ratio, "%s", argv[2]);
        sprintf(str, "%s", argv[2]);
        ntok = sparse(str, token, ":", 2);
        if (ntok != 2)
        {
          fprintf(stderr,"ERROR: interpol ratio syntax\n");
          exit(1);
        }
        ipfact = atoi(token[0]);
        dcfact = atoi(token[1]);
    }

    if (ipfact < 1 || ipfact > 100)
    {
      fprintf(stderr,"ERROR: unsupported oversampling value\n");
      exit(1);
    }
    if (dcfact < 1 || dcfact > 100)
    {
      fprintf(stderr,"ERROR: unsupported decimation value\n");
      exit(1);
    }
    fratio = (double) ipfact / (double) dcfact;
    if (fratio <= 1.0)
    {
      fprintf(stderr,"ERROR: interpol ratio smaller than 1 not supported\n");
      ipfact *= 2;
      fratio *= 2;
      firDecim = 2;
      
      if (fratio <= 1.0)
          exit(1);
    }
    sectlen1 = (sectlen1 / dcfact) * dcfact;
    sectlen2 = (sectlen2 / dcfact) * dcfact;

/*
 * Allocate memory
 */
    inpbuf  = (float *)  calloc(BUFLEN+1000, sizeof(float));
    outbuf  = (float *)  calloc(BUFLEN*((int) fratio +1), sizeof(float));
    filtbuf = (double *) calloc(BUFLEN*((int) fratio +1), sizeof(double));
    firbuf  = (float *)  calloc(BUFLEN*((int) fratio +1), sizeof(float));

    if (!inpbuf || !outbuf || !filtbuf || !firbuf)
    {
        fprintf(stderr,"ERROR: interpol: calloc\n");
        exit(1);
    }

printf("\n");
printf("====== interpol: ipfact=%d dcfact=%d ratio=%.4f\n",
        ipfact, dcfact, fratio);
printf("====== interpol: sectlen1=%d sectlen2=%d\n", sectlen1, sectlen2);
printf("====== interpol: opening input %s\n", ifname);
printf("====== interpol: opening output %s\n", ofname);
printf("====== interpol: alloc %d data\n", (BUFLEN*((int) fratio +1)));

/*
 * Make output filename
 */
    len = strlen(ifname);
    strncpy(ofname, ifname, len-4);
    sprintf(&ofname[len-4], ".%s.SAC", ratio);

/* Open input & output files */

    if (!(Fp_inp = fopen(ifname, "r")))
    {
        fprintf(stderr,"ERROR: interpol: can't open file '%s'\n", ifname);
        exit(1);
    }

    if (!(Fp_out = fopen(ofname, "w")))
    {
        fprintf(stderr,"ERROR: interpol: can't open file '%s'\n", ofname);
        exit(1);
    }

/* Write sac nullheader to output file */

    fwrite(&sacH, sizeof(struct sac), 1, Fp_out);

/* Read sac header from input file */

    fread(&sacH, sizeof(struct sac), 1, Fp_inp);

/*
 * Compute start time
 */
/* Attention: padd char '0' at end of string to make 10000th of secs */

    sprintf(str, "%04ld,%03ld,%02ld:%02ld:%02ld.%03ld0",
            sacH.nzyear,
            sacH.nzjday,
            sacH.nzhour,
            sacH.nzmin,
            sacH.nzsec,
            sacH.nzmsec);
    asc_to_dbt(str, &stime, &startime);

    nInpData = sacH.npts;
    srate = (double) 1.0 / (rint(sacH.delta*100000.0) / 100000.0);


/*
 * Process data sections 1
 */
    nsection1   = nInpData / sectlen1;
    n_remaining = nInpData % sectlen1;

printf("==== interpol: section1: ndata=%d nsection1=%d remain=%d\n",
        nInpData, nsection1, n_remaining);

    resampleSection(nsection1, sectlen1, ipfact, dcfact, firDecim);


/*
 * Process remaining input data by reducing the section length
 */
    nInpData = n_remaining;
    nsection2   = nInpData / sectlen2;
    n_remaining = nInpData % sectlen2;

printf("==== interpol: section2: ndata=%d nsection2=%d remain=%d\n",
        nInpData, nsection2, n_remaining);

    resampleSection(nsection2, sectlen2, ipfact, dcfact, firDecim);

    fclose(Fp_inp); Fp_inp = NULL;

/*
 * Set filter delay and update time
 */
/* delay of 1 sample for oversampling process */
    filtdelay += (double) 1.0 / srate;

/* srate after resampling */
    srate = srate * fratio;

    startime = startime - filtdelay;

printf("==== end interpolate: %d in file %s srate=%f delay=%f\n",
        nOutData, ofname, srate, filtdelay);

/*
 * Write new sac header
 */
    newSacHeader(startime, nOutData, srate);

/*
 * Rewind and write SAC header
 */
    rewind(Fp_out);
    fwrite(&sacH, sizeof(struct sac), 1, Fp_out);
    fclose(Fp_out); Fp_out = NULL;


/*=========== FIR filter decimation ==============*/

    sprintf(ifname, "%s.%d", ofname, getpid());
    rename(ofname, ifname);

    if (!(Fp_inp = fopen(ifname, "r")))
    {
        fprintf(stderr,"ERROR: interpol: can't open file '%s'\n", ifname);
        exit(1);
    }

    if (!(Fp_out = fopen(ofname, "w")))
    {
        fprintf(stderr,"ERROR: interpol: can't open file '%s'\n", ofname);
        exit(1);
    }

printf("====== interpol: opening input %s\n", ifname);
printf("====== interpol: opening output %s\n", ofname);

/* Read sac header from input file */

    fread(&sacH, sizeof(struct sac), 1, Fp_inp);
    fwrite(&sacH, sizeof(struct sac), 1, Fp_out);

    nInpData = nOutData;
    sectlen1 = 10000;
    sectlen2 = 100;
    nOutData = 0;

/*
 * Process data sections 1
 */
    nsection1   = nInpData / sectlen1;
    n_remaining = nInpData % sectlen1;

    decimSection(nsection1, sectlen1, firDecim);

/*
 * Process remaining input data by reducing the section length
 */
    nInpData = n_remaining;
    nsection2   = nInpData / sectlen2;
    n_remaining = nInpData % sectlen2;

    decimSection(nsection2, sectlen2, firDecim);

printf("==== end FIR filter: %d in file %s srate=%f\n",
        nOutData, ofname, srate);

    fclose(Fp_inp); Fp_inp = NULL;
    unlink(ifname);

/* FIR filter delay, sample rate, start time */
    filtdelay = (double) ncoef / (double) 2 / srate;
    srate = srate / (double) firDecim;
    startime = startime - filtdelay;

/*
 * Write new sac header
 */
    newSacHeader(startime, nOutData, srate);

/*
 * Rewind and write SAC header
 */
    rewind(Fp_out);
    fwrite(&sacH, sizeof(struct sac), 1, Fp_out);
    fclose(Fp_out); Fp_out = NULL;

    free(inpbuf);
    free(outbuf);
    free(filtbuf);
    free(firbuf);

    return 0;
}


/*===================================================================*/
static void resampleSection(nsection, sectlen, ipfact, dcfact, firDecim)
int nsection;
int sectlen;
int ipfact, dcfact, firDecim;
{
static double y1;
static double y2, dy;
int n, nout, n_out;
int i, j, k ,kk;


    n_out   = 0;
    for (n=0; n<nsection; n++)
    {
        if (fread(inpbuf, sizeof(float), sectlen, Fp_inp) != sectlen)
        {
          fprintf(stderr, "ERROR: interpol: error reading file '%s'\n",ifname);
          exit(1);
        }

    /*
     * First time: set first data y1
     */
        if (nOutData == 0)
        {
            y1 = inpbuf[0];
        }

    /*
     * Resample: take "dcfact" over "ipfact" samples
     */
        n_out   = 0;
        outbuf[n_out++] = y1;
        for (i=0,j=0,kk=0; j<sectlen; j++,i+=ipfact)
        {
            y2 = inpbuf[j];
            dy = (y2 - y1) / (double) ipfact;

            for (k=0; k<ipfact; k++, kk++)
            {
                if (kk >= dcfact)
                {
                    kk = 0;
                    outbuf[n_out++] = y1 + (double) k * dy;
                }
            }
            y1 = y2;
        }
        nout = sectlen * ipfact / dcfact;

    /*
     * Write interpolated data to disk
     */
        if (fwrite(outbuf, sizeof(float), nout, Fp_out) != nout)
        {
           fprintf(stderr, "ERROR writing file '%s'\n", ofname);
           exit(1);
        }
        nOutData += nout;

    }  /* end for nsection */

    printf(" end resampleSection; %d data\n", nOutData);
}


/*===================================================================*/
static void decimSection(nsection, sectlen, firDecim)
int nsection;
int sectlen;
int firDecim;
{
int n, ndec;
int i, j;


    for (n=0; n<nsection; n++)
    {
        if (fread(inpbuf, sizeof(float), sectlen, Fp_inp) != sectlen)
        {
          fprintf(stderr, "ERROR: decimSection: error reading '%s'\n",ifname);
          exit(1);
        }

    /*
     * Apply low pass FIR filter
     */
    /*
     * First time: pad "ncoef" first input data to filter buffer
     */
        if (nOutData == 0) for (i=0; i<ncoef; i++) filtbuf[i] = inpbuf[0];

    /*
     * Load "sectlen" data
     */

        for (i=ncoef,j=0; j<sectlen; j++,i++) filtbuf[i] = inpbuf[j];

    /* Apply Fir filter */

        ndec = firFilter(fir, ncoef, filtbuf, firbuf, sectlen, firDecim);

    /*
     * Shift last input data to beg of filter buffer
     */
        for (i=sectlen, j=0; i<(ncoef+sectlen); i++,j++)
            filtbuf[j] = filtbuf[i];

    /*
     * Write filtered data to disk
     */
        if (fwrite(firbuf, sizeof(float), ndec, Fp_out) != ndec)
        {
           fprintf(stderr, "ERROR: decimSection: error writing '%s'\n", ofname);
           exit(1);
        }
        nOutData += ndec;

    }  /* end for nsection */

    printf(" end decimSection; %d data\n", nOutData);
}


/*===================================================================*/
static int firFilter(coef, ncoef, inp, out, ndata, decim)
double  *coef;
int     ncoef;
double  *inp;
float   *out;
int     ndata;
int     decim;
{
int    nout;
int    i, j;
double sum;

if (0) printf("++++ firFilter: ndata=%d  ", ndata);

    for (nout = 0, i=0; i<ndata; nout++,i+=decim)
    {
        sum = 0;
        for (j=0; j<ncoef; j++) sum += coef[j] * inp[i+j];
        out[nout] = sum;
    }

if (0) printf("nout=%d\n", nout);

    return nout;
}



/*===================================================================*/
static void newSacHeader(startime, nsamp, srate)
double startime;
int    nsamp;
double srate;
{
long       ltime;                /* UNIX time */
static struct tm *tms;

/*
 * Update SAC header
 */
    sacH.delta  = 1 / srate;
    sacH.npts   = nsamp;

printf("SAC: n=%ld sint=%f\n",
        sacH.npts, (rint(sacH.delta*100000.0)/100000.0));

/* convert double time into SAC time and record it */

    ltime = (long) startime;
    tms = gmtime(&ltime);
    sacH.nzyear = tms->tm_year + 1900;
    sacH.nzjday = tms->tm_yday + 1;
    sacH.nzhour = tms->tm_hour;
    sacH.nzmin  = tms->tm_min;
    sacH.nzsec  = tms->tm_sec;
    sacH.nzmsec = (int) ((startime - (double) ltime) * 1000.0);
}


#ifndef leap_year
#define leap_year(i) ((i % 4 == 0 && i % 100 != 0) || i % 400 == 0)
#endif

typedef struct
{
    int    year;
    int    month;
    int    day;
    int    hour;
    int    min;
    float  sec;
} Tstruct;


/*===================================================================*/
static void asc_to_dbt(str, stime, dbltime)
char  *str;
struct seed_time *stime;
double *dbltime;
{
int    i,j,k;
char str1[24];

    sprintf(str1, "0000,000,00:00:00.0000");
    strncpy(str1, str, strlen(str));   /* Do not use sprintf here */
    str1[strlen(str)] = '\0';

    stime->year   = stime->day    = stime->hour = 0;
    stime->minute = stime->second = stime->fracsec = 0;
    i = j = k = 0;
    sscanf(str1, "%4hd,%3hd,%2d:%2d:%2d.%4hd",
      &stime->year, &stime->day, &i, &j, &k, &stime->fracsec );
    stime->hour =   i;
    stime->minute = j;
    stime->second = k;
    tstruct_to_dbt(stime, dbltime);
}


/*===================================================================*/
/* Convert time structure into double UNIX time                      */
/*===================================================================*/
static void tstruct_to_dbt(time, dbltime)
struct seed_time *time;
double *dbltime;
{
int year;
double nsec;

    nsec = 0.0;
    for (year=1970; year<(time->year); year++)
    {
        if (leap_year(year)) nsec += 366.0 * 86400.0;
        else                 nsec += 365.0 * 86400.0;
    }
    if (time->day == 0)
    {
        printf("WARNING: tstruct_to_dbt: day number is null; ");
        printf("set to 1\n");
        time->day = 1;
    }
    nsec += (double) (time->day-1) * 86400.0;
    nsec += (double) time->hour    *  3600.0;
    nsec += (double) time->minute  *    60.0;
    nsec += (double) time->second;
    nsec += (double) time->fracsec / 10000.0;
    *dbltime = nsec;
}


/*===================================================================*
 *  Parse the given string.
 *===================================================================*/
static int sparse(input, argv, delimiters, max_tokens)
char *input;
char *argv[];
char *delimiters;
int  max_tokens;
{
extern char *strtok();
int i = 0;

    if (max_tokens < 1) {
        fprintf(stderr,"parse: illegal 'max_tokens'\n");
        return -1;
    }

    i = 0;
    if ((argv[i] = strtok(input, delimiters)) == NULL) return 0;
    for (i = 1; i < max_tokens; i++) {
        if ((argv[i] = strtok(NULL, delimiters)) == NULL) return i;
    }

    return i;
}

