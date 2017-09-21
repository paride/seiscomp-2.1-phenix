/*====================================================================
    rsampsac.c
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

#ifdef ANSI_C
static void   getRatio(char *, int*, int*, double*, int*);
static void   resampleSection(int, int, int, int);
static void   FirFilter(char*, char*, double, int);
static void   commonDivider(int*, int*);
static void   decimSection(int, int, int);
static double sacTime(struct sac *);
static void   newSacHeader(double, int, double);
static void   asc_to_dbt(char*, struct seed_time*, double *);
static void   tstruct_to_dbt(struct seed_time*, double*);
static int    sparse(char*, char**, char*, int);
#else
static void   getRatio();
static void   resampleSection();
static void   FirFilter();
static void   commonDivider();
static void   decimSection();
static double sacTime();
static void   newSacHeader();
static void   asc_to_dbt();
static void   tstruct_to_dbt();
static int    sparse();
#endif


#define BUFLEN 10000

static FILE    *Fp_inp, *Fp_out;
static char    ifName[511], ofName[511];
static float   *inpbuf;
static float   *outbuf;
static double  *filtbuf;
static float   *firbuf;
static double  *fir;
static int     ncoef;
static int     npast;
static double  TotDelay;
static int     nOutData;
static struct  sac sacH;
static int     dbug;


/*===================================================================*/
int main(argc, argv)
int     argc;
char    *argv[];
{
int     nInpData, n_remaining;
int     sectlen1, sectlen2, nsection1, nsection2;
int     ipfact, dcfact, firDecim;
double  startime, srate, filtdelay;
int     i, len;
char    ratio[256];
double  fratio;
int npassFir128;
int npassFir149;


    sacH = sac_nullswap;
    sacH = sac_null;
    Fp_inp = NULL;
    Fp_out = NULL;
    ipfact = 16;
    dcfact = 1;
    fratio = ipfact / dcfact;
    sprintf(ratio, "%d:%d", ipfact, dcfact);
    firDecim  = 1;
    fir   = Fir128;
    ncoef = 128;
    npast = ncoef - 1;
    sectlen1 = 10000;
    sectlen2 = 100;
    srate    = 1;
    filtdelay = 0.0;
    nOutData = 0;
    npassFir128 = 0;
    npassFir149 = 0;
    dbug = 0;


    if (argc < 3)
    {
        printf("Usage: rsampsac sacfile ratio\n");
        printf("Example rsampsac data.sac 16:5\n");
        exit(1);
    }

    sprintf(ifName, "%s", argv[1]);

/*
 * Get and check resampling ratio
 */
    sprintf(ratio, "%s", argv[2]);
    getRatio(argv[2], &ipfact, &dcfact, &fratio, &firDecim);

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

/*
 * Make output filename
 */
    len = strlen(ifName);
    strncpy(ofName, ifName, len-4);
    sprintf(&ofName[len-4], ".%s.SAC", ratio);

/* Open input & output files */

    if (!(Fp_inp = fopen(ifName, "r")))
    {
        fprintf(stderr,"ERROR: interpol: can't open file '%s'\n", ifName);
        exit(1);
    }

    if (!(Fp_out = fopen(ofName, "w")))
    {
        fprintf(stderr,"ERROR: interpol: can't open file '%s'\n", ofName);
        exit(1);
    }

if (dbug)
{
printf("\n");
printf("====== interpol: ipfact=%d dcfact=%d ratio=%.4f firdecim=%d\n",
        ipfact, dcfact, fratio, firDecim);
printf("====== interpol: sectlen1=%d sectlen2=%d\n", sectlen1, sectlen2);
printf("====== interpol: opening input %s\n", ifName);
printf("====== interpol: opening output %s\n", ofName);
printf("====== interpol: alloc %d data\n", (BUFLEN*((int) fratio +1)));
}

/* Write sac nullheader to output file */

    fwrite(&sacH, sizeof(struct sac), 1, Fp_out);

/* Read sac header from input file */

    fread(&sacH, sizeof(struct sac), 1, Fp_inp);

/*
 * Get start time, num of samples and sample rate
 */
    startime = sacTime(&sacH);
    nInpData = sacH.npts;
    srate = (double) 1.0 / (rint(sacH.delta*100000.0) / 100000.0);

/*
 * Process data sections 1
 */
    nsection1   = nInpData / sectlen1;
    n_remaining = nInpData % sectlen1;

if (dbug)
    printf("==== interpol: section1: ndata=%d nsection1=%d remain=%d\n",
            nInpData, nsection1, n_remaining);

    resampleSection(nsection1, sectlen1, ipfact, dcfact);


/*
 * Process remaining input data by reducing the section length
 */
    if (sectlen2 == 0)
    {
        nsection2 = 0;
    }
    else
    {
        nInpData = n_remaining;
        nsection2   = nInpData / sectlen2;
        n_remaining = nInpData % sectlen2;
    }

if (dbug)
    printf("==== interpol: section2: ndata=%d nsection2=%d remain=%d\n",
            nInpData, nsection2, n_remaining);

    resampleSection(nsection2, sectlen2, ipfact, dcfact);

    fclose(Fp_inp); Fp_inp = NULL;

/*
 * Set filter delay, srate and update time
 */
    /* delay for oversampling process is 1 sample */
    filtdelay += (double) 1.0 / srate;
    startime = startime - filtdelay;
    srate = srate * fratio;

if (dbug)
    printf("==== end interpolate: %d in file %s srate=%f delay=%f\n",
            nOutData, ofName, srate, filtdelay);

    printf("output %s n=%d %.8f sps\n", ofName, nOutData, srate);

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

if (0)
{
/*
 * Fir128 is for decim by 2, Fir149 for decim by 4
 */
    switch (firDecim)
    {
      case   1: npassFir128 = 1; npassFir149 = 0; break;
      case   2: npassFir128 = 1; npassFir149 = 0; break;
      case   4: npassFir128 = 0; npassFir149 = 1; break;
      case   8: npassFir128 = 1; npassFir149 = 1; break;
      case  16: npassFir128 = 0; npassFir149 = 2; break;
      case  32: npassFir128 = 1; npassFir149 = 2; break;
      case  64: npassFir128 = 0; npassFir149 = 3; break;
      case 128: npassFir128 = 1; npassFir149 = 3; break;
      default :
        fprintf(stderr,"ERROR: interpol: unsupported decimation factor %d\n",
                firDecim);
        exit(1);
    }

    if (npassFir128 == 1)
    {
        fir  = Fir128;
        ncoef = 128;
        sprintf(ifName, "tmpfilt.%d", getpid());
        rename(ofName, ifName);

        if (firDecim == 1)
        {
            FirFilter(ifName, ofName, srate, 1);
        }
        else
        { 
            FirFilter(ifName, ofName, srate, 2);
            srate /= (double) 2;
        }
    }
    for (i=0; i<npassFir149; i++)
    {
        fir  = Fir149;
        ncoef = 149;
        sprintf(ifName, "tmpfilt.%d", getpid());
        rename(ofName, ifName);

        FirFilter(ifName, ofName, srate, 4);
        srate /= (double) 4;
    }
}
else
{
    switch (firDecim)
    {
      case   1: npassFir128 = 1; npassFir149 = 0; break;
      case   2: npassFir128 = 1; npassFir149 = 0; break;
      case   4: npassFir128 = 2; npassFir149 = 0; break;
      case   8: npassFir128 = 3; npassFir149 = 0; break;
      case  16: npassFir128 = 4; npassFir149 = 0; break;
      case  32: npassFir128 = 5; npassFir149 = 0; break;
      case  64: npassFir128 = 6; npassFir149 = 0; break;
      case 128: npassFir128 = 7; npassFir149 = 0; break;
      default :
        fprintf(stderr,"ERROR: interpol: unsupported decimation factor %d\n",
                firDecim);
        exit(1);
    }

    for (i=0; i<npassFir128; i++)
    {
        fir  = Fir128;
        ncoef = 128;
        sprintf(ifName, "tmpfilt.%d", getpid());
        rename(ofName, ifName);

        if (firDecim == 1)
        {
            FirFilter(ifName, ofName, srate, 1);
        }
        else
        { 
            FirFilter(ifName, ofName, srate, 2);
            srate /= (double) 2;
        }
    }
}
    free(inpbuf);
    free(outbuf);
    free(filtbuf);
    free(firbuf);

    return 0;
}




/*===================================================================*/
static void FirFilter(ifname, ofname, srate, firDecim)
char *ifname, *ofname;
double srate;
int firDecim;
{
int     ndata;
double  startime, filtdelay;
int     sectlen1, sectlen2, nsection1, nsection2;
int     n_remaining;
double  nsdelay;

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

if (dbug)
{
printf("====== FirFilter: srate=%.6f decimation %d\n", srate, firDecim);
printf("====== FirFilter: opening input %s\n", ifname);
printf("====== FirFilter: opening output %s\n", ofname);
}

/*
 * Read sac header from input file and copy to output file
 */
    fread(&sacH, sizeof(struct sac), 1, Fp_inp);
    fwrite(&sacH, sizeof(struct sac), 1, Fp_out);

/*
 * Get start time and num of samples from sac header
 */
    startime = sacTime(&sacH);
    ndata    = sacH.npts;

/*
 * FIR filter delay, sample rate, start time
 */

/*
 * Compute filter delay expressed as a number of samples
 */
    if      (npast == ncoef)
        nsdelay = ((double) ncoef + 1.0)  / 2.0;
    else if (npast == (ncoef-1))
        nsdelay = ((double) ncoef - 1.0)  / 2.0;
    else
    {
        fprintf(stderr,"ERROR: interpol: wrong 'npast' value: %d\n", npast);
        exit(1);
    }
    filtdelay = nsdelay / srate;
    TotDelay += filtdelay;

printf("==== FirFilter: ncoef=%d del=%.1f samp (%.5f sec) total=%.5f\n",
        ncoef, nsdelay, filtdelay, TotDelay);

    srate = srate / (double) firDecim;
    startime = startime - filtdelay;


    sectlen1 = 10000;
    sectlen2 = 100;
    sectlen1 = (sectlen1 / firDecim) * firDecim;
    sectlen2 = (sectlen2 / firDecim) * firDecim;

    nOutData = 0;

/*
 * Process data sections 1
 */
    nsection1   = ndata / sectlen1;
    n_remaining = ndata % sectlen1;

    decimSection(nsection1, sectlen1, firDecim);

/*
 * Process remaining input data by reducing the section length
 */
    ndata = n_remaining;
    nsection2   = ndata / sectlen2;
    n_remaining = ndata % sectlen2;

    decimSection(nsection2, sectlen2, firDecim);


if (dbug)
    printf("==== end FIR filter: %d in file %s srate=%f\n",
            nOutData, ofname, srate);

    printf("output %s n=%d %.8f sps\n", ofname, nOutData, srate);

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
    fclose(Fp_inp); Fp_inp = NULL;
    unlink(ifname);
}



/*===================================================================*/
static void getRatio(argv, num, den, ratio, firdec)
char *argv;
int *num, *den;
double *ratio;
int    *firdec;
{
char    str[256];
char    *token[3];
int     ntok;
int     ipfact, dcfact;
double  fratio;
int     firDecim;

    firDecim = 1;
    sprintf(str, "%s", argv);
    ntok = sparse(str, token, ":", 2);
    if (ntok != 2)
    {
        fprintf(stderr,"ERROR: getRatio: ratio syntax\n");
        exit(1);
    }
    ipfact = atoi(token[0]);
    dcfact = atoi(token[1]);

    if (ipfact > 1000)
    {
        fprintf(stderr,"ERROR: getRatio: oversampling value too big\n");
        exit(1);
    }
    if (dcfact > 1000)
    {
        fprintf(stderr,"ERROR: getRatio: decimation value too big\n");
        exit(1);
    }

    fratio = (double) ipfact / (double) dcfact;

/*
 * If resampling ratio smaller or equal to unity, oversample and decimate
 */
    while (1)
    {
        if (fratio <= 1.0)
        {
            ipfact *= 2;
            fratio *= 2;
            firDecim *= 2;
        }
        if (fratio > 1.0)
            break;
    }
/*
printf("==== getRatio: ipfact=%d dcfact=%d ratio %.4f firdecim=%d\n",
        ipfact, dcfact, fratio, firDecim);
*/
    if (firDecim > 128)
    {
        fprintf(stderr,"ERROR: getRatio: FIR decim value %d too big\n",
                firDecim);
        exit(1);
    }

/*
 * Find common greater divider and simplifiy ratio
 */

    commonDivider(&ipfact, &dcfact);

    *num = ipfact;
    *den = dcfact;
    *ratio = fratio;
    *firdec = firDecim;
}



/*===================================================================*/
static void commonDivider(num, den)
int *num, *den;
{
int a, b;
int aa, ndiva;
int diva[10];
int bb, ndivb;
int divb[10];
int commonDivider;
int i, j;
static int dbug = 0;

    aa = a = *num;
    bb = b = *den;
    ndiva = 0;
    ndivb = 0;
    commonDivider = 1;

    if (dbug) printf("a=%d b=%d\n", aa, bb);
    for (i=2; i<aa; i++)
    {
        if ((a % i) == 0)
        {
            a = a / i;
            diva[ndiva++] = i;
            if (a == 1) break;
            i = 1;
        }
    }
    for (i=2; i<bb; i++)
    {
        if ((b % i) == 0)
        {
            b = b / i;
            divb[ndivb++] = i;
            if (b == 1) break;
            i = 1;
        }
    }

    if (dbug)
    {
        for (i=0; i<ndiva; i++) printf(" %d", diva[i]);
        printf("\n");
        for (i=0; i<ndivb; i++) printf(" %d", divb[i]);
        printf("\n");
    }

    commonDivider = 1;
    i = 0;
    j = 0;
    while (1)
    {
        if (diva[i] > divb[j])
        {
            if (++j > ndivb) break;
        }
        else if (diva[i] < divb[j])
        {
            if (++i > ndiva) break;
        }
        else if (diva[i] == divb[j])
        {
            if (dbug) printf("common div %d at i=%d j=%d\n", diva[i], i, j);
            commonDivider *= diva[i];
            if (++i > ndiva) break;
            if (++j > ndivb) break;
        }
    }

    if (dbug) printf("greater common divider %d \n", commonDivider);
    *num /= commonDivider;
    *den /= commonDivider;
    if (((double) (*num) / (double) (*den)) != ((double) aa / (double) bb))
    {
        fprintf(stderr, "ERROR: commonDivider\n");
        exit(1);
    }
}


/*===================================================================*/
static void resampleSection(nsection, sectlen, ipfact, dcfact)
int nsection;
int sectlen;
int ipfact, dcfact;
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
          fprintf(stderr, "ERROR: interpol: error reading file '%s'\n",ifName);
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
for (i=0; i<nout; i++) outbuf[i] = 0.0;
outbuf[1] = 1.0;
*/
    /*
     * Write interpolated data to disk
     */
        if (fwrite(outbuf, sizeof(float), nout, Fp_out) != nout)
        {
           fprintf(stderr, "ERROR writing file '%s'\n", ofName);
           exit(1);
        }
        nOutData += nout;

    }  /* end for nsection */

if (dbug)
    printf("==== end resampleSection; %d data\n", nOutData);
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
        if (fread(inpbuf, sizeof(float), sectlen, Fp_inp) != sectlen)
        {
          fprintf(stderr, "ERROR: decimSection: error reading '%s'\n",ifName);
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
            firbuf[ndec] = sum;
        }

    /*
     * Shift last input data to beg of filter buffer
     */
        for (i=sectlen, j=0; i<(npast+sectlen); i++,j++)
            filtbuf[j] = filtbuf[i];

    /*
     * Write filtered data to disk
     */
        if (fwrite(firbuf, sizeof(float), ndec, Fp_out) != ndec)
        {
           fprintf(stderr, "ERROR: decimSection: error writing '%s'\n",
                            ofName);
           exit(1);
        }
        nOutData += ndec;

    }  /* end for nsection */

if (dbug)
    printf("==== end decimSection; %d data\n", nOutData);
}



/*===================================================================*/
static double sacTime(head)
struct  sac *head;
{
char    str[255];
struct  seed_time stime;
double  startime;

/* Attention: padd char '0' at end of string to make 10000th of secs */

    sprintf(str, "%04ld,%03ld,%02ld:%02ld:%02ld.%03ld0",
            head->nzyear,
            head->nzjday,
            head->nzhour,
            head->nzmin,
            head->nzsec,
            head->nzmsec);
    asc_to_dbt(str, &stime, &startime);
    return startime;
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

/*
printf("SAC: n=%ld sint=%f\n",
        sacH.npts, (rint(sacH.delta*100000.0)/100000.0));
*/

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

