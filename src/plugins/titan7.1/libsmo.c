/*=====================================================================
    libsmo.c


 * smoothTop: lissage et interpolation de la derive horaire
 * En entree, une liste de (top,derive), en sortie une liste
 * avec un point tout les speriode sec.
 *
 *    speriode= la derive est moyennee sur speriode heures,
 *              et en sortie, on a un point toute les speriode heures.
 *    seuil1= seuil d'elimination points aberrant, 1ere passe (~1sec)
 *    seuil2= seuil d'elimination points aberrant, 2nde passe (~.01sec)
 *    linear: si des points manquent:
 *     linear<0: interpolation lineaire
 *     linear>0: interpolation lineaire si moins de 'linear' points manquant,
 *               polynomiale (ordre 4) sinon
 *    smooth= lissage derive par triangle sur smooth points
 *
 =======================================================================*/
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include "proto.h"
#include "libxplot/xplot.h"
#include "libxplot/proto.h"


typedef struct {
  int     nval;
  int     imin;
  float   dt;
} STop;


#define NOVALUE 9999999
#define ABS(x) ((x)>0? (x): -(x))


#ifdef ANSI_C
int    find_file_format         (FILE*);
void   missing_points           (STop*, int);
void   Smooth                   (STop*, int, int);
void   smoothTop                (int, Top*, int, XY**, int*);
static double swap_double       (double);
static int    swap4byte         (int);
#else
int    find_file_format         ();
void   missing_points           ();
void   Smooth                   ();
void   smoothTop                ();
static double swap_double       ();
static int    swap4byte         ();
#endif


extern char Station[5];
extern int  BegYear;


/*================================================================*/
int read_top(fname, ptop)
char *fname;
Top **ptop;
{
FILE *Fp;
Top *top;
int      nentries, npt, lus;
char     line[255];
char     *token[4];
int      ntok;
char     temp[40];
int      i;
int      format;
double   sec;
int      imin;
float    dt;
int      ii;
int      *pi;
float    *pf;
double   delta_t;
double   dbtime;
double   prev_dbtime;
int      lnum;
struct stat stat;
int byteswap = 0;


    prev_dbtime = 0.0;

    if (!(Fp = fopen(fname,"r")))
    {
        fprintf(stderr,"read_top: can't open %s\n", fname);
        exit(1);
    }

    fread(temp,sizeof(char),20,Fp);
    rewind(Fp);
    for (i=0; i<20; i++)
    {
        if (isascii((int)temp[i]));
        else break;
    }

/* File is ASCII */

    if (i == 20)
    {
/* How many entries do we have in the observed delta file ? */

        nentries = 0;
        while (getline(Fp, line)) ++nentries;
        rewind(Fp);

/* Allocate memory */

        npt = nentries + 20;
        if (!(top = (Top *) malloc(npt*sizeof(Top))))
        {
            fprintf(stderr, "ERROR: read_top: malloc failed (1) for ");
            fprintf(stderr, "%d bytes\n", npt*sizeof(Top));
            exit(1);
        }

        format = find_file_format(Fp);

        npt = 0;
        lnum = 0;
        while (getline(Fp, line))
        {
            ++ lnum;
            trim(line);
            if (strstr(line, "time reset"))
            {
/* printf("read_top: %s\n", line); */
                continue; 
            }

/* Parse line, 3 words max */
            ntok = sparse(line, token, " \t", 3);

/* Discard unknown times */
            if (ntok > 1 && token[1][0] == '?') continue;

/* Convert  ascii string into double Unix time */
            if      (format == 1) str2utime3(token[0], &dbtime);
            else if (format == 4) str2utime1(token[0], &dbtime);
            else                  dbtime = atof(token[0]);

/* Discard bad times */
            if (!TIME_OK(dbtime)) continue;

/* Get delta time */
            if (format == 3)
            {
                get_secs(dbtime, &delta_t);
                if (delta_t> 29.0) delta_t-= 60.0;
            }
            else
                delta_t = atof(token[1]);

            if (dbtime < prev_dbtime)
            {
                printf("    WARNING: TIME IS GOING BACKWARD line %d %s\n",
                        lnum, fname);
            }
            prev_dbtime = dbtime;

/* Fill-up Top structure */
            top[npt].sec  = dbtime;
            top[npt].imin = ((int) top[npt].sec / 60) * 60;
            top[npt].dt   = delta_t;
            ++npt;
        }
printf("read_top: %d points in ascii top file (format %d)\n",npt,format);
        goto end;
    }



/* File is BINARY: .top */

/* 
 * Verify real top file size. Warning, file can be padded on disk
 * so this give just a rough idea
 */

    fstat(fileno(Fp),&stat);
    npt=stat.st_size/sizeof(Top);

    if (npt==0)
    {
        printf(" no point in this file\n");
        exit(1);
    } 
/*  Allocate memory */

    if (!(top = (Top *) malloc(sizeof(Top)*npt)))
    {
      fprintf(stderr, "ERROR: read_top: malloc failed for ");
      fprintf(stderr, "%d bytes\n", (sizeof(Top)*npt));
      exit(1);
    }

    lus = fread(top,sizeof(Top),npt,Fp);
    if (lus != npt)
    {
      fprintf(stdout," WARNING: read_top: expected %d tops but read %d\n",
          npt,lus);
      npt=lus;
    }

    if (top[0].imin < 0 || top[0].sec < 0. || fabs(top[0].dt) > 2000.0)
    {
        printf("Hmm... looks like the file needs byte swapping\n");
        byteswap = TRUE;
    }

    if (byteswap) for (i=0; i<npt; i++)
    {
        sec = swap_double(top[i].sec);
        imin = swap4byte(top[i].imin);

        pi = (int *) &top[i].dt;
        ii = swap4byte(*pi);
        pf = (float*) &ii;
        dt = *pf;

        if (0) printf("==== sec %.3f min %d dt %.3f\n", sec, imin, dt);
        top[i].sec = sec;
        top[i].imin = imin;
        top[i].dt = dt;
    }

    printf("read_top: %d points in binary top file\n", npt);
end:
    fclose(Fp);

if (0) for (i=0; i<npt; i++)
     printf("%d  %d  %.3f  %.3f\n", i, top[i].imin, top[i].sec, top[i].dt);

    *ptop = top;
    return npt;
}


/*================================================================*/
int find_file_format(Fp_dt)
FILE *Fp_dt;
{
char     line[255];
char    *token[4];
int      ntok;
int      format;

    format = 0;
    while (getline(Fp_dt, line))
    {
        trim(line);
        if (strstr(line, "time reset")) continue;
/* Parse line, 3 words max */
        ntok = sparse(line, token, " \t", 3);
/* Discard unknown entries */
        if (ntok > 1 && token[1][0] == '?') continue;

        if (strlen(token[0]) > 21)      /* 1997.06.11-18:12:59.986 -0.456 */
            format = 4;
        else if (strlen(token[0]) > 15) /* 97.06.11-18:12:59.986 -0.456 */
            format = 1;
        else if (ntok == 1)             /* 866050560.234  */
            format = 3;
        else                            /* 875485800 -0.0874 */
            format = 2;
    }
    rewind(Fp_dt);
    return format;
}


/*=======================================================================*/
void smoothTop(sper, top, npt, xy, nxy)
int sper;
Top *top;
int npt;
XY **xy;
int *nxy;
{
int      i,j,k,l,firstPer,lastPer,nval,imin,nstop;
int      manquant;
double   dtmean,dtmean1,dtavg;
STop     *SmoothTop;

int speriode;
int smooth = 3;
double seuil1 = 1.00;
double seuil2 = 0.04;
double begDay;
double first_top;
double last_top;
double dt0;
double x1, y1, x2, y2;

    speriode = sper;
    if (speriode < 100)
    {
        printf("smoothTop: Duration too short: min is 100 seconds\n");
        if (*xy) { free_xy(*xy, "smoothtop: xy"); *xy = NULL; }
        *nxy = 0;
        return;
    }

    first_top = top[0].sec;
    last_top  = top[npt-1].sec;

/* Check first top */

    if (!TIME_OK(top[0].imin) || !TIME_OK(top[0].sec))
    {
        printf("smoothTop: unexpected start time: min=%d sec=%d\n",
                (int) top[0].imin, (int) top[0].sec);
        exit(1);
    }

if (0) printf("==== firstPer %d  speriode %d\n", top[0].imin, speriode);

/*
 * lissage avec 1 point toutes les HPERIODE heures (ex 3h)
 * Les tops lisses sont stockes dans SmoothTop
 */
    firstPer=(int)((double)top[0].imin/speriode);
    lastPer=(int)((double)top[npt-1].imin/speriode);

if (0) fprintf(stderr,"smoothTop: firstPer %d  lastPer %d n %d per %d\n",
           firstPer, lastPer, npt, speriode);

    l = sizeof(STop)*(lastPer-firstPer+2);
    if (!(SmoothTop = (STop *) malloc(l)))
    {
        fprintf(stderr, "ERROR: smoothTop: malloc failed; %d data; ",
            (lastPer-firstPer+2));
        fprintf(stderr, "firstPer %d lastPer %d\n", firstPer,lastPer);
        exit(1);
    }
if (0) printf("alloc %d bytes for SmoothTop[]\n", l);

    firstPer*=speriode;
    lastPer*=speriode;
    dtavg=0.;
    manquant=0;

if (0) fprintf(stderr,"smoothTop: firstPer %d  lastPer %d n %d per %d\n",
           firstPer, lastPer, npt, speriode);


    l = 0;
    i = 0;
    for (imin=firstPer;  imin<lastPer+speriode;  imin+=speriode)
    {
        if (i >= npt) break;

/* Process tops falling inside "speriode" sample interval */

        dtmean=0.;
        nval=0;
        j=i;
/*        while(top[i].imin < (imin+speriode) && i<npt) */
        while (i<npt)
        {
            if (top[i].imin >= (imin+speriode)) break;
            dtmean+=top[i].dt;
            if (i>0 && top[i].imin != top[i-1].imin)
                top[i].sec=(top[i].dt-top[i-1].dt)/(top[i].imin-top[i-1].imin);
            if (i==1)
                top[0].sec=top[1].sec;
if (0) printf("dt=%.3f top[%4d].sec=%.3f\n", top[i].dt, i, top[i].sec);
            i++;
            nval++;
        }
        
        if (nval)
        {
            dtmean/=nval;
if (0) printf("nval=%d  dtmean=%.3f \n", nval, dtmean);
/* 
 * elimine les valeurs aberrantes en 2 passes
 */

/*
 * 1ere passe: on compare le top avec un filtrage passe bas des
 * tops. si l<2, le filtre n'est pas encore construit.
 * si il manque trop de points (>5) , on ne peut pas le construire
 * non plus. 
   dtmean1=((l <2 ) || (manquant >= 5)) ? dtmean : dtavg;
 */
            if (l < 2 || manquant >= 5 ) dtmean1 = dtmean;
            else                         dtmean1 = dtavg;

            for (k=j;k<i;k++)
            {
                if (ABS(top[k].dt-dtmean1) > seuil1)
                {
                  fprintf(stdout,"smoothTop ==> reject top %f (>%f sec) ",
                      ABS(top[k].dt-dtmean1),seuil1);
                  fprintf(stdout,"at day %.3f (%d)\n",
                      dbtime2dbday((double)top[k].imin,BegYear), top[k].imin);
                  dtmean = (dtmean*nval-top[k].dt)/(nval-1);
                  nval--;
                  top[k].dt = NOVALUE;
                }
                else
                  dtavg=.7*top[k].dt+.3*dtavg;
            }

 /* 2eme passe */

            for (k=j;k<i;k++)
            {
               if (top[k].dt == NOVALUE) continue;

               if (ABS(top[k].dt-dtmean) > seuil2)
               {
                 fprintf(stdout,"smoothTop ==> reject top %f (>%f sec) ",
                     ABS(top[k].dt-dtmean),seuil2);
                 fprintf(stdout,"at day %.3f (%d)\n",
                     dbtime2dbday((double)top[k].imin,BegYear), top[k].imin);
                 dtmean = (dtmean*nval-top[k].dt)/(nval-1);
                 nval--;
                 top[k].dt = NOVALUE;
               }
            }
        }   /* end if nval */


        SmoothTop[l].dt=(float)dtmean;
        SmoothTop[l].imin=imin+speriode/2;
        SmoothTop[l].nval=nval;
        manquant=(nval==0)? manquant+1: 0;

if (0) printf("SmoothTop[%d].dt=%.3f\n", l, SmoothTop[l].dt);

        ++l;
    }
    nstop=l;


/*
 * traitement des points manquant
 */
    missing_points(SmoothTop, nstop);


/*
 * lissage
 */
    Smooth(SmoothTop, nstop, smooth);


    if (*xy) { free_xy(*xy, "smoothtop: xy"); *xy = NULL; }
    *nxy = nstop;
    malloc_xy(xy, (*nxy)+2, "smoothtop: xy");

    begDay = dbtime2dbday((double) firstPer,BegYear);

/* Extrapolate first and last smoothed top to first and last raw top */ 
/*
printf("==== first top %d last %d\n",(int)first_top,(int)last_top);
printf("first smo %d %E\n", SmoothTop[0].imin, SmoothTop[0].dt);
printf("last  smo %d %E\n", SmoothTop[nstop-1].imin, SmoothTop[nstop-1].dt);
*/
    x1 = SmoothTop[0].imin;
    x2 = SmoothTop[1].imin;
    y1 = SmoothTop[0].dt;
    y2 = SmoothTop[1].dt;
    dt0 = y1 + ((y2-y1) / (x2-x1)) * (first_top - x1);
    SmoothTop[0].imin = first_top;
    SmoothTop[0].dt  = dt0;
/*
printf("--> first smo %d %E\n", SmoothTop[0].imin, SmoothTop[0].dt);
*/
    x1 = SmoothTop[nstop-2].imin;
    x2 = SmoothTop[nstop-1].imin;
    y1 = SmoothTop[nstop-2].dt;
    y2 = SmoothTop[nstop-1].dt;
    dt0 = y2 + ((y2-y1) / (x2-x1)) * (last_top - x2);
    SmoothTop[nstop-1].imin = last_top;
    SmoothTop[nstop-1].dt  = dt0;
/*
printf("--> last  smo %d %E\n", SmoothTop[nstop-1].imin, SmoothTop[nstop-1].dt);
*/

    for (i=0;i<nstop;i++) 
    {
/*
        x = (float) (((double)SmoothTop[i].imin-(double)firstPer) /
                      (double)DAY + begDay);
*/
        (*xy)[i].x = SmoothTop[i].imin;
        (*xy)[i].y = SmoothTop[i].dt;

        if (0) printf("%.3f %E\n", (*xy)[i].x, (*xy)[i].y);
    }
    free(SmoothTop);
}

/*================================================================*/
void missing_points(SmoothTop, nstop)
STop     *SmoothTop;
int      nstop;
{
int i, j, jj, k;
double a;
int linear = -1;
int top_before,top_after;
int dbug = 0;

if (dbug) printf("==== missing_points; nstop=%d\n", nstop);
  /*
   * traitement des points manquant
   */
    for (i=0;i<nstop;i++)
    {
        if (SmoothTop[i].nval) continue;

    /*
     * 1) nombre de points manquant
     */
        j=i;
        k=i;
        while (!SmoothTop[j].nval && j>0)     j--;
        while (!SmoothTop[k].nval && k<nstop) k++;
        top_before=j;
        top_after=nstop-k;

if (dbug) printf("==== top #%d missing  previous=%d  next=%d\n",
        i,  top_before, k /*top_after*/);

        


    /*
     * interpolation lineaire
     */
        if (linear<=0 || (k-j+1 <= linear)|| top_before<3 || top_after<3)
        {

            a=(SmoothTop[k].dt-SmoothTop[j].dt)/
              (SmoothTop[k].imin-SmoothTop[j].imin);
            for (jj=j+1;jj<k;jj++)
            {
                SmoothTop[jj].nval=1;
                SmoothTop[jj].dt=a*(SmoothTop[jj].imin-SmoothTop[j].imin)+
                        SmoothTop[j].dt;
if (dbug)
    printf("     interpol   SmoothTop[%4d].dt %.3f\n", jj, SmoothTop[jj].dt);
            }
        }
        i=k;
    }
}

/*================================================================*/
void Smooth(SmoothTop, nstop, smooth)
STop     *SmoothTop;
int      nstop;
int      smooth;
{
int i, j, k, kk;
double   sum,*b,*c;

    if (smooth < 1)
    {
         printf("Smooth: smooth val = %d unsuited; return\n", smooth);
         return;
    }
if (0) printf("==== smooth: %d \n", smooth);
    j=smooth/2;
    k=2*j+1;
    if (!(b=(double*)malloc(k*sizeof(double))))
    {
        fprintf(stderr,"ERROR: smooth: malloc failed\n");
        exit(1);
    }
    if (!(c=(double*)malloc(nstop*sizeof(double))))
    {
        fprintf(stderr,"ERROR: smooth: malloc failed\n");
        exit(1);
    }
    for (i=0;i<nstop;i++) 
        c[i]=SmoothTop[i].dt;
    sum=1.;
    for (i=0;i<j;i++) 
        sum+=2.*(i+1)/(j+1);
    b[j]=1./sum;
    for (i=0;i<j;i++)
        b[k-i-1]=b[i]=(float)(i+1)/(float)(j+1)*b[j];
    for (i=0+j;i<nstop-j;i++)
    {
        SmoothTop[i].dt=c[i]*b[j];
        for (kk=0;kk<j;kk++)
            SmoothTop[i].dt+=b[kk]*(c[i-j+kk]+c[i+kk+1]);
    }
    free(b);
    free(c);
}


/*============================================================*/
static int swap4byte(w)
int w;
{
union
{
    unsigned char character[4];
    unsigned long int integer;
} swap4byte;                                /* holds 4-byte word */

char temp0;
char temp1;

    swap4byte.integer = w;

    temp0 = swap4byte.character[0];
    temp1 = swap4byte.character[1];
    swap4byte.character[0] = swap4byte.character[3];
    swap4byte.character[1] = swap4byte.character[2];
    swap4byte.character[2] = temp1;
    swap4byte.character[3] = temp0;

    return (swap4byte.integer);
}


/*============================================================*/
double swap_double(db)
double db;
{
union
{
    long lint[2];
    double dbl;
} swdb;

static long l0;
static long l1;

    swdb.dbl = db;
if (0) printf("%0lX %0lX\n", (long) swdb.lint[0], (long) swdb.lint[1]);

    l0 = swap4byte(swdb.lint[0]);
    l1 = swap4byte(swdb.lint[1]);
if (0) printf("%0lX %0lX\n", l0, l1);

    swdb.lint[1] = l0;
    swdb.lint[0] = l1;
if (0) printf("%0lX %0lX\n", (long) swdb.lint[0], (long) swdb.lint[1]);

if (0) printf("=== %E\n", swdb.dbl);
    return swdb.dbl;
}

