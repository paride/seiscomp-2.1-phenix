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
#include "libxplot/proto.h"
#include "libxplot/xplot.h"

#define NOVALUE 9999999
#define ABS(x) ((x)>0? (x): -(x))

typedef struct {
  int     nval;
  int     imin;
  float   dt;
} STop;

#ifdef ANSI_C
extern void  smoothTop        (int, Top*, int, XY**, int*);
extern int   find_file_format (FILE*);
extern void  Smooth           (STop*, int, int);
extern void  missing_points   (STop*, int);
static int   process_delta_t  (char*, Top**);
static void  smoothTop_       (int, Top*, int, FILE*);
#else
extern void  smoothTop        ();
extern int   find_file_format ();
extern void  Smooth           ();
extern void  missing_points   ();
static int   process_delta_t  ();
static void  smoothTop_       ();
#endif

char    Station[8];
int     dballoc = 1;
int     BegYear;


/*=====================================================================*/
void main(argc, argv)
int    argc;
char **argv;
{
char dtfile[255];
int  ntop;
XY   *xy;
int  nxy;
int i;
Top *top;

    if (argc < 3)
    {
        printf("Usage:\n");
        printf(" smo sta filename\n");
        printf("\n");
        exit(1);
    }

    sprintf(Station, "%s", argv[1]);
    sprintf(dtfile, "%s", argv[2]);
    ucase(Station);

    ntop = process_delta_t(dtfile, &top);

exit(0);

    for (i=0;i<ntop;i++)
    {
        printf("%d %.4f\n", (int) top[i].imin, top[i].dt);
    }



    smoothTop(3, top, ntop, &xy, &nxy);

    printf(" %d nxy\n", nxy);
    for (i=0;i<nxy;i++)
        printf("%d %E\n", (int) xy[i].x, xy[i].y);
}


/*================================================================*
case 1  NORMAL
------
891309960.000 time reset (synchro)
892168980.3660
892170960.3670

case 2
------
874679580.000 time reset (synchro)
874730159.8430
874814459.6870
874989959.4450
875076779.3340
875268720.000 new time reset (synchro)
875271479.9890
875312219.8680
875321759.9000

case 3
------
no time reset in .dt file

case 4   no intial time reset, 2 consecutive time reset,
------   time going backward
866142096.0700
866142276.0690
866142336.0670
866142336.0670
866142576.0640
866142876.0610
866142996.0600
866143116.0580
866050500.000 time reset (synchro)
866050560.000 new time reset (synchro)
866050560.0000
866050620.0000
866050980.0010

case 5   2 consecutive time reset at beg of .dt file
------
892135268.000 time reset (reboot)
892135440.000 new time reset (synchro)
892135699.8360
892135819.8360
892139539.8350
892139599.8350
 *================================================================*/
static int process_delta_t(fname, ptop)
char *fname;
Top **ptop;
{
FILE   *Fp_dt = NULL;
FILE   *Fp_rah = NULL;
char   rahfile[255];
FILE   *Fp_top = NULL;
char   topfile[255];
FILE   *Fp_dft = NULL;
char   dftfile[255];
Top    *top;
int    nentries, npt;
char   line[255];
char   t_reset_label[255];
char   curr_label[255];
char   *token[5];
int    ntok;
char   temp[40];
int    i;
int    format;

int    *rah;
int    nrah;
double delta_t;
int    prev_rah = NOVALUE;
int    curr_rah = NOVALUE;
double dbtime;
int    ltime;
int    ntop;
static int dbug = 0;

    ntop = 0;
    if (!(Fp_dt = fopen(fname,"r")))
    {
        fprintf(stderr,"process_delta_t: can't open %s\n", fname);
        exit(1);
    }

    fread(temp,sizeof(char),20,Fp_dt);
    rewind(Fp_dt);
    for (i=0; i<20; i++)
    {
        if (isascii((int)temp[i]));
        else break;
    }

/* File must be ASCII */

    if (i <20)
    {
        fprintf(stderr,"process_delta_t: non-ASCII file %s\n", fname);
        exit(1);
    }

/* How many entries do we have in the observed delta file ? */

    nentries = 0;
    nrah = 0;
    while (getline(Fp_dt, line))
    {
        ++nentries;
        if (strstr(line, "time reset")) ++nrah;
    }
    rewind(Fp_dt);
if (0) printf("nentries=%d  nrah=%d\n", nentries, nrah);

/* Allocate memory */

    if (!(top = (Top *) malloc((nentries+10)*sizeof(Top))))
    {
        fprintf(stderr, "ERROR: process_delta_t: malloc failed for top\n");
        exit(1);
    }
    if (!(rah = (int *) malloc((nrah+1)*sizeof(int))))
    {
        fprintf(stderr, "ERROR: process_delta_t: malloc failed for rah\n");
        exit(1);
    }

    format = find_file_format(Fp_dt);

    npt = 0;
    nrah = 0;
    while (getline(Fp_dt, line))
    {
        trim(line);

/*============ GET RAH ==============*/

        if (strstr(line, "time reset"))
        {
            strcpy(t_reset_label, strstr(line, " "));

printf("======== %s\n", line);
printf("======== label %s\n", t_reset_label);

            ntok = sparse(line, token, " \t", 5);

/* Convert  ascii string into double Unix time */
            if      (format == 1) str2utime3(token[0], &dbtime);
            else if (format == 4) str2utime1(token[0], &dbtime);
            else                  dbtime = atof(token[0]);
            ltime = (int) dbtime;
/* Discard bad times */
            if (!TIME_OK(ltime)) continue;


/*======= New time reset =======*/
            if ((prev_rah == NOVALUE) || (ltime != prev_rah))
            {
              rah[nrah] = ltime;
if (dbug) printf("process_delta_t: rah %d %d\n", nrah, rah[nrah]);

              if ((nrah > 0) && (rah[nrah] < rah[nrah-1]))
              {
              printf("WARNING: TIME RESET %d IS GOING BACKWARD by %d secs\n",
                  rah[nrah], (rah[nrah-1]-rah[nrah]));
              }
              if (Fp_rah != NULL && curr_rah != NOVALUE)
              {
                  if (ntop != 0)
                  {
printf("==== write rahfile %d ntop %d\n", curr_rah, ntop);
/* format .rah: 898095133 1998.06.17-14:52:13 nnn */
                      time_asc4(temp, (double) curr_rah);
                      fprintf(Fp_rah,"%d %.19s %d\n", curr_rah, temp, ntop);

/* format .dft filename: 1998.06.17-14.52.13.STA.dft */
                      sprintf(dftfile, "%d.%s.dft", curr_rah, Station);

if (0) printf("process_delta_t: opening dft %s\n", dftfile);
                      open_Fwr(dftfile, &Fp_dft);

/* first line in .dft: 1998.06.17-14.52.13.STA.dft */
                      fprintf(Fp_dft,"%s %s\n", temp, curr_label);

/* Run smoothing */
                      smoothTop_(3, top, ntop, Fp_dft);
                      fclose(Fp_dft); Fp_dft = NULL;

                  }
/* Close top file */
                  if (Fp_top != NULL)
                  {
if (dbug) printf("process_delta_t: closing top %s\n", topfile);
                      fclose(Fp_top); Fp_top = NULL;
                      if (ntop == 0) unlink(topfile);
                  }
              }

              sprintf(topfile, "%d.%s.ddt", rah[nrah], Station);
              open_Fwr(topfile, &Fp_top);
if (dbug) printf("process_delta_t: opening top case 1 %s\n", topfile);
              ntop = 0;
              ++nrah;
              curr_rah = ltime;
              strcpy(curr_label, t_reset_label);
            }
/*======= End new time reset =======*/

            prev_rah = ltime;
            continue; 
        }




/*============ GET TOP ==============*/

/* Parse line, 3 words max */
        ntok = sparse(line, token, " \t", 3);

/* Discard unknown entries */
        if (ntok > 1 && token[1][0] == '?') continue;

/* Convert ascii string into double Unix time */
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

/* Fill-up Top structure */
        top[ntop].sec  = dbtime;
        top[ntop].imin = ((int) top[ntop].sec / 60) * 60;
        top[ntop].dt  = delta_t;

/* If not done yet, open rah file: name is build according to */
/* the top reception time. */
        if (Fp_rah == NULL)
        {
            sprintf(rahfile, "%d.%s.rah", (int) dbtime, Station);
            open_Fwr(rahfile, &Fp_rah);
if (dbug) printf("process_delta_t: opening rah %s\n", rahfile);
        }

/* Open top file */

        if (Fp_top == NULL)
        {

/* If rah not found yet, fake it */
            if (nrah == 0)
            {
                rah[nrah] = (int) dbtime;
if (dbug) printf("process_delta_t: rah %d %d\n", nrah, rah[nrah]);
                if ((nrah > 0) && (rah[nrah] < rah[nrah-1]))
                {
                  printf("WARNING: TIME RESET %d IS GOING BACKWARD by %d secs\n",
                      rah[nrah], (rah[nrah-1]-rah[nrah]));
                }
                nrah++;
                curr_rah = (int) dbtime;
                sprintf(curr_label, " time reset (fake)");
            }
            sprintf(topfile, "%d.%s.ddt", rah[nrah-1], Station);
            open_Fwr(topfile, &Fp_top);
if (dbug) printf("process_delta_t: opening top case 2 %s\n", topfile);
            ntop = 0;
        }

        time_asc4(temp, top[ntop].sec);
        fprintf(Fp_top,"%s %+.3f\n", temp, top[ntop].dt);
        ++npt;
        ++ntop;
    }

/* End of delta_t file: */
/*   - write pending info to rahfile */
/*   - close files */

    if (ntop != 0)
    {
printf("==== write rahfile %d ntop %d\n", curr_rah, ntop);
        strftime(temp,40,"%Y.%m.%d-%H:%M:%S",
            gmtime((time_t*)&curr_rah));
        fprintf(Fp_rah,"%d %s %d\n", curr_rah, temp, ntop);

        sprintf(dftfile, "%d.%s.ddft", curr_rah, Station);

if (1) printf("process_delta_t: opening dft %s\n", dftfile);
        open_Fwr(dftfile, &Fp_dft);

/* first line in .dft: 1998.06.17-14.52.13.STA.dft */
        fprintf(Fp_dft,"%s %s\n", temp, curr_label);

/* Run smoothing */
        smoothTop_(3, top, ntop, Fp_dft);
        fclose(Fp_dft); Fp_dft = NULL;
    }


    if (Fp_top != NULL) { fclose(Fp_top); Fp_top = NULL; }
    if (Fp_rah != NULL) { fclose(Fp_rah); Fp_rah = NULL; }
    fclose(Fp_dt); Fp_dt = NULL;

printf("process_delta_t: %d points in ascii top file (format %d)\n",
    npt,format);

if (1) for (i=0; i<npt; i++)
     printf("%d  %d  %.3f  %.3f\n", i, top[i].imin, top[i].sec, top[i].dt);

    *ptop = top;
    return npt;
}


/*=======================================================================*/
static void smoothTop_(hper, top, npt, Fp_dft)
int hper;
Top *top;
int npt;
FILE *Fp_dft;
{
int      i,j,k,l,firstPer,lastPer,nval,imin,nstop;
int      manquant;
double   dtmean,dtmean1,dtavg;
STop     *SmoothTop = NULL;

int speriode;
int smooth = 3;
double seuil1 = 1.00;
double seuil2 = 0.04;
char     date[40];

    speriode = hper * 3600;

/* Check first top */

    if (!TIME_OK(top[0].imin) || !TIME_OK(top[0].sec))
    {
        printf("smoothTop_: unexpected start time: min=%d sec=%d\n",
                (int) top[0].imin, (int) top[0].sec);
        exit(1);
    }

if (0) printf("==== firstPer %d  speriode %d\n", top[0].imin, speriode);

/*
 * lissage avec 1 point toutes les HPERIODE heures (ex 3h)
 * Les tops lisses sont stockes dans SmoothTop
 */
    firstPer = (int) ((double)top[0].imin/speriode);
    lastPer  = (int) ((double)top[npt-1].imin/speriode);

    l = sizeof(STop)*(lastPer-firstPer+2);
    if (!(SmoothTop = (STop *) malloc(l)))
    {
        fprintf(stderr, "ERROR: smoothTop: malloc failed for ");
        fprintf(stderr, "lastPer %d firstPer %d\n", lastPer,firstPer);
        exit(1);
    }
if (0) printf("alloc %d bytes for SmoothTop[]\n", l);

    firstPer *= speriode;
    lastPer  *= speriode;

if (0) fprintf(stderr,"smoothTop_: firstPer %d  lastPer %d n %d per %d\n",
           firstPer, lastPer, npt, speriode);

    if (0 && lastPer == firstPer)
    {
        lastPer  = top[npt-1].imin + 60;
        speriode = (lastPer - firstPer);
    }
    dtavg=0.;
    manquant=0;

if (0) fprintf(stderr,"smoothTop_: firstPer %d  lastPer %d n %d per %d\n",
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
        while(top[i].imin < (imin+speriode) && i<npt)
        {
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
            dtmean /= nval;
if (0) printf("smoothTop_: nval=%d  dtmean=%.3f \n", nval, dtmean);
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
                 fprintf(stdout,"==> reject top %f (>%f sec) ",
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
                 fprintf(stdout,"==> reject top %f (>%f sec) ",
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

    if (nstop == 1)
    {
        time_asc4(date, (double) firstPer);
        fprintf(Fp_dft, "%s %+.4f\n",date,SmoothTop[0].dt);
        time_asc4(date, (double) (firstPer+speriode));
        fprintf(Fp_dft, "%s %+.4f\n",date,SmoothTop[0].dt+0.0001);
    }
    else for (i=0;i<nstop;i++)
    {
        time_asc4(date, (double) SmoothTop[i].imin);
        fprintf(Fp_dft, "%s %+.4f\n",date, SmoothTop[i].dt);
    }

    free(SmoothTop);
}



/*-------------------------------------------------------------------
 *                        malloc_xy
 *------------------------------------------------------------------*/
void malloc_xy(xy, nxy, name)
XY **xy;
int nxy;
char *name;
{
    if (*xy == NULL)
    {
      if (!(*xy = (XY *)malloc(nxy*sizeof(XY))))
      {
          fprintf(stderr,"ERROR: malloc_xy failed (%s) %d data\n",name, nxy);
          exit(1);
      }
      if (dballoc) printf("alloc_pointer %p %s %d data\n", *xy, name, nxy);
      return;
    }
    fprintf(stderr,"ERROR: malloc_xy: (%s) pointer not null\n", name);
    exit(1);
}

/*-------------------------------------------------------------------
 *                        free_xy
 *------------------------------------------------------------------*/
void free_xy(xy, name)
XY *xy;
char *name;
{
    if (xy == NULL)
    {
        fprintf(stderr,"ERROR: free_xy: %s null pointer\n", name);
        exit(1);
    }
    if (dballoc) printf("free_pointer %p %s\n", xy, name);
}


