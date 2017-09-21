/*=========================================================================
 *             gettime.c
 *
 *   Contents:
 *      GetOutputDataTime
 *      get_smoothed_dt
 *      openDftfile
 *      readDftFiles
 *      readXtcorrFile
 *      getClockDritf
 ==========================================================================*/
#include "titan.h"
#include "proto.h"

#ifdef ANSI_C
double getClockDritf(double, char*);
static int openDftfile(int, char*, FILE**, char**);
#else
double getClockDritf();
static int openDftfile();
#endif

typedef struct {
    int    imin;
    double dt;
} TOP;


char    tqf;            /* Time quality factor (SISMALP) */

extern char    Station[8];
extern struct  flags  flags;
extern struct  Channel Channel[];
extern outData OutData[NCHAN];
extern double  SystTime;
extern double  Observ_dt;
extern double  ExtraTcorr;
extern int     NumEstimDeltaTime;
extern int     NumFitEntries;
extern FILE    *Fp_log;
extern struct  dft_list  *dft_head;
extern struct  dft_list  *dft_tail;
extern int     foundDftFiles;
extern struct  xtc_list *xtc_head;
extern struct  xtc_list *xtc_tail;
extern Paths   db_paths;


static TimeDrift *timeDrift;
static int ntimeDrift;



/*==================================================================*
    GetOutputDataTime

    Compute the corrected time of the current output data series.
    This module is called when output data in memory are going to be
    written out to disk, usually in the module close_data_files().
    The time involved her is the start time of output data series in
    memory, which may be be far anterior of the current time.
    This module MUST NOT BE USED to get the current corrected time 
    as regard of the system time.
    The time is corrected in 3 different ways, according to the correction
    mode:
       CORRECT_TOP -> correction by the more recent internal clock delta_t
       CORRECT_DFT -> correction by the estimated delta_t, from a .dft file
       NOCORRECTION -> no correction by the delta_t.

    Note the in all cases, the time is also corrected by the digitizer
    and the FIR filter time delays.

    input:
           action                           time correction mode
           station                          station name
           chan                             chan number
           OutData[chan].uncorrected_time   uncorrected start time of data
           OutData[chan].extra_tcorr;
           OutData[chan].observ_dt;
           Channel[chan].adcdelay           digitizer time delay
           Channel[chan].filtdelay          FIR filter time delay
    output:
           dftfile           name of time drift file used to correct the time
           OutData[chan].estim_dt           estimated time correction
           Channel[chan].start_time         corrected start time of data
           tqf                              time quality factor
    return:
           corrected start time of data series

    Note:
           Observed delta_t was "Observ_dt" in previous version, now
           it is OutData[chan].observ_dt.
           The difference may be several milliseconds.
 *==================================================================*/
double GetOutputDataTime(action, station, chan, dftfile)
int     action;
char    *station;
int     chan;
char    **dftfile;
{
double       uTime;
double       delay;
double       dt;
double       estim_dt;
double       observ_dt;
double       xtcorr;

    *dftfile = NULL;

    uTime  = OutData[chan].uncorrected_time;
    xtcorr = OutData[chan].extra_tcorr;
    observ_dt = OutData[chan].observ_dt;
    delay  = Channel[chan].adcdelay + Channel[chan].filtdelay;

    if (action == NOCORRECTION)
    {
        OutData[chan].estim_dt = (double) UNKNOWN;
        Channel[chan].start_time = uTime - delay;
        tqf = '?';
        return (Channel[chan].start_time);
    }

    if (action == CORRECT_TOP)
    {
        OutData[chan].estim_dt = (double) UNKNOWN;
        if (observ_dt != (double) UNKNOWN) {dt = observ_dt; tqf = '2';}
        else                               {dt = 0.0;       tqf = '?';}
        Channel[chan].start_time = uTime - dt - delay;
        return (Channel[chan].start_time);
    }

    if (action != CORRECT_DFT)
    {
        fprintf(stderr,"ERROR: GetOutputDataTime: unsupported action: %d\n",
                action);
        exit(1);
    }

/* Get estimated delta time */

    estim_dt = get_smoothed_dt(uTime, station, dftfile);
    if (estim_dt == (double) UNKNOWN)
    {
        flags.missing_estim_dt = TRUE;
    }

    if      (estim_dt  != (double) UNKNOWN) {dt = estim_dt;  tqf = '1';}
    else if (observ_dt != (double) UNKNOWN) {dt = observ_dt; tqf = '2';}
    else                                    {dt = 0.0;       tqf = '?';}

/*
    getClockDritf(uTime, *dftfile);
*/

    OutData[chan].estim_dt = estim_dt;
    Channel[chan].start_time = uTime - dt - delay + xtcorr;

if (0)
{
printf("==== GetOutputDataTime: chan %d system_time=%.4f:\n", chan,SystTime);
printf("     corrected_time: %.4f -(%.4f) -(%.4f) - %.4f = %.4f\n",
    OutData[chan].uncorrected_time,
    dt,
    Channel[chan].adcdelay,
    Channel[chan].filtdelay,
    Channel[chan].start_time);
}

    return (Channel[chan].start_time);
}


/*==================================================================*/
double getClockDritf(uTime, dftfile)
double  uTime;
char    *dftfile;
{
struct dft_list *pdft = NULL;


    if (foundDftFiles && dftfile)
        for (pdft=dft_head; pdft!=NULL; pdft=pdft->next)
    {
        if (!strcmp(pdft->dft_name, dftfile))
        {
            printf("++++ getClockDritf: %s drift=%E\n",
                   pdft->dft_name, pdft->clock_drift);
            return pdft->clock_drift;
        }
    }
    return 0.0;
}

/*==================================================================*
    WARNING: This returns the estimated_dt (smoothed) or 
             the most recent Observ_dt.
             The most recent Observ_dt is not always what we want.
 *==================================================================*/
double GetEstimDt(uTime, station, dftfile)
double  uTime;
char    *station;
char   **dftfile;
{
double  estim_dt;

/* Get estimated delta time */

    estim_dt = get_smoothed_dt(uTime, station, dftfile);

if (0)
{
char str[40];
    time_asc4(str, uTime);
    printf("==== GetEstimDt uTime=%s dt=%.4f %s\n", str,estim_dt,*dftfile);
}

    return estim_dt;
}


/*================================================================*
 *    get_smoothed_dt
 *    Return interpolated delta_t as regard of
 *    -  UTC time
 *    -  station name
 *    -  smoothed delta_t .dft ascii file
 *    The module searches the a matching .dft file from a list
 *    (the filename will be returned).
 *    File format: 1997.09.26-16:30:00.000 -0.0715
 *================================================================*/
double get_smoothed_dt(uTime, station, dftfile)
double uTime;
char   *station;
char   **dftfile;
{
FILE    *Fpdft;
TOP     top1, top2;
int     ltime;
double  dt, dbtime;
char    line[255];
char    *token[4];
int     ntok;

    ltime = uTime;
    trim(station);

    if (openDftfile((int) uTime, station, &Fpdft, dftfile) < 1)
    {
        return ((double) UNKNOWN);
    }

/* On cherche les tops lisses entourant l'heure non corrigee */
    top1.imin = 0;
    top1.dt  = 0.0;

/* First line */
    ntok = 0;
    while (getline(Fpdft, line))
    {
        ntok = sparse(line, token, " \t", 4);
        if (ntok == 2 && strlen(token[0]) >= 21)
        {
            str2utime1(token[0], &dbtime);
            if (TIME_OK(dbtime))
                break;
        }
    }

/*============================================
 * First line *
    getline(Fpdft, line);
 * Skip time reset info in .dft file *
    if (strstr(line, "time reset"))
        getline(Fpdft, line);
    trim(line);

    ntok = sparse(line, token, " \t", 2);
    str2utime1(token[0], &dbtime);
=============================================*/

    top2.imin = dbtime;
    top2.dt   = atof(token[1]);

/* Following lines */
    while (fgets(line, 100, Fpdft) && top2.imin < ltime)
    {
        if (line[0] == '#' || strlen(line) < 3) continue;
        top1.imin = top2.imin;
        top1.dt   = top2.dt;
        sparse(line, token, " \t", 2);
        str2utime1(token[0], &dbtime);
        top2.imin = dbtime;
        top2.dt   = atof(token[1]);
    }

if (0) printf("== utime=%d top1=%d %.3f  top2=%d %.3f\n",
           ltime, top1.imin,top1.dt,top2.imin,top2.dt);

/* If at end-of-file, check that utime is not too far beyond last min.*/
    if (feof(Fpdft))
    {
      if (ltime > (top2.imin+1000))
      {
        fprintf(Fp_log,"  ==========================================\n");
        fprintf(Fp_log,"  WARNING: GetTime: Unknown time correction:\n");
        fprintf(Fp_log,"  time outside of .dft limits\n");
        fprintf(Fp_log,"  time: %d   Last .dft time: %d\n", ltime, top2.imin);
        fprintf(Fp_log,"  ==========================================\n");
        return ((double) UNKNOWN);
      }
    }
    fclose(Fpdft);

    if (ltime > top2.imin)
        dt = top2.dt;
    else 
        dt = (ltime-top1.imin)*(top2.dt-top1.dt)/(top2.imin-top1.imin)+top1.dt;
/*
     printf("TEST TEST -> %.3f  estim_dt=%.3f observ_dt=%.3f (top=%.3f)\n",
         uTime, dt, Observ_dt, top1.dt);
*/
    return (dt);
}


/*======================================================================*
    Find a matching dft file and open it
 *======================================================================*/
static int openDftfile(utime, station, Fp, fname)
int utime;
char *station;
FILE **Fp;
char **fname;
{
int    foundDftfile;
struct dft_list *pdft = NULL;
int    dbug = 0;

    trim(station);
    foundDftfile = FALSE;
    *fname    = NULL;
    *Fp       = NULL;
if (dbug) printf("==== openDftfile: utime=%d sta=%s foundDftFiles=%d\n",
    utime, station, foundDftFiles);

/*
 * Loop thru all dft structures
 */
    if (foundDftFiles > 0) for (pdft=dft_head; pdft!=NULL; pdft=pdft->next)
    {

    /*
     * dft filename must match the station name
     */
        if (!strstr(pdft->dft_name, station)) continue;

if (dbug) printf("==== openDftfile: %s %s\n", pdft->dft_name, station);
if (dbug) printf("==== openDftfile: utime %d  %d %d\n",
               utime, pdft->beg_systime, pdft->end_systime);

    /*
     * utime must fall inside the beg-end dft time segemnt
     */
        if (utime > pdft->beg_systime &&
            utime < pdft->end_systime)
        {

if (dbug) printf("==== openDftfile: %s matches\n", pdft->dft_name);

            foundDftfile = TRUE;
            break;
        }
    }
    
/*
 * if dft file found, open it, return the file pointer and its pathname
 */
    if (foundDftfile)
    {

if (dbug) printf("==== openDftfile: %s begtime %d %+.4f\n",
          pdft->dft_name, pdft->beg_systime, pdft->beg_dt);

        if (!(*Fp = fopen(pdft->dft_name, "r"))) return 0;
        *fname = pdft->dft_name;
        return 1;
    }
    return 0;
}



/*======================================================================*
    Read .dft file and store samples in timeDrift[] array.
    Number of samples is ntimeDrift.
    Both variables timeDrift and ntimeDrift are static.
    Memory for array timeDrift is allocated here. Memory is freed with
    module freeTimeDrift();
 *======================================================================*/
int loadDftFile(utime, station)
double utime;
char *station;
{
FILE *Fp;
char    *dftfile;
char    line[255];
char    *token[4];
int     ntok;
double  dbtime;
int i;
int n;
double t1;
double drift;

/*
printf("++++ entering loadDftFile: %s %.3f\n", station, utime);
*/
    dftfile = NULL;
    if (openDftfile((int) utime, station, &Fp, &dftfile) < 1)
    {
        return 0;
    }

    if (!(Fp = fopen(dftfile, "r")))
    {
        return 0;
    }

/*
 * Read dftfile, first pass: find out number of entries.
 */
    n = 0;
    ntok = 0;
    while (getline(Fp, line))
    {
        trim(line);
        ntok = sparse(line, token, " \t", 4);
        if (ntok == 2 && strlen(token[0]) >= 21) ++n;
    }
    if (n == 0)
    {
        return 0;
    }

    ntimeDrift = n;

/*
printf("++++ loadDftFile: %d entries in %s\n", ntimeDrift, dftfile);
*/
/*
 * Alloc memory for TimeDrift structures
 */
    timeDrift = (TimeDrift *)
        mem_alloc(sizeof(TimeDrift) * (ntimeDrift+2), "loadDftFile");


/*
 * Read dftfile, first pass: save dft entries.
 */
    rewind(Fp);
    n = 0;
    while (getline(Fp, line))
    {
        trim(line);
        ntok = sparse(line, token, " \t", 4);
        if (ntok == 2 && strlen(token[0]) >= 21)
        {
            str2utime1(token[0], &dbtime);
            timeDrift[n].t = dbtime;
            timeDrift[n].dft = atof(token[1]);
            if (!TIME_OK(dbtime)) continue;
            ++n;
        }
    }
    ntimeDrift = n;

    if (n <= 1) return 0;

/*
 * Extrapolate dft by one sample distant from 3600 sec from last
 */


/* Compute drift mean value */
    t1 = timeDrift[ntimeDrift-1].t - 3600.0 * 4.0;
    for (i=0; i<ntimeDrift; i++)
    {
        if (timeDrift[i].t >= t1) break;
    }
    drift = (timeDrift[ntimeDrift-1].dft - timeDrift[i].dft) /
            (timeDrift[ntimeDrift-1].t   - timeDrift[i].t);

/* Compute extrapolated dft sample */
    timeDrift[ntimeDrift].t   = timeDrift[ntimeDrift-1].t + 3600.0;
    timeDrift[ntimeDrift].dft = timeDrift[ntimeDrift-1].dft + 3600.0 * drift;

    ++ntimeDrift;

if (0)
{
    for (i=1; i<ntimeDrift; i++)
printf("==== %d %.1f %.4f delta=%.1f\n",
        i-1, timeDrift[i-1].t, timeDrift[i-1].dft,
        timeDrift[i].t - timeDrift[i-1].t);

printf("==== first %.1f %.4f\n", 
        timeDrift[0].t, timeDrift[0].dft);
printf("==== last  %.1f %.4f\n", 
        timeDrift[ntimeDrift-1].t, timeDrift[ntimeDrift-1].dft);
}

    return ntimeDrift;
}

/*======================================================================*/
void freeTimeDrift()
{
    free(timeDrift);
}


typedef struct {
    double t;
    double dft;
} TOFS;

/*======================================================================*/
double GetEstimDt_(utime)
double  utime;
{
int i;
TOFS t1, t2;
double  dt;

    if (ntimeDrift == 0)
    {
        return ((double) UNKNOWN);
    }

    t1.t   = t1.dft = 0;
    t2.t   = timeDrift[0].t;
    t2.dft = timeDrift[0].dft;

    for (i=1; i<ntimeDrift; i++)
    {
        t1.t   = t2.t;
        t1.dft = t2.dft;
        t2.t   = timeDrift[i].t;
        t2.dft = timeDrift[i].dft;
        if (t2.t >= utime) break;
    }
    if (utime > t2.t)
        dt = t2.dft;
    else
        dt = (utime-t1.t) * (t2.dft-t1.dft) / (t2.t-t1.t) + t1.dft;

if (0)
    printf("GetEstimDt_: utime=%.3f i=%d dt=%.4f\n", utime, i, dt);

    return dt;
}

/*======================================================================*
    readDftFiles()
    Read all .dft files available and savetime drift parameters
    in a linked list.
    Parameters stored in the linked list are:
        - dft filename
        - first system time and delta_t
        - first system time and delta_t
        - overall internal clock drift
 *======================================================================*/
void readDftFiles()
{
FILE    *Fp;
char    path[PATHLEN];
char    fname[PATHLEN];
char    dftfile[PATHLEN];
int     nfile;
char    suffix[20];
char    line[255];
char    line2[255];
char    *token[4];
int     ntok;
double  dbtime;
struct  dft_list *pdft;

if (0) printf("==== readDftFiles: db_paths.timedft: '%s'\n",
              db_paths.timedft);

    sprintf(path, ".");
    sprintf(suffix, ".dft");
    fname[0] = '\0';

/*
 * Case where the dft files are in the working directory
 */
    if (db_paths.timedft == NULL || !strlen(db_paths.timedft))
    {
        sprintf(path, ".");
    }

/*
 * Case where the dft files are in the cvtit database
 */
    else
    {
        if (!isdir(db_paths.timedft))
        {
            fprintf(stderr,"\n\n");
            fprintf(stderr,"  ERROR: readDftFiles: can't find dir '%s'\n",
                db_paths.timedft);
            exit(1);
        }
        if (!strlen(Station))
        {
            fprintf(stderr,"\n\n");
            fprintf(stderr,"  ERROR: readDftFiles: missing station name.");
            fprintf(stderr," Can't access dir '%s/STA'\n", db_paths.timedft);
            exit(1);
        }
        sprintf(path, "%s/%s", db_paths.timedft, Station);
        if (!isdir(path))
        {
            fprintf(stderr,"\n\n");
            fprintf(stderr,"  ERROR: readDftFiles: can't find dir %s\n",
                path);
            exit(1);
        }
    }

if (0) printf("==== readDftFiles: reading dir %s\n", path);

/*
 * Loop thru all dft files in the dir pointed to by "path"
 */
    nfile = 0;
    while (read_dir(path, fname, suffix))
    {

/* Open dft file */
        sprintf(dftfile,"%s/%s", path, fname);
        if (!(Fp = fopen(dftfile, "r"))) continue;

/* Get first valid t and dt */
/* Initial time reset is not anymore used */
        ntok = 0;
        while (getline(Fp, line))
        {
            ntok = sparse(line, token, " \t", 4);
            if (ntok == 2 && strlen(token[0]) >= 21)
            {
                str2utime1(token[0], &dbtime);
                if (TIME_OK(dbtime))
                    break;
            }
        }

/* If file empty or unvalid, read next file */
        if (!ntok)
        {
            fclose(Fp); Fp = NULL;
            continue;
        }
        if (ntok < 2 || strlen(token[0]) < 21)
        {
            goto end;
        }

        pdft = (struct dft_list *)
               mem_alloc(sizeof(struct dft_list), "readDftFiles");
        append_linklist_element(pdft, dft_head, dft_tail);
        sprintf(pdft->dft_name, "%s", dftfile);

        str2utime1(token[0], &dbtime);
        pdft->beg_systime = (int) dbtime;
        pdft->beg_dt      = atof(token[1]);

/* Get last valid t and dt */
/* line2 is used to deal with last lines with leading '#' */
        while(getline(Fp, line2))
        {
            sprintf(line, "%s", line2);
        }
        ntok = sparse(line, token, " \t", 3);
        if (ntok < 2 || strlen(token[0]) < 21)
        {
            goto end;
        }
        str2utime1(token[0], &dbtime);
        pdft->end_systime = (int) dbtime;
        pdft->end_dt      = atof(token[1]);
        
/* Compute overall clock drift */

        if ((pdft->end_systime - pdft->beg_systime) != 0.0)
        {
            pdft->clock_drift = (pdft->end_dt - pdft->beg_dt) /
                   (pdft->end_systime - pdft->beg_systime);
        }

if (0)
{
        printf("==== readDftFiles: %s dt1=%E dt2=%E\n",
            pdft->dft_name, pdft->beg_dt, pdft->end_dt);
        printf("                   drift=%E\n",
            pdft->clock_drift);
        printf("==== readDftFiles: %s \n", pdft->dft_name);
        printf("                     time_reset=%d beg=%d end=%d\n",
            pdft->time_reset, pdft->beg_systime, pdft->end_systime);
}
end:
        fclose(Fp); Fp = NULL;
        ++nfile;
    }
    if (nfile > 0) foundDftFiles = nfile;
    else           foundDftFiles = 0;

    printf("  readDftFiles: dir %s     found %d dft files\n",
           path, foundDftFiles);
}



/*======================================================================*/
void readXtcorrFile()
{
FILE    *Fp_xtcorr = NULL;
char    xtcorr_fname[PATHLEN];
char    line[255];
char    *token[4];
int     ntok;
struct  dft_list *pdft = NULL;
struct  xtc_list *pxtc = NULL;

    if (foundDftFiles <= 0)
        return;

    if (!strlen(db_paths.extra_tcorr))
    {
        printf("  readXtcorrFile: no extra correction file for sta %s\n",
                Station);
        return;
    }
    sprintf(xtcorr_fname, "%s/%s/extra_tcorr", db_paths.extra_tcorr,Station);
/*
printf("==== readXtcorrFile: opening %s\n", xtcorr_fname);
*/
    if (!(Fp_xtcorr = fopen(xtcorr_fname, "r")))
    {
        printf("  readXtcorrFile: no extra correction file for sta %s\n",
                Station);
        return;
    }

    while(getline(Fp_xtcorr, line))
    {
        trim(line);
        ntok = sparse(line, token, " \t", 2);
        if (ntok < 2) continue;

        for (pdft=dft_head; pdft!=NULL; pdft=pdft->next)
        {
/*
printf("==== readXtcorrFile: %s tok %s\n", pdft->dft_name, token[0]);
*/

            if (strstr(pdft->dft_name, token[0]))
            {
               pxtc = (struct xtc_list *)
                      mem_alloc(sizeof(struct xtc_list), "readXtcorrFile");
               append_linklist_element(pxtc, xtc_head, xtc_tail);

               sprintf(pxtc->dft_name, "%s", token[0]);
               pxtc->tcorr = atof(token[1]);

               printf("  readXtcorrFile: found %s\n", pdft->dft_name);
               printf("                  extra time correction: %.4f secs\n",
                       pxtc->tcorr);
            }
        }
    }
    fclose(Fp_xtcorr); Fp_xtcorr = NULL;
    return;
}

