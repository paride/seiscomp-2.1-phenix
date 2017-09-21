/*=========================================================================
 *    titdt.c
 *    Print corrected time from titan .dft files
 *
 *    input:
 *        station    station code
 *        utime      raw titan time (Unix time, real number)
 *
 *    Contents:
 *        paths_init_()
 *        get_smoothed_dt()
 *        open_dftfile()
 *        readDftFiles()
 *        getClockDritf()
 ==========================================================================*/
#include "titan.h"
#include "proto.h"

typedef struct {
    int    imin;
    double dt;
} TOP;

#ifdef ANSI_C
static void paths_init_   ();
double getClockDritf      (double, char*);
#else
static void paths_init_   ();
double getClockDritf      ();
#endif

extern char netname[];      /* declared in libutil.c */
char   Station[8];
struct option opt;
FILE   *Fp_log;
int    foundDftFiles = -1;
int    verb = 0;          
struct dft_list  *dft_head;
struct dft_list  *dft_tail;

extern Paths  db_paths;



/*===================================================================*/
int main(argc, argv)
int    argc;
char   *argv[];
{
char   *dftfile;
double uTime;
double estim_dt;
double clockDrift;
int    i;

    opt.use_database    = FALSE;
    if (argc < 3)
    {
        printf("Usage:   titdt utime sta [db=network] [-v]\n");
        printf("\n");
        printf("    utime can be either Unix time or a time string.\n");
        printf("    Time string format:\n");
        printf("        from YYYY.MM.DD  to YYYY.MM.DD-hh:mm:ss.mmm\n");
        printf("\n");
        printf("Examples:\n");
        printf("    titdt 912259776.25 XXX\n");
        printf("    titdt 904073024 MLS db=pyren\n");
        printf("    titdt 2000.07.06-23:58:55 ATE db=pyren\n");
        printf("    titdt 2000.06.06 MLS db=pyren\n");
        exit (1);
    }

    sprintf(Station, "%s", "XXX");
    sprintf(Station, "%s", "MLS");
    uTime = 912259776.0;
    uTime = 904073024.0;

/*==== Argument is a time string yyyy.mm.dd-hh.mm.ss ====*/
/*==== Incomplete time string accepted ====*/
/*==== Code borrowed from utime ====*/

    if (strchr(argv[1], ':') ||
        strchr(argv[1], '-') ||
        strchr(argv[1], '.'))
    {
      char    *token[10];
      int     ntok;
      char    str[256];
      char    ttime[40];

        strncpy(ttime, argv[1], strlen(argv[1]));
        ntok = sparse(ttime, token, ":-.", 6);
        if (ntok <= 2)
            goto utime;

        sprintf(ttime, "1970.01.01-00.00.00.000");
        strncpy(ttime, argv[1], strlen(argv[1]));
        sprintf(str, "%s", ttime);

        str2utime1(str, &uTime);
        if (uTime== 0.0)
            exit(1);
    } 
    else
    {
utime:
        uTime = atof(argv[1]);
    }

    sprintf(Station, "%s", argv[2]);

    for (i=2; i<argc; i++)
    {
        if (!strncmp(argv[i],"db=",   3))
        {
            sprintf(netname, "%s", &argv[i][3]);
            if (!strlen(netname))
            {
                fprintf(Fp_log,"\n  Please use db=your_network_name.\n\n");
            }
            ucase(netname);
            opt.use_database = TRUE;
        }
    }

    if (opt.use_database == TRUE) paths_init_();

    readDftFiles();

    if ((estim_dt = get_smoothed_dt(uTime, Station, &dftfile)) != UNKNOWN)
    {
        clockDrift = getClockDritf(uTime, dftfile);
        printf("%.4f %s drift=%.3E\n", estim_dt, dftfile, clockDrift);
    }

    return 0;
}


#define pr(m) fprintf(stderr,"%s", m);


/*===================================================================* 
                    paths_init()
 *===================================================================*/
static void paths_init_()
{
#define MAXTOKEN 4
#define pr(m) fprintf(stderr,"%s", m);
FILE   *Fp;
char    line[LINELEN];
char   *token[MAXTOKEN];
int     ntok;
int     len;

/* This module should be call just once.  */

    db_paths.timedt[0]      = '\0';
    db_paths.timedft[0]     = '\0';
    db_paths.extra_tcorr[0] = '\0';
    db_paths.stations[0]    = '\0';

    if (!(db_paths.cvtit_conf = getenv("CVTIT_CONF")))
    {
        pr("\n\tpaths_init(): ENVIRONMENTAL VARIABLE ");
        pr("\"CVTIT_CONF\" NOT FOUND\n");
        pr("\tExample: setenv CVTIT_CONF ");
        pr("/home/fels/sismnet/cvtit.conf\n\n");
        exit(1);
    }
    if (!(Fp = fopen(db_paths.cvtit_conf,"r")))
    {
        pr("\n\tpaths_init(): CONFIG FILE ");
        pr(db_paths.cvtit_conf);
        pr(" NOT FOUND\n\n");
        exit(1);
    }

    ucase(netname);
    len = strlen(netname);
    while (getline(Fp, line))
    {
        trim(line);
        ntok = sparse(line, token, " \t", MAXTOKEN);
        if (ntok < 2) continue;
        ucase(token[0]);

        if (!strncmp(token[0], netname, strlen(netname)))
        {
            if (!strcmp(&token[0][len+1], "DT"))
            {
                sprintf(db_paths.timedt, "%s", token[1]);
            }
            if (!strcmp(&token[0][len+1], "DFT"))
            {
                sprintf(db_paths.timedft, "%s", token[1]);
            }
            if (!strcmp(&token[0][len+1], "TC"))
            {
                sprintf(db_paths.extra_tcorr, "%s", token[1]);
            }
            if (!strcmp(&token[0][len+1], "STATIONS"))
            {
                sprintf(db_paths.stations, "%s", token[1]);
            }
        }
    }
    if (/* !strlen(db_paths.stations) || */
        !strlen(db_paths.timedt) ||
        !strlen(db_paths.timedft) /* ||
        !strlen(db_paths.extra_tcorr) */)
    {
       pr("\n\tpaths_init(): wrong syntax or missing path \n");
       pr("\tin cvtit config file  ");
       pr(db_paths.cvtit_conf);
       pr("\n");
       pr("\tKey words:\n");
       pr("\t  NETWORK_DT path        or  network_dt path\n");
       pr("\t  NETWORK_DFT path       or  network_dft path\n");
       pr("\t  NETWORK_TC path        or  network_tc path\n");
       pr("\t  NETWORK_STATIONS path  or  network_stations path\n");
       pr("\n");
       exit(1);
    }

if (verb)
{
printf("\n");
printf("  paths_init: timedft       '%s'\n", db_paths.timedft);
printf("  paths_init: extra_tcorr   '%s'\n", db_paths.extra_tcorr);
}
    fclose(Fp);
    return;
}


/*================================================================*
 *    get_smoothed_dt
 *    Return interpolated delta_t as regard of
 *    -  UTC time
 *    -  station name
 *    -  smoothed delta_t .dft ascii file
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

    if (open_dftfile((int) uTime, station, &Fpdft, dftfile) < 1)
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
    return (dt);
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
            return pdft->clock_drift;
        }
    }
    return 0.0;
}

/*======================================================================*/
int open_dftfile(utime, station, Fp, fname)
int utime;
char *station;
FILE **Fp;
char **fname;
{
int    foundDftfile;
struct dft_list *pdft = NULL;

    foundDftfile = FALSE;
    *fname    = NULL;
    *Fp       = NULL;
    if (foundDftFiles > 0) for (pdft=dft_head; pdft!=NULL; pdft=pdft->next)
    {
        if (!strstr(pdft->dft_name, station)) continue;
        if (utime > pdft->beg_systime &&
            utime < pdft->end_systime)
        {
if (0) printf("==== open_dftfile: found %s\n", pdft->dft_name);
            foundDftfile = TRUE;
            break;
        }
    }
    
    if (foundDftfile)
    {
if (0) printf("==== open_dftfile: %s begtime %d %+.4f\n",
          pdft->dft_name, pdft->beg_systime, pdft->beg_dt);

        if (!(*Fp = fopen(pdft->dft_name, "r"))) return 0;
        *fname = pdft->dft_name;
        return 1;
    }
    return 0;
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

        if (verb) printf("  %s\n", pdft->dft_name);
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

    if (verb) printf("  dir %s     found %d dft files\n",
           path, foundDftFiles);
}


