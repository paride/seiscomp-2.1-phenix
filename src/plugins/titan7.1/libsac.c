/*======================================================================*

    libsac.c

    Library for Titan to SAC converter

    Author: J.-F. Fels, OMP, Toulouse

*======================================================================*/
#include "titan.h"
#include "proto.h"
#include "sac.h"
#include "seed.h"


extern outData OutData[NCHAN];
extern struct option opt;
extern int    byteswap;
extern char   Station[8];
extern FILE   *Fp_data[NCHAN][NCOMP];
extern char   fname[NCHAN][NCOMP][PATHLEN];
extern struct StationParm *staparms_head;


/*===================================================================*/
void write_sac_head(chan, comp, startime)
int chan;
int comp;
double startime;
{
struct sac sach;                 /* SAC header structure */
int        nsamp;
double     srate;
static char *CP = "ZNE";
char       cp[2];
char       temp[8+1];            /* for character transfer */
long       ltime;                /* UNIX time */
static struct tm *tms;

    nsamp    = OutData[chan].nsamples;
    srate    = OutData[chan].srate;

    sprintf(cp, "%1.1s", &CP[comp]);

/* initialize SAC Header to all nulls */

    if (!byteswap && opt.sacsun) sach = sac_nullswap;
    else                         sach = sac_null;

/* SAC-required variables */

    sach.delta  = 1 / srate; 
    sach.b      = 0.0;
    sach.e      = ((double) nsamp - 1) / ((double) srate);
    sach.npts   = nsamp;
    sach.iftype = ITIME;               /* known a priori */
    sach.leven  = TRUE;                /* known a priori */
    if (!byteswap && opt.sacsun)
    {
        swap_4byte(&sach.delta);
        swap_4byte(&sach.b);
        swap_4byte(&sach.e);
        swap_4byte(&sach.npts);
        swap_4byte(&sach.iftype);
        swap_4byte(&sach.leven);
    }

/* SAC optional variables */

/* convert double time into SAC time and record it */

    ltime = (long) startime;
    tms = gmtime(&ltime);
    sach.nzyear = tms->tm_year + 1900;
    sach.nzjday = tms->tm_yday + 1;
    sach.nzhour = tms->tm_hour;
    sach.nzmin  = tms->tm_min;
    sach.nzsec  = tms->tm_sec;
    sach.nzmsec = (int) ((startime - (double) ltime) * 1000.0);
/* describe the file written */

    if (0 && opt.verb)
    {
        printf("  %s %5ld samples ", fname[chan][comp], sach.npts);
        printf(" %04ld.%03ld.%02ld:%02ld:%02ld.%03ld\n",
            sach.nzyear,
            sach.nzjday,
            sach.nzhour,
            sach.nzmin,
            sach.nzsec,
            sach.nzmsec);
    }
    if (!byteswap && opt.sacsun)
    {
        swap_4byte(&sach.nzyear);
        swap_4byte(&sach.nzjday);
        swap_4byte(&sach.nzhour);
        swap_4byte(&sach.nzmin);
        swap_4byte(&sach.nzsec);
        swap_4byte(&sach.nzmsec);
    }

/* station and component names */
    sprintf(temp, "%-8.8s", Station);
    strncpy(sach.kstnm, temp, 8);
    if (HasStatDbParms())
    {
        struct StationParm *ps = NULL;


        for (ps=staparms_head; ps!=NULL; ps=ps->next)
           if (startime >= ps->begtime && startime <= ps->endtime) break;
        if (ps == NULL) for (ps=staparms_head; ps!=NULL; ps=ps->next)
           if (ps->begtime == 0 && ps->endtime == 0) break;
        if (ps == NULL)
        {
           fprintf(stderr,
              "\tERROR write_sac_head: can't find station parameters\n");
           fprintf(stderr,"\n");
           exit(1);
        }

        sprintf(temp, "%s %s", cp, ps->sismchan[chan][comp].sensor);
        strncpy(sach.kcmpnm, temp, 8);
        sprintf(temp, "%-8.8s",    ps->sismchan[chan][comp].sensor);
        strncpy(sach.kinst, temp, 8);
    }
    else
    {
        sprintf(temp, "%-8.8s", cp);
        strncpy(sach.kcmpnm, temp, 8);
    }

if (0) printf("==== SAC comp '%.8s'\n", sach.kcmpnm);

/* orentation */
    if (HasStatDbParms() &&
        staparms_head->sismchan[chan][comp].azim != NOVALUE &&
        staparms_head->sismchan[chan][comp].dip  != NOVALUE)
    {
        sach.cmpaz  = staparms_head->sismchan[chan][comp].azim;
        sach.cmpinc = staparms_head->sismchan[chan][comp].dip;
    }
    else { sach.cmpaz =  0.0; sach.cmpinc =  0.0; }
if (0) printf("==== SAC azim=%.4f dip=%.4f\n", sach.cmpaz, sach.cmpinc);

    if (!byteswap && opt.sacsun)
    {
        swap_4byte(&sach.cmpaz);
        swap_4byte(&sach.cmpinc);
    }

/* coordinates */
    if (HasStatDbParms())
    {
        sach.stla = staparms_head->lat;
        sach.stlo = staparms_head->lon;
        sach.stel = staparms_head->elev;
        sach.stdp = staparms_head->depth;
    }
if (0) printf("==== SAC lat=%.4f lon=%.4f elev=%.4f depth=%.4f\n",
        sach.stla, sach.stlo, sach.stel, sach.stdp);

    if (!byteswap && opt.sacsun)
    {
        swap_4byte(&sach.stla);
        swap_4byte(&sach.stlo);
        swap_4byte(&sach.stel);
        swap_4byte(&sach.stdp);    
    }

/* mysterious LLL-set values */
    sach.internal1 = 2.0;
    sach.internal4 = 6;
    sach.internal5 = 0;
    sach.internal6 = 0;
    sach.unused27 = FALSE;
    if (!byteswap && opt.sacsun)
    {
        swap_4byte(&sach.internal1);
        swap_4byte(&sach.internal4);
        swap_4byte(&sach.internal5);
        swap_4byte(&sach.internal6);
        swap_4byte(&sach.unused27);
    }

/* not sure if these are needed, but they might be. */
/* Values might be wrong */
    sach.lpspol = FALSE;
    sach.lcalda = TRUE;
    if (!byteswap && opt.sacsun)
    {
        swap_4byte(&sach.lpspol);
        swap_4byte(&sach.lcalda);
    }

/* write the SAC header */

    if (fwrite(&sach, sizeof(struct sac), 1, Fp_data[chan][comp]) != 1)
    {
        fprintf(stderr, "\tERROR write_sac_head: write failed\n");
        exit(1);
    }

    return;
}

