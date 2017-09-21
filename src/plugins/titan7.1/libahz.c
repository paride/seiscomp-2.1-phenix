/*======================================================================
    libahz.c

    Library for Titan to AH compressed converter

    Author: O. Coutant, LGIT Grenoble, J.-F. Fels, OMP, Toulouse

*======================================================================*/
#include "titan.h"
#include "proto.h"


extern int    inp_data[NCHAN][NCOMP][NINP]; /* Input data array */
extern struct Channel Channel[NCHAN];
extern outData OutData[NCHAN];
extern struct option opt;
extern FILE   *Fp_log;
extern char   Station[8];
extern FILE   *Fp_data[NCHAN][NCOMP];
extern struct StationParm *staparms_head;


int *Fsamp[NCHAN][NCOMP];


/*===================================================================*/
void write_ahz_head(chan, comp, startime)
int chan;
int comp;
double startime;
{
ahhed      ahd;                 /* AH header structure */
int        nsamp;
double     freq;
static char *CP = "ZNE";
char       cp[2];
char       temp[8+1];            /* for character transfer */
long       ltime;                /* UNIX time */
static struct tm *tms;
int        msec;

    nsamp    = OutData[chan].nsamples;
    freq     = OutData[chan].srate;

    sprintf(cp, "%1.1s", &CP[comp]);

    get_null_head(&ahd);

/* AH-required variables */

    ahd.record.type  = INTZ;
    ahd.record.delta  = 1 / freq;
    ahd.record.ndata   = nsamp;

/* AH optional variables */

/* Time correction is stored in extras #15 and #16
 * extra[16]=-extra[15] means no correction applied
 * extra[16]= extra[15] means correction applied
 */
    ahd.extra[15]=startime - OutData[chan].uncorrected_time;
    ahd.extra[16]=-ahd.extra[15];

/* convert start time strings into AH time and record it */

    ltime = (long) startime;
    tms = gmtime(&ltime);
    ahd.record.abstime.yr = tms->tm_year + 1900;
    ahd.record.abstime.day = tms->tm_mday;
    ahd.record.abstime.mo = tms->tm_mon+1;
    ahd.record.abstime.hr = tms->tm_hour;
    ahd.record.abstime.mn  = tms->tm_min;
    msec = (int) ((startime - (double) ltime) * 1000.0);
    ahd.record.abstime.sec  = tms->tm_sec+msec/1000.;

/* station and component names */
    sprintf(temp, "%-.6s", Station);
    strncpy(ahd.station.code, temp, 6);
    sprintf(temp, "%-.6s", cp);
    strncpy(ahd.station.chan, temp, 6);

/* coordinates */
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
             "\tERROR write_ahz_head: can't find station parameters\n");
          fprintf(stderr,"\n");
          exit(1);
       }

       ahd.station.slat = ps->lat;
       ahd.station.slon = ps->lon;
       ahd.station.elev = ps->elev;
    }

/* write the AH header */

    xdr_puthead(&ahd, &((AHFILE*)Fp_data[chan][comp])->xdr);

    return;
}

/*==================================================================*/
void write_ahz_data(chan)
int chan;
{
static short initDone = FALSE;
static int   size[NCHAN][NCOMP];
int i,j,comp;

      if (!initDone)
      {
        memset(Fsamp,0,NCHAN*NCOMP*sizeof(int));
        memset(size,0,NCHAN*NCOMP*sizeof(int));
        initDone = TRUE;
      }

      for (comp=0; comp<Channel[chan].numcomp; comp++)
      {
         if (opt.comp >= 0 && (comp != opt.comp)) continue;

/* Alloc memory */

         if (Fsamp[chan][comp] == NULL)
         {
           size[chan][comp] = Channel[chan].ninp;
           Fsamp[chan][comp] =
             (int*)mem_alloc(sizeof(int)*size[chan][comp],"write_ahz_data");
           j=0;
         }

/* Realloc memory */
         else
         {
           j = size[chan][comp];
           size[chan][comp] += Channel[chan].ninp;
           Fsamp[chan][comp] =
              (int*)realloc(Fsamp[chan][comp],sizeof(int)*size[chan][comp]);
         }

/* Store input data */
         for (i=0;i<Channel[chan].ninp;i++)
            Fsamp[chan][comp][j+i]=inp_data[chan][comp][i];
      }
}
