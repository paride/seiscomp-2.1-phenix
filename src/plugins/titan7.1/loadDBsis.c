/*======================================================================
 *=====================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "seed.h"
#include "proto.h"
#include "proto_seed.h"


#define MAXADC     20
#define MAXSENSOR  40
#define MAXAMPLI   40
#define MAXCHAN    150
#define MAXCASCLEN 20

#define MAXTOK    50
#define DELIM1 " \t\n"
#define DELIM2 " :=\t\n"
#define MAXTOK3   25
#define ANY_FIR   77


#ifdef ANSI_C
void   print_filter           (FILE*, struct filter*, int);

static int  LoadDBsisResponses_sub (char*);
static int  check_kword       (char*, char**, int);
static int  read_staname      (char**, int, struct station*);
static int  read_staloc       (char**, int, struct loc*);
static int  read_sensor       (char**, int, struct sensor**, int*);
static int  read_sensor_sub   (char*, struct sensor*);
static int  read_ampli        (char**, int, struct ampli**, int*);
static int  read_ampli_sub    (char*, struct ampli*);
static int  read_adc          (char**, int, struct adc**, int*);
static int  read_adc_sub      (char*, struct adc*);
static int  read_dig_filter   (char**, int, struct filt_cascade**);
static int  read_channel      (char**, int, struct station*);
static int  readPZ            (char*, FILE*, struct filter*);
static int  readFirCoef       (char*, struct firCoef**, int, int);
static int  rd_coef           (FILE*, char*, double*, int);
static int  check_roots       (double root[], int, int, char*);
static double asc2dbtime      (char*);
static void sortStations      (struct station *);
static void check_seed_chan_name (char*);

#else
void   print_filter           ();

static int  LoadDBsisResponses_sub ();
static int  check_kword       ();
static int  read_staname      ();
static int  read_staloc       ();
static int  read_sensor       ();
static int  read_sensor_sub   ();
static int  read_ampli        ();
static int  read_ampli_sub    ();
static int  read_adc          ();
static int  read_adc_sub      ();
static int  read_dig_filter   ();
static int  read_channel      ();
static int  readPZ            ();
static int  readFirCoef       ();
static int  rd_coef           ();
static int  check_roots       ();
static double asc2dbtime      ();
static void sortStations      ();
static void check_seed_chan_name ();
#endif


extern FILE  *Fp_err;
extern char  *PZdir;      /* PZ Response directory name (env var "PZ") */
extern int   dbug;
extern struct station *stalist_head;
extern struct station *stalist_tail;

static int   lineno = 0;
static char  *token[MAXTOK];
static int   ntokens;

static struct filt_cascade **DigCascade; /* array of filter cascades */
static int    nDigCascade;               /* number of filter cascades */
static  int FileOffset = 0;

struct dates {
   double  beg;
   double  end;
}; 
static struct dates Dates[MAXCHAN];
static int nDates;

extern char Network[3];

/*=====================================================================*/
static int sort_strings(a, b) char *a, *b; { return (strcmp(a,b)); }



/*======================================================================
 *=====================================================================*/
int LoadDBsisResponses(resplist, nfiles, net)
char   **resplist;
int    nfiles;
char   *net;
{
int i;
int retValue;

    for (i=0; i<nfiles; i++)
    {
       FileOffset = 0;
       while (1)
       {
          if ((retValue = LoadDBsisResponses_sub(resplist[i])) != TRUE)
              break;
       }
       if (retValue != FALSE)
          return -1;
    }
    return FALSE;
}


/*======================================================================
 *=====================================================================*/
static int LoadDBsisResponses_sub(staRespFname)
char *staRespFname;
{
#define saveln lineno_sav = lineno;
#define restln lineno = lineno_sav;
#define BEGIN_STATION    0
#define BEGIN_SENSOR     1
#define END_SENSOR       2
#define BEGIN_AMPLIFIER  3
#define END_AMPLIFIER    4
#define BEGIN_DIGITIZER  5
#define END_DIGITIZER    6
#define BEGIN_DECIMATION 7
#define END_DECIMATION   8
#define BEGIN_CHANNEL    9
#define END_CHANNEL      10
#define END_STATION      11
#define NKWORD1          12
static char *keyword[NKWORD1] =
{
  "BEGIN_STATION",
  "BEGIN_SENSOR",
  "END_SENSOR",
  "BEGIN_AMPLIFIER",
  "END_AMPLIFIER",
  "BEGIN_DIGITIZER",
  "END_DIGITIZER",
  "BEGIN_DECIMATION",
  "END_DECIMATION",
  "BEGIN_CHANNEL",
  "END_CHANNEL",
  "END_STATION"
};
int     found_keyw[NKWORD1];

int     eofFileOffset;
FILE    *Fp;
char    line[255];
char    line_cp[255];
int     lineno_sav;
int     retValue;
int     err;
int     i, j, k, n;
int     adc_nstage;
static struct station *sta;
struct filt_cascade   *casc;
struct channel        *chan;

    Fp_err = stdout;
    nDigCascade = 0;
    for (i=0; i<NKWORD1; i++) found_keyw[i] = FALSE;
    retValue = 0;

    if (!(Fp = fopen(staRespFname, "r")))
    {
        fprintf(stderr,"ERROR: LoadDBsisResponses_sub: can't open '%s'\n",
                staRespFname);
        exit(1);
    }

if (dbug) printf("LoadDBsisResponses_sub: %s fileoffset %d\n",
               staRespFname, FileOffset);

    fseek(Fp, (long) FileOffset, SEEK_SET);

    sta = (struct station *) mem(sizeof(struct station));

    sprintf(sta->network, "%.2s", Network);

/*  Set pointers  */

    sta->sensor  = (struct sensor  **)
                    mem(sizeof(struct sensor  *) * MAXSENSOR);
    sta->ampli   = (struct ampli   **)
                    mem(sizeof(struct ampli   *) * MAXAMPLI);
    sta->adc     = (struct adc     **)
                    mem(sizeof(struct adc     *) * MAXADC);
    sta->channel = (struct channel **)
                    mem(sizeof(struct channel *) * MAXCHAN);
    DigCascade   = (struct filt_cascade **)
                    mem(sizeof(struct filt_cascade *) * MAXCASCLEN);

/*  Read in database  */

    while (1)
    {
       if ((err = getLine(Fp, line, 250, '#', &lineno) == 1)) break;
       sprintf(line_cp, "%s", line);
       ntokens = sparse(line_cp, token, DELIM2, MAXTOK);
       ucase(token[0]);


       if (check_kword(token[0], keyword, NKWORD1) == FALSE)
          return -1;

       if (!strcmp(token[0], "BEGIN_STATION"))
       {
if (dbug) printf("%s\n", token[0]);
          found_keyw[BEGIN_STATION] = TRUE;
          if ((err = getLine(Fp, line, 250, '#', &lineno) != 0))
          {
             break;
          }
          sprintf(line_cp, "%s", line);
          ntokens = sparse(line_cp, token, DELIM1, MAXTOK);

          if (read_staname(token, ntokens, sta) == FALSE)
              return -1;

if (dbug) printf("\n==== Reading station %s (line %d)\n", sta->name, lineno);

          if (read_staloc(token, ntokens, &sta->loc) == FALSE)
              return -1;
       }


       else if (!strcmp(token[0], "BEGIN_SENSOR"))
       {
if (dbug) printf("%s\n", token[0]);
          if (found_keyw[BEGIN_STATION] == FALSE) goto error1;
          found_keyw[BEGIN_SENSOR] = TRUE;
          while (1)
          {
             if ((err = getLine(Fp, line, 250, '#', &lineno)) != 0)
             {
                 fprintf(stderr,
                   "ERROR: LoadDBsisResponses_sub: error get line in '%s'\n",
                   staRespFname);
                 return -1;
             }
             sprintf(line_cp, "%s", line);
             ntokens = sparse(line_cp, token, DELIM1, MAXTOK);
             ucase(token[0]);

             if (!strcmp(token[0], "END_SENSOR"))
             {
if (dbug) printf("%s\n", token[0]);
                found_keyw[END_SENSOR] = TRUE;
                break;
             }

             saveln
             retValue =
                 read_sensor(token, ntokens, sta->sensor, &sta->nsensor);
             restln

             if (retValue == FALSE) return -1;

          }
       }


       else if (!strcmp(token[0], "BEGIN_AMPLIFIER"))
       {
if (dbug) printf("%s\n", token[0]);
          found_keyw[BEGIN_AMPLIFIER] = TRUE;
          while (1)
          {
             if ((err = getLine(Fp, line, 250, '#', &lineno)) != 0)
             {
                fprintf(stderr,
                   "ERROR: LoadDBsisResponses_sub: error get line in '%s'\n",
                   staRespFname);
                return -1;
             }
             sprintf(line_cp, "%s", line);
             ntokens = sparse(line_cp, token, DELIM1, MAXTOK);
             ucase(token[0]);

             if (!strcmp(token[0], "END_AMPLIFIER"))
             {
if (dbug) printf("%s\n", token[0]);
                found_keyw[END_AMPLIFIER] = TRUE;
                break;
             }

             saveln
             retValue =
                 read_ampli(token, ntokens, sta->ampli, &sta->nampli);
             restln

             if (retValue == FALSE) return -1;

          }
       }


       else if (!strcmp(token[0], "BEGIN_DIGITIZER"))
       {
if (dbug) printf("%s\n", token[0]);
          found_keyw[BEGIN_DIGITIZER] = TRUE;
          while (1)
          {
             if ((err = getLine(Fp, line, 250, '#', &lineno)) != 0)
             {
                fprintf(stderr,
                    "ERROR: LoadDBsisResponses_sub: error get line in '%s'\n",
                    staRespFname);
                return -1;
             }
             sprintf(line_cp, "%s", line);
             ntokens = sparse(line_cp, token, DELIM1, MAXTOK);
             ucase(token[0]);

             if (!strcmp(token[0], "END_DIGITIZER"))
             {
if (dbug) printf("%s\n", token[0]);
                found_keyw[END_DIGITIZER] = TRUE;
                break;
             }

             saveln
             retValue = read_adc(token, ntokens, sta->adc, &sta->nadc);
             restln

             if (retValue == FALSE) return -1;
          }
       }


       else if (!strcmp(token[0], "BEGIN_DECIMATION"))
       {
if (dbug) printf("%s\n", token[0]);
          found_keyw[BEGIN_DECIMATION] = TRUE;
          while (1)
          {
             if ((err = getLine(Fp, line, 250, '#', &lineno)) != 0)
             {
                fprintf(stderr,
                   "ERROR: LoadDBsisResponses_sub: error get line in '%s'\n",
                   staRespFname);
                return -1;
             }
             sprintf(line_cp, "%s", line);
             ntokens = sparse(line_cp, token, DELIM1, MAXTOK);
             ucase(token[0]);

             if (!strcmp(token[0], "END_DECIMATION"))
             {
if (dbug) printf("%s\n", token[0]);
                found_keyw[END_DECIMATION] = TRUE;
                break;
             }

             saveln
             retValue =
                 read_dig_filter(token, ntokens, &DigCascade[nDigCascade]);
             restln

             if (retValue == FALSE || (&DigCascade[nDigCascade] == NULL))
             {
                return -1;
             }
             if (++nDigCascade >= MAXCASCLEN)
             {
                fprintf(stderr,
                     "ERROR: LoadDBsisResponses_sub: too many dig cascade\n");
                return -1;
             }
          }
       }
       if (!strcmp(token[0], "BEGIN_CHANNEL"))
       {
if (dbug) printf("%s\n", token[0]);
          found_keyw[BEGIN_CHANNEL] = TRUE;
          while (1)
          {
             if ((err = getLine(Fp, line, 250, '#', &lineno)) != 0)
             {
               fprintf(stderr,
                  "ERROR: LoadDBsisResponses_sub: error get line in '%s'\n",
                  staRespFname);
                return -1;
             }
             sprintf(line_cp, "%s", line);
             ntokens = sparse(line_cp, token, DELIM1, MAXTOK);
             ucase(token[0]);

             if (!strcmp(token[0], "END_CHANNEL"))
             {
if (dbug) printf("%s\n", token[0]);
                found_keyw[END_CHANNEL] = TRUE;
                break;
             }

             saveln
             retValue = read_channel(token, ntokens, sta);
             restln

             if (retValue == FALSE) return -1;
          }
       }


       if (!strcmp(token[0], "END_STATION"))
       {
if (dbug) printf("%s\n", token[0]);
          if (found_keyw[BEGIN_STATION] == TRUE)
          {
             found_keyw[END_STATION] = TRUE;
             break;
          }
          else
          {
             goto error1;
          }
       }
    }  /* end while (1) */

/*
 * End station; Check and save current file offset
 */
    FileOffset = ftell(Fp);
    fseek(Fp, 0, SEEK_END);
    eofFileOffset = ftell(Fp);
    fclose(Fp);

    if ((eofFileOffset - FileOffset) < 100) retValue = FALSE;
    else                                    retValue = TRUE;

    if (found_keyw[BEGIN_STATION] == FALSE) goto error1;


if (0)
{
    struct adc *adc;

    printf("    station: %s nadc=%d\n", sta->name, sta->nadc);
    for (i=0; i<sta->nadc; i++)
    {
        adc = sta->adc[i];
        printf("        %s %s  nstage=%d\n",
                 adc->label,
                 adc->filt_cascade->filename,
                 adc->filt_cascade->nstage);
    }
}

if (0)
{
    struct dig_filt *temp = NULL;

    for (i=0; i<nDigCascade; i++)
    {
       printf("    dig_cascade: %s %s nstage=%d\n",
                DigCascade[i]->label,
                DigCascade[i]->filename,
                DigCascade[i]->nstage);

       if (0) for (j=0; j<DigCascade[i]->nstage; j++)
       {
          temp = DigCascade[j]->dig_filt;
          printf("              %s decim=%d ncoef=%d\n",
                 dbdecode(temp->filter->type),
                 temp->filter->decim,
                 temp->filter->ncoef);
        }
    }
}

if (0)
{
     printf("    station: %s nsensor=%d nampli=%d nadc=%d nchan=%d\n",
            sta->name, sta->nsensor, sta->nampli, sta->nadc, sta->nchannel);

     for (i=0; i<sta->nsensor; i++)
          printf("             sensor=%s: file=%s\n",
             sta->sensor[i]->label,
             sta->sensor[i]->sname);
}


/*
 *  Assign sensors to channels
 *  ==========================
 */
    for (i=0; i<sta->nchannel; i++)
    {
       chan = sta->channel[i];
       for (j=0; j<sta->nsensor; j++)
       {
          if (!strcmp(sta->sensor[j]->label, chan->sensor_label))
          {
              chan->sensor = sta->sensor[j];
              break;
          }
       }
       /* check if exist */
       if (chan->sensor == NULL)
       {
          fprintf(stderr,"ERROR: LoadDBsisResponses_sub: unknown SENSOR ");
          fprintf(stderr,"'%s' in channel num %d\n", chan->sensor_label, i);
          return -1;
       }
    }

/*
 *  Assign amplis to channels
 *  =========================
 */
    if (sta->nampli == 0)
    {
       for (i=0; i<sta->nchannel; i++)
       {
          chan = sta->channel[i];
          if (strcmp(chan->ampli_label, "NONE"))    
          {   
             fprintf(stderr, "ERROR: LoadDBsisResponses_sub: ");
             fprintf(stderr, "chan %d: found undefined ampli-filter %s\n",
                     i, chan->ampli_label);
             return -1;
          }
       }
    }
    else for (j=0; j<sta->nampli; j++)
    {
       for (i=0; i<sta->nchannel; i++)
       {
          chan = sta->channel[i];
          chan->ampli = NULL;
          if (!strcmp(chan->ampli_label, "NONE"))
          {
             continue;
          }
          for (k=0; k<sta->nampli; k++)
          {
             if (!strcmp(sta->ampli[k]->label, chan->ampli_label))
                 break;
          }
          if (k == sta->nampli)
          {
             fprintf(stderr, "ERROR: LoadDBsisResponses_sub: ");
             fprintf(stderr, "chan %d: found undefined ampli-filter %s\n",
                     i, chan->ampli_label);
             return -1;
          }
          if (!strcmp(sta->ampli[j]->label, chan->ampli_label))
          {
             if (sta->ampli[j] == NULL)
             {
                fprintf(stderr, "ERROR: LoadDBsisResponses_sub: empty ");
                fprintf(stderr, "ampli-filter '%s' in channel num %d\n",
                        chan->ampli_label, i);
                return -1;
             }
             chan->ampli = sta->ampli[j];
          }
       }
    }

/*
 * Assign digitizers to channels
 * =============================
 */
    if (sta->nadc == 0)
    {
       for (i=0; i<sta->nchannel; i++)
       {
          chan = sta->channel[i];
          if (strcmp(chan->adc_label, "NONE"))    
          {   
             fprintf(stderr, "ERROR: LoadDBsisResponses_sub: ");
             fprintf(stderr, "chan %d: found undefined digitizer %s\n",
                     i, chan->adc_label);
             return -1;
          }
       }
    }

    else for (j=0; j<sta->nadc; j++)
    {
       for (i=0; i<sta->nchannel; i++)
       {
          chan = sta->channel[i];
          if (!strcmp(chan->adc_label, "NONE"))
          {
             continue;
          }
          for (k=0; k<sta->nadc; k++)
          {
             if (!strcmp(sta->adc[k]->label, chan->adc_label))
                 break;
          }
          if (k == sta->nadc)
          {
             fprintf(stderr, "ERROR: LoadDBsisResponses_sub: ");
             fprintf(stderr, "chan %d: found undefined digitizer %s\n",
                     i, chan->adc_label);
             return -1;
          }
          if (!strcmp(sta->adc[j]->label, chan->adc_label))
          {
             if (sta->adc[j] == NULL)
             {
                fprintf(stderr, "ERROR: LoadDBsisResponses_sub: empty ");
                fprintf(stderr, "digitizer '%s' in channel num %d\n",
                                chan->adc_label, i);
                return -1;
             }
             chan->adc = sta->adc[j];
          }
       }
    }


/*
 * Assign digital filters to channels
 * ==================================
 */

    if (nDigCascade == 0)
    {
       for (i=0; i<sta->nchannel; i++)
       {
          if (strcmp(sta->channel[i]->casc_label, "NONE"))    
          {   
             fprintf(stderr, "ERROR: LoadDBsisResponses_sub: ");
             fprintf(stderr, "chan %d: found undefined digital filter %s\n",
                     i, sta->channel[i]->casc_label);
             return -1;
          }
       }
    }

/*
 * For each channel, compute number of digital filter:
 * adc->nstage + DigCascade->nstage and alloc memory
 */
    for (i=0; i<sta->nchannel; i++)
    {
       n = 0;
       chan = sta->channel[i];
       adc_nstage = chan->adc->filt_cascade->nstage;
       n += adc_nstage;

       /* DigCascade->nstage */
       for (j=0; j<nDigCascade; j++)
       {
          if (!strcmp(chan->casc_label, "NONE"))
             break;

          if (!strcmp(chan->casc_label, DigCascade[j]->label))
          {
             casc = DigCascade[j];
             n += casc->nstage;
             break;
          }
       }
       if (strcmp(chan->casc_label, "NONE") && j >= nDigCascade)
       {
          fprintf(stderr, "ERROR: LoadDBsisResponses_sub: ");
          fprintf(stderr, "chan %d: found undefined digital filter %s\n",
                  i, chan->casc_label);
          return -1;
       }
       /* Alloc memory */
       chan->ndig_filt = n;
       chan->dig_filt = (struct dig_filt *) mem(n*sizeof(struct dig_filt));
    }

    for (i=0; i<sta->nchannel; i++)
    {
       n = 0;
       chan = sta->channel[i];
       adc_nstage = chan->adc->filt_cascade->nstage;
/*
 * Compute channel sint
 */
       chan->sint = chan->adc->sint;

/*
 * Assign adc filters to channels
 * ==============================
 */
       for (j=0; j<adc_nstage; j++)
       {
          chan->dig_filt[n] = chan->adc->filt_cascade->dig_filt[j];
          chan->sint *= chan->dig_filt[n].decim;
          ++n;
       }

/*
 * Assign digital filters to channels
 * =================================
 */
       for (j=0; j<nDigCascade; j++)
       {
          if (!strcmp(chan->casc_label, DigCascade[j]->label))
          {
             casc = DigCascade[j];
             for (k=0; k<casc->nstage; k++)
             {
                 chan->dig_filt[n] = casc->dig_filt[k];
                 chan->sint *= chan->dig_filt[n].decim;
                 ++n;
             }
          }
       }
/*
 * Set channel reported sint equal to sint
 */
       chan->reported_sint = chan->sint;
    }


    if (0) for (i=0; i<sta->nchannel; i++)
    {
        printf("    %s dig filt: ", sta->channel[i]->name);
        for (n=0; n<sta->channel[i]->ndig_filt; n++)
        {
            printf("[%d,%d] ",
                sta->channel[i]->dig_filt[n].filter->ncoef,
                sta->channel[i]->dig_filt[n].decim);
        }
        printf("\n");
    }

/*
 *  Fill in channel channel gain (not in database)
 */
    calc_channel_gain(sta);

    if (0) for (i=0; i<sta->nchannel; i++)
    {
        printf("++++ gain=%E\n", sta->channel[i]->gain);
    }
/*
 *  Fill in channel channel sint (not in database)
 */
    calc_channel_sint(sta);

/*
 * Check sensor azimuth, dip
 */
    check_sensor_orient(sta);

    sortStations(sta);

/*
 * Check seed channel name
 */
    check_seed_chan_name(staRespFname);


if (dbug) fprintf(Fp_err,
              "\n    LoadDBsisResponses_sub: success %d\n\n", retValue);

    return retValue;

error1:
    fprintf(stderr, "ERROR: LoadDBsisResponses_sub failed\n");
    fprintf(stderr,"ERROR: key word BEGIN_STATION missing\n");
    return -1;
}



/*====================================================================
 *===================================================================*/
static void sortStations(sta)
struct station *sta;
{
static char DateStr[MAXCHAN][50];
struct station *ss;
int    i, j, k;
int    dbug = 0;


if (dbug) printf("========= beg sortStations: sta=%s nchannel=%d\n",
               sta->name, sta->nchannel);

/*
 * Make dates strings
 */
    for (i=0; i<sta->nchannel; i++)
    {
        if (i >= MAXCHAN)
        {
           fprintf(stderr,
              "ERROR: sortStations: too many channels; max is MAXCHAN\n");
           exit(1);
        }
        sprintf(DateStr[i], "%.0f  %.0f",
               sta->channel[i]->beg_utime,
               sta->channel[i]->end_utime);
    }

/*
 * Sort channels dates strings
 */
    qsort(DateStr, sta->nchannel, 50, sort_strings);

/*
 * Keep uniques and convert back dates-strings to doubles
 */
    for (i=0,nDates=0; i<sta->nchannel; i++)
    {
       if (i>0 && (!strcmp(DateStr[i], DateStr[i-1])))
          continue;
       sscanf(DateStr[i], "%lf %lf", &Dates[nDates].beg, &Dates[nDates].end);
       ++nDates;
    }

if (dbug) printf("%s nsensor=%d nampli=%d nadc=%d nchan=%d\n",
         sta->name, sta->nsensor, sta->nampli, sta->nadc, sta->nchannel);

if (dbug) for (i=0; i<nDates; i++)
          printf("station %d: from %.0f to %.0f\n",
               i+1, Dates[i].beg, Dates[i].end);

/*
 * Create station-channels
 */
    for (i=0; i<nDates; i++)
    {
       ss = (struct station *) mem(sizeof(struct station));
       append_linklist_element(ss, stalist_head, stalist_tail);

       strncpy(ss->name, sta->name, strlen(sta->name));
       strncpy(ss->network, sta->network, strlen(sta->network));
       memcpy(&ss->loc, &sta->loc, sizeof(struct loc));
       ss->beg_utime = Dates[i].beg;
       ss->end_utime = Dates[i].end;

/*
 * Copy station sensors
 */
       ss->nsensor = sta->nsensor;
       ss->sensor  = (struct sensor **)
                     mem(sizeof(struct sensor *) * ss->nsensor);
       for (j=0; j<ss->nsensor; j++)
       {
          ss->sensor[j] = (struct sensor *) mem(sizeof(struct sensor));
          memcpy(ss->sensor[j], sta->sensor[j], sizeof(struct sensor));
       }

/*
 * Copy station ampli_filters
 */
       ss->nampli = sta->nampli;
       ss->ampli  = (struct ampli **)
                    mem(sizeof(struct ampli *) * ss->nampli);
       for (j=0; j<ss->nampli; j++)
       {
          ss->ampli[j] = (struct ampli *) mem(sizeof(struct ampli));
          memcpy(ss->ampli[j], sta->ampli[j], sizeof(struct ampli));
       }
/*
 * Copy digitizers
 */
       ss->nadc   = sta->nadc;
       ss->adc    = (struct adc **)
                    mem(sizeof(struct adc *) * ss->nadc);
       for (j=0; j<ss->nadc; j++)
       {
          ss->adc[j] = (struct adc *) mem(sizeof(struct adc));
          memcpy(ss->adc[j], sta->adc[j], sizeof(struct adc));
       }
/*
 * Copy selected station channels
 */
       ss->nchannel = 0;
       for (j=0; j<sta->nchannel; j++)
       {
          if ((sta->channel[j]->beg_utime == ss->beg_utime) &&
              (sta->channel[j]->end_utime == ss->end_utime))
          {
              ++(ss->nchannel);
          }
       }

if (dbug) printf(" sortStations: %s: %d channels: from %.0f to %.0f\n",
        ss->name, ss->nchannel, ss->beg_utime, ss->end_utime);

       ss->channel = (struct channel **)
                    mem(sizeof(struct channel *) * ss->nchannel);

       for (j=0,k=0; j<sta->nchannel; j++)
       {
          if ((sta->channel[j]->beg_utime == ss->beg_utime) &&
              (sta->channel[j]->end_utime == ss->end_utime))
          {
             ss->channel[k] = (struct channel *) mem(sizeof(struct channel));
             memcpy(ss->channel[k], sta->channel[j], sizeof(struct channel));

if (dbug) printf("    %s ss->channel[%d] = sta->channel[%d]\n",
                      ss->channel[k]->name, k, j);

             ++k;
          }
       }
    }

if (dbug) for (ss=stalist_head; ss!=NULL; ss=ss->next)
{
    printf("========= end sortStations: sta=%s nchannel=%d\n",
           ss->name, ss->nchannel);
}

}
    

/*====================================================================
 *===================================================================*/
static void check_seed_chan_name(fname)
char *fname;
{
struct station *sta;
struct channel *chan;
char seed_chan[4];
int  i;

   for (sta=stalist_head; sta!=NULL; sta=sta->next)
     for (i=0; i<sta->nchannel; i++)
   {
      chan = sta->channel[i];
      seed_channel_map(chan->sensor->type,
                       '\0',
                       chan->name,
                       (double) (1.0 / chan->sint),
                       0.0,
                       seed_chan);
      if (strcmp(chan->name, seed_chan))
      {
         printf("\nWARNING: check_seed_chan_name: file %s: %s %s %E:\n",
                fname, sta->name, chan->sensor->type,
                (double) (1.0 / chan->sint));
         printf("         seed chan: %s desagree with expected: %s\n",
                chan->name, seed_chan);
         strncpy(chan->name, seed_chan, 3);
      }
   }
}


/*====================================================================
 *===================================================================*/
static int check_kword(kword, list, len)
char *kword;
char **list;
int  len;
{
int i;

    if (strstr(kword, "BEGIN_") || strstr(kword, "END_"))
    {
       for (i=0; i<len; i++)
       {
          if (!strcmp(kword, list[i]))
          {
if (0) printf("---- %s %s\n", list[i], kword);
             return TRUE;
          }
       }
       if (i >= len)
       {
          fprintf(stderr,"ERROR: unknown key word '%s'\n", kword);
          return FALSE;
       }
    }
    return TRUE;
}

/*====================================================================
 *
 *  read_staname     Read station identity
 *
 *===================================================================*/
static int read_staname(token, ntok, sta)
char **token;
int  ntok;
struct station *sta;
{

    if (ntok < 3)
    {
        fprintf(stderr,"ERROR: read_staname: Expect 3+ tokens\n");
        return FALSE;
    }


    ucase (token[0]);
    sprintf(sta->name, "%s", token[0]);

    return TRUE;
}

/*====================================================================
 *
 *  read_staloc     Read station location
 *
 *===================================================================*/
static int read_staloc(token, ntok, loc)
char  **token;
int   ntok;
struct loc *loc;
{
char line[256];
int  i;

    if (ntok < 6)
    {
       fprintf(stderr,"ERROR: read_staloc: Expected 6+ tokens, got %d.\n",
               ntok);
       return FALSE;
    }

    loc->lat = atof(token[3]);
    loc->lon = atof(token[4]);
    loc->elev = atof(token[5]);
    loc->depth = 0.0;

/*  Build the station description string  */
    line[0] = 0;
    for (i=6; i<ntok; i++) {
        sprintf(&line[strlen(line)], "%s ", token[i]);
    }
    line[strlen(line)-1] = 0;
    loc->desc = (char *) mem(strlen(line)+1);

    sprintf(loc->desc, "%s", line);

if (0) printf("read_staloc %.3f %.3f %d %d %s\n",
        loc->lat, loc->lon, loc->elev, loc->depth, loc->desc);
    return TRUE;
}



/*======================================================================
 *=====================================================================*/
static int read_sensor(token, ntok, psensor, count)
char  **token;
int   ntok;
struct sensor **psensor;
int   *count;
{
struct sensor *p;
char   sensorFname[511];
char   lastchar;
/* int    lineno_sav; */

    ucase(token[0]);
    if (ntok != 10)
    {
       fprintf(stderr,"ERROR: read_sensor: Expect 10 tokens, got %d.\n",
               ntok);
       return FALSE;
    }
    psensor[*count] = (struct sensor*) mem(sizeof(struct sensor));
    p = psensor[*count];

    sprintf(p->label, "%s", token[0]);

    p->type = mem(strlen(token[1])+3);
    sprintf(p->type, "%s", token[1]);

    p->id   = mem(strlen(token[2])+3);
    sprintf(p->id, "%s", token[2]);

    p->delta_lat   = atof(token[4]);
    p->delta_lon   = atof(token[5]);
    p->delta_elev  = atof(token[6]);
    p->delta_depth = atof(token[7]);

    p->azimuth = atof(token[8]);
    p->dip     = atof(token[9]);

    if ((lastchar = token[3][strlen(token[3])-1]) == '/')
    {
       fprintf(stderr,"ERROR: read_sensor: empty sensor file '%s'\n",
               token[3]);
       return FALSE;
    }
    sprintf(sensorFname, "%s/%s", PZdir, token[3]);
    p->sname = mem(strlen(sensorFname)+3);
    sprintf(p->sname, "%s", sensorFname);

if (0)
printf("==== read_sensor: n=%d code=%s type=%s id=%s azim=%.2f dip=%.2f\n",
        *count,
        p->label,
        p->type,
        p->id,
        p->azimuth,
        p->dip);
if (0)
if (p->delta_lat || p->delta_lon || p->delta_elev || p->delta_depth)
printf("     delta_lat=%.2f delta_lon=%.2f delta_elev=%d delta_depth=%d\n",
        p->delta_lat,
        p->delta_lon,
        p->delta_elev,
        p->delta_depth);



    if (read_sensor_sub(p->sname, p) == FALSE)
        return FALSE;

    p->fname  = NULL;
    p->filter = NULL;
    p->g_err  = 0.0;

    if(++(*count) > MAXSENSOR)
    {
        fprintf(stderr,"ERROR: read_sensor: too many sensors\n");
        exit(1);
    }
    return TRUE;
}



/*======================================================================
 *=====================================================================*/
static int read_sensor_sub(sensorFname, sensor)
char   *sensorFname;
struct sensor *sensor;
{
#define TRANSFER_FUNCTION_TYPE            0
#define TRANSFER_NORMALIZATION_FREQUENCY  1
#define INCLUDE                           2
#define SENSITIVITY                       3
#define NUMBER_OF_ZEROES                  4
#define NUMBER_OF_POLES                   5
#define INPUT_UNIT                        6
#define OUTPUT_UNIT                       7
#define NKWORD2  8
static char *keyword[NKWORD2] =
{
  "TRANSFER_FUNCTION_TYPE",
  "TRANSFER_NORMALIZATION_FREQUENCY",
  "INCLUDE",
  "NUMBER_OF_ZEROES",
  "SENSITIVITY",
  "NUMBER_OF_POLES",
  "INPUT_UNIT",
  "OUTPUT_UNIT"
};
int     found_keyw[NKWORD2];

FILE    *Fp;
char    PZname[80];
char    line[255];
char    line_cp[255];
char    inpUnits[20], outUnits[20];
int     err;
int     foundInclude;
int     foundPZ;
int     fileOffset;
int     i;

    for (i=0; i<NKWORD2; i++) found_keyw[i] = FALSE;
    foundInclude = FALSE;
    foundPZ      = FALSE;
    fileOffset   = 0;
    PZname[0]    = '\0';

    if (!(Fp = fopen(sensorFname, "r")))
    {
        fprintf(stderr,"ERROR: read_sensor_sub: can't open '%s'\n",
                sensorFname);
        return FALSE;
    }

if (dbug) printf("    read_sensor_sub: opening '%s'\n", sensorFname);

/*
 * Allocate memory for sensor
 */
    sensor->sensor = (struct filter*) mem(sizeof(struct filter));
    sensor->sensor->ncoef = 0;
    sensor->sensor->nzero = 0;
    sensor->sensor->npole = 0;
    sensor->sensor->decim = 1;
    sensor->sensor->sint  = 0.0;
    sensor->sensor->norm  = 1.0;
    sensor->sensor->gain  = 1.0;
    sensor->sensor->A0    = 1.0;

    sensor->units = 0;

    lineno = 0;
    while (1)
    {
        if ((err = getLine(Fp, line, 250, '#', &lineno) == 1)) break;
        sprintf(line_cp, "%s", line);
        ntokens = sparse(line_cp, token, DELIM2, MAXTOK);
        ucase(token[0]);

/*
if (0) printf("---- %s\n", token[0]);
*/
        if (!strcmp(token[0], "TRANSFER_FUNCTION_TYPE"))
        {
            if (ntokens < 2) goto error;
            found_keyw[TRANSFER_FUNCTION_TYPE] = TRUE;
            lcase(token[1]);
            if (strcmp(token[1], "analog"))
            {
               fprintf(stderr,
                   "ERROR: read_sensor_sub: TF type %s unsupported\n",
                   token[1]);
               return FALSE;
            }
            sensor->sensor->type = ANALOG;
        }
        if (!strcmp(token[0], "INPUT_UNIT"))
        {
            if (ntokens < 2) goto error;
            found_keyw[INPUT_UNIT] = TRUE;
            ucase(token[1]);
            sprintf(inpUnits, "%s", token[1]);
        }
        if (!strcmp(token[0], "OUTPUT_UNIT"))
        {
            if (ntokens < 2) goto error;
            found_keyw[OUTPUT_UNIT] = TRUE;
            ucase(token[1]);
            sprintf(outUnits, "%s", token[1]);
        }
        if (!strcmp(token[0], "SENSITIVITY"))
        {
            if (ntokens < 2) goto error;
            found_keyw[SENSITIVITY] = TRUE;
            sensor->sensor_G = atof(token[1]);
        }
        if (!strcmp(token[0], "TRANSFER_NORMALIZATION_FREQUENCY"))
        {
            if (ntokens < 2) goto error;
            found_keyw[TRANSFER_NORMALIZATION_FREQUENCY] = TRUE;
            sensor->sensor->fn = atof(token[1]);
            fileOffset = ftell(Fp);
        }
        if (!strcmp(token[0], "INCLUDE"))
        {
            if (ntokens < 2) goto error;
            found_keyw[INCLUDE] = TRUE;
            sprintf(PZname, "%s", token[1]);
            foundInclude = TRUE;
        }
        if (!strcmp(token[0], "NUMBER_OF_ZEROES"))
        {
            if (ntokens < 2) goto error;
            found_keyw[NUMBER_OF_ZEROES] = TRUE;
        }
        if (!strcmp(token[0], "NUMBER_OF_POLES"))
        {
            if (ntokens < 2) goto error;
            found_keyw[NUMBER_OF_POLES] = TRUE;
        }
    }
    if ((found_keyw[NUMBER_OF_ZEROES] == TRUE) &&
        (found_keyw[NUMBER_OF_POLES]  == TRUE))
    {
       foundPZ = TRUE;
    }

    if (found_keyw[INCLUDE] == TRUE)
    {
       if (foundPZ == FALSE)
       {
          found_keyw[NUMBER_OF_ZEROES] = TRUE;
          found_keyw[NUMBER_OF_POLES]  = TRUE;
       }
    }
    else if (foundPZ == TRUE)
    {
       if (found_keyw[INCLUDE] == FALSE)
          found_keyw[INCLUDE] = TRUE;
    }
    else
    {
       fprintf(stderr,"ERROR: read_sensor_sub: file '%s':\n", sensorFname);
       fprintf(stderr,"       key word %s or %s or %s missing\n",
               keyword[INCLUDE],
               keyword[NUMBER_OF_ZEROES],
               keyword[NUMBER_OF_POLES]);
       return FALSE;
    }

    for (i=0; i<NKWORD2; i++) if (found_keyw[i] == FALSE)
    {
       fprintf(stderr,"ERROR: read_sensor_sub: file '%s':\n", sensorFname);
       fprintf(stderr,"       key word %s missing\n", keyword[i]);
       return FALSE;
    }

    if (strcmp(outUnits, "V"))
    {
       fprintf(stderr,
          "ERROR: read_sensor_sub: output units: expect 'V' got '%s'\n",
          token[1]);
       return FALSE;
    }

    if (!strcmp(inpUnits, "M"))
    {
        sensor->units = DIS;
    }
    else if (!strcmp(inpUnits, "M/S"))
    {
        sensor->units = VEL;
    }
    else if (!strcmp(inpUnits, "M/S/S"))
    {
        sensor->units = ACC;
    }
    else
    {
        fprintf(stderr,
            "ERROR: read_sensor_sub: unsupported units %s\n", token[1]);
        return FALSE;
    }

if (0) printf("    read_sensor_sub: sensor_G=%.2f units=%s\n",
        sensor->sensor_G, dbdecode(sensor->units));


    if (foundInclude)
    {
       if (readPZ(PZname, NULL, (struct filter *) sensor->sensor) == FALSE)
       {
          fprintf(stderr, "ERROR: read_sensor_sub: readPZ failed\n");
          return FALSE;
       }
    }
    else if (foundPZ)
    {
       fseek(Fp, fileOffset, SEEK_SET);
       if (readPZ(NULL, Fp, (struct filter *) sensor->sensor) == FALSE)
       {
          fprintf(stderr, "ERROR: read_sensor_sub: readPZ failed\n");
          return FALSE;
       }
    }
    else
    {
        return FALSE;
    }


    if (check_roots(sensor->sensor->pole,
                    sensor->sensor->npole,
                    sensor->sensor->type, "pole") == FALSE)
    {
        fprintf(stderr, "read_sensor_sub: check_roots: poles: failed\n");
        return FALSE;
    }
    if (check_roots(sensor->sensor->zero,
                    sensor->sensor->nzero,
                    sensor->sensor->type, "zero") == FALSE)
    {
        fprintf(stderr, "read_sensor_sub: check_roots: zeroes: failed\n");
        return FALSE;
    }

    calc_filter_A0(sensor->sensor);

    fclose(Fp); Fp = NULL;

if (0) printf("==== end read_sensor_sub: success\n");

    return TRUE;

error:
    fprintf(stderr,"ERROR: read_sensor_sub: missing arg for %s\n", token[0]);
    return FALSE;
}


/*======================================================================
 *=====================================================================*/
static int read_ampli(token, ntok, pampli, count)
char  **token;
int   ntok;
struct ampli **pampli;
int   *count;
{
char   ampliFname[511];
struct ampli *p;

    ucase(token[0]);
    if (ntok != 2)
    {
       fprintf(stderr,"ERROR: read_ampli: Expect 2 tokens, got %d.\n",
               ntok);
       return FALSE;
    }

/*
 * Allocate memory for ampli
 */
    pampli[*count] = (struct ampli *) mem(sizeof(struct ampli));
    p = pampli[*count];

    sprintf(p->label, "%s", token[0]);

    if (!strcmp(p->label, "NONE"))
        return TRUE;

    sprintf(ampliFname, "%s/%s", PZdir, token[1]);
    p->fname = mem(strlen(ampliFname)+3);
    sprintf(p->fname, "%s", ampliFname);
    
    p->filter = NULL;
    p->ampli_G = 1.0;


    if (read_ampli_sub(p->fname, p) == FALSE)
        return FALSE;

    if(++(*count) > MAXAMPLI)
    {
        fprintf(stderr,"ERROR: read_ampli: too many amplifiers-filter\n");
        exit(1);
    }

    return TRUE;
}

/*======================================================================
 *=====================================================================*/
static int read_ampli_sub(ampliFname, ampli)
char   *ampliFname;
struct ampli *ampli;
{
#define TRANSFER_FUNCTION_TYPE            0
#define TRANSFER_NORMALIZATION_FREQUENCY  1
#define INCLUDE                           2
#define SENSITIVITY                       3
#define NUMBER_OF_ZEROES                  4
#define NUMBER_OF_POLES                   5
#define NKWORD3  6
static char *keyword[NKWORD3] =
{
  "TRANSFER_FUNCTION_TYPE",
  "TRANSFER_NORMALIZATION_FREQUENCY",
  "INCLUDE",
  "SENSITIVITY",
  "NUMBER_OF_ZEROES",
  "NUMBER_OF_POLES"
};
int     found_keyw[NKWORD3];
FILE    *Fp;
char    PZname[80];
char    line[255];
char    line_cp[255];
int     err;
int     foundInclude;
int     foundPZ;
int     fileOffset;
int     i;

    for (i=0; i<NKWORD3; i++) found_keyw[i] = FALSE;
    foundInclude = FALSE;
    foundPZ      = FALSE;
    fileOffset   = 0;
    PZname[0]    = '\0';

    if (!(Fp = fopen(ampliFname, "r")))
    {
        fprintf(stderr,"ERROR: read_ampli_sub: can't open '%s'\n",
                ampliFname);
        return FALSE;
    }

if (dbug) printf("    read_ampli_sub: opening '%s'\n", ampliFname);

/*
 * Allocate memory for filter
 */
    ampli->filter= (struct filter*) mem(sizeof(struct filter));
    ampli->filter->ncoef = 0;
    ampli->filter->nzero = 0;
    ampli->filter->npole = 0;
    ampli->filter->decim = 1;
    ampli->filter->sint  = 0.0;
    ampli->filter->norm  = 1.0;
    ampli->filter->gain  = 1.0;
    ampli->filter->A0    = 1.0;


    lineno = 0;
    while (1)
    {
        if ((err = getLine(Fp, line, 250, '#', &lineno) == 1)) break;
        sprintf(line_cp, "%s", line);
        ntokens = sparse(line_cp, token, DELIM2, MAXTOK);
        ucase(token[0]);

        if (!strcmp(token[0], "TRANSFER_FUNCTION_TYPE"))
        {
            if (ntokens < 2) goto error;
            found_keyw[TRANSFER_FUNCTION_TYPE] = TRUE;
            lcase(token[1]);
            if (strcmp(token[1], "analog"))
            {
               fprintf(stderr,
                   "ERROR: read_ampli_sub: TF type %s unsupported\n",
                   token[1]);
               return FALSE;
            }
            ampli->filter->type = ANALOG;
        }
        if (!strcmp(token[0], "SENSITIVITY"))
        {
            if (ntokens < 2) goto error;
            found_keyw[SENSITIVITY] = TRUE;
            ampli->ampli_G = atof(token[1]);
        }
        if (!strcmp(token[0], "TRANSFER_NORMALIZATION_FREQUENCY"))
        {
            if (ntokens < 2) goto error;
            found_keyw[TRANSFER_NORMALIZATION_FREQUENCY] = TRUE;
            ampli->filter->fn = atof(token[1]);
            fileOffset = ftell(Fp);
        }
        if (!strcmp(token[0], "INCLUDE"))
        {
            if (ntokens < 2) goto error;
            found_keyw[INCLUDE] = TRUE;
            sprintf(PZname, "%s", token[1]);
            foundInclude = TRUE;
        }
        if (!strcmp(token[0], "NUMBER_OF_ZEROES"))
        {
            if (ntokens < 2) goto error;
            found_keyw[NUMBER_OF_ZEROES] = TRUE;
        }
        if (!strcmp(token[0], "NUMBER_OF_POLES"))
        {
            if (ntokens < 2) goto error;
            found_keyw[NUMBER_OF_POLES] = TRUE;
        }
    }
    if ((found_keyw[NUMBER_OF_ZEROES] == TRUE) &&
        (found_keyw[NUMBER_OF_POLES]  == TRUE))
    {
       foundPZ = TRUE;
    }

    if (found_keyw[INCLUDE] == TRUE)
    {
       if (foundPZ == FALSE)
       {
          found_keyw[NUMBER_OF_ZEROES] = TRUE;
          found_keyw[NUMBER_OF_POLES]  = TRUE;
       }
    }
    else if (foundPZ == TRUE)
    {
       if (found_keyw[INCLUDE] == FALSE)
          found_keyw[INCLUDE] = TRUE;
    }
    else
    {
       fprintf(stderr,"ERROR: read_ampli_sub: file '%s':\n", ampliFname);
       fprintf(stderr,"       key word %s or %s missing\n",
               keyword[INCLUDE], keyword[NUMBER_OF_ZEROES]);
       return FALSE;
    }

    for (i=0; i<NKWORD3; i++) if (found_keyw[i] == FALSE)
    {
       fprintf(stderr,"ERROR: read_ampli_sub: file '%s':\n", ampliFname);
       fprintf(stderr,"       key word %s missing\n", keyword[i]);
       return FALSE;
    }


    if (foundInclude)
    {
       if (readPZ(PZname, NULL, (struct filter *) ampli->filter) == FALSE)
       {
          fprintf(stderr, "ERROR: read_ampli_sub: readPZ failed\n");
          return FALSE;
       }
    }
    else if (foundPZ)
    {
       fseek(Fp, fileOffset, SEEK_SET);
       if (readPZ(NULL, Fp, (struct filter *) ampli->filter) == FALSE)
       {
          fprintf(stderr, "ERROR: read_ampli_sub: readPZ failed\n");
          return FALSE;
       }
    }
    else
    {
        return FALSE;
    }


    if (check_roots(ampli->filter->pole,
                    ampli->filter->npole,
                    ampli->filter->type, "pole") == FALSE)
    {
        fprintf(stderr, "read_ampli_sub: check_roots: poles: failed\n");
        return FALSE;
    }
    if (check_roots(ampli->filter->zero,
                    ampli->filter->nzero,
                    ampli->filter->type, "zero") == FALSE)
    {
        fprintf(stderr, "read_ampli_sub: check_roots: zeroes: failed\n");
        return FALSE;
    }

    calc_filter_A0(ampli->filter);

    fclose(Fp); Fp = NULL;

if (0) printf("==== end read_ampli_sub: success\n");

    return TRUE;

error:
    fprintf(stderr,"ERROR: read_ampli_sub: missing arg for %s\n", token[0]);
    return FALSE;
    return TRUE;
}


/*======================================================================
 *  read_adc.c
 *====================================================================*/
static int read_adc(token, ntok, padc, count)
char   **token;
int    ntok;
struct adc **padc;
int    *count;
{
struct  adc *p;
char    digFilterFname[511];

    ucase(token[0]);
    if (ntok != 2)
    {
       fprintf(stderr,"ERROR: read_adc: Expect 2 tokens, got %d.\n", ntok);
       return FALSE;
    }

    padc[*count] = (struct adc *) mem(sizeof(struct adc));
    p = padc[*count];

    sprintf(p->label, "%s", token[0]);
    p->dataformat = 0;
 
    sprintf(digFilterFname, "%s/%s", PZdir, token[1]);

    if (read_adc_sub(digFilterFname, p) == FALSE)
       return FALSE;

if (0) printf("    read_adc: code=%s sint=%.7E sensit=%.7E\n",
       p->label, p->sint, p->fact);

    if(++(*count) > MAXADC)
    {
       fprintf(stderr,"ERROR: read_adc: too many digitizers\n");
       return -1;;
    }
    return TRUE;
}

/*======================================================================
 *=====================================================================*/
static int read_adc_sub(digFilterFname, adc)
char   *digFilterFname;
struct adc *adc;
{
FILE    *Fp;
char    coefFname[511];
char    prevFname[511];
char    line[255];
char    line_cp[255];
int     foundSensitivityStage;
int     type;
double  sint;
int     decim;
struct dig_filt *work = NULL;
int     nstage, stage;
struct filt_cascade *casc = NULL;
struct firCoef *coef = NULL;
int     err;
int     i;

    adc->filt_cascade = NULL;
    coefFname[0] = '\0';
    prevFname[0] = '\0';
    nstage = 0;
    decim  = 0;
    type = 0;
    foundSensitivityStage = FALSE;

    if (!(Fp = fopen(digFilterFname, "r")))
    {
        fprintf(stderr,"ERROR: read_adc_sub: can't open '%s'\n",
                digFilterFname);
        exit(1);
    }

if (dbug) printf("    read_adc_sub: opening '%s'\n", digFilterFname);

    while (1)
    {
        if ((err = getLine(Fp, line, 250, '#', &lineno) == 1)) break;
        sprintf(line_cp, "%s", line);
        ntokens = sparse(line_cp, token, DELIM2, MAXTOK);
        ucase(token[0]);

        if (!strcmp(token[0], "BEGIN"))
        {
        }
        if (!strcmp(token[0], "TRANSFER_FUNCTION_TYPE"))
        {
            if (!strcmp(token[1], "AD_CONVERSION"))
            {
                foundSensitivityStage = TRUE;
            }
        }
        if (!strcmp(token[0], "OUTPUT_SAMPLING_INTERVAL"))
        {
            adc->sint = atof(token[1]);
        }
        if (!strcmp(token[0], "SENSITIVITY"))
        {
            adc->fact = atof(token[1]);
        }
        if (!strcmp(token[0], "END"))
        {
            if (foundSensitivityStage == TRUE)
            {
                break;
            }
        }
    }
    if (foundSensitivityStage == FALSE)
    {
        fprintf(stderr,
            "ERROR: read_adc_sub: can't find TRANSFER_FUNCTION_TYPE");
        fprintf(stderr, " : AD_CONVERSION stage\n");
        fprintf(stderr, "      in '%s' line %d\n", digFilterFname, lineno);
        return FALSE;
    }

if (0) printf("==== read_adc_sub: sint=%.7E sensit=%.7E\n",
       adc->sint, adc->fact);

/*
 * Find number of dig filters in cascade
 */
    while (1)
    {
        if ((err = getLine(Fp, line, 250, '#', &lineno) == 1)) break;
        sprintf(line_cp, "%s", line);
        ntokens = sparse(line_cp, token, DELIM2, MAXTOK);
        ucase(token[0]);
        if (!strcmp(token[0], "BEGIN"))
        {
        }
        if (!strcmp(token[0], "END"))
        {
            ++nstage;
        }
    }
    rewind(Fp);
    lineno = 0;

    if (nstage == 0)
    {
       fprintf(stderr,"ERROR: read_adc_sub: no filter stage found in '%s'\n",
                      digFilterFname);
       return FALSE;
    }

/*
 * Allocate mem for filter cascade
 */
    casc = (struct filt_cascade *) mem(sizeof(struct filt_cascade));
    casc->filename = mem(strlen(digFilterFname)+3);
    sprintf(casc->label, "none");
    sprintf(casc->filename, "%s", digFilterFname);
    casc->nstage   = nstage;
    casc->dig_filt = (struct dig_filt *) mem(nstage*sizeof(struct dig_filt));

    work = casc->dig_filt;

/*
 * Second pass: load cascade of filters
 */
    stage = 0;
    while (1)
    {
        if ((err = getLine(Fp, line, 250, '#', &lineno) == 1)) break;
        sprintf(line_cp, "%s", line);
        ntokens = sparse(line_cp, token, DELIM2, MAXTOK);
        ucase(token[0]);

        if (!strcmp(token[0], "BEGIN"))
        {
            if (ntokens < 2) goto error;
            work[stage].filter = (struct filter*) mem(sizeof(struct filter));
            work[stage].filename = NULL;
        }
        if (!strcmp(token[0], "TRANSFER_FUNCTION_TYPE"))
        {
            if (ntokens < 2) goto error;
            lcase(token[1]);
            if (!strcmp(token[1], "fir_sym_2"))
            {
                type = FIR_SYM_2;
                work[stage].filter->type  = type; 
            }
            else if (!strcmp(token[1], "fir_sym_1"))
            {
                type = FIR_SYM_1;
                work[stage].filter->type  = type; 
            }
            else if (!strcmp(token[1], "ad_conversion"))
            {
                continue;
            }
            else
            {
                ucase(token[1]);
                fprintf(stderr,
                    "ERROR: read_adc_sub: unsupported filter type %s\n",
                    token[1]);
                fprintf(stderr,"      in '%s' line %d\n",
                    digFilterFname, lineno);
                return FALSE;
            }
        }
        if (!strcmp(token[0], "SENSITIVITY"))
        {
            if (ntokens < 2) goto error;
        }
        if (!strcmp(token[0], "OUTPUT_SAMPLING_INTERVAL"))
            if (ntokens < 2) goto error;
        {
        }
        if (!strcmp(token[0], "INPUT_SAMPLING_INTERVAL"))
        {
            if (ntokens < 2) goto error;
            sint = atof(token[1]);
            work[stage].filter->sint = sint;
        }
        if (!strcmp(token[0], "DECIMATION_FACTOR"))
        {
            if (ntokens < 2) goto error;
            decim = atoi(token[1]);
            work[stage].decim = decim;
            work[stage].filter->decim = decim;
        }
        if (!strcmp(token[0], "INCLUDE"))
        {
            if (ntokens < 2) goto error;
            sprintf(coefFname, "%s/%s", PZdir, token[1]);
        }
        if (!strcmp(token[0], "END"))
        {
            if (ntokens < 2) goto error;
            if (!strlen(coefFname)) continue;
           
            work[stage].filter->nzero = work[stage].filter->npole = 0;
            work[stage].filter->norm = 1.0;
            work[stage].filter->gain = 1.0;
            work[stage].filter->fn   = 0.0;
            work[stage].filter->A0   = 1.0;

        /*
         * First time we find this PZ FIR coef:
         */
            if (!strlen(prevFname) || (strcmp(coefFname, prevFname)))
            {
                if ((readFirCoef(coefFname, &coef, decim, type) == FALSE) ||
                    (coef == NULL))
                {
                    fprintf(stderr,
                        "ERROR: read_adc_sub: readFirCoef failed in '%s'\n",
                        coefFname);
                    return FALSE;
                }
            }
            work[stage].filter->decim = decim;
            work[stage].filter->type  = type;
            work[stage].filter->ncoef = coef->ncoef;
            work[stage].filter->coef  = coef->coef;

            sprintf(prevFname, "%s", coefFname);

            ++stage;
        }
    }
    fclose(Fp); Fp = NULL;

    for (i=0; i<nstage; i++)
    {
        if (!check_sym(work[i].filter))
        {
            fprintf(stderr,"ERROR: read_adc_sub: check symetry failed\n");
            return FALSE;
        }
    }

    adc->filt_cascade = casc;

if (0) printf("==== end read_adc_sub: success\n");

    return TRUE;
error:
    fprintf(stderr, "ERROR: read_adc_sub: missing arg for %s in '%s'\n",
                     token[0], digFilterFname);
    return FALSE;
}



/*======================================================================
 *
 *  read_channel   Read channel information
 *
 *====================================================================*/
static int read_channel(token, ntok, sta)
char   **token;
int    ntok;
struct station *sta;
{
struct channel *p;
int    i;

    if (ntok != 9)
    {
       fprintf(stderr,"ERROR: read_channel: Expect 9 tokens, got %d.\n",
               ntok);
       return FALSE;
    }

    for (i=0; i<6; i++) ucase(token[i]);
    ucase(token[8]);
    if (strncmp(token[8], Network, 2))
    {
/*
       printf(
          "======== read_channel: channel doesn't belong to network '%s'\n",
          Network);
*/
       return TRUE;
    }

/* Alloc memory */
    sta->channel[sta->nchannel] =
                (struct channel *) mem(sizeof(struct channel));
    p = sta->channel[sta->nchannel];

/* Seed chan name */
    strncpy(p->name, token[1], 3);

/* Chan label */
    sprintf(p->sensor_label, "%s", token[2]);

/* Ampli label */
    sprintf(p->ampli_label, "%s", token[3]);

/* Digitizer label */
    sprintf(p->adc_label, "%s", token[4]);

/* Digital filter label */
    sprintf(p->casc_label, "%s", token[5]);

/* Beg and end time */

    p->beg_utime = asc2dbtime(token[6]);
    if (!strcmp(token[7], "present")) p->end_utime = 0.0;
    else                              p->end_utime = asc2dbtime(token[7]);

    sprintf(p->network, "%.2s", token[8]);

    p->ampli_nom_gain = 1.0;
    p->gain_corr      = 1.0;
    p->gain_err       = 0.0;
    p->reported_sint  = 0.0;
    p->ndig_filt      = 0;
    p->dig_filt       = NULL;


if (dbug)
    printf("    read_channel: %s %s %s %s %s %s %s from %.0f to %.0f\n",
        sta->name, p->network, p->name,
        p->sensor_label, p->ampli_label, p->adc_label, p->casc_label,
        p->beg_utime, p->end_utime);

    if(++(sta->nchannel) > MAXCHAN)
    {
        fprintf(stderr,"ERROR: read_channel: too many channels\n");
        exit(1);
    }

    return TRUE;
}


/*======================================================================
 *=====================================================================*/
static int readPZ(PZname, Fp, filt)
char   *PZname;
FILE   *Fp;
struct filter *filt;
{
char    PZfname[511];
char    line[255];
char    line_cp[255];
int     np, nz;
int     err;
int     j;
char    *tok2[3];
int     ntok2;
char    line2[255];
int     fileOffset;

    if (PZname != NULL)
    {
       sprintf(PZfname, "%s/%s", PZdir, PZname);
       if (!(Fp = fopen(PZfname, "r")))
       {
           fprintf(stderr,"ERROR: readPZ: can't open %s\n", PZfname);
           return FALSE;
       }
if (dbug) printf("    readPZ: opening '%s'\n", PZfname);

       lineno = 0;
    }
    fileOffset = ftell(Fp);

    while (1)
    {
        if ((err = getLine(Fp, line, 250, '#', &lineno) == 1)) break;
        sprintf(line_cp, "%s", line);
        ntokens = sparse(line_cp, token, DELIM2, MAXTOK);
        ucase(token[0]);

        if (!strcmp(token[0], "NUMBER_OF_ZEROES"))
        {
            if (ntokens < 2) goto error;
            nz = atoi(token[1]);
            filt->nzero = nz;
if (0) printf("========== nz %d\n", nz);
        }
        if (!strcmp(token[0], "NUMBER_OF_POLES"))
        {
            if (ntokens < 2) goto error;
            np = atoi(token[1]);
            filt->npole = np;
if (0) printf("========== np %d\n", np);
        }
    }
    fseek(Fp, fileOffset, SEEK_SET);


/*
 * Allocate memory for poles ans zeroes
 */
    if (filt->nzero > 0)
        filt->zero = (double *) mem(filt->nzero*sizeof(double)*2);
    if (filt->npole > 0)
        filt->pole = (double *) mem(filt->npole*sizeof(double)*2);


    lineno = 0;
    while (1)
    {
        if ((err = getLine(Fp, line, 250, '#', &lineno) == 1)) break;
        sprintf(line_cp, "%s", line);
        ntokens = sparse(line_cp, token, DELIM2, MAXTOK);
        ucase(token[0]);

        if (!strcmp(token[0], "NUMBER_OF_ZEROES"))
        {
            if (ntokens < 2) goto error;
            nz = atoi(token[1]);
            for (j=0; j<nz; j++)
            {
                if ((err = getLine(Fp, line2, 250, '#', &lineno)) != 0)
                {
                    fprintf(stderr,
                        "ERROR: readPZ: error reading zeroes in '%s'\n",
                        PZfname);
                    return FALSE;
                }
                ntok2 = sparse(line2, tok2, " ", 3);
                if (ntok2 != 2)
                {
                    fprintf(stderr,
                        "ERROR: readPZ: expecting 2 values for zero '%s' line %d\n",
                        PZfname, lineno);
                    return FALSE;
                }
                else
                {
                    filt->zero[2*j]   = atof(tok2[0]);
                    filt->zero[2*j+1] = atof(tok2[1]);
                }
            }
        }
        if (!strcmp(token[0], "NUMBER_OF_POLES"))
        {
            if (ntokens < 2) goto error;
            np = atoi(token[1]);
            for (j=0; j<np; j++)
            {
                if ((err = getLine(Fp, line2, 250, '#', &lineno)) != 0)
                {
                    fprintf(stderr, "ERROR: readPZ: error reading poles\n");
                    return FALSE;
                }
                ntok2 = sparse(line2, tok2, " ", 3);
                if (ntok2 != 2)
                {
                    fprintf(stderr,
                        "ERROR: readPZ: expecting 2 values for pole'%s' line %d\n",
                         PZfname, lineno);
                    return FALSE;
                }
                else
                {
                    filt->pole[2*j]   = atof(tok2[0]);
                    filt->pole[2*j+1] = atof(tok2[1]);
                }
            }
        }

    }
    if (PZname != NULL)
    {
       fclose(Fp); Fp = NULL;
    }

    return TRUE;

error:
    fprintf(stderr, "ERROR: readPZ: missing arg for %s\n", token[0]);
    return FALSE;
}


/*======================================================================
    read_dig_filter()
    Load a cascade of digital filter
 *=====================================================================*/
int read_dig_filter(token, ntok, dig_cascade)
char   **token;
int    ntok;
struct filt_cascade **dig_cascade;
{
#define BEGIN4                           0
#define TRANSFER_FUNCTION_TYPE4          1
#define INPUT_SAMPLING_INTERVAL4         2
#define DECIMATION_FACTOR4               3
#define END4                             4
#define NKWORD4                          5
static char *keyword[NKWORD4] =
{
  "BEGIN",
  "TRANSFER_FUNCTION_TYPE",
  "INPUT_SAMPLING_INTERVAL",
  "DECIMATION_FACTOR",
  "END"
};
int     found_keyw[NKWORD4];

FILE    *Fp;
char    cascadeFilename[511];
char    coefFname[511];
char    prevFname[511];
char    line[255];
char    line_cp[255];
int     err;
int     i;
double  sint;
int     decim;
int     nstage, stage;
int     foundBegin;
int     type;
struct  dig_filt *work = NULL;
struct  filt_cascade *casc = NULL;
struct  firCoef *coef = NULL;
char    *label;
char    *cascadeName;


    for (i=0; i<NKWORD4; i++) found_keyw[i] = FALSE;
    *dig_cascade = NULL;
    coefFname[0] = '\0';
    prevFname[0] = '\0';
    nstage = 0;
    decim  = 0;
    type = 0;
    foundBegin = FALSE;
    label       = token[0];
    cascadeName = token[1];

    sprintf(cascadeFilename, "%s/%s", PZdir, cascadeName);

    if (!(Fp = fopen(cascadeFilename, "r")))
    {
        fprintf(stderr,"ERROR: read_dig_filter: can't open '%s'\n",
                      cascadeFilename);
        return FALSE;
    }

if (dbug) printf("    read_dig_filter: opening '%s'\n", cascadeFilename);

/*
 * First pass: find number of dig filters in cascade
 */
    lineno = 0;
    while (1)
    {
        if ((err = getLine(Fp, line, 250, '#', &lineno) == 1)) break;
        sprintf(line_cp, "%s", line);
        ntokens = sparse(line_cp, token, DELIM2, MAXTOK);
        ucase(token[0]);

        if (!strcmp(token[0], "BEGIN"))
        {
        }
        if (!strcmp(token[0], "END"))
        {
            ++nstage;
        }
    }
    rewind(Fp);
    lineno = 0;

    if (nstage == 0)
    {
       fprintf(stderr,
               "ERROR: read_dig_filter: no filter stage found in '%s'\n",
                cascadeFilename);
       return FALSE;
    }
/*
 * Allocate mem for filter cascade
 */
    casc = (struct filt_cascade *) mem(sizeof(struct filt_cascade));
    sprintf(casc->label, "%s", label);
    casc->filename = mem(strlen(cascadeFilename)+3);
    sprintf(casc->filename, "%s", cascadeFilename);
    casc->nstage   = nstage;
    casc->dig_filt = (struct dig_filt *) mem(nstage*sizeof(struct dig_filt));

    work = casc->dig_filt;

/*
 * Second pass: load cascade of filters
 */
    stage = 0;
    while (1)
    {
        if ((err = getLine(Fp, line, 250, '#', &lineno) == 1)) break;
        sprintf(line_cp, "%s", line);
        ntokens = sparse(line_cp, token, DELIM2, MAXTOK);
        ucase(token[0]);

        if (!strcmp(token[0], "BEGIN"))
        {
            if (ntokens < 2) goto error;
            found_keyw[BEGIN4] = TRUE;
            work[stage].filter = (struct filter*) mem(sizeof(struct filter));
            work[stage].filename = NULL;
        }
        if (!strcmp(token[0], "TRANSFER_FUNCTION_TYPE"))
        {
            if (ntokens < 2) goto error;
            found_keyw[TRANSFER_FUNCTION_TYPE4] = TRUE;
            lcase(token[1]);
            if (!strcmp(token[1], "fir_sym_2"))
            {
                type = FIR_SYM_2;
                work[stage].filter->type  = type; 
            }
            else if (!strcmp(token[1], "fir_sym_1"))
            {
                type = FIR_SYM_1;
                work[stage].filter->type  = type; 
            }
            else
            {
                fprintf(stderr,
                    "ERROR: read_dig_filter: unsupported filter type %s\n",
                    token[1]);
                return FALSE;
            }
        }
        if (!strcmp(token[0], "INPUT_SAMPLING_INTERVAL"))
        {
            if (ntokens < 2) goto error;
            found_keyw[INPUT_SAMPLING_INTERVAL4] = TRUE;
            sint = atof(token[1]);
            work[stage].filter->sint = sint;
        }
        if (!strcmp(token[0], "DECIMATION_FACTOR"))
        {
            if (ntokens < 2) goto error;
            found_keyw[DECIMATION_FACTOR4] = TRUE;
            decim = atoi(token[1]);
            work[stage].decim = decim;
            work[stage].filter->decim = decim;
        }
        if (!strcmp(token[0], "INCLUDE"))
        {
            if (ntokens < 2) goto error;
            sprintf(coefFname, "%s/%s", PZdir, token[1]);
        }
        if (!strcmp(token[0], "END"))
        {
            if (ntokens < 2) goto error;
            found_keyw[END4] = TRUE;
            if (!strlen(coefFname)) continue;
           
            for (i=0; i<NKWORD4; i++) if (found_keyw[i] == FALSE)
            {
               fprintf(stderr,"ERROR: read_dig_filter: file '%s':\n",
                       cascadeName);
               fprintf(stderr,"       key word %s missing\n", keyword[i]);
               return FALSE;
            }

            work[stage].filter->nzero = work[stage].filter->npole = 0;
            work[stage].filter->norm = 1.0;
            work[stage].filter->gain = 1.0;
            work[stage].filter->fn   = 0.0;
            work[stage].filter->A0   = 1.0;

        /*
         * First time we find this PZ FIR coef:
         */
            if (!strlen(prevFname) || (strcmp(coefFname, prevFname)))
            {
                if ((readFirCoef(coefFname, &coef, decim, type) == FALSE) ||
                    (coef == NULL))
                {
                    fprintf(stderr,
                        "ERROR: read_dig_filter: readFirCoef failed in '%s'\n",
                        coefFname);
                    return FALSE;
                }
            }
            work[stage].filter->decim = decim;
            work[stage].filter->type  = type;
            work[stage].filter->ncoef = coef->ncoef;
            work[stage].filter->coef  = coef->coef;

            sprintf(prevFname, "%s", coefFname);

            ++stage;
        }
    }

    for (i=0; i<nstage; i++)
    {
        if (!check_sym(work[i].filter))
        {
            fprintf(stderr,"ERROR: read_dig_filter: check symetry failed\n");
            return FALSE;
        }
    }

    *dig_cascade = casc;
if (0) printf("==== end read_dig_filter: success\n");

    return TRUE;
error:
    fprintf(stderr,"ERROR: read_dig_filter: missing arg for %s\n", token[0]);
    return FALSE;
}



/*======================================================================
 *=====================================================================*/
static int readFirCoef(coefFname, firCoef, decim, type)
char   *coefFname;
struct firCoef **firCoef;
int    decim;
int    type;
{
FILE    *Fp;
char    line[255];
char    line_cp[255];
int     ncoef;
int     foundNcoef;
struct  firCoef *coef;
int     err;

    if (!(Fp = fopen(coefFname, "r")))
    {
        fprintf(stderr,"ERROR: readFirCoef: can't open '%s'\n", coefFname);
        exit(1);
    }
    if (decim <= 0)
    {
        fprintf(stderr,"ERROR: readFirCoef: wrong decim value %d\n", decim);
        return FALSE;
    }

if (dbug) printf("    readFirCoef: opening '%s'\n", coefFname);

    foundNcoef = FALSE;
    ncoef = 0;
    lineno = 0;
    while (1)
    {
        if ((err = getLine(Fp, line, 250, '#', &lineno) == 1)) break;
        sprintf(line_cp, "%s", line);
        ntokens = sparse(line_cp, token, DELIM2, MAXTOK);
        ucase(token[0]);

        if (!strcmp(token[0], "NUMBER_OF_ZEROS"))
        {
            if (ntokens < 2)
            {
                fprintf(stderr, "ERROR: readFirCoef: missing arg for %s\n",
                        token[0]);
                return FALSE;
            }
            foundNcoef = TRUE;
            ncoef = atoi(token[1]);
            break;
        }
    }

    if (foundNcoef == FALSE || ncoef == 0)
    {
        fprintf(stderr, "ERROR: readFirCoef: can't find num of coef\n");
        return FALSE;
    }
    if (ncoef == 128)
    {
        if (type != FIR_SYM_2)
        {
            fprintf(stderr,
                "readFirCoef: ncoef = 128 and type not FIR_SYM_2\n");
            return FALSE;
        }
    }
    else if (ncoef == 13)
    {
        if (type != FIR_SYM_1)
        {
            fprintf(stderr,
                "readFirCoef: ncoef = 13 and type not FIR_SYM_1\n");
            return FALSE;
        }
    }
    else if (ncoef == 101)
    {
        if (type != FIR_SYM_1)
        {
            fprintf(stderr,
                "readFirCoef: ncoef = 13 and type not FIR_SYM_1\n");
            return FALSE;
        }
    }
    else
    {
        fprintf(stderr,"ERROR: readFirCoef: unsupported FIR coeffs\n");
        return FALSE;
    }

    coef = (struct firCoef*) mem(sizeof(struct firCoef));
    coef->type  = type;
    coef->decim = decim;
    coef->ncoef = ncoef;
    coef->coef = (double *) mem(ncoef*sizeof(double)); 
    if (!rd_coef(Fp, line, coef->coef, coef->ncoef))
    {
        fprintf(stderr, "readFirCoef: rd_coef failed\n");
        return FALSE;
    }
    *firCoef = coef;

    fclose(Fp); Fp = NULL;
    return TRUE;
}



/*======================================================================
 *  rd_coef()
 *====================================================================*/
static int rd_coef(Fp, line, data, ndata)
FILE *Fp;
char *line;
double *data;
int ndata;
{
int i, ntoken;
int nread = 0;
char *token[MAXTOK3];

    lineno = 0;
    if (ndata == 0) return TRUE;
    do {
        if (getLine(Fp, line, 255, '#', &lineno))
            return FALSE;
        ntoken = sparse(line, token, DELIM1, MAXTOK3);
        for (i = 0; i < ntoken; i++)
        {
            data[nread++] = atof(token[i]);
        }
    } while (nread != ndata);

    return TRUE;
}


/*======================================================================
 *
 *  check_roots()    Check real an imag parts of roots
 *
 *====================================================================*/
static int check_roots(root, nroot, type, p_z)
double root[];
int nroot, type;
char *p_z;
{
int i;
double err;

    for (i = 0; i < nroot*2; i += 2)
    {
      if (type != IIR_PZ && root[i] > 0.0)
      {
        fprintf(stderr,"check_roots: %s real part not negative\n",p_z);
        return FALSE;
      }
    }
    for (i = 0; i < nroot*2; )
    {
        if (root[i+1] == 0.)
        {
            i += 2;
            continue;
        }
        if (root[i] != root[i+2])
        {
            err = fabs((root[i]-root[i+2]) / root[i]);
            if (err > 5.e-07)
            {
               fprintf(stderr,
                 "check_roots: %s real parts not equal %.10e %.10e\n",
                 p_z, root[i],root[i+2]);
               fprintf(stderr, "err = %.7e\n", err);
               return FALSE;
            }
        }
        if ((root[i+1] + root[i+3]) != 0.)
        {
            err = fabs((root[i+1]+root[i+3]) / root[i+1]);
            if (err > 5.e-07)
            {
               fprintf(stderr,
                 "check_roots: %s imag parts not opposite %.10e %.10e\n",
                 p_z, root[i+1],root[i+3]);
               fprintf(stderr, "err = %.7e\n", err);
               return FALSE;
            }
        }
        i += 4;
    }
    return TRUE;
}


void print_filter(fp, f, n)
FILE *fp;
struct filter *f;
int n;
{
int type;
char atype[12];

    type = f->type;
    if      (type == ANALOG)    sprintf(atype, "ANALOG   ");
    else if (type == LAPLACE)   sprintf(atype, "LAPLACE  ");
    else if (type == FIR_SYM_1) sprintf(atype, "FIR_SYM_1");
    else if (type == FIR_SYM_2) sprintf(atype, "FIR_SYM_2");
    else if (type == FIR_ASYM)  sprintf(atype, "FIR_ASYM ");
    else if (type == IIR_PZ)    sprintf(atype, "IIR_PZ   ");
    else if (type == IIR_POLY)  sprintf(atype, "IIR_POLY ");
    else if (type == COMB)      sprintf(atype, "COMB     ");
    else                        sprintf(atype, ".........");

    fprintf(fp, "stage %d %s ", n, atype);

    if      (type == ANALOG || type == LAPLACE || type == IIR_PZ)
        fprintf(fp, "             nz=%d np=%d\n", f->nzero, f->npole);
    else
    {
        fprintf(fp, "%.9E sps nc=%d dec=%d\n",
                     1.0/f->sint, f->ncoef, f->decim);
        if (0)
        {
            int i;
            for (i=0; i<10; i++) fprintf(fp, "%.14E\n", f->coef[i]);
        }
    }
}


/*======================================================================
    Format 1 1999.11.26-12:00:00
    Format 2 19990708-13:42:00   (old)
 *=====================================================================*/
static double asc2dbtime(str)
char *str;
{
double dbtime;
char   asctime[24];
char   s[24];
char   month[3], day[3];

    sprintf(asctime, "1970.01.01-00.00.00.000");
    sprintf(month, "%.2s", &str[5]);
    sprintf(day,   "%.2s", &str[8]);
    if (atoi(month) > 0 && atoi(month) < 13 &&
        atoi(day)   > 0 && atoi(day)   < 32)
    {
       sprintf(asctime, "1970.01.01-00.00.00.000");
       strncpy(asctime, str, strlen(str));
       str2utime1(asctime, &dbtime);
       return dbtime;
    }
    sprintf(month, "%.2s", &str[4]);
    sprintf(day,   "%.2s", &str[6]);
    sprintf(s, "%.4s.%.2s.%.2s-%s",
        &str[0],
        &str[4],
        &str[6],
        &str[9]);
    strncpy(asctime, s, strlen(s));
    str2utime1(asctime, &dbtime);
    return dbtime;

    fprintf(stderr,"ERROR: asc2dbtime: unsupported time format: %s\n",str);
    exit(1);
}
