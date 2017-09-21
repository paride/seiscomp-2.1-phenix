/*======================================================================
  
    loadresp.c  Read in and fill out one station data structure
  
    A station has:
        - several sensors
        - several digitizers (adc)
        - several channels
    A channels is the combination of:
        - one sensor
        - one analog anti-aliasing filter or none
        - one amplifier or none
        - one digitizer
        - several digital filters or none

        According to the SEED format, a normalization factor (A0) and
    a normalization frequency (fn) must be specified for the analog
    stages (blockette 053). In addition, a sensitivity factor (or gain)
    and a frequency of sensitivity factor must be specified for all
    stages, analog or digital (blockette 058).
        In SEED, there is an extra stage (last, number 0) which has a
    special purpose: it gives in a blockette 058 the overall
    sensitivity of the channel at the specified frequency.
        For the Geoscope responses, there is a A0 factor for the sensor
    and eventually for the analog anti-aliasing filter. If this 
    anti-aliasing filter exists, its normalization frequency must be
    equal to the sensor's normalization frequency. It is also mandatory
    to set the frequency associated to the overall sensitivity in the
    SEED last stage 0 equal to the sensor's normalization frequency.
        The only stages which have a sensitivity factor (blockette 58)
    different from unity (1.0) are the sensor, the amplifier and the
    digitizer.  For the digital stages (digitizer and digital filters), 
    the frequency of sensitivity factor is set to null (0).
        To summarize, the overall channel sensitivity is the product of 
    the sensor sensitivity factor by the conversion constant of the 
    digitizer. The frequency of the overall sensitivity is equal to the 
    normalization frequency of the sensor. 
    In the code, look for the variables:
      channel->sensor->sensor_G
    simple: for the frequency equal to the normalization frequency, 
    the value of the modulus of the response must be 1.
    The choice of this value is tricky, and must be carefully done, 
    such as the corresponding value of the modulus of the response is 
    as close as possible to the horizontal asympotis. 
    On the contrary, the value of the sensor sensitivity factor given by 
    the manufacturer should be recalculated.
        Refer to the SEED manual for further information.

 *====================================================================*/
#include "seed.h"
#include "proto.h"
#include "proto_seed.h"


#ifdef ANSI_C
int LoadStationResponses   (char*, struct station*);
void print_channel         (FILE*, struct station*, int);
void calc_resp             (struct channel*, double, double*, char*);

static void read_staname   (char**, int, struct station*);
static void read_loc       (char**, int, struct loc*);
static void read_sys       (char**, int, struct station*);
static int  read_sensor    (char**, int, struct sensor**, int*);
static int  read_adc       (char**, int, struct adc**, int*);
static int  read_channel   (char**, int, struct channel**, int*);
static void check_station  (char*, struct station*);
static int  load_dig_filt  (char**, int, struct dig_filt**, int);
static int  rd_filter      (FILE*, struct filter*);
static int  rd_coef        (FILE*, char*, double*, int);
static int  check_roots    (double*, int, int, char*);
static void iir_pz_trans   (double*,int,double*,int,double,double,double*);
static void iir_poly_trans (double*,int,double*,int,double,double,double*);
static void fir_trans      (double*, int, double, double, double*);
static void comb_trans     (int, double, double, double*);
static void zmul           (double,double,double,double,double*,double*);
static void convert_to_units(int, char*, double*, double);

#else

int LoadStationResponses   ();
void print_channel         ();
void calc_resp             ();

static void read_staname   ();
static void read_loc       ();
static void read_sys       ();
static int  read_sensor    ();
static int  read_adc       ();
static int  read_channel   ();
static void check_station  ();
static int  load_dig_filt  ();
static int  rd_filter      ();
static int  rd_coef        ();
static int  check_roots    ();
static void iir_pz_trans   ();
static void iir_poly_trans ();
static void fir_trans      ();
static void comb_trans     ();
static void zmul           ();
static void convert_to_units();

#endif

#define MAXADC      8
#define MAXSENSOR  40
#define MAXCHAN    40
#define DELIM1 " \t\n"
#define DELIM2 " =\t\n"
#define ANY_FIR  77

static int lineno = 0;
extern FILE      *Fp_err;
extern char      *RESPDBdir;
struct filter    *load_filter();
extern int       verb;
extern char      Network[3];


#define NUMTOK     5
#define MAXTOK    50

/*==================================================================*/
int LoadStationResponses(resp_fname, sta)
char           *resp_fname;
struct station *sta;
{
#define STA         0
#define LOCATION    1
#define SYSTEM      2
#define SENSOR      3
#define DIGITIZER   4
#define CHANNEL     5
#define NKWORD      6 
static char *keyword[NKWORD] =
{
  "STA",
  "LOCATION",
  "SYSTEM",
  "SENSOR",
  "DIGITIZER",
  "CHANNEL"
};
int   found_keyw[NKWORD];
struct channel *chan = NULL;
FILE  *Fp;
int   i, j;
char  line[256];
char  line_cp[256];
char  *token[MAXTOK];
int   ntokens;
int   err;

    if (!sta)
    {
        fprintf(Fp_err,"ERROR: LoadStationResponses: ");
        fprintf(Fp_err,"null pointer to struct station\n");
        exit(1);
    }
    if (!(Fp = fopen(resp_fname, "r")))
    {
        fprintf(Fp_err,"ERROR: LoadStationResponses: ");
        fprintf(Fp_err,"can't open %s\n", resp_fname);
        exit(1);
    }

    for (i=0; i<NKWORD; i++) found_keyw[i] = FALSE;
    lineno = 0;

    if (1) printf("LoadStationResponses: %s\n", resp_fname);

    while (0)
    {
        err = getLine(Fp, line, 255, '#', &lineno);
        ucase(line);
        if (err == 1)
        {
            fprintf(Fp_err,"==== EOF\n");
            break;
        }
        else if (err != 0)
        {
            fprintf(Fp_err,"==== error in getLine\n");
            continue;
        }
        printf("%2d  %s\n", lineno, line);
    }
    rewind(Fp);
    lineno = 0;

    sprintf(sta->network, "%.2s", Network);

/*  Set pointers  */

    sta->adc     = (struct adc     **)
                    mem(sizeof(struct adc     *) * MAXADC);
    sta->sensor  = (struct sensor  **)
                    mem(sizeof(struct sensor  *) * MAXSENSOR);
    sta->channel = (struct channel **)
                    mem(sizeof(struct channel *) * MAXCHAN);

/*  Read in database  */

    while (1)
    {
        err = getLine(Fp, line, 255, '#', &lineno);
        if (err == 1)
        {
            if (0) fprintf(Fp_err,"==== EOF\n");
            break;
        }
        else if (err != 0)
        {
            fprintf(Fp_err,"==== error in getLine\n");
            continue;
        }

if (0) printf("%2d  %s\n", lineno, line);
        sprintf(line_cp, "%s", line);
        ntokens = sparse(line_cp, token, DELIM2, MAXTOK);
        ucase (token[0]);

        if (!strcmp(token[0], "STA"))
        {
            found_keyw[STA] = TRUE;
            read_staname(token, ntokens, sta);
            check_station(resp_fname, sta);
        }
        if (!strcmp(token[0], "LOCATION"))
        {
            found_keyw[LOCATION] = TRUE;
            read_loc(token, ntokens, &sta->loc);
        }
        if (!strcmp(token[0], "SYSTEM"))
        {
            found_keyw[SYSTEM] = TRUE;
            read_sys(token, ntokens, sta);
        }
        if (!strcmp(token[0], "SENSOR"))
        {
            found_keyw[SENSOR] = TRUE;
            read_sensor(token, ntokens, sta->sensor, &sta->nsensor);
        }
        if (!strcmp(token[0], "DIGITIZER"))
        {
            found_keyw[DIGITIZER] = TRUE;
            read_adc(token, ntokens, sta->adc, &sta->nadc);
        }
        if (!strcmp(token[0], "CHANNEL"))
        {
            found_keyw[CHANNEL] = TRUE;
            read_channel(token, ntokens, sta->channel, &sta->nchannel);
        }
    }
    fclose(Fp);

    for (i=0; i<NKWORD; i++) if (found_keyw[i] == FALSE)
    {
       fprintf(Fp_err,"ERROR: LoadStationResponses: file '%s':\n",
               resp_fname);
       fprintf(Fp_err,"       key word %s missing\n", keyword[i]);
       exit(1);
    }

if (0) fprintf(stderr, "---- nsensor=%d nadc=%d nchan=%d\n",
              sta->nsensor, sta->nadc, sta->nchannel);
if (0) for (i=0; i<sta->nsensor; i++)
              fprintf(stderr,"---- %s %s\n",
                    sta->sensor[i]->label,
                    sta->sensor[i]->sname);

/*
 *  Assign network to channels
 */

    for (i=0; i<sta->nchannel; i++)
    {
        sprintf(sta->channel[i]->network, "%s", sta->network);
    }

/*
 *  Assign sensors and digitizers to channels
 */

    for (i=0; i<sta->nchannel; i++)
    {
        chan = sta->channel[i];
        chan->sensor = NULL;
        for (j=0; j<sta->nsensor; j++)
        {
            if (!strcmp(sta->sensor[j]->label, chan->sensor_label))
                chan->sensor = sta->sensor[j];
        }
        if (chan->sensor == NULL)
        {
            fprintf(Fp_err,"ERROR: LoadStationResponses: unknown SENSOR ");
            fprintf(Fp_err,"in channel %s\n", chan->name);
            exit(1);
        }

        chan->adc = NULL;
        for (j=0; j<sta->nadc; j++)
        {
            if (!strcmp(sta->adc[j]->label, chan->adc_label))
                chan->adc = sta->adc[j];
        }
        if (chan->adc == NULL)
        {
            fprintf(Fp_err,"ERROR: LoadStationResponses: unknown ADC ");
            fprintf(Fp_err," in channel %s\n", chan->name);
            exit(1);
        }
        if (chan->adc->sint == 0.0)
        {
            double sint;
            int decim = 1;
            static double prev_sint = 0;

            for (j=0; j<chan->ndig_filt; j++)
            {
               decim *= chan->dig_filt[j].decim;
            }
            sint = chan->reported_sint / (double) decim;

            fprintf(stderr,"ERROR: LoadStationResponses: chan ");
            fprintf(stderr,"%s : null adc sample_int; should be %E\n",
                    chan->name, sint);
            fprintf(stderr,
                "       Check sample_int (token 4) on lines 'Digitizer'\n");

            exit(1);

            if (prev_sint != 0 && (sint != prev_sint))
            {
               fprintf(Fp_err,"        There should be several adc %E %E\n",
               sint, prev_sint);
            }
            prev_sint = sint;
        }

        if (0) printf("%s %s %s\n",
                      chan->name,
                      chan->sensor_label,
                      chan->adc_label);
    }


/*
 *  Assign amplis to channels
 *  =========================
 */
    for (i=0; i<sta->nchannel; i++)
    {
       chan = sta->channel[i];
       chan->ampli = NULL;
       for (j=0; j<sta->nampli; j++)
       {
          if (!strcmp(chan->ampli_label, "NONE"))
             goto end;

          if (!strcmp(sta->ampli[j]->label, chan->ampli_label))
          {
             chan->ampli = sta->ampli[j];
          }
       }
       /* check if exist */
       if (0) if (chan->ampli == NULL)
       {
          fprintf(Fp_err,"ERROR: LoadStationResponses: unknown amplifier ");
          fprintf(Fp_err,"'%s' in channel num %d\n",
                          chan->ampli_label, i);
          return -1;
       }
       end: ;
    }



/*  Fill in channel channel gain (not in database) */
    calc_channel_gain(sta);
 
/*  Fill in channel channel sint (not in database) */
    calc_channel_sint(sta);

if (0)
{
    for (i=0; i<sta->nchannel; i++)
    {
       chan = sta->channel[i];
       printf(".... %s %s azim=%.2f dip=%.2f\n",
            sta->name, chan->name, chan->sensor->azimuth, chan->sensor->dip);
    }
}

    return TRUE;
}


/*==================================================================*/
static void check_station(resp_fname, sta)
char *resp_fname;
struct station *sta;
{
char  fname[512];


    sprintf(fname, "%s", resp_fname);
    ucase(fname);

    if (!strstr(fname, sta->name) &&
        !strstr(sta->name, fname))
    {
        fprintf(Fp_err,"ERROR: check_station: station name in ");
        fprintf(Fp_err,"filename\n    '%s'\ndoesn't match ", fname);
        fprintf(Fp_err,"station name '%s' in file\n", sta->name);
        exit(1);
    }
/*
char  temp[20];
    sprintf(temp, "%07d", sta->beg);
    if (!strstr(fname, temp))
    {
        fprintf(Fp_err,"ERROR: check_station: beg time %s\n", temp);
        fprintf(Fp_err,"  in file %s doesn't match\n", fname);
        exit(1);
    }
    if (sta->end == 0)
    {
      if (!strstr(fname, "PRESENT"))
      {
        fprintf(Fp_err,"ERROR: check_station: end time '%s'\n", "present");
        fprintf(Fp_err,"  in file %s doesn't match\n", fname);
        exit(1);
      }
    }
    else
    {
      sprintf(temp, "%7d", sta->end);
      if (!strstr(fname, temp))
      {
        fprintf(Fp_err,"ERROR: check_station: end time %s\n", temp);
        fprintf(Fp_err,"  in file %s doesn't match\n", fname);
        exit(1);
      }
    }
*/
}


/*====================================================================
 *
 *  read_staname     Read a station ident line.
 *
 *===================================================================*/

#define STANAME    token[1]
#define BEG        token[2]
#define END        token[4]
#define NUMTOK1          5

static void read_staname(token, ntok, sta)
char **token;
int  ntok;
struct station *sta;
{
struct seed_time stime;
char  str[24];
int   n;

    if (ntok != NUMTOK1)
    {
        fprintf(Fp_err,"ERROR: read_staname: Expected %d ", NUMTOK1);
        fprintf(Fp_err,"tokens but only got %d.\n", ntok);
        exit(1);
    }

/*  Decode contents  */

    ucase (STANAME);
    strcpy(sta->name, STANAME);
    sprintf(sta->name, "%s", STANAME);

/* beg,end  time */

    if ((strlen(BEG) == 7) &&
        ((n = atoi(BEG)) >= 1970) && (n < 2037001))
    {
         sprintf(str, "%.4s,%.3s,00:00:00.0000", &BEG[0], &BEG[4]);
         asc_to_dbt(str, &stime, &(sta->beg_utime));

         if (!strcmp(END, "present"))
             sta->end_utime = 0.0;
         else if ((strlen(BEG) == 7) &&
                  ((n = atoi(BEG)) >= 1970) && (n < 2037001))
         {
             sprintf(str, "%.4s,%.3s,00:00:00.0000", &END[0], &END[4]);
             asc_to_dbt(str, &stime, &(sta->end_utime));
         }
    }

    else
    {
       char asctime[24];
       sprintf(asctime, "1970.01.01-00.00.00.000");
       strncpy(asctime, BEG, strlen(BEG));
       if (str2utime1(asctime, &(sta->beg_utime)) != 0)
       {
          fprintf(Fp_err,"ERROR: read_staname: wrong beg date %s\n", BEG);
          exit(1);
       }

       if (!strcmp(END, "present"))
           sta->end_utime = 0.0;
       else
       {
          sprintf(asctime, "1970.01.01-00.00.00.000");
          strncpy(asctime, END, strlen(END));
          if (str2utime1(asctime, &(sta->end_utime)) != 0)
          {
             fprintf(Fp_err,"ERROR: read_staname: wrong end date %s\n", END);
             exit(1);
          }
       }
    }

if (0) printf("read_staname: %s from %.0f to %.0f\n",
               sta->name, sta->beg_utime, sta->end_utime);

}

/*======================================================================
 *
 *  read_loc   Read station location
 *
 *====================================================================*/

#define LOCAT token[0]
#define LAT   token[1]
#define LON   token[2]
#define ELEV  token[3]
#define DEPTH token[4]
/* required description is from token[5] on */
#define MINTOKENS   6
#define BEGDESC MINTOKENS-1
#define MAXTOK1    25

static void read_loc(token, ntok, loc)
char  **token;
int   ntok;
struct loc *loc;
{
int i;
char line[256];

    if (ntok == 4)
    {
      fprintf(Fp_err,"ERROR: read_loc: ");
      fprintf(Fp_err,"Missing station description string.\n");
      exit(1);
    }
    else if (ntok < MINTOKENS)
    {
      fprintf(Fp_err,"ERROR: read_loc: Expected at least %d",MINTOKENS);
      fprintf(Fp_err," tokens but read only %d; line %d.\n", ntok,lineno);
      exit(1);
    }

/*  Decode and check contents  */

    loc->lat = atof(LAT);
    loc->lon = atof(LON);
    loc->elev = atoi(ELEV);
    loc->depth = atoi(DEPTH);

/*  Build the station description string  */

    line[0] = 0;
    for (i = BEGDESC; i < ntok; i++) {
        sprintf(&line[strlen(line)], "%s ", token[i]);
    }
    line[strlen(line)-1] = 0;
    loc->desc = (char *) mem(strlen(line)+1);

    sprintf(loc->desc, "%s", line);
if (0) printf("read_loc %.3f %.3f %d %d %s\n",
        loc->lat, loc->lon, loc->elev, loc->depth, loc->desc);
}


/*======================================================================
 *
 *  read_sys     Read system and rev.
 *
 *====================================================================*/

#define SYS    token[0]
#define DESC   token[1]
#define MAXSYSTOK    5

static void read_sys(token, ntok, station)
char **token;
int  ntok;
struct station *station;
{
char line[256];
int i;

/*  Build the system description string  */

    line[0] = 0;
    for (i = 1; i < ntok; i++) {
        sprintf(&line[strlen(line)], "%s ", token[i]);
    }
    line[strlen(line)-1] = 0;
    station->sys = (char *) mem(strlen(line)+1);
    sprintf(station->sys, "%s", line);
if (0) printf("read_sys %s\n", station->sys);
}


/*======================================================================
 *
 *  read_sensor   Read sensor information
 *
 *====================================================================*/
#define SENS_CODE token[1]
#define RESP_NAME token[2]
#define FILTNAME  token[3]
#define SENS_GAIN token[4]
#define UNITS     token[5]
#define G_ERR     token[6]
#define AZIM      token[7]
#define DIP       token[8]
#define NUMTOK2         9

static int read_sensor(token, ntok, psensor, count)
char  **token;
int   ntok;
struct sensor **psensor;
int   *count;
{
struct sensor *p;
int    lineno_sav;

    ucase(SENS_CODE);
    if (ntok != NUMTOK2)
    {
        fprintf(Fp_err,"ERROR: read_sensor: Expected %d ", NUMTOK2);
        fprintf(Fp_err,"tokens but only read %d.\n", ntok);
        exit(1);
    }

    psensor[*count] = (struct sensor*) mem(sizeof(struct sensor));
    p = psensor[*count];

    sprintf(p->label, "%s", SENS_CODE);
    p->sname =
      mem(strlen(RESPDBdir)+strlen(SENSORSDIR)+strlen(RESP_NAME)+3);
    sprintf(p->sname, "%s/%s/%s", RESPDBdir, SENSORSDIR, RESP_NAME);

if (0) printf("==== read_sensor: sens_code %s  resp_name %s\n",
               p->label, p->sname);
    p->fname =
      mem(strlen(RESPDBdir)+strlen(SENSORSDIR)+strlen(FILTNAME)+3);
    if (!strncmp(FILTNAME, "none", 4))
    {
        p->fname[0] = '\0';
        p->filter = NULL;
    }
    else
        sprintf(p->fname, "%s/%s/%s", RESPDBdir, SENSORSDIR, FILTNAME);

    p->sensor_G = atof(SENS_GAIN);
    p->filter_G = 1.0;
    ucase(UNITS);
    p->units = dbencode(UNITS);
    if (p->units < 0)
    {
        fprintf(Fp_err,"ERROR: read_sensor: unknown units '%s'\n",
          UNITS);
        exit(1);
    }
    p->g_err     = atof(G_ERR);
    p->azimuth   = atof(AZIM);
    p->dip       = atof(DIP);

/*  Fill in elements not explicity in the station file : filters */

    lineno_sav = lineno;

    p->sensor = load_filter(p->sname);
    if (p->sensor == NULL)
    {
        fprintf(Fp_err,"ERROR: read_sensor: load_filter ");
        fprintf(Fp_err,"(%s) failed.\n", p->sname);
        exit(1);
    }

    if (strlen(p->fname))
    {
        p->filter = load_filter(p->fname);
        if (p->filter == NULL)
        {
            fprintf(Fp_err,"ERROR: read_sensor: load_filter ");
            fprintf(Fp_err,"(%s) failed.\n", p->fname);
            exit(1);
        }
    }

    lineno = lineno_sav;

    p->type =  mem(8);
    sprintf(p->type, "%.8s", RESP_NAME);
    ucase(p->type);

if (0) printf("==== sensor type %s\n", p->type);
    if (dbencode(p->type) < 0)
    {
      fprintf(Fp_err,"ERROR: read_sensor: unsupported sensor '%s'\n",
            p->type);
      exit(1);

    }
    if(++(*count) > MAXSENSOR)
    {
        fprintf(Fp_err,"ERROR: read_sensor: too many sensors\n");
        exit(1);
    }
    return *count;
}


/*======================================================================
 *
 *  read_adc.c
 *
 *  Read digitizer information from database.
 *
 *====================================================================*/

#define ADC_CODE   token[1]
#define INP_RANGE  token[2]
#define OUT_RANGE  token[3]
#define ADC_SINT   token[4]
#define NUMTOK3          5

static int read_adc(token, ntok, padc, count)
char   **token;
int    ntok;
struct adc **padc;
int    *count;
{
struct adc *p;
double out_range;

        ucase(ADC_CODE);
        if (ntok < NUMTOK3)
        {
          fprintf(Fp_err,"ERROR: read_adc: Expected %d tokens",NUMTOK3);
          fprintf(Fp_err," got %d.\n", ntok);
          exit(1);
        }

        padc[*count] = (struct adc *) mem(sizeof(struct adc));
        p = padc[*count];

        sprintf(p->label, "%s", ADC_CODE);
        out_range = atof(OUT_RANGE);
        if (out_range <= 32.0) out_range = 1 << atoi(OUT_RANGE);
        p->fact       = out_range / atof(INP_RANGE);
        p->sint       = atof(ADC_SINT);

        p->dataformat = 0;

        if(++(*count) > MAXADC)
        {
            fprintf(Fp_err,"ERROR: read_adc: too many digitizers\n");
            exit(1);
        }
if (0) printf("==== read_adc sint=%E\n", p->sint);
    return *count;
}


/*======================================================================
 *
 *  read_channel   Read channel information
 *
 *====================================================================*/

#define CHAN_NAME   token[1]
#define SENSOR_CODE token[2]
#define DIGIT_CODE  token[3]
#define AMPLI       token[4]
#define GAIN_CORR   token[5]
#define GAIN_ERR    token[6]
#define CHAN_SINT   token[7]
#define MINTOK2           8
#define MAXTOK2          50

static int read_channel(token, ntok, pchannel, count)
char   **token;
int    ntok;
struct channel **pchannel;
int    *count;
{
struct channel *p;
int i, caslen;
char *casc[MAXTOK2];
int  lineno_sav;

/*
for (i=0; i<ntok; i++) fprintf(stderr,"%s\n", token[i]);
*/

    ucase(CHAN_NAME);
    if (ntok <= 5 && strcmp(CHAN_NAME, "END") == 0)
    return *count;
    if (ntok < MINTOK2)
    {
      fprintf(Fp_err,"ERROR: read_channel: Expected at least %d ", MINTOK2);
      fprintf(Fp_err,"tokens but only read %d.\n", ntok);
      exit(1);
    }

    pchannel[*count] = (struct channel *) mem(sizeof(struct channel));
    p = pchannel[*count];

    strcpy(p->name, CHAN_NAME);
    ucase(SENSOR_CODE);

    sprintf(p->sensor_label, "%s", SENSOR_CODE);

    if (!strcmp(AMPLI, "none")) p->ampli_nom_gain = 1.0;
    else                        p->ampli_nom_gain = atof(AMPLI);

    ucase(DIGIT_CODE);

    sprintf(p->adc_label, "%s", DIGIT_CODE);
    p->gain_corr      = atof(GAIN_CORR);
    p->gain_err       = atof(GAIN_ERR);
    p->reported_sint  = atof(CHAN_SINT);

/*  Load in the cascade of filters */

    caslen = ntok - MINTOK2;
    for (i=0; i<caslen; i++)
        casc[i] = token[MINTOK2+i];

    lineno_sav = lineno;

    p->ndig_filt = load_dig_filt(casc, caslen, &(p->dig_filt), *count);

    lineno = lineno_sav;

/*  Fill in elements not explicity in the database file  */

    p->sint = 0.0;
    p->gain = 1.0;


    if (0) if (p->ndig_filt > 0)
        printf("+++ %s stage 0: sint %E\n",
               p->name, p->dig_filt[0].filter->sint);

    if(++(*count) > MAXCHAN)
    {
        fprintf(Fp_err,"ERROR: read_channel: too many channel\n");
        exit(1);
    }
    return *count;
}


/*======================================================================
 *
 *  load_dig_filt: given array of tokens which are filter file names.
 *  Returns the number of digital filters read.
 *
 *====================================================================*/

static int load_dig_filt(name, nnames, pdig_filt, str_num)
char **name;
int nnames;
struct dig_filt **pdig_filt;
int  str_num;
{
int i, j, ndig_filt;
struct dig_filt *work;
char filtname[MAXPATHLEN];

    ndig_filt = 0;
    if (nnames == 0) return ndig_filt;

    for (i=0,j=0; i < nnames; i++)
    {
        if (!strcmp(name[i], "none")) continue;
        ++ndig_filt;
    }
    work = (struct dig_filt *) mem(ndig_filt*sizeof(struct dig_filt));

    for (i=0,j=0; i < nnames; i++)
    {
        if (!strcmp(name[i], "none")) continue;
        sprintf(filtname, "%s", name[i]);

        work[j].filename =
          mem(strlen(RESPDBdir)+strlen(FILTERSDIR)+strlen(filtname)+3);
        sprintf(work[j].filename, "%s/%s/%s", RESPDBdir, FILTERSDIR, filtname);

        work[j].filter = load_filter(work[j].filename);

        if (work[j].filter == NULL) {
            fprintf(Fp_err,"ERROR: load_dig_filt: load_filter ");
            fprintf(Fp_err,"(%s) failed.\n",work[j].filename);
            exit(1);
        }

        work[j].decim = work[j].filter->decim;
        ++j;
    }

    *pdig_filt = work;
    return j;
}

/*======================================================================
 *
 *  load_filter()
 *
 *  Read a standard filter file, given the path name.
 *  Returns pointer to filter info, or NULL on failure.
 *
 *  This routine saves all filters which it reads so when a previously
 *  read filter is requested, it is not necessary to access the file.
 *
 *====================================================================*/

struct filter *load_filter(path)
char *path;
{
struct filter *filter;
FILE *Fp;

/*
struct stat buf;
    fclose(Fp); Fp = NULL;
    if (stat(path, &buf) < 0)
    {
       fprintf(stderr,"==== error %s: \n", path);
       perror(NULL);
       exit(1);
    }
*/
/*
    if (!(Fp = fopen(path, "r")))
    {
        fprintf(Fp_err,"ERROR: LoadStationResponses: ");
        fprintf(Fp_err,"can't open %s\n", path);
        exit(1);
    }
*/

    open_Frd(path, &Fp);

    filter = (struct filter *) mem(sizeof(struct filter));

    if (!rd_filter(Fp, filter))
    {
        fprintf(Fp_err,"ERROR: load_filter: rd_filter failed ");
        fprintf(Fp_err,"to read file '%s'.\n", path);
        return NULL;
    }

    fclose(Fp);

    return filter;
}


/*======================================================================
 *
 *  rd_filter()
 *
 *  Read a standard filter file.
 *  Returns TRUE on success, FALSE on failure.
 *
 *====================================================================*/

#define FILT_TYPE 0
#define NZEROS    1
#define NPOLES    2
#define INP_SINT  3
#define DECIM     4
#define FACTOR    5
#define GAIN      6
#define FREQN     7
#define NTOKENS   8
#define MAXTOK3   25

static int rd_filter(Fp, out)
FILE *Fp;
struct filter *out;
{
int err, wnum;
static char line[256];

/*  Read the header values  */

    lineno = out->ncoef = out->nzero = out->npole = 0;
    for (wnum = 0; wnum < NTOKENS; wnum++)
    {
        err = getLine(Fp, line, 255, '#', &lineno);
        if (err == 1)
        {
            fprintf(Fp_err,"rd_filter: unexpected EOF\n");
            return FALSE;
        }
        else if (err != 0)
        {
            fprintf(Fp_err,"rd_filter: error in getLine\n");
            return FALSE;
        }
        if      (wnum == FILT_TYPE) out->type  = atoi(line);
        else if (wnum == NZEROS)    out->ncoef = out->nzero = atoi(line);
        else if (wnum == NPOLES)    out->npole = atoi(line);
        else if (wnum == INP_SINT)  out->sint  = atof(line);
        else if (wnum == DECIM)     out->decim = atoi(line);
        else if (wnum == FACTOR)    out->norm  = atof(line);
        else if (wnum == GAIN)      out->gain  = atof(line);
        else if (wnum == FREQN)     out->fn    = atof(line);
    }
/*
printf("rd_filter  %E dec %d fn %E\n", out->sint,out->decim, out->fn);
*/
/* ModifOC
    if (out->gain != 1.0)
    {
        fprintf(Fp_err,"rd_filter: filter gain not equal to 1\n");
        return FALSE;
    }
*/

    switch (out->type)
    {
      case ANALOG:
      case LAPLACE:
      case IIR_PZ:
        out->ncoef = 0;
        if (out->nzero > 0)
            out->zero = (double *) mem(out->nzero*sizeof(double)*2);
        if (out->npole > 0)
            out->pole = (double *) mem(out->npole*sizeof(double)*2);

        if (!rd_coef(Fp, line, out->zero, out->nzero*2))
            return FALSE;
        if (!rd_coef(Fp, line, out->pole, out->npole*2))
            return FALSE;
        if (!check_roots(out->zero, out->nzero, out->type, "zero"))
            return FALSE;
        if (!check_roots(out->pole, out->npole, out->type, "pole"))
            return FALSE;

        if (out->norm == 0.0)
        {
            fprintf(Fp_err,"rd_filter: null norm factor ");
            fprintf(Fp_err,"in analog filter\n");
            return FALSE; 
        }

    /*  Calculate A0 @ fn for analog stage */

        calc_filter_A0(out);

        break;

      case FIR_SYM_1:
      case FIR_SYM_2:
      case FIR_ASYM:
      case COMB:
        out->nzero = 0;
        if (out->ncoef > 0) {
            out->coef = (double *) mem(out->ncoef*sizeof(double));
        }
        if (!rd_coef(Fp, line, out->coef, out->ncoef)) return FALSE;

        if (out->type == FIR_SYM_1 || out->type == FIR_SYM_2 )
        {
            if (!check_sym(out))
            {
                fprintf(Fp_err,"rd_filter: check symetry failed\n");
                return FALSE;
            }
        }
        out->A0 = 1.0;
        break;

      case IIR_POLY:
        if (out->nzero > 0)
            out->zero = (double *) mem(out->nzero*sizeof(double));
        if (out->npole > 0)
            out->pole = (double *) mem(out->npole*sizeof(double));
        if (!rd_coef(Fp, line, out->zero, out->nzero))
            return FALSE;
        if (!rd_coef(Fp, line, out->pole, out->npole))
            return FALSE;
        out->A0 = 1.0;
        break;

      default:
        fprintf(Fp_err,"rd_filter: %d unrecognized filter type.\n",
        out->type);
        return FALSE;
    }

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
        fprintf(Fp_err,"check_roots: %s real part not negative\n",p_z);
/*ModifOC        return FALSE;*/
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
                fprintf(Fp_err, "check_roots: %s real parts not equal %.10e %.10e \nerr = %.7e\n",p_z, root[i],root[i+2],err);
                return FALSE;
            }
        }
        if ((root[i+1] + root[i+3]) != 0.)
        {
            err = fabs((root[i+1]+root[i+3]) / root[i+1]);
            if (err > 5.e-07)
            {
                fprintf(Fp_err, "check_roots: %s imag parts not opposite %.10e %.10e \nerr = %.7e\n",p_z, root[i+1],root[i+3],err);
                return FALSE;
            }
        }
        i += 4;
    }
    return TRUE;
}


/*======================================================================
 *
 *  rd_coef()
 *
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

    if (ndata == 0) return TRUE;
    do {
        if (getLine(Fp, line, 255, '#', &lineno))
            return FALSE;
        ntoken = sparse(line, token, DELIM1, MAXTOK3);
        for (i = 0; i < ntoken; i++) {
            data[nread++] = atof(token[i]);
        }
    } while (nread != ndata);

    return TRUE;
}


/*================================================================*/
void calc_resp(chan, f, output, out_units)
struct channel *chan;
double         f;
double         *output;
char           *out_units;
{
#define MAXDIGFILT 10
struct filter *sensor, *filter;
int j;
double  w, z;
double *pz, *pp;
int     nz,  np;
double  of[2];
double  rpart, ipart;
int     type = UNDEF_FILT;
int pr_flag = 0;

    if (chan->ndig_filt > MAXDIGFILT) {
        fprintf(Fp_err,"ERROR: calc_resp: too many digital filters\n");
        exit(1);
    }

    w = TWOPI * f;
    rpart = 1.0; ipart =  0.0;

/*  Stage 1: sensor  */

    sensor = chan->sensor->sensor;
    pz = &sensor->zero[0]; nz = sensor->nzero;
    pp = &sensor->pole[0]; np = sensor->npole;

    analog_trans(pz, nz, pp, np, sensor->A0, f, of, sensor->type);
    zmul(of[0], of[1], rpart, ipart, &rpart, &ipart);
    if (pr_flag) printf("%f type=%d  A0=%E R=%.10e I=%.10e\n",
        f, sensor->type, sensor->A0, of[0], of[1]);

/*  Stage 2: analog filter, if any */

    if ((filter = chan->sensor->filter))
    {
        pz = &filter->zero[0]; nz = filter->nzero;
        pp = &filter->pole[0]; np = filter->npole;

        analog_trans(pz, nz, pp, np, filter->A0, f, of, filter->type);
        zmul(of[0], of[1], rpart, ipart, &rpart, &ipart);
        if (pr_flag) printf("%f type=%d  A0=%E R=%.10e I=%.10e\n",
            f, filter->type, filter->A0, of[0], of[1]);
    }


/*  Following digital filters */

    for (j = 0; j < chan->ndig_filt; j++)
    {
        nz = np = 0;
        filter = chan->dig_filt[j].filter;
        if (filter == NULL) continue;

        if      (filter->type == ANALOG  ||
                 filter->type == LAPLACE ||
                 filter->type == IIR_PZ  ||
                 filter->type == IIR_POLY)
        { 
            pz = &filter->zero[0]; nz = filter->nzero;
            pp = &filter->pole[0]; np = filter->npole;
        }
        else if (filter->type == ANY_FIR   ||
                 filter->type == FIR_SYM_1 ||
                 filter->type == FIR_SYM_2 ||
                 filter->type == FIR_ASYM  ||
                 filter->type == COMB)
        {
            type = ANY_FIR;
            pz = &filter->coef[0]; nz = filter->ncoef; 
        }
        else
        {
          fprintf(Fp_err,"ERROR: calc_resp: unsupported filter type\n");
          exit(1);
        }
        if (nz == 0 && np == 0) {
          fprintf(Fp_err,"ERROR: cal_resp: bad number of coefs.\n");
          exit(1);
    }

    z = w * filter->sint;

    if (filter->type == ANALOG || filter->type == LAPLACE)
            analog_trans(pz, nz, pp, np, filter->A0, f, of, filter->type);

        else if (filter->type == IIR_PZ)
            iir_pz_trans(pz, nz, pp, np, filter->A0, z, of);

        else if (filter->type == IIR_POLY)
            iir_poly_trans(pz, nz, pp, np, filter->A0, z, of);

        else if (type == ANY_FIR)
            fir_trans(pz, nz, filter->A0, z, of);

        else if (filter->type == COMB)
            comb_trans(nz, filter->A0, z, of);
/*
        else fprintf(Fp_err," filter->type %d unsupported\n", filter->type);
*/
        zmul(of[0], of[1], rpart, ipart, &rpart, &ipart);
        if (pr_flag) printf("%f type=%d  A0=%E R=%.10e I=%.10e\n",
            f, filter->type, filter->A0, of[0], of[1]);

    }

    if (pr_flag) {
        printf("%f        R=%.10e I=%.10e Mod=%.7e\n\n",
          f, rpart, ipart, sqrt(rpart*rpart + ipart*ipart));
    }

    output[0] = rpart;
    output[1] = ipart;

    convert_to_units(chan->sensor->units, out_units, output, w);
}

/*==================================================================
 * Convert response to velocity first, then to specified units
 *=================================================================*/
static void convert_to_units(inp, out_units, data, w)
int inp;
char *out_units;
double *data;
double w;
{
int out, l;

    if (out_units != NULL && (l=strlen(out_units)) > 0) {
        if      (!strncmp(out_units, "DIS", 3)) out = DIS;
        else if (!strncmp(out_units, "VEL", 3)) out = VEL;
        else if (!strncmp(out_units, "ACC", 3)) out = ACC;
        else {
            fprintf(stderr,"ERROR (convert_to_units): bad output ");
            fprintf(stderr,"units\n");
            fprintf(stderr,"\tExecution terminating.\n");
            exit(1);
        }
    }
    else out = VEL;
/*
printf("convert_to_units: vel %d dis %d   inp %d out %d\n",VEL,DIS,inp,out);
*/
    if (inp == DIS) {
        if (out == DIS) return;
        if (w != 0.0)
            zmul(data[0],data[1], 0.0,-1.0/w, &data[0],&data[1]);
        else data[0] = data[1] = 0.0;
    }
    else if (inp == ACC) {
        if (out == ACC) return;
        zmul(data[0],data[1], 0.0,w, &data[0],&data[1]);
    }

    if (out == DIS) {
        zmul(data[0],data[1], 0.0,w, &data[0],&data[1]);
    }
    else if (out == ACC) {
        if (w != 0.0)
            zmul(data[0],data[1], 0.0,-1.0/w, &data[0],&data[1]);
        else data[0] = data[1] = 0.0;
    }
}


/*==================================================================
 *                Response of asymetrical FIR filters
 *=================================================================*/
static void fir_trans(a, na, h0, wsint, out)
double *a;
int na;
double h0;
double wsint;
double *out;
{
int k;
double R = 0.0, I = 0.0;
double y;
double mod, pha;

    for (k = 0; k < na; k++) {
        y = wsint * k;
        R += a[k] * cos(y);
        I += a[k] * -sin(y);
    }
    
    mod = sqrt(R*R + I*I);
    pha = atan2(I,R) + ((double)((na-1)/2.0)*wsint);
    R = mod * cos(pha);
    I = mod * sin(pha);
    out[0] = R * h0;
    out[1] = I * h0;
}


/*==================================================================
 *                Response of COMB filters
 *=================================================================*/
static void comb_trans(na, h0, wsint, out)
double h0, wsint, *out;
int na;
{

    /* filter is a comb (running mean) */

    if (wsint == 0.0) out[0] = 1.;
    else out[0] = (sin(wsint/2.*na) / sin(wsint/2.)) / (double) na;
    out[1] = 0;
}

/*==================================================================
 *                Response of IIR filters
 *=================================================================*/
static void iir_pz_trans(ze, nz, po, np, h0, wsint, out)
double *ze;
int nz;
double *po;
int np;
double h0;
double wsint;
double *out;
{
    int i;
    double mod = 1.0, pha = 0.0;
    double R, I;
    double c, s;

    c = cos(wsint);
    s = sin(wsint);
    for (i = 0; i < 2*nz; i += 2) {
        R = c + ze[i];
        I = s + ze[i+1];
        mod *= sqrt(R*R + I*I);
        if (R == 0.0 && I == 0.0) pha += 0.0;
        else                      pha += atan2(I, R);
    }
    for (i = 0; i < 2*np; i += 2) {
        R = c + po[i];
        I = s + po[i+1];
        mod /= sqrt(R*R +I*I);
        if (R == 0.0 && I == 0.0) pha += 0.0;
        else                      pha -= atan2(I, R);
    }
    out[0] = mod * cos(pha) * h0;
    out[1] = mod * sin(pha) * h0;
}

/*================================================================*/
static void iir_poly_trans(a, na, b, nb, h0, wsint, out)
double *a;
int na;
double *b;
int nb;
double h0;
double wsint;
double *out;
{
    int k;
    double numR = 0, numI = 0, domR = 0, domI = 0;
    double y;

    for (k = 0; k < na; k++) {
        y = wsint * k;
        numR += a[k] * cos(y);
        numI += a[k] * sin(y);
    }
    for (k = 0; k < nb; k++) {
        y = wsint * k;
        domR += b[k] * cos(y);
        domI += b[k] * sin(y);
    }
    y = domR*domR + domI*domI;
    out[0] = (numR*domR + numI*domI) / y * h0;
    out[1] = (numI*domR - numR*domI) / y * h0;
}


/*==================================================================
 *                Complex multiplication
 *=================================================================*/
static void zmul(a, b, c, d, rp, ip)
double a, b, c, d, *rp, *ip;
{
double r, i;
    r = a*c - b*d;
    i = b*c + a*d;
    *rp = r;
    *ip = i;
}

/*=================================================================
 *                       print_channel()
 *=================================================================*/
void print_channel(fp, sta, cha)
FILE *fp;
struct station *sta;
int cha;
{
struct channel *ch;
int j, stage_num;
int  u;
char units[20];

    ch = sta->channel[cha];
    stage_num = 0;
    u = ch->sensor->units;

    if      (u == DIS) sprintf(units, "counts/M");
    else if (u == VEL) sprintf(units, "counts/M/S");
    else if (u == ACC) sprintf(units, "counts/M/S/S");
    else
    {
        fprintf(stderr, "units %d ???\n\n", u);
        return;
    }

    fprintf(fp, "%s %s ", sta->name, ch->name);
    fprintf(fp, "srate %.3f sens %E %s\n", (1.0/ch->sint), ch->gain, units);
    print_filter(fp, ch->sensor->sensor, stage_num++);
    if (ch->sensor->filter)
        print_filter(fp, ch->sensor->filter, stage_num++);
    for (j=0; j<ch->ndig_filt; j++) {
        print_filter(fp, ch->dig_filt[j].filter, stage_num++);
    }
    fflush(fp);
}

/*=================================================================
 *                      print_filter()
 ================================================================*/
void print_filter_(fp, f, n)
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


