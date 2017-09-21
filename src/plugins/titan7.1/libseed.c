/*======================================================================
    libseed.c

    Library for Titan to binary data converter

    Author: J.-F. Fels, OMP, Toulouse

*======================================================================*/
#include "seed.h"
#include "proto_seed.h"


extern FILE    *Fp_err;        /* file ptr to output SEED log file */
extern FILE    *Fp_log;



/*===================================================================*/
/* Convert time structure into double UNIX time                      */
/*===================================================================*/
void tstruct_to_dbt(time, dbltime)
struct seed_time *time;
double *dbltime;
{
int year;
double nsec;

    nsec = 0.0;
    for (year=1970; year<(time->year); year++)
    {
        if (isaleap(year)) nsec += 366.0 * 86400.0;
        else               nsec += 365.0 * 86400.0;
    }
    if (time->day == 0)
    {
        fprintf(Fp_log,"WARNING: tstruct_to_dbt: day number is null; ");
        fprintf(Fp_log,"set to 1\n");
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
 *  Make time string from time structure                             *
 *===================================================================*/
void time_asc(time, str)
struct seed_time *time;
char *str;
{
    sprintf(str, "%04d,%03d,%02d:%02d:%02d.%04d",
        time->year,
        time->day,
        time->hour,
        time->minute,
        time->second,
        time->fracsec
    );
}


/*===================================================================*/
void dbt_to_tstruct(dbltime, time)
double dbltime;
struct seed_time *time;
{
static struct tm *tms;
long ltime = (long) dbltime;

    tms = gmtime(&ltime);

    time->year   = (short) tms->tm_year + 1900;
    time->day    = (short) tms->tm_yday + 1;
    time->hour   = (char)  tms->tm_hour;
    time->minute = (char)  tms->tm_min;
    time->second = (char)  tms->tm_sec;
    time->fracsec = (short) ((dbltime - (double) ltime) * 10000.0);
}


/*===================================================================*/
void asc_to_dbt(str, stime, dbltime)
char  *str;
struct seed_time *stime;
double *dbltime;
{
int    i,j,k;
char str1[24];

    sprintf(str1, "0000,000,00:00:00.0000");
    strncpy(str1, str, strlen(str));   /* Do not use sprintf here */

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
void dbt_to_asc(dbltime, str)
double dbltime;
char *str;
{
static struct tm *tms;
long ltime = (long) dbltime;

    tms = gmtime(&ltime);

    sprintf(str, "%04d,%03d,%02d:%02d:%02d.%04d",
        tms->tm_year + 1900,
        tms->tm_yday + 1,
        tms->tm_hour,
        tms->tm_min,
        tms->tm_sec,
        (short) ((dbltime - (double) ltime) * 10000.0)
    );
}


/*==================================================================*
    The first 3 lines of code below doesn't work, and I added some code.
    See Reference Manual SEED Format Version 2.3 page 94.
    Some other changes have to be made in the future.
 *==================================================================*/
int seed_srate(srate, hdr)
double srate;
struct seed_data_hdr *hdr;
{
double sint = 1.0/srate;
double mult;
int    lrate, k;

    if (0 && sint > 1.0)
    {
        hdr->sample_rate = (short) -sint;
        hdr->sample_rate_multiplier = 1;
if (1) printf("==== seed_srate: sint=%.8f   rate %d mult %d\n",
           sint,
           hdr->sample_rate,
           hdr->sample_rate_multiplier);

        return 0;
    }

    mult = 1.0;
    k = 0;
    while (1)
    {
        lrate = rint(srate*mult);
        if (fabs(((double) lrate/mult) - srate) < 1.e-5 || k++ > 3) break;
if (0) printf("==== lrate %d mult %.1f\n", lrate, mult);
        mult *= 10.0;
    }
if (0) printf("==== lrate %d mult %.1f\n", lrate, mult);
if (0) printf("==== seed_srate: mult %.1f -> %.1f %.1f\n",
           mult, ((double) lrate * mult), (srate * mult));

    if (k > 4)
    {

        printf("ERROR: seed_srate failed: multiplier too big\n");
        printf("      sint=%E mult=%d srate(int)=%d\n",sint,(int)mult,lrate);

        hdr->sample_rate = 0;
        hdr->sample_rate_multiplier = 0;
        return 1;
    }
    k = rint(srate * mult);
    if (k > 32766)
    {

        printf("ERROR: seed_srate failed: srate (int) too big\n");
        printf("      sint=%E mult=%d srate(int)=%d\n",sint,(int)mult,lrate);

        hdr->sample_rate = 0;
        hdr->sample_rate_multiplier = 0;
        return 1;
    }

/* If sample rate is greater than 1 and not integer */
    if (mult > 1.0)
    {
        hdr->sample_rate = (short) rint(srate * mult);
        hdr->sample_rate_multiplier = (short) (-mult);
    }
    else
    {
        hdr->sample_rate = (short) rint(srate);
        hdr->sample_rate_multiplier = (short) (mult);
    }

if (0) printf("==== seed_srate: sint=%.8f   rate %d mult %d\n",
           sint,
           hdr->sample_rate,
           hdr->sample_rate_multiplier);

    return 0;
}

/*====================================================================
 *  get_channel_gain. Given a station, a channel, and beg-end dates,
 *  get the channel overall sensitivity.
 *===================================================================*/
void get_channel_gain(station, channel, begtime, endtime, gain)
char   *station;
char   *channel;
double begtime;
double endtime;
double *gain;
{
extern struct station *stalist_head;
struct station *sta;
struct seed_time st;
double sta_beg, sta_end;
int    foundSta, foundCha, foundDates;
int    beg, end;
int    i;

    *gain = 0.0;
    foundSta = foundCha = foundDates = FALSE;
    st.hour = st.minute = st.second = st.fracsec = 0;

    for (sta=stalist_head; sta!=NULL; sta=sta->next)
    {
       if (!strcmp(sta->name, station))
       {
          foundSta = TRUE;
          for (i=0; i<sta->nchannel; i++)
          {
             if (!strcmp(sta->channel[i]->name, channel));
             {
                foundCha = TRUE;
                sta_beg = sta->beg_utime;
                if (sta->end_utime == 0) sta_end = END_OF_WORLD;
                else                     sta_end = sta->end_utime;

                if (begtime >= sta_beg &&
                    endtime <= sta_end)
                {
                   foundDates = TRUE;
                   *gain = sta->channel[i]->gain;
                   return;
                }
             }
          }
       }
    }
    fprintf(Fp_err,"WARNING: get_channel_gain: ");
    fprintf(Fp_err,"    %s: %s",
        station, ((foundSta == TRUE) ? "OK" : "??"));
    fprintf(Fp_err,"    %s: %s",
        channel, ((foundCha == TRUE) ? "OK" : "??"));
    fprintf(Fp_err,"    %d-%d: %s",
        beg, end, ((foundDates == TRUE) ? "OK" : "??"));
    fprintf(Fp_err,"\n");

}



/*====================================================================
 *  calc_channel_gain: Compute channel overall gain.
 *===================================================================*/
int calc_channel_gain(sta)
struct station *sta;
{
int i, j;
struct channel *chan = NULL;
double gain;

/*========  Check all gains which must have a unity value =======*/

if (0) printf("==== calc_channel_gain: %d channels\n", sta->nchannel);

    for (i=0; i<sta->nchannel; i++)
    {
       chan = sta->channel[i];

    /* Check sensor */

       if (chan->sensor->sensor->gain != 1.0)
       {
          fprintf(Fp_err,"ERROR: calc_channel_gain: ");
          fprintf(Fp_err,"sensor->sensor->gain not 1\n");
          return FALSE;
       }

    /* amplifier */

       if (chan->ampli)
       {
if (0) printf("==== calc_channel_gain: chan=%d ampli_label=%s gain=%E\n",
               i, chan->ampli->label, chan->ampli->ampli_G);
       }

    /* Check anti-aliasing filter gain, if any */

       if (chan->sensor->filter)
       {
          if (chan->sensor->filter->gain != 1.0 ||
              chan->sensor->filter_G     != 1.0)
          {
             fprintf(Fp_err,"ERROR: calc_channel_gain: ");
             fprintf(Fp_err,"sensor->filter gains not 1\n");
             return FALSE;
          }
       }

    /* Check digital filter gain, if any */

       if (chan->ndig_filt)
       {
          for (j=0; j<chan->ndig_filt; j++)
          {
             if (chan->dig_filt[j].filter->gain != 1.0)
             {
                fprintf(Fp_err,"ERROR: calc_channel_gain: ");
                fprintf(Fp_err,"dig_filt->filter->gain not 1\n");
                return FALSE;
             }
          }
       }
    }

/*======== Now calculate channel gain ========*/

    for (i=0; i<sta->nchannel; i++)
    {
        chan = sta->channel[i];

/* channel sensor filter */
        gain  = chan->sensor->sensor_G;

/* channel ampli */
        if (chan->ampli)
           gain *= chan->ampli->ampli_G;
/* channel digitizer */
        gain *= chan->adc->fact;
        chan->gain = gain * chan->ampli_nom_gain * chan->gain_corr;
if (0)
{
   printf("    chan=%s  ", chan->name);
   printf("sensor=%.4E  ", chan->sensor->sensor_G);
   if (chan->ampli)
       printf("ampli=%.4E  ", chan->ampli->ampli_G);
   printf("adc=%.4E  ",chan->adc->fact);
   printf("--> %.4E\n", chan->gain);
}

    }
    return TRUE;
}

/*====================================================================
 *  calc_channel_sint: Compute channel sample interval.
 *===================================================================*/
int calc_channel_sint(sta)
struct station *sta;
{ 
int i, k;
struct channel *chan = NULL;
double sint = 0.0;
int    dbug = 0;

    for (i=0; i<sta->nchannel; i++)
    {
        chan = sta->channel[i];

if (dbug) printf("    chan=%s rep_sint=%.6E sint=%.6E\n",
              chan->name, chan->reported_sint, chan->sint);

        if (chan->reported_sint == 0.0)
        { 
            fprintf(Fp_err,"ERROR: calc_channel_sint: %s %s : ",
                    sta->name, chan->name);
            fprintf(Fp_err,"missing reported sint\n");
            return FALSE;
        }

        if (chan->sint == chan->reported_sint)
            return TRUE;

        if (chan->adc->sint == 0.0)
        { 
            fprintf(Fp_err,"ERROR: calc_channel_sint: %s : ", chan->name);
            fprintf(Fp_err,"null adc sint\n");
            return FALSE;
        }

/*
 * calculate chan->sint
 */
        if (chan->sint == 0.0)
        {
            sint = chan->adc->sint;
            for (k=0; k<chan->ndig_filt; k++)
            {
if (dbug) printf("        %d decim=%d\n", k, chan->dig_filt[k].decim);
                    sint *= (double) chan->dig_filt[k].decim;
            }
            chan->sint = sint;
        }

        if (chan->sint != chan->reported_sint)
        {
            fprintf(Fp_err,"ERROR: calc_channel_sint: chan %s : ",
                chan->name);
            fprintf(Fp_err,"sint %.10E differ from reported: %.10E\n",
                chan->sint, chan->reported_sint);
            fprintf(Fp_err,"diff %.10E \n",
              (chan->sint - chan->reported_sint));
            return FALSE;
        }

    }
    return TRUE;
}

/*======================================================================
 *=====================================================================*/
void check_sensor_orient(sta)
struct station *sta;
{
int i;
int error = 0;
struct channel *chan;

    for (i=0; i<sta->nchannel; i++)
    {
       chan = sta->channel[i];
if (0) printf(".... %s %s %s azim=%.2f dip=%.2f\n",
            sta->name, chan->name, chan->sensor->type,
            chan->sensor->azimuth, chan->sensor->dip);

       if (chan->name[2] == 'Z')
       {
          if (fabs(chan->sensor->azimuth) > 0.01)        error = 1;
          if (fabs(chan->sensor->dip - (-90.0)) > 0.01)  error = 1;
       }
       if (chan->name[2] == 'N')
       {
          if (fabs(chan->sensor->azimuth) > 0.01)        error = 1;
          if (fabs(chan->sensor->dip) > 0.01)            error = 1;
       }
       if (chan->name[2] == 'E')
       {
          if (fabs(chan->sensor->azimuth - 90.0) > 0.01) error = 1;
          if (fabs(chan->sensor->dip) > 0.01)            error = 1;
       }
    }

    if (error) fprintf(Fp_err,"\n");

    for (i=0; i<sta->nchannel; i++)
    {
       chan = sta->channel[i];
       if (chan->name[2] == 'Z')
       {
          if (fabs(chan->sensor->azimuth) > 0.01)
          {
             fprintf(Fp_err,"\tWARNING: %s %s azim=%.2f ",
                     sta->name, chan->name, chan->sensor->azimuth);
             fprintf(Fp_err,"differ from expected=0.0 (Seed convention)\n");
          }
          if (fabs(chan->sensor->dip - (-90.0)) > 0.01)
          {
             fprintf(Fp_err,"\tWARNING: %s %s dip=%.2f ",
                     sta->name, chan->name, chan->sensor->dip);
             fprintf(Fp_err,"differ from expected=-90.0 (Seed convention)\n");
          }
       }
       if (chan->name[2] == 'N')
       {
          if (fabs(chan->sensor->azimuth) > 5.0)
          {
             fprintf(Fp_err,"\tWARNING: %s %s azim=%.2f ",
                     sta->name, chan->name, chan->sensor->azimuth);
             fprintf(Fp_err,"differ from expected=0.0 (Seed convention)\n");
          }
          if (fabs(chan->sensor->dip) > 0.01)
          {
             fprintf(Fp_err,"\n");
             fprintf(Fp_err,"\tWARNING: %s %s dip=%.2f ",
                     sta->name, chan->name, chan->sensor->dip);
             fprintf(Fp_err,"differ from expected=0.0 (Seed convention)\n");
          }
       }
       if (chan->name[2] == 'E')
       {
          if (fabs(chan->sensor->azimuth - 90.0) > 5.0)
          {
             fprintf(Fp_err,"\tWARNING: %s %s azim=%.2f ",
                     sta->name, chan->name, chan->sensor->azimuth);
             fprintf(Fp_err,"differ from expected=90.0 (Seed convention)\n");
          }
          if (fabs(chan->sensor->dip) > 0.01)
          {
             fprintf(Fp_err,"\n");
             fprintf(Fp_err,"\tWARNING: %s %s dip=%.2f ",
                     sta->name, chan->name, chan->sensor->dip);
             fprintf(Fp_err,"differ from expected=0.0 (Seed convention)\n");
          }
       }
    }
    if (error) fprintf(Fp_err,"\n");
}

/*=================================================================
 *                       check_sym()
 *   Rewritting of symmetrical filters removed 5 June 1996
 *================================================================*/
int check_sym(f)
struct filter *f;
{
int nc, n0, k;
double sum = 0.0;

    nc = f->ncoef;

/* CHECK IF IF FILTER IS NORMALIZED TO 1 AT FREQ 0 */

    for (k=0; k<nc; k++) sum += f->coef[k];
/*
printf("check_sym: type %d, %d coef, sum %E\n", f->type, f->ncoef, sum);
*/
    if (sum < (1.0-FIR_NORM_TOL) || sum > (1.0+FIR_NORM_TOL)) {
        printf("WARNING: FIR normalized: sum[coef]=%E\n", sum);
        for (k=0; k<nc; k++) f->coef[k] /= sum;
    }

/* CHECK IF FILTER IS SYMETRICAL WITH EVEN NUM OF WEIGHTS */

    if ((nc%2) == 0)
    {
        n0 = nc / 2;
        for (k=0; k < n0; k++)
        {
            if (f->coef[n0+k] != f->coef[n0-k-1])
            {
                fprintf(Fp_err,"check_sym: coefficients are not ");
                fprintf(Fp_err,"symmetrical\n");
                fprintf(Fp_err,"n=%d coef=%.8E\n", n0+k, f->coef[n0+k]);
                fprintf(Fp_err,"n=%d coef=%.8E\n", n0-k-1, f->coef[n0-k-1]);
                return FALSE;
            }
        }
        return TRUE;
    }

/* CHECK IF FILTER IS SYMETRICAL WITH ODD NUM OF WEIGHTS */

    else
    {
        n0 = (nc - 1) / 2;
        for (k=1; k<nc-n0; k++)
        {
            if (f->coef[n0+k] != f->coef[n0-k])
            {
                fprintf(Fp_err,"check_sym: coefficients are not ");
                fprintf(Fp_err,"symmetrical\n");
                fprintf(Fp_err,"n=%d coef=%.8E\n", n0+k, f->coef[n0+k]);
                fprintf(Fp_err,"n=%d coef=%.8E\n", n0-k, f->coef[n0-k]);
                return FALSE;
            }
        }
        return TRUE;
    }
}

/*================================================================*/
void calc_filter_A0(filter)
struct filter *filter;
{
double *pz, *pp;
int nz, np;
double of[2];

    pz = &filter->zero[0]; nz = filter->nzero;
    pp = &filter->pole[0]; np = filter->npole;

    analog_trans(pz, nz, pp, np, 1.0, filter->fn, of, filter->type);
    filter->A0 = 1.0 / sqrt(of[0]*of[0] + of[1]*of[1]);
}

/*==================================================================
 *                Response of analog filter
 *=================================================================*/
void analog_trans(ze, nz, po, np, h0, f, out, type)
double *ze;
int nz;
double *po;
int np;
double h0;
double f;
double *out;
int type;
{
    int i;
    double mod = 1.0, pha = 0.0;
    double R, I;

    if (type == LAPLACE) f = TWOPI * f;

    for (i = 0; i < 2*nz; i += 2) {
        R = -ze[i];
        I = -ze[i+1];
        if (R == 0.0 && I == 0.0) { mod *= f; pha += PI/2.0; }
        else {
            mod *= sqrt((f+I)*(f+I) + R*R);
            pha += atan2(f+I, R);
        }
    }
    for (i = 0; i < 2*np; i += 2) {
        R = -po[i];
        I = -po[i+1];
        if (R == 0.0 && I == 0.0) { mod /= f; pha -= PI/2.0; }
        else {
            mod /= sqrt((f+I)*(f+I) + R*R);
            pha -= atan2(f+I, R);
        }
    }
    out[0] = mod * cos(pha) * h0;
    out[1] = mod * sin(pha) * h0;
}

/*==================================================================
 *  dbencode
 *=================================================================*/
int dbencode(string)
char *string;
{
    if (string == NULL) return -1;

    if (strcmp(string, "ACC")            == 0) return ACC;
    if (strcmp(string, "DIS")            == 0) return DIS;
    if (strcmp(string, "VEL")            == 0) return VEL;

    if (strncmp(string, "STS1", 4)       == 0) return STS1;
    if (strncmp(string, "STS2", 4)       == 0) return STS2;
    if (strncmp(string, "CMG3T", 4)      == 0) return CMG3T;
    if (strncmp(string, "CMG3ESP", 4)    == 0) return CMG3ESP;
    if (strncmp(string, "CMG5", 4)       == 0) return CMG5;
    if (strncmp(string, "CMG40", 5)      == 0) return CMG40;
    if (strncmp(string, "GS13", 4)       == 0) return GS13;
    if (strncmp(string, "LE3D_5S", 4)    == 0) return LE3D_5S;
    if (strncmp(string, "LE3D_20S", 4)   == 0) return LE3D_20S;
    if (strncmp(string, "L4C",  3)       == 0) return L4C;
    if (strncmp(string, "L22",  3)       == 0) return L22;
    if (strncmp(string, "STS1POS", 7)    == 0) return STS1POS;
    if (strncmp(string, "STS2POS", 7)    == 0) return STS2POS;
    if (strncmp(string, "SDJ_S2A", 7)    == 0) return SDJ_S2A;
    if (strncmp(string, "ES_T", 4)       == 0) return ES_T;

    fprintf(stderr,"WARNING: dbdecode: unknown input code: %s\n",string);
    return(UNKNOWN);
}

/*==================================================================
 *  dbdecode
 *=================================================================*/
char *dbdecode(code)
int code;
{
    if (code == DIS)           return "DIS";
    if (code == VEL)           return "VEL";
    if (code == ACC)           return "ACC";
    if (code == ANALOG)        return "ANALOG";
    if (code == LAPLACE)       return "LAPLACE";
    if (code == FIR_SYM_1)     return "FIR_SYM_1";
    if (code == FIR_SYM_2)     return "FIR_SYM_2";
    if (code == FIR_ASYM)      return "FIR_ASYM";
    if (code == COMB)          return "COMB";
    if (code == IIR_PZ)        return "IIR_PZ";
    if (code == IIR_POLY)      return "IIR_POLY";
    if (code == UNDEF_FILT)    return "UNDEF_FILT";
    if (code == 0)    return "";

    fprintf(stderr,"WARNING: dbencode: unknown input code: %d\n",code);
    return NULL;
}

