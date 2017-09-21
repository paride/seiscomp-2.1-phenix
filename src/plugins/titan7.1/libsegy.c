/*======================================================================
    libsegy.c

    Library for Titan to SEGY converter

    Author: J.-F. Fels, OMP, Toulouse

    Updates:

*======================================================================*/
#include "titan.h"
#include "proto.h"

#define DEFAULT_GAIN 1
#define DEFAULT_SCALE  1.0


extern int    inp_data[NCHAN][NCOMP][NINP]; /* Input data array */
extern struct Channel Channel[NCHAN];
extern outData OutData[NCHAN];
extern struct option opt;
extern FILE   *Fp_log;
extern char   Station[8];
extern FILE   *Fp_data[NCHAN][NCOMP];
extern char   fname[NCHAN][NCOMP][PATHLEN];

extern int imax[NCHAN][NCOMP];
extern int imin[NCHAN][NCOMP];

#ifdef ANSI_C
static void init_segy_hd(SEGYHEAD*);
static int put_tstatic(double, SEGYHEAD*);
#else
static void init_segy_hd();
static int put_tstatic();
#endif


/*===================================================================*/
void write_segy_head(chan, comp, startime)
int chan;
int comp;
double startime;
{
SEGYHEAD   segy;                 /* SEGY header structure */
int        nsamp;
double     freq;
static char *CP = "ZNE";
char       cp[2];
char       temp[8+1];            /* for character transfer */
long       ltime;                /* UNIX time */
static struct tm *tms;
double sampint;

    nsamp    = OutData[chan].nsamples;
    freq     = OutData[chan].srate;

    sprintf(cp, "%1.1s", &CP[comp]);

/* here's where we set segy header */

    init_segy_hd(&segy);

    ltime = (long) startime;
    tms = gmtime(&ltime);
    segy.year   = segy.trigyear   = tms->tm_year + 1900;
    segy.day    = segy.trigday    = tms->tm_yday + 1;
    segy.hour   = segy.trighour   = tms->tm_hour;
    segy.minute = segy.trigminute = tms->tm_min;
    segy.second = segy.trigsecond = tms->tm_sec;
    segy.m_secs = segy.trigmills  = 
                  (int) ((startime - (double) ltime) * 1000.0);
    segy.data_form = 1;
    segy.sampleLength = 0;
    segy.num_samps = 0;

/* station and component names */
    sprintf(temp, "%-6.6s", Station);
    strncpy(segy.station_name, temp, 5);
    sprintf(temp, "%-4.4s", cp);
    strncpy(segy.channel_name, temp, 3);
    strcpy(segy.sensor_serial, "1234567 ");
/*
 short deltaSample;  Sampling interval in MICROSECONDS (unless == 1)
 When the value is too large to fit in a short, 
 deltaSample become flags and requires its long counterpart,
 samp_rate, to contain that value.
*/

    sampint = 1 / (double) freq;
    segy.samp_rate = rint(sampint * (double) 1000000);
    if (segy.samp_rate <32767) 
        segy.deltaSample = segy.samp_rate;
    else
        segy.deltaSample = 1;

    segy.sampleLength = nsamp;
    segy.num_samps = nsamp;
    if (nsamp>32767)
        segy.sampleLength = 32767;

    segy.lmax = 0;
    segy.lmin = 0;
    segy.lmax = imax[chan][comp];
    segy.lmin = imin[chan][comp];

/* coordinates */
    segy.coordUnits = 2;
/* JFF: Don't know how to convert (double) latitude,longitude into long */
    if (HasStatDbParms())
    {
/*
        segy.recLatOrY    = staparms.lat;
        segy.recLongOrX   = staparms.lon;
        segy.recElevation = staparms.elev;
*/
    }

/* write the SEGY header */

    if (fwrite((char *) &segy, 1, 240, Fp_data[chan][comp]) != 240)
    {
        fprintf(stderr, "\tERROR write_segy_head: write failed\n");
        exit(1);
    }

/* describe the file written */

    if (0 && opt.verb)
    {
        printf("  %s %5d samples min %d max %d ",
            fname[chan][comp], nsamp, imin[chan][comp], imax[chan][comp]);
        printf(" %04d.%03d.%02d:%02d:%02d.%03d\n",
            segy.year,
            segy.day,
            segy.hour,
            segy.minute,
            segy.second,
            segy.m_secs);
    }

    return;
}


/*================================================================*/
static void init_segy_hd(segy)
SEGYHEAD *segy;
{
int gain    = DEFAULT_GAIN;
float scale = DEFAULT_SCALE;

/*
    printf("SEGYHEAD %d bytes\n", sizeof(SEGYHEAD));
*/

    memset((char *) segy, 0, sizeof(SEGYHEAD));

    segy->lineSeq = 1;
    segy->reelSeq = 1;
    segy->channel_number = 0;
    segy->event_number = 0;
    segy->timeBasisCode = 2;
    segy->traceID = 1;
    segy->elevationScale = 1;
    segy->energySourcePt = 0;
    segy->cdpEns = 0;
    segy->traceInEnsemble = 0;
    segy->vertSum = 0;
    segy->horSum = 0;
    segy->dataUse = 0;
    segy->recElevation = 0;
    segy->sourceSurfaceElevation = 0;
    segy->sourceToRecDist = 0;
    segy->sourceDepth= 0;
    segy->datumElevRec = 0;
    segy->datumElevSource = 0;
    segy->sourceWaterDepth = 0;
    segy->recWaterDepth = 0;
    segy->sourceLongOrX = 0;
    segy->sourceLatOrY = 0;
    segy->recLongOrX = 0;
    segy->recLatOrY = 0;
    segy->weatheringVelocity = 0;
    segy->subWeatheringVelocity = 0;
    segy->sourceUpholeTime = 0;
    segy->recUpholeTime = 0;
    segy->sourceStaticCor = 0;
    segy->lagTimeA = 0;
    segy->lagTimeB = 0;
    segy->delay = 0;
    segy->muteStart = 0;
    segy->muteEnd = 0;
    segy->initialGain= 1;
    segy->correlated = 0;
    segy->traceWeightingFactor = 0;
    segy->phoneRollPos1 = segy->phoneFirstTrace = segy->phoneLastTrace = 0;
    segy->gapSize= segy->taperOvertravel = 0;
    segy->sweepStart = segy->sweepEnd = segy->sweepLength = 0;
    segy->sweepType = segy->sweepTaperAtStart = segy->sweepTaperAtEnd = 0;
    segy->taperType = 0;
    segy->aliasFreq = segy->aliasSlope = 0;
    segy->notchFreq = segy->notchSlope = 0;
    segy->lowCutFreq = segy->hiCutFreq = 0;
    segy->lowCutSlope = segy->hiCutSlope = 0;
    put_tstatic((double) 0.0, segy);
    segy->totalStaticHi = 0;
    segy->coordScale = 1;
    segy->coordUnits = 2;
    segy->inst_no = atoi("0");
    segy->scale_fac = scale;
    segy->gainType = 1;
    segy->gainConst = gain;
    segy->year   = segy->trigyear   = 0;
    segy->day    = segy->trigday    = 0;
    segy->hour   = segy->trighour   = 0;
    segy->minute = segy->trigminute = 0;
    segy->second = segy->trigsecond = 0;
    segy->m_secs = segy->trigmills  = 0;
    segy->data_form = 1;
    segy->sampleLength = 0;
    segy->num_samps = 0;
    strcpy(segy->station_name, "STA   ");
    strcpy(segy->sensor_serial, "1234567 ");
    strcpy(segy->channel_name, "CHA ");
    segy->samp_rate = (int) 1000000L;
/* short deltaSample; Sampling interval in MICROSECONDS (unless == 1) */
    if (segy->samp_rate <32767) 
        segy->deltaSample = segy->samp_rate;
    else
        segy->deltaSample = 1;
}


/*================================================================*/
/* Returns FALSE if totStatic is out of int range, otherwise TRUE */
static int put_tstatic(totStatic, head_ptr)
double        totStatic;
SEGYHEAD      *head_ptr;
{
  long adjStatic;
  adjStatic = (int) rint(totStatic);
  if (adjStatic >= -32768 && adjStatic <= 32767) {
  /* just express the shift as a short */
    head_ptr->totalStatic = (short) adjStatic;
    head_ptr->totalStaticHi = 0;
  }
  else if (totStatic >= 0x7FFFFFFF ) {
  /* flag as max that can't be "undone" */
    head_ptr->totalStatic = 0xFFFF; head_ptr->totalStaticHi = 0x7FFF;
    fprintf(stderr,"Warning: %.0f is greater than %d and cannot be \"undone\" b\
y 'segyshift -u ..\'\n",
        totStatic, 0x7FFFFFFE );
    return (FALSE);
  }
  else if ( totStatic < -0x7FFFFFFF ) {
  /* flag as max that can't be "undone" */
    head_ptr->totalStatic = 0; head_ptr->totalStaticHi = 0x8000;
    fprintf(stderr,"Warning: %.0f is less than %d and cannot be \"undone\" by \
'segyshift -u ..\'\n",
        totStatic, -0x7FFFFFFF );
    return (FALSE);
  }
  else {  /* swap bit zones if reqd (see above explanation) */
    if ((adjStatic & 0xFFFF8000) == 0xFFFF8000) adjStatic += 65536;
    else if ((adjStatic & 0xFFFF8000) == 0x8000) adjStatic -= 65536;
    head_ptr->totalStatic = (short) (((unsigned long) adjStatic) & 0xFFFF) ;
    head_ptr->totalStaticHi = (short) (((unsigned long) adjStatic) >> 16) ;
  }
  return (TRUE);
}


/*
typedef struct SegyHead {   *  Offset Description  *
   long  lineSeq;           *   0 Sequence numbers within line *
   long  reelSeq;           *   4 Sequence numbers within reel *
   long  event_number;      *   8 Original field record num or trigger num *
   long  channel_number;    *  12 Trace num within the original field record*
   long  energySourcePt;    *  16 X *
   long  cdpEns;            *  20 X *
   long  traceInEnsemble;   *  24 X *
   short traceID;           *  28 Trace id code: seismic data = 1 *
   short vertSum;           *  30 X *
   short horSum;            *  32 X *
   short dataUse;           *  34 X *
   long  sourceToRecDist;   *  36 X *
   long  recElevation;      *  40 X *
   long  sourceSurfaceElevation; *  44 X *
   long  sourceDepth;       *  48 X *
   long  datumElevRec;      *  52 X *
   long  datumElevSource;   *  56 X *
   long  sourceWaterDepth;  *  60 X *
   long  recWaterDepth;     *  64 X *
   short elevationScale;    *  68 Elevation Scaler: scale = 1 *
   short coordScale;        *  70 Coordinate Scaler: scale = 1 *
   long  sourceLongOrX;     *  72 X *
   long  sourceLatOrY;      *  76 X *
   long  recLongOrX;        *  80 X *
   long  recLatOrY;         *  84 X *
   short coordUnits;        *  88 Coordinate Units:  = 2 (LatLong) *
   short weatheringVelocity;*  90 X *
   short subWeatheringVelocity; *  92 X *
   short sourceUpholeTime;  *  94 X *
   short recUpholeTime;     *  96 X *
   short sourceStaticCor;   *  98 X *
   short recStaticCor;      * 100 X *
   short totalStatic;       * 102 Total Static in MILLISECS added *
   short lagTimeA;          * 104 X *
   short lagTimeB;          * 106 X *
   short delay;             * 108 X *
   short muteStart;         * 110 X *
   short muteEnd;           * 112 X *
   short sampleLength;      * 114 Number of samples (unless == 32767) *
   short deltaSample;       * 116 Sampling interval in MICROSECONDS  *
   short gainType;          * 118 Gain Type: 1 = Fixed Gain *
   short gainConst;         * 120 Gain of amplifier *
   short initialGain;       * 122 X *
   short correlated;        * 124 X *
   short sweepStart;        * 126 X *
   short sweepEnd;          * 128 X *
   short sweepLength;       * 130 X *
   short sweepType;         * 132 X *
   short sweepTaperAtStart; * 134 X *
   short sweepTaperAtEnd;   * 136 X *
   short taperType;         * 138 X *
   short aliasFreq;         * 140 X *
   short aliasSlope;        * 142 X *
   short notchFreq;         * 144 X *
   short notchSlope;        * 146 X *
   short lowCutFreq;        * 148 X *
   short hiCutFreq;         * 150 X *
   short lowCutSlope;       * 152 X *
   short hiCutSlope;        * 154 X *
   short year;              * 156 year of Start of trace *
   short day;               * 158 day of year at Start of trace *
   short hour;              * 160 hour of day at Start of trace *
   short minute;            * 162 minute of hour at Start of trace *
   short second;            * 164 second of minute at Start of trace *
   short timeBasisCode;     * 166 Time basis code: 2 = GMT *
   short traceWeightingFactor; * 168 X *
   short phoneRollPos1;     * 170 X *
   short phoneFirstTrace;   * 172 X *
   short phoneLastTrace;    * 174 X *
   short gapSize;           * 176 X *
   short taperOvertravel;   * 178 X *
   char  station_name[6];   * 180 Station Name code (5 chars + \0) *
   char  sensor_serial[8];  * 186 Sensor Serial code (7 chars + \0) *
   char  channel_name[4];   * 194 Channel Name code (3 chars + \0) *
   short totalStaticHi;     * 198 Total Static in MILLISECS added to Trace Start Time (high 2 bytes)*
   long  samp_rate;         * 200 Sample interval in MICROSECS as a 32 bit integer *
   short data_form;         * 204 Data Format flag: 0=16 bit, 1=32 bit integer *
   short m_secs;            * 206 MILLISECONDS of seconds of Start of trace *
   short trigyear;          * 208 year of Trigger time *
   short trigday;           * 210 day of year at Trigger time *
   short trighour;          * 212 hour of day at Trigger time *
   short trigminute;        * 214 minute of hour at Trigger time *
   short trigsecond;        * 216 second of minute at Trigger time *
   short trigmills;         * 218 MILLISECONDS of seconds of Trigger time *
   float scale_fac;         * 220 Scale Factor (IEEE 32 bit float) *
   short inst_no;           * 224 Instrument Serial Number *
   short not_to_be_used;    * 226 X *
   long  num_samps;         * 228 Number of Samples as a 32 bit integer
                             * (when sampleLength == 32767) *
   long  max;               * 232 Maximum value in Counts *
   long  min;               * 236 Minimum value in Counts *
} SEGYHEAD;                 * end of segy trace header *
*/
