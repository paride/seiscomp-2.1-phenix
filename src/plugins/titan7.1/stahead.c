/*=====================================================================
 *
 *  stahead.c  SEED Station Control Headers
 *
 *====================================================================*/
#include "seed.h"
#include "proto.h"
#include "proto_seed.h"

#ifdef ANSI_C
void MakeStationHeaders  (struct station*);
void blk_030             (void);
void blk_033             (void);
void blk_034             (void);
void blk_031_S           (void);
void blk_031_C           (void); 
void add_blockette       (char*, char);

static void blk_050          (struct station*);
static void blk_051          (char*, char*, int);
static void blk_052          (struct station*, struct channel*);
static void blk_053          (struct filter*, int, int, int);
static void blk_054          (struct filter*, int, int, int);
static void blk_057          (struct dig_filt*, int);
static void blk_058          (double, double, int);
static void blk_059          (char*, char*, int);
static void get_sta_comments (struct station*);
static void get_sta_cha_comments(struct station*, struct channel*);

#else
void MakeStationHeaders  ();
void blk_030             ();
void blk_033             ();
void blk_034             ();
void blk_031_S           ();
void blk_031_C           ();
void add_blockette       ();

static void blk_050          ();
static void blk_051          ();
static void blk_052          ();
static void blk_053          ();
static void blk_054          ();
static void blk_057          ();
static void blk_058          ();
static void blk_059          ();
static void get_sta_comments ();
static void get_sta_cha_comments();
#endif

#define  VELOC  1
#define  ACCEL  2 
#define  VOLT   3
#define  COUNT  4 


char   *cmts_dir = NULL;  /* Stations-Channels Comments directory name */

extern struct lrec *VH_list_head;
extern struct lrec *VH_list_tail;
extern struct lrec *AH_list_head;
extern struct lrec *AH_list_tail;
extern struct lrec *SH_list_head;
extern struct lrec *SH_list_tail;
extern struct lrec *TH_list_head;
extern struct lrec *TH_list_tail;
extern int      nVH, nAH, nSH, nTH;
extern char     blk[10000];
extern char     logrec[5000];
extern int      nc;


extern struct data_segment *dsegm_head;
struct inp_cmt      *sta_cha_cmt_head;
struct inp_cmt      *inp_sta_cmt_head;
struct sta_cmt      *sta_cmt_head;


extern FILE     *Fp_err;
extern char     *cmts_dir;
extern char     blk[10000];
extern char     *token[];
extern int      ntokens;
extern double   SeedClockTolerance;
extern int      SeedDataformat;

/*=================================================================*/
/*            Stations Control Headers                             */
/*=================================================================*/
void MakeStationHeaders(sta)
struct station  *sta;
{
int  i;

if (1)
{
    struct channel *chan;
    char s1[24], s2[24];

    dbt_to_asc(sta->beg_utime, s1);
    if (sta->end_utime == 0)
        sprintf(s2, "present          ");
    else
        dbt_to_asc(sta->end_utime, s2);

    printf("  MakeStationHeaders: %s %s %.17s-%.17s chan: ",
           sta->name, sta->network, s1, s2);

    for (i=0; i<sta->nchannel; i++)
    {
       chan = sta->channel[i];
       if (chan->name[2] == 'Z')
       {
          printf("%.2s ", chan->name);
       }
    }
    printf("\n");
}

/* Write logical record number into structure */

    sta->lrec_num = nSH;

/* start new logical record */

    add_blockette("init", 'S');

/* Station blockette */

    blk_050(sta);

/* Station comment blockettes */

    get_sta_comments(sta);
    get_sta_cha_comments(sta, NULL);

/* Channels blockettes */

    for (i=0; i<sta->nchannel; i++)
    {
        blk_052(sta, sta->channel[i]);
    }

/* Flush out last log rec */
    add_blockette("end", 'S');

}


/*===================================================================*
 *  Build blockette 50
 *===================================================================*/
static void blk_050(sta)
struct station *sta;
{
int j;
char temp[24];
char begtime[24], endtime[24];
int lookup_code;

    if (strlen(NETWORK) <= 0 || strlen(NETWORK) > 2)
    {
        fprintf(stderr,
           "ERROR: blk_050: network code '%s' must be 1 or 2 char long\n",
           NETWORK);
        exit(1);
    }


    if (strlen(sta->network) <= 0 || strlen(sta->network) > 2)
    {
       fprintf(stderr,
           "ERROR: blk_050: wrong network code '%s'; using default: '%s'\n",
            sta->network, NETWORK);

       sprintf(sta->network, "%.2s", NETWORK);
    }


    dbt_to_asc(sta->beg_utime, temp);
    sprintf(begtime, "%.17s~", temp);
    if (sta->end_utime == 0.0) sprintf(endtime, "~");
    else
    {
        dbt_to_asc(sta->end_utime, temp);
        sprintf(endtime, "%.17s~", temp);
    }

    lookup_code = 1;

    j = 0;
    sprintf(&blk[j], "0500000");                         j += 7;
    sprintf(&blk[j], "     ");
    strncpy(&blk[j], sta->name, strlen(sta->name));      j += 5;
    sprintf(&blk[j], "%+10.6f", sta->loc.lat);           j += 10;
    sprintf(&blk[j], "%+11.6f", sta->loc.lon);           j += 11;
    sprintf(&blk[j], "%+7.1f",  (float) sta->loc.elev);  j += 7;
    sprintf(&blk[j], "    ");                            j += 4;
    sprintf(&blk[j], "   ");                             j += 3;
    sprintf(&blk[j], "%s~",     sta->loc.desc);          j += strlen(sta->loc.desc) + 1;
    sprintf(&blk[j], "%3d",     lookup_code);            j += 3; 
    find_wordorder(temp);
    sprintf(&blk[j], "%s", temp);                        j += 6;
    sprintf(&blk[j], "%s", begtime);                     j += strlen(begtime);
    sprintf(&blk[j], "%s", endtime);                     j += strlen(endtime);
    sprintf(&blk[j], "N");                               j += 1;
    sprintf(&blk[j], "  ");
    strncpy(&blk[j], NETWORK, strlen(NETWORK));
    strncpy(&blk[j], sta->network, strlen(sta->network));

    add_blockette(blk, 'S');

if (0) printf("blk_050 --%s--\n", blk);
}


/*===================================================================*
 *  Build blockette 52
 *===================================================================*/
static void blk_052(sta, p)
struct station *sta;
struct channel *p;
{
int i, j;
char temp[24];
char begtime[24], endtime[24];
int dataformat_lookup;
int sensor_units_lookup;
int sensor_lookup;
int inp_units_lookup;
int out_units_lookup;
double max_clock_drift;
int stage_num;
int           maxSamplesPerRecord;


/*
 * Set data records data format
 */

    p->adc->dataformat = 0;
    if (SeedDataformat == 0)
        dataformat_lookup = STEIM1;
    else
        dataformat_lookup = SeedDataformat;

    if (dataformat_lookup != STEIM1 &&   /* Steim Integer Compression */
        dataformat_lookup != INT_4  &&   /* 32-bit Integers */
        dataformat_lookup != IEEEFLOAT)  /* 32-bit Floats */
    {
        fprintf(Fp_err, "ERROR: blk_052: unsupported data format ");
        fprintf(Fp_err, "%d for %s %s\n",
                dataformat_lookup, sta->name, p->name);
        exit(1);
    }

/*
 * Set maximum clock drift;
 * This variable is called clock_tolerance in rdseed.
 * Its value will be such as 'max_clock_drift' cumulated
 * over the number of samples in a record should be less than
 * 1/20 of the sample interval.
 * We assume that there are at most:
 *    4048 / 2 = 2000 samples in a compressed data record of 4096 bytes.
 *    4048 / 4 = 1000 samples in a Float or INT_4 data record.
 */

    if (dataformat_lookup == IEEEFLOAT || dataformat_lookup == INT_4)
        maxSamplesPerRecord = 1020;
    else if (dataformat_lookup == STEIM1)
        maxSamplesPerRecord = 2000;

    if (SeedClockTolerance == 0.0)
        max_clock_drift = (p->sint / 20.0) / (double) maxSamplesPerRecord;
    else
        max_clock_drift = SeedClockTolerance;

if (0) printf("==== blk_052: max_clock_drift %6.4E sint=%E max_samp=%d\n",
               max_clock_drift, p->sint, maxSamplesPerRecord);


    if      (p->sensor->units == VEL) sensor_units_lookup = VELOC;
    else if (p->sensor->units == ACC) sensor_units_lookup = ACCEL;
    else
    {
        fprintf(Fp_err, "ERROR: blk_052: can't find sensor units for:\n");
        fprintf(Fp_err, "%s %s\n", sta->name, p->name);
        exit(1);
    }

    if      (!strncmp(p->sensor->type, "STS1",  4))   sensor_lookup = 2;
    else if (!strncmp(p->sensor->type, "STS2",  4))   sensor_lookup = 3;
    else if (!strncmp(p->sensor->type, "CMG3",  4))   sensor_lookup = 4;
    else if (!strncmp(p->sensor->type, "CMG5",  4))   sensor_lookup = 5;
    else if (!strncmp(p->sensor->type, "CMG40", 5))   sensor_lookup = 6;
    else if (!strncmp(p->sensor->type, "GS13",  4))   sensor_lookup = 7;
    else if (!strncmp(p->sensor->type, "LE3D",  4))   sensor_lookup = 8;
    else if (!strncmp(p->sensor->type, "L4C",   3))   sensor_lookup = 9;
    else if (!strncmp(p->sensor->type, "L22",   3))   sensor_lookup = 10;
    else if (!strncmp(p->sensor->type, "STS1POS", 7)) sensor_lookup = 11;
    else if (!strncmp(p->sensor->type, "STS2POS", 7)) sensor_lookup = 12;
    else if (!strncmp(p->sensor->type, "SDJ_S2A", 7)) sensor_lookup = 13;
    else if (!strncmp(p->sensor->type, "ES_T",  4))   sensor_lookup = 14;
    else if (!strncmp(p->sensor->type, "EST",   3))   sensor_lookup = 15;
    else
    {
        fprintf(Fp_err, "ERROR: blk_052: unsupported sensor type %s\n",
                p->sensor->type);
        exit(1);
    }

    dbt_to_asc(sta->beg_utime, temp);
    sprintf(begtime, "%.17s~", temp);
    if (sta->end_utime == 0.0) sprintf(endtime, "~");
    else
    {
        dbt_to_asc(sta->end_utime, temp);
        sprintf(endtime, "%.17s~", temp);
    }

    j = 0;
    sprintf(&blk[j], "0520000");                        j += 7;
    sprintf(&blk[j], "  ");                             j += 2;
    sprintf(&blk[j], "%.3s",    p->name);               j += 3;
    sprintf(&blk[j], "0000");                           j += 4;
    sprintf(&blk[j], "%3d",     sensor_lookup);         j += 3;
    sprintf(&blk[j], "~");                              j += 1;
    sprintf(&blk[j], "%3d",     sensor_units_lookup);   j += 3;
    sprintf(&blk[j], "%3d",     0);                     j += 3;
    sprintf(&blk[j], "%+10.6f", sta->loc.lat);          j += 10;
    sprintf(&blk[j], "%+11.6f", sta->loc.lon);          j += 11;
    sprintf(&blk[j], "%+7.1f",  (float)sta->loc.elev);  j += 7;
    sprintf(&blk[j], "%5.1f",   (float)sta->loc.depth); j += 5;
    sprintf(&blk[j], "%5.1f",   p->sensor->azimuth);    j += 5;
    sprintf(&blk[j], "%+5.1f",  p->sensor->dip);        j += 5;
    sprintf(&blk[j], "%04d",    dataformat_lookup);     j += 4;
    sprintf(&blk[j], "12");                             j += 2;
    sprintf(&blk[j], "%10.4E",  1.0 / p->sint);         j += 10;
    sprintf(&blk[j], "%10.4E",  max_clock_drift);       j += 10;
    sprintf(&blk[j], "%4d",     0);                     j += 4;
    sprintf(&blk[j], "CG~");                            j += 3;
    sprintf(&blk[j], "%s",      begtime);        j += strlen(begtime);
    sprintf(&blk[j], "%s",      endtime);        j += strlen(endtime);
    sprintf(&blk[j], "N");                              j += 1;

    add_blockette(blk, 'S');

if (0) printf("\n%s\n", blk);


/*================  Sensor ===================*/
    stage_num = 1;
    if (p->sensor->sensor && p->sensor->sensor->type == ANALOG)
    {
if (0) printf("\n%s\n", p->sensor->sname);
if (0) print_filter(stdout, p->sensor->sensor, stage_num);

        inp_units_lookup = sensor_units_lookup;
        out_units_lookup = VOLT;

        blk_053(p->sensor->sensor, stage_num, inp_units_lookup, out_units_lookup);
        add_blockette(blk, 'S');

        blk_058(p->sensor->sensor_G, p->sensor->sensor->fn, stage_num);
        add_blockette(blk, 'S');
    }

/*================  Analog filter  ===================*/
    if (p->sensor->filter && p->sensor->filter->type == ANALOG)
    {
        ++stage_num;
if (0) printf("\n%s\n", p->sensor->fname);
if (0) print_filter(stdout, p->sensor->filter, stage_num);

        inp_units_lookup = VOLT;
        out_units_lookup = VOLT;

        blk_053(p->sensor->filter, stage_num, inp_units_lookup, out_units_lookup);
        add_blockette(blk, 'S');

        blk_058(p->sensor->filter_G, p->sensor->filter->fn, stage_num);
        add_blockette(blk, 'S');
    }

/*=================  Amplifier ====================*/
    if (p->ampli_nom_gain != 1.0 || p->gain_corr != 1.0)
    {
/* blk_058 */
        ++stage_num;
/*
printf("===== gains adc %.3f ampli %.3f\n", p->adc->fact, p->ampli_nom_gain);
*/
        blk_058((p->ampli_nom_gain * p->gain_corr), 0.0, stage_num);
        add_blockette(blk, 'S');
    }
 
/*=================  Digitizer ====================*/
    ++stage_num;
    inp_units_lookup = VOLT;
    out_units_lookup = COUNT;

/* blk_054 */
    j = 0;
    sprintf(&blk[j], "0540000");                      j += 7;
    sprintf(&blk[j], "D");                            j += 1;
    sprintf(&blk[j], "%02d", stage_num);              j += 2;
    sprintf(&blk[j], "%03d", inp_units_lookup);       j += 3;
    sprintf(&blk[j], "%03d", out_units_lookup);       j += 3;
    sprintf(&blk[j], "%04d", 0);                      j += 4;
    sprintf(&blk[j], "%04d", 0);                      j += 4;
    add_blockette(blk, 'S');

/* blk_057 */
    j = 0;
    sprintf(&blk[j], "0570000");                      j += 7;
    sprintf(&blk[j], "%02d", stage_num);              j += 2;
    if ((double) p->adc->sint != (double) 0.0)
        sprintf(&blk[j], "%10.4E", (1.0 / p->adc->sint));
    else
        sprintf(&blk[j], "%10.4E", (1.0 / p->sint));
                                                      j += 10;
    sprintf(&blk[j], "%05d",   1);                    j += 5;
    sprintf(&blk[j], "%05d",   0);                    j += 5;
    sprintf(&blk[j], "%11.4E", 0.0);                  j += 11;
    sprintf(&blk[j], "%11.4E", 0.0);                  j += 11;
    add_blockette(blk, 'S');

/* blk_058 */
    blk_058(p->adc->fact, 0.0, stage_num);
    add_blockette(blk, 'S');

if (0) printf("%s\n", blk);


/*=================  Digital filters  ====================*/

    for (i=0; i<p->ndig_filt; i++)
    {
        ++stage_num;
        inp_units_lookup = COUNT;
        out_units_lookup = COUNT;

if (0) printf("\n%s\n", p->dig_filt[i].filename);
if (0) print_filter(stdout, p->dig_filt[i].filter, stage_num);

        blk_054(p->dig_filt[i].filter, stage_num, inp_units_lookup, out_units_lookup);
        add_blockette(blk, 'S');

        blk_057(&(p->dig_filt[i]), stage_num);
        add_blockette(blk, 'S');

        blk_058(1.0, 0.0, stage_num);
        add_blockette(blk, 'S');
    }

/*==============  Last stage: 0, overall sensitivity ====*/
    stage_num = 0;
    blk_058(p->gain, p->sensor->sensor->fn, stage_num);
    add_blockette(blk, 'S');

/*============= Look for channel comments ====*/

    get_sta_cha_comments(sta, p);

}


/*===================================================================*
 *  Build blockette 51 - Station Comment Blockette
 *===================================================================*/
static void get_sta_comments(sta)
struct station *sta;
{
struct inp_cmt  *cmt;
struct data_segment *segm;
double   cmt_begtime, cmt_endtime;
int      found_comment;
char     s1[24], s2[24];
char     s3[24], s4[24];


    for (cmt=inp_sta_cmt_head; cmt!=NULL; cmt=cmt->next)
    {
        found_comment = FALSE;
        for (segm=dsegm_head; segm!=NULL; segm=segm->next)
        {
            if (!strcmp(segm->sta, cmt->station) &&
                !strcmp(sta->name, cmt->station))
            {
                cmt_begtime = (double) cmt->beg;
                cmt_endtime = (double) cmt->end;

          /* Look if data segment times falls within comment times */

                if ((segm->begtime >= cmt_begtime &&
                     segm->begtime <= cmt_endtime)  ||
                    (segm->endtime >= cmt_begtime &&
                     segm->endtime <= cmt_endtime))
                {
                     dbt_to_asc(cmt_begtime,   s1);
                     dbt_to_asc(cmt_endtime,   s2);
                     dbt_to_asc(segm->begtime, s3);
                     dbt_to_asc(segm->endtime, s4);
              if (0) printf("%s FOUND CMT %s-%s # %d\n", segm->sta, s1, s2, cmt->code);
              if (0) printf("    for  SEGM %s-%s\n", s3, s4);
                     found_comment = TRUE;
                     break;
                }
            }
        }
        if (found_comment == TRUE)
        {
            blk_051(s1, s2, cmt->code);
        }
    }
}


/*===================================================================*
 *  Get station or channel comments, blockette 51 or 59
 *===================================================================*/
static void get_sta_cha_comments(sta, cha)
struct station *sta;
struct channel *cha;
{
char     s1[24], s2[24];
char     s3[24], s4[24];
struct data_segment *segm;
struct inp_cmt  *cmt;


/* Loop through comments and data segments.      */
/* Comment will be candidate if we find the same name in          */
/* the station struct, the comment and the data segment.          */
/* Then, if the comment applies to all channels, it is considered */
/* as a station comment (blk 051); else if a channel is specified */
/* and if the channel name matches the comment channel name, the  */
/* comment is considered as a channel comment (blk 059)           */
/* Finally,  if we find a data segment times overlaping the       */
/* comment times, we create a comment blockette.                  */

  for (cmt=sta_cha_cmt_head; cmt!=NULL; cmt=cmt->next)
  {
      if (strcmp(sta->name, cmt->station)) continue;

      for (segm=dsegm_head; segm!=NULL; segm=segm->next)
      {

          if (strcmp(segm->sta, cmt->station)) continue;
          if (strcmp(segm->sta, sta->name))    continue;

          if (cha == NULL && !strcmp(cmt->channel, "all"))
          {
              if ((segm->begtime < (double) (cmt->end)) &&
                  (segm->endtime > (double) (cmt->beg)))
              {
                  dbt_to_asc((double) (cmt->beg), s1);
                  dbt_to_asc((double) (cmt->end), s2);
                  dbt_to_asc(segm->begtime, s3);
                  dbt_to_asc(segm->endtime, s4);

                  if (1) printf("Station comment num %4d %s %s-%s\n",
                                 cmt->code, cmt->station,s1,s2);
                  if (1) printf("         for SEGM %s-%s\n", s3, s4);

                  if (!strncmp(s2, "2020,001", 8)) s2[0] = '\0';
                  blk_051(s1, s2, cmt->code);
              /* go to next comment */
                  break;
              }
          }
          else if (cha != NULL && !strcmp(cmt->channel, cha->name))
          {
              if ((segm->begtime < (double) (cmt->end)) &&
                  (segm->endtime > (double) (cmt->beg)))
              {
                  dbt_to_asc((double) (cmt->beg), s1);
                  dbt_to_asc((double) (cmt->end), s2);
                  dbt_to_asc(segm->begtime, s3);
                  dbt_to_asc(segm->endtime, s4);

                  if (1) printf("Channel comment num %4d %s %s %s-%s\n",
                          cmt->code, cmt->station,cmt->channel,s1,s2);
                  if (1) printf("         for SEGM %s-%s\n", s3, s4);

                  if (!strncmp(s2, "2020,001", 8)) s2[0] = '\0';
                  blk_059(s1, s2, cmt->code);
              /* go to next comment */
                  break;
              }
          }
      }
  }

}



/*===================================================================*
 *  Build blockette 30: data format dictionary
 *
 * key 1: M0      no multiplexing
 * key 2: W3      copy 3 bytes in working buffer without reordering
 *        D0-23   extract bits 0 to 23
 *        C2      use 2's complement
 *
0300104Geoscope gain-range on 3 bits       
            ~000100104M3I0L2~W2 D0-11 A-2048~D12-14~E2:0:-1~
            ~000100104M0~W2 D0-11 A-2048~D12-14~E2:0:-1~
0300085Geoscope-3 byte (switched gain)     
            ~000200002M3I0L3~W3 D0-23 C2~
            ~000200002M0~W3 D0-23 C2~
0300104Geoscope gain range on 4 bits       
            ~000300104M3I0L2~W2 D0-11 A-2048~D12-15~E2:0:-1~
            ~000300104M0~W2 D0-11 A-2048~D12-15~E2:0:-1~

"030 232Steim Integer Compression Format~"
"0004050 6"
"F1 P4 W4 D0-31 C2 R1 P8 W4 D0-31 C2~"
"P0 W4 N15 S2,0,1~"
"T0 X N0 W4 D0-31 C2~"
"T1 N0 W1 D0-7 C2 N1 W1 D0-7 C2 N2 W1 D0-7 C2 N3 W1 D0-7 C2~"
"T2 N0 W2 D0-15 C2 N1 W2 D0-15 C2~"
"T3 N0 W4 D0-31 C2~"

    key 1:
       M0      no multiplexing
    key 2:
       W2      copy 2 bytes in working buffer without reordering
       D0-15   extract bits 0 to 15
       D0-23   extract bits 0 to 23
       C2      use 2's complement
}

 *===================================================================*/
void blk_030()
{
int j;
int dataformat_lookup;        /* data format lookup code */
int data_family_type;   /* family type: 0=integers, 1=gain-ranged, 50=integer differences compression */
int num_of_keys;        /* number of keys */
char desc[50];
char keys[50];

    sprintf(desc, "Steim Integer Compression Format~");
    dataformat_lookup = STEIM1;
    data_family_type = 50;
    num_of_keys      = 6;
    sprintf(keys, "M0~W2 D0-11 A-2048~D12-15~E2:0:-1~");
    j = 0;
    sprintf(&blk[j], "0300000");                 j += 7;
    sprintf(&blk[j], "%s",   desc);              j += strlen(desc);
    sprintf(&blk[j], "%04d", dataformat_lookup); j += 4;
    sprintf(&blk[j], "%03d", data_family_type);  j += 3;
    sprintf(&blk[j], "%02d", num_of_keys);       j += 2;
    sprintf(&blk[strlen(blk)], "F1 P4 W4 D0-31 C2 R1 P8 W4 D0-31 C2~");
    sprintf(&blk[strlen(blk)], "P0 W4 N15 S2,0,1~");
    sprintf(&blk[strlen(blk)], "T0 X N0 W4 D0-31 C2~");
    sprintf(&blk[strlen(blk)], "T1 N0 W1 D0-7 C2 N1 W1 D0-7 C2 N2 W1 ");
    sprintf(&blk[strlen(blk)], "D0-7 C2 N3 W1 D0-7 C2~");
    sprintf(&blk[strlen(blk)], "T2 N0 W2 D0-15 C2 N1 W2 D0-15 C2~");
    sprintf(&blk[strlen(blk)], "T3 N0 W4 D0-31 C2~");
    add_blockette(blk, 'A');


    sprintf(desc, "SUN IEEE Floats~");
    dataformat_lookup = IEEEFLOAT;
    data_family_type = 0;
    num_of_keys      = 2;
    sprintf(keys, "M0~W4 D0-31 C2~");
    j = 0;
    sprintf(&blk[j], "0300000");                 j += 7;
    sprintf(&blk[j], "%s",   desc);              j += strlen(desc);
    sprintf(&blk[j], "%04d", dataformat_lookup); j += 4;
    sprintf(&blk[j], "%03d", data_family_type);  j += 3;
    sprintf(&blk[j], "%02d", num_of_keys);       j += 2;
    sprintf(&blk[j], "%s",   keys);              j += strlen(keys);
    add_blockette(blk, 'A');


    sprintf(desc, "32-bit Integers~");
    dataformat_lookup = INT_4;
    data_family_type = 0;
    num_of_keys      = 2;
    sprintf(keys, "M0~W4 D0-31 C2~");
    j = 0;
    sprintf(&blk[j], "0300000");                 j += 7;
    sprintf(&blk[j], "%s",   desc);              j += strlen(desc);
    sprintf(&blk[j], "%04d", dataformat_lookup); j += 4;
    sprintf(&blk[j], "%03d", data_family_type);  j += 3;
    sprintf(&blk[j], "%02d", num_of_keys);       j += 2;
    sprintf(&blk[j], "%s",   keys);              j += strlen(keys);
    add_blockette(blk, 'A');


if (0) printf("--%s--\n", blk);
}

/*===================================================================*
 *  Build blockette 33
 *===================================================================*/
void blk_033()
{
int j;
int lookup_code;

    lookup_code = 1;
    j = 0;
    sprintf(&blk[j], "0330000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "GEOSCOPE~");
    add_blockette(blk, 'A');

    lookup_code = 2;
    j = 0;
    sprintf(&blk[j], "0330000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "STRECKEISEN STS1~");
    add_blockette(blk, 'A');

    lookup_code = 3;
    j = 0;
    sprintf(&blk[j], "0330000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "STRECKEISEN STS2~");
    add_blockette(blk, 'A');

    lookup_code = 4;
    j = 0;
    sprintf(&blk[j], "0330000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "GURALP CMG3~");
    add_blockette(blk, 'A');

    lookup_code = 5;
    j = 0;
    sprintf(&blk[j], "0330000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "GURALP CMG5~");
    add_blockette(blk, 'A');

    lookup_code = 6;
    j = 0;
    sprintf(&blk[j], "0330000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "GURALP CMG40~");
    add_blockette(blk, 'A');

    lookup_code = 7;
    j = 0;
    sprintf(&blk[j], "0330000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "GEOTECH GS13~");
    add_blockette(blk, 'A');

    lookup_code = 8;
    j = 0;
    sprintf(&blk[j], "0330000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "LENNARTZ LE3D~");
    add_blockette(blk, 'A');

    lookup_code = 9;
    j = 0;
    sprintf(&blk[j], "0330000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "MARK PRODUCT L4C~");
    add_blockette(blk, 'A');

    lookup_code = 10;
    j = 0;
    sprintf(&blk[j], "0330000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "MARK PRODUCT L22~");
    add_blockette(blk, 'A');

    lookup_code = 11;
    j = 0;
    sprintf(&blk[j], "0330000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "STS1 MASS POS~");
    add_blockette(blk, 'A');

    lookup_code = 12;
    j = 0;
    sprintf(&blk[j], "0330000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "STS2 MASS POS~");
    add_blockette(blk, 'A');

    lookup_code = 13;
    j = 0;
    sprintf(&blk[j], "0330000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "CHINESE SDJ_S2A~");
    add_blockette(blk, 'A');

    lookup_code = 14;
    j = 0;
    sprintf(&blk[j], "0330000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "KINEMETRICS ES_T~");
    add_blockette(blk, 'A');

    lookup_code = 15;
    j = 0;
    sprintf(&blk[j], "0330000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "KINEMETRICS EST~");
    add_blockette(blk, 'A');
}

/*===================================================================*
 *  Build blockette 34
 *===================================================================*/
void blk_034()
{
int j;
int lookup_code;

    lookup_code = VELOC;
    j = 0;
    sprintf(&blk[j], "0340000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "M/S~Velocity~");
    add_blockette(blk, 'A');

    lookup_code = ACCEL;
    j = 0;
    sprintf(&blk[j], "0340000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "M/S**2~Acceleration~");
    add_blockette(blk, 'A');

    lookup_code = VOLT;
    j = 0;
    sprintf(&blk[j], "0340000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "V~Volts~");
    add_blockette(blk, 'A');

    lookup_code = COUNT;
    j = 0;
    sprintf(&blk[j], "0340000%03d", lookup_code);  j += 10;
    sprintf(&blk[j], "COUNTS~Digital Counts~");
    add_blockette(blk, 'A');
}


/*===================================================================*
 *  Build blockette 51
 *===================================================================*/
static void blk_051(t1, t2, code)
char *t1;
char *t2;
int code;
{
    sprintf(&blk[0], "0510000");
    sprintf(&blk[strlen(blk)], "%s~", t1);
    sprintf(&blk[strlen(blk)], "%s~", t2);
    sprintf(&blk[strlen(blk)], "%04d", code);
    sprintf(&blk[strlen(blk)], "000000");
    add_blockette(blk, 'S');
}


/*===================================================================*
 *  Build blockette 53
 *===================================================================*/
static void blk_053(p, stage_num, inp_units_lookup, out_units_lookup)
struct filter *p;
int stage_num;
int inp_units_lookup;
int out_units_lookup;
{
int i, j;

    j = 0;
    sprintf(&blk[j], "0530000");                   j += 7;
    sprintf(&blk[j], "B");                         j += 1;
    sprintf(&blk[j], "%2d",    stage_num);         j += 2;
    sprintf(&blk[j], "%3d",    inp_units_lookup);  j += 3;
    sprintf(&blk[j], "%3d",    out_units_lookup);  j += 3;
    sprintf(&blk[j], "%12.5E", p->A0);             j += 12;
    sprintf(&blk[j], "%12.5E", p->fn);             j += 12;
    sprintf(&blk[j], "%3d",    p->nzero);            j += 3;
    for (i=0; i<p->nzero*2; i+=2)
    {
        sprintf(&blk[j], "%12.5E%12.5E",
                       p->zero[i], p->zero[i+1]);    j += 12*2;
        sprintf(&blk[j], "%12.5E%12.5E", 0.0, 0.0); j += 12*2;
    }
    sprintf(&blk[j],"%3d", p->npole);                j += 3;
    for (i=0; i<p->npole*2; i+=2)
    {
        sprintf(&blk[j], "%12.5E%12.5E",
                       p->pole[i], p->pole[i+1]);    j += 12*2;
        sprintf(&blk[j], "%12.5E%12.5E", 0.0, 0.0); j += 12*2;
    }

if (0) printf("%s\n", blk);
}


/*===================================================================*
 *  Build blockette 54
 *===================================================================*/
static void blk_054(p, stage_num, inp_units_lookup, out_units_lookup)
struct filter *p;
int stage_num;
int inp_units_lookup;
int out_units_lookup;
{
int i, j;

if (0) fprintf(stderr,"blk_054: filter type: %d\n", p->type);

    j = 0;
    sprintf(&blk[j], "0540000");                           j += 7;
    sprintf(&blk[j], "D");                                 j += 1;
    sprintf(&blk[j], "%02d",     stage_num);               j += 2;
    sprintf(&blk[j], "%03d",     inp_units_lookup);        j += 3;
    sprintf(&blk[j], "%03d",     out_units_lookup);        j += 3;
if (p->type == IIR_POLY)
{
    sprintf(&blk[j], "%04d",     p->nzero);                j += 4;
    for (i=0; i<p->nzero; i++)
    {
        sprintf(&blk[j], "%12.5E%12.5E", p->zero[i], 0.0); j += 12*2;
    }
    sprintf(&blk[j], "%04d",     p->npole);                j += 4;
    for (i=0; i<p->npole; i++)
    {
        sprintf(&blk[j], "%12.5E%12.5E", p->pole[i], 0.0); j += 12*2;
    }
}
else if (p->type == IIR_PZ)
{
    fprintf(stderr,"blk_054: unsupported filter: IIR_PZ\n");
    exit(1);
}
else
{
    sprintf(&blk[j], "%04d",     p->ncoef);                j += 4;
    for (i=0; i<p->ncoef; i++)
    {
        sprintf(&blk[j], "%12.5E%12.5E", p->coef[i], 0.0); j += 12*2;
    }
    sprintf(&blk[j], "%04d",     0);                       j += 4;
}

if (0) printf("%s\n", blk);
}


/*===================================================================*
 *  Build blockette 57
 *===================================================================*/
static void blk_057(p, stage_num)
struct dig_filt *p;
int stage_num;
{
int j;

    j = 0;
    sprintf(&blk[j], "0570000");                         j += 7;
    sprintf(&blk[j], "%02d", stage_num);                 j += 2;
/* MUST BE 10.4E, not 10.3E */
    sprintf(&blk[j], "%10.4E", (1.0 / p->filter->sint)); j += 10;
    sprintf(&blk[j], "%05d",   p->decim);                j += 5;
    sprintf(&blk[j], "%05d",   0);                       j += 5;
    sprintf(&blk[j], "%11.4E", 0.0);                     j += 11;
    sprintf(&blk[j], "%11.4E", 0.0);                     j += 11;

if (0) printf("--%s--\n", blk);
}


/*===================================================================*
 *  Build blockette 58
 *===================================================================*/
static void blk_058(gain, freq_gain, stage_num)
double gain;
double freq_gain;
int    stage_num;
{
int j;

    j = 0;
    sprintf(&blk[j], "0580000");                j += 7;
    sprintf(&blk[j], "%02d",   stage_num);      j += 2;
    sprintf(&blk[j], "%12.5E", gain);           j += 12;
    sprintf(&blk[j], "%12.5E", freq_gain);      j += 12;
    sprintf(&blk[j], "%02d", 0);                j += 2;

if (0) printf("--%s--\n", blk);
}


/*===================================================================*
 *  Build blockette 59
 *===================================================================*/
static void blk_059(t1, t2, code)
char *t1;
char *t2;
int code;
{
    sprintf(&blk[0], "0590000");
    sprintf(&blk[strlen(blk)], "%s~", t1);
    sprintf(&blk[strlen(blk)], "%s~", t2);
    sprintf(&blk[strlen(blk)], "%04d", code);
    sprintf(&blk[strlen(blk)], "000000");
    add_blockette(blk, 'S');
}


/*===================================================================*
 *  Build blockette 31 - Station Comment Description blockette
 *===================================================================*/
void blk_031_S()
{
#define STA_CLASS_CODE   'S'
#define CHA_CLASS_CODE   'C'
struct sta_cmt *cmt;

    for (cmt=sta_cmt_head; cmt!=NULL; cmt=cmt->next)
    {
        sprintf(&blk[0],           "0310000");
        sprintf(&blk[strlen(blk)], "%04d", cmt->code);
        sprintf(&blk[strlen(blk)], "%c",   STA_CLASS_CODE);
        sprintf(&blk[strlen(blk)], "%s~",  cmt->comment);
        sprintf(&blk[strlen(blk)], "000");
        add_blockette(blk, 'A');
    }
}


/*===================================================================*
 *  Build blockette 31
 *===================================================================*/
void blk_031_C()
{
char str[256];
FILE *Fp_cmt;

if (!cmts_dir) return;

    sprintf(str, "%s/%s", cmts_dir, "sta_cha_comments.list");
    if (!(Fp_cmt = fopen(str, "r")))
    {
        fprintf(Fp_err,"ERROR: blk_031_C: can't open %s\n", str);
        exit(1);
    } 

    while (fgets(str, 255, Fp_cmt))
    {
        trim_cr(str);
        if (str[0] == '#') continue;
        if (strlen(str) == 0) continue;
        sprintf(&blk[0],           "0310000");
        sprintf(&blk[strlen(blk)], "%s~", str);
        sprintf(&blk[strlen(blk)], "000");
        add_blockette(blk, 'A');
    }
    fclose(Fp_cmt);
}


/*===================================================================*
       add_blockette
       append blockette to logical record
 *===================================================================*/
void add_blockette(blk, rec_type)
char *blk;
char rec_type;
{
struct lrec *pt = NULL;
struct lrec **tail = NULL;
struct lrec **head = NULL;
int         *nrec = NULL;
int          blen;
char         temp[10];
static int   debug = 0;

/*=========== Get pointers according to header type ============*/

    if      (rec_type == 'V')
    {
        nrec = &nVH;
        pt   = VH_list_tail;
        tail = &VH_list_tail;
        head = &VH_list_head;
    }
    else if (rec_type == 'A')
    {
        nrec = &nAH;
        pt   = AH_list_tail;
        tail = &AH_list_tail;
        head = &AH_list_head;
    }
    else if (rec_type == 'S')
    {
        nrec = &nSH;
        pt   = SH_list_tail;
        tail = &SH_list_tail;
        head = &SH_list_head;
    }
    else if (rec_type == 'T')
    {
        nrec = &nTH;
        pt   = TH_list_tail;
        tail = &TH_list_tail;
        head = &TH_list_head;
    }

/*============  First call : initializations =================*/

    if (!strcmp(blk, "init"))
    {
        if (pt == NULL)
        {
            if ((*nrec) != 0)
            {
                fprintf(Fp_err,"ERROR: append %c: initial ", rec_type);
                fprintf(Fp_err,"number of records should be null\n");
                exit(1);
            }
            pt = (struct lrec *) mem(sizeof(struct lrec));
            append_linklist_element(pt, (*head), (*tail));
        }
        memset(logrec, 0, LRECL);
        sprintf(logrec, "%06d%c ", *nrec, rec_type);
        nc = 8;
        return;
    }

    if (blk == NULL || pt == NULL || nrec == NULL)
    {
        fprintf(Fp_err, "ERROR: append %c: null pointer\n", rec_type);
        fprintf(Fp_err, "blk=%p pt=%p  nrec=%p\n",
          blk, pt, nrec);
        exit(1);
    }

/*==== Last call :  padd spaces and copy last logrec into array ====*/

    if (!strcmp(blk, "end"))
    {
        while (nc < LRECL) logrec[nc++] = ' ';

        memcpy(pt->rec, logrec, LRECL);
if (debug)  printf("    c- record %.30s recnum %d  %p\n", pt->rec, *nrec, pt);
        ++(*nrec);

        pt = (struct lrec *) mem(sizeof(struct lrec));
        append_linklist_element(pt, (*head), (*tail));
        return;
    }

/*==== Append blockette to logrec and eventually copy into array ===*/

/* Remove trailing carriage return, if any, but not trailing blanks */
    trim_cr(blk);

/* Find blockette lenght */
    blen = strlen(blk);

/* write blockette lenght */
    sprintf(temp, "%04d", blen);
    strncpy(&blk[3], temp, 4);


/* If not enough room to write blk type and lenght, flush log record */
    if ((LRECL-nc) < 7)
    {
if (0)  printf("==== not enough room for blk header; flush logical record\n");
        while (nc < LRECL) logrec[nc++] = ' ';

        memcpy(pt->rec, logrec, 4096);
if (debug)  printf("    b- record %.30s recnum %d  %p\n", pt->rec, *nrec, pt);
        ++(*nrec);

        pt = (struct lrec *) mem(sizeof(struct lrec));
        append_linklist_element(pt, (*head), (*tail));

    /* start new logical record */
        memset(logrec, 0, LRECL);
        sprintf(logrec, "%06d%c ", *nrec, rec_type);
        nc = 8;
    }



/* Check if we can write the entire blockette */
    if ((LRECL - nc) >= blen)
    {
if (0)  printf("==== %3.3s blen=%4d copy %4d bytes at nc=%4d\n",
                      blk, blen, blen, nc);
        strncpy(&logrec[nc], blk, blen);
        nc += blen;
    }

/* else blockette will cross log record boundary */
    else
    {
      char *ptr     = blk;
      int   n_cpy   = LRECL-nc;
      int   b_left  = blen;

        while (b_left > 0)
        {
if (0)     printf("==== %3.3s blen=%4d copy %4d bytes at nc=%4d\n",
                          blk, blen, n_cpy, nc);
            strncpy(&logrec[nc], ptr, n_cpy);
            b_left -= n_cpy;
            ptr   += n_cpy;
            nc    += n_cpy;

            if (nc >= LRECL)
            {
              memcpy(pt->rec, logrec, 4096);
if (debug)    printf("    a- record %.30s recnum %d  %p\n", pt->rec, *nrec, pt);
              ++(*nrec);

              pt = (struct lrec *) mem(sizeof(struct lrec));
              append_linklist_element(pt, (*head), (*tail));

            /* start new logical record */
              memset(logrec, 0, LRECL);
              sprintf(logrec, "%06d%c*", *nrec, rec_type);
              nc = 8;
            }
            if (b_left > (LRECL-8)) n_cpy = LRECL-8;
            else                    n_cpy = b_left;
        }
    }
}


