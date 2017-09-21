/*
   This code uses number_time_corrections in FSDH and includes
   Blockettes 1000 & 1001 in data records.
   Blockette 1000 gives
       - encoding_fmt (steim = 10)
       - rec_length
       - word_order
   Blockette 1001 is there but doesn't contain info.
*/
/*====================================================================
    Name:       encode_steim.c

    Purpose:  translate signed long integers to steim compressed format.

    Usage:    Steim_comp( FILE *Fp;
                          float *p_dbuf;
                          char  *header,
                          int nsamples;
                          double nom_sint,
                          int  *p_seed_data_record;

    Calls:      memcpy, printf

    Algorithm:  The compressor is implemented as a Deterministic
                Finite Automaton.  A second DFA takes over when
                the input tape (the raw data) ends.  The transition
                table for the DFA is -

           note: _f signifies a final state.
           ----------------------------------------------------------
                          | # of    |                | # of  |
                          | bytes   |                | DIFS  |
                          | DIF     |                | to    | DIF
           Current state  | fits in | New State      | unget | index
           ---------------+---------+----------------+-------+-------
           _START_STATE   |  1      | _D1            | 0     | 0
           _START_STATE   |  2      | _D2            | 0     | 0
           _START_STATE   |  4      | _D4_f          | 0     | 0
                    _D1   |  1      | _D1_D1         | 0     | 1
                    _D1   |  2      | _D2_D2_f       | 0     | 1
                    _D1   |  4      | _D4_f          | 1     | -1
                    _D2   |  1      | _D2_D2_f       | 0     | 1
                    _D2   |  2      | _D2_D2_f       | 0     | 1
                    _D2   |  4      | _D4_f          | 1     | -1
                 _D1_D1   |  1      | _D1_D1_D1      | 0     | 2
                 _D1_D1   |  2      | _D2_D2_f       | 1     | -1
                 _D1_D1   |  4      | _D2_D2_f       | 1     | -1
              _D1_D1_D1   |  1      | _D1_D1_D1_D1_f | 0     | 3
              _D1_D1_D1   |  2      | _D2_D2_f       | 2     | -1
              _D1_D1_D1   |  4      | _D2_D2_f       | 2     | -1
           ----------------------------------------------------------


    Problems:   None known.

    Language:   C, ANSI standard.

    Author:     Guy Stewart, Round Rock TX  (512) 244-9081
                IRIS         Austin TX      (512) 471-0405

    Revision:
       04/26/1991  G. Stewart   Initial preliminary release 0.9
       05/01/1991  G. Stewart   First release of version 1.0
       10/15/1992  A. Nance     Added parms for data record blockettes
                           primarily blk 100
                           Parms added:
                           num_cont_blk, num_cont_blk_bytes, cont_blk,
                           num_once_blk, num_once_blk_bytes, once_blk

       04/12/95    CL - added some bug fixes from Neuberger
       17/07/1996  JFF - Rewritten for mkseed. Time calculations
                         done with actual sample rate.
       07/06/2000  JFF - Time calculations done with beging time and
                         total number of samples from begining to avoid
                         roundings errors.
                       - Beg of data set to 64 (recommendation in Seed
                         manual). There is still room for an optional
                         B100 sample rate blockette.

       12/21/2000  JFF - Many changes.
                         Support for input extras blockettes removed.
                         Blockettes 1000 & 1001 added.
                         Time expressed as uncorrected (system) time and
                         a time correction (hdr->number_time_corrections).
                         Computation of time correction is done here
                         with the module GetEstimDt_(dbltime, ...)
                         where dbltime is the uncorrected time.
                         Calls to  to this module must preceeded by a call
                         to loadDftFile() and followed by a call to
                         freeTimeDrift().

 ====================================================================*/
#include <stdio.h>
#include "seed.h"
#include "proto.h"
#include "proto_seed.h"

#define _START_STATE    0
#define _D1             1
#define _D2             2
#define _D1_D1          3
#define _D1_D1_D1       4
#define _D4_f           5
#define _D2_D2_f        6
#define _D1_D1_D1_D1_f  7

/*===================================================================
    _DATA_STATE - structure used to store the state of the current
                  fixed section data header.
 ===================================================================*/
typedef struct _DATA_STATE
{
    int get_new_x0;      /* set TRUE if new x0 is needed            */
    int x0;              /* forward integration constant (x sub 0)  */
    int xN;              /* reverse integration constant (x sub n)  */
    int w0;              /* storage for all cks for a frame         */
    int num_data_rec;
    int seed_frame;
    int seed_index;
    int record_offset;
    int frames_per_record;
    double nom_sint;
} DATA_STATE;

int final[] =
{
    0,
    0,
    0,
    0,
    0,
    1,
    1,
    1
};

typedef struct _TRANSITION
{
    int new_state;
    int unget;
    int diff_index;
} TRANSITION;


TRANSITION transition[] =
{    
     { _D1            , 0,   0 },
     { _D2            , 0,   0 },
     { _D4_f          , 0,   0 },
     { _D1_D1         , 0,   1 },
     { _D2_D2_f       , 0,   1 },
     { _D4_f          , 1,  -1 },
     { _D2_D2_f       , 0,   1 },
     { _D2_D2_f       , 0,   1 },
     { _D4_f          , 1,  -1 },
     { _D1_D1_D1      , 0,   2 },
     { _D2_D2_f       , 1,  -1 },
     { _D2_D2_f       , 1,  -1 },
     { _D1_D1_D1_D1_f , 0,   3 },
     { _D2_D2_f       , 2,  -1 },
     { _D2_D2_f       , 2,  -1 }
};

#ifdef ANSI_C
void blk_1000_1001        (char*, int);
static int Add_word       (FILE*, int*, char*, int, int, DATA_STATE*);
static void finish_record (FILE*, int*, char*, DATA_STATE*);
#else
void blk_1000_1001        ();
static int Add_word       ();
static void finish_record ();
#endif


/*=========================================================================
               Reminder, from seed.h

struct seed_data_hdr               * fixed data header *
{
    char               station[5];             * station name *
    char               location[2];            * station location *
    char               channel[3];             * channel name *
    char               network[2];             * network name *
    struct seed_time   time;                   * time *
    unsigned short int nsamples;               * number samples *
    short int          sample_rate;            * sample rate factor *
    short int          sample_rate_multiplier; * sample rate multiplier *
    char               activity_flags;         * activity flags *
    char               io_flags;               * io flags *
    char               data_quality_flags;     * data quality flags *
    char               number_blockettes;      * # blockettes which follow *
    long int           number_time_corrections;* # .0001s time corrections *
    unsigned short int beg_of_data;            * beginning of data *
    unsigned short int beg_first_blk;          * beginning 1st blkt *
};

struct data_blk_1000
{
    unsigned short int type;
    unsigned short int next_blk_byte;
    unsigned char  encoding_fmt;
    unsigned char  word_order;
    unsigned char  rec_length;
    unsigned char  reserved;
};

struct data_blk_1001
{
    unsigned short int type;
    unsigned short int next_blk_byte;
    unsigned char  timing_quality;
    unsigned char  micro_sec;
    unsigned char  reserved;
    unsigned char  frame_count;
};
=========================================================================*/

static struct  data_blk_1000 *blk_1000;
static struct  data_blk_1001 *blk_1001;
static double  InitialUncorrTime;
static double  FiltDelay;
static double  ObservedDt;
static int     TotalSamples;
static int     do_write = 1;
static int     foundDftFile;

extern FILE    *Fp_log;


/*===================================================================
    STEIM COMPRESSION -
    Output: An array of data records containing steim compressed data.
 ===================================================================*/
long Steim_comp(Fp, p_dbuf, header, p_seed_data_record, segm)

FILE   *Fp;                  /* File pointer to output Seed volume */
float  *p_dbuf;              /* Input data array; no max size */
char   *header;              /* Input data Seed header */
int    *p_seed_data_record;  /* Pointer to output compressed data */
struct data_segment *segm;
{
DATA_STATE  ds;
int   dbuf_index;
int   diff;
int   state = _START_STATE;
int   d[4];
int   stat[4];
int   ck = 0;
int   wk = 0;
int   token, i;
int   nsamples;
char   new_header[DATA_REC_HD_SIZE]; /* header for compressed records */
struct seed_data_hdr *data_hdr;
struct seed_data_hdr *new_data_hdr;
int beg_of_data = DATA_REC_HD_SIZE;
char station[4];

/*
 * Reminder:
 * LRECL            = 4096
 * DATA_REC_HD_SIZE = 64
 * FSDH_SIZE        = 48
 * Data record header section is 64 bytes long.
 * Fix section is 48 bytes long.
 * We add blockettes 1000 and 1001, each 8 bytes long.
 * There is no more room for extra blockettes.
*/

    memset(new_header, 0, DATA_REC_HD_SIZE);
    data_hdr     = (struct seed_data_hdr *)(header+8);
    new_data_hdr = (struct seed_data_hdr *)(new_header+8);

/*-----------------------------------------------------------*\
       Build the seed data record initial state
\*-----------------------------------------------------------*/
    ds.nom_sint          = 1.0 / segm->srate;
    ds.frames_per_record = (LRECL - beg_of_data)/64;
    ds.record_offset     = beg_of_data / sizeof(int);
    ds.seed_frame        = 0;
    ds.seed_index        = 3;    /* points past initial w0, x0, xN */
    ds.w0                = 0;
    ds.num_data_rec      = 0;
    for (i=0; i<4; stat[i++]=0);


/*-----------------------------------------------------------*\
       Initialize x sub -1 for use in computing the forward
       and reverse integration constants x sub 0 and x sub N.
\*-----------------------------------------------------------*/
    ds.get_new_x0       = TRUE;
    ds.xN               = 0;

    /*-----------------------------------------------------------*\
       Copy all the values from the input header, then set
       values in the new header.
       There are 2 extra blockettes.
    \*-----------------------------------------------------------*/
    memcpy(new_header, header, FSDH_SIZE);
    strncpy(new_header, "      D ", 8);
    new_data_hdr->nsamples                = 0;
    new_data_hdr->number_blockettes       = 2;
    new_data_hdr->beg_first_blk           = FSDH_SIZE;
    new_data_hdr->beg_of_data             = beg_of_data;

/*
 * Get timespan system time (uncorrected) and filters delay
 */
    InitialUncorrTime = segm->uncorrBegTime;
    FiltDelay = segm->filtdelay;
    ObservedDt = segm->observ_dt;

    nsamples = segm->nsamples;

/*
 * Init output number of samples; this variable is used to
 * compute the seed data record time as:
 *        t = InitialUncorrTime + (TotalSamples * sint)
 */
    TotalSamples = 0;

/*
 * Build extra header blockettes 1000 & 1001, starting at new_header+48.
 * Encoding format is 10 -> steim-1 compression.
 */
    blk_1000_1001(new_header+FSDH_SIZE, 10);

    sprintf(station, "%.4s", new_data_hdr->station);

/*
 * Look for a .dft file matching InitialUncorrTime and station name.
 * If .dft file is found, get timeDrift samples (see array of
 * structures 'TimeDrift'.
 * This array will be used in finish_record() by GetEstimDt_(utime)
 * to compute the estimated delta_t.
 * Don't forget to free the array of TimeDrift: see below call
 * freeTimeDrift().
 */

    foundDftFile = loadDftFile(InitialUncorrTime, station);

if (1)
{
    char s2[24];
    time_asc(&(new_data_hdr->time), s2);
    printf("  comp_steim: %.6s %.4s %.2s %.3s %6d %s (uncorr) it=%.5f\n",
            new_header,
            new_data_hdr->station,
            new_data_hdr->network,
            new_data_hdr->channel,
            nsamples,
            s2,
            ds.nom_sint);
}

if (0) printf("  comp_steim: beg_of_data=%d\n", new_data_hdr->beg_of_data);

    for (dbuf_index=0; dbuf_index<nsamples; dbuf_index++ )
    {

        diff = ((int)p_dbuf[dbuf_index]) - ds.xN;
        ds.xN = (int)p_dbuf[dbuf_index];

        if (ds.get_new_x0)
        {
            ds.x0 = (int)p_dbuf[dbuf_index];
            ds.get_new_x0 = FALSE;
if (0) printf("---- %3d get new x0 : ds.x0=%d p_dbuf[%d]=%d\n",
     dbuf_index, ds.x0, dbuf_index, (int)p_dbuf[dbuf_index]);
        }


        /*-----------------------------------------------------------*\
           Check for a one-byte diff - _D1_
        \*-----------------------------------------------------------*/
        if ((diff <= 127) && (diff >= -128))
        {
            token = 0;
        }

        /*-----------------------------------------------------------*\
           Check for a two-byte diff - _D2_
        \*-----------------------------------------------------------*/
        else if ((diff <= 32767) && (diff > -32768))
        {
            token = 1;
        }

        /*-----------------------------------------------------------*\
           Must be a four-byte diff - _D4_
        \*-----------------------------------------------------------*/
        else
        {
            token = 2;
        }

        /*-----------------------------------------------------------*\
           Make the transition ...
        \*-----------------------------------------------------------*/
        {
        /* (begin scope local variable tran_index) */
          int tran_index = state * 3 + token;

            if (transition[tran_index].unget)
            {
                dbuf_index -= transition[tran_index].unget;
                ds.xN = (int)p_dbuf[dbuf_index];
            }

            if (-1 < transition[tran_index].diff_index)
            {
                d[transition[tran_index].diff_index] = diff;
            }

        /* The state assignment must be done last. */
            state = transition[tran_index].new_state;

if (0) printf("---- %3d diff=%d (%d - %d)     token %d state %d\n",
    dbuf_index, diff, (int)p_dbuf[dbuf_index], ds.xN, token, state);

        /* (end scope local variables) */
        }

        /*-----------------------------------------------------------*\
           Got to a final state, put values into data section ...
        \*-----------------------------------------------------------*/
        if (final[state])
        {
            switch (state)
            {
                case _D4_f:
            /* one 4-byte difference (one 32 bit sample) */
                    wk = d[0];
                    ck = 3;
                    new_data_hdr->nsamples += 1;
                    break;
                case _D2_D2_f:
            /* two 2-byte differences (two 16 bit samples) */
                    wk = ((d[0]&0xFFFFL) << 16) |
                          (d[1]&0xFFFFL);
                    ck = 2;
                    new_data_hdr->nsamples += 2;
                    break;
                case _D1_D1_D1_D1_f:
            /* four 1-byte differences (four 8 bit samples) */
                    wk = ((d[0]&0xFFL) << 24) |
                         ((d[1]&0xFFL) << 16) |
                         ((d[2]&0xFFL) << 8)  |
                          (d[3]&0xFFL);
                    ck = 1;
                    new_data_hdr->nsamples += 4;
                    break;
            }
            stat[ck]++;
            state = _START_STATE;
            if (-1 == Add_word(Fp, p_seed_data_record, new_header, wk, ck, &ds))
                return(-1);

        } /* end if */

    }   /* end for loop */

    /*-----------------------------------------------------------*\
       Ran out of input, decide what to do with
       the data already in the buffer ...
    \*-----------------------------------------------------------*/
    if (!final[state])
    {
        switch (state)
        {
            case _START_STATE:
        /* nothing !*/
                break;

            case _D1:
                ck = 3;
                stat[ck]++;
                wk = d[0];
                new_data_hdr->nsamples += 1;
                break;

            case _D2:
                ck = 3;
                stat[ck]++;
                wk = d[0];
                new_data_hdr->nsamples += 1;
                break;

            case _D1_D1:
                ck = 2;
                stat[ck]++;
                wk = ((d[0]&0xFFFFL) << 16) | (d[1] & 0xFFFFL);
                new_data_hdr->nsamples += 2;
                break;

            case _D1_D1_D1:
                ck = 2;
                stat[ck]++;
                wk = ((d[0]&0xFFFFL) << 16) | (d[1] & 0xFFFFL);
                new_data_hdr->nsamples += 2;

           /*:: call to Add_word may finish a record. */
                ds.xN = p_dbuf[nsamples-2];

                if (-1 == Add_word(Fp, p_seed_data_record, 
                   new_header, wk, ck, &ds )) 
                return(-1);

                ck = 3;
                stat[ck]++;
                wk = d[2];
                new_data_hdr->nsamples += 1;

           /*:: Get new X0 value if necessary.    */
           /*:: Explicitly set xN to last value.  */
           /*:: We may be starting a new record.  */
                if (ds.get_new_x0) {
                        ds.x0 = p_dbuf[nsamples-1];
                        ds.get_new_x0 = FALSE;
                }
                ds.xN = p_dbuf[nsamples-1];

                break;

        }       /* end switch */

        if (-1 == Add_word(Fp, p_seed_data_record,
                        new_header, wk, ck, &ds )) 
            return(-1);

    }   /* end if */


    if (new_data_hdr->nsamples)
    {
      /* Finish the frame ... */
      p_seed_data_record[ds.record_offset+(ds.seed_frame*16)] = ds.w0;

      /* Finish the record ... */
      finish_record(Fp, p_seed_data_record, new_header, &ds);
    }

/*
 * Free memory allocated for timeDrift.
 */
    freeTimeDrift();

    if (0 && foundDftFile == 0)
    {
       fprintf(stderr,"\n");
       fprintf(stderr,
   "  =================================================================\n");
       fprintf(stderr,
   "  WARNING: Steim_comp: couldn't find a valid estimated time drift\n");
       fprintf(stderr,"  required for seed format.\n");
       fprintf(stderr,"  Time was corrected by observed delta_t: %.4f\n",
           ObservedDt);
       fprintf(stderr,
   "  =================================================================\n");
       fprintf(stderr,"\n");
    }

    return ds.num_data_rec;
}

/*===================================================================
    blk_1000_1001
    Build Seed data record extras header blockettes 1000 & 1001
    Blockette 1000 gives
       - encoding_fmt (steim = 10)
       - rec_length
       - word_order
    Blockette 1001 is there but doesn't contain info.
 *==================================================================*/
void blk_1000_1001(pt, encoding_fmt)
char *pt;           /* set to (new_header+FSDH_SIZE) with FSDH_SIZE=48 */
int  encoding_fmt;  /* set to steim1 cmpression (=10) */
{
char temp[10];

/*
 * Blockette 1000; size = 8 bytes
 */
    blk_1000 =  (struct data_blk_1000 *) pt;
    memset(blk_1000, 0, sizeof(struct data_blk_1000));
    blk_1000->type = 1000;
    blk_1000->next_blk_byte = 56;           /* FSDH_SIZE=48 + 8 = 56 */
    blk_1000->encoding_fmt = encoding_fmt;
    find_wordorder(temp);
    if (temp[0] == '0') blk_1000->word_order = 0;
    else                blk_1000->word_order = 1;
    blk_1000->rec_length = atoi(LREC_LEN);
    blk_1000->reserved = 0;

if (0) printf("==== blk_1000_1001: word_order=%d record_length=%d fmt=%d\n",
               blk_1000->word_order,
               blk_1000->rec_length,
               blk_1000->encoding_fmt);

/*
 * Blockette 1001; size = 8 bytes
 */

    blk_1001 =  (struct data_blk_1001 *) (pt+8);
    memset(blk_1001, 0, sizeof(struct data_blk_1001));
    blk_1001->type = 1001;
    blk_1001->next_blk_byte = 0;
}


/*===================================================================
    Add_word - append a compressed word to the data record.
 ===================================================================*/
static int Add_word(Fp, p_seed_data_record, header, wk, ck, ds )
FILE   *Fp;
int    *p_seed_data_record;
char   *header;
int     wk;
int     ck;
DATA_STATE *ds;
{

    ds->w0 |= ck << ( 2 * ( 15 - ds->seed_index ) );

    p_seed_data_record
      [ds->record_offset+(ds->seed_frame*16)+ds->seed_index] = wk;
    ds->seed_index++;

if (0) printf("Add_word: ds->seed_index=%3d wk=%0X ck=%d ds->w0=%0X\n",
               ds->seed_index, wk, ck, ds->w0);

    if (ds->seed_index > 15)
    {
/* Finish a frame ... */
        p_seed_data_record[ds->record_offset+(ds->seed_frame*16)] = ds->w0;

/* Start next frame ... */
        ds->seed_index = 1;
        ds->seed_frame ++;
        ds->w0 = 0;
        if (ds->seed_frame >= ds->frames_per_record)
        {
            finish_record(Fp, p_seed_data_record, header, ds);
        }
    }
    return (0);
}


/*===================================================================
    FINISH_RECORD
    - Fill in rest of the record fixed section data header (FSDH).
    - Copy FSDH into data record.
    - write compressed data record to disk.
    - Update compression stuff
    - Update time variables into FSDH
 ===================================================================*/
static void finish_record( Fp, p_seed_data_record, header, ds )
FILE   *Fp;
int    *p_seed_data_record;
char   *header;
DATA_STATE *ds;
{
char tmp[10];
double dbltime;
char s2[24];
struct seed_data_hdr *data_hdr;
double dt;
extern double GetEstimDt_(double);

    data_hdr  = (struct seed_data_hdr *)(header+8);

    p_seed_data_record[ds->record_offset+1] = ds->x0;
    p_seed_data_record[ds->record_offset+2] = ds->xN;
    ds->get_new_x0 = TRUE;

/*
 * Calculate data record begtime and time correction and write into header.
 */
    dbltime = InitialUncorrTime - FiltDelay + TotalSamples * ds->nom_sint;

/*
 * Get and check time correction
 */
    dt = GetEstimDt_(dbltime);

    if (dt == (double) UNKNOWN)
    {
       dbltime -= ObservedDt;
    }
    else
    {
       data_hdr->number_time_corrections = (int) (-dt * 10000.0);
    }
    dbt_to_asc(dbltime, s2);
    asc_to_dbt(s2, &(data_hdr->time), &dbltime);

if (0) printf("==== steim: finish: dt=%ld ObservedDt=%.4f\n",
               data_hdr->number_time_corrections, ObservedDt);

/* Write record number in header*/

    sprintf(tmp, "%06d", ds->num_data_rec+1);
    memcpy(header, tmp, strlen(tmp));

/* Copy FSDH header into seed data record */

    memcpy(p_seed_data_record, header, DATA_REC_HD_SIZE);

/* Write data record */

    if (do_write) if (fwrite(p_seed_data_record, 1, LRECL, Fp) != LRECL)
    {
      fprintf (Fp_log, "ERROR output_steim: write failed\n");
      exit(1);
    }

/* Debug info */

if (0)
{
   /* Compute corrected time and convert to string */
   tstruct_to_dbt(&(data_hdr->time), &dbltime);
   dbltime += (double) data_hdr->number_time_corrections / 10000.0;
   dbt_to_asc(dbltime, s2);

   printf("steim: finish: %.6s %.4s %.2s %.3s %6d %s tc=%d tot=%d\n",
            header,
            data_hdr->station,
            data_hdr->network,
            data_hdr->channel,
            data_hdr->nsamples,
            s2,
            (int) data_hdr->number_time_corrections,
            TotalSamples + data_hdr->nsamples);
}


/*
 * Set up DATA_STATE for next record
 */

    /* increment record number */
    ds->num_data_rec++;

    /* start first frame of next record */
    ds->seed_frame = 0;

    /* leave room for w0, x0, xN */
    ds->seed_index = 3;
    ds->w0 = 0;

/* Update number of samples */
    TotalSamples += data_hdr->nsamples;

    data_hdr->nsamples = 0;
}

