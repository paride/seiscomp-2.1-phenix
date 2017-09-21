/*======================================================================
    Program synchro.c

    Library for Titan to Sismalp converter

    Author: J.-F. Fels, OMP, Toulouse
*======================================================================*/
#include "titan.h"
#include "inter.h"
#include "proto.h"

#ifdef ANSI_C
static void lookForDataOffset (int);
#else
static void lookForDataOffset ();
#endif

extern TITFILE  *Fp_tit;
extern FILE     *Fp_log;
extern struct   option opt;
extern int      byteswap;
extern struct   acq acq;
extern double   BasicSamprate;
extern STA_INFO STA_infos;
extern struct   Channel Channel[];
extern int      DataOffsetCorr;
extern double   SystTime;


/*===================================================================*
    sync_on_frame:
    - synchronize reading on begining of frames. Test on 10 consecutive
      frames at least.
    - Get basic sample rate from the first TIME frame
    - Get acquisition system params from the first 32 INFO frames
    - Get data offset form the first OFFSET frames
 *===================================================================*/
int sync_on_frame(initial_file_ofs)
int initial_file_ofs;
{
int   file_ofs;
int   sync;
int   frame_type;
char  frame[20];
int   found_srate = 0;
int   found_info  = 0;
int   n;
int   db = 0;

    file_ofs = initial_file_ofs;
top:
    if (teof(Fp_tit)) goto eof;

    file_ofs = sync_phase1(file_ofs);
    if      (file_ofs == -1) goto eof;
    else if (file_ofs == -2) return -1;

    file_ofs = sync_phase2();
    if (file_ofs < 0) goto eof;


if (0) fprintf(Fp_log,"  sync_on_frame: start @ ofs %d\n", file_ofs);

/*============ Get basic sample rate from time frames ============*/

    while (tread(frame, 1, 12, Fp_tit))
    {
        GET_SYNC;
        GET_FRAME_TYPE;
        if (SYNC_KO)
        {
            fprintf(Fp_log,"  sync_on_frame: lost frame synchro @ofs %d\n",
                ttell(Fp_tit));
            file_ofs = ttell(Fp_tit);
            goto top;
        }

        if (frame_type == TIME)
        {
            n = bytes2int4(0,frame[8],frame[9],frame[10]);
            BasicSamprate = (n & 0X400000) ?
                BASIC_SAMPRATE2 : BASIC_SAMPRATE1;
            found_srate = TRUE;
            break;
        }
    }
    tseek(Fp_tit, file_ofs, SEEK_SET);


/*====== Get acquisition system parameters: look for info frames  ======*/

    while (tread(frame, 1, 12, Fp_tit))
    {
        GET_SYNC;
        GET_FRAME_TYPE;
        if (SYNC_KO)
        {
            fprintf(Fp_log,"  sync_on_frame: lost frame synchro @ofs %d\n",
                ttell(Fp_tit));
            file_ofs = ttell(Fp_tit);
            goto top;
        }

        if (frame_type == INFO)
        {
            found_info = processInfoFrames(frame, "sync_on_frame");
            if (found_info == FALSE)
            {
                fprintf(Fp_log,"  sync_on_frame: skip 12 bytes\n");
                file_ofs = ttell(Fp_tit) + 12;
                goto top;
            }
        }

        if (found_srate == TRUE && found_info == TRUE)
            break;
    }

    if (teof(Fp_tit)) goto eof;
    if (acq.AdcDelay == (double) UNKNOWN)
    {
        fprintf(Fp_log,"  ERROR: sync_on_frame: can't find valid ");
        fprintf(Fp_log,"acquisition system parameters\n");
        exit(1);
    }

/*
 * Search titan file for data offset frames.
 * If they are found, we must get the initial data offsets values,
 * in order to correct the first samples.
 * Note that the data offset setting is also given in the info frames.
 */ 

    lookForDataOffset(file_ofs);

/* Rewind file to start offset */

    tseek(Fp_tit, file_ofs, SEEK_SET);

    if (ttell(Fp_tit) != file_ofs)
    {
        fprintf(Fp_log,"  ERROR: sync_on_frame: fseek failed\n");
        exit(1);
    }

    if (file_ofs == 12)
    {
            tseek(Fp_tit, 0, SEEK_SET);
            file_ofs = 0;
    }
    if (db) fprintf(Fp_log,"  sync_on_frame: SUCCES @ ofs %d\n", file_ofs);
    return file_ofs;

eof:
    fprintf(Fp_log,"  sync_on_frame: EOF at ");
    fprintf(Fp_log,"file ofs %d\n", ttell(Fp_tit));
    return -1;
}


/*===================================================================*
   synchro test 1:
      Set file offset to the first frame with sync OK, starting
      at the specified offset.
 *===================================================================*/
int sync_phase1(initial_file_ofs)
int initial_file_ofs;
{
char buf[511];
int   sync1, sync2, sync3;
int   i, j, i0, nOK;
int   kkk = 0;
int   fileOfs;
int   db = 0;

   tseek(Fp_tit, initial_file_ofs, SEEK_SET);

if (db) printf("entering sync_phase1: file ofs %d\n", ttell(Fp_tit));

   while (1)
   {
   /* Check end of file */
      if (teof(Fp_tit))
      {
         fprintf(Fp_log,
            "  sync_phase1: EOF at file ofs %d\n", ttell(Fp_tit));
         return -1;
      }

   /* Don't check sync farther than xxx bytes */
      if ((ttell(Fp_tit)-initial_file_ofs) > 10000000)
      {
         fprintf(Fp_log,
            "  sync_phase1: sync not found passed ofs %d; giving up...\n",
            ttell(Fp_tit));
         return -2;
      }

      tread(buf, 1, 250, Fp_tit);

      if (++kkk > 4000)
      {
         printf("sync_phase1: %.2f MBytes\n",
                   (double)(ttell(Fp_tit)) / 1000000.0);
         kkk = 0;
      }


      i0 = -1;
      j = 0;
      nOK = 0;
      for (i=0; i<200; i++)
      {
         sync1 = (int)(buf[i] & 0xF0);
         if (sync1 == 0X50 || sync1 == 0XA0)
         {
            sync2 = (int)(buf[i+12] & 0xF0);
            sync3 = (int)(buf[i+24] & 0xF0);
            if (((sync1^sync2) & 0XF0) == 0XF0 && sync1 == sync3)
            {
               if (i0 < 0) i0 = i;

if (0) printf("sync1 %d sync2 %d sync3 %d i=%d j=%d i-j=%d\n",
                 sync1, sync2, sync3, i, j, (i-j));

               if ((i-j) == 12)
               {
                  if (++nOK >= 10) break;
               }
               j = i;
            }
         }
      }

   /*
    * Synchro not found
    */
      if ((i0 < 0) || (nOK == 0))  continue;
   /*
    * Found synchro
    */
      if (nOK == 10)               break;
   }

   fileOfs = ttell(Fp_tit);
if (db) printf("i0=%d nOK=%d ofs=%d\n", i0, nOK, fileOfs);

   if (i0 == 11) i0 = 0;
   else ++i0;
if (db) printf("i0=%d\n", i0);

   if (fileOfs > initial_file_ofs)
      initial_file_ofs = fileOfs - 250;
   initial_file_ofs += i0;
if (db) printf("sync at %d\n", initial_file_ofs);

   tseek(Fp_tit, initial_file_ofs, SEEK_SET);

if (db) printf("sync_phase1: synchro at file ofs %d\n", ttell(Fp_tit));

   return initial_file_ofs;
}


/*===================================================================*
   synchro test 2:
      look for at least 10 consecutive frames with sync OK
 *===================================================================*/
int sync_phase2()
{
int   nframe_ok;
int   sync_byte_ofs;
int   sync, sync1, sync2;
char  frame[20];
int   jjj = 0;
int   db = 0;
int   db0 = 0;

    sync_byte_ofs = ttell(Fp_tit);
if (db) printf("entering sync_phase2: file ofs %d\n", sync_byte_ofs);
    nframe_ok = 0;
    sync2 = -1;
    while (tread(frame, 1, 12, Fp_tit))
    {
        if (++jjj > 10000)
        {
          printf("  sync_phase2: %.6f MBytes\n",
                    (double)(ttell(Fp_tit)) / 1000000.0);
          jjj = 0;
        }

        if (teof(Fp_tit)) goto eof;
        GET_SYNC;
        sync1 = sync;
        if (db0) printf("==== sync_phase2 read 12 sync %02X @ ofs %d\n",
                   sync, ttell(Fp_tit));
        if (sync2 != -1)
        {
            if      (((sync1^sync2) & 0XF0) == 0XF0);
            else
            {
                if (db) printf("==== synchro KO; restart sync_phase1\n");
                sync_phase1(ttell(Fp_tit));
            }
        }
        if ((++nframe_ok) >= 10) break;
        sync2 = sync1;
    }

/* Rewind file to start offset */

/*
    if (sync_byte_ofs > 12) sync_byte_ofs -= 12;
*/
    tseek(Fp_tit, sync_byte_ofs, SEEK_SET);
    if (ttell(Fp_tit) != sync_byte_ofs)
    {
        fprintf(Fp_log,"  ERROR: sync_phase2: fseek failed\n");
        exit(1);
    }
    if (sync_byte_ofs == 12)
    {
        tseek(Fp_tit, 0, SEEK_SET);
        tread(frame, 1, 12, Fp_tit);
        GET_SYNC;
        sync1 = sync;
        tread(frame, 1, 12, Fp_tit);
        GET_SYNC;
        sync2 = sync;

        if (((sync1^sync2) & 0XF0) == 0XF0)
        {
            tseek(Fp_tit, 0, SEEK_SET);
            sync_byte_ofs = 0;
        }
        else
        {
            tseek(Fp_tit, sync_byte_ofs, SEEK_SET);
        }
    }

if (db) fprintf(Fp_log,"sync_phase2: SUCCES @ ofs %d\n",sync_byte_ofs);
    return sync_byte_ofs;

eof:
    fprintf(Fp_log,"  sync_phase2: EOF at ");
    fprintf(Fp_log,"file ofs %d\n", ttell(Fp_tit));
    return -1;
}


/*==================================================================*/
void check_sync(frame)
char *frame;
{
int   file_ofs;
char  str[40];
char  prev_f[14], curr_f[14], next_f[14];
int   prev_ofs, curr_ofs, next_ofs;

    file_ofs = ttell(Fp_tit);
/*
printf("==== check_sync ofs=%d\n", file_ofs);
*/

/* Get previous, current, next synchro bits */

/* rewind 2 streams */
    tseek(Fp_tit, (file_ofs-24), SEEK_SET);

/* read previous stream */
    if (tread(frame, 1, 12, Fp_tit) != 12) return;
    memcpy(prev_f, frame, 12);
    prev_ofs = ttell(Fp_tit);


/* read current stream */
    if (tread(frame, 1, 12, Fp_tit) != 12) return;
    memcpy(curr_f, frame, 12);
    curr_ofs = ttell(Fp_tit);

    if (teof(Fp_tit)) return;

/* read next stream */
    if (tread(frame, 1, 12, Fp_tit) != 12) return;
    memcpy(next_f, frame, 12);
    next_ofs = ttell(Fp_tit);


    if (TIME_OK(SystTime)) time_asc4(str, SystTime);
    else                   sprintf(str,"time UNKNOWN");
    fprintf(Fp_log,"\n  check_sync: lost frame synchro @ ofs %d ",
        file_ofs);
    fprintf(Fp_log," (%s)\n", str);


/* rewind and read current stream */
    tseek(Fp_tit, file_ofs-12, SEEK_SET);
    tread(frame, 1, 12, Fp_tit);

    return;
}


/*================================================================*
    lookForDataOffset
        - set data offset correction variable
        - search offset frames in titan file
        - if not found, stop looking after 10 MBytes.
 *================================================================*/
static void lookForDataOffset(file_ofs)
int  file_ofs;
{
char   frame[20];
int    sync, frame_type;
int    found_offset[NCHAN];
int    numcomp[NCHAN];
int    chan, comp;
static int dbug = 0;
static int  kkk;

/*
 * Set data offset correction according to the setting found
 * in the info frames.
 * Remember:
 *     if "absOffset' is 0, amplitude of data samples
 *     is not absolute and a correction is need to get the
 *     absolute value.
 *     if "absOffset' is 1, amplitude of data samples
 *     is absolute and no correction is needed.
 */

    if (STA_infos.chan[0].absOffset == 0) DataOffsetCorr = ABS_OFS;
    else                                  DataOffsetCorr = NO_OFS;

    for (chan=0; chan<NCHAN; chan++) for (comp=0; comp<NCOMP; comp++)
    {
        Channel[chan].abs_ofs[comp] = 0;
        Channel[chan].rel_ofs[comp] = 0;
        Channel[chan].offset[comp]  = 0;
        numcomp[chan] = 0;
    }

    while (tread(frame, 1, 12, Fp_tit))
    {

        if ((ttell(Fp_tit) - file_ofs) > 10000000) break;

        if (dbug && ++kkk > 333333)
        {
          fprintf(Fp_log,"  WARNING: lookForDataOffset: offset frames");
          fprintf(Fp_log," not found; file_ofs %d\n", ttell(Fp_tit));
          kkk = 0;
        }

        GET_SYNC;
        if (SYNC_KO)
        {
           fprintf(Fp_log,"  WARNING: lookForDataOffset: lost ");
           fprintf(Fp_log,"frame synchro @ ofs %d\n",ttell(Fp_tit));
           return;
        }

        GET_FRAME_TYPE;
        switch (frame_type)
        {

          case DATA_PRE_TRIG:
          case DATA_POST_TRIG:
              chan = (short) (frame[9] >> 4) & 0XF;
              if (chan < NCHAN && numcomp[chan] == 0)
                numcomp[chan] = ((frame[10] >> 4) & 0X01) ? 1:3;
              break;

          case OFFSET:
              processOffsetFrame(frame, 1);

              for (chan=0; chan<NCHAN; chan++) found_offset[chan] = FALSE;

              for (chan=0; chan<NCHAN; chan++) if (numcomp[chan])
              {
                if (opt.chan >= 0 && (chan != opt.chan))
                    continue;
                if (chan == 13)
                    continue;
                if (Channel[chan].abs_ofs[0] != 0)
                    found_offset[chan] = TRUE;
              }
              for (chan=0; chan<NCHAN; chan++) if (found_offset[chan])
              {
                  goto success;
              }
              break;

          case TIME: 
          case INFO:
          case MISC:
              break;

          case TIME_CORRECTED:
          case FILLING:
          default:
              fprintf(Fp_log, "  lookForDataOffset: frame type ");
              fprintf(Fp_log, "not supported @ ofs %d\n",ttell(Fp_tit));
              return;

        }  /* end switch */

    }  /* end while */

    if (1)
    {
        printf("  lookForDataOffset: No offset frames found\n");

        if (STA_infos.chan[0].absOffset == 0)
            printf("          WARNING: this desagree with info frames.\n");

        printf( "  lookForDataOffset: NO OFFSET CORRECTION");
        printf("\n");
    }
    return;

success:

    if (1)
    {
        printf("  lookForDataOffset: offset frames found\n");
        if (STA_infos.chan[0].absOffset == 1)
            printf("          WARNING: this desagree with info frames.\n");

        printf( "  lookForDataOffset: correction set to ");
        if      (opt.do_offset == NO_OFS)   printf("NO CORR");
        else if (opt.do_offset == REL_OFS)  printf("RELATIVE");
        else if (opt.do_offset == ABS_OFS)  printf("ABSOLUTE");
        else if (DataOffsetCorr == NO_OFS)  printf("NO CORR");
        else if (DataOffsetCorr == ABS_OFS) printf("ABSOLUTE");
        printf("\n");
    }
/*
 * Some checkings
 */
    if (dbug)
    {
      if ((Channel[0].abs_ofs[0] == 0) || 
          (Channel[1].abs_ofs[0] == 0))
      {
        if ((Channel[0].abs_ofs[0] == 0) &&
            (Channel[1].abs_ofs[0] != 0))
        {
           fprintf(Fp_log, "  WARNING: lookForDataOffset: ");
           fprintf(Fp_log, "offset correction:\n");
           fprintf(Fp_log,
               "           null for channel 0 but not null for channel 1\n");
        }
        if ((Channel[0].abs_ofs[0] != 0) &&
            (Channel[1].abs_ofs[0] == 0))
        {
           fprintf(Fp_log, "  WARNING: lookForDataOffset: ");
           fprintf(Fp_log, "offset correction:\n");
           fprintf(Fp_log,
               "           null for channel 1 but not null for channel 0\n");
        }
      }
    }
    return;
}

