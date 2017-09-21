/*   Client Test Program. Gets data and blockettes from servers.
     Copyright 1994-1996 Quanterra, Inc.
     Written by Woodrow H. Owens

Edit History:
   Ed Date      By  Changes
   -- --------- --- ---------------------------------------------------
    0 11 Mar 94 WHO First created.
    1 30 Mar 94 WHO Uses new service calls.
    2  6 Jun 94 WHO Two new status strings added.
    3  9 Jun 94 WHO Cleanup to avoid warnings.
    4 13 Dec 94 WHO Add verbose flag to show more contents of packets.
    5 27 Feb 95 WHO Start of conversion to run on OS9.
    6  3 Dec 96 WHO Add support for Blockette Records.
    7 12 Jun 97 WHO Show seed sequence number for each record.
*/
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#ifndef _OSK
#include <unistd.h>
#include <termio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <sys/time.h>
#endif
#include <signal.h>
#include "dpstruc.h"
#include "seedstrc.h"
#include "stuff.h"
#include "timeutil.h"
#include "seedutil.h"
#include "service.h"
#include "pascal.h"
#ifdef _OSK
#include "os9stuff.h"
#endif

#ifdef __cplusplus
#define this _this
#endif

char name[5] = "DATA" ;
char sname[5] = "*" ;
tstations_struc stations ;
typedef char char23[24] ;
boolean verbose = FALSE ;

char23 stats[13] = { "Good", "Enqueue Timeout", "Service Timeout", "Init Error",
                       "Attach Refused", "No Data", "Server Busy", "Invalid Command",
                       "Server Dead", "Server Changed", "Segment Error",
                       "Command Size", "Privileged Command" } ;

extern pchar seednamestring (seed_name_type *sd, location_type *loc) ;

  double seed_jul (seed_time_struc *st)
    begin
      double t ;
      
      t = jconv (st->yr - 1900, st->jday) ;
      t = t + (double) st->hr * 3600.0 + (double) st->minute * 60.0 + 
          (double) st->seconds + (double) st->tenth_millisec * 0.0001 ;
      return t ;
    end
    
  void depad (pchar ain, pchar sout, short cnt)
    begin
      short n ;
      
      for (n = 0 ; n < cnt ; n++)
        sout[n] = ain[n] ;
      sout[cnt] = '\0' ;
      for (n = cnt - 1 ; n > 0 ; n--)
        if (sout[n] != ' ')
          then
            begin
              sout[n + 1] = '\0' ;
              break ;
            end
    end

  int main (int argc, char *argv[], char **envp)
    begin
      typedef seed_fixed_data_record_header *tpdh ;
      typedef data_only_blockette *tpdob ;
      typedef data_extension_blockette *tpdeb ;
      typedef timing *tptim ;
      typedef murdock_detect *tpmd ;
      typedef threshold_detect *tptd ;
      typedef cal2 *tpcal2 ;
      typedef step_calibration *tpstep ;
      typedef sine_calibration *tpsine ;
      typedef random_calibration *tprand ;
      typedef opaque_hdr *tophdr ;

      pclient_struc me ;
      pclient_station this ;
      short i, j, k, err ;
      boolean alert, mh ;
      pdata_user pdat ;
      seed_record_header *pseed ;
      tophdr popaque ;
      pselarray psa ;
      char s[80], s2[130] ;
      char af[10], ifl[10], df[10] ;
      float a ;
      double b ;
      tpdh pdh ;
      tpdob pdob ;
      tpdeb pdeb ;
      tptim ptim ;
      tpmd pmd ;
      tptd ptd ;
      tpstep pstep ;
      tpsine psine ;
      tprand prand ;
      tpcal2 pcal2 ;
      pchar pc1, pc2 ;

/* Allow override of station name on command line and -V option */
      for (j = 1 ; j < argc ; j++)
        begin
          strcpy (s, argv[j]) ;
          upshift(s) ;
          if (strcmp(s, "-V") == 0)
            then
              verbose = TRUE ;
            else
              begin
                strncpy(sname, s, 4) ;
                sname[4] = '\0' ;
              end
        end

/* Generate an entry for all available stations */      
      cs_setup (&stations, name, sname, TRUE, TRUE, 10, 5, CSIM_MSG or CSIM_DATA or CSIM_CAL or
              CSIM_EVENT or CSIM_TIMING or CSIM_BLK, 6000) ;

/* Create my segment and attach to all stations */      
      me = cs_gen (&stations) ;

/* For selector test, only accept data from ??BH?
      this = (pclient_station) ((long) me + me->offsets[0]) ;
      psa = (pselarray) ((long) me + this->seloffset) ;
      strcpy (&((*psa)[1]), "??BH?") ;
      this->sels[DATAQ].first = 1 ;
      this->sels[DATAQ].last = 1 ; */

/* Show beginning status of all stations */
      for (j = 0 ; j < me->maxstation ; j++)
        begin
          this = (pclient_station) ((long) me + me->offsets[j]) ;
#ifdef _OSK
          printf ("[%s] Status=%s\n", long_str(this->name.l),
                   (pchar) stats[this->status]) ;
#else
          printf ("[%s] Status=%s\n", long_str(this->name.l),
                   &(stats[this->status])) ;
#endif
        end
      
      do
        begin
          j = cs_scan (me, &alert) ;
          if (j != NOCLIENT)
            then
              begin
                this = (pclient_station) ((long) me + me->offsets[j]) ;
                if (alert)
                  then
#ifdef _OSK
                    printf("New status on station %s is %s\n", long_str(this->name.l),
                           &(&(stats[this->status]))) ;
#else
                    printf("New status on station %s is %s\n", long_str(this->name.l),
                           &(stats[this->status])) ;
#endif
                if (this->valdbuf)
                  then
                    begin
                      pdat = (pdata_user) ((long) me + this->dbufoffset) ;
                      for (k = 0 ; k < this->valdbuf ; k++)
                        begin
                          pseed = (seed_record_header *) &pdat->data_bytes ;
                          printf("[%4.4s] <%2d> Channel=%s Seq=%6.6s Received at=%s\n",
                              &this->name, k, seednamestring(&pseed->channel_id, 
                              &pseed->location_id), &pseed->sequence,
                              time_string(pdat->reception_time)) ;
                          if (verbose)
                            then
                              begin
                                b = seed_jul(&pseed->starting_time) ;
                                pdh = (tpdh) pseed ;
                                pdob = &pdh->dob ;
                                if ((pseed->samples_in_record) land (pseed->sample_rate_factor))
                                  then
                                    begin /* data record */
                                      pdeb = &pdh->deb ;
                                      b = b + (double) pdeb->usec99 / 1.0E6 ;
                                    end
                                else if ((pdob->blockette_type == 1000) land (pdob->next_offset))
                                  then
                                    begin /* might be timing blockette */
                                      ptim = (tptim) ((long) pdh + pdob->next_offset) ;
                                      if (ptim->blockette_type == 500)
                                        then
                                          b = b + (double) ptim->usec99 / 1.0E6 ; /* it is */
                                    end
                                printf ("     Time=%s   Blks=%d   Samples=%d\n",
                                    time_string(b), 
                                    pseed->number_of_following_blockettes,
                                    (short) pseed->samples_in_record) ;
                                if (pseed->activity_flags == SEED_ACTIVITY_FLAG_END_EVENT)
                                  then
                                    begin /* end of detection */
                                      printf ("     End of Detection\n") ;
                                    end
                                else if (pseed->samples_in_record)
                                  then
                                    if (pseed->sample_rate_factor)
                                      then
                                        begin /* data */
                                          a = pseed->sample_rate_factor ;
                                          if (a < 0.0)
                                            then
                                              a = 1.0 / (-a) ;
                                          strcpy (af, "--------") ;
                                          strcpy (ifl, "--------") ;
                                          strcpy (df, "--------") ;
                                          if (pseed->activity_flags and SEED_ACTIVITY_FLAG_CAL_IN_PROGRESS)
                                            then
                                              af[7] = 'C' ;
                                          if (pseed->activity_flags and SEED_ACTIVITY_FLAG_BEGIN_EVENT)
                                            then
                                              af[5] = 'B' ;
                                          if (pseed->activity_flags and SEED_ACTIVITY_FLAG_EVENT_IN_PROGRESS)
                                            then
                                              af[1] = 'P' ;
                                          if (pseed->IO_flags and SEED_IO_CLOCK_LOCKED)
                                            then
                                              ifl[2] = 'L' ;
                                          if (pseed->data_quality_flags and SEED_QUALITY_FLAG_MISSING_DATA)
                                            then
                                              df[3] = 'M' ;
                                          if (pseed->data_quality_flags and SEED_QUALITY_FLAG_QUESTIONABLE_TIMETAG)
                                            then
                                              df[0] = 'Q' ;
                                          printf ("     Activity Flags=%s   IO Flags=%s   Data Quality Flags=%s\n",
                                                  af, ifl, df) ;
                                          switch (pdob->encoding_format)
                                            begin
                                              case 10 : 
                                                begin
                                                  strcpy (s2, "Steim1") ;
                                                  break ;
                                                end
                                              case 11 : 
                                                begin
                                                  strcpy (s2, "Steim2") ;
                                                  break ;
                                                end
                                              case 19 : 
                                                begin
                                                  strcpy (s2, "Steim3") ;
                                                  break ;
                                                end
                                              default : strcpy (s2, "Unknown") ;
                                            end
                                          printf ("     Encoding Format=%s   Frequency=%5.2fHz  Clock Quality=%d%%\n",
                                                  s2, a, (short) pdeb->qual) ;
                                        end
                                      else
                                        begin /* messages */
                                          pc1 = (pchar) ((long) pseed + pseed->first_data_byte) ;
                                          pc2 = (pchar) ((long) pc1 + pseed->samples_in_record - 2) ;
                                          *pc2 = '\0' ;
                                          printf ("     %s\n", pc1) ;
                                        end
                                else if (pdob->blockette_type == 1000)
                                  then
                                    begin /* must be timing, detection, or calibration */
                                      ptim = (tptim) ((long) pdh + pdob->next_offset) ;
                                      switch (ptim->blockette_type)
                                        begin
                                          case 500 :
                                            begin
                                              depad (ptim->exception_type, s, 16) ;
                                              printf ("     %s Timemark   VCO Correction=%5.2f%%   Reception=%d%%   Count=%d\n",
                                                s, (float) ptim->vco_correction, 
                                                ptim->reception_quality, (long) ptim->exception_count) ;
                                              depad (ptim->clock_model, s, 32) ;
                                              depad (ptim->clock_status, s2, 128) ;
                                              printf ("     Model=%s\n", s) ;
                                              printf ("     Status:%s\n", s2) ;
                                              break ;
                                            end
                                          case 200 : ;
                                          case 201 :
                                            begin
                                              mh = (ptim->blockette_type == 201) ;
                                              pmd = (tpmd) ptim ;
                                              ptd = (tptd) ptim ;
                                              if (mh)
                                                then
                                                  printf ("     Murdock-Hutt") ;
                                                else
                                                  printf ("     Threshold") ;
                                              printf (" Detection  Counts=%3.0f  Period=%5.3f  Background=%3.0f\n",
                                                (float) pmd->signal_amplitude, (float) pmd->signal_period, (float) pmd->background_estimate) ;
                                              if (mh)
                                                then
                                                  depad (pmd->s_detname, s, 24) ;
                                                else
                                                  depad (ptd->s_detname, s, 24) ;
                                              printf ("     Flags=%d  Detector Name=%s", pmd->event_detection_flags, s) ;
                                              if (mh)
                                                then
                                                  printf ("  SNR=%d%d%d%d%d  Lookback=%d  Pick=%d\n",
                                                    (short) pmd->snr[0], (short) pmd->snr[1],
                                                    (short) pmd->snr[2], (short) pmd->snr[3],
                                                    (short) pmd->snr[4], (short) pmd->lookback_value,
                                                    (short) pmd->pick_algorithm) ;
                                                else
                                                  printf ("\n") ;
                                              break ;
                                            end
                                          case 300 : ;
                                          case 310 : ;
                                          case 320 : ;
                                          case 395 :
                                            begin
                                              pstep = (tpstep) ptim ;
                                              psine = (tpsine) ptim ;
                                              prand = (tprand) ptim ;
                                              switch (ptim->blockette_type)
                                                begin
                                                  case 300 :
                                                    begin
                                                      pcal2 = (tpcal2) &pstep->step2 ;
                                                      strcpy (s, "Step") ;
                                                      break ;
                                                    end
                                                  case 310 :
                                                    begin
                                                      pcal2 = (tpcal2) &psine->sine2 ;
                                                      strcpy (s, "Sine") ;
                                                      break ;
                                                    end
                                                  case 320 :
                                                    begin
                                                      pcal2 = (tpcal2) &prand->random2 ;
                                                      depad (prand->noise_type, s, 8) ;
                                                      strcat (s, " Noise") ;
                                                      break ;
                                                    end
                                                  default :
                                                    begin
                                                      pcal2 = NULL ;
                                                      strcpy (s, "Abort") ;
                                                      break ;
                                                    end
                                                end
                                              printf ("     %s Calibration", s) ;
                                              if (pcal2)
                                                then
                                                  begin
                                                    strcpy (s2, "-----M--") ;
                                                    if (pstep->calibration_flags and 0x40)
                                                      then
                                                        s2[1] = 'R' ;
                                                    if (pstep->calibration_flags and 0x20)
                                                      then
                                                        s2[2] = 'Z' ;
                                                    if (pstep->calibration_flags and 0x10)
                                                      then
                                                        s2[3] = 'P' ;
                                                    if (pstep->calibration_flags and 0x04)
                                                      then
                                                        s2[5] = 'A' ;
                                                    if (pstep->calibration_flags and 0x01)
                                                      then
                                                        s2[7] = '+' ;
                                                    printf ("   Duration=%6.4f    Flags=%s\n",
                                                      pstep->calibration_duration * 0.0001, s2) ;
                                                    if (ptim->blockette_type == 310)
                                                      then
                                                        printf ("     Sine Period=%6.4f  ", (float) psine->sine_period) ;
                                                      else
                                                        printf ("     ") ;
                                                    printf ("Amplitude=%5.3f   Ref. Amp.=%5.3f\n", 
                                                      (float) pcal2->calibration_amplitude, (float) pcal2->ref_amp) ;
                                                    depad (pcal2->calibration_input_channel, s, 3) ;
                                                    if (s[0] > ' ')
                                                      then
                                                        printf ("     Monitor Ch=%s  ", s) ;
                                                      else
                                                        printf ("     ") ;
                                                    depad (pcal2->coupling, s, 12) ;
                                                    depad (pcal2->rolloff, s2, 12) ;
                                                    printf ("Coupling=%s  Rolloff=%s\n", s, s2) ;
                                                  end
                                                else
                                                  printf ("\n") ;
                                            end
                                          case 2000 :
                                            begin /* Must be Blockette */
                                              popaque = (tophdr) ptim ;
                                              do
                                                begin
                                                  printf ("        Blockette Length=%d    Record Number %d\n", 
                                                              (short) popaque->blockette_lth, (long) popaque->record_num) ;
                                                  if (popaque->next_blockette)
                                                    then
                                                      popaque = (tophdr)((long) pseed + popaque->next_blockette) ;
                                                    else
                                                      popaque = NULL ;
                                                end
                                              while (popaque) ;
                                            end
                                      end 
                                  end
                             end
                          pdat = (pdata_user) ((long) pdat + this->dbufsize) ;
                        end
                    end
              end
            else
#ifdef _OSK
              tsleep (0x80000100) ;
#else
              sleep (1) ; /* Bother the server once every second */
#endif
        end
      while (1) ;
      return 0 ;
    end
