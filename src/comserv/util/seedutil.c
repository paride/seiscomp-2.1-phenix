/*   Quanterra to Seed format utility module.
     Copyright 1994-1996 Quanterra, Inc.
     Written by Woodrow H. Owens

Edit History:
   Ed Date      By  Changes
   -- --------- --- ---------------------------------------------------
    0 27 Mar 94 WHO Translated from seedutil.p
    1  6 Jun 94 WHO In seedheader, make sequence number out of packet_seq (DSN).
                    Since longinteger station name is right justified,
                    use routine to make left justified Seed station name.
    2  9 Jun 94 WHO Cleanup to avoid warnings.
    3 25 Sep 94 WHO Add conversion of RECORD_HEADER_3 to compression 19.
    4 12 Dec 94 WHO Add support for SEED version 2.3B.
    5 28 Feb 95 WHO Start of conversion to run on OS9.
    6 26 Jun 95 DSN Added external symbols to allow change to LOG and ACE
                    SEED channel names and locations.
    7 13 Jun 96 WHO Add copying of seedname and location in comment and
                    clock correction packet if new "SEEDIN" flag is set.
    8 11 Jun 97 WHO In seedblocks set seed sequence number based on
                    value in header_flag.
    9 17 Oct 97 WHO Add VER_SEEDUTIL.
   10 26 Nov 97 WHO Set deb_flags based on header_revision.
*/
#include <stdio.h>
#include <errno.h>
#include <string.h>
#ifndef _OSK
#include <termio.h>
#include <fcntl.h>
#include <sys/types.h>
#endif
#include "quanstrc.h"
#include "stuff.h"
#include "seedutil.h"
#include "pascal.h"

short VER_SEEDUTIL = 10 ;

/************************************************************************/
/* External symbols for SEED channel_id and location_id names for the */
/* channels derived from COMMENT and CLOCK_EXCEPTION packets.  */
/* These can be customized by comserv on a per-station basis by  */
/* changing the contents of these variables.    */
/************************************************************************/
char log_channel_id[4] = "LOG" ;
char log_location_id[3] = "  " ;
char clock_channel_id[4] = "ACE" ;
char clock_location_id[3] = "  " ;
boolean seedin = FALSE ;

  typedef data_only_blockette *tpdob ;
  
  void seedsequence (seed_record_header *seed, long seq) ;
  short julian_day (time_array *gt) ;
  short decode_rate (byte rate) ;
  long julian (time_array *gt) ;
  void gregorian (long jdate, time_array *ret) ;

  void seed_time (time_array *t, long usec, seed_time_struc *st)
    begin
      st->yr = ord(t->year) + 1900 ;
      st->jday = julian_day(t) ;
      st->hr = t->hour ;
      st->minute = t->min ;
      st->seconds = t->sec ;
      st->unused = 0 ;
      st->tenth_millisec = usec div 100 ;
    end

/* Converts bits from the quanterra SOH byte to SEED activitiy flags */
  byte translate_to_seed_activity_flags (byte soh)
    begin
      byte act ;

      act = 0 ;
      if (soh and SOH_BEGINNING_OF_EVENT)
        then
          act = act or SEED_ACTIVITY_FLAG_BEGIN_EVENT ;
      if (soh and SOH_CAL_IN_PROGRESS)
        then
          act = act or SEED_ACTIVITY_FLAG_CAL_IN_PROGRESS ;
      if (soh and SOH_EVENT_IN_PROGRESS)
        then
          act = act or SEED_ACTIVITY_FLAG_EVENT_IN_PROGRESS ;
      return act ;
    end

/* Converts bits from the quanterra SOH byte to SEED quality flags */
  byte translate_to_seed_quality_flags (byte soh)
    begin
      byte qual ;

      qual = 0 ;
      if (soh and SOH_GAP)
        then
          qual = qual or SEED_QUALITY_FLAG_MISSING_DATA ;
      if (((soh and SOH_INACCURATE) != 0) lor
         (((soh eor SOH_EXTRANEOUS_TIMEMARKS) and
           (SOH_EXTRANEOUS_TIMEMARKS or SOH_EXTERNAL_TIMEMARK_TAG)) == 0))
        then
          qual = qual or SEED_QUALITY_FLAG_QUESTIONABLE_TIMETAG ;
      return qual ;
    end

/* Get percentage of clock quality */
  short clk_perc (signed char b)
    begin
      short perc[7] = {0, 40, 10, 20, 60, 80, 100} ;
      
      return perc[b + 1] ;
    end

/* Setup common area of data only blockette */
  void set_dob (tpdob pdob)
    begin
      pdob->blockette_type = 1000 ;
      pdob->word_order = 1 ;
      pdob->rec_length = 9 ;
      pdob->dob_reserved = 0 ;
    end

/* Take a C string, move it into a fixed length array, and pad
   on the right with spaces to fill the array */
  void cpadright (pchar s, pchar b, short fld)
    begin
      short i, j ;
      
      j = strlen(s) ;
      if (j > fld)
        then
          j = fld ;
      for (i = 0 ; i < j ; i++)
        b[i] = s[i] ;
      for (i = j ; i < fld ; i++)
        b[i] = ' ' ;
    end

/* Take a Pascal string, move it into a fixed length array, and pad
   on the right with spaces to fill the array */
  void padright (pchar s, pchar b, short fld)
    begin
      short i, j ;
      
      j = s[0] ;
      if (j > fld)
        then
          j = fld ;
      for (i = 0 ; i < j ; i++)
        b[i] = s[i + 1] ;
      for (i = j ; i < fld ; i++)
        b[i] = ' ' ;
    end

/* Passed the address of a quanterra header, and the record size, will convert the
   header to SEED format.
*/
  double seedheader (header_type *h, seed_fixed_data_record_header *sh)
    begin
      short i ;
      pchar ps ;

      seedsequence (&sh->header, h->packet_seq mod 1000000) ;
      sh->header.seed_record_type = 'D' ;
      sh->header.continuation_record = ' ' ;
      /* station name converted from right justified 4 byte to left justified 5 byte */
      ps = long_str(h->station.l) ; /* get string version of station */
      memset(sh->header.station_ID_call_letters, ' ', 5) ; /* initialize to spaces */
      memcpy(sh->header.station_ID_call_letters, ps, strlen(ps)) ; /* copy valid part in */
      /* channel+id, location_id, and seednet are just copied directly */
      memcpy(sh->header.channel_id, h->seedname, 3) ;
      memcpy(sh->header.location_id, h->location, 2) ;
      memcpy(sh->header.seednet, h->seednet, 2) ;
      /* convert to seed time */
      seed_time (&h->time_of_sample, ((long) h->millisec) * 1000L + ((long) h->microsec),
                 &sh->header.starting_time) ;
      sh->header.samples_in_record = h->number_of_samples ;
      sh->header.sample_rate_factor = decode_rate(h->rate) ;
      sh->header.sample_rate_multiplier = 1 ;
      sh->header.activity_flags = translate_to_seed_activity_flags (h->soh) ;
      sh->header.IO_flags = 0 ;
      sh->header.data_quality_flags = translate_to_seed_quality_flags (h->soh) ;
      sh->header.number_of_following_blockettes = h->blockette_count + 2 ;
      sh->header.tenth_msec_correction = 0 ; /* this is a delta, which we don't do */
      sh->header.first_data_byte = (ord(h->blockette_count) + 1) * 64 ;
      sh->header.first_blockette_byte = 48 ;
      sh->deb.blockette_type = 1001 ;
      if (h->blockette_count != 0)
        then
          sh->deb.next_offset = 64 ;
        else
          sh->deb.next_offset = 0 ;
      i = clk_perc (h->clkq) ;
      if (h->soh and SOH_INACCURATE)
        then
          i = 15 + (short) h->clkq ;
        else
          begin
            if (h->soh and SOH_EXTRANEOUS_TIMEMARKS)
              then
                i = i - 30 ;
            if ((h ->soh and SOH_EXTERNAL_TIMEMARK_TAG) == 0)
              then
                i = i - 15 ;
          end
      if (i < 0)
        then
          sh->deb.qual = 0 ;
        else
          sh->deb.qual = (byte) i ;
      if (h->clkq >= 3)
        then
          sh->header.IO_flags = 0x20 ;
      sh->deb.usec99 = h->microsec mod 100 ;
      if (h->header_revision >= 5)
        then
          sh->deb.deb_flags = DEB_NEWTIME ;
        else
          sh->deb.deb_flags = 0 ;
      sh->deb.frame_count = h->frame_count ;
      set_dob (&sh->dob) ;
      sh->dob.next_offset = 56 ;
      if (h->frame_type == RECORD_HEADER_1)
        then
          sh->dob.encoding_format = 10 ;
      else if (h->frame_type == RECORD_HEADER_2)
        then
          sh->dob.encoding_format = 11 ;
        else
          sh->dob.encoding_format = 19 ;
      return julian(&h->time_of_sample) + h->millisec / 1000.0 + h->microsec / 1000000.0 ;
    end

/* Converts the sequence number into a string with leading zeroes */
  void seedsequence (seed_record_header *seed, long seq)
    begin
      char tmp[8] ;

      if ((seq < 0) or (seq > 999999))
        then
          seq = 0 ;
      sprintf(tmp, "%-7d", seq + 1000000) ; /* convert to string */
      strncpy(seed->sequence, &tmp[1], 6) ; /* move 2nd through 7th characters */
    end

/* given the seed name and location, returns a string in the format LL-SSS */
  pchar seednamestring (seed_name_type *sd, location_type *loc)
    begin
      static char s2[7] ;
      short i, j ;

      i = 0 ;
      if ((*loc)[0] != ' ')
        then
          begin
            s2[i++] = (*loc)[0] ;
            if ((*loc)[1] != ' ')
              then
                s2[i++] = (*loc)[1] ;
            s2[i++] = '-' ;
          end
      for (j = 0 ; j < 3 ; j++)
        s2[i++] = (*sd)[j] ;
      s2[i] = '\0' ;
      return (pchar) &s2 ;
    end

#define FIRSTDATA 56
#define FIRSTBLK 48

/* 
   sl is the buffer where the SEED header and blockette will go. log is the address
   of the commo_record that contains either a commo_event or commo_comment structure.
*/
  double seedblocks (seed_record_header *sl, commo_record *log)
    begin
      typedef void *pvoid ;
      typedef char string9[10] ;
      
      pchar pc1, pc2 ;
      time_array ta ;
      short i, j, lth ;
      short ms ;
      murdock_detect *det ;
      threshold_detect *dett ;
      timing *tim ;
      step_calibration *calstep ;
      sine_calibration *calsine ;
      random_calibration *calrand ;
      abort_calibration *calabort ;
      cal2 *calend ;
      time_quality_descriptor *ptqd ;
      squeezed_event_detection_report *psedr ;
      clock_exception *pce ;
      calibration_result *pcr ;
      boolean mh ;
      char mots[10] ;
      long dbadj ;
      complong stat ;
      seed_net_type net ;
      complong ltemp ;
      double headtime ;
      long usec ;
      pchar ps ;
      tpdob pdob ;
      char s[130] ;
      char s2[32] ;
      string9 filters[5] = {"None", "3dB@40Hz", "3dB@10Hz", "3dB@1Hz", "3dB@0.1Hz"} ;
      string15 mtypes[6] = {"Unknown", "Missing", "Expected",
                            "Valid", "Unexpected", "Daily"} ;
      string15 omeganames[8] = {"Norway", "Liberia", "Hawaii", "North Dakota",
                                "La Reunion", "Argentina", "Australia", "Japan"} ;
      string23 ctypes[11] = {"Unknown", "OS9 System", "Kinemetrics GOES",
        "Kinemetrics Omega", "Kinemetrics DCF", "Meinburg DCF",
        "Quanterra GPS1/QTS", "Quanterra GPS1/GOES", "Quanterra GPS1/UA31S",
        "Quanterra GPS2/QTS2", "Altus K2 GPS"} ;
      
      sl->seed_record_type = 'D' ;
      sl->continuation_record = ' ' ;
      sl->location_id[0] = ' ' ;
      sl->location_id[1] = ' ' ;
      sl->first_blockette_byte = FIRSTBLK ;
      usec = 0 ;
      pce = &log->ce.header_elog.clk_exc ;
      pdob = (tpdob) ((long) sl + FIRSTBLK) ;
      set_dob (pdob) ;
      pdob->encoding_format = 0 ;
      pdob->next_offset = 56 ; /* valid except for comments */
      sl->number_of_following_blockettes = 2 ; /* ditto */
      switch (pce->frame_type)
        begin
          case COMMENTS :
            begin
              if (seedin)
                then
                  memcpy (sl->location_id, log->cc.cc_location, 5) ;
                else
                  begin
                    sl->channel_id[0] = log_channel_id[0] ;
                    sl->channel_id[1] = log_channel_id[1] ;
                    sl->channel_id[2] = log_channel_id[2] ;
                    sl->location_id[0] = log_location_id[0] ;
                    sl->location_id[1] = log_location_id[1] ;
                  end
              pdob->next_offset = 0 ;
              sl->number_of_following_blockettes = 1 ;
              ta = log->cc.time_of_transmission ;
              memcpy(net, log->cc.cc_net, 2) ;
              memcpy((pvoid) &stat, (pvoid) &log->cc.cc_station, 4) ;
              sl->first_data_byte = FIRSTDATA ;
              pc1 = (pchar) ((long) sl + FIRSTDATA) ;
              pc2 = log->cc.ct ;
              lth = ord(*pc2++) + 2 ; /* get dynamic length plus room for CR/LF */
              for (i = 3 ; i <= lth ; i++)
                *pc1++ = *pc2++ ;
              *pc1++ = '\xd' ;
              *pc1++ = '\xa' ;
              sl->samples_in_record = lth ;
              headtime = julian(&ta) ;
              break ;
            end
          case CLOCK_CORRECTION :
            begin
              if (seedin)
                then
                  memcpy (sl->location_id, pce->cl_location, 5) ;
                else
                  begin
                    sl->channel_id[0] = clock_channel_id[0] ;
                    sl->channel_id[1] = clock_channel_id[1] ;
                    sl->channel_id[2] = clock_channel_id[2] ;
                    sl->location_id[0] = clock_location_id[0] ;
                    sl->location_id[1] = clock_location_id[1] ;
                  end
              pce = &log->ce.header_elog.clk_exc ;
              ta = pce->time_of_mark ;
              memcpy(net, pce->cl_net, 2) ;
              memcpy((pvoid) &stat, (pvoid) &pce->cl_station, 4) ;
              tim = (timing *) ((long) sl + FIRSTDATA) ;
              tim->blockette_type = 500 ;
              tim->exception_count = pce->count_of ;
              tim->vco_correction = pce->vco / 40.96 ;
              ptqd = (time_quality_descriptor *) &pce->correction_quality ;
              tim->reception_quality = (byte) clk_perc (ptqd->reception_quality_indicator) ;
              if (pce->clock_exception <= DAILY_TIME_CORRECTION_REPORT)
                then
                  strcpy (s, mtypes[pce->clock_exception]) ;
                else
                  strcpy (s, "????") ;
              cpadright (s, tim->exception_type, 16) ;
              if (pce->cl_spec.model <= 10)
                then
                  strcpy (s, ctypes[pce->cl_spec.model]) ;
                else
                  begin
                    i = (short) pce->cl_spec.model ;
                    sprintf (s2, "%d", i) ;
                    strcpy (s, "Model ") ;
                    strcat (s, s2) ;
                  end
              cpadright (s, tim->clock_model, 32) ;
              strcpy (s, "Drift=") ;
              sprintf (s2, "%d", (long) pce->clock_drift) ;
              strcat (s, s2) ;
              strcat (s, "usec") ;
              switch (pce->cl_spec.model)
                begin
                  case 3 :
                    begin
                      strcat (s, ", Station=") ;
                      if ((pce->cl_spec.cl_status[0] >= 'A') land
                          (pce->cl_spec.cl_status[0] <= 'H'))
                        then
                          strcat (s, omeganames[pce->cl_spec.cl_status[0] - (short) 'A']) ;
                      else if ((pce->cl_spec.cl_status[0] >= 'I') land
                               (pce->cl_spec.cl_status[0] <= 'Z'))
                        then
                          strncat (s, (pchar) &pce->cl_spec.cl_status[0], 1) ;
                        else
                          begin
                            i = (short) pce->cl_spec.cl_status[0] ;
                            sprintf (s2, "%d", i) ;
                            strcat (s, s2) ;
                          end
                      break ;
                    end
                  case 6 : ;
                  case 9 :
                    begin
                      strcat (s, ", Satellite SNR in dB=") ;
                      for (i = 0 ; i <= 5 ; i++)
                        begin
                          if (i)
                            then
                              strcat (s, ", ") ;
                          j = (short) pce->cl_spec.cl_status[i] ;
                          sprintf (s2, "%d", j) ;
                          strcat (s, s2) ;
                        end
                      break ;
                    end
                  default : strcpy (s, "") ;
                end
              cpadright (s, tim->clock_status, 128) ;
              usec = ((long) ptqd->msec_correction) * 1000 + ((long) pce->usec_correction) ;
              seed_time (&pce->time_of_mark, usec, &tim->time_of_exception) ;
              tim->usec99 = pce->usec_correction mod 100 ;
              headtime = julian(&ta) + ptqd->msec_correction / 1000.0 + pce->usec_correction / 1000000.0 ;
              break ;
            end
          case DETECTION_RESULT :
            begin
              psedr = &log->ce.header_elog.det_res.pick ;
              memcpy (sl->channel_id, psedr->seedname, 3) ;
              memcpy (sl->location_id, psedr->location, 2) ;
              memcpy (net, psedr->ev_network, 2) ;
              memcpy ((pvoid) &stat, (pvoid) &psedr->ev_station, 4) ;
              mh = (log->ce.header_elog.det_res.detection_type == MURDOCK_HUTT) ;
              det = (murdock_detect *) ((long) sl + FIRSTDATA) ;
              if (mh)
                then
                  det->blockette_type = 201 ;
                else
                  det->blockette_type = 200 ;
              det->signal_amplitude = (float) psedr->peak_amplitude ;
              if (mh)
                then
                  det->signal_period = ((float) psedr->period_x_100) * 0.01 ;
              det->background_estimate = (float) psedr->background_amplitude ;
              sprintf (mots, "%8X", (long) psedr->motion_pick_lookback_quality) ;
              if (mh)
                then
                  det->event_detection_flags = mots[0] - 'C' ;
                else
                  det->event_detection_flags = 4 ;
              gregorian(psedr->jdate, &ta) ;
              usec = ((long) psedr->millisec) * 1000 ;
              seed_time(&ta, usec, &det->signal_onset_time) ;
              if (mh)
                then
                  begin
                    for (i = 0 ; i <= 4 ; i++)
                      det->snr[i] = mots[i + 3] - '0' ;
                    det->lookback_value = mots[2] - '0' ;
                    det->pick_algorithm = mots[1] - 'A' ;
                    padright (psedr->detname, det->s_detname, 24) ;
                  end
                else
                  begin
                    dett = (threshold_detect *) det ;
                    padright (psedr->detname, dett->s_detname, 24) ;
                  end
              headtime = (double) psedr->jdate + SECCOR + psedr->millisec / 1000.0 ;
              break;
            end
          case CALIBRATION :
            begin
              pcr = &log->ce.header_elog.cal_res ;
              memcpy(sl->channel_id, pcr->cr_seedname, 3) ;
              memcpy(sl->location_id, pcr->cr_location, 2) ;
              ta = pcr->cr_time ;
              memcpy(net, pcr->cr_network, 2) ;
              memcpy((pvoid) &stat, (pvoid) &pcr->cr_station, 4) ;
              calstep = (step_calibration *) ((long) sl + FIRSTDATA) ;
              calsine = (sine_calibration *) calstep ;
              calrand = (random_calibration *) calstep ;
              calabort = (abort_calibration *) calstep ;
              seed_time (&pcr->cr_time, 0, &calstep->fixed.calibration_time) ;
              calstep->calibration_flags = pcr->cr_flags ;
              ltemp.s[0] = pcr->cr_duration ;
              ltemp.s[1] = pcr->cr_duration_low ;
              calstep->calibration_duration = ltemp.l * 10000 ;
              switch (pcr->cr_type)
                begin
                  case STEP_CAL :
                    begin
                      calstep->fixed.blockette_type = 300 ;
                      calstep->number_of_steps = pcr->cr_stepnum ;
                      calstep->interval_duration = 0 ;
                      calend = &calstep->step2 ;
                      break ;
                    end
                  case SINE_CAL :
                    begin
                      calsine->fixed.blockette_type = 310 ;
                      calsine->res = 0 ;
                      ltemp.s[0] = pcr->cr_period ;
                      ltemp.s[1] = pcr->cr_period_low ;
                      calsine->sine_period = ltemp.l / 1000.0 ;
                      calend = &calsine->sine2 ;
                      break ;
                    end
                  case RANDOM_CAL :
                    begin
                      calrand->fixed.blockette_type = 320 ;
                      calrand->res = 0 ;
                      calend = &calrand->random2 ;
                      if (pcr->cr_flags2 and 2)
                        then
                          cpadright ("White", calrand->noise_type, 8) ;
                        else
                          cpadright ("Red", calrand->noise_type, 8) ;
                      break ;
                    end ;
                  case ABORT_CAL :
                    begin
                      calabort->fixed.blockette_type = 395 ;
                      calabort->res = 0 ;
                      calend = NULL ;
                      break ;
                    end
                end
              if (calend)
                then
                  begin
                    ltemp.s[0] = pcr->cr_0dB ;
                    ltemp.s[1] = pcr->cr_0dB_low ;
                    ltemp.f = (float) ltemp.l ;
                    if (ltemp.f == 0.0)
                      then
                        calend->calibration_amplitude = (float) pcr->cr_amplitude ;
                      else
                        begin
                          dbadj = 1 ;
                          i = pcr->cr_amplitude ;
                          while (i < 0)
                            begin
                              dbadj = dbadj * 2 ;
                              i = i + 6 ;
                            end
                          calend->calibration_amplitude = ltemp.f / dbadj ;
                        end
                    memcpy (calend->calibration_input_channel, pcr->cr_input_seedname, 3) ;
                    calend->cal2_res = 0 ;
                    calend->ref_amp = ltemp.f ;
                    if (pcr->cr_flags2 and 1)
                      then
                        cpadright ("Capacitive", calend->coupling, 12) ;
                      else
                        cpadright ("Resistive", calend->coupling, 12) ;
                    if (pcr->cr_filt <= 4)
                      then
                        strcpy (s, filters[pcr->cr_filt]) ;
                      else
                        strcpy (s, "Unknown") ;
                    cpadright (s, calend->rolloff, 12) ;
                  end
              headtime = julian (&ta) ;
              break ;
            end
          case END_OF_DETECTION :
            begin
              sl->number_of_following_blockettes = 1 ;
              pdob->next_offset = 0 ;
              psedr = &log->ce.header_elog.det_res.pick ;
              memcpy(sl->channel_id, psedr->seedname, 3) ;
              memcpy(sl->location_id, psedr->location, 2) ;
              memcpy(net, psedr->ev_network, 2) ;
              memcpy((pvoid) &stat, (pvoid) &psedr->ev_station, 4) ;
              ta = log->ce.header_elog.det_res.time_of_report ;
              sl->activity_flags = SEED_ACTIVITY_FLAG_END_EVENT ;
              headtime = julian(&ta) ;
              break ;
            end
        end /* switch */
      seedsequence (sl, log->ce.header_flag) ;
      /* station name converted from right justified 4 byte to left justified 5 byte */
      ps = long_str(stat.l) ; /* get string version of station */
      cpadright (ps, sl->station_ID_call_letters, 5) ;
      memcpy (sl->seednet, net, 2) ;
      seed_time (&ta, usec, &sl->starting_time) ;
      return headtime ;
    end
