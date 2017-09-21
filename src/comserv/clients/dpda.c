/*   Send and receive commands from dp to da.
     Copyright 1994-1998 Quanterra, Inc.
     Written by Woodrow H. Owens

Edit History:
   Ed Date      By  Changes
   -- --------- --- ---------------------------------------------------
    0  8 Apr 94 WHO First created from test.c
    1 12 Apr 94 WHO The saga continues.
    2 16 Apr 94 WHO In "getcal", set the "channels" structure to the
                    calibrator specified.
    3 30 May 94 WHO Update Ultra Info and Comm Event commands.
    4  6 Jun 94 WHO Add string display of CSCR_PRIVILEGE error.
    5 10 Jun 94 WHO Cleanup to avoid warnings.
    6  9 Aug 94 WHO Change Download File & Upload File to avoid
                    confusing Doug. Add display of last_good and last_bad
                    in linkstat command.
    7 11 Aug 94 WHO Check for NULL result from gets on EOF.
    8 23 Dec 94 WHO Fix changing comm/event comlink priority checks.
    9 16 Jun 95 WHO Changes to link settings to conform to new field
                    definitions.
   10 20 Jun 95 WHO Show new fields in linkstat and add support for linkset.
   11 29 May 96 WHO Start of conversion to run on OS9.
   12 15 Jul 96 WHO If first character in comm event name list is null, there
                    is no list. Show station connected to on banner.
   13  4 Aug 96 WHO In a few places, treat a CR only entry to a prompt as an
                    abort command.
   14 28 Sep 96 WHO In getcal, return zero when the user selects calibrator
                    number 0, instead of using board 1. When entering channel
                    list for calibrate, allow comma separator.
   15 16 Jun 97 WHO Display and accept "DEF" for default comlink priority.
   16 27 Jul 97 WHO Add command to turn flooding on and off.
   17               Unwise changes, reversed.
   18 16 Oct 97 WHO Don't show stream for channel dump.
   19  9 Nov 98 WHO Add CSDP_ABT to indicate command aborted by operator.
                    Abort detector commands if user enters CR. All locations
                    with calibrator channels.
   20 12 Nov 98 WHO Change get_det so that it validates the entered channel
                    before requesting detector info for it.
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
#include <sys/stat.h>
#include <sys/time.h>
#endif
#include <signal.h>
#include "dpstruc.h"
#include "seedstrc.h"
#include "stuff.h"
#include "timeutil.h"
#include "seedutil.h"
#include "service.h"
#include "cfgutil.h"
#include "pascal.h"
#ifdef _OSK
#include "os9stuff.h"
#endif

#ifdef __cplusplus
#define this _this
#endif

#define WAIT_SECONDS 30
#define CSDP_ABT 100
char name[5] = "CMDS" ;
char sname[5] = "RAND" ;
tstations_struc stations ;
chan_struc *pcs = NULL ;
pclient_struc me ;
pclient_station this ;
comstat_rec *pcomm ;
pselarray psa ;
char s1[200], s2[200], s3[200], chanstring[80] ;

#ifdef _OSK
int sleep (unsigned seconds) ; /* should be available in sys_clib */
#endif

typedef char char15[16] ;
typedef char char7[8] ;
char15 waves[4] = { "Sine", "Step", "Red Noise", "White Noise" } ;

char7 freqs[Hz0_0005+1] = { "DC", "25.000", "10.000", "8.0000", "6.6667", "5.0000", "4.0000", "3.3333",
               "2.5000", "2.0000", "1.6667", "1.2500", "1.0000", "0.8000", "0.6667", "0.5000", "0.2000",
               "0.1000", "0.0667", "0.0500", "0.0400", "0.0333", "0.0250", "0.0200", "0.0167", "0.0125",
               "0.0100", "0.0050", "0.0020", "0.0010", "0.0005" } ;

char15 outpath[6] = { "Cont-Comlink", "Event-Comlink", "Cont-Tape", "Event-Tape", "Cont-Disk", "Event-Disk" } ;

char7 formats[2] = { "QSL", "Q512" } ;

link_record curlink = { 0, 0, 0, 0, 0, 0, CSF_QSL, 0, 0, 0, 0, 0, 0, 0, 0 } ;
linkstat_rec linkstat = { TRUE, FALSE, FALSE, FALSE, 0, 0, 0, 0, 0, 0, 0, 0, CSF_QSL } ;

char7 channels[4] ;
short channel_count ;

/* Convert comlink priority to string, either numeric or "DEF" */
  pchar cl_prio (byte clpr)
    begin
      static char s[8] ;
      short m ;
      
      m = clpr ;
      if (clpr == 120)
        then
          strcpy (s, "DEF") ;
        else
          sprintf(s, "%d", m) ;
      return (pchar) &s ;
    end
 
/* Show a bitmap a series of bit number separated by spaces */
  pchar showmap (long map)
    begin
      short m ;
      static char s[80] ;
      char s2[80] ;
      
      s[0] = '\0' ;
      for (m = 0 ; m < 32 ; m++)
        if (test_bit(map, m))
          then
            begin
              sprintf(s2, "%d ", m) ;
              strcat(s, s2) ;
            end
      return (pchar) &s ;
    end

/* Show waveform bitmap as a series of names separated by spaces */
  pchar showwaves (long map)
    begin
      short m ;
      static char s[80] ;
      
      s[0] = '\0' ;
      for (m = SINE ; m <= WRAND ; m++)
        if (test_bit(map, m))
          then
            begin
              strcat(s, waves[m]) ;
              strcat(s, "   ") ;
            end
      return (pchar) &s ;
    end

/* Shows a frequency bitmap as a series of frequencies separated by spaces */ 
  void showfreqs (long map)
    begin
      short k ;
      char s1[80] ;

      s1[0] = '\0' ;
      for (k = Hz25_000 ; k <= Hz0_0005 ; k++)
      if (test_bit(map, k))
        then
          begin
            strcat (s1, freqs[k]) ;
            strcat (s1, " ") ;
            if (strlen(s1) > 65)
              then
                begin
                  printf ("      %s\n", s1) ;
                  s1[0] = '\0' ;
                end
          end
      if (s1[0] != '\0')
        then
          printf ("      %s\n", s1) ;
    end

/* Find the lowest bit on in a bitmap */
  short lowest (long cm)
    begin
      short i ;
      
      for (i = 0 ; i <= 31 ; i++)
        if (test_bit(cm, i))
          then
            return i + 1 ;
      return 0 ;
    end

/* Find the highest bit on in a bitmap */
  short highest (long cm)
    begin
      short i ;
      
      for (i = 31 ; i >= 0 ; i--)
        if (test_bit(cm, i))
          then
            return i + 1 ;
      return 0 ;
    end

/* Show an output bitmap as series of paths separated by spaces */ 
  pchar showout (byte map)
    begin
      short k ;
      static char s1[80] ;
      
      s1[0] = '\0' ;
      for (k = 0 ; k <= 5 ; k++)
        if (test_bit(map, k))
          then
            begin
              strcat(s1, outpath[k]) ;
              strcat(s1, " ") ;
            end
      return (pchar) &s1 ;
    end

/* Write text equivalent of command status to screen */
  void showerr (short err)
    begin
      switch (err)
        begin
          case CSCR_ENQUEUE :
            begin
              printf ("No Empty Service Queues\n") ;
              break ;
            end
          case CSCR_TIMEOUT :
            begin
              printf ("Command not processed by server\n") ;
              break ;
            end
          case CSCR_INIT :
            begin
              printf ("Server in initialization\n") ;
              break ;
            end
          case CSCR_REFUSE :
            begin
              printf ("Could not attach to server\n") ;
              break ;
            end
          case CSCR_NODATA :
            begin
              printf ("The requested data is not available\n") ;
              break ;
            end
          case CSCR_BUSY :
            begin
              printf ("Command buffer in use\n") ;
              break ;
            end
          case CSCR_INVALID :
            begin
              printf ("Command is not known by server or is invalid for this DA\n") ;
              break ;
            end
          case CSCR_DIED :
            begin
              printf ("Server is dead\n") ;
              break ;
            end
          case CSCR_CHANGE :
            begin
              printf ("Server has restarted since last service request\n") ;
              break ;
            end
          case CSCR_PRIVATE :
            begin
              printf ("Could not create my shared memory segment\n") ;
              break ;
            end
          case CSCR_SIZE :
            begin
              printf ("Command buffer is too small\n") ;
              break ;
            end
          case CSCR_PRIVILEGE :
            begin
              printf ("That command is privileged\n") ;
              break ;
            end
          case CSCS_IDLE :
            begin
              printf ("Command buffer is Idle\n") ;
              break ;
            end
          case CSCS_INPROGRESS :
            begin
              printf ("Waiting for response from DA\n") ;
              break ;
            end
          case CSCS_FINISHED :
            begin
              printf ("Command response available\n") ;
              break ;
            end
          case CSCS_REJECTED :
            begin
              printf ("Command rejected by DA\n") ;
              break ;
            end
          case CSCS_ABORTED :
            begin
              printf ("File transfer aborted\n") ;
              break ;
            end
          case CSCS_NOTFOUND :
            begin
              printf ("File not found\n") ;
              break ;
            end
          case CSCS_TOOBIG :
            begin
              printf ("File is larger than 65K bytes\n") ;
              break ;
            end
          case CSCS_CANT :
            begin
              printf ("Cannot create file on DA\n") ;
              break ;
            end
          case CSDP_ABT :
            begin
              printf ("Command aborted by Operator\n") ;
              break ;
            end
        end
    end

/* 
   If a command is in progress, wait up to 30 seconds for it to finish, else
   return actual status.
*/
  short wait_finished (comstat_rec *pcomm) 
    begin
#ifdef _OSK
      double finish ;
      
      finish = dtime () + 30.0 ;
      do
        begin
          if (pcomm->completion_status == CSCS_FINISHED)
            then
              return CSCS_FINISHED ;
          else if (pcomm->completion_status == CSCS_INPROGRESS)
            then
              tsleep (0x80000100) ;
            else
              break ;
        end
      while (dtime() < finish) ;
#else
      short ct ;

      for (ct = 0 ; ct < WAIT_SECONDS ; ct++)
        if (pcomm->completion_status == CSCS_FINISHED)
          then
            return CSCS_FINISHED ;
        else if (pcomm->completion_status == CSCS_INPROGRESS)
          then
            sleep (1) ;
          else
            break ;
#endif
      return pcomm->completion_status ;
    end

/* Write the prompt and current value, if user enters CR, return current value, else new value */
  long replace (pchar s, long v)
    begin
      long l ;
      char s2[200] ;
      
      printf ("%s (CR = %d) : ", s, v) ;
      gets (s2) ;
      if (s2[0] != '\0')
        then
          sscanf (s2, "%d", &l) ;
        else
          l = v ;
      return l ;
    end

/* Write the prompt and explanation of CR. CR returns -1, else returns new value */
  short canchange (pchar s)
    begin
      long l ;
      char s2[200] ;
      
      printf ("%s, (CR = No Change) : ", s) ;
      gets (s2) ;
      if (s2[0] != '\0')
        then
          sscanf (s2, "%d", &l) ;
        else
          l = -1 ;
      return (short) l ;
    end
            
/* Gets the channel mapping from the server and stores it in dynamically allocated memory (PCS) */     
  short loadchan (void)
    begin
      short err ;
      short *pshort ;
      long size ;

      this->sels[CHAN].first = 0 ;
      this->sels[CHAN].last = 0 ;
      this->command = CSCM_CHAN ;
      err = cs_svc(me, 0) ;
      if (err == CSCR_GOOD)
        then
          begin
            pshort = (short *) &pcomm->moreinfo ;
            if (pcs)
              then
                free (pcs) ;
            size = *pshort * sizeof(chan_record) + sizeof(short) ;
            pcs = (chan_struc *) malloc(size) ;
            memcpy ((pchar) pcs, (pchar) &pcomm->moreinfo, size) ;
            pcomm->completion_status = CSCS_IDLE ;
          end
      return err ;
    end

/* Show what calibrators are available, and then prompt for the calibrator to use */
  short getcal (void)
    begin
      int sel ;
      short low, high, i, j, k, err ;
      boolean found ;
      cal_record *pcal ;
      eachcal *pe ;
    
      channel_count = 0 ;
      if (pcs == NULL)
        then
          err = loadchan () ;
        else
          err = CSCR_GOOD ;
      if (err == CSCR_GOOD)
        then
          begin
            this->command = CSCM_CAL ;
            err = cs_svc(me, 0) ;
            if (err == CSCR_GOOD)
              then
                begin
                  pcal = (cal_record *) &pcomm->moreinfo ;
                  for (i = 0 ; i < pcal->number ; i++)
                    begin
                      pe = &pcal->acal[i] ;
                      if (pe->max_mass_dur > 0)
                        then
                          begin
                            low = lowest(pe->map) ;
                            high = highest(pe->map) ;
                            s1[0] = '\0' ;
                            for (j = low ; j <= high ; j++)
                              then
                                begin
                                  k = 0 ;
                                  found = FALSE ;
                                  while ((lnot found) land (k < pcs->chancount))
                                    begin
                                      if (pcs->chans[k].physical == j)
                                        then
                                          begin
                                            if (s1[0] != '\0')
                                              then
                                                 strcat (s1, ",") ;
                                            strcat(s1, seednamestring(&pcs->chans[k].seedname,
                                                                      &pcs->chans[k].seedloc)) ;
                                            found = TRUE ;
                                          end
                                        else
                                          k++ ;
                                    end
                                end
                            strpcopy (s2, pe->name) ;
                            printf ("Calibrator #%d = %s, Channels %s\n", i + 1, &s2, &s1) ;
                          end
                    end
                  pcomm->completion_status = CSCS_IDLE ;
                  printf ("Calibrator number [0 to exit] : ") ;
                  gets(s1) ;
                  if (s1[0] == '\0')
                    then
                      return 0 ;
                  sscanf (s1, "%i", &sel) ;
                  if ((sel < 1) lor (sel > pcal->number))
                    then
                      return 0 ; /* exit */
                  pe = &pcal->acal[sel - 1] ;
                  if (pe->max_mass_dur > 0)
                    then
                      begin
                        low = lowest(pe->map) ;
                        high = highest(pe->map) ;
                        chanstring[0] = '\0' ;
                        for (j = low ; j <= high ; j++)
                          then
                            begin
                              k = 0 ;
                              found = FALSE ;
                              while ((lnot found) land (k < pcs->chancount))
                                begin
                                  if (pcs->chans[k].physical == j)
                                    then
                                      begin
                                        strcpy(s2, seednamestring(&pcs->chans[k].seedname,
                                                                   &pcs->chans[k].seedloc)) ;
                                        if (chanstring[0] != '\0')
                                          then
                                            strcat (chanstring, ",") ;
                                        if (channel_count < 4)
                                          then
                                            strcpy (channels[channel_count++], s2) ;
                                       strcat (chanstring, s2) ;
                                       found = TRUE ;
                                      end
                                    else
                                      k++ ;
                                end
                            end
                      end
                  return sel ;
                end
              else
                begin
                  showerr (err) ;
                  return 0 ;
                end
          end
        else
          begin
            showerr (err) ;
            return 0 ;
          end
    end

/* Take a string in the form [LL-]SSS and return a string suitable as a selector */
  pchar seedloc (pchar s)
    begin
      static char s1[20] ;
      char s2[20] ;

      strcpy (s1, s) ; /* pass by value of strings not supported by C */
      untrail (s1) ;
      upshift (s1) ;
      split (s1, s2, '-') ;
      if (s2[0] == '\0')
        then
          begin
            strcpy (s2, s1) ; /* no location, move to channel, set location to any */
            strcpy (s1, "  ") ;
          end
      while (strlen(s1) < 2)
        strcat(s1, " ") ; /* make sure location is 2 characters */
      s1[2] = '\0' ; /* and no more */
      while (strlen(s2) < 3)
        strcat(s2, " ") ; /* make sure channel is 3 characters */
      s2[3] = '\0' ; /* and no more */
      strcat(s1, s2) ; /* merge location and channel */
      return (pchar) &s1 ;
    end
  
  short get_det (void)
    begin
      short err, i ;
      boolean banner ;
      boolean found ;
      char s1[100] ;
      chan_record *pcr ;
      
      if (pcs == NULL)
        then
          err = loadchan () ;
        else
      err = CSCR_GOOD ;
      banner = TRUE ;
      if (err == CSCR_GOOD)
        then
          begin
            s1[0] = '\0' ;
            for (i = 0 ; i < pcs->chancount ; i++)
              begin
                pcr = &pcs->chans[i] ;
                if (pcr->det_count)
                  then
                    begin
                      if (banner)
                        then
                          begin
                            printf ("Channels with detectors are :\n") ;
                            banner = FALSE ;
                          end
                      strcat (s1, seednamestring(&pcr->seedname, &pcr->seedloc)) ;
                      strcat (s1, " ") ;
                      if (strlen(s1) > 70)
                        then
                          begin
                            printf("%s\n", s1) ;
                            s1[0] = '\0' ;
                          end
                    end
              end
            if (s1[0] != '\0')
              then
                printf ("%s\n", s1) ;
            if (banner)
              then
                return CSCR_NODATA ;
            do
              begin
                pcomm->completion_status = CSCS_IDLE ;
                printf ("Seed Specification [LL-]SSS (CR to abort) : ") ;
                gets (s1) ;
                if (s1[0] == '\0')
                  then
                    return CSDP_ABT ;
                found = FALSE ;
                for (i = 0 ; i < pcs->chancount ; i++)
                  begin
                    pcr = &pcs->chans[i] ;
                    if ((pcr->det_count) land
#ifdef _OSK
                       (strcasecmp((pchar)s1, seednamestring(&pcr->seedname, &pcr->seedloc)) == 0))
#else
                       (strcasecmp((pchar)&s1, seednamestring(&pcr->seedname, &pcr->seedloc)) == 0))
#endif
                      then
                        begin
                          found = TRUE ;
                          break ;
                        end
                  end
              end
            while (lnot found) ;  
            strcpy ((*psa)[1], seedloc (s1)) ; /* make it selector 1 */
            this->sels[CHAN].first = 1 ;
            this->sels[CHAN].last = 1 ;
            this->command = CSCM_DET_REQUEST ;
            err = cs_svc(me, 0) ;
            if (err == CSCR_GOOD)
              then
                begin
                  err = wait_finished (pcomm) ;
                  if (err == CSCS_FINISHED)
                    then
                      begin
                        pcomm->completion_status = CSCS_IDLE ; /* not really done, but close enough */
                        return CSCR_GOOD ;
                      end
                    else
                      return err ;
                end
              else
                return err ;
          end
        else
          return err ;
    end

 
  int main (int argc, char *argv[], char **envp)
    begin
      short i, j, k, m, err, low, high ;
      int sel, sel2, upshmid ;
      boolean alert, found, stopcal, cp_valid, ep_valid, good, enable ;
      pdata_user pdat ;
      pchar pc1, pc2 ;
      linkstat_rec *pls ;
      link_record *plink ;
      digi_record *pdigi ;
      ultra_rec *pur ;
      cal_record *pcal ;
      eachcal *pe ;
      det_request_rec *pdrr ;
      det_descr *pdd ;
      long *plong, *plong2 ;
      short *pshort, *pshort2 ;
      chan_record *pcr, *pcr2 ;
      shell_com *pshell ;
      linkadj_com *padj ;
      recenter_com *prc ;
      cal_start_com *pcsc ;
      det_enable_com *pdec ;
      tdurations *pd ;
      det_change_com *pdcc ;
      shortdetload *psdl ;
      client_info *pci ;
      one_client *poc ;
      rec_enable_com *prec ;
      rec_one *pro ;
      download_com *pdc ;
      download_result *pdr ;
      upload_com *puc ;
      tupbuf *pupbuf ;
      upload_result *pupres ;
      comm_event_com *pcec ;
      linkset_com *plsc ;
      boolean *pflood ;
      char7 s[4] ;
      byte widx[4] ;
      byte fidx[32] ;
      byte frq ;
      long ltemp, mtemp ;
      unsigned short cnt, upsize ;
      long l ;
      FILE *path ;
#ifndef _OSK
      struct stat statbuf ;
#endif

/* Allow override of station name on command line */
      if (argc >= 2)
        then
          begin
            strncpy(sname, argv[1], 4) ;
            sname[4] = '\0' ;
          end
      upshift(sname) ;

/* Generate an entry for the station */      
      cs_setup (&stations, name, sname, TRUE, TRUE, 0, 2, CSIM_MSG, 5000) ;

      if (stations.station_count == 0)
        then
          begin
            printf ("Station not found\n") ;
            exit (3) ;
          end
 
/* Create my segment and attach to the selected station */      
      me = cs_gen (&stations) ;
      this = (pclient_station) ((long) me + me->offsets[0]) ;
      this->command = CSCM_LINKSTAT ;
      pcomm = (comstat_rec *) ((long) me + this->comoutoffset) ;
      pls = (linkstat_rec *) &pcomm->moreinfo ;
      pcomm->completion_status = CSCS_IDLE ;
      psa = (pselarray) ((long) me + this->seloffset) ;
      if (cs_svc(me, 0) == CSCR_GOOD)
        then
          memcpy ((pchar) &linkstat, (pchar) &pcomm->moreinfo, sizeof(linkstat_rec)) ;
      pcomm->completion_status = CSCS_IDLE ;
      this->command = CSCM_LINK ;
      err = cs_svc(me, 0) ;
      if (err == CSCR_GOOD)
        then
          memcpy ((pchar) &curlink, (pchar) &pcomm->moreinfo, sizeof(link_record)) ;
      pcomm->completion_status = CSCS_IDLE ;
      
      do
        begin
          printf ("Edition 20 Station %4.4s--------------------------------------------------\n", sname) ;
          printf (" 0=Exit                   1=Link Status            2=Link Format\n") ;
          printf (" 3=Digitizer Info         4=Ultra Info             5=Calibrator Info\n") ;
          printf (" 6=Channel Info           7=Detector Info          8=Client Info\n") ;
          printf (" 9=Unblock packets       10=Reconfigure Link      11=Suspend Link\n") ;
          printf ("12=Resume Link           13=Command Clear         14=Command Status\n") ;
          printf ("15=Terminate Server      16=Shell Command         17=Set VCO Value\n") ;
          printf ("18=Change Link Settings  19=Mass Recenter         20=Calibrate\n") ;
          printf ("21=Detector Enable       22=Detector Change       23=Recording Enables\n") ;
          printf ("24=Comm Event            25=Xfer File to Host     26=Xfer File to DA\n") ;
          printf ("27=Server Link Settings  28=Flooding Control\n") ;
          printf ("Command : ") ;
          if (gets (s1) == NULL)
            then
              break ;
          sscanf (s1, "%i", &sel) ;
          if (sel == 0)
            then
              break ;
          printf ("-------------------------------------------------------------------------\n") ;
          switch (sel)
            begin
              case 1 :
                begin
                  this->command = CSCM_LINKSTAT ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        memcpy ((pchar) &linkstat, (pchar) &pcomm->moreinfo, sizeof(linkstat_rec)) ;
                        pls = (linkstat_rec *) &pcomm->moreinfo ;
                        printf ("  Ultra=%d, Link Recv=%d, Ultra Recv=%d, Suspended=%d, Data Format=%s\n",
                          pls->ultraon, pls->linkrecv, pls->ultrarecv, pls->suspended,
                          formats[pls->data_format]) ;
                        printf ("  Total Packets=%d, Sync Packets=%d, Sequence Errors=%d\n",
                          pls->total_packets, pls->sync_packets, pls->seq_errors) ;
                        printf ("  Checksum Errors=%d, IO Errors=%d, Last IO Error=%d\n",
                          pls->check_errors, pls->io_errors, pls->lastio_error) ;
                        printf ("  Blocked Packets=%d, Seed Format=%5.5s, Seconds in Operation=%d\n",
                          pls->blocked_packets, pls->seedformat, pls->seconds_inop) ;
                        if (pls->last_good > 1)
                          then
                            printf ("  Last Good Packet Received at=%s\n", time_string(pls->last_good)) ;
                        if (pls->last_bad > 1)
                          then
                            printf ("  Last Bad Packet Received at=%s\n", time_string(pls->last_bad)) ;
                        strpcopy (s1, pls->description) ;
                        printf ("  Station Description=%s\n", &s1) ;
                        printf ("  Polling=%dus, Reconfig=%d, Net Timeout=%d, Net Polling=%d\n",
                          pls->pollusecs, pls->reconcnt, pls->net_idle_to, pls->net_conn_dly) ;
                        printf ("  Group Size=%d, Group Timeout=%d\n", pls->grpsize, pls->grptime) ;
                        pcomm->completion_status = CSCS_IDLE ;
                      end
                    else
                      showerr (err) ;
                    break ;
                  end
              case 2 :
                begin
                  this->command = CSCM_LINK ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        memcpy ((pchar) &curlink, (pchar) &pcomm->moreinfo, sizeof(link_record)) ;
                        plink = (link_record *) &pcomm->moreinfo ;
                        printf ("  Window Size=%d, Total priority levels=%d, Message Priority=%d\n",
                                plink->window_size, plink->total_prio - 1, plink->msg_prio) ;
                        printf ("  Detection Priority=%d, Timing Priority=%d, Cal Priority=%d, CmdEcho=%d\n",
                                plink->det_prio, plink->time_prio, plink->cal_prio, plink->rcecho) ;
                        printf ("  Resend Time=%d  Sync Time=%d  Resend Pkts=%d  Net Restart Dly=%d\n",
                                plink->resendtime, plink->synctime, plink->resendpkts, plink->netdelay) ;
                        printf ("  Net Conn. Time=%d  Net Packet Limit=%d  Group Pkt Size=%d  Group Timeout=%d\n",
                                plink->nettime, plink->netmax, plink->groupsize, plink->grouptime) ;
                        pcomm->completion_status = CSCS_IDLE ;
                      end
                    else
                      showerr (err) ;
                  break ;
                end
              case 3 :
                begin
                  this->command = CSCM_DIGI ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        pcomm = (comstat_rec *) ((long) me + this->comoutoffset) ;
                        pdigi = (digi_record *) &pcomm->moreinfo ;
                        strpcopy (s1, pdigi->name) ;
                        strpcopy (s2, pdigi->version) ;
                        strpcopy (s3, pdigi->clockmsg) ;
                        printf ("  Digitizer=%s, Version=%s\n", s1, s2) ;
                        printf ("  Clock Message=%s\n", s3) ;
                        printf ("  Prefilter OK=%d, Detector Load OK=%d, Set Map OK=%d\n",
                           pdigi->prefilter_ok, pdigi->detector_load_ok, pdigi->setmap_ok) ;
                        printf ("  Clock String OK=%d, Int/Ext OK=%d, Send Message OK=%d\n",
                           pdigi->clockstring_ok, pdigi->int_ext_ok, pdigi->send_message_ok) ;
                        printf ("  Message Channel OK=%d, Set OSC OK=%d, Set Clock OK=%d\n",
                           pdigi->message_chan_ok, pdigi->set_osc_ok, pdigi->set_clock_ok) ;
                        printf ("  Wait For data Seconds=%d\n", pdigi->wait_for_data) ;
                        pcomm->completion_status = CSCS_IDLE ;
                      end
                    else
                      showerr (err) ;
                  break ;
                end
              case 4 :
                begin
                  this->command = CSCM_ULTRA ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        pur = (ultra_rec *) &pcomm->moreinfo ;
                        printf ("  VCO Value=%d, PLL ON=%d, Mass Rec. OK=%d, Revision=%d\n",
                          pur->vcovalue, pur->pllon, pur->umass_ok, pur->ultra_rev) ;
                        s1[0] = '\0' ;
                        pc1 = (pchar) &pur->commnames ;
                        if (pc1[0] != '\0')
                          then
                            begin
                              for (i = 0 ; i < CE_MAX ; i++)
                                begin
                                  strpcopy (s2, pc1) ;
                                  pc1 = (pchar) ((long) pc1 + *pc1 + 1) ;
                                  strcat (s1, s2) ;
                                  if (test_bit(pur->comm_mask, i))
                                    then
                                      strcat (s1, "+") ;
                                    else
                                      strcat (s1, "-") ;
                                  strcat (s1, " ") ;
                                  if (strlen(s1) > 70)
                                    then
                                      begin
                                        printf ("  %s\n", s1) ;
                                        s1[0] = '\0' ;
                                      end
                                end
                              if (s1[0] != '\0')
                                then
                                  printf ("  %s\n", s1) ;
                            end
                        pcomm->completion_status = CSCS_IDLE ;
                      end
                    else
                      showerr (err) ;
                  break ;
                end
              case 5 :
                begin
                  this->command = CSCM_CAL ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        pcal = (cal_record *) &pcomm->moreinfo ;
                        printf ("  Number of calibrators=%d, Mass Rec. OK=%d\n",
                          pcal->number, pcal->mass_ok) ;
                        for (i = 0 ; i < pcal->number ; i++)
                          begin
                            pe = &pcal->acal[i] ;
                            printf ("  Calibrator number %d\n", i + 1) ;
                            printf ("    Coupling Option=%d, Polarity Option=%d, Board=%d\n",
                              pe->coupling_option, pe->polarity_option, pe->board) ;
                            printf ("    Min Settle=%d, Max Settle=%d, Settle Increment=%d\n",
                              pe->min_settle, pe->max_settle, pe->inc_settle) ;
                            printf ("    Min Mass=%d, Max Mass=%d, Mass Increment=%d, Mass Default=%d\n",
                              pe->min_mass_dur, pe->max_mass_dur, pe->inc_mass_dur, pe->def_mass_dur) ;
                            printf ("    Min Filter=%d, Max Filter=%d\n",
                              pe->min_filter, pe->max_filter) ;
                            printf ("    Min Amplitude=%d, Max Amplitude=%d, Amplitude Step=%d\n",
                              pe->min_amp, pe->max_amp, pe->amp_step) ;
                            printf ("    Monitor Chan=%d, Min Random Period=%d, Max Random Period=%d\n",
                              pe->monitor, pe->rand_min_period, pe->rand_max_period) ;
                            printf ("    Default Step Filter=%d, Default Random Period=%d\n",
                              pe->default_step_filt, pe->default_rand_filt) ;
                            for (k = SINE ; k <= WRAND ; k++)
                              begin
#ifdef _OSK
                                printf ("    %s Periods\n", (pchar) waves[k]) ;
#else
                                printf ("    %s Periods\n", &(waves[k])) ;
#endif
                                printf ("      Minimum=%d, Maximum=%d, Increment=%d\n",
                                  pe->durations[k].min_dur, pe->durations[k].max_dur, pe->durations[k].inc_dur) ;
                              end
                            printf ("    Channel Map=%s\n", showmap(pe->map)) ;
                            printf ("    Waveforms supported=%s\n", showwaves(pe->waveforms)) ;
                            printf ("    Sine wave frequencies supported : \n") ;
                            showfreqs (pe->sine_freqs) ;
                            for (k = 0 ; k < MAXCAL_FILT ; k++)
                              begin
                                printf ("    Default Sine frequencies for filter number %d\n", k + 1) ;
                                showfreqs (pe->default_sine_filt[k]) ;
                              end
                            strpcopy (s1, pe->name) ;
                            strpcopy (s2, pe->filtf) ;
                            printf ("    Calibrator Name=%s\n", s1) ;
                            printf ("    Filter Description=%s\n", s2) ;
                          end
                        pcomm->completion_status = CSCS_IDLE ;
                      end
                     else
                      showerr (err) ;
                  break ;
                end
              case 6 :
                begin
                  err = loadchan () ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        for (i = 0 ; i < pcs->chancount ; i++)
                          begin
                            pcr = &pcs->chans[i] ;
                            printf ("    %s Physical=%d, Detectors=%d", 
                                seednamestring (&pcr->seedname, &pcr->seedloc), 
                                pcr->physical, pcr->det_count) ;
                            if ((pcr->available and 1) != 0)
                              then
                                printf (", Cprio=%s", cl_prio(pcr->c_prio)) ;
                            if ((pcr->available and 2) != 0)
                              then
                                printf (", Eprio=%s", cl_prio(pcr->e_prio)) ;
                            printf (", Rate=") ;
                            if (pcr->rate > 0)
                              then
                                printf ("%4.3f\n", 1.0 * pcr->rate) ;
                              else
                                printf ("%4.3f\n", 1.0 / (0 - pcr->rate)) ;
                            printf ("      Available Outputs=%s\n", showout(pcr->available)) ;
                            printf ("      Enabled   Outputs=%s\n", showout(pcr->enabled)) ;
                          end
                        pcomm->completion_status = CSCS_IDLE ;
                      end
                    else
                      showerr (err) ;
                  break ;
                end
              case 7 :
                begin
                  err = get_det () ;
                  if (err == CSCR_NODATA)
                    then
                      begin
                        printf ("No detectors available\n") ;
                        break ;
                      end
                  if (err != CSCR_GOOD)
                    then
                      begin
                        showerr (err) ;
                        break ;
                      end
                  pdrr = (det_request_rec *) &pcomm->moreinfo ;
                  printf ("%d Detectors found\n", pdrr->count) ;
                  for (i = 0 ; i < pdrr->count ; i++)
                    begin
                      pdd = &pdrr->dets[i] ;
                      strpcopy (s1, pdd->name) ;
                      strpcopy (s2, pdd->params[0]) ;
                      printf ("Detector %s, Type=%s, Enabled=%d, Remote=%d, ID=%d\n",
                               &s1, &s2, pdd->enabled, pdd->remote, pdd->id) ;
                      s1[0] = '\0' ;
                      plong = (long *) &pdd->cons ;
                      for (j = 1 ; j < 12 ; j++)
                        begin
                          if ((pdd->params[j])[0] != '\0')
                            then
                              begin
                                strpcopy (s2, pdd->params[j]) ;
                                strcat (s1, s2) ;
                                strcat (s1, "=") ;
                                if (j == 11)
                                  then
                                    begin
                                      pshort = (short *) plong ;
                                      sprintf(s2, "%d", *pshort) ;
                                    end
                                  else
                                    sprintf(s2, "%d", *plong) ;
                                strcat (s1, s2) ;
                                strcat (s1, " ") ;
                                if (strlen(s1) > 70)
                                  then
                                    begin
                                      printf("  %s\n", s1) ;
                                      s1[0] = '\0' ;
                                    end
                              end
                          plong++ ;
                        end
                      if (s1[0] != '\0')
                        then
                          printf("  %s\n", s1) ;
                    end
                  break ;                             
                end
              case 8 : /* Client info */
                begin
                  this->command = CSCM_CLIENTS ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        pci = (client_info *) &pcomm->moreinfo ;
                        for (i = 0 ; i < pci->client_count ; i++)
                          begin
                            poc = &(pci->clients[i]) ;
                            printf ("Client Number=%d Name=%s, Memory ID=%d, Process ID=%d, Active=%d\n",
                                    i + 1, long_str(poc->client_name.l), poc->client_memid, 
                                    poc->client_pid, poc->active) ;
                            printf ("    Timeout=%d, Packets Blocked=%d, Blocking=%d, Reserved=%d\n",
                                    poc->timeout, poc->block_count, poc->blocking, poc->reserved) ;
                            printf ("    Last Service=%s\n", time_string(poc->last_service)) ;
                            pcomm->completion_status = CSCS_IDLE ;
                          end
                      end
                    else
                      showerr (err) ;
                  break ;
                end
              case 9 :
                begin
                  printf ("Unblock packets from Client Number : ") ;
                  gets (s1) ;
                  if (s1[0] == '\0')
                    then
                      break ;
                  sscanf (s1, "%i", &sel2) ;
                  if ((sel2 < 1) lor (sel2 > MAXCLIENTS))
                    then
                      begin
                        printf ("Invalid client number\n") ;
                        break ;
                      end
                  pshort = (short *) ((long) me + this->cominoffset) ;
                  *pshort = sel2 - 1 ;
                  this->command = CSCM_UNBLOCK ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        printf ("  Packets unblocked\n") ;
                        pcomm->completion_status = CSCS_IDLE ;
                      end
                    else
                      showerr (err) ;
                  break ;
                end
              case 10 :
                begin
                  this->command = CSCM_RECONFIGURE ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        printf ("  Reconfiguration in progress\n") ;
                        pcomm->completion_status = CSCS_IDLE ;
                      end
                    else
                      showerr (err) ;
                  break ;
                end
              case 11 :
                begin
                  this->command = CSCM_SUSPEND ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        printf ("  Link suspended\n") ;
                        pcomm->completion_status = CSCS_IDLE ;
                      end
                    else
                      showerr (err) ;
                  break ;
                end
              case 12 :
                begin
                  this->command = CSCM_RESUME ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        printf ("  Link resumed\n") ;
                        pcomm->completion_status = CSCS_IDLE ;
                      end
                    else
                      showerr (err) ;
                  break ;
                end
              case 13 :
                begin
                  this->command = CSCM_CMD_ACK ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      printf ("  Command Status Cleared\n") ;
                    else
                      showerr (err) ;
                  break ;
                end
              case 14 :
                begin
                  showerr (pcomm->completion_status) ;
                  break ;
                end
              case 15 :
                begin
                  this->command = CSCM_TERMINATE ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      printf ("Server Terminated\n") ;
                    else
                      showerr (err) ;
                  break ;
                end
              case 16 :
                begin
                  this->command = CSCM_SHELL ;
                  pshell = (shell_com *) ((long) me + this->cominoffset) ;
                  pshell->log_local = FALSE ;
                  pshell->log_host = FALSE ;
                  printf ("Shell: ") ;
                  gets (s1) ;
                  if (s1[0] == '\0')
                    then
                      break ;
                  strpas (pshell->shell_parameter, s1) ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        err = wait_finished (pcomm) ;
                        if (err == CSCS_FINISHED)
                          then
                            begin
                              printf ("Shell Command Sent\n") ;
                              pcomm->completion_status = CSCS_IDLE ;
                            end
                          else
                            showerr (err) ;
                      end
                    else
                      showerr (err) ;
                  break ;
                end
              case 17 :
                begin
                  this->command = CSCM_VCO ;
                  pshort = (short *) ((long) me + this->cominoffset) ;
                  printf ("VCO Value (0-4095, -1=PLL) : ") ;
                  gets (s1) ;
                  if (s1[0] == '\0')
                    then
                      break ;
                  sscanf (s1, "%i", &sel) ;
                  *pshort = sel ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        err = wait_finished (pcomm) ;
                        if (err == CSCS_FINISHED)
                          then
                            begin
                              printf ("VCO value sent\n") ;
                              pcomm->completion_status = CSCS_IDLE ;
                            end
                          else
                            showerr (err) ;
                      end
                    else
                      showerr (err) ;
                  break ;
                end
              case 18 :
                begin
                  this->command = CSCM_LINKADJ ;
                  padj = (linkadj_com *) ((long) me + this->cominoffset) ;
                  padj->window_size = replace ("Window Size", curlink.window_size) ;
                  padj->set_msg = replace ("Message Priority", curlink.msg_prio) ;
                  padj->set_det = replace ("Detection Priority", curlink.det_prio) ;
                  padj->set_time = replace ("Timing Priroity", curlink.time_prio) ;
                  padj->set_calp = replace ("Calibration Priority", curlink.cal_prio) ;
                  padj->resendtime = replace ("Resend Timeout", curlink.resendtime) ;
                  padj->synctime = replace ("Sync Packet Interval", curlink.synctime) ;
                  padj->resendpkts = replace ("Packets in Resend Block", curlink.resendpkts) ;
                  padj->netdelay = replace ("Network Restart Delay", curlink.netdelay) ;
                  padj->nettime = replace ("Network Connect Timeout", curlink.nettime) ;
                  padj->netmax = replace ("Network Packet Limit", curlink.netmax) ;
                  padj->groupsize = replace ("Packets in Group", curlink.groupsize) ;
                  padj->grouptime = replace ("Group Timeout", curlink.grouptime) ;
                  padj->lasp1 = 0 ;
                  padj->lasp2 = 0 ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        err = wait_finished (pcomm) ;
                        if (err == CSCS_FINISHED)
                          then
                            begin
                              printf ("New Link parameters sent\n") ;
                              curlink.window_size = padj->window_size ;
                              curlink.msg_prio = padj->set_msg ;
                              curlink.det_prio = padj->set_det ;
                              curlink.time_prio = padj->set_time ;
                              curlink.cal_prio = padj->set_calp ;
                              curlink.resendtime = padj->resendtime ;
                              curlink.synctime = padj->synctime ;
                              curlink.resendpkts = padj->resendpkts ;
                              curlink.netdelay = padj->netdelay ;
                              curlink.nettime = padj->nettime ;
                              curlink.netmax = padj->netmax ;
                              curlink.groupsize = padj->groupsize ;
                              curlink.grouptime = padj->grouptime ;
                              pcomm->completion_status = CSCS_IDLE ;
                            end
                          else
                            showerr (err) ;
                      end
                    else
                      showerr (err) ;
                  break ;
                end
              case 19 :
                begin
                  sel = getcal () ;
                  if (sel == 0)
                    then
                      break ;
                  pcal = (cal_record *) &pcomm->moreinfo ;
                  pe = &pcal->acal[sel-1] ;
                  if (pe->max_mass_dur == 0)
                    then
                      break ;
                  if (pe->min_mass_dur != pe->max_mass_dur)
                    then
                      begin
                        printf ("Duration in ms [%d-%d CR=%d] : ", pe->min_mass_dur, pe->max_mass_dur, pe->def_mass_dur) ;
                        gets(s1) ;
                        if (s1[0] == '\0')
                          then
                            k = pe->def_mass_dur ;
                          else
                            begin
                              sscanf (s1, "%i", &sel2) ;
                              k = sel2 ;
                            end
                      end
                    else
                      k = pe->def_mass_dur ;
                  if ((k < pe->min_mass_dur) lor (k > pe->max_mass_dur))
                    then
                      printf ("Invalid duration\n") ;
                    else
                      begin
                        prc = (recenter_com *) ((long) me + this->cominoffset) ;
                        prc->board = sel ;
                        prc->duration = k ;
                        this->command = CSCM_MASS_RECENTER ;
                        err = cs_svc(me, 0) ;
                        if (err == CSCR_GOOD)
                          then
                            begin
                              err = wait_finished (pcomm) ;
                              if (err == CSCS_FINISHED)
                                then
                                  begin
                                    printf ("Mass Recentering Command sent\n") ;
                                    pcomm->completion_status = CSCS_IDLE ;
                                  end
                                else
                                  showerr (err) ;
                            end
                          else
                            showerr (err) ;
                      end
                  break ;
                end 
              case 20 :
                begin
                  sel = getcal () ;
                  if (sel == 0)
                    then
                      break ;
                  pcal = (cal_record *) &pcomm->moreinfo ;
                  pe = &pcal->acal[sel-1] ;
                  low = lowest(pe->map) ;
                  high = highest(pe->map) ;
                  pcsc = (cal_start_com *) ((long) me + this->cominoffset) ;
                  pcsc->calnum = sel ;
                  stopcal = FALSE ;
                  do
                    begin
                      pcsc->map = 0 ;
                      printf ("Channels to Calibrate [i.e. %s, CR=Stop Cal] : ", chanstring) ;
                      gets (s1) ;
                      if (s1[0] == '\0')
                        then
                          begin
                            pshort = (short *) pcsc ;
                            *pshort = sel ;
                            this->command = CSCM_CAL_ABORT ;
                            err = cs_svc(me, 0) ;
                            if (err == CSCR_GOOD)
                              then
                                begin
                                  err = wait_finished (pcomm) ;
                                  if (err == CSCS_FINISHED)
                                    then
                                      begin
                                        printf ("Calibrate Abort Command sent\n") ;
                                        pcomm->completion_status = CSCS_IDLE ;
                                      end
                                    else
                                      showerr (err) ;
                                end
                              else
                                showerr (err) ;
                            stopcal = TRUE ;
                            break ;
                          end
                      err = FALSE ;
                      i = 0 ;
                      while (s1[i] != '\0')
                        begin
                          if (s1[i] == ',')
                            then
                              s1[i] = ' ' ;
                          i++ ;
                        end
                      for (i = 0 ; i < 4 ; i++)
                        s[i][0] = '\0' ;
                      sscanf (s1, "%s%s%s%s", s[0], s[1], s[2], s[3]) ;
                      for (i = 0 ; i < 4 ; i++)
                        begin
                          if (s[i][0] == '\0')
                            then
                              break ;
                          found = FALSE ;
                          for (j = 0 ; j < channel_count ; j++)
#ifdef _OSK
                            if (strcasecmp((pchar) channels[j], (pchar) s[i]) == 0)
#else
                            if (strcasecmp((pchar)&channels[j], (pchar)&s[i]) == 0)
#endif
                              then
                                begin
                                  pcsc->map = pcsc->map or (1 << j) ;
                                  found = TRUE ;
                                  break ;
                                end
                          if (lnot found)
                            then
                              err = TRUE ;
                        end
                    end
                  while (err) ;
                  if ((stopcal) lor (pcsc->map == 0))
                    then
                      break ;
                  pcsc->plus = FALSE ;
                  pcsc->capacitor = FALSE ;
                  pcsc->settle = 1 ;
                  pcsc->autoflag = 0 ;
                  pcsc->ext_sp1 = 0 ;
                  pcsc->ext_sp2 = 0 ;
                  pcsc->filt = pe->min_filter ;
                  if (pe->coupling_option)
                    then
                      begin
                        printf ("Capacitive or Resistive Coupling (C/*R) : ") ;
                        gets(s1) ;
                        pcsc->capacitor = ((s1[0] == 'C') lor (s1[0] == 'c')) ;
                      end
                  if (pe->min_amp == pe->max_amp)
                    then
                      begin
                        pcsc->amp = pe->max_amp ;
                        printf ("Amplitude fixed at %d dB\n", pe->max_amp) ;
                      end
                    else
                      do
                        begin
                          printf ("Amplitude [%d to %d dB in %d dB steps] : ",
                            pe->min_amp, pe->max_amp, pe->amp_step) ;
                          gets(s1) ;
                          sscanf (s1, "%i", &sel2) ;
                          pcsc->amp = sel2 ;
                        end
                      while ((sel2 > pe->max_amp) lor (sel2 < pe->min_amp) lor
                         (((0 - pcsc->amp) mod pe->amp_step) != 0)) ;
                  do
                    begin
                      i = 0 ;
                      for (k = SINE ; k <= WRAND ; k++)
                        if (test_bit(pe->waveforms, k))
                          then
                            begin
                              widx[i++] = k ;
#ifdef _OSK
                              printf ("%d=%s ", i, (pchar) waves[k]) ;
#else
                              printf ("%d=%s ", i, &(waves[k])) ;
#endif
                            end
                      if (i == 1)
                        then
                          begin
                            k = widx[0] ;
#ifdef _OSK
                            printf ("Waveform is %s\n", (pchar) waves[k]) ;
#else
                            printf ("Waveform is %s\n", &(waves[k])) ;
#endif
                            break ;
                          end
                        else
                          begin
                            printf (": ") ;
                            gets(s1) ;
                            sscanf (s1, "%i", &sel2) ;
                            j = sel2 ;
                            if ((j > 0) and (j <= i))
                              then
                                begin
                                  k = widx[j - 1] ;
                                  break ;
                                end
                          end
                    end
                  while (1) ;
                  pcsc->calcmd = k ;
                  switch (k)
                    begin
                      case STEP :
                        begin
                          if (pe->polarity_option)
                            then
                              begin
                                printf ("Polarity of step [*P/N] : ") ;
                                gets(s1) ;
                                pcsc->plus = lnot ((s1[0] == 'N') lor (s1[0] == 'n')) ;
                              end
                            else
                              pcsc->plus = TRUE ;
                          pcsc->filt = pe->default_step_filt ;
                          break ;
                        end
                      case SINE :
                        begin
                          i = 0 ;
                          for (frq = Hz25_000 ; frq <= Hz0_0005 ; frq++)
                            if (test_bit(pe->sine_freqs, frq))
                              then
                                begin
                                  fidx[i++] = frq ;
#ifdef _OSK
                                  printf ("%2d=%sHz ", i, (pchar) freqs[frq]) ;
#else
                                  printf ("%2d=%sHz ", i, &(freqs[frq])) ;
#endif
                                  if ((i mod 6) == 0)
                                    then
                                      printf ("\n") ;
                                end
                          if ((i mod 6) != 0)
                            then
                              printf ("\n") ;
                          do
                            begin
                              printf ("Sine frequency number [1-%d] : ", i) ;
                              gets(s1) ;
                              sscanf (s1, "%i", &sel2) ;
                            end
                          while ((sel2 < 1) lor (sel2 > i)) ;
                          pcsc->sfrq = fidx[sel2 - 1] ;
                          for (i = 0 ; i < MAXCAL_FILT ; i++)
                            if (test_bit(pe->default_sine_filt[i], pcsc->sfrq))
                              then
                                begin
                                  pcsc->filt = i + 1 ;
                                  break ;
                                end
                          break ;
                        end
                      case RAND : ;
                      case WRAND :
                        begin
                          if (pe->rand_min_period != pe->rand_max_period)
                            then
                              begin
                                do
                                  begin
                                    printf ("Period multiplier [%d-%d] : ",
                                        pe->rand_min_period, pe->rand_max_period) ;
                                    gets(s1) ;
                                    sscanf (s1, "%i", &sel2) ;
                                    pcsc->rmult = sel2 ;
                                  end
                                while ((pcsc->rmult < pe->rand_min_period) lor 
                                       (pcsc->rmult > pe->rand_max_period)) ;
                              end
                            else
                              pcsc->rmult = pe->rand_min_period ;
                          pcsc->filt = pe->default_rand_filt ;
                        end
                    end
                  if (pe->max_filter > 0)
                    then
                      begin
                        j = pcsc->filt ;
                        do
                          begin
                            strpcopy (s2, pe->filtf) ;
                            printf ("3dB Filter cutoff %s [*%d] : ", &s2, j) ;
                            gets(s1) ;
                            if (s1[0] != '\0')
                              then
                                begin
                                  sscanf (s1, "%i", &sel2) ;
                                  pcsc->filt = sel2 ;
                                end
                          end
                        while ((pcsc->filt < pe->min_filter) lor (pcsc->filt > pe->max_filter)) ;
                      end
                  pd = &pe->durations[k] ;
                  do
                    begin
                      if (pd->inc_dur == 60)
                        then
                          begin
                            printf ("Duration in minutes [%d-%d] : ", pd->min_dur div 60, 
                                    pd->max_dur div 60) ;
                            gets(s1) ;
                            if (strcasecmp((pchar)&s1, "MAX") == 0)
                              then
                                begin
                                  pcsc->duration = 0 ;
                                  break ;
                                end
                            sscanf (s1, "%d", &sel2) ;
                            pcsc->duration = sel2 * 60 ;
                          end
                        else
                          begin
                            printf ("Duration in seconds [%d-%d] : ", pd->min_dur, pd->max_dur) ;
                            gets(s1) ;
                            if (strcasecmp((pchar)&s1, "MAX") == 0)
                              then
                                begin
                                  pcsc->duration = 0 ;
                                  break ;
                                end
                            sscanf (s1, "%d", &sel2) ;
                            pcsc->duration = sel2 ;
                          end
                    end
                  while ((pcsc->duration < pd->min_dur) lor (pcsc->duration > pd->max_dur)) ;
                  if (pe->inc_settle == 1)
                    then
                      do
                        begin
                          printf ("Relay settling delay in seconds [%d-%d] : ",
                            pe->min_settle, pe->max_settle) ;
                          gets(s1) ;
                          sscanf (s1, "%d", &sel2) ;
                          pcsc->settle = sel2 ;
                        end
                      while ((pcsc->settle < pe->min_settle) lor (pcsc->settle > pe->max_settle)) ;
                  else if (pe->inc_settle == 60)
                    then
                      do
                        begin            
                          printf ("Relay settling delay in minutes [%d-%d] : ",
                            pe->min_settle div 60, pe->max_settle div 60) ;
                          gets(s1) ;
                          sscanf (s1, "%d", &sel2) ;
                          pcsc->settle = sel2 * 60 ;
                        end
                      while ((pcsc->settle < pe->min_settle) lor (pcsc->settle > pe->max_settle)) ;
                  this->command = CSCM_CAL_START ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        err = wait_finished (pcomm) ;
                        if (err == CSCS_FINISHED)
                          then
                            begin
                              printf ("Calibration Start Command sent\n") ;
                              pcomm->completion_status = CSCS_IDLE ;
                            end
                          else
                            showerr (err) ;
                      end
                    else
                      showerr (err) ;
                  break ;
                end
              case 21 :
                begin
                  pdec = (det_enable_com *) ((long) me + this->cominoffset) ;
                  pdec->count = 0 ;
                  printf ("You must know the detector ID to use this command. Use command 7 to obtain IDs\n") ;
                  while (pdec->count < 20)
                    begin
                      printf ("ID of detector [CR when done] : ") ;
                      gets (s1) ;
                      if (s1[0] == '\0')
                        then
                          break ;
                      sscanf (s1, "%i", &sel2) ;
                      pdec->detectors[pdec->count].detector_id = sel2 ;
                      pdec->detectors[pdec->count].de_sp1 = 0 ;
                      printf ("Enable Detector [Y/N] : ") ;
                      gets (s1) ;
                      pdec->detectors[pdec->count++].enable = ((s1[0] == 'Y') lor (s1[0] == 'y')) ;
                    end
                  if (pdec->count)
                    then
                      begin
                        this->command = CSCM_DET_ENABLE ;
                        err = cs_svc(me, 0) ;
                        if (err == CSCR_GOOD)
                          then
                            begin
                              err = wait_finished (pcomm) ;
                              if (err == CSCS_FINISHED)
                                then
                                  begin
                                    printf ("Detector Enable Command sent\n") ;
                                    pcomm->completion_status = CSCS_IDLE ;
                                  end
                                else
                                  showerr (err) ;
                            end
                          else
                            showerr (err) ;
                      end
                  break ;
                end
              case 22 :
                begin
                  err = get_det () ;
                  if (err == CSCR_NODATA)
                    then
                      begin
                        printf ("No detectors available\n") ;
                        break ;
                      end
                  if (err != CSCR_GOOD)
                    then
                      begin
                        showerr (err) ;
                        break ;
                      end
                  pdrr = (det_request_rec *) &pcomm->moreinfo ;
                  printf ("%d Detectors found\n", pdrr->count) ;
                  for (i = 0 ; i < pdrr->count ; i++)
                    begin
                      pdd = &pdrr->dets[i] ;
                      strpcopy (s1, pdd->name) ;
                      strpcopy (s2, pdd->params[0]) ;
                      printf ("Detector %s, Type=%s, Enabled=%d, Remote=%d, ID=%d\n",
                               &s1, &s2, pdd->enabled, pdd->remote, pdd->id) ;
                      s1[0] = '\0' ;
                    end
                  printf ("Detector ID to Change : ") ;
                  gets (s1) ;
                  if (s1[0] == '\0')
                    then
                      break ;
                  sscanf (s1, "%i", &sel2) ;
                  found = FALSE ;
                  for (i = 0 ; i < pdrr->count ; i++)
                    begin
                      pdd = &pdrr->dets[i] ;
                      if (pdd->id == sel2)
                        then
                          begin
                            found = TRUE ;
                            break ;
                          end
                    end
                  if (lnot found)
                    then
                      begin
                        printf ("Detector ID not found\n") ;
                        break ;
                      end
                  pdcc = (det_change_com *) ((long) me + this->cominoffset) ;
                  pdcc->id = sel2 ;
                  pdcc->dct_sp = 0 ;
                  printf ("Enable Detector [Y/N] : ") ;
                  gets (s1) ;
                  pdcc->enab = ((s1[0] == 'Y') lor (s1[0] == 'y')) ;
                  plong = (long *) &pdd->cons ;
                  plong2 = (long *) &pdcc->ucon ;
                  for (j = 1 ; j < 12 ; j++)
                    begin
                      if ((pdd->params[j])[0] != '\0')
                        then
                          begin
                            strpcopy (s2, pdd->params[j]) ;
                            if (j == 11)
                              then
                                begin
                                  pshort = (short *) plong ;
                                  pshort2 = (short *) plong2 ;
                                  *pshort2 = replace(s2, *pshort) ;
                                end
                              else
                                *plong2 = replace (s2, *plong) ;
                          end
                      plong++ ;
                      plong2++ ;
                    end
                  this->command = CSCM_DET_CHANGE ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        err = wait_finished (pcomm) ;
                       if (err == CSCS_FINISHED)
                         then
                           begin
                             printf ("Detector Change Command sent\n") ;
                             pcomm->completion_status = CSCS_IDLE ;
                           end
                         else
                           showerr (err) ;
                      end
                    else
                      showerr (err) ;
                  break ;
                end
              case 23 :
                begin
                  this->command = CSCM_LINK ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        plink = (link_record *) &pcomm->moreinfo ;
                        high = plink->total_prio - 1 ;
                      end
                    else
                      begin
                        showerr (err) ;
                        break ;
                      end
                  pcomm->completion_status = CSCS_IDLE ;
                  err = loadchan () ;
                  if (err != CSCR_GOOD)
                    then
                      begin
                        showerr (err) ;
                        break ;
                      end
                  prec = (rec_enable_com *) ((long) me + this->cominoffset) ;
                  prec->count = 0 ;
                  while (1)
                    begin
                      if (prec->count == 8)
                        then
                          break ;
                      printf ("Channel specification to change ([LL-]SSS or CR if done) : ") ;
                      gets(s1) ;
                      upshift(s1) ;
                      if (s1[0] == '\0')
                        then
                          break ;
                      found = FALSE ;
                      for (j = 0 ; j < pcs->chancount ; j++)
                        begin
                          pcr = &pcs->chans[j] ;
                          if (strcmp(s1, seednamestring(&pcr->seedname, &pcr->seedloc)) == 0)
                            then
                              begin
                                found = TRUE ;
                                break ;
                              end
                        end
                      if (found)
                        then
                          begin
                            pro = &prec->changes[prec->count++] ;
                            pro->rec_sp1 ;  /* Statement with no effect! Bug? */
                            memcpy(pro->seedname, pcr->seedname, 3) ;
                            memcpy(pro->seedloc, pcr->seedloc, 2) ;
                            ltemp = 0 ;
                            for (k = 0 ; k <= 5 ; k++)
                              if (test_bit(pcr->available, k))
                                then
                                  begin
                                    if (test_bit(pcr->enabled, k))
                                      then
                                        strcpy (s2, "Y") ;
                                      else
                                        strcpy (s2, "N") ;
#ifdef _OSK
                                    printf ("Enable %s (Y/N, CR=%s) : ", (pchar) outpath[k], s2) ;
#else
                                    printf ("Enable %s (Y/N, CR=%s) : ", &(outpath[k]), s2) ;
#endif
                                    gets(s1) ;
                                    if (s1[0] == '\0')
                                      then
                                        strcpy(s1, s2) ;
                                    if ((s1[0] == 'Y') lor (s1[0] == 'y'))
                                      then
                                        set_bit (&ltemp, k) ;
                                  end
                            pro->mask = ltemp ;
                            pro->c_prio = pcr->c_prio ;
                            pro->e_prio = pcr->e_prio ;
                            cp_valid = ((pcr->available and 1) != 0) ;
                            ep_valid = ((pcr->available and 2) != 0) ;
                            if (cp_valid)
                              then
                                begin
                                  while (1)
                                    begin
                                      printf ("Continuous priority (CR = %s) : ", cl_prio(pcr->c_prio)) ;
                                      gets (s2) ;
                                      if (s2[0] != '\0')
                                        then
                                          if (strcasecmp((pchar) s2, "DEF") == 0)
                                            then
                                              l = 120 ;
                                            else
                                              sscanf (s2, "%d", &l) ;
                                        else
                                          l = pcr->c_prio ;
                                      i = l ;
                                      good = (((i >= 1) land (i <= high)) lor (i == 120)) ;
                                      if (cp_valid land ep_valid)
                                        then
                                          for (j = 0 ; j < pcs->chancount ; j++)
                                            begin
                                              pcr2 = &pcs->chans[j] ;
                                              if ((pcr2->stream != pcr->stream) land
                                                 ((pcr2->c_prio == i) lor (pcr2->e_prio == i)))
                                                then
                                                  begin
                                                    good = FALSE ;
                                                    break ;
                                                  end
                                            end
                                      if (good)
                                        then
                                          begin
                                            pro->c_prio = i ;
                                            break ;
                                          end
                                        else
                                          printf ("Invalid priority value\n") ;
                                    end
                                end
                            if (ep_valid)
                              then
                                begin
                                  while (1)
                                    begin
                                      printf ("Event priority (CR = %s) : ", cl_prio(pcr->e_prio)) ;
                                      gets (s2) ;
                                      if (s2[0] != '\0')
                                        then
                                          if (strcasecmp((pchar) s2, "DEF") == 0)
                                            then
                                              l = 120 ;
                                            else
                                              sscanf (s2, "%d", &l) ;
                                        else
                                          l = pcr->e_prio ;
                                      i = l ;
                                      good = (((i >= 1) land (i <= high)) lor (i == 120)) ;
                                      if (cp_valid land ep_valid)
                                        then
                                          for (j = 0 ; j < pcs->chancount ; j++)
                                            begin
                                              pcr2 = &pcs->chans[j] ;
                                              if ((pcr2->stream != pcr->stream) land
                                                 ((pcr2->c_prio == i) lor (pcr2->e_prio == i)))
                                                then
                                                  begin
                                                    good = FALSE ;
                                                    break ;
                                                  end
                                            end
                                      if (good)
                                        then
                                          begin
                                            pro->e_prio = i ;
                                            break ;
                                          end
                                        else
                                          printf ("Invalid priority value\n") ;
                                    end
                                end
                          end
                    end
                  if (prec->count)
                    then
                      begin
                        this->command = CSCM_REC_ENABLE ;
                        err = cs_svc(me, 0) ;
                        if (err == CSCR_GOOD)
                          then
                            begin
                              err = wait_finished (pcomm) ;
                              if (err == CSCS_FINISHED)
                                then
                                  begin
                                    printf ("Recording Enable Command sent\n") ;
                                    pcomm->completion_status = CSCS_IDLE ;
                                  end
                                else
                                  showerr (err) ;
                            end
                          else
                            showerr (err) ;
                      end
                  break ;
                end
              case 24 :
                begin
                  this->command = CSCM_ULTRA ;
                  err = cs_svc(me, 0) ;
                  if (err != CSCR_GOOD)
                    then
                      begin
                        showerr (err) ;
                        break ;
                      end
                  printf ("Follow Comm Event name with + to enable, - to disable\n") ;
                  pcomm->completion_status = CSCS_IDLE ;
                  pur = (ultra_rec *) &pcomm->moreinfo ;
                  ltemp = 0 ;
                  mtemp = 0 ;
                  while (1)
                    begin
                      printf ("Comm Event name to Change (CR to stop) : ") ;
                      gets (s1) ;
                      untrail(s1) ;
                      if (s1[0] == '\0')
                        then
                          break ;
                      upshift(s1) ;
                      found = FALSE ;
                      i = strlen(s1) - 1 ;
                      enable = (s1[i] == '+') ;
                      s1[i] = '\0' ;
                      pc1 = (pchar) &pur->commnames ;
                      for (i = 0 ; i < CE_MAX ; i++)
                        begin
                          strpcopy (s2, pc1) ;
                          if (strcmp(s2, s1) == 0)
                            then
                              begin
                                set_bit (&mtemp, i) ; /* change this bit */
                                if (enable)
                                  then
                                    set_bit (&ltemp, i) ;
                                  else
                                    clr_bit (&ltemp, i) ;
                                found = TRUE ;
                                break ;
                              end
                            else
                              pc1 = (pchar) ((long) pc1 + *pc1 + 1) ;
                        end
                      if (lnot found)
                        then
                          printf ("Name not found\n") ;
                    end
                  pcec = (comm_event_com *) ((long) me + this->cominoffset) ;
                  pcec->remote_map = ltemp ;
                  pcec->remote_mask = mtemp ;
                  this->command = CSCM_COMM_EVENT ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        err = wait_finished (pcomm) ;
                        if (err == CSCS_FINISHED)
                          then
                            begin
                              printf ("Comm Event Command sent\n") ;
                              pcomm->completion_status = CSCS_IDLE ;
                            end
                          else
                            showerr (err) ;
                       end
                     else
                       showerr (err) ;
                   break ;
                end
              case 25 :
                begin
                  pdc = (download_com *) ((long) me + this->cominoffset) ;
                  printf ("File name on DA : ") ;
                  gets (s1) ;
                  if (s1[0] == '\0')
                    then
                      break ;
                  s1[59] = '\0' ; /* just in case */
                  strpas(pdc->dasource, s1) ; /* Must be Pascal string */
                  pdc->dpmodname[0] = '\0' ;    /* Not used on Unix */
                  this->command = CSCM_DOWNLOAD ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        pdr = (download_result *) &pcomm->moreinfo ;
                        pc1 = NULL ;
                        while (1)
                          begin
                            err = wait_finished (pcomm) ;
                            if (err != CSCS_INPROGRESS)
                              then
                                break ;
                            if (pdr->fsize == 0)
                              then
                                printf ("No data transferred yet, continue to wait (Y/N) : ") ;
                              else
                                printf ("%d%% percent transferred, continue to wait (Y/N) : ",
                                  (long) ((pdr->byte_count * 100.0) / (unsigned int) pdr->fsize)) ;
                            gets(s1) ;
                            if ((s1[0] != 'Y') land (s1[0] != 'y'))
                              then
                                begin
                                  this->command = CSCM_DOWNLOAD_ABORT ;
                                  err = cs_svc(me, 0) ;
                                end
                          end
                        if (err == CSCS_FINISHED)
                          then
                            begin
#ifndef _OSK
                              printf ("Is this a text file (Y/N) : ") ;
                              gets (s1) ;
#endif
                              pcomm->completion_status = CSCS_IDLE ;
                              pc1 = (pchar) shmat(pdr->dpshmid, NULL, 0) ; /* attach to data module */
                              pc2 = pc1 ;
                              if ((long) pc1 == ERROR)
                                then
                                  begin
                                    printf ("Could not attach to shared memory segment\n") ;
                                    break ;
                                  end
#ifndef _OSK
                              if ((s1[0] == 'Y') lor (s1[0] == 'y'))
                                then
                                  begin /* On Unix convert CR to LF */
                                    cnt = pdr->fsize ;
                                    while (cnt-- > 0)
                                      if (*pc2 == 0xd)
                                        then
                                          *pc2++ = 0xa ;
                                        else
                                          pc2++ ;
                                  end
#endif
                              while (1)
                                begin
                                  printf ("File name on host (CR to abort) : ") ;
                                  gets(s1) ;
                                  if (s1[0] == '\0')
                                    then
                                      break ;
                                  path = fopen (s1, "w") ;
                                  if (path == NULL)
                                    then
                                      printf ("Could not open %s\n", s1) ;
                                    else
                                      begin
                                        fwrite (pc1, 1, pdr->fsize, path) ;
                                        fclose (path) ;
                                        printf ("File transferred\n") ;
                                        break ;
                                      end
                                end 
                            end
                          else
                            showerr (err) ;
                        if (pc1 != NULL)   /* If attached, then de-attach */
                          then
                            shmdt(pc1) ;
                        if (pdr->dpshmid != NOCLIENT)  /* If exists, delete it */
                          then
                            shmctl(pdr->dpshmid, IPC_RMID, NULL) ;
                        pcomm->completion_status = CSCS_IDLE ;
                      end
                    else
                      showerr (err) ;
                  break ;
                end
              case 26 :
                begin
                  puc = (upload_com *) ((long) me + this->cominoffset) ;
                  printf ("File name on DA : ") ;
                  gets (s1) ;
                  if (s1[0] == '\0')
                    then
                      break ;
                  s1[59] = '\0' ; /* just in case */
                  strpas(puc->dadest, s1) ; /* Must be Pascal string */
                  puc->dpmodname[0] = '\0' ;    /* Not used on Unix */
                  found = FALSE ;
                  while (1)
                    begin
                      printf ("File name on host (CR to abort) : ") ;
                      gets(s1) ;
                      if (s1[0] == '\0')
                        then
                          break ;
                      path = fopen (s1, "r") ;
                      if (path == NULL)
                        then
                          printf ("Could not open %s\n", s1) ;
                        else
                          begin
#ifndef _OSK
                            if (stat (s1, &statbuf) == ERROR)
                              then
                                begin
                                  printf ("Could not get file size\n") ;
                                  break ;
                                end
                            puc->fsize = statbuf.st_size ;
#else
                            puc->fsize = _gs_size(_fileno(path)) ;
                            if (puc->fsize == ERROR)
                              then
                                begin
                                  printf ("Could not get file size\n") ;
                                  break ;
                                end
#endif
                            upshmid = shmget(IPC_PRIVATE, puc->fsize, IPC_CREAT or PERM) ;
                            if (upshmid == ERROR)
                              then
                                begin
                                  showerr (CSCR_PRIVATE) ;
                                  break ;
                                end
                            pupbuf = (tupbuf *) shmat (upshmid, NULL, 0) ;
                            if ((long) pupbuf == ERROR)
                              then
                                begin
                                  showerr (CSCR_PRIVATE) ;
                                  break ;
                                end
                            fread (pupbuf, 1, puc->fsize, path) ;
                            fclose (path) ;
                            found = TRUE ;
                            break ;
                          end
                    end 
                  if (lnot found)
                    then
                      break ;
#ifndef _OSK
                  printf ("Is this a text file (Y/N) : ") ;
                  gets (s1) ;
                  if ((s1[0] == 'Y') lor (s1[0] == 'y'))
                    then
                      begin /* On Unix convert LF to CR */
                        cnt = puc->fsize ;
                        pc2 = (pchar) pupbuf ;
                        while (cnt-- > 0)
                          if (*pc2 == 0xa)
                            then
                              *pc2++ = 0xd ;
                            else
                              pc2++ ;
                      end
#endif
                  puc->dpshmid = upshmid ;
                  shmdt ((pchar) pupbuf) ; /* don't really need to access it anymore */
                  this->command = CSCM_UPLOAD ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        pupres = (upload_result *) &pcomm->moreinfo ;
                        while (1)
                          begin
                            err = wait_finished (pcomm) ;
                            if (err != CSCS_INPROGRESS)
                              then
                                break ;
                            if (pupres->bytecount == 0)
                              then
                                printf ("No data transferred yet, continue to wait (Y/N) : ") ;
                            else if (pupres->retries > 0)
                              then
                                printf ("%d Missed packets resent, continue to wait (Y/N) : ",
                                  pupres->retries) ;
                              else
                                printf ("%d%% percent transferred, continue to wait (Y/N) : ",
                                  (long) ((pupres->bytecount * 100.0) / (unsigned int) puc->fsize)) ;
                            gets(s1) ;
                            if ((s1[0] != 'Y') land (s1[0] != 'y'))
                              then
                                begin
                                  this->command = CSCM_UPLOAD_ABORT ;
                                  err = cs_svc(me, 0) ;
                                end
                          end
                        if (err == CSCS_FINISHED)
                          then
                            begin
                              printf ("File Transferred\n") ;
                              pcomm->completion_status = CSCS_IDLE ;
                            end
                          else
                            showerr (err) ;
                      end
                    else
                      showerr (err) ;
                  shmctl(upshmid, IPC_RMID, NULL) ;
                  break ;
                end
              case 27 :
                begin
                  this->command = CSCM_LINKSTAT ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        pls = (linkstat_rec *) &pcomm->moreinfo ;
                        plsc = (linkset_com *) ((long) me + this->cominoffset) ;
                        plsc->pollusecs = replace ("Server polling delay in us", pls->pollusecs) ;
                        plsc->reconcnt = replace ("Number of sequence errors before reconfigure", pls->reconcnt) ;
                        plsc->net_idle_to = replace ("Network packet reception timeout in seconds", pls->net_idle_to) ;
                        plsc->net_conn_dly = replace ("Network connection polling interval", pls->net_conn_dly) ;
                        plsc->grpsize = replace ("ACK packet grouping size", pls->grpsize) ;
                        plsc->grptime = replace ("ACK packet grouping timeout", pls->grptime) ;
                        pcomm->completion_status = CSCS_IDLE ;
                        this->command = CSCM_LINKSET ;
                        err = cs_svc(me, 0) ;
                        if (err == CSCR_GOOD)
                          then
                            begin
                              printf ("Server link settings changed\n") ;
                              pcomm->completion_status = CSCS_IDLE ;
                            end
                          else
                            showerr (err) ;
                       end
                     else
                       showerr (err) ;
                  break ;
                end
              case 28 :
                begin
                  this->command = CSCM_FLOOD_CTRL ;
                  pflood = (boolean *) ((long) me + this->cominoffset) ;
                  printf ("Enable Flooding (Y/N) : ") ;
                  gets(s1) ;
                  *pflood = ((s1[0] == 'Y') lor (s1[0] == 'y')) ;
                  err = cs_svc(me, 0) ;
                  if (err == CSCR_GOOD)
                    then
                      begin
                        err = wait_finished (pcomm) ;
                        if (err == CSCS_FINISHED)
                          then
                            begin
                              printf ("Flood Command Sent\n") ;
                              pcomm->completion_status = CSCS_IDLE ;
                            end
                          else
                            showerr (err) ;
                      end
                    else
                      showerr (err) ;
                  break ;
                end
           end
        end
      while (1) ;

      cs_off (me) ;
      return 0 ;
    end
