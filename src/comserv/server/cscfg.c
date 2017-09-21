/*   Server Config file parser
     Copyright 1994-1998 Quanterra, Inc.
     Written by Woodrow H. Owens

Edit History:
   Ed Date      By  Changes
   -- --------- --- ---------------------------------------------------
    0 23 Mar 94 WHO Pulled out of server.c
    1 27 Mar 94 WHO Merged blockette ring changed to separate rings.
    2 16 Apr 94 WHO Setup fake ultra structure for Shear stations.
    3  6 Jun 94 WHO Add setting user privilege structure.
    4  9 Jun 94 WHO Cleanup to avoid warnings.
    5 11 Aug 94 WHO Add setting of ipport.
    6 20 Jun 95 WHO allow setting of netto, netdly, grpsize, and
                    grptime parameters.
    7  2 Oct 95 WHO Allow setting of sync parameter.
    8  8 Oct 95 WHO Allow setting of flow parameter.
    9 17 Nov 95 WHO sync is now called notify in config file.
   10 28 Jan 96 DSN Add LOG_SEED and TIMING_SEED directives.
   11 29 May 96 WHO Start of conversion to run on OS9.
   12 13 Jun 96 WHO Add setting of "SEEDIN" flag.
   13  3 Aug 96 WHO Add setting of "ANYSTATION" and "NOULTRA" flags.
   14  3 Dec 96 WHO Add setting of "BLKBUFS".  
   15  7 Dec 96 WHO Add setting of "UDPADDR".
   16 16 Oct 97 WHO Add "MSHEAR" as duplicate of "SEEDIN" flag. Add
                    VER_CSCFG
   17 22 Oct 97 WHO Add setting of verbosity flags based on vopttable.
   18 29 Nov 97 DSN Add optional LOCKFILE directive again.
   19  8 Jan 98 WHO Lockfile not for OS9 version.
   20 23 Dec 98 WHO Add setting of "LINKRETRY".
*/
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <ctype.h>
#ifndef _OSK
#include <termio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/file.h>
#else
#include <sgstat.h>
#include <module.h>
#include <types.h>
#include <sg_codes.h>
#include <modes.h>
#include "os9inet.h"
#endif
#include "quanstrc.h"
#include "stuff.h"
#include "service.h"
#include "cfgutil.h"
#include "timeutil.h"
#include "server.h"
#include "pascal.h"
#ifdef _OSK
#include "os9stuff.h"
#endif

short VER_CSCFG = 20 ;

extern char log_channel_id[4] ;
extern char log_location_id[3] ;
extern char clock_channel_id[4] ;
extern char clock_location_id[3] ;
extern complong station ;
extern config_struc cfg ;
extern tuser_privilege user_privilege ;
extern char str1[CFGWIDTH] ;
extern char str2[CFGWIDTH] ;
extern char stemp[CFGWIDTH] ;
extern char port[SECWIDTH] ;
extern char ipport[SECWIDTH] ;
extern char ipaddr[SECWIDTH] ;
#ifndef _OSK
extern char lockfile[CFGWIDTH] ;
#endif
extern long baud ;
extern char parity ; 
extern long polltime ;
extern long reconfig_on_err ;
extern long grpsize ;
extern long grptime ;
extern long link_retry ;
extern long sl_standby ;
extern long sl_uptime ;
extern char sl_ifup[CFGWIDTH] ;
extern char sl_ifdown[CFGWIDTH] ;
extern char sl_timetable_loader[CFGWIDTH] ;
extern char sl_seqfile[CFGWIDTH] ;
extern long sl_seqsave ;
extern char sl_selectors[CFGWIDTH] ;
extern char sl_schedule_str[CFGWIDTH] ;
extern char sl_dlock_file[CFGWIDTH] ;
extern complong sl_network ;
extern boolean sl_unistation ;
extern tring rings[NUMQ] ;
extern long blockmask ;
extern long netto ;
extern long netdly ;
extern int segkey ;
extern unsigned char buf[BLOB] ;
extern tclients clients[MAXCLIENTS] ;
extern string3 seed_names[20][7] ;
extern seed_net_type network ;
extern linkstat_rec linkstat ;
extern boolean verbose ;
extern boolean rambling ;
extern boolean insane ;
extern boolean override ;
extern boolean notify ;
extern boolean flow ;
extern boolean udplink ;
extern boolean seedin ;
extern boolean anystation ;
extern boolean noultra ;
extern short sequence_mod ;
extern short highclient ;
extern short uids ;
extern ultra_type *pultra ;
extern location_type seed_locs[20][7] ;
#ifdef _OSK
extern voptentry *pvopt ;
#endif
      
static char s1[CFGWIDTH], s2[CFGWIDTH], s3[20] ;

double dtime (void) ; 
void terminate (pchar s) ;

int seedlink_stream_renaming(const char *s);

/* Sets "seed" to the nth element in the passed string if there is one */
  void parsechan (pchar s, short n, seed_name_type *seed, location_type *loc, short *phys)
    begin
      short k, j ;
      
      strcpy(s1, s) ;
      upshift (s1) ;
      (*seed)[0] = '\0' ;
      *phys = 0 ;
      for (j = 0 ; j <= n ; j++)
        begin
          split (s1, s2, ',') ;
          if ((s1[0] != '\0') land (j == n)) /* there is a replacement string for requested index */
            then
              begin
                split (s1, s3, '#') ;
                if (s3[0] != '\0')
                  then
                    *phys = atoi(s3) ;
                split (s1, s3, '-') ;
                if (s3[0] == '\0')
                  then
                    begin
                      strcpy (s3, s1) ; /* no location, move to channel */
                      strcpy (s1, "  ") ;
                    end
                while (strlen(s1) < 2)
                  strcat(s1, " ") ;
                while (strlen(s3) < 3)
                  strcat(s3, " ") ;
                memcpy((pchar) seed, s3, 3) ;
                memcpy((pchar) loc, s1, 2) ;
                return ;
              end
          strcpy (s1, s2) ;
          if (s1[0] == '\0')
            then
              return ;
        end
    end

  void set_verb (int i)
    begin
      verbose = i > 0 ;
      rambling = i > 1 ;
      insane = i > 2 ;
    end

  void readcfg (void)
    begin                           /* Read parameters for this station */
      pchar tmp ;
      short i, j, k, l, found ;
      widestring streams[7] = { "", "", "", "", "", "", "" } ;
      short rates[7] = { 20, 80, 80, 10, 1, -10, -100 } ; /* believable rates */
      seed_name_type seed ;
      location_type loc ;
      int ultra_size ;
      boolean supercal = FALSE ;
      boolean qapcal = FALSE ;
      chan_record *pcr ;
      cal_record *pcal ;
      eachcal *pec ;
 
      do
        begin
          read_cfg(&cfg, str1, str2) ;
          if (str1[0] == '\0')
            then
              break ;
#ifndef _OSK
          if (strcmp(str1, "LOCKFILE") == 0)
            then
              strcpy(lockfile, str2) ;
#endif
          if (strcmp(str1, "PORT") == 0)
            then
              strcpy(port, str2) ;
          if (strcmp(str1, "IPPORT") == 0)
            then
              strcpy(ipport, str2) ;
          if (strcmp(str1, "UDPADDR") == 0)
            then
              begin
                strcpy(ipaddr, str2) ;
                udplink = TRUE ;
              end
          else if (strcmp(str1, "BAUD") == 0)
            then
              baud = atol(str2) ;
          else if (strcmp(str1, "PARITY") == 0)
            then
              parity = toupper(str2[0]) ;
          else if (strcmp(str1, "VERBOSITY") == 0)
            then
              begin
#ifdef _OSK
                pvopt->verbosity = atoi((pchar)&str2) ;
                set_verb (longhex(pvopt->verbosity)) ;
#else
                set_verb (atoi((pchar)&str2)) ;
#endif
              end
          else if (strcmp(str1, "OVERRIDE") == 0)
            then
              override = ((str2[0] == 'y') lor (str2[0] == 'Y')) ;
          else if (strcmp(str1, "ANYSTATION") == 0)
            then
              anystation = ((str2[0] == 'y') lor (str2[0] == 'Y')) ;
          else if (strcmp(str1, "NOULTRA") == 0)
            then
              noultra = ((str2[0] == 'y') lor (str2[0] == 'Y')) ;
          else if (strcmp(str1, "NOTIFY") == 0)
            then
              notify = ((str2[0] == 'y') lor (str2[0] == 'Y')) ;
          else if (strcmp(str1, "FLOW") == 0)
            then
              flow = ((str2[0] == 'y') lor (str2[0] == 'Y')) ;
          else if (strcmp(str1, "SEEDIN") == 0)
            then
              seedin = ((str2[0] == 'y') lor (str2[0] == 'Y')) ;
          else if (strcmp(str1, "MSHEAR") == 0)
            then
              seedin = ((str2[0] == 'y') lor (str2[0] == 'Y')) ;
          else if (strcmp(str1, "STATION") == 0)
            then
              begin
                str2[5] = '\0';
                station.l = str_long (str2);
              end
          else if (strcmp(str1, "LOG_SEED") == 0)
            then
              begin
                upshift(str2) ;
                tmp = strchr(str2, '-') ;
                if (tmp)
                  then
                    begin
                      *tmp++ = '\0' ;
                      strncpy(log_location_id, str2, 2);
                    end
                  else
                    tmp = str2;
                tmp[3] = '\0';
                strncpy(log_channel_id, tmp, 3);
              end
          else if (strcmp(str1, "TIMING_SEED") == 0)
            then
              begin
                upshift(str2) ;
                tmp = strchr(str2, '-') ;
                if (tmp)
                  then
                    begin
                      *tmp++ = '\0' ;
                      strncpy(clock_location_id, str2, 2);
                    end
                  else
                    tmp = str2;
                tmp[3] = '\0';
                strncpy(clock_channel_id, tmp, 3);
              end
          else if (strcmp(str1, "SEGID") == 0)
            then
              segkey = atoi((pchar)&str2) ;
          else if (strcmp(str1, "POLLUSECS") == 0)
            then
              polltime = atol((pchar)&str2) ;
          else if (strcmp(str1, "DATABUFS") == 0)
            then
              rings[DATAQ].count = atoi((pchar)&str2) ;
          else if (strcmp(str1, "DETBUFS") == 0)
            then
              rings[DETQ].count = atoi((pchar)&str2) ;
          else if (strcmp(str1, "CALBUFS") == 0)
            then
              rings[CALQ].count = atoi((pchar)&str2) ;
          else if (strcmp(str1, "TIMBUFS") == 0)
            then
              rings[CALQ].count = atoi((pchar)&str2) ;
          else if (strcmp(str1, "MSGBUFS") == 0)
            then
              rings[MSGQ].count = atoi((pchar)&str2) ;
          else if (strcmp(str1, "BLKBUFS") == 0)
            then
              rings[BLKQ].count = atoi((pchar)&str2) ;
          else if (strcmp(str1, "RECONFIG") == 0)
            then
              reconfig_on_err = atoi((pchar)&str2) ;
          else if (strcmp(str1, "NETTO") == 0)
            then
              netto = atol((pchar)&str2) ;
          else if (strcmp(str1, "NETDLY") == 0)
            then
              netdly = atol((pchar)&str2) ;
          else if (strcmp(str1, "GRPSIZE") == 0)
            then
              begin
                grpsize = atol((pchar)&str2) ;
                if (grpsize < 1)
                  then
                    grpsize = 1 ;
                if (grpsize > 63)
                  then
                    grpsize = 63 ;
              end
          else if (strcmp(str1, "GRPTIME") == 0)
            then
              grptime = atol((pchar)&str2) ;
          else if (strcmp(str1, "LINKRETRY") == 0)
            then
              link_retry = atol((pchar)&str2) ;

/* SeedLink-specific parameters */
          
          else if (strcmp(str1, "STANDBY") == 0)
            then
              sl_standby = atol((pchar)&str2) ;
          else if (strcmp(str1, "UPTIME") == 0)
            then
              sl_uptime = atol((pchar)&str2) ;
          else if (strcmp(str1, "IFUP") == 0)
            then
              strcpy(sl_ifup, str2) ;
          else if (strcmp(str1, "IFDOWN") == 0)
            then
              strcpy(sl_ifdown, str2) ;
          else if (strcmp(str1, "TIMETABLE_LOADER") == 0)
            then
              strcpy(sl_timetable_loader, str2) ;
          else if (strcmp(str1, "SEQFILE") == 0)
            then
              strcpy(sl_seqfile, str2) ;
          else if (strcmp(str1, "SEQSAVE") == 0)
            then
              sl_seqsave = atol((pchar)&str2) ;
          else if (strcmp(str1, "SELECTORS") == 0)
            then
              strcpy(sl_selectors, str2) ;
          else if (strcmp(str1, "SCHEDULE") == 0)
            then
              strcpy(sl_schedule_str, str2) ;
          else if (strcmp(str1, "NETWORK") == 0)
            then
              begin
                str2[5] = '\0';
                sl_network.l = str_long (str2);
              end
          else if (strcmp(str1, "UNISTATION") == 0)
            then
              sl_unistation = ((str2[0] == 'y') lor (str2[0] == 'Y')) ;
          else if (strcmp(str1, "DIAL_LOCK") == 0)
            then
              strcpy(sl_dlock_file, str2) ;
          else if (strcmp(str1, "STREAM_RENAMING") == 0)
            then
              seedlink_stream_renaming(str2) ;

/* end of SeedLink-specific parameters */

            else
              begin  /* look for client[xx]=name[,timeout] */
                strcpy(stemp, str1) ;
                stemp[6] = '\0' ; /* strip down to "client" only */
                if (strcmp(stemp, "CLIENT") == 0)
                  then
                    begin
                      i = highclient++ ;
                      /* get client name */
                      upshift(str2) ;
                      split (str2, stemp, ',') ;
                      for (j = 0 ; j < 4 ; j ++)
                        if (j < strlen(str2))
                          then
                            clients[i].client_name.b[j] = str2[j] ;
                          else
                            clients[i].client_name.b[j] = ' ' ;
                      /* get timeout, if any */
                      if (stemp[0] != '\0')
                        then
                          begin
                            clients[i].timeout = atol((pchar)&stemp) ;
                            if (clients[i].timeout)
                              then
                                begin
                                  set_bit (&blockmask, i) ;
                                  clients[i].last_service = dtime () ;
                                  clients[i].active = TRUE ;
                                  clients[i].blocking = TRUE ;
                                end
                          end
                    end
                  else
                    begin /* look for uidxxx=nnn */
                      strcpy(stemp, str1) ;
                      stemp[3] = '\0' ; /* strip down to "uid" only */
                      if (strcmp(stemp, "UID") == 0)
                        then
                          begin
                            i = uids++ ;
                            /* get user id */
                            str_right (stemp, &str1[2]) ;
                            user_privilege[i].user_id = atoi((pchar)&stemp) ;
                            /* get privilege mask */
                            user_privilege[i].user_mask = atoi((pchar)&str2) ;
                          end
                    end
              end
        end
      while (1) ;

/* Process the [shear] section, if there is one */
      if (lnot skipto (&cfg, "shear"))
        then
          begin
            linkstat.data_format = CSF_Q512 ;
            linkstat.ultraon = FALSE ;
            do
              begin
                read_cfg (&cfg, str1, str2) ;
                if (str1[0] == '\0')
                  then
                    break ;
                if (strcmp(str1, "NETWORK") == 0)
                  then
                    begin
                      upshift(str2) ;
                      for (i = 0 ; i < 2 ; i++)
                        if (i < strlen(str2))
                          then
                            network[i] = str2[i] ;
                          else
                            network[i] = ' ' ;
                    end
                else if (strcmp(str1, "SEQMOD") == 0)
                  then
                    sequence_mod = atoi(str2) ;
                else if (strcmp(str1, "CALIB") == 0)
                  then
                    begin
                      upshift(str2) ;
                      if (strcmp(str2, "SUPERCAL") == 0)
                        then
                          supercal = TRUE ;
                      else if (strcmp(str2, "QAPCAL") == 0)
                        then
                          qapcal = TRUE ;
                        else
                          terminate ("Invalid calibrator\n") ;
                    end
                else if (strcmp(str1, "VBB") == 0)
                  then
                    strcpy (streams[0], str2) ;
                else if (strcmp(str1, "VSP") == 0)
                  then
                    strcpy (streams[1], str2) ;
                else if (strcmp(str1, "LG") == 0)
                  then
                    strcpy (streams[2], str2) ;
                else if (strcmp(str1, "MP") == 0)
                  then
                    strcpy (streams[3], str2) ;
                else if (strcmp(str1, "LP") == 0)
                  then
                    strcpy (streams[4], str2) ;
                else if (strcmp(str1, "VLP") == 0)
                  then
                    strcpy (streams[5], str2) ;
                else if (strcmp(str1, "ULP") == 0)
                  then
                    strcpy (streams[6], str2) ;
              end
            while (1) ;
            k = 0 ;
            for (i = 0 ; i <= 6 ; i++)
              for (j = 0 ; j <= 19 ; j++)
                begin
                  memset((pchar) &(seed_locs[j][i]), ' ', 2) ; /* Initialize to spaces */
                  parsechan(streams[i], j, &seed, &loc, &l) ;
                  if (seed[0] != '\0')
                    then
                      k++ ;
                end
            ultra_size = k * sizeof(chan_record) + sizeof(ultra_type) + 16 ;
            if (supercal lor qapcal)
              then
                ultra_size = ultra_size + (sizeof(cal_record) - (MAXCAL - 2) * sizeof(eachcal)) ;
            pultra = (ultra_type *) malloc(ultra_size) ;
            /* Clear the whole bugger out, this saves alot of assignments */
            memset ((pchar) pultra, '\0', ultra_size) ;
            pultra->vcovalue = -1 ;
            pultra->umass_ok = supercal lor qapcal ;
            pultra->usedcount = k ;
            pultra->usedoffset = sizeof(ultra_type) ;
            pcr = (chan_record *) ((long) pultra + pultra->usedoffset) ;
            for (i = 0 ; i <= 6 ; i++)
              for (j = 0 ; j <= 19 ; j++)
                begin
                  parsechan(streams[i], j, &seed, &loc, &l) ;
                  if (seed[0] != '\0')
                    then
                      begin
                        memcpy(pcr->seedname, seed, 3) ;
                        memcpy(pcr->seedloc, loc, 2) ;
                        pcr->stream = i ;
                        pcr->physical = l ;
                        pcr->available = 3 ;
                        pcr->enabled = 3 ;
                        pcr->rate = rates[i] ;
                        memcpy((seed_names[j][i]), seed, 3) ;
                        memcpy((seed_locs[j][i]), loc, 2) ;
                        pcr++ ;
                      end
                end
            if (qapcal lor supercal)
              then
                begin
                  pultra->calcount = 2 ;
                  pultra->caloffset = ((long) pcr - (long) pultra + 7) and 0xFFFFFFF8 ;
                  pcal = (cal_record *) ((long) pultra + pultra->caloffset) ;
                  pcal->number = 2 ;
                  pcal->mass_ok = TRUE ;
                  for (i = 0 ; i <= 1 ; i++)
                    begin
                      pec = &(pcal->acal[i]) ;
                      pec->board = i + 1 ;
                      pec->map = 7 << (3 * i) ;
                      if (qapcal)
                        then
                          begin
                            strpas(pec->name, "QAPCAL") ;
                            pec->coupling_option = FALSE ;
                            pec->polarity_option = TRUE ;
                            pec->min_mass_dur = 500 ;
                            pec->max_mass_dur = 500 ;
                            pec->inc_mass_dur = 10 ;
                            pec->def_mass_dur = 500 ;
                            pec->waveforms = 1 << STEP ;
                            pec->min_amp = -6 ;
                            pec->max_amp = -6 ;
                            pec->amp_step = 1 ;
                            pec->durations[STEP].min_dur = 60 ;
                            pec->durations[STEP].max_dur = 14400 ;
                            pec->durations[STEP].inc_dur = 60 ;
                          end
                        else
                          begin
                            strpas(pec->name, "SUPERCAL") ;
                            pec->coupling_option = TRUE ;
                            pec->polarity_option = TRUE ;
                            pec->min_mass_dur = 500 ;
                            pec->max_mass_dur = 500 ;
                            pec->inc_mass_dur = 10 ;
                            pec->def_mass_dur = 500 ;
                            for (j = SINE ; j <= WRAND ; j++)
                              set_bit(&pec->waveforms, j) ;
                            pec->min_amp = -96 ;
                            pec->amp_step = 6 ;
                            pec->durations[STEP].min_dur = 1 ;
                            pec->durations[STEP].max_dur = 14400 ;
                            pec->durations[STEP].inc_dur = 1 ;
                            pec->durations[SINE].min_dur = 60 ;
                            pec->durations[SINE].max_dur = 14400 ;
                            pec->durations[SINE].inc_dur = 60 ;
                            pec->durations[RAND].min_dur = 60 ;
                            pec->durations[RAND].max_dur = 14400 ;
                            pec->durations[RAND].inc_dur = 60 ;
                            pec->durations[WRAND].min_dur = 60 ;
                            pec->durations[WRAND].max_dur = 14400 ;
                            pec->durations[WRAND].inc_dur = 60 ;
                            pec->rand_min_period = 1 ;
                            pec->rand_max_period = 16 ;
                            for (j = Hz8_0000 ; j <= Hz0_0005 ; j++)
                              set_bit(&pec->sine_freqs, j) ;
                          end
                    end
                end ;
            linkstat.ultrarecv = TRUE ;
          end
      close_cfg(&cfg) ;
    end
