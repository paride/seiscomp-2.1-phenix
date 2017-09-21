/*   Client Server Access Library Routines
     Copyright 1994-1996 Quanterra, Inc.
     Written by Woodrow H. Owens

Edit History:
   Ed Date      By  Changes
   -- --------- --- ---------------------------------------------------
    0 14 Mar 94 WHO Derived from client.c test program.
    1 31 Mar 94 WHO Client data & blockettes merged.
    2  7 Apr 94 WHO Interface to servers tightened up.
    3  8 Apr 94 WHO Initialize command output buffers.
    4  9 Apr 94 WHO cs_all renamed to cs_setup and station name added.
    5 18 Apr 94 WHO "stuff.h" added since dtime was moved there from service.h
    6 30 May 94 WHO In cs_svc, set curclient->status to CSCR_DIED if that is
                    the status to be returned (DSN).
    7  9 Jun 94 WHO Cleanup to avoid warnings.
    8  9 Aug 94 WHO Put protection against improbable duplicate detach (DSN).
    9 14 oct 94 jms use "nanosleep" if NANOSLEEP
   10  3 Nov 94 WHO Use SOLARIS2 definition to use nanosleep/sleep instead of usleep.
   11 27 Feb 95 WHO Start of conversion to run on OS9.
   12 29 May 96 WHO Somebody slipped in a change from "getuid" to
                    "geteuid" without updating change list.
   13  7 Jun 96 WHO Don't assume a good return from "kill" is zero,
                    check against -1.
   14  4 Dec 96 WHO Fix sels[CHAN] not being initialized.
   15 11 Jun 97 WHO Fix Solaris2/OSK conditionals for sleeping.e
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
#endif
#include <signal.h>
#include "pascal.h"
#include "dpstruc.h"
#include "service.h"
#include "cfgutil.h"
#include "stuff.h"
#include "pascal.h"
#ifdef _OSK
#include "os9stuff.h"
#endif
#ifdef SOLARIS2
#include <time.h>
#endif

#ifdef __cplusplus
#define this _this
#endif

  static void dummy_handler (int sig)
    begin
    end
  
/* Try to put the structure pointed to by "client" into service queue for
   server pointed to by "srvr". Returns 0 if no error, -1 if cannot
*/
  short cs_svc (pclient_struc client, short station_number)
    begin
      short found, i ;
      int err ;
      long sofar, sleeptime ;
      struct sembuf busy = { 0, -1, SEM_UNDO } ;
      struct sembuf notbusy = { 0, 1, SEM_UNDO } ;
      pclient_station curclient ;
      pserver_struc srvr ;
#ifdef SOLARIS2
      timespec_t rqtp, rmtp ;
#endif

      found = FALSE ;
      client->done = FALSE ;
      client->error = 0 ;
      client->curstation = station_number ;
      curclient = (pclient_station) ((long) client + client->offsets[station_number]) ;
      curclient->last_attempt = dtime () ;
      srvr = curclient->base ;
      if (srvr == (pserver_struc) NOCLIENT)
        then
          begin
            curclient->status = CSCR_DIED ;
            return CSCR_DIED ;
          end
      if (srvr->init != 'I')
        then
          begin
#ifndef _OSK
            sleep (srvr->client_wait) ;
#else
            tsleep (0x80000000 + (srvr->client_wait * 256)) ;
#endif
            if (srvr->init != 'I')
              then
                return CSCR_INIT ;
          end
#ifndef _OSK
      client->client_uid = geteuid () ;
#else
      client->client_uid = getuid () ;
#endif

/* Try to find free service request buffer, logic is stolen from digitizer server */
      for (i = 0 ; i <= MAXCLIENTS ; i++)
        if ((srvr->svcreqs[i].clientseg == NOCLIENT) land
           ((srvr->svcreqs[i].clientname.l == 0) or
            (srvr->svcreqs[i].clientname.l == client->myname.l)))
          then
            begin
              semop (srvr->server_semid, &busy, 1) ;
              if (srvr->svcreqs[i].clientseg == NOCLIENT)
                then
                  begin
                    srvr->svcreqs[i].clientseg = client->client_shm ;
                    semop (srvr->server_semid, &notbusy, 1) ; /* now in queue */
                    found = TRUE ;
                    break ;
                  end
                else
                  semop (srvr->server_semid, &notbusy, 1) ; /* Somebody filled my slot, try another */
            end
      if (lnot found)
        then
          return CSCR_ENQUEUE ;
      if (srvr->server_uid == client->client_uid)
        then
          begin
#ifdef _OSK
            if (kill (srvr->server_pid, SIGWAKE) == ERROR) /* get its attention */
#else
            if (kill (srvr->server_pid, SIGALRM) == ERROR) /* get its attention */
#endif
              then
                begin
                  srvr->svcreqs[i].clientseg = NOCLIENT ;
                  curclient->base = (pserver_struc) NOCLIENT ;
                  curclient->status = CSCR_DIED ;
                  return CSCR_DIED ;
                end
            sleeptime = srvr->privusec ;
          end
        else
          begin
            /*if (kill (srvr->server_pid, 0))
              then
                begin
                  srvr->svcreqs[i].clientseg = NOCLIENT ;
                  curclient->base = NOCLIENT ;
                  curclient->status = CSCR_DIED ;
                  return CSCR_DIED ;
                end */
            sleeptime = srvr->nonusec ;
          end
      sofar = 0 ;
      while ((lnot client->done) land (sofar <= srvr->client_wait))
        begin
#ifdef SOLARIS2
          if (sleeptime >= 1000000)
            then
              sleep (sleeptime / 1000000) ;
            else
              begin
                rqtp.tv_sec = 0 ;
                rqtp.tv_nsec = 1000 * sleeptime ;
                err = nanosleep (&rqtp, &rmtp) ;
              end
#elif defined _OSK
          if (sleeptime < 11000)
            then
              tsleep (1) ;
            else
              tsleep (0x80000000 or (sleeptime / 3906)) ;
#else
          usleep (sleeptime) ;
#endif
          sofar = sofar + sleeptime ;
        end
      if (lnot client->done)
        then
          begin
            semop (srvr->server_semid, &busy, 1) ;
            if (srvr->svcreqs[i].clientseg == client->client_shm) /* still valid */
              then
                srvr->svcreqs[i].clientseg = NOCLIENT ;
            semop (srvr->server_semid, &notbusy, 1) ;
            return CSCR_TIMEOUT ;
          end
      curclient->last_good = dtime () ;
      return client->error ;
    end          

  void cs_setup (pstations_struc stations, pchar name, pchar sname, boolean shared, 
      boolean blocking, short databufs, short sels, short mask, long comsize)
    begin
      int segkey ;
      short i, j ;
      boolean any ;
      config_struc cfg ;
      char str1[CFGWIDTH] ;
      char str2[CFGWIDTH] ;
      char stemp[CFGWIDTH] ;
      char source[SECWIDTH] ;
      char filename[CFGWIDTH] ;

      if (comsize < 100)
        then
          comsize = 100 ;
      any = sname[0] == '*' ;
/* Initialize the header */ 
      stations->myname.l = str_long(name) ;       
      stations->shared = shared ;
      stations->station_count = 0 ;
      stations->data_buffers = databufs ;

/* open the stations list and look for any station */
#ifdef _OSK
      strcpy (filename, "/r0/stations.ini") ;
#else
      strcpy (filename, "/etc/stations.ini") ;
#endif
      if (open_cfg(&cfg, filename, sname))
        then
          begin
            close_cfg (&cfg) ; /* no stations */
            return ;
          end

/* outer loop looks for stations, just stores directory for now */
      do
        begin
          strcpy(stemp, &cfg.lastread[1]) ;
          stemp[strlen(stemp)-1] = '\0' ; /* remove [   ] */
          upshift(stemp) ;
          source[0] = '\0' ;

/* Try to find the station directory, source, and description */
          do
            begin
              read_cfg(&cfg, str1, str2) ;
              if (str1[0] == '\0')
                then
                  break ;
              if (strcmp(str1, "DIR") == 0)
                then
                  strcpy(filename, str2) ;
              else if (strcmp(str1, "SOURCE") == 0)
                then
                  strcpy(source, str2) ;
            end
          while (1) ;
          j = stations->station_count++ ; /* new station */
          stations->station_list[j].stationname.l = str_long(stemp) ;
          strncpy (stations->station_list[j].source, source, 19) ;
          stations->station_list[j].source[19] = 0 ;
          strncpy (stations->station_list[j].directory, filename, 119) ;
          stations->station_list[j].directory[119] = 0 ;
          stations->station_list[j].comoutsize = comsize ;
          stations->station_list[j].selectors = sels ;
          stations->station_list[j].mask = mask ;
          stations->station_list[j].segkey = NOCLIENT ;
          stations->station_list[j].blocking = blocking ;
        end
      while (any land (lnot skipto (&cfg, sname))) ;
      close_cfg(&cfg) ;
      
      for (j = 0 ; j < stations->station_count ; j++)
        begin
/* Try to open the station.ini file in this station's directory */
          strcpy (filename, stations->station_list[j].directory) ;
          addslash (filename) ;
          strcat (filename, "station.ini") ;
          if (lnot open_cfg(&cfg, filename, stations->station_list[j].source))
            then
              begin
                do
                  begin
                    read_cfg(&cfg, str1, str2) ;
                    if (str1[0] == '\0')
                      then
                        break ;
                    else if (strcmp(str1, "SEGID") == 0)
                      then
                        stations->station_list[j].segkey = atoi((pchar) &str2) ;
                  end
                while (1) ;
                close_cfg(&cfg) ;
              end
        end
    end

  void cs_remove (pstations_struc stations, short num)
    begin
      short i ;
      
      for (i = num ; i < stations->station_count - 1 ; i++)
        stations->station_list[i] = stations->station_list[i + 1] ;
      stations->station_count-- ;
    end

/* try to link to server's shared memory segment */
  void cs_link (pclient_struc client, short station_number, boolean first)
    begin
      int shmid ;
      pclient_station curclient ;
      pserver_struc srvr ;
      
      curclient = (pclient_station) ((long) client + client->offsets[station_number]) ;
      shmid = shmget(curclient->seg_key, sizeof(tserver_struc), PERM) ;
      if (shmid == ERROR)
        then
          begin
            curclient->base = (pserver_struc) NOCLIENT ;
            curclient->status = CSCR_DIED ;
          end
        else
          begin
            curclient->base = (pserver_struc) shmat(shmid, NULL, 0) ;
            if (curclient->base == (pserver_struc) ERROR)
              then
                curclient->status = CSCR_DIED ;
              else
                begin
                  srvr = curclient->base ;
                  if ((client->client_uid == srvr->server_uid) 
#ifdef _OSK
                      land (kill(srvr->server_pid, SIGWAKE) == ERROR))
#else
                      land (kill(srvr->server_pid, 0) == ERROR))
#endif
                    then
                      begin
                        curclient->status = CSCR_DIED ;
                        shmdt ((pchar)srvr) ;
                        curclient->base = (pserver_struc) NOCLIENT ;
                      end
                    else
                      begin
                        if (first)
                          then
                            curclient->servcode = srvr->servcode ;
                        curclient->status = CSCR_GOOD ;
                      end
                end
          end
    end

/* Try to do CSCM_ATTACH command */
  void cs_attach (pclient_struc client, short station_number)
    begin
      pclient_station curclient ;
      
      curclient = (pclient_station) ((long) client + client->offsets[station_number]) ;
      if (curclient->base == (pserver_struc) ERROR)
        then
          curclient->status = CSCR_DIED ;
        else
          begin
            curclient->command = CSCM_ATTACH ;
            curclient->status = cs_svc (client, station_number) ;
          end
    end

  pclient_struc cs_gen (pstations_struc stations)
    begin
      pserver_struc base ;
      pclient_struc me ;
      pclient_station this ;
      int shmid, myshm ;
      int val, found, i, j, k ;
      long min, max ;
      short err ;
      long total, dsize, curoff, inoff, outoff, doff ;
      seltype *psel ;
      comstat_rec *pcr ;

#ifdef _OSK
      signal (SIGALRM, SIG_IGN) ;
#else
      struct sigaction sa ;
      
      sa.sa_handler = dummy_handler ;
      sa.sa_flags = SA_RESTART ;
      sigemptyset(&sa.sa_mask) ;
      sigaction(SIGALRM, &sa, NULL) ;
#endif
      
/* Figure out how large it needs to be for all stations, all values are double word aligned */  
      total = (sizeof(tclient_struc) + 7) and 0xFFFFFFF8 ;
      dsize = (sizeof(tdata_user) + 7) and 0xFFFFFFF8 ;
      total = total + (dsize * stations->data_buffers) ;
      for (j = 0 ; j < stations->station_count ; j++)
        begin
          total = total + ((sizeof(tclient_station) + 7) and 0xFFFFFFF8) ;
          if ((j == 0) lor (lnot stations->shared) land (stations->station_list[j].comoutsize))
            then
              total = total + ((stations->station_list[j].comoutsize) + 115) and 0xFFFFFFF8 ;
          total = total + ((sizeof(seltype) * stations->station_list[j].selectors) + 7) and 0xFFFFFFF8 ;
        end
 
/* Try to create the shared memory segment */
      myshm = shmget(IPC_PRIVATE, total, IPC_CREAT or PERM) ;
      if (myshm == ERROR)
        then
          return (pclient_struc) CSCR_PRIVATE ;
      me = (pclient_struc) shmat(myshm, NULL, 0) ;
      if (me == (pclient_struc) ERROR)
        then
          return (pclient_struc) CSCR_PRIVATE ;

/* Start filling global fields */
      me->myname.l = stations->myname.l ;
      me->client_pid = getpid () ;
      me->client_shm = myshm ;
      me->spare = 0 ;
      me->done = FALSE ;
      me->error = 0 ;
      me->maxstation = stations->station_count ;
      me->curstation = 0 ;
      for (j = 0 ; j < MAXSTATIONS ; j++)
        me->offsets[j] = 0 ;
      curoff = (sizeof(tclient_struc) + 7) and 0xFFFFFFF8 ;
 
/* If shared command output buffer, allocate it at the beginning */
      if (stations->shared)
        then
          begin
            inoff = curoff ;
            curoff = curoff + 104 ;
            outoff = curoff ;
            curoff = curoff + ((stations->station_list[0].comoutsize + 7) and 0xFFFFFFF8) ;
          end

/* Data buffers are always shared */
      doff = curoff ;
      curoff = curoff + (dsize * stations->data_buffers) ;
     
/* For each station, build entry */
      for (j = 0 ; j < stations->station_count ; j++)
        begin
          this = (pclient_station) ((long) me + curoff) ; 
          curoff = curoff + ((sizeof(tclient_station) + 7) and 0xFFFFFFF8) ;
          me->offsets[j] = (long) this - (long) me ;
          this->name.l = stations->station_list[j].stationname.l ;
          this->seg_key = stations->station_list[j].segkey ;
          this->command = CSCM_ATTACH ;
          this->blocking = stations->station_list[j].blocking ;
          this->status = OK ;
          this->next_data = 0 ;
          this->last_attempt = 0.0 ;
          this->last_good = 0.0 ;
          this->servcode = 0.0 ;
          this->base = (pserver_struc) NOCLIENT ;
          if (lnot stations->shared)
            then
              begin
                inoff = curoff ;
                curoff = curoff + 104 ;
                outoff = curoff ;
                curoff = curoff + ((stations->station_list[j].comoutsize + 7) and 0xFFFFFFF8) ;
              end
          pcr = (comstat_rec *) ((long) me + outoff) ;
          pcr->command_tag = 0 ;
          pcr->completion_status = CSCS_IDLE ;
          this->cominoffset = inoff ;
          this->comoutoffset = outoff ;
          this->comoutsize = stations->station_list[j].comoutsize ;
          this->dbufoffset = doff ;
          this->dbufsize = dsize ;
          this->maxdbuf = stations->data_buffers ;
          this->reqdbuf = this->maxdbuf ;
          this->valdbuf = 0 ;
          this->seqdbuf = CSQ_FIRST ;
          this->startdbuf = 0.0 ;
          this->seloffset = curoff ;
          psel = (seltype *) ((long) me + curoff) ;
          curoff = curoff + ((sizeof(seltype) * stations->station_list[j].selectors) + 7) and 0xFFFFFFF8 ;
          this->maxsel = stations->station_list[j].selectors ;
          for (k = DATAQ ; k <= CHAN ; k++)
            begin
              this->sels[k].first = 0 ;
              this->sels[k].last = 0 ;
            end
          this->datamask = stations->station_list[j].mask ;
          strcpy(psel[0], "?????") ;

/* now attach to the server's shared memory segment */
          cs_link (me, j, TRUE) ;
          if (this->base != (pserver_struc) NOCLIENT)
            then
              cs_attach (me, j) ;
        end
      return me ;
    end

  byte cs_check (pclient_struc client, short station_number, double now)
    begin
      pclient_station curclient ;
      pserver_struc srvr ;
      
      curclient = (pclient_station) ((long) client + client->offsets[station_number]) ;
      if (curclient->status != CSCR_GOOD)
        then
          begin
            if ((curclient->last_attempt + 10) < now)
              then
                begin
                  curclient->last_attempt = now ;
                  if (curclient->base == (pserver_struc) NOCLIENT)
                    then
                      cs_link (client, station_number, FALSE) ;
                  if (curclient->base != (pserver_struc) NOCLIENT)
                    then
                      cs_attach (client, station_number) ;
                end
          end
      if ((curclient->status == CSCR_GOOD) land (curclient->base != (pserver_struc) NOCLIENT))
        then
          begin
            srvr = curclient->base ;
            if (curclient->servcode != srvr->servcode)
              then
                begin
                  curclient->servcode = srvr->servcode ;
                  curclient->next_data = 0 ;
                  curclient->seqdbuf = CSQ_FIRST ;
                  cs_attach (client, station_number) ;
                  curclient->status = CSCR_CHANGE ;
                end
              else
                begin
                  if (curclient->next_data < srvr->next_data)
                    then
                      return CSCR_GOOD ;
                  if ((curclient->last_attempt + 10) < now)
                    then
                      begin
                        curclient->last_attempt = now ;
                        if ((client->client_uid == srvr->server_uid) 
#ifdef _OSK
                           land (kill(srvr->server_pid, SIGWAKE) == ERROR))
#else
                           land (kill(srvr->server_pid, 0) == ERROR))
#endif
                          then
                            begin
                              curclient->status = CSCR_DIED ;
                              shmdt ((pchar)srvr) ;
                              curclient->base = (pserver_struc) NOCLIENT ;
                            end
                          else
                            return CSCR_GOOD ;
                      end
                    else
                      return CSCR_NODATA ;
                end
          end
      return curclient->status ;
    end

  void cs_detach (pclient_struc client, short station_number)
    begin
      pclient_station this ;
      
      this = (pclient_station) ((long) client + client->offsets[station_number]) ;
/* Detach from server segment */
      if (lnot (this->base == (pserver_struc) NOCLIENT))
        then
          shmdt((pchar)this->base) ;
      this->status = CSCR_DIED ;
      this->base = (pserver_struc) NOCLIENT ;
    end

  void cs_off (pclient_struc client)
    begin
      short j ;
      int seg ;

/* Detach from all servers */      
      for (j = 0 ; j < client->maxstation ; j++)
        cs_detach (client, j) ;

/* Detach from my segment */
      seg = client->client_shm ;
      shmdt((pchar)client) ;

/* Delete my segment */
      shmctl(seg, IPC_RMID, NULL) ;
    end

  short cs_scan (pclient_struc client, boolean *alert)
    begin
      short j ;
      byte old_status, result ;
      short old_station ;
      pclient_station this ;
      double curtime ;
      
      old_station = client->curstation ;
      curtime = dtime () ;
      *alert = FALSE ;
      do
        begin
          if (++client->curstation >= client->maxstation)
            then
              client->curstation = 0 ;
          this = (pclient_station) ((long) client + client->offsets[client->curstation]) ;
          old_status = this->status ;
          result = cs_check (client, client->curstation, curtime) ;
          *alert = (this->status != old_status) ;
          this->valdbuf = 0 ;
          if (result == CSCR_GOOD lor (result == CSCR_NODATA land this->reqdbuf == 0))
            then
              begin
                this->command = CSCM_DATA_BLK ;
                old_status = this->status ;
                this->status = cs_svc (client, client->curstation) ;
                *alert = *alert lor (this->status != old_status) ;
                if (*alert lor (this->valdbuf != 0))
                  then
                    return client->curstation ;
              end
          if (*alert)
            then
              return client->curstation ;
        end
      while (client->curstation != old_station) ;
      return NOCLIENT ;      
    end

