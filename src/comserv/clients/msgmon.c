/*   Shows messages from the station on the command line
     Copyright 1994-1996 Quanterra, Inc.
     Written by Woodrow H. Owens

Edit History:
   Ed Date      By  Changes
   -- --------- --- ---------------------------------------------------
    0  8 Apr 94 WHO First created from test.c
    1 17 Apr 94 WHO Added options to use CSQ_LAST and CSQ_TIME to be able
                    to check those out in the server.
    2 30 May 94 WHO TIME option changed for new method in comserv.
    3  6 Jun 94 WHO Two new command status strings added.
    4  9 Jun 94 WHO Cleanup to avoid warnings.
    5  7 Jun 96 WHO Start of conversion to run on OS9.
    6 22 Jul 96 WHO Fix offset calculation in final scan.
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
#include "service.h"
#include "pascal.h"
#ifdef _OSK
#include "os9stuff.h"
#endif

#ifdef __cplusplus
#define this _this
#endif

char name[5] = "MSGM" ;
char sname[5] = "RAND" ;
tstations_struc stations ;

typedef char char23[24] ;

char23 stats[13] = { "Good", "Enqueue Timeout", "Service Timeout", "Init Error",
                       "Attach Refused", "No Data", "Server Busy", "Invalid Command",
                       "Server Dead", "Server Changed", "Segment Error",
                       "Command Size", "Privileged Command" } ;

void finish_handler(int sig) ;
int terminate_program (int error) ;
static int terminate_proc ;
static pclient_struc me ;
static int verbosity ; /* defaults to zero? */

  int main (int argc, char *argv[], char **envp)
    begin
      pclient_station this ;
      short j, k, err ;
      boolean alert ;
      pdata_user pdat ;
      seed_record_header *pseed ;
      pchar pc1, pc2 ;
      double ctime ;
      int sel ;
      char s1[200] ;

/* Allow override of station name on command line */
      if (argc >= 2)
        then
          begin
            strncpy(sname, argv[1], 4) ;
            sname[4] = '\0' ;
          end
      upshift(name) ;
      upshift(sname) ;

/* Generate an entry for all available stations */      
      cs_setup (&stations, name, sname, TRUE, TRUE, 10, 1, CSIM_MSG, 0) ;

      if (stations.station_count == 0)
        then
          begin
            printf ("Station not found\n") ;
            return 0;
          end
 
/* Create my segment and attach to the selected station */      
      terminate_proc = 0;
      signal (SIGINT,finish_handler);
      signal (SIGTERM,finish_handler);
      me = cs_gen (&stations) ;

/* Get startup options */
      printf ("Startup option (0 = First, 1 = Last, 2 = At Time) : ") ;
      gets (s1) ;
      sscanf (s1, "%i", &sel) ;
      switch (sel)
        begin
          case 0 : break ; /* this is default setup */
          case 1 :
            begin
              for (j = 0 ; j < me->maxstation ; j++)
                begin
                  this = (pclient_station) ((long) me + me->offsets[j]) ;
                  this->seqdbuf = CSQ_LAST ;
                end
              break ;
            end
          case 2 :
            begin
              ctime = dtime () ;
              printf ("Number of seconds previous to current time to start : ") ;
              gets(s1) ;
              sscanf (s1, "%i", &sel) ;
              if (sel < 0)
                then
                  sel = 0 ;
              ctime = ctime - sel ;
              for (j = 0 ; j < me->maxstation ; j++)
                begin
                  this = (pclient_station) ((long) me + me->offsets[j]) ;
                  this->startdbuf = ctime ;
                end
              break ;
            end
        end
    
/* Try to get message from station, if none, wait 1 second */
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
                           (pchar) stats[this->status]) ;
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
                          pc1 = (pchar) ((long) pseed + pseed->first_data_byte) ;
                          pc2 = (pchar) ((long) pc1 + pseed->samples_in_record - 2) ;
                          *pc2 = '\0' ;
                          printf ("%s\n", pc1) ;
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
      while (lnot terminate_proc) ;
      terminate_program(0);
    end

  void finish_handler(int sig)
    begin
      signal (sig,finish_handler) ;    /* Re-install handler (for SVR4) */
      terminate_proc = 1 ;
    end

#ifndef TIMESTRLEN
#define TIMESTRLEN  40
#endif
/************************************************************************/
/*  terminate_program       */
/* Terminate prog and return error code.  Clean up on the way out. */
/************************************************************************/
  int terminate_program (int error) 
    begin
      pclient_station this ;
      char time_str[TIMESTRLEN] ;
      int j ;
      boolean alert ;
  
      strcpy(time_str, localtime_string(dtime())) ;
      if (verbosity and 2)
        then
          begin
            printf ("%s - Terminating program.\n", time_str) ;
            fflush (stdout) ;
          end
  
      /* Perform final cs_scan for 0 records to ack previous records. */
      /* Detach from all stations and delete my segment.   */
      if (me != NULL)
        then
          begin
            for (j=0; j < me->maxstation; j++)
              begin
                this = (pclient_station) ((long) me + me->offsets[j]) ;
                this->reqdbuf = 0 ;
              end
            if (verbosity)
              then
                begin
                  strcpy(time_str, localtime_string(dtime())) ;
                  printf ("%s - Final scan to ack all received packets\n", time_str) ;
                end
            fflush (stdout) ;
            cs_scan (me, &alert) ;
            cs_off (me) ;
          end
  
      if (verbosity)
        then
          begin
            strcpy(time_str, localtime_string(dtime())) ;
            printf ("%s - Terminated\n", time_str) ;
          end
      exit(error) ;
    end
