/*
Edit History:
   Ed Date      By  Changes
   -- --------- --- ---------------------------------------------------
    0 13 MAR 97 W.R First created from msgmon

*/

#include <stdio.h>
#include <errno.h>
#include <termio.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#if 0
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>
#endif
#include <sys/time.h>
#include <signal.h>
#include "dpstruc.h"
#if 0
#include "seedstrc.h"
#endif
#include "stuff.h"
#include "timeutil.h"
#include "service.h"

char *network_ini  = "/etc/network.ini";
char *stations_ini = "/etc/stations.ini";
char name[9]  = "TAPE" ;
char sname[5] = "RAND" ;
tstations_struc stations ;
int iJoinedPath = FALSE; /* for active file compare function in FileFind */
int fdLog,
    fdDLog;
FILE *fpErLog;

typedef char char23[24] ;

char23 stats[13] = {    "Good", "Enqueue Timeout", 
                        "Service Timeout", "Init Error",
                        "Attach Refused", "No Data", 
                        "Server Busy", "Invalid Command",
                        "Server Dead", "Server Changed", "Segment Error",
                        "Command Size", "Privileged Command" } ;

extern void MvLogAndEject( void );

void    FinishHandler   ( int );
void    CleanUpHandler  ( int );
int     TerminateProgram( int );
int     DumpMainLoop    ( int* );

static  int terminate_proc;
static  int CleanUpAndExit;
static  pclient_struc   me ;
static  int dump_init = FALSE;

int main (int argc, char *argv[]) {
pclient_station this ;
short       j;
boolean     alert ;
double      ctime ;
#if 0
short       k;
pdata_user  pdat ;
pchar       pc1, pc2 ;
seed_record_header *pseed;
#endif

    /* Allow override of station name on command line */
    if(argc >= 2) {
        strncpy(sname, argv[1], 4) ;
        sname[4] = '\0' ;
    }
    upshift(name) ;
    upshift(sname) ;

    /* Generate an entry for all available stations */      
    cs_setup (&stations, name, sname, TRUE, TRUE, 10, 1, CSIM_MSG, 0) ;

    if(stations.station_count == 0) {
        printf ("Station not found\n") ;
    return 0;
    }
 
    /* Create my segment and attach to the selected station */      
    terminate_proc = 0;
    signal(SIGINT,FinishHandler);
    signal(SIGTERM,FinishHandler);
    CleanUpAndExit = 0;
    signal(SIGHUP, CleanUpHandler );
    
    me = cs_gen (&stations) ;
    ctime = dtime () ;

    for (j = 0 ; j < me->maxstation ; j++) {
        this = (pclient_station) ((long) me + me->offsets[j]) ;
        this->startdbuf = ctime ;
    }

    
    /* Try to get message from station, if none, wait 1 second */
    do {
        j = cs_scan (me, &alert) ;
        if(j != NOCLIENT) {
            this = (pclient_station) ((long) me + me->offsets[j]) ;
        if(alert)
            printf("New status on station %s is %s\n", 
                                        (char*)long_str(this->name.l),
                                        (char*)&(stats[this->status])) ;
#if 0
          if(this->valdbuf) {
               pdat = (pdata_user) ((long) me + this->dbufoffset) ;
               for (k = 0 ; k < this->valdbuf ; k++) {
                    pseed = (pvoid) &pdat->data_bytes ;
                    pc1 = (pchar) ((long) pseed + pseed->first_data_byte) ;
                    pc2 = (pchar) ((long) pc1 + pseed->samples_in_record - 2) ;
                   *pc2 = '\0' ;
                   /* don't log mesaages */
                   /*
                   printf ("%s\n", pc1) ;
                   */
                   pdat = (pdata_user) ((long) pdat + this->dbufsize) ;
                }
            }
#endif
        } else {       
            DumpMainLoop( &dump_init );
        }

#ifdef EJECT
        if( CleanUpAndExit )
            MvLogAndEject();
#endif
            
    } while ( !terminate_proc && !CleanUpAndExit ) ;
    TerminateProgram(0);
    return 0;
}

/************************************************************************/
/*  FinishHandler                                                       */
/************************************************************************/
void FinishHandler(int sig)
{
    signal (sig,FinishHandler);    /* Re-install handler (for SVR4)     */
    terminate_proc = 1;
}

/************************************************************************/
/*  HUP FinishHandler                                                   */
/************************************************************************/
void CleanUpHandler( int sig ) {
    signal (sig, CleanUpHandler);
    CleanUpAndExit = 1;
}

#ifndef TIMESTRLEN
#define TIMESTRLEN  40
#endif

/************************************************************************/
/*  TerminateProgram                                                    */
/*      Terminate prog and return error code.  Clean up on the way out. */
/************************************************************************/
int TerminateProgram (int error) 
{
    pclient_station     this ;
    char                time_str[TIMESTRLEN];
    int                 j;
    boolean             alert ;

    strcpy(time_str, localtime_string(dtime()));
    printf ("%s - Terminating program.\n", time_str);
    fflush (stdout);

    /* Perform final cs_scan for 0 records to ack previous records.     */
    /* Detach from all stations and delete my segment.                  */
    if (me != NULL) {
        for (j=0; j< me->maxstation; j++) {
            this = (pclient_station) ((long) me + me->offsets[0]) ;
            this->reqdbuf = 0;
        }
        strcpy(time_str, localtime_string(dtime()));
        printf ("%s - Final scan to ack all received packets\n", time_str);
        fflush (stdout);
        cs_scan (me, &alert);
        cs_off (me) ;
    }
    strcpy(time_str, localtime_string(dtime()));
    printf ("%s - Terminated\n", time_str);
    exit(error);
}
