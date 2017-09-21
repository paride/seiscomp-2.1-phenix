/*   Server Main file
     Copyright 1994-1998 Quanterra, Inc.
     Written by Woodrow H. Owens

Edit History:
   Ed Date      By  Changes
   -- --------- --- ---------------------------------------------------
    0 11 Mar 94 WHO First created.
    1 23 Mar 94 WHO First testing done, start splitting code of into modules.
    2 27 Mar 94 WHO Merged blockette ring changed to separate rings.
    3  7 Apr 94 WHO Individual fields for link statistics removed in favor
                    linkstat_rec. 
    4 16 Apr 94 WHO Define seed format and extension level. Show station
                    description.
    5 30 May 94 WHO Add global comm_mask variable.
    6  6 Jun 94 WHO client_wait field changed to microseconds. add setting
                    of privusec and nonusec fields. Don't send SIGALRM if
                    not same UID as client. Add user privilege variables
                    (handled by cscfg and commands).
    7  9 Jun 94 WHO Add support to remove foreign clients after timeout.
                    Cleanup to avoid warnings.
    8 16 Jun 94 WHO Fix calculation of foreign_to in procedure check_clients.
    9  9 Aug 94 WHO Fix initialization of linkstat record.
   10 11 Aug 94 WHO Add network support.
   11 18 Aug 94 WHO Fix = instead of == in detach_client.
   12 25 Sep 94 WHO Add REUSEADDR and KEEPALIVE options to socket.
   13  3 Nov 94 WHO Add SOLARIS2 definition to use nanosleep call instead of usleep.
                    SOLARIS2 also makes slight syntax differences in socket calls.
   14 13 Dec 94 WHO Add size of data only blockette to ring buffers allocations.
                    Change SEED version extension from A to B.
   15 14 Jun 95 WHO Add ignoring the SIGPIPE signal so program doesn't terminate when
                    the socket is disconnected.
   16 20 Jun 95 WHO Process netto and netdly parameters. Allows giving up on network
                    connection and trying again. Check for timeout of ack grouping.
   17  2 Oct 95 WHO Don't do automatic 60 second link_req polling if sync set.
   18  8 Oct 95 WHO Allow enabling RTS/CTS flow control.
   19  2 Jun 96 WHO Start of conversion to run on OS9. Don't compare
                    the result of "kill" with zero, use ERROR.
   20 10 Jun 96 WHO Simplify file upload, just send packet every second.
   21 19 Jun 96 WHO Allocate cal ring buffer room based on random_calibration
                    instead of sine_calibration, which is 4 bytes shorter.
   22 30 Jun 96 WHO Don't set active flag to FALSE in detach_client if this
                    is a reserved client that hasn't timed out yet, or else
                    the timeout will never occur.
   23  4 Aug 96 WHO If noultra flag is on, don't poll for ultra packet.
   24  3 Dec 96 WHO Add support for Blockette Records.
   25  7 Dec 96 WHO Add support for UDP.
   26 16 Oct 97 WHO Change in cscfg. Add reporting of all module versions.
   27 22 Oct 97 WHO Add vopttable access for OS9 version.
   28 29 Nov 97 DSN Added optional lockfile directive again.
   29  8 Jan 98 WHO lockfile stuff doesn't apply to OS9 version.
   30 23 Jan 98 WHO Add link_retry variable to use with linkpoll.
*/
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <ctype.h>
#ifndef _OSK
#include <unistd.h>
#include <time.h>
#include <termio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
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
#include "lockutil.h"
#include "schedule.h"
#include "server.h"
#include "pascal.h"
#ifdef _OSK
#include "os9stuff.h"
#endif

#ifdef SOLARIS2
     typedef struct {
          time_t        tv_sec; /* seconds */
          long          tv_nsec;/* and nanoseconds */
     } timespec ;
#endif

#if defined(__GNU_LIBRARY__) && !defined(_SEM_SEMUN_UNDEFINED)
/* union semun is defined by including <sys/sem.h> */
#else
/* according to X/OPEN we have to define it ourselves */
union semun {
        int val;                    /* value for SETVAL */
        struct semid_ds *buf;       /* buffer for IPC_STAT, IPC_SET */
        unsigned short int *array;  /* array for GETALL, SETALL */
        struct seminfo *__buf;      /* buffer for IPC_INFO */
};
#endif

#define MAXPROC 2 /* maximum number of service requests to process before checking serial port */
#define MAXWAIT 10 /* maximum number of seconds for clients to wait */
#define PRIVILEGED_WAIT 1000000 /* 1 second */
#define NON_PRIVILEGED_WAIT 100000 /* 0.1 second */
#define NON_PRIVILEGED_TO 60.0
#define EDITION 30

char seedformat[4] = { 'V', '2', '.', '3' } ;
char seedext = 'B' ;
seed_net_type network = {' ', ' '} ;
complong station ;
 
tring rings[NUMQ] ;                /* Access structure for rings */

pserver_struc base = NULL ;        /* Base address of server memory segment */
pclient_struc cursvc = NULL ;      /* Current client being processed */
pclient_station curclient = NULL ; /* Offset into client's memory for that station */

config_struc cfg ;
tuser_privilege user_privilege ;
char str1[CFGWIDTH] ;
char str2[CFGWIDTH] ;
char stemp[CFGWIDTH] ;
char port[SECWIDTH] = "" ;    /* assume not an serial port */
char ipport [SECWIDTH] = "" ;
char ipaddr [SECWIDTH] = "" ;
#ifndef _OSK
char lockfile[CFGWIDTH] = "" ;
#endif
long baud = 19200 ;
char parity = 'N' ; 
long polltime = 50000 ;
long blockmask = 0 ;
long noackmask = 0 ;
long oldackmask = 0 ;
long comm_mask = 0 ;
long grpsize = 1 ;
long grptime = 5 ;
long link_retry = 10 ;
int segkey = 0 ;
int upmemid = NOCLIENT ;
tclients clients[MAXCLIENTS] ;
byte inphase = SOHWAIT ;
byte upphase = UP_IDLE ;
short highclient = 0 ;
short resclient = 0 ;
short uids = 0 ;
short txwin = 0 ;
int path = -1 ;
int sockfd = -1 ;
struct sockaddr_in cli_addr, serv_addr ;
      
typedef struct sockaddr *psockaddr ;

/* These pointers are used to access the input source buffer "sbuf"
   and the destination buffer, which is "dbuf"
*/
pchar src = NULL ;
pchar srcend = NULL ;
pchar dest = NULL ;
pchar destend = NULL ;
pchar term = NULL ;
unsigned char sbuf[BLOB] ;
DA_to_DP_buffer dbuf ;

byte last_packet_received = 0 ;
byte lastchar = NUL ;

DP_to_DA_buffer temp_pkt ;
tcrc_table crctable ;

long seq = 0 ;
long start_time = 0 ;      /* For seconds in operation */
boolean verbose = TRUE ;   /* normal, client on/off etc. */
boolean rambling = FALSE ; /* Incoming packets display a line */
boolean insane = FALSE ;   /* Client commands are shown */
boolean override = FALSE ; /* Override station/component */
boolean detavail_ok = FALSE ;
boolean detavail_loaded = FALSE ;
boolean first = TRUE ;
boolean firstpacket = TRUE ;
boolean seq_valid = FALSE ;
boolean xfer_down_ok = FALSE ;
boolean xfer_up_ok = FALSE ;
boolean map_wait = FALSE ;
boolean ultra_seg_empty = TRUE ;
boolean stop = FALSE ;
boolean follow_up = FALSE ;
boolean serial = FALSE ;         /* serial port not enabled */
boolean notify = FALSE ;        /* Ultra_req polling not disabled */
boolean flow = FALSE ;
boolean udplink = FALSE ;
boolean noultra = FALSE ;
short combusy = NOCLIENT ;           /* <>NOCLIENT if processing a command for a station */
short ultra_percent = 0 ;
unsigned short lowest_seq = 300 ;
short linkpoll = 0 ;
long con_seq = 0 ;
long netto = 120 ; /* network timeout */
long netdly = 30 ; /* network reconnect delay */
long netto_cnt = 0 ; /* timeout counter (seconds) */
long netdly_cnt = 0 ; /* reconnect delay */
byte cmd_seq = 1 ;
link_record curlink = { 0, 0, 0, 0, 0, 0, CSF_QSL, 0, 0, 0, 0, 0, 0, 0, 0, 0 } ;
linkstat_rec linkstat = { TRUE, FALSE, FALSE, FALSE, 0, 0, 0, 0, 0, 0, 0, 0, 0.0, 0.0, "", 'A', CSF_QSL, "" } ;   /* seedformat will be set to "V2.3" in main() */
string59 xfer_destination = "" ;
string59 xfer_source = "" ;
char station_desc[CFGWIDTH] = "" ;
byte ultra_seg[14] ;
byte detavail_seg[14] ;
seg_map_type xfer_seg ;
unsigned short seg_size = 0 ;
unsigned short xfer_size = 0 ;
unsigned short xfer_up_curseg = 0 ;
unsigned short xfer_bytes = 0 ;
unsigned short xfer_total = 0 ;
unsigned short xfer_last = 0 ;
unsigned short xfer_segments = 0 ;
unsigned short cal_size = 0 ;
unsigned short used_size = 0 ;
unsigned short ultra_size = 0 ;
cal_record *pcal = NULL ;
short sequence_mod = 8 ;
short vcovalue = -1 ; 
short xfer_resends = -1 ;
short mappoll = 0 ;
short sincemap = 0 ;
short minctr = 0 ;
short clientpoll = 0 ;
short down_count = 0 ;
download_struc *pdownload = NULL ;     
ultra_type *pultra = NULL ;
tupbuf *pupbuf = NULL ;
double curtime, lastsec ;
double last_sent = 0.0 ;
DP_to_DA_msg_type gmsg ;

/* SeedLink-specific variables */

enum
  begin
    SL_IFDOWN,
    SL_IFDOWN_IN_PROGRESS,
    SL_IFUP_IN_PROGRESS,
    SL_IFUP,
    SL_CONNECT_IN_PROGRESS,
    SL_CONNECTED
  end sl_state = SL_IFDOWN ;
long sl_standby = 0 ;          /* standby in seconds */
long sl_uptime = 0 ;           /* maximum connection time in seconds */
long sl_cnt = 0 ;              /* counter */
long sl_seqsave_cnt = 0 ;      /* counter */
char sl_ifup[CFGWIDTH] ;       /* shell command to bring network interface up */
char sl_ifdown[CFGWIDTH] ;     /* shell command to take network interface down */
char sl_timetable_loader[CFGWIDTH] ;  /* shell command to load timetable for
                                  overlap detection */
char sl_seqfile[CFGWIDTH] ;    /* file where sequence number is saved */
long sl_seqsave ;              /* interval of saving the sequence number */
char sl_selectors[CFGWIDTH] ;  /* selectors */
char sl_schedule_str[CFGWIDTH] ;
boolean sl_have_schedule ;     /* do we have time schedule or just standby? */
schedule sl_schedule ;
double sl_schedule_check ;     /* last time the schedule was checked */
boolean sl_trigger ;           /* should run ifup according to schedule */
complong sl_network ;          /* network code for SeedLink */
boolean sl_unistation ;        /* fall back to uni-station mode if
                                  multi-station mode is not supported */
boolean sl_have_dlock ;        /* are we using dial lock */
int sl_dlock_fd ;              /* lock file descriptor */
char sl_dlock_file[CFGWIDTH] ; /* lock file name */


/* end of SeedLink-specific variables */

int data_source ;              /* SRC_COMLINK, SRC_SEEDLINK */

#ifdef _OSK
mh_com *vopthdr ;
voptentry *pvopt ;
#endif

string3 seed_names[20][7] =
          /* VBB   VSP    LG    MP    LP   VLP   ULP */
  { /*V1*/ {"BHZ","EHZ","HLZ","MHZ","LHZ","VHZ","UHZ"},
    /*V2*/ {"BHN","EHN","HLN","MHN","LHN","VHN","UHN"},
    /*V3*/ {"BHE","EHE","HLE","MHE","LHE","VHE","UHE"},
    /*V4*/ {"BXZ","EXZ","HXZ","MXZ","LXZ","VXZ","UXZ"},
    /*V5*/ {"BXN","EXN","HXN","MXN","LXN","VXN","UXN"},
    /*V6*/ {"BXE","EXE","HXE","MXE","LXE","VXE","UXE"},
    /*ZM*/ {"BMZ","EMZ","HMZ","MMZ","LMZ","VMZ","UMZ"},
    /*NM*/ {"BMN","EMN","HMN","MMN","LMN","VMN","UMN"},
    /*EM*/ {"BME","EME","HME","MME","LME","VME","UME"},
    /*L1*/ {"BCI","ECI","HCI","MCI","LCI","VCI","UCI"},
    /*L2*/ {"BTI","ETI","HTI","MTI","LTI","VTI","UTI"},
    /*L3*/ {"BPI","EPI","HPI","MPI","LPI","VPI","UPI"},
    /*L4*/ {"BX1","EX1","HX1","MX1","LX1","VX1","UX1"},
    /*L5*/ {"BX2","EX2","HX2","MX2","LX2","VX2","UX2"},
    /*L6*/ {"BX3","EX3","HX3","MX3","LX3","VX3","UX3"},
    /*L7*/ {"BX4","EX4","HX4","MX4","LX4","VX4","UX4"},
    /*L8*/ {"BX5","EX5","HX5","MX5","LX5","VX5","UX5"},
    /*L9*/ {"BX6","EX6","HX6","MX6","LX6","VX6","UX6"},
    /*R1*/ {"BX7","EX7","HX7","MX7","LX7","VX7","UX7"},
    /*R2*/ {"BX8","EX8","HX8","MX8","LX8","VX8","UX8"}} ;

location_type seed_locs[20][7] ;

string15 queue_names[STOPACK+1] =
  { "Data", "Detection", "Calibration", "Timing", "Message", "Blockette", "Every" } ;

#ifdef _OSK
extern short VER_OS9STUFF ;
#endif
extern short VER_TIMEUTIL ;
extern short VER_CFGUTIL ;
extern short VER_SEEDUTIL ;
extern short VER_STUFF ;
extern short VER_COMLINK ;
extern short VER_SEEDLINK ;
extern short VER_CSCFG ;
extern short VER_BUFFERS ;
extern short VER_COMMANDS ;

void unblock (short clientnum) ;
byte handler (pclient_struc svc, short clientnum) ;
void next_segment (void) ;
void request_map (void) ;
void send_window (void) ;
void readcfg (void) ;
void gcrcinit (void) ;
void comlink_check_input (void) ;
int seedlink_load_timetable (void) ;
int seedlink_check_input (void) ;
int seedlink_check_ifup (void) ;
int seedlink_check_ifdown (void) ;
int seedlink_check_connect(void) ;
void seedlink_ifup (void) ;
void seedlink_ifdown (void) ;
int seedlink_connect(const char *address) ;
void setupbuffers (void) ;
boolean checkmask (short qnum) ;
void request_ultra (void) ;
void request_link (void) ;
void set_verb (int i) ;
  
  void dummy_handler (int sig)
    begin
    end
  
  void term_handler (int sig)
    begin
      stop = TRUE;
    end
  
  void terminate (pchar s)
    begin
      fprintf(stderr, "%s %s", localtime_string(dtime()), s) ;
      exit(12) ;
    end

  double last_full_minute(double sec)
    begin
      time_t isec = (time_t) sec ;
      struct tm* tm = localtime(&isec) ;
      return (sec - (double) tm->tm_sec) ;
    end
  
  void detach_client (short clientnum, boolean timedout)
    begin
      short i, j ;
      tclients *pt ;
      boolean sameuid ;

      if (combusy == clientnum)
        then
          combusy = NOCLIENT ;
      pt = &clients[clientnum] ;
      pt->client_pid = NOCLIENT ;
      pt->client_uid = NOCLIENT ;
      if (pt->client_address)
        then
          begin
            sameuid = clients[clientnum].client_address->client_uid == base->server_uid ;
            if (shmdt((pchar) pt->client_address) == 0)
              then
                if (sameuid) /* Don't delete foreign client, we don't know for sure it is dead */
                  then
                    shmctl(pt->client_memid, IPC_RMID, NULL) ;
            pt->client_address = NULL ;
          end
      if (timedout)
        then
          begin
            pt->active = FALSE ;
            unblock (clientnum) ;
            clr_bit (&blockmask, clientnum) ;
          end
      if (clientnum >= resclient)
        then
          begin /* remove this client from list */
            pt->active = FALSE ;
            for (i = clientnum ; i < highclient - 1 ; i++)
              clients[i] = clients[i + 1] ;
            highclient-- ; /* contract the window */
          end
    end
         
  void check_clients (void)
    begin
      short i ;
      tclients *pt ;
      boolean timedout, died, foreign_to, sameuid, alive ;

      if (++clientpoll >= highclient)
        then
          clientpoll = 0 ;
      pt = &clients[clientpoll] ;
      if (lnot pt->active)
        then
          return ;
      timedout = (pt->blocking) land ((curtime - pt->last_service) > pt->timeout) ;
      alive = (pt->client_address) land (pt->client_pid != NOCLIENT) ;
      sameuid = alive land (pt->client_uid == base->server_uid) ;
#ifdef _OSK
      died =  alive land sameuid land (kill(pt->client_pid, SIGWAKE) == ERROR) ;
#else
      died =  alive land sameuid land (kill(pt->client_pid, 0) == ERROR) ;
#endif
      foreign_to = alive land (lnot sameuid) land ((curtime - pt->last_service) > NON_PRIVILEGED_TO)
                         land (combusy != clientpoll) ;
      if (timedout land verbose)
        then
          printf("Client %4.4s timed out at %s", &pt->client_name, localtime_string(dtime())) ;
      else if (died land verbose)
        then
          printf("Client %4.4s has died at %s", &pt->client_name, localtime_string(dtime())) ;
      else if (foreign_to land verbose)
        then
          printf("Foreign Client %4.4s presumed dead at %s", &pt->client_name, localtime_string(dtime())) ;
      if (died lor timedout lor foreign_to)
        then
          begin
            if (verbose)
              then
                begin
                  if (timedout)
                    then
                      printf(", unblocking") ;
                  if (clientpoll >= resclient)
                    then
                      printf(", Removed from client table\n") ;
                    else
                      printf(", Reserved client, not removed\n") ;
                end
            detach_client (clientpoll, timedout) ;
          end
      fflush (stdout) ;
    end

  void load_seq ()
    begin
      FILE *fp;
    
      if (sl_seqfile[0] != 0 land access(sl_seqfile, F_OK) == 0)
        then
          begin
            if ((fp = fopen(sl_seqfile, "r")) != NULL land fscanf(fp, "%lX", &seq) == 1)
              then
                begin
                  seq_valid = TRUE ;
                  fclose(fp) ;
                end
              else
                begin
                  fprintf (stderr, "%s Could not read sequence number from %s\n",
                    localtime_string(dtime()), sl_seqfile) ;
                  exit(12) ;
                end
          end
    end

  void save_seq ()
    begin
      FILE *fp;
    
      if (seq_valid land sl_seqfile[0] != 0)
        then
          begin
            if((fp = fopen(sl_seqfile, "w")) != NULL)
              then
                begin
                  fprintf (fp, "%06X", seq) ;
                  fclose(fp) ;
                end
              else
                fprintf (stderr, "%s Could not save sequence number to %s\n",
                  localtime_string(dtime()), sl_seqfile) ;
          end
    end

  int main (int argc, char *argv[], char **envp)
    begin
#ifdef _OSK
      struct sgbuf sttynew ;
      struct sgbuf sttyold ;
#else
      struct termio sttynew ;
      struct termio sttyold ;
      struct sigaction sa ;
#endif
      int shmid ;
      int semid, clientid ;
      short i, j, k, l, zerocnt, found, first, uppoll ;
      long total, tscan, lasttotal, services, cflag ;
      short cur, did ;
      short min, max ;
      int err, outque ;
      char c ;
      long ct, ctmax, ctmin, ctcount ;
      float cttotal ;
      struct sembuf notbusy = { 0, 1, SEM_UNDO } ;
      struct sembuf notbusy_without_undo = { 0, 1, 0 } ;
      struct sembuf busy = { 0, -1, SEM_UNDO } ;
      union semun semarg ;
      long bufsize, size, stemp ;
      int flags, ruflag ;
      int status ;
#ifdef SOLARIS2
      timespec_t rqtp, rmtp ;
#endif
#ifdef _OSK
      unsigned pollslp ;
#endif


       
      char filename[CFGWIDTH] ;
      char station_dir[CFGWIDTH] = "" ;
      char station_name[5] ;
      char source[SECWIDTH] = "" ;
      
      strncpy(linkstat.seedformat, seedformat, 4);

/* must have at least one command line argument (the station) */
      if (argc < 2)
        then
          begin
            fprintf (stderr, "No station name specified\n") ;
            exit(12) ;
          end
          
      start_time = (long) dtime () ;

/* Copy the first argument into the station name, make sure it's null
   terminated.
*/
      strncpy (station_name, argv[1], 5) ;
      station_name[4] = '\0' ;
      station.l = str_long (station_name) ;

/* Report versions of all modules */
      printf ("%s Comserv Edition %d / SeedLink started for station %s\n", 
       localtime_string(dtime()), EDITION, &station_name) ;
      printf ("      Quanstrc Ver=%d, DPstruc Ver=%d, Seedstrc Ver=%d, Timeutil Ver=%d\n",
              VER_QUANSTRC, VER_DPSTRUC, VER_SEEDSTRC, VER_TIMEUTIL) ;
      printf ("      Cfgutil Ver=%d, Seedutil Ver=%d, Stuff Ver=%d, Comlink Ver=%d\n",
              VER_CFGUTIL, VER_SEEDUTIL, VER_STUFF, VER_COMLINK) ;
      printf ("      Seedlink Ver=%d, Cscfg Ver=%d, Buffers Ver=%d, Commands Ver=%d",
              VER_SEEDLINK, VER_CSCFG, VER_BUFFERS, VER_COMMANDS) ;
#ifdef _OSK
      printf (", OS9stuff Ver=%d\n", VER_OS9STUFF) ;
#else
      printf ("\n") ;
#endif

/* for OS9, open a pointer into vopttable */
#ifdef _OSK
      err = vopt_open (&vopthdr, &pvopt) ;
      if (err)
        then
          begin
            fprintf (stderr, "Could not access VOPTTABLE module\n") ;
            exit(err) ;
          end
      pvopt->verbosity = 0 ;
      strcpy (str1, "comserv/") ;
      strcat (str1, station_name) ;
      strpas (pvopt->modname, str1) ;
      pvopt->option1 = 0 ;
      pvopt->option2 = 0 ;
      pvopt->option3 = 0 ;
#endif

/* open the stations list and look for that station */
#ifdef _OSK
      strcpy (filename, "/r0/stations.ini") ;
#else
      strcpy (filename, "/etc/stations.ini") ;
#endif
      if (open_cfg(&cfg, filename, station_name))
        then
          terminate ("Could not find station\n") ;

/* Try to find the station directory, source, and description */
      do
        begin
          read_cfg(&cfg, str1, str2) ;
          if (str1[0] == '\0')
            then
              break ;
          if (strcmp(str1, "DIR") == 0)
            then
              strcpy(station_dir, str2) ;
          else if (strcmp(str1, "DESC") == 0)
            then
              begin
                strcpy(station_desc, str2) ;
                station_desc[59] = '\0' ;
                printf ("%s\n", station_desc) ;
              end
          else if (strcmp(str1, "SOURCE") == 0)
            then
              strcpy(source, str2) ;
        end
      while (1) ;
      close_cfg(&cfg) ;
      
/* Check for data source */
      if (!strcasecmp((pchar) &source, "comlink"))
        then
            data_source = SRC_COMLINK ;
      else if (!strcasecmp((pchar) &source, "seedlink"))
        then
            data_source = SRC_SEEDLINK ;
      else
        terminate ("Unknown data source\n") ;

/* Try to open the station.ini file in this station's directory */
      addslash (station_dir) ;
      strcpy (filename, station_dir) ;
      strcat (filename, "station.ini") ;
      if (open_cfg(&cfg, filename, source))
        then
          terminate ("Could not find station.ini file\n") ;
          
/* Initialize client control structure */
      for (i = 0 ; i < MAXCLIENTS ; i++)
        begin
          clients[i].client_memid = NOCLIENT ;
          clients[i].client_pid = NOCLIENT ;
          clients[i].client_uid = NOCLIENT ;
          clients[i].client_name.l = 0 ;
          clients[i].client_address = NULL ;
          clients[i].blocking = FALSE ;
          clients[i].timeout = 0 ; /* blocking not allowed */
          clients[i].active = FALSE ;
          for (j = DATAQ ; j < NUMQ ; j++)
            begin
              clients[i].last[j].scan = NULL ;
              clients[i].last[j].packet = -1 ;
            end
        end
       
/* Initialize ring structure */
      for (i = DATAQ ; i < NUMQ ; i++)
        begin
          rings[i].head = NULL ;
          rings[i].tail = NULL ;
          rings[i].count = 20 ;
          rings[i].spare = 0 ;
          rings[i].size = 0 ;
        end

/* call routine to parse config file */
      readcfg () ;
      fflush (stdout) ;

      sl_have_dlock = (sl_dlock_file[0] != '\0') ;
      sl_have_schedule = (sl_schedule_str[0] != '\0') ;
      
      if (sl_have_schedule)
        then
          begin
            err = init_schedule(&sl_schedule, sl_schedule_str) ;
            switch(err)
              begin
                case MINUTE_ERR:
                  fprintf(stderr, "Invalid minute in schedule %s\n", sl_schedule_str) ;
                  exit (1) ;
                case HOUR_ERR:
                  fprintf(stderr, "Invalid hour in schedule %s\n", sl_schedule_str) ;
                  exit (1) ;
                case DOM_ERR:
                  fprintf(stderr, "Invalid day of month in schedule %s\n", sl_schedule_str) ;
                  exit (1) ;
                case MONTH_ERR:
                  fprintf(stderr, "Invalid month in schedule %s\n", sl_schedule_str) ;
                  exit (1) ;
                case DOW_ERR:
                  fprintf(stderr, "Invalid day of week in schedule %s\n", sl_schedule_str) ;
                  exit (1) ;
                case EOI_ERR:
                  fprintf(stderr, "Extra characters at the end of schedule %s\n", sl_schedule_str) ;
                  exit (1) ;
              end
          end

      resclient = highclient ; /* reserved clients */

#ifndef _OSK
/* Open the lockfile for exclusive use if lockfile is specified.*/
/* This prevents more than one copy of the program running for */
/* a single station.      */
      if (lockfile[0] != '\0')
        then
          begin
            if ((status = acquire_lock (lockfile)) < 0)
              then
                begin
                  fprintf (stderr, "Unable to lock lockfile: "
                           "%s status=%d errno=%d\n", lockfile, status, errno) ;
                  exit (12) ;
                end
          end
#endif      
      if (data_source == SRC_SEEDLINK)
        then
          begin
            load_seq () ;
            
            if (seedlink_load_timetable() < 0)
              then
                begin
                  fprintf (stderr, "%s Could not load timetable\n",
                    localtime_string(dtime()), sl_seqfile) ;
                end
          end

/* create access control semaphore */
      semid = semget(segkey, 1, IPC_CREAT or PERM) ;
      if (semid == ERROR)
        then
          begin
            fprintf (stderr, "Could not create semaphore with key %d\n", segkey) ;
            exit (12) ;
          end
      semarg.val = 0 ;
      if (semctl(semid, 0, SETVAL, semarg) == ERROR)
        then
          begin
            fprintf (stderr, "Could not initialize semaphore ID %d\n", semid) ;
            exit (12) ;
          end

/* set the size of each ring buffer and add them up to get total size */
      bufsize = 0 ;
/* get base size for blockettes, including reception and header time */
      stemp = sizeof(seed_record_header) + sizeof(data_only_blockette) +
              (sizeof(tdata_user) - 512) ;
      for (i = DATAQ ; i < NUMQ ; i++)
        begin
          switch (i)
            begin
              case DATAQ : ;
              case BLKQ :
                begin
                  size = sizeof(tdata_user) ;               /* whole enchilada */
                  break ;
                end
              case DETQ : 
                begin
                  size = stemp + sizeof(murdock_detect) ;   /* just enough for detections */
                  break ;
                end
              case CALQ : 
                begin
                  size = stemp + sizeof(random_calibration) ; /* Just enough for calibrations */
                  break ;
                end
              case TIMQ : 
                begin
                  size = stemp + sizeof(timing) ;           /* just enough for timing */
                  break ;
                end
              case MSGQ :
                begin 
                  size = stemp + COMMENT_STRING_LENGTH + 2 ; /* just enough for messages */
                  break ;
                end
            end
          rings[i].xfersize = size ; /* number of bytes to transfer to client */
          size = (size + 31) and 0xfffffff8 ; /* add in overhead and double word align */
          bufsize = bufsize + size * rings[i].count ;
          rings[i].size = size ;
        end
      bufsize = (bufsize + 15) and 0xfffffff8 ; /* double word align */

/* create shared memory segment and install my process id */
      shmid = shmget(segkey, sizeof(tserver_struc) + 
                     bufsize, IPC_CREAT or PERM) ;
      if (shmid == ERROR)
        then
          begin
            fprintf (stderr, "Could not create server segment with key %d\n", segkey) ;
            exit (12) ;
          end
      base = (pserver_struc) shmat(shmid, NULL, 0) ;
      if (base == (pserver_struc) ERROR)
        then
          begin
            fprintf (stderr, "Could not attach to server segment ID %d\n", shmid) ;
            exit (12) ;
          end
      base->init = '\0' ;
      base->server_pid = getpid () ;
      base->server_uid = getuid () ;
      base->client_wait = MAXWAIT * 1000000 ; /* convert to microseconds */
      base->privusec = PRIVILEGED_WAIT ;
      base->nonusec = NON_PRIVILEGED_WAIT ;
      base->server_semid = semid ;
      base->servcode = dtime () ;

/* initialize service queue */
      for (i = 0 ; i < MAXCLIENTS ; i++)
        begin
          base->svcreqs[i].clientseg = NOCLIENT ;
          base->svcreqs[i].clientname.l = clients[i].client_name.l ;
        end

/* initialize next packet numbers */
      base->next_data = 0 ;

/* Call routine to setup ring buffers for data and blockettes */
      setupbuffers () ;
   
/* Allow access to service queue */
      if (semop(semid, &notbusy_without_undo, 1) == ERROR)
        then
          begin
            fprintf (stderr, "Could not set semaphore ID %d to not busy\n", semid) ;
            exit (12) ;
          end
          
/* Server shared memory segment is now initialized */
      base->init = 'I' ;

/* Open and configure port if data source is Comlink */
      if (data_source == SRC_COMLINK)
        then
          begin
            if (port[0] != '\0')
              then
                begin
                  udplink = FALSE ; /* no such thing as udp serial */
/* Open serial port, get it's settings (save them in sttyold) */
#ifdef _OSK
                  path = open (port, FAM_READ or FAM_WRITE) ;
#else
                  path = open (port, O_RDWR or O_NDELAY) ;
#endif
                  if (path < 0)
                    then
                      begin
                        fprintf (stderr, "Could open serial port %s\n", port) ;
                        exit (12) ;
                      end
#ifdef _OSK
                  if (_gs_opt(path, &sttyold) == ERROR)
#else
                  if (ioctl(path, TCGETA, &sttyold) == ERROR)
#endif
                    then
                      begin
                        fprintf (stderr, "Could not obtain settings for %s\n", port) ;
                        exit (12) ;
                      end
#ifdef _OSK
                  memcpy ((pchar) &sttynew, (pchar) &sttyold, sizeof(struct sgbuf)) ;
#else
                  memcpy ((pchar) &sttynew, (pchar) &sttyold, sizeof(struct termio)) ;
#endif

/* Setup new settings for Raw mode */
#ifdef _OSK
                  memset((pchar) &sttynew.sg_case, '\0', 19) ; /* clear line editing */
                  memset((pchar) &sttynew.sg_xon, '\0', 4) ; /* clear flow and tabs */
                  switch (baud)
                    begin
                      case 2400 : 
                        begin
                          sttynew.sg_baud = 0xA ;
                          break ;
                        end
                      case 4800 :
                        begin
                          sttynew.sg_baud = 0xC ;
                          break ;
                        end
                      case 9600 :
                        begin
                          sttynew.sg_baud = 0xE ;
                          break ;
                        end
                      case 19200 :
                        begin
                          sttynew.sg_baud = 0xF ;
                          break ;
                        end
                      case 38400 :
                        begin
                          sttynew.sg_baud = 6 ;
                          break ;
                        end
                      default :
                        begin
                          fprintf(stderr, "Invalid baudrate %d\n", baud) ;
                          exit(12) ;
                        end
                    end
                  cflag = 0 ; /* assume no parity */
                  switch (parity)
                    begin
                      case 'E' :
                        begin
                          cflag = 3 ;
                          break ;
                        end
                      case 'O' :
                        begin
                          cflag = 2 ;
                          break ;
                        end
                    end
                  sttynew.sg_parity = (sttynew.sg_parity and 0xfc) or cflag ;

/* Configure port using new settings */
                  if (_ss_opt(path, &sttynew) == ERROR)
                    then
                      begin
                        fprintf (stderr, "Cound not configure port %s\n", port) ;
                        exit (12) ;
                      end
                  if (resetp (path, 4096) != 0)
                    then
                      fprintf (stderr, "Could not change serial input buffer size on port %s\n", port) ;
                  if (flow)
                    then
                      if (hardon (path) != 0)
                        then
                          begin
                            fprintf (stderr, "Hardware flow control not supported \n") ;
                            exit (12) ;
                          end
#else
                  sttynew.c_iflag = IGNBRK ;
                  sttynew.c_oflag = 0 ;
                  switch (baud)
                    begin
                      case 2400 : 
                        begin
                          cflag = B2400 ;
                          break ;
                        end
                      case 4800 :
                        begin
                          cflag = B4800 ;
                          break ;
                        end
                      case 9600 :
                        begin
                          cflag = B9600 ;
                          break ;
                        end
                      case 19200 :
                        begin
                          cflag = B19200 ;
                          break ;
                        end
                      case 38400 :
                        begin
                          cflag = B38400 ;
                          break ;
                        end
                      default :
                        begin
                          fprintf(stderr, "Invalid baudrate %d\n", baud) ;
                          exit(12) ;
                        end
                    end
                  switch (parity)
                    begin
                      case 'E' :
                        begin
                          cflag = cflag or PARENB ;
                          break ;
                        end
                      case 'O' :
                        begin
                          cflag = cflag or PARENB or PARODD ;
                          break ;
                        end
                    end
                  if (flow)
                    then
                      cflag = cflag or CRTSCTS ;
                  sttynew.c_cflag = CS8 or CREAD or CLOCAL or cflag ;
                  sttynew.c_lflag = NOFLSH ;
                  sttynew.c_cc[VMIN] = 0 ;
                  sttynew.c_cc[VTIME] = 0 ;
 
/* Configure port using new settings */
                  if (ioctl(path, TCSETAW, &sttynew) == ERROR)
                    then
                      begin
                        fprintf (stderr, "Cound not configure port %s\n", port) ;
                        exit (12) ;
                      end
#endif
                  serial = TRUE ;
                end
            else if (ipport[0] == '\0')
              then
                begin
                  fprintf (stderr, "No serial or IP port specified\n") ;
                  exit (12) ;
                end
              else
                begin
                  if (udplink)
                    then
                      begin
                        if (ipaddr[0] == '\0')
                          then
                            begin
                              fprintf (stderr, "No IP address specfied for UDP link\n") ;
                              exit (12) ;
                            end
                        sockfd = socket(AF_INET, SOCK_DGRAM, 0) ;
                      end
                    else
                      sockfd = socket(AF_INET, SOCK_STREAM, 0) ;
                  if (sockfd < 0)
                    then
                      begin
                        fprintf (stderr, "Could not open stream socket\n") ;
                        exit (12) ;
                      end
                  memset ((pchar) &serv_addr, sizeof(serv_addr), 0) ;
                  serv_addr.sin_family = AF_INET ;
                  serv_addr.sin_addr.s_addr = htonl(INADDR_ANY) ;
                  serv_addr.sin_port = htons(atoi(ipport)) ;
                  ruflag = 1 ; /* turn on REUSE option */
                  if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, (char *) &ruflag, sizeof(ruflag)) < 0)
                    then
                      begin
                        fprintf (stderr, "Could not set REUSEADDR socket option\n") ;
                        exit (12) ;
                      end
                  ruflag = 1 ; /* turn on KEEPALIVE optin */
                  if (setsockopt(sockfd, SOL_SOCKET, SO_KEEPALIVE, (char *) &ruflag, sizeof(ruflag)) < 0)
                    then
                      begin
                        fprintf (stderr, "Could not set KEEPALIVE socket option\n") ;
                        exit (12) ;
                      end
                  if (bind(sockfd, (psockaddr) &serv_addr, sizeof(serv_addr)) < 0)
                    then
                      begin
                        fprintf (stderr, "Could not bind local address\n") ;
                        exit (12) ;
                      end
                  path = -1 ;
#ifdef _OSK
                  _gs_opt(sockfd, &sttynew) ;
                  sttynew.sg_noblock = 1 ;
                  _ss_opt(sockfd, &sttynew) ;
#else
                  flags = fcntl(sockfd, F_GETFL, 0) ;
                  flags = flags or FNDELAY ;
                  fcntl(sockfd, F_SETFL, flags) ;
#endif
                  if (udplink)
                    then
                      begin /* who to send packets to */
                        memset ((pchar) &cli_addr, sizeof(cli_addr), 0) ;
                        cli_addr.sin_family = AF_INET ;
                        cli_addr.sin_addr.s_addr = inet_addr(ipaddr) ;
                        cli_addr.sin_port = htons(atoi(ipport)) ;
                        path = sockfd ; /* fake it being opened */
                      end
                    else
                      listen (sockfd, 1) ;
                  netdly_cnt = netdly - 1 ; /* we can try to connect immediately */
                end
          end
        else  /* data_source == SRC_SEEDLINK */
          begin
            serial = FALSE ;
            sl_cnt = sl_standby ;
          end
            

/* Client will send a SIGALRM signal to me when it puts it's segment ID into the
   service queue, So make sure we don't die on it, just exit sleep.
*/
/* According to POSIX, don't exit sleep if signal is ignored! Bug? */
#ifdef _OSK
      signal (SIGALRM, SIG_IGN) ;
      signal (SIGPIPE, SIG_IGN) ;
/* Intercept various abort type signals so they close the socket, which the OS
   apparently doesn't do when the server is abnormally terminated
*/
      signal (SIGHUP, term_handler) ;
      signal (SIGINT, term_handler) ;
      signal (SIGQUIT, term_handler) ;
#else
/* Use POSIX calls with standardized semantics */
      sa.sa_handler = dummy_handler ;
      sa.sa_flags = SA_RESTART ;
      sigemptyset(&sa.sa_mask) ;
      sigaction(SIGALRM, &sa, NULL) ;
      
      sa.sa_handler = term_handler ;
      sigaction(SIGINT, &sa, NULL) ;
      sigaction(SIGQUIT, &sa, NULL) ;
      sigaction(SIGTERM, &sa, NULL) ;

      sa.sa_handler = SIG_IGN ;
      sigaction(SIGHUP, &sa, NULL) ;
      sigaction(SIGPIPE, &sa, NULL) ;
#endif
      
      stop = 0 ;
      uppoll = 0 ;
      total = 0 ;
      zerocnt = 0 ;
      tscan = 0 ;
      lasttotal = 0 ;
      services = 0 ;
      min = 32767 ;
      max = 0 ;
      first = TRUE ;
      ctmax = 0 ;
      ctmin = 2147483647 ;
      ctcount = 0 ;
      cttotal = 0 ;
      src = (pchar) &sbuf ;
      srcend = src ;
    /* destend must be past right end of packet */
      destend = (pchar) ((long) &dbuf.crc_low + sizeof(short)) ;
      dest = (pchar) &dbuf.seq ; /* skip "skipped" */
      gcrcinit () ;
      for (i = 0 ; i < 14 ; i++)
        begin
          ultra_seg[i] = 0 ;
          detavail_seg[i] = 0 ;
        end
      if (linkstat.ultraon)
        then
          request_link () ;
#ifdef SOLARIS2
      rqtp.tv_sec = (1000 * polltime) div 1000000000 ;
      rqtp.tv_nsec = (1000 * polltime) mod 1000000000 ;
#endif
#ifdef _OSK
      if (polltime < 11000)
        then
          pollslp = 1 ;
        else
          pollslp = 0x80000000 or (polltime / 3906) ;
#endif
      lastsec = dtime () ;
      sl_schedule_check = last_full_minute(lastsec) ;
      
      do
        begin
          if (data_source == SRC_COMLINK)
            then
              comlink_check_input () ;
            else  /* data_source == SRC_SEEDLINK */
              begin
                if (sl_state >= SL_IFUP land sl_uptime != 0 land sl_cnt >= sl_uptime)
                  then
                    begin
                      sl_state = SL_IFDOWN_IN_PROGRESS ;
                      seedlink_ifdown () ;
                    end
                else if (sl_state == SL_IFDOWN)
                  then
                    begin
                      if (((lnot sl_have_schedule land sl_cnt >= sl_standby)
                          lor (sl_have_schedule land sl_trigger))
                              land lnot checkmask(-1)
                              land (lnot sl_have_dlock lor
                                  (sl_dlock_fd = acquire_lock(sl_dlock_file)) >= 0))
                        then
                          begin
                            sl_state = SL_IFUP_IN_PROGRESS ;
                            sl_trigger = FALSE ;
                            seedlink_ifup () ;
                          end
                    end
                else if (sl_state == SL_IFUP_IN_PROGRESS)
                  then
                    begin
                      if (seedlink_check_ifup())
                        then
                          begin
                            sl_state = SL_IFUP ;
                            sl_cnt = 0 ;
                            netdly_cnt = netdly ;
                          end
                    end
                else if (sl_state == SL_IFUP)
                  then
                    begin
                      if (netdly_cnt >= netdly)
                        then
                          begin
                            netto_cnt = 0 ;
                            if(seedlink_connect(port) < 0)
                              then
                                begin
                                  sl_state = SL_IFDOWN_IN_PROGRESS ;
                                  seedlink_ifdown () ;
                                end
                              else
                                sl_state = SL_CONNECT_IN_PROGRESS ;
                          end
                    end
                else if (sl_state == SL_CONNECT_IN_PROGRESS)
                  then
                    begin
                      if((err = seedlink_check_connect()) > 0)
                        then
                          begin
                            sl_state = SL_CONNECTED ;
                            netto_cnt = 0 ;
                          end
                      else if (err < 0 lor netto_cnt >= netto)
                        then
                          begin
                            sl_state = SL_IFDOWN_IN_PROGRESS ;
                            seedlink_ifdown () ;
                          end
                    end
                else if (sl_state == SL_CONNECTED)
                  then
                    begin
                      if ((err = seedlink_check_input()) > 0 lor netto_cnt >= netto)
                        then
                          begin
                            sl_state = SL_IFDOWN_IN_PROGRESS ;
                            seedlink_ifdown () ;
                          end
                      else if (err < 0)
                        then
                          begin
                            sl_state = SL_IFUP ;
                            netdly_cnt = 0 ;
                          end
                      else if (sl_seqsave_cnt >= sl_seqsave)
                        then
                          begin
                            save_seq () ;
                            sl_seqsave_cnt = 0 ;
                          end
                    end
                else if (sl_state == SL_IFDOWN_IN_PROGRESS)
                  then
                    begin
                      if (seedlink_check_ifdown())
                        then
                          begin
                            sl_state = SL_IFDOWN ;
                            sl_cnt = 0 ;
                            if (sl_have_dlock)
                              then
                                release_lock(sl_dlock_fd) ;
                          end
                    end
              end

#ifdef _OSK
          set_verb (longhex(pvopt->verbosity)) ;
#endif
          tscan++ ;
          did = 0 ;
/* Look for service requests */
          for (cur = 0 ; cur < MAXCLIENTS ; cur++)
            begin
              clientid = base->svcreqs[cur].clientseg ;
              if (clientid != NOCLIENT)
                then
                  begin
/* Search in table to find previously registered client */
                    cursvc = NULL ;
                    for (i = 0 ; i < highclient ; i++)
                      if (clients[i].client_memid == clientid)
                        then
                          begin
                            cursvc = clients[i].client_address ;
                            if (cursvc->client_pid != clients[i].client_pid)
                              then
                                cursvc = NULL ; /* not a complete match */
                              else
                                break ; /* found a complete match of segment ID and PID */
                          end
                    if (cursvc == NULL)
                      then
                        begin
                          cursvc = (pclient_struc) shmat(clientid, NULL, 0) ;
                          found = FALSE ;
                          for (i = 0 ; i < highclient ; i++)
                            if ((clients[i].client_name.l == cursvc->myname.l) land
                               ((clients[i].client_pid == cursvc->client_pid) lor
                                (clients[i].client_pid == NOCLIENT) lor (i < resclient)))
                              then
                                begin
                                  found = TRUE ; /* Name and PID match, new segment */
                                  clients[i].client_memid = clientid ;
                                  clients[i].client_pid = cursvc->client_pid ;
                                  clients[i].client_uid = cursvc->client_uid ;
                                  clients[i].client_address = cursvc ;
                                  if (verbose)
                                    then
                                      begin
                                        if (i < resclient)
                                          then
                                            if (clients[i].timeout)
                                              then
                                                printf("Blocking") ;
                                              else
                                                printf("Reserved") ;
                                          else
                                            printf("Returning") ;
                                        printf (" client %4.4s, with PID of %d with memory ID %d\n",
                                                &clients[i].client_name,
                                                cursvc->client_pid, clientid) ;
                                        fflush (stdout) ;
                                      end
                                  if (clients[i].timeout) /* client can be blocking */
                                    then
                                      begin
                                        curclient = (pclient_station) ((long) cursvc +
                                                     cursvc->offsets[cursvc->curstation]) ;
                                        if (curclient->blocking)
                                          then
                                            begin /* client is taking blocking option */
                                              clients[i].blocking = TRUE ;
                                              set_bit (&blockmask, i) ;
                                            end
                                          else
                                            begin /* client elected not to block */
                                              clients[i].blocking = FALSE ;
                                              unblock(i) ;
                                              clr_bit (&blockmask, i) ;
                                            end
                                      end
                                  break ;
                                end
                          if (lnot found)
                            then
                              begin
                                i = highclient++ ;
                                if (i >= MAXCLIENTS)
                                  then
                                    begin
                                      cursvc->error = CSCR_REFUSE ;
                                      highclient-- ;
                                    end
                                  else
                                    begin
                                      clients[i].client_memid = clientid ;
                                      clients[i].client_pid = cursvc->client_pid ;
                                      clients[i].client_uid = cursvc->client_uid ;
                                      clients[i].client_address = cursvc ;
                                      clients[i].client_name.l = cursvc->myname.l ;
                                      for (j = DATAQ ; j < NUMQ ; j++)
                                        begin
                                          clients[i].last[j].scan = NULL ;
                                          clients[i].last[j].packet = -1 ;
                                        end
                                      if (verbose)
                                        then
                                          begin
                                            if (cursvc->client_uid == base->server_uid)
                                              then
                                                printf ("New client %4.4s, with PID %d and memory ID %d\n", 
                                                         &clients[i].client_name,
                                                         cursvc->client_pid, clientid) ;
                                              else
                                                printf ("New Foreign client %4.4s, with PID %d, UID %d, and memory ID %d\n", 
                                                         &clients[i].client_name, cursvc->client_pid,
                                                         cursvc->client_uid, clientid) ;
                                            fflush (stdout) ;
                                          end
                                    end
                              end
                        end
                    if (cursvc->error == OK)
                      then
                        begin
                          did++ ;
                          services++ ;
                          if ((services mod 1000) == 0)
                            then
                              begin
                                ct = (long int)(cttotal / ctcount) ;
                                if (rambling)
                                  then
                                    begin
                                      printf("%d Client services, %d read calls, %d avg. time\n",
                                             services, ctcount, ct) ;
                                      fflush (stdout) ;
                                    end
                              end
                          clients[i].last_service = dtime () ;
                          clients[i].active = TRUE ;
                          cursvc->error = handler(cursvc, i) ;
                        end
                    semop(semid, &busy, 1) ;
                    cursvc->done = TRUE ;
                    base->svcreqs[cur].clientseg = NOCLIENT ;
                    semop(semid, &notbusy, 1) ;
                    if (cursvc->client_uid == base->server_uid)
                      then
#ifdef _OSK
                        kill (cursvc->client_pid, SIGWAKE) ;
#else
                        kill (cursvc->client_pid, SIGALRM) ;
#endif
                    if (did >= MAXPROC)
                      then
                        break ;
                  end
            end
          if (data_source == SRC_COMLINK)
            then
              begin
                comlink_check_input () ;
                curtime = dtime () ;
                if ((curtime - lastsec) >= 1.0)
                  then
                    begin
                      lastsec += 1.0 ;
                      uppoll++ ;
                      check_clients () ;
                      if (lnot serial)
                        then
                          if ((path >= 0) land lnot udplink)
                            then
                              begin
                                if (netto_cnt++ >= netto)
                                  then
                                    begin
                                      if (verbose)
                                        then
                                          printf("%d second network timeout, closing connection to DA\n", netto) ;
                                      shutdown (path, 2) ;
                                      close (path) ;
                                      path = -1 ;
                                      netto_cnt = 0 ;
                                      netdly_cnt = 0 ;
                                      seq_valid = FALSE ;
                                    end
                              end
                          else if ((++netdly_cnt == netdly) land (rambling)
                                   land lnot udplink)
                            then
                              printf("Checking for pending network connection request from DA\n") ;
                      if (path >= 0)
                        then
                          begin
                            if (linkstat.ultraon land (lnot linkstat.linkrecv))
                              then
                                if (++linkpoll >= link_retry)
                                  then
                                    request_link () ;
                            if (mappoll != 0)
                              then
                                if (--mappoll == 0)
                                  then
                                    request_map () ;
                            if (linkstat.ultraon land linkstat.linkrecv land (lnot linkstat.ultrarecv)
                                land (lnot notify) land (lnot noultra) land (++minctr >= 60))
                              then
                                begin
                                  minctr = 0 ;
                                  request_ultra () ;
                                end
                            if ((mappoll != 0) land (--mappoll <= 0))
                              then
                                request_map () ;
                          end                
                    end
                if ((txwin > 0) land (curtime > last_sent + grptime))
                  then
                    send_window () ; 
                comlink_check_input () ;
                if (xfer_up_ok)
                  then
                    begin
                      if ((path >= 0) land (uppoll >= 0))
                        then
                          begin
                            uppoll = 0 ;
                            next_segment () ;
                          end
                    end
              end
            else  /* data_source == SRC_SEEDLINK */
              begin
                curtime = dtime () ;
                if ((curtime - lastsec) >= 1.0)
                  then
                    begin
                      lastsec += 1.0 ;
                      netto_cnt++ ;
                      netdly_cnt++ ;
                      sl_seqsave_cnt++ ;
                      sl_cnt++ ;
                      check_clients () ;
                    end
                if (sl_have_schedule land (curtime - sl_schedule_check) >= 60.0)
                  then
                    begin
                      sl_schedule_check += 60.0 ;
                      sl_trigger |= check_schedule(&sl_schedule, (time_t) curtime) ;
                    end
              end
          if (verbose land (noackmask != oldackmask))
            then
              begin
                for (i = DATAQ ; i <= STOPACK ; i++)
                  if (verbose land (test_bit(noackmask eor oldackmask, i)))
                    then
                      begin
                        if (test_bit(noackmask, i))
                          then
                            printf("%s queue blocked at %s\n", queue_names[i], localtime_string(dtime())) ;
                          else
                            printf("%s queue unblocked at %s\n", queue_names[i], localtime_string(dtime())) ;
                        fflush (stdout) ;
                      end
                oldackmask = noackmask ;
              end
#ifdef SOLARIS2
          nanosleep (&rqtp, &rmtp) ;
#elif defined _OSK
          tsleep (pollslp) ;
#else
          usleep (polltime) ;
#endif
 
        end
      while (lnot stop) ;
      if (serial)
        then
          begin
            if (path >= 0)
             then
                close(path) ;
          end
        else
          begin
            if (path >= 0)
              then
                begin
                  shutdown(path, 2) ;
                  close(path) ;
                end
            shutdown(sockfd, 2) ;
            close(sockfd) ;
          end
#ifdef _OSK
      vopt_close (vopthdr) ;
#endif
      if (data_source == SRC_SEEDLINK)
        then
          begin
            sl_cnt = 0 ;
            netto_cnt = 0 ;
            if (sl_state == SL_IFUP_IN_PROGRESS)
              then
                while (lnot seedlink_check_ifup())
                  sleep (1) ;
            if (sl_state >= SL_IFUP)
              then
                begin
                  sl_state = SL_IFDOWN_IN_PROGRESS ;
                  seedlink_ifdown() ;
                end
            if (sl_state == SL_IFDOWN_IN_PROGRESS)
              then
                while (lnot seedlink_check_ifdown())
                  sleep (1) ;
            save_seq () ;
          end
      
      fflush (stdout) ;
      fprintf (stderr, "%s Server terminated\n", localtime_string(dtime())) ;
      return 12 ;
    end

