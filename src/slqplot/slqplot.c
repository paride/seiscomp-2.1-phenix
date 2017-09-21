/***************************************************************************** 
 * slqplot.c
 *
 * Plot seismic traces
 *
 * (c) 1998, 2003 Andres Heinloo, GFZ Potsdam
 * 
 * Modified:
 *  2003.7.1  - update to libslink version 0.8.3+ usage, Chad
 *  2003.6.11 - Ported to be a SeedLink client, Chad
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <termios.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/time.h>

#include "slqplot.h"

#define TIMESTRLEN  40

static int MSrecord_to_mshdrdata(MSrecord *msr, struct ms_hdrdata *head);
static struct termios tio_save;

static SLCD *slcd;

static void int_handler(int sig)
  {
    sl_terminate(slcd);
  }

#ifdef SIGUSR1_REDISPLAY
static volatile sig_atomic_t redisplay_request = 0;

static void sigusr1_handler(int sig)
  {
    redisplay_request = 1;
    sl_terminate(slcd);
  }
#endif

static void exit_proc(void)
  {
    save_state(state_file);
    
    if(c_flags & FL_INTERACTIVE)
        N(tcsetattr(STDIN_FILENO, TCSANOW, &tio_save));
    
    sl_log(0, 0, "Terminated\n");
  }

static void reset_sequence(int seq)
  {
    SLpacket *slpack = NULL;
    
    if(slcd == NULL || slcd->streams == NULL)
        return;

    sl_terminate(slcd);

    while(sl_collect(slcd, &slpack) == SLPACKET)
        ;

    slcd->streams->seqnum = (seq - 1) & 0xffffff;
    slcd->terminate = 0;
  }

static void log_print(const char *s)
  {
    time_t t = time(NULL);
    char* ts = asctime(localtime(&t));
    char* p = strchr(ts, '\n');

    if(p != NULL) *p = 0;
    
    fprintf(stderr, "%s - slqplot: %s", ts, s);
    fflush(stderr);
  }

int main(int argc, char **argv)
  {
    int i, n;
    int seqnum;
    int first = 1;
    unsigned char cc;
    char statenv[20];
    SLpacket *slpack = NULL;
    int ptype;
    int retval;
    MSrecord *msr = NULL;
    struct ms_hdrdata head;

    struct sigaction sa;
    struct termios tio;

    N(putenv("TZ=UTC"));  /* needed for mktime() */
    
    slcd = sl_newslcd();
    
    qpsetup(argc, argv, slcd);

    restore_state(state_file, &seqnum);
    
    if(state_file != NULL)
        reset_sequence(seqnum);

    /* Init logging and send all output to stderr */
    sl_loginit(c_verbosity, log_print, "", log_print, "error:");

    sa.sa_handler = int_handler;
    sa.sa_flags = SA_RESTART;
    N(sigemptyset(&sa.sa_mask));
    N(sigaction(SIGINT, &sa, NULL));
    N(sigaction(SIGTERM, &sa, NULL));
    
    sa.sa_handler = SIG_IGN;
    N(sigaction(SIGHUP, &sa, NULL));
    N(atexit(exit_proc));

#ifdef SIGUSR1_REDISPLAY
    sa.sa_handler = sigusr1_handler;
    N(sigaction(SIGUSR1, &sa, NULL));
#endif
    
    sl_log(0, 0, "slqplot v" SLQP_VERSION " started\n");
    
    /* Check the parsing here for a single station, a bit redundant */
    if(! slcd->streams || slcd->streams->next != NULL)
      {
        sl_log(1, 0, "There are no streams specified\n");
        exit(1);
      }

    sl_log(0, 0, "Opening plotters\n");
    
    msr = msr_new();
    create_plotters();
    
    /* List configured channels */
    for(i = 0; i < chcount; ++i)
      {
        sl_log(0, 1, "Sta: %s Chan: %s, Loc: %s, Mag: %d\n",
          station_name, c_channel[i].chname,
          c_channel[i].location, c_channel[i].mag);
      }

    if(c_flags & FL_INTERACTIVE)
      {
        if(tcgetattr(STDIN_FILENO, &tio_save) < 0)
          {
            if(errno != ENOTTY)
              {
                perror("tcgetattr(STDIN_FILENO, &tio_save)");
                exit(1);
              }

            c_flags &= ~FL_INTERACTIVE;
          }
        else
          {
            tio = tio_save;
            tio.c_lflag &= ~(ICANON | ECHO);
            tio.c_cc[VMIN] = 0;
            N(tcsetattr(STDIN_FILENO, TCSANOW, &tio));
    
            printf("Type `Q' to exit slqplot\n");
          }
      }
    
    while(1)
      {
        if(c_flags & FL_INTERACTIVE)
          {
            N(n = read(STDIN_FILENO, &cc, 1));
            if(n > 0 && (cc == 'Q' || cc == 'q')) break;

            retval = sl_collect_nb(slcd, &slpack);
          }
        else
          {
            retval = sl_collect(slcd, &slpack);
          }
            
        if(retval <= 0)
          {
#ifdef SIGUSR1_REDISPLAY
            if(redisplay_request)
              {
                redisplay_request = 0;
                closepage();
                destroy_plotters();
                create_plotters();
                redisplay(&seqnum);
                reset_sequence(seqnum);
                continue;
              }
            else
#endif
            if(retval == SLNOPACKET)
              {
                sleep(1);
                continue;
              }
            else
              break;
          }
        
        ptype = sl_packettype(slpack);
      
        if(ptype == SLDATA)
          {            
            char chan[4];
            char loc[3];

            /* Extract the channel and location */
            strncpclean(loc, slpack->msrecord + 13, 2);
            strncpclean(chan, slpack->msrecord + 15, 3);
            
            /* If using uni-station mode, reset the station_name after
               the first record is received */
            if(first)
              {
                if(!strncmp(station_name, "UNI", 3))
                  {
                    strncpclean(station_name, slpack->msrecord + 8, SSTNAME - 1);
                    sprintf(statenv, "STATION=%s", station_name);
                    putenv(statenv);
                    first = 0;
                  }
              }

            /* Find the matching channel entry and display() it */
            for(i = 0; i < chcount; ++i)
              {
                if(!strncasecmp(chan, c_channel[i].chname, 3) &&
                  (c_channel[i].location[0] == 0 ||
                   !strncasecmp(loc, c_channel[i].location, 2)))
                  {
                    if(!msr_parse(NULL, slpack->msrecord, &msr, 1, 1))
                      continue;

                    MSrecord_to_mshdrdata(msr, &head);
                    seqnum = sl_sequence(slpack);
                    display(i, &head, msr->datasamples, &seqnum);

                    if(seqnum != sl_sequence(slpack))
                        reset_sequence(seqnum);

                    if(c_flags & FL_SHOW_PACKETS)
                      msr_print(NULL, msr, 0);

                    break;
                  }
              }
          }
        else if(c_flags & FL_SHOW_PACKETS)
          {
            if(!msr_parse(NULL, slpack->msrecord, &msr, 0, 0))
              continue;

            msr_print(NULL, msr, 0);
          }
      }
    
    sl_log(0, 0, "Terminating program\n");
    sl_log(0, 0, "Closing plotters\n");
    
    destroy_plotters();
    
    return 0;
  }


/* Convert an MSrecord struct to a ms_hdrdata struct */
static int MSrecord_to_mshdrdata(MSrecord *msr, struct ms_hdrdata *head)
  {
    time_t timet;
    struct tm *hdrtime;

    head->packet = 0;
    strncpclean(head->station_id, msr->fsdh.station, 5);
    strncpclean(head->channel_id, msr->fsdh.channel, 3);

    timet = (time_t) msr_depochstime(msr);
    hdrtime = gmtime(&timet);
    
    head->hdrtime.tm_year  = hdrtime->tm_year;
    head->hdrtime.tm_yday  = hdrtime->tm_yday;
    head->hdrtime.tm_mon   = hdrtime->tm_mon;
    head->hdrtime.tm_mday  = hdrtime->tm_mday;
    head->hdrtime.tm_wday  = hdrtime->tm_wday;
    head->hdrtime.tm_hour  = hdrtime->tm_hour;
    head->hdrtime.tm_min   = hdrtime->tm_min;
    head->hdrtime.tm_sec   = hdrtime->tm_sec;
    head->hdrtime.tm_isdst = hdrtime->tm_isdst;

    head->tenth_millisec = msr->fsdh.start_time.fract;
    head->time_correction = msr->fsdh.time_correct;
    head->nsamples = msr->fsdh.num_samples;

    msr_dsamprate(msr, &head->sample_rate);

    head->data_diff = 0;
    head->data_consistency = 0;

    return 0;
  }

