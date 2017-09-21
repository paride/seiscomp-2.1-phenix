/*****************************************************************************
 * seedlink.c
 *
 * SeedLink protocol implementation for Comserv
 *
 * (c) 2000 Andres Heinloo, GFZ Potsdam
 * 
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/ 
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <unistd.h>
#include <syslog.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "seedstrc.h"
#include "service.h"
#include "server.h"
#include "cfgutil.h"
#include "timeutil.h"
#include "stuff.h"
#include "qtime.h"

#define PACKSIZE        512
#define MAX_HEADER_LEN  64
#define SEQSIZE         8
#define SELSIZE         8
#define LOGMSGLEN       200
#define SIGNATURE       "SL"
#define SHELL           "/bin/sh"

short VER_SEEDLINK = 6;

static pid_t pid = 0;
static int sock = -1;
static struct sockaddr_in inet_addr;

static enum
  { 
    REQ_INIT,
    REQ_STATION,
    REQ_SELECT,
    REQ_DATA,
    REQ_END,
  } req_state = REQ_INIT;

static enum
  {
    ACK_OK,
    ACK_ERROR,
    ACK_WAIT
  } ack_state = ACK_OK;

extern complong station;
extern boolean verbose;
extern boolean rambling;
extern boolean insane;
extern int path;
extern char sl_ifup[CFGWIDTH];
extern char sl_ifdown[CFGWIDTH];
extern char sl_timetable_loader[CFGWIDTH];
extern char sl_selectors[CFGWIDTH];
extern long sl_uptime;
extern long sl_cnt;
extern complong sl_network;
extern boolean sl_unistation;
extern long netto;
extern long netto_cnt;
extern long noackmask;
extern long seq;
extern boolean seq_valid;
extern linkstat_rec linkstat;
extern tring rings[NUMQ];
 
tring_elem *getbuffer(short qnum);
boolean checkmask(short qnum);

struct blockette_head
  {
    be_u_int16_t blockette_type;
    be_u_int16_t next_offset;
  };

struct seedlink_head
  {
    char seq[SEQSIZE];
    seed_record_header seedhead;
  };

struct stream_descriptor
  {
    int type;
    char loc[2];
    char chn[3];
  };

struct stream_time_list
  {
    struct stream_time_list *prev;
    struct stream_time_list *next;
    struct stream_descriptor sd;
    INT_TIME it;
    int seq;
  };

struct stream_rename_list
  {
    struct stream_rename_list *next;
    struct stream_descriptor from;
    struct stream_descriptor to;
  };

static struct stream_time_list *stream_times = NULL;

static struct stream_rename_list *stream_renaming = NULL;

#define list_insert_after(a,b) do { \
  (a)->prev = (b); \
  if(b) { (a)->next = (b)->next; (b)->next = (a); } \
  else (a)->next = NULL; \
  if((a)->next) (a)->next->prev = (a); } while(0)

#define list_insert_before(a,b) do { \
  (a)->next = (b); \
  if(b) { (a)->prev = (b)->prev; (b)->prev = (a); } \
  else (a)->prev = NULL; \
  if((a)->prev) (a)->prev->next = (a); } while(0)

#define list_delete(a) do { \
  if((a)->prev) (a)->prev->next = (a)->next; \
  if((a)->next) (a)->next->prev = (a)->prev; \
  } while(0)

static int DOY [] = { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 };
#define IS_LEAP(yr) (yr % 400 == 0 || (yr % 4 == 0 && yr % 100 != 0))
#define LDOY(y, m)  (DOY[m] + (IS_LEAP(y) && m >= 2))

static void dy2mdy (int day, int year, int *month, int *mday)
  {
    *month = 0;
    while (day >= LDOY(year, *month + 1)) ++*month;
    *mday = day - LDOY(year, *month) + 1;
  }

static double stime2sec(seed_time_struc *stime)
  {
    struct tm t;
    double result;
    
    t.tm_sec = stime->seconds;
    t.tm_min = stime->minute;
    t.tm_hour = stime->hr;
    t.tm_year = stime->yr - 1900;
    t.tm_wday = 0;
    t.tm_yday = stime->jday - 1;
    t.tm_isdst = -1;
    dy2mdy(t.tm_yday, t.tm_year + 1900, &t.tm_mon, &t.tm_mday);
    result = (double)mktime(&t) + (double)stime->tenth_millisec / 10000.0;
    return result + (double)timezone;
  } 

static int lprintf(int priority, const char *fmt, ...)
  {
    int r;
    char buf[LOGMSGLEN];
    va_list argptr;
  
    va_start(argptr, fmt);
    
    r = vsnprintf(buf, LOGMSGLEN, fmt, argptr);
    
    if(priority < LOG_INFO || verbose)
      {
        printf("%s - %s", localtime_string(dtime()), buf);
        fflush(stdout);
      }
    
    return r;
  }

static int sequence(const struct seedlink_head *datapack)
  {
    int seq;
    char seqstr[7], *tail;
    
    if(strncmp(datapack->seq, SIGNATURE, 2)) return -1;
    strncpy(seqstr, &datapack->seq[2], 6);
    seqstr[6] = 0;
    seq = strtoul(seqstr, &tail, 16);
    if(seq & ~0xffffff || *tail) return -1;
    return seq;
  }

static int sel_type(const char *ext)
  {
    if(ext == NULL) return NUMQ;
    else if(strlen(ext) != 1) return -1;
    
    switch(toupper(*ext))
      {
        case 'D': return DATAQ;
        case 'E': return DETQ;
        case 'T': return TIMQ;
        case 'C': return CALQ;
        case 'L': return MSGQ;
        case 'O': return BLKQ;
      }

    return -1;
  }

static struct stream_descriptor parse_stream_specifier(const char *llssspt)
  {
    struct stream_descriptor s;
    const char* pt = NULL;
    int len = strcspn(llssspt, ".");

    if(llssspt[len] == '.') pt = &llssspt[len + 1];
    
    if(len == 5)
      {
        strncpy(s.loc, llssspt, 2);
        strncpy(s.chn, llssspt + 2, 3);
        s.type = sel_type(pt);
      }
    else if(len == 3)
      {
        strncpy(s.loc, "??", 2);
        strncpy(s.chn, llssspt, 3);
        s.type = sel_type(pt);
      }
    else if(pt == NULL)
      {
        strncpy(s.loc, "??", 2);
        strncpy(s.chn, "???", 3);
        s.type = sel_type(llssspt);
      }
    else
      {
        strncpy(s.loc, "??", 2);
        strncpy(s.chn, "???", 3);
        s.type = -1;
      }

    return s;
  }

static int queue_type(const struct seedlink_head *datapack)
  {
    int b2000 = 0;
    const struct blockette_head *p = (const struct blockette_head *)((const char *) &datapack->seedhead +
      datapack->seedhead.first_blockette_byte);

    do
      {
        if(((const char *) p) - ((const char *) &datapack->seedhead) > MAX_HEADER_LEN)
            return NUMQ;
        
        if(p->blockette_type >=200 && p->blockette_type <= 299) return DETQ;
        if(p->blockette_type >=300 && p->blockette_type <= 399) return CALQ;
        if(p->blockette_type >=500 && p->blockette_type <= 599) return TIMQ;
        if(p->blockette_type == 2000) b2000 = 1;
        p = (const struct blockette_head *)((const char *) &datapack->seedhead + p->next_offset);
      }
    while((const seed_record_header *) p != &datapack->seedhead);

    if(datapack->seedhead.sample_rate_factor == 0)
      {
        if(datapack->seedhead.samples_in_record != 0) return MSGQ;
        if(b2000) return BLKQ;
      }

    return DATAQ;
  }

static int qbuf_size(int qnum)
  {
    int qsize = rings[qnum].xfersize - (sizeof(tdata_user) - PACKSIZE);
    if(qsize > PACKSIZE) return PACKSIZE;  /* not needed any more? */
    return qsize;
  }

static void shellcmd(const char *cmd)
  {
    pid = 0;
    if(*cmd == 0) return;

    if((pid = fork()) < 0)
      {
        lprintf(LOG_ERR, "fork: %s\n", strerror(errno));
        pid = 0;
        return;
      }
      
    if(pid != 0) return;
    
    if(execl(SHELL, SHELL, "-c", cmd, NULL) < 0)
      {
        lprintf(LOG_ERR, "cannot execute shell %s\n", SHELL);
        exit(1);
      }
  }

static int checkcmd(const char *cmd)
  {
    int status, completed;
    
    if(pid <= 0) return 1;
    
    if((completed = waitpid(pid, &status, WNOHANG)) < 0)
      {
        lprintf(LOG_ERR, "waitpid: %s\n", strerror(errno));
        return 1;
      }

    if(!completed) return 0;

    if(WIFSIGNALED(status))
      {
        lprintf(LOG_ERR, "shell command \"%s\" was terminated by signal %d\n",
          cmd, WTERMSIG(status));
      }
    else if(WIFEXITED(status) && WEXITSTATUS(status) != 0)
      {
        lprintf(LOG_ERR, "shell command \"%s\" exited with error status %d\n",
          cmd, WEXITSTATUS(status));
      }

    return 1;
  }

static void disconnect(void)
  {
    shutdown(path, 2);
    close(path);
    path = -1;
    req_state = REQ_INIT;
    lprintf(LOG_INFO, "network connection closed\n");
  }

static int string_match(const char *tmpl, const char *str, int len)
  {
    int i;
    const char *pt, *ps;

    for(i = 0, pt = tmpl, ps = str; i < len && *pt; ++i, ++pt, ++ps)
        if(*ps == 0 || (*pt != '?' && *ps != *pt)) return 0;

    return 1;
  }

int seedlink_stream_renaming(const char *s)
  {
    int len = 0;
    const char* p = s;
    char rename_from[15], *rename_to;
    struct stream_rename_list *sr;
    
    while(p += len, p += strspn(p, ", "), len = strcspn(p, ", "))
      {
        if(len == 0) continue;
        
        if(len > 12 || len < 0)
          {
            lprintf(LOG_ERR, "invalid syntax: rename_streams=%s\n", s);
            return -1;
          }

        strncpy(rename_from, p, len);
        rename_from[len] = 0;
        
        if((rename_to = strstr(rename_from, "->")) == NULL)
          {
            lprintf(LOG_ERR, "invalid syntax: rename_streams=%s\n", s);
            return -1;
          }
        
        *rename_to = 0;
        rename_to += 2;
        
        if((sr = (struct stream_rename_list *)malloc(sizeof(struct stream_rename_list))) == NULL)
          {
            lprintf(LOG_ERR, "memory allocation error\n");
            exit(1);
          }

        sr->from = parse_stream_specifier(rename_from);
        sr->to = parse_stream_specifier(rename_to);

        if(sr->from.type != NUMQ || sr->to.type != NUMQ)
          {
            lprintf(LOG_ERR, "invalid syntax: rename_streams=%s\n", s);
            free(sr);
            return -1;
          }
        
        sr->next = stream_renaming;
        stream_renaming = sr;
      }

    return 0;
  }

static void rename_streams(seed_record_header *pseed)
  {
    int i;
    const struct stream_rename_list *sr;

    for(sr = stream_renaming; sr; sr = sr->next)
      {
        if(string_match(sr->from.loc, pseed->location_id, 2) &&
          string_match(sr->from.chn, pseed->channel_id, 3))
          {
            if(strncmp(sr->to.loc, "??", 2))
              {
                for(i = 0; i < 2; ++i)
                    if(sr->to.loc[i] != '?')
                        pseed->location_id[i] = sr->to.loc[i];
              }
            else if(strncmp(sr->from.loc, "??", 2))
                memset(pseed->location_id, 32, 2);

            if(strncmp(sr->to.chn, "???", 3))
              {
                for(i = 0; i < 3; ++i)
                    if(sr->to.chn[i] != '?')
                        pseed->channel_id[i] = sr->to.chn[i];
              }
            else if(strncmp(sr->from.chn, "???", 3))
                memset(pseed->channel_id, 32, 3);

            break;
          }
      }
  }

void seedlink_ifup(void)
  {
    path = sock = -1;
    req_state = REQ_INIT;
    shellcmd(sl_ifup);
  }

void seedlink_ifdown(void)
  {
    if(sl_uptime != 0 && sl_cnt >= sl_uptime) lprintf(LOG_WARNING, "uptime expired\n");
    else if(netto_cnt >= netto) lprintf(LOG_INFO, "timeout\n");
    
    if(path >= 0)
      {
        disconnect();
      }
    else if(sock >= 0)
      {
        shutdown(sock, 2);
        close(sock);
        sock = -1;
      }

    shellcmd(sl_ifdown);
  }

int seedlink_check_ifup(void)
  {
    return checkcmd(sl_ifup);
  }

int seedlink_check_ifdown(void)
  {
    return checkcmd(sl_ifdown);
  }

int seedlink_connect(const char *address)
  {
    char *host_name, *p, *tail;
    unsigned short int host_port;
    struct hostent *hostinfo;

    if((host_name = strdup(address)) == NULL)
      {
        lprintf(LOG_ERR, "cannot strdup\n");
        exit(1);
      }

    if((p = strchr(host_name, ':')) == NULL)
      {
        lprintf(LOG_ERR, "host address is not in `hostname:port' format\n");
        exit(1);
      }
    
    *p = 0;
    host_port = strtoul(p + 1, &tail, 0);
    if(*tail || host_port > 0xffff)
      {
        lprintf(LOG_ERR, "host address is not in `hostname:port' format\n");
        exit(1);
      }
    
    if((hostinfo = gethostbyname(host_name)) == NULL)
      {
        /* unknown host, maybe temporary nameserver outage */
        return -1;
      }
    
    free(host_name);
    
    memset(&inet_addr, 0, sizeof(inet_addr));
    inet_addr.sin_family = AF_INET;
    inet_addr.sin_port = htons(host_port);
    inet_addr.sin_addr = *(struct in_addr *)hostinfo->h_addr_list[0];

    if((sock = socket(PF_INET, SOCK_STREAM, 0)) < 0)
      {
        lprintf(LOG_ERR, "socket: %s\n", strerror(errno));
        exit(1);
      }
    
    fcntl(sock, F_SETFL, O_NONBLOCK);
    connect(sock, (struct sockaddr *) &inet_addr, sizeof(inet_addr));
    return 0;
  }

int seedlink_check_connect(void)
  {
    if(connect(sock, (struct sockaddr *) &inet_addr, sizeof(inet_addr)) < 0 &&
      errno != EISCONN)
      {
        if(errno != EALREADY && errno != EAGAIN) return -1;
        return 0;
      }

    lprintf(LOG_INFO, "network connection open\n");
        
    path = sock;
    sock = -1;
    return 1;
  }

static void clear_timetable(void)
  {
    struct stream_time_list *s;
    
    while(stream_times)
      {
        s = stream_times;
        stream_times = s->next;
        list_delete(s);
        free(s);
      }
  }

int seedlink_load_timetable(void)
  {
    int seed_seq, r;
    EXT_TIME et;
    FILE *fp;
    char llssspt[8];
    struct stream_time_list *s;

    clear_timetable();
    
    if(*sl_timetable_loader == 0) return 0;
    
    if((fp = popen(sl_timetable_loader, "r")) == NULL)
      {
        lprintf(LOG_ERR, "could not start %s\n", sl_timetable_loader);
        return -1;
      }

    memset(&et, 0, sizeof(EXT_TIME));
    while((r = fscanf(fp, "%7s %d %d %d %d %d %d %d\n", llssspt, &seed_seq,
      &et.year, &et.doy, &et.hour, &et.minute, &et.second, &et.usec)) == 8)
      {
        dy_to_mdy(et.doy, et.year, &et.month, &et.day);
        if((s = (struct stream_time_list *)malloc(sizeof(struct stream_time_list))) == NULL)
          {
            lprintf(LOG_ERR, "memory allocation error\n");
            exit(1);
          }

        s->sd = parse_stream_specifier(llssspt);
        s->it = ext_to_int(et);
        s->seq = seed_seq;
        
        if(s->sd.type < 0)
          {
            clear_timetable();
            return -1;
          }

        list_insert_before(s, stream_times);
        stream_times = s;
      }

    pclose(fp);
    
    if(r == 0 || r == EOF) return 0;
    
    clear_timetable();
    return -1;
  }

static int overlap(const struct seedlink_head *datapack)
  {
    EXT_TIME et;
    INT_TIME it;
    int seed_seq;
    double dt;
    int ds;
    struct stream_time_list *s;

    memset(&et, 0, sizeof(EXT_TIME));
    et.year = datapack->seedhead.starting_time.yr;
    et.doy = datapack->seedhead.starting_time.jday;
    et.hour = datapack->seedhead.starting_time.hr;
    et.minute = datapack->seedhead.starting_time.minute;
    et.second = datapack->seedhead.starting_time.seconds;
    et.usec = datapack->seedhead.starting_time.tenth_millisec * 100;
    dy_to_mdy(et.doy, et.year, &et.month, &et.day);
    it = ext_to_int(et);
    sscanf(datapack->seedhead.sequence, "%6d", &seed_seq);
    
    for(s = stream_times; s; s = s->next)
      {
        if(queue_type(datapack) == s->sd.type &&
          string_match(s->sd.loc, datapack->seedhead.location_id, 2) &&
          string_match(s->sd.chn, datapack->seedhead.channel_id, 3))
          {
            dt = tdiff(it, s->it);
            ds = seed_seq - s->seq;
    
            if(dt > 0 || (dt > -1.0 && ((ds > 0 && ds < 500000) || ds < -500000)))
              {
                if(s == stream_times)
                    stream_times = s->next;

                list_delete(s);
                return 0;
              }
            else
              {
                return 1;
              }
          }
      }

    return 0;
  }

int seedlink_check_input(void)
  {
    static int rptr = 0, wptr = 0, sellen;
    static char buf[BLOB];
    static char *selptr;
    static boolean multistation;
    char request[15];
    int bytes_read, qnum, tmpseq;
    const char *seedlink_cmd = (sl_uptime == 0) ? "DATA": "FETCH";

    if(req_state == REQ_INIT)
      {
        sprintf(request, "STATION %s %s\r", long_str(station.l),
          long_str(sl_network.l));
    
        if(write(path, request, strlen(request)) < 0)
          { 
            lprintf(LOG_INFO, "error sending STATION request\n");
            disconnect();
            return -1;
          }

        req_state = REQ_STATION;
        ack_state = ACK_WAIT;
        multistation = 1;
        rptr = wptr = 0;
        netto_cnt = 0;
      }

    if(req_state == REQ_STATION && ack_state != ACK_WAIT)
      {
        if(ack_state == ACK_ERROR)
          {
            if(sl_unistation)
              {
                lprintf(LOG_INFO, "entering uni-station mode\n");
                multistation = 0;
              }
            else
              {
                lprintf(LOG_WARNING, "STATION request not accepted (%s %s)\n",
                  long_str(station.l), long_str(sl_network.l));
                disconnect();
                return -1;
              }
          }
    
        selptr = sl_selectors;
        sellen = 0;
        req_state = REQ_SELECT;
        ack_state = ACK_OK;
      }

    if(req_state == REQ_SELECT && ack_state != ACK_WAIT)
      {
        if(ack_state == ACK_ERROR)
          {
            lprintf(LOG_WARNING, "selector not accepted: %.*s\n", sellen, selptr);
          }
    
        selptr += sellen;
        selptr += strspn(selptr, " ");
        sellen = strcspn(selptr, " ");

        if(sellen == 0)
          {
            if(seq_valid)
              {
                lprintf(LOG_INFO, "resuming transmission from packet %06X\n",
                  (seq + 1) & 0xffffff);
                sprintf(request, "%s %06X\r", seedlink_cmd, (seq + 1) & 0xffffff);
              }
            else
              {
                sprintf(request, "%s\r", seedlink_cmd);
              }
        
            if(write(path, request, strlen(request)) < 0)
              { 
                lprintf(LOG_INFO, "error sending %s request\n", seedlink_cmd);
                disconnect();
                return -1;
              }

            if(multistation)
              {
                req_state = REQ_DATA;
                ack_state = ACK_WAIT;
              }
            else
              {
                req_state = REQ_END;
              }
          }
        else if(sellen > SELSIZE)
          {
            lprintf(LOG_ERR, "invalid selector: %.*s\n", sellen, selptr);
            selptr += sellen;
          }
        else
          {
            sprintf(request, "SELECT %.*s\r", sellen, selptr);
        
            if(write(path, request, strlen(request)) < 0)
              { 
                lprintf(LOG_INFO, "error sending SELECT request\n");
                disconnect();
                return -1;
              }

            ack_state = ACK_WAIT;
          }
      }
        
    if(req_state == REQ_DATA && ack_state != ACK_WAIT)
      {
        if(ack_state == ACK_ERROR)
          {
            lprintf(LOG_WARNING, "%s request not accepted\n",
              seedlink_cmd);
            disconnect();
            return -1;
          }

        sprintf(request, "END\r");
    
        if(write(path, request, strlen(request)) < 0)
          { 
            lprintf(LOG_INFO, "error sending END request\n");
            disconnect();
            return -1;
          }

        req_state = REQ_END;
      }

    if(!noackmask || ack_state == ACK_WAIT)
      {
        bytes_read = read(path, &buf[rptr], BLOB - rptr);
    
        if(bytes_read == 0)
          {
            disconnect();
            return 1;
          }
        else if(bytes_read < 0)
          {
            if(errno != EAGAIN)
              {
                linkstat.io_errors++;
                linkstat.lastio_error = errno;
                lprintf(LOG_INFO, "read: %s\n", strerror(errno));
                disconnect();
                return -1;
              }
            
            return 0;
          }

        netto_cnt = 0; 
        rptr += bytes_read;
      }
    
    if(ack_state == ACK_WAIT)
      {
        if(!strncmp(buf, "OK\r\n", rptr))
          {
            if(rptr == 4) ack_state = ACK_OK;
            rptr = 0;
          }
        else if(!strncmp(buf, "ERROR\r\n", rptr))
          {
            if(rptr == 7) ack_state = ACK_ERROR;
            rptr = 0;
          }
        else
          {
            lprintf(LOG_ERR, "invalid response from SeedLink server: %.*s\n", buf, rptr);
            disconnect();
            return -1;
          }
        
        return 0;
      }
    
    while(rptr - wptr >= PACKSIZE + SEQSIZE)
      {
        if(checkmask(-1)) return 0;

        if((tmpseq = sequence((struct seedlink_head *)&buf[wptr])) < 0)
          {
            lprintf(LOG_ERR, "received packet with invalid signature or sequence count\n");
            disconnect();
            return -1;
          }
        
        if(sl_selectors[0] == 0 && seq_valid && tmpseq != ((seq + 1) & 0xffffff))
            lprintf(LOG_WARNING, "sequence gap %06X - %06X\n", 
              (seq + 1) & 0xffffff, (tmpseq - 1) & 0xffffff);
        
        seq = tmpseq;
        seq_valid = TRUE;
        linkstat.total_packets++;
        linkstat.last_good = dtime();
          
        if((qnum = queue_type((struct seedlink_head *)&buf[wptr])) == NUMQ)
          {
            lprintf(LOG_ERR, "invalid Mini-SEED packet detected");
            disconnect();
            return -1;
          }
            
        if(!overlap((struct seedlink_head *)&buf[wptr]))
          {
            tring_elem* freebuf = getbuffer(qnum);
            seed_record_header* pseed = (seed_record_header *) &freebuf->user_data.data_bytes;
            int netlen = strlen(long_str(sl_network.l));
            
            memcpy (pseed, &buf[wptr + SEQSIZE], qbuf_size(qnum));
            freebuf->user_data.reception_time = dtime(); 
            freebuf->user_data.header_time = stime2sec(&pseed->starting_time);
            
            if(netlen != 0)
              {
                strncpy(pseed->seednet, long_str(sl_network.l), 2);
                if(netlen < 2) memset(pseed->seednet + netlen, 32, 2 - netlen);
              }

            rename_streams(pseed);
          }
        
        wptr += (PACKSIZE + SEQSIZE);
      }
        
    memmove(buf, &buf[wptr], rptr - wptr);
    rptr -= wptr;
    wptr = 0;

    if(sl_uptime != 0 && rptr == 3 && !strncmp(buf, "END", 3))
      {
        lprintf(LOG_INFO, "end of data\n");
        disconnect();
        return 1;
      }

    return 0;
  }

