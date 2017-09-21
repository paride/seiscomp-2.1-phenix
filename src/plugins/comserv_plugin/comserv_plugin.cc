/***************************************************************************** 
 * comserv_plugin.cc
 *
 * SeedLink Comserv plugin
 *
 * (c) 2000 Andres Heinloo, GFZ Potsdam
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

#include <iostream>
#include <iomanip>
#include <string>
#include <set>
#include <cstring>
#include <cstdio>
#include <csignal>
#include <cerrno>

#include <unistd.h>
#include <syslog.h>
#include <termios.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#if defined(__GNU_LIBRARY__) || defined(__GLIBC__)
#include <getopt.h>
#endif

#include "qdefines.h"
#include "qutils.h"
#include "qtime.h"
#include "sdr_utils.h"
#include "ms_unpack.h"

#include "stuff.h"
#include "timeutil.h"
#include "seedstrc.h"
#include "service.h"

#include "confbase.h"
#include "conf_ini.h"
#include "confattr.h"
#include "cppstreams.h"
#include "utils.h"
#include "plugin.h"
#include "plugin_exceptions.h"
#include "diag.h"

#define MYVERSION   "2.0 (2005.212)"
#define CLIENT_NAME "SLNK"

using namespace std;
using namespace SeedlinkPlugin;
using namespace CfgParser;
using namespace CPPStreams;
using namespace Utilities;

namespace {

const int MAX_PLUGIN_NAME_LEN = 25;
const int POLL_USEC           = 1000000;
const int PACKET_SIZE         = 512;
const int HEADER_SIZE         = 64;
const int MAX_SAMPLES         = ((PACKET_SIZE - HEADER_SIZE) * 2);

const char *const ident_str = "SeedLink Comserv Plugin v" MYVERSION;

#if defined(__GNU_LIBRARY__) || defined(__GLIBC__)
const char *const opterr_message = "Try `%s --help' for more information\n";
const char *const help_message = 
    "Usage: %s [options] plugin_name\n"
    "\n"
    "'plugin_name' is the section name in config file; it is also used\n"
    "as a signature in log messages\n"
    "\n"
    "-v                            Increase verbosity level\n"
    "    --verbosity=LEVEL         Set verbosity level\n"
    "-D, --daemon                  Daemon mode\n"
    "-f, --config-file=FILE        Alternative configuration file\n"
    "-V, --version                 Show version information\n"
    "-h, --help                    Show this help message\n";
#else
const char *const opterr_message = "Try `%s -h' for more information\n";
const char *const help_message =
    "Usage: %s [options] plugin_name\n"
    "\n"
    "'plugin_name' is the section name in config file; it is also used\n"
    "as a signature in log messages\n"
    "\n"
    "-v             Increase verbosity level\n"
    "-D             Daemon mode\n"
    "-f FILE        Alternative configuration file\n"
    "-V             Show version information\n"
    "-h             Show this help message\n";
#endif

string plugin_name;
string cfifo_name;
string network_id;
int default_timing_quality = -1;
set<string> station_names;
int verbosity = 0;
bool daemon_mode = false, daemon_init = false;
volatile sig_atomic_t terminate_proc = 0;

void int_handler(int sig)
  {
    terminate_proc = 1;
  }

//*****************************************************************************
// Exceptions
//*****************************************************************************

class PluginCannotOpenFIFO: public PluginLibraryError
  {
  public:
    PluginCannotOpenFIFO(const string &fifo_name):
      PluginLibraryError(string() + "cannot open fifo '" + fifo_name + "'") {}
  };

//*****************************************************************************
// LogFunc
//*****************************************************************************

class LogFunc
  {
  public:
    enum { msglen = 200 };
    
    void operator()(int priority, const string &msg)
      {
        if(daemon_init)
          {
            syslog(priority, "%s", msg.c_str());
          }
        else
          {
            int verb = 2;
            
            switch(priority)
              {
              case LOG_EMERG:
              case LOG_ALERT:
              case LOG_CRIT:
              case LOG_ERR:
                verb = -1; break;

              case LOG_WARNING:
              case LOG_NOTICE:
                verb = 0; break;

              case LOG_INFO:
                verb = 1; break;

              case LOG_DEBUG:
                verb = 2;
              }

            if(verbosity < verb)
                return;

            time_t t = time(NULL);
            char *p = asctime(localtime(&t));
            string msgout = string(p, strlen(p) - 1) + " - " + plugin_name + ": " + msg;
            write(STDOUT_FILENO, msgout.c_str(), msgout.length());
          }
      }
  };

//*****************************************************************************
// TriggerBuffer
//*****************************************************************************

class StreamPacket
  {
  public:
    INT_TIME it;
    char data[PACKET_SIZE];
  };

class TriggerBufferHelper
  {
  private:
    const int buffer_length;
    const int pre_seconds;
    const int post_seconds;
    bool trigger_on;
    bool trigger_off_requested;
    INT_TIME trigger_off_time;
    list<rc_ptr<StreamPacket> > packets;

  public:
    const string station_name;
    
    TriggerBufferHelper(const string &station_name_init, int buffer_length_init,
      int pre_seconds_init, int post_seconds_init):
      buffer_length(buffer_length_init), pre_seconds(pre_seconds_init),
      post_seconds(post_seconds_init), trigger_on(false),
      trigger_off_requested(false), station_name(station_name_init) {}
      
    void set_trigger_on(int year, int month, int day,
      int hour, int minute, int second);

    void set_trigger_off(int year, int month, int day,
      int hour, int minute, int second);

    void push_packet(INT_TIME begin_time, void *pseed);
  };

void TriggerBufferHelper::set_trigger_on(int year, int month, int day,
  int hour, int minute, int second)
  {
    if(year   < 1900 || year   >   2099 ||
       month  <    1 || month  >     12 ||
       day    <    1 || day    >     31 ||
       hour   <    0 || hour   >     23 ||
       minute <    0 || minute >     59 ||
       second <    0 || second >     61)
       {
         logs(LOG_WARNING) << "trigger_on: invalid time" << endl;
         return;
       }

    EXT_TIME et;
    et.year = year;
    et.month = month;
    et.day = day;
    et.hour = hour;
    et.minute = minute;
    et.second = second;
    et.usec = 0;
    et.doy = mdy_to_doy(et.month, et.day, et.year);

    INT_TIME it = ext_to_int(et);

    for(; !packets.empty(); packets.pop_front())
      {
        if(int(tdiff(it, packets.front()->it) / 1000000.0) > pre_seconds)
            continue;

        int r = send_mseed(station_name.c_str(), packets.front()->data,
          PACKET_SIZE);
            
        if(r < 0) throw PluginBrokenLink(strerror(errno));
        else if(r == 0) throw PluginBrokenLink();
      }

    trigger_on = true;
    trigger_off_requested = false;
  }

void TriggerBufferHelper::set_trigger_off(int year, int month, int day,
  int hour, int minute, int second)
  {
    if(year   < 1900 || year   >   2099 ||
       month  <    1 || month  >     12 ||
       day    <    1 || day    >     31 ||
       hour   <    0 || hour   >     23 ||
       minute <    0 || minute >     59 ||
       second <    0 || second >     61)
       {
         logs(LOG_WARNING) << "trigger_off: invalid time" << endl;
         return;
       }

    EXT_TIME et;
    et.year = year;
    et.month = month;
    et.day = day;
    et.hour = hour;
    et.minute = minute;
    et.second = second;
    et.usec = 0;
    et.doy = mdy_to_doy(et.month, et.day, et.year);

    trigger_off_time = ext_to_int(et);
    trigger_off_requested = true;
  }

void TriggerBufferHelper::push_packet(INT_TIME begin_time, void *pseed)
  {
    while(!packets.empty() &&
      int(tdiff(begin_time, packets.front()->it) / 1000000.0) > buffer_length)
         packets.pop_front();

    if(trigger_on)
      {
        int r = send_mseed(station_name.c_str(), pseed, PACKET_SIZE);
            
        if(r < 0) throw PluginBrokenLink(strerror(errno));
        else if(r == 0) throw PluginBrokenLink();
      }
    else
      {
        StreamPacket* pack = new StreamPacket;
        pack->it = begin_time;
        memcpy(pack->data, pseed, PACKET_SIZE);
        packets.push_back(pack);
      }

    if(trigger_off_requested &&
      int(tdiff(begin_time, trigger_off_time) / 1000000.0) > post_seconds)
      {
        trigger_on = false;
        trigger_off_requested = false;
      }
  }

class TriggerBuffer
  {
  private:
    int buffer_length;
    int pre_seconds;
    int post_seconds;
    string current_station_name;
    map<string, rc_ptr<TriggerBufferHelper> > station_map;
    map<string, rc_ptr<TriggerBufferHelper> >::iterator current_station;

    void select_station(const string &station_name);

  public:
    const string stream_id;

    TriggerBuffer(const string &stream_id_init, int buffer_length_init,
      int pre_seconds_init, int post_seconds_init):
      buffer_length(buffer_length_init), pre_seconds(pre_seconds_init),
      post_seconds(post_seconds_init), stream_id(stream_id_init) {}

    void set_trigger_on(const string &station_name, int year,
      int month, int day, int hour, int minute, int second)
      {
        select_station(station_name);
        current_station->second->set_trigger_on(year, month, day,
          hour, minute, second);
      }
            
    void set_trigger_off(const string &station_name, int year,
      int month, int day, int hour, int minute, int second)
      {
        select_station(station_name);
        current_station->second->set_trigger_off(year, month, day,
          hour, minute, second);
      }
            
    void push_packet(const string &station_name, INT_TIME begin_time,
      void *pseed)
      {
        select_station(station_name);
        current_station->second->push_packet(begin_time, pseed);
      }
  };

void TriggerBuffer::select_station(const string &station_name)
  {
    if(station_name != current_station_name &&
      (current_station = station_map.find(station_name)) == station_map.end())
      {
        current_station = station_map.insert(make_pair(station_name,
          new TriggerBufferHelper(station_name, buffer_length,
            pre_seconds, post_seconds))).first;

        current_station_name = station_name;
      }
  }
  
//*****************************************************************************
// TriggerDef -- definition of element "trigger"
//*****************************************************************************

map<string, rc_ptr<TriggerBuffer> > trigger_buffers;

class TriggerDef: public CfgElement
  {
  private:
    string stream_id;
    int buffer_length;
    int pre_seconds;
    int post_seconds;
 
  public:
    TriggerDef(const string &name): CfgElement(name) {}
    rc_ptr<CfgAttributeMap> start_attributes(ostream &cfglog,
      const string &name);
    void end_attributes(ostream &cfglog);
  };

rc_ptr<CfgAttributeMap> TriggerDef::start_attributes(ostream &cfglog,
  const string &name)
  {
    if(trigger_buffers.find(name) != trigger_buffers.end())
      {
        cfglog << "stream \"" << name << "\" is already used" << endl;
        return NULL;
      }

    switch(name.length())
      {
      case 5:
        stream_id = name;
        break;

      case 3:
        stream_id = "  " + name;
        break;

      default:
        cfglog << "stream ID \"" << name << "\" has incorrect length" << endl;
        return NULL;
      }

    buffer_length = 60;
    pre_seconds = 20;
    post_seconds = 20;
    
    rc_ptr<CfgAttributeMap> atts = new CfgAttributeMap;
    atts->add_item(IntAttribute("buffer_length", buffer_length, 10, 600));
    atts->add_item(IntAttribute("pre_seconds", pre_seconds, 10, 600));
    atts->add_item(IntAttribute("post_seconds", post_seconds, 10, 600));
    return atts;
  }

void TriggerDef::end_attributes(ostream &cfglog)
  {
    trigger_buffers.insert(make_pair(stream_id,
      new TriggerBuffer(stream_id, buffer_length, pre_seconds, post_seconds)));
  }

//*****************************************************************************
// UnpackDef -- definition of element "unpack"
//*****************************************************************************

map<string, string> unpack_map;

class UnpackDef: public CfgElement
  {
  private:
    string stream_id;
    string target_id;
 
    set<string> targets;
    
  public:
    UnpackDef(const string &name): CfgElement(name) {}
    rc_ptr<CfgAttributeMap> start_attributes(ostream &cfglog,
      const string &name);
    void end_attributes(ostream &cfglog);
  };

rc_ptr<CfgAttributeMap> UnpackDef::start_attributes(ostream &cfglog,
  const string &name)
  {
    if(unpack_map.find(name) != unpack_map.end())
      {
        cfglog << "source stream  \"" << name << "\" is already used" << endl;
        return NULL;
      }

    switch(name.length())
      {
      case 5:
        stream_id = name;
        break;

      case 3:
        stream_id = "  " + name;
        break;

      default:
        cfglog << "stream ID \"" << name << "\" has incorrect length" << endl;
        return NULL;
      }

    rc_ptr<CfgAttributeMap> atts = new CfgAttributeMap;
    atts->add_item(StringAttribute("target_id", target_id));
    return atts;
  }

void UnpackDef::end_attributes(ostream &cfglog)
  {
    if(target_id.length() > PLUGIN_CIDLEN)
      {
        cfglog << "target channel name \"" << target_id <<
          "\" is too long" << endl;
        return;
      }

    if(targets.find(target_id) != targets.end())
      {
        cfglog << "target channel \"" << target_id <<
          "\" is already used" << endl;
        return;
      }

    unpack_map.insert(make_pair(stream_id, target_id));

    targets.insert(target_id);
  }

//*****************************************************************************
// Loading config file
//*****************************************************************************

void configure_plugin(const string &config_file)
  {
    list<string> station_list;
    
    rc_ptr<CfgAttributeMap> atts = new CfgAttributeMap;
    atts->add_item(StringAttribute("command_fifo", cfifo_name));
    atts->add_item(StringAttribute("network", network_id));
    atts->add_item(StringListAttribute("stations", station_list, ","));
    atts->add_item(IntAttribute("default_timing_quality",
      default_timing_quality, -1, 100));
    
    rc_ptr<CfgElementMap> elms = new CfgElementMap;
    elms->add_item(UnpackDef("unpack"));
    elms->add_item(TriggerDef("trigger"));
    
    logs(LOG_INFO) << "loading configuration from file '" << config_file << "'" << endl;
    read_config_ini(config_file, plugin_name, atts, elms);
    
    list<string>::iterator p;
    for(p = station_list.begin(); p != station_list.end(); ++p)
        station_names.insert(*p);
  }

//*****************************************************************************
// process_mseed()
//*****************************************************************************

void decoding_error(const char *station, DATA_HDR *hdr)
  {
    logs(LOG_ERR) << "error decoding Mini-SEED packet " <<
      hdr->seq_no << ", station " << hdr->station_id <<
      " (" << station << "), channel " << hdr->location_id <<
      hdr->channel_id << endl;
  }

void process_mseed(const char *station_name, void *pseed)
  {
    DATA_HDR *hdr;
    BLOCKETTE_HDR *bh;
    BS *bs;
    int nsamples;
    int32_t data[MAX_SAMPLES];

    if((hdr = decode_hdr_sdr((SDR_HDR *)pseed, PACKET_SIZE)) == NULL)
      {
        logs(LOG_ERR) << "invalid Mini-SEED packet, station " <<
          station_name << endl;

        return;
      }
    
    trim(hdr->station_id);
    trim(hdr->channel_id);
    trim(hdr->location_id);
    trim(hdr->network_id);
    
    int timing_quality = default_timing_quality, usec99 = 0;
    bool data_record = false;
    
    for(bs = hdr->pblockettes; bs != (BS *)NULL; bs = bs->next)
      {
        if(bs->wordorder != my_wordorder &&
          swab_blockette(bs->type, bs->pb, bs->len) == 0)
            bs->wordorder = my_wordorder;

        bh = (BLOCKETTE_HDR *) bs->pb;
        
        if(bh->type == 1000 && ((BLOCKETTE_1000 *)bh)->format != 0)
            data_record = true;

        if(bh->type == 1001)
          {
            timing_quality = ((BLOCKETTE_1001 *)bh)->clock_quality;
            usec99 = ((BLOCKETTE_1001 *)bh)->usec99;
          }
      }
    
    if(!data_record)
      {
        free_data_hdr(hdr);
        
        int r = send_mseed(station_name, pseed, PACKET_SIZE);
            
        if(r < 0) throw PluginBrokenLink(strerror(errno));
        else if(r == 0) throw PluginBrokenLink();

        return;
      }
            
    // string stream_id = string(hdr->location_id) + string(hdr->channel_id);
    char stream_id[10];
    sprintf(stream_id, "%2.2s%3.3s", hdr->location_id, hdr->channel_id);
      
    map<string, rc_ptr<TriggerBuffer> >::iterator p;
    
    try
      {
        if((p = trigger_buffers.find(stream_id)) != trigger_buffers.end())
          {
            p->second->push_packet(station_name, hdr->hdrtime, pseed);
          }
        else
          {
            int r = send_mseed(station_name, pseed, PACKET_SIZE);
            
            if(r < 0) throw PluginBrokenLink(strerror(errno));
            else if(r == 0) throw PluginBrokenLink();
          }
      }
    catch(exception &e)
      {
        free_data_hdr(hdr);
        throw;
      }

    map<string, string>::iterator q;
    if((q = unpack_map.find(stream_id)) == unpack_map.end())
      {
        free_data_hdr(hdr);
        return;
      }
    
    nsamples = ms_unpack(hdr, MAX_SAMPLES, (char *) pseed, data);
    free_data_hdr(hdr);
    
    if(nsamples < 0)
      {
        decoding_error(station_name, hdr);
        return;
      }
    
    // Already done by Qlib2!
    // INT_TIME it = add_time(hdr->hdrtime, 0, usec99);

    EXT_TIME et = int_to_ext(hdr->hdrtime);
    struct ptime pt;
    pt.year = et.year;
    pt.yday = et.doy;
    pt.hour = et.hour;
    pt.minute = et.minute;
    pt.second = et.second;
    pt.usec = et.usec;

    int r = send_raw3(station_name, q->second.c_str(), &pt,
      hdr->num_ticks_correction, timing_quality, data, nsamples);
    
    DEBUG_MSG("sent " << r << " bytes of data, station \"" << station <<
      "\", channel \"" << q->second << "\"" << endl);
    
    if(r < 0) throw PluginBrokenLink(strerror(errno));
    else if(r == 0) throw PluginBrokenLink();
  }
    
//*****************************************************************************
// Command FIFO
//*****************************************************************************

const int CMDLEN = 100;

char cmdbuf[CMDLEN + 1];
int cmdwp;
int cfifo_fd = -1;

void cfifo_setup()
  {
    if(cfifo_name.length() == 0) return;
    
//    unlink(cfifo_name.c_str());
    mknod(cfifo_name.c_str(), S_IFIFO | 0666, 0);
    
    if((cfifo_fd = open(cfifo_name.c_str(), O_RDONLY | O_NONBLOCK)) < 0)
        throw PluginCannotOpenFIFO(cfifo_name);

    cmdwp = 0;
  }

void cfifo_cleanup()
  {
    if(cfifo_fd < 0) return;
    
    close(cfifo_fd);
  }

void cfifo_request(const char *cmd)
  {
    int year, month, day, hour, min, sec;
    char trigger_state[4], station[6];
    bool trigger_on;
    
    if(strncasecmp(cmd, "TRIGGER ", 8))
      {
        logs(LOG_WARNING) << "invalid command: " << cmd << endl;
        return;
      }
    
    if(sscanf(cmd, "%*s %3s %5s %d %d %d %d %d %d", trigger_state,
      station, &year, &month, &day, &hour, &min, &sec) != 8)
      {
        logs(LOG_WARNING) << "wrong syntax: " << cmd << endl;
        return;
      }

    if(!strcasecmp(trigger_state, "ON"))
      {
        trigger_on = true;
      }
    else if(!strcasecmp(trigger_state, "OFF"))
      {
        trigger_on = false;
      }
    else
      {
        logs(LOG_WARNING) << "wrong syntax: " << cmd << endl;
        return;
      }
    
    logs(LOG_NOTICE) << "command accepted: " << cmd << endl;
    
    map<string, rc_ptr<TriggerBuffer> >::iterator p;
    for(p = trigger_buffers.begin(); p != trigger_buffers.end(); ++p)
      {
        if(trigger_on)
            p->second->set_trigger_on(station, year, month, day, hour, min, sec);
        else
            p->second->set_trigger_off(station, year, month, day, hour, min, sec);
      }
  }

void cfifo_check()
  {
    if(cfifo_fd < 0) return;
    
    fd_set cfifo_fdset;
    struct timeval tv;
    
    FD_ZERO(&cfifo_fdset);
    FD_SET(cfifo_fd, &cfifo_fdset);
    
    tv.tv_sec = 0;
    tv.tv_usec = 0;

    if(select(cfifo_fd + 1, &cfifo_fdset, NULL, NULL, &tv) <= 0 ||
      !FD_ISSET(cfifo_fd, &cfifo_fdset))
        return;

    int bytes_read;
    if((bytes_read = read(cfifo_fd, &cmdbuf[cmdwp], CMDLEN - cmdwp)) <= 0)
        return;

    cmdwp += bytes_read;
    cmdbuf[cmdwp] = 0;

    int cmdrp = 0, cmdlen, seplen;
    while(cmdlen = strcspn(cmdbuf + cmdrp, "\r\n"),
      seplen = strspn(cmdbuf + cmdrp + cmdlen, "\r\n"))
      {
        cmdbuf[cmdrp + cmdlen] = 0;
        cfifo_request(cmdbuf + cmdrp);
        cmdrp += (cmdlen + seplen);
      }
    
//    printf("cmdrp = %d, cmdlen = %d, seplen = %d\n", cmdrp, cmdlen, seplen);

    if(cmdlen >= CMDLEN)
      {
        logs(LOG_WARNING) << "command buffer overflow" << endl;
        cmdwp = cmdrp = 0;
        return;
      }
        
    memmove(cmdbuf, cmdbuf + cmdrp, cmdlen);
    cmdwp -= cmdrp;
    cmdrp = 0;
  }

//*****************************************************************************
// Comserv Setup
//*****************************************************************************

tclient_struc *client = NULL;

void comserv_setup(void)
  {
    tstations_struc tstations;
    cs_setup(&tstations, CLIENT_NAME, "*", TRUE, TRUE, 10, 5, 0xffff, 6000); 

    if(station_names.size() != 0)
      {
        for(int i = 0; i < tstations.station_count; ++i)
          {
            if(station_names.find(long_str(tstations.station_list[i].stationname.l)) ==
              station_names.end())
              {
                cs_remove(&tstations, i);
                --i;
              }
          }
      }
    
    if(!tstations.station_count)
      {
        logs(LOG_ERR) << "requested stations are not accessible via comserv" << endl;
        exit(1);
      }
    
    if((client = cs_gen (&tstations)) == (tclient_struc *) CSCR_PRIVATE)
      {
        logs(LOG_ERR) << "error creating shared memory segment" << endl;
        exit(1);
      }

    for (int i = 0; i < client->maxstation; ++i) 
      {
        ((tclient_station *)((char *)client + client->offsets[i]))->seqdbuf = CSQ_NEXT;
      }
  }

//*****************************************************************************
// Comserv Status
//*****************************************************************************

const char *comserv_status(unsigned int n)
  {
    static const char *const stats[13] =
      {
        "Good", "Enqueue Timeout", "Service Timeout", "Init Error",
        "Attach Refused", "No Data", "Server Busy", "Invalid Command",
        "Server Dead", "Server Changed", "Segment Error", "Command Size",
        "Privileged Command"
      };

    if(n > 12) return "Unknown";
    return stats[n];
  }
    
//*****************************************************************************
// Comserv Poll
//*****************************************************************************

void comserv_poll()
  {
    int n;
    unsigned char alert;
    
    while((n = cs_scan(client, &alert)) != NOCLIENT)
      {
        tclient_station *pstation;
        pstation = (tclient_station *)((char *)client + client->offsets[n]);

        if(alert)
            logs(LOG_INFO) << long_str(pstation->name.l) << " : status="
              << comserv_status(pstation->status) << endl;

        for(int i = 0; i < pstation->valdbuf; ++i)
          {
            tdata_user* dbuf = (tdata_user *)((char *)client + pstation->dbufoffset + 
                i * pstation->dbufsize);
            seed_record_header* pseed = (seed_record_header *) dbuf->data_bytes;

            int netlen = network_id.length();
            
            if(netlen != 0)
              {
                strncpy(pseed->seednet, network_id.c_str(), 2);
                if(netlen < 2) memset(pseed->seednet + netlen, 32, 2 - netlen);
              }
            
            process_mseed(long_str(pstation->name.l), dbuf->data_bytes);
          }
      }
  }

//*****************************************************************************
// Comserv Cleanup
//*****************************************************************************

void comserv_cleanup(void)
  {
    unsigned char alert;
    
    for (int i = 0; i < client->maxstation; ++i) 
      {
        ((tclient_station *)((char *)client + client->offsets[i]))->reqdbuf = 0;
      }
        
    logs(LOG_INFO) << "final scan to ack all received packets" << endl;
    
    cs_scan(client, &alert);
    cs_off(client);
  }

//*****************************************************************************
// get_progname()
//*****************************************************************************

string get_progname(char *argv0)
  {
    string::size_type pos;
    string s = argv0;
    if((pos = s.rfind('/')) != string::npos)
        s = string(argv0, pos + 1, string::npos);

    return s;
  }

} // unnamed namespace

namespace CPPStreams {

Stream logs = make_stream(LogFunc());

}

//*****************************************************************************
// Main
//*****************************************************************************

int main(int argc, char **argv)
try
  {
#if defined(__GNU_LIBRARY__) || defined(__GLIBC__)
    struct option ops[] = 
      {
        { "verbosity",      required_argument, NULL, 'X' },
        { "daemon",         no_argument,       NULL, 'D' },
        { "config-file",    required_argument, NULL, 'f' },
        { "version",        no_argument,       NULL, 'V' },
        { "help",           no_argument,       NULL, 'h' },
        { NULL }
      };
#endif

    string config_file = CONFIG_FILE;
    
    int c;
#if defined(__GNU_LIBRARY__) || defined(__GLIBC__)
    while((c = getopt_long(argc, argv, "vDf:Vh", ops, NULL)) != EOF)
#else
    while((c = getopt(argc, argv, "vDf:Vh")) != EOF)
#endif
      {
        switch(c)
          {
          case 'v': ++verbosity; break;
          case 'X': verbosity = atoi(optarg); break;
          case 'D': daemon_mode = true; break;
          case 'f': config_file = optarg; break;
          case 'V': cout << ident_str << endl;
                    exit(0);
          case 'h': fprintf(stdout, help_message, get_progname(argv[0]).c_str());
                    exit(0);
          case '?': fprintf(stderr, opterr_message, get_progname(argv[0]).c_str());
                    exit(1);
          }
      }

    if(optind != argc - 1)
      {
        fprintf(stderr, help_message, get_progname(argv[0]).c_str());
        exit(1);
      }

    plugin_name = string(argv[optind], 0, MAX_PLUGIN_NAME_LEN);
    
    struct sigaction sa;
    sa.sa_handler = int_handler;
    sa.sa_flags = SA_RESTART;
    N(sigemptyset(&sa.sa_mask));
    N(sigaction(SIGINT, &sa, NULL));
    N(sigaction(SIGTERM, &sa, NULL));
    
    sa.sa_handler = SIG_IGN;
    N(sigaction(SIGHUP, &sa, NULL));
    N(sigaction(SIGPIPE, &sa, NULL));
    
    if(daemon_mode)
      {
        logs(LOG_INFO) << ident_str << " started" << endl;
        logs(LOG_INFO) << "take a look into syslog files for more messages" << endl;
        openlog(plugin_name.c_str(), 0, SYSLOG_FACILITY);
        daemon_init = true;
      }

    redirect_ostream(cout, LogFunc(), LOG_INFO);
    redirect_ostream(cerr, LogFunc(), LOG_ERR);
    redirect_ostream(clog, LogFunc(), LOG_ERR);

    logs(LOG_NOTICE) << ident_str << " started" << endl;
    
    init_qlib2(0);
    configure_plugin(config_file);
    comserv_setup();
    cfifo_setup();
    
    for (int i = 0; i < client->maxstation; i++)
      {
        tclient_station *pstation;
        pstation = (tclient_station *)((char *)client + client->offsets[i]);
        logs(LOG_INFO) << long_str(pstation->name.l) << " : status="
          << comserv_status(pstation->status) << endl;
      }
    
    try
      {
        while(!terminate_proc)
          {
            comserv_poll();
            cfifo_check();
            usleep(POLL_USEC);
          }
      }
    catch(exception &e)
      {
        comserv_cleanup();
        cfifo_cleanup();
        throw;
      }

    comserv_cleanup();
    cfifo_cleanup();
    return 0;
  }
catch(exception &e)
  {
    logs(LOG_ERR) << e.what() << endl;
    return 1;
  }
catch(...)
  {
    logs(LOG_ERR) << "unknown exception" << endl;
    return 1;
  }
 
