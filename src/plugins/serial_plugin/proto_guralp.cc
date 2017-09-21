/***************************************************************************** 
 * proto_guralp.cc
 *
 * Guralp protocol implementation
 *
 * (c) 2002 Andres Heinloo, GFZ Potsdam
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

#include <iomanip>
#include <vector>
#include <algorithm>
#include <cstdlib>
#include <cstddef>

#include <unistd.h>
#include <time.h>
#include <sys/types.h>

#include "qtime.h"

#include "utils.h"
#include "cppstreams.h"
#include "serial_plugin.h"
#include "plugin_channel.h"
#include "diag.h"

#define STDC

extern "C" {
#include "gcf.h"
#include "gcf_term.h"
}

extern int gcf_error;

using namespace std;
using namespace Utilities;
using namespace CPPStreams;
using namespace SeedlinkPlugin;

namespace {

//*****************************************************************************
// GuralpProtocol
//*****************************************************************************

class GuralpProtocol: public Proto
  {
  private:
    int fd;
    map<string, rc_ptr<OutputChannel> > source_map;

    void get_gcf_message(GCFhdr *hdr);
    void get_gcf_error();
    void do_start();

  public:
    const string myname;

    GuralpProtocol(const string &myname_init): fd(-1), myname(myname_init) {}

    void attach_output_channel(const string &source_id,
      const string &channel_name, const string &station_name);
    void flush_channels();
    void start();
  };

void GuralpProtocol::attach_output_channel(const string &source_id,
  const string &channel_name, const string &station_name)
  {
    map<string, rc_ptr<OutputChannel> >::iterator p;

    if((p = source_map.find(source_id)) != source_map.end())
        throw PluginADInUse(source_id, p->second->channel_name);
    
    source_map[source_id] = new OutputChannel(channel_name, station_name,
      dconf.zero_sample_limit);
  }

void GuralpProtocol::flush_channels()
  {
    map<string, rc_ptr<OutputChannel> >::iterator p;
    for(p = source_map.begin(); p != source_map.end(); ++p)
        if(p->second != NULL) p->second->flush_streams();
  }

void GuralpProtocol::get_gcf_message(GCFhdr *hdr)
  {
    char *bp;
    size_t size;
    FILE* fp = open_memstream(&bp, &size);
    gcfhdr_printstatus(hdr, fp);
    fclose(fp);
    seed_log << bp << flush;
    free(bp);
  }

void GuralpProtocol::get_gcf_error()
  {
    char *msg;
    
    switch(gcf_error)
      {
      case ERR_SRATEBAD:
        msg = "the sample rate in the GCF packet is not an even number";
        break;

      case ERR_REVINTBAD:
        msg = "the reverse integration constant does not match the calculated value";
        break;

      case ERR_TIMEBAD:
        msg = "the GCF timecode is greater than 23:59:59 (in the number of seconds";
        break;

      case ERR_COMPCODEBAD:
        msg = "the GCF compression code is not 1, 2, or 4";
        break;

      case ERR_DURATNBAD:
        msg = "the block duration is not an even number of seconds";
        break;

      case ERR_SYSIDBAD:
        msg = "the system ID is suspicious and probably not valid";
        break;

      case ERR_FRSTDIFBAD:
        msg = "the first difference value is not zero";
        break;

      default:
        msg = "unknown error";
      }

    logs(LOG_WARNING) << "error decoding digitizer packet: " << msg << endl;
  }

void GuralpProtocol::start()
  {
    speed_t bps = dconf.port_bps;
    char *port_name;

    if(check_speed(&bps) < 0)
        throw PluginInvalidBaudrate(dconf.port_name, dconf.port_bps);

    if((port_name = (char *) malloc(dconf.port_name.length() + 1)) == NULL)
        throw bad_alloc();

    strcpy(port_name, dconf.port_name.c_str());
    fd = gcf_open(port_name, bps);
    free(port_name);

    if(fd < 0) throw PluginCannotOpenPort(dconf.port_name);

    try
      {
        do_start();
      }
    catch(PluginError &e)
      {
        seed_log << "closing device" << endl;
        close(fd);
        throw;
      }

    seed_log << "closing device" << endl;
    close(fd);
  }

void GuralpProtocol::do_start()
  {
    char *raw_buf;
    GCFhdr gcf_buf;
    
    while(!terminate_proc)
      {
        if(gcf_read(fd, &raw_buf) < 0)
            throw PluginReadError(dconf.port_name);

        if(gcfhdr_read(raw_buf, &gcf_buf, GCFTP) < 0)
          {
            get_gcf_error();
            continue;
          }
        
        if(gcf_buf.sample_rate == 0)
          {
            get_gcf_message(&gcf_buf);
            continue;
          }

        map<string, rc_ptr<OutputChannel> >::iterator p;
        if((p = source_map.find(gcf_buf.stream_id)) == source_map.end())
            continue;
        
        GTime gcf_time;
        memset(&gcf_time, 0, sizeof(GTime));
        if(gepoch2gmt(&gcf_buf.epoch, &gcf_time) < 0)
          {
            logs(LOG_WARNING) << "cannot convert time to GMT" << endl;
            continue;
          }

        EXT_TIME et;
        et.year = gcf_time.year;
        et.doy = gcf_time.day_of_year;
        et.hour = gcf_time.hour;
        et.minute = gcf_time.min;
        et.second = gcf_time.sec;
        et.usec = (gcf_time.msec * 1000) + gcf_time.usec;
        dy_to_mdy(et.doy, et.year, &et.month, &et.day);
        digitime.it = add_time(ext_to_int(et), 0, dconf.time_offset);
        
        p->second->set_timemark(digitime.it, 0, -1);

        for(int i = 0; i < gcf_buf.num_samps; ++i)
            if(dconf.lsb <= 8)
                p->second->put_sample(gcf_buf.data[i]);
            else
                p->second->put_sample(gcf_buf.data[i] >> (dconf.lsb - 8));
      }
  }

RegisterProto<GuralpProtocol> proto("guralp");

} // unnamed namespace

