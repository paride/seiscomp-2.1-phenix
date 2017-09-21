/***************************************************************************** 
 * qmacserv.cc
 *
 * Q330 comserv compatibility routines
 *
 * (c) 2004 Andres Heinloo, GFZ Potsdam
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

#include <string>
#include <list>
#include <set>

#include "qmacserv.h"
#include "confbase.h"
#include "conf_ini.h"
#include "confattr.h"
#include "plugin.h"

#include "qtime.h"
#include "qdefines.h"
#include "qutils.h"
#include "sdr_utils.h"
#include "ms_unpack.h"

using namespace std;
using namespace Utilities;
using namespace CfgParser;

namespace {

const int MAX_SAMPLES = (4096 - 64) * 2;

set<string> unpack_streams;

} // unnamed namespace

int getQmacfg(struct qma_cfg* out_cfg, char *configfile, char *section)
  {
    list<string> unpack_list;
    
    rc_ptr<CfgAttributeMap> atts = new CfgAttributeMap;
    atts->add_item(CharArrayAttribute("udpaddr", out_cfg->udpaddr, 80));
    atts->add_item(CharArrayAttribute("baseport", out_cfg->baseport, 80));
    atts->add_item(CharArrayAttribute("dataport", out_cfg->dataport, 80));
    atts->add_item(CharArrayAttribute("serialnumber", out_cfg->serialnumber, 80));
    atts->add_item(CharArrayAttribute("authcode", out_cfg->authcode, 80));
    atts->add_item(CharArrayAttribute("ipport", out_cfg->ipport, 80));
    atts->add_item(CharArrayAttribute("ipport", out_cfg->ipport, 80));
    atts->add_item(CharArrayAttribute("verbosity", out_cfg->verbosity, 80));
    atts->add_item(CharArrayAttribute("diagnostic", out_cfg->diagnostic, 80));
    atts->add_item(CharArrayAttribute("startmsg", out_cfg->startmsg, 80));
    atts->add_item(CharArrayAttribute("statusinterval", out_cfg->statusinterval, 80));
    atts->add_item(CharArrayAttribute("datarateinterval", out_cfg->datarateinterval, 80));
    atts->add_item(StringListAttribute("unpack_streams", unpack_list, ", "));

    clearConfig(out_cfg);

    try
      {
        read_config_ini(configfile, section, atts, new CfgElementMap);
      }
    catch(CfgCannotOpenFile &e)
      {
        cout << "Could not open file '" << configfile << "'" << endl;
        exit(1);
      }
    catch(CfgCannotFindSection &e)
      {
        cout << "Could not find section '" << section << "' in file '" <<
          configfile << "'" << endl;
        exit(1);
      }
    catch(exception &e)
      {
        cout << e.what() << endl;
        exit(1);
      }
    catch(...)
      {
        cout << "unknown exception" << endl;
        exit(1);
      }
        
    list<string>::iterator p;
    for(p = unpack_list.begin(); p != unpack_list.end(); ++p)
        unpack_streams.insert(*p);
    
    return 1;
  }

void clearConfig(struct qma_cfg* cfg)
  {
    strcpy(cfg->udpaddr,"");
    strcpy(cfg->ipport,"");
    strcpy(cfg->baseport,"");
    strcpy(cfg->dataport,"");
    strcpy(cfg->serialnumber,"");
    strcpy(cfg->authcode,"");
    strcpy(cfg->verbosity,"");
    strcpy(cfg->diagnostic,"");
    strcpy(cfg->startmsg,"");
    strcpy(cfg->statusinterval,"");
    strcpy(cfg->datarateinterval,"");
  }

int sendRecord(void *pseed, int recsize)
  {
    if(*(char *)pseed == 0) // empty packet
      {
        cout << "void packet" << endl;
        return 0;
      }
    
    DATA_HDR *hdr;
    if((hdr = decode_hdr_sdr((SDR_HDR *)pseed, recsize)) == NULL)
      {
        cout << "invalid Mini-SEED packet" << endl;
        return 0;
      }

    trim(hdr->station_id);
    trim(hdr->channel_id);
    trim(hdr->location_id);
    trim(hdr->network_id);
    
    BLOCKETTE_HDR *bh;
    BS *bs;
    int timing_quality = -1;
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
            timing_quality = ((BLOCKETTE_1001 *)bh)->clock_quality;
      }
    
    char stream_id[PLUGIN_CIDLEN + 1];
    snprintf(stream_id, PLUGIN_CIDLEN + 1, "%s%s", hdr->location_id,
      hdr->channel_id);
      
    set<string>::iterator p;
    if(!data_record ||
      (p = unpack_streams.find(stream_id)) == unpack_streams.end())
      {
        int r = send_mseed(hdr->station_id, pseed, recsize);

        free_data_hdr(hdr);

        if(r < 0)
          {
            cout << "error sending data to seedlink: " << strerror(errno) << endl;
            return -1;
          }
        else if(r == 0)
          {
            cout << "error sending data to seedlink" << endl;
            return -1;
          }

        return recsize;
      }
    
    int nsamples;
    int32_t data[MAX_SAMPLES];

    nsamples = ms_unpack(hdr, MAX_SAMPLES, (char *)pseed, data);

    if(nsamples < 0)
      {
        cout << "error decoding Mini-SEED packet " << hdr->seq_no <<
          ", station " << hdr->station_id << ", stream " << stream_id << endl;

        free_data_hdr(hdr);
        return 0;
      }

    EXT_TIME et = int_to_ext(hdr->hdrtime);
    struct ptime pt;
    pt.year = et.year;
    pt.yday = et.doy;
    pt.hour = et.hour;
    pt.minute = et.minute;
    pt.second = et.second;
    pt.usec = et.usec;

    int r = send_raw3(hdr->station_id, stream_id, &pt,
      hdr->num_ticks_correction, timing_quality, data, nsamples);
    
    free_data_hdr(hdr);

    if(r < 0)
      {
        cout << "error sending data to seedlink: " << strerror(errno) << endl;
        return -1;
      }
    else if(r == 0)
      {
        cout << "error sending data to seedlink" << endl;
        return -1;
      }

    return recsize;
  }
 
