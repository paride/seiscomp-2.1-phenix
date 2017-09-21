/***************************************************************************** 
 * java_plugin.cc
 *
 * Java->SeedLink interface for bud_plugin
 *
 * (c) 2004 Andres Heinloo, GFZ Potsdam
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

#include <cstdio>

#include "qtime.h"
#include "qdefines.h"
#include "qutils.h"
#include "sdr_utils.h"
#include "ms_unpack.h"

#include "plugin.h"

#include <gcj/cni.h>

namespace {

using namespace std;

const int MAX_SAMPLES = (4096 - 64) * 2;
const int WRITE_ERROR = -1;
const int SEED_ERROR  = -2;

//*****************************************************************************
// do_send_packet()
// Sends a Mini-SEED record to SeedLink.
//
// If recsize == 512, the record goes to SeedLink untouched, otherwise
// it is decoded and sent as raw data
//
// Return values:
// recsize: success
// 0: non-data record ignored
// WRITE_ERROR (-1): error sending data to SeedLink (fatal)
// SEED_ERROR (-2): error decoding Mini-SEED
//*****************************************************************************

int do_send_packet(void *pseed, int recsize)
  {
    DATA_HDR *hdr;
    if((hdr = decode_hdr_sdr((SDR_HDR *)pseed, recsize)) == NULL)
        return SEED_ERROR;

    trim(hdr->station_id);
    trim(hdr->channel_id);
    trim(hdr->location_id);
    trim(hdr->network_id);
    
    // Station ID
    // Make sure you have "station NET.STA" in seedlink.ini!
    char station[PLUGIN_SIDLEN + 1];
    snprintf(station, PLUGIN_SIDLEN + 1, "%s.%s", hdr->network_id,
      hdr->station_id);
    
    if(recsize == 512)
      {
        free_data_hdr(hdr);
        if(send_mseed(station, pseed, 512) <= 0)
            return WRITE_ERROR;

        return recsize;
      }

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
    
    if(!data_record)
      {
        free_data_hdr(hdr);
        return 0;
      }
        
    int nsamples;
    int32_t data[MAX_SAMPLES];

    nsamples = ms_unpack(hdr, MAX_SAMPLES, (char *)pseed, data);

    if(nsamples < 0)
      {
        free_data_hdr(hdr);
        return SEED_ERROR;
      }

    EXT_TIME et = int_to_ext(hdr->hdrtime);
    struct ptime pt;
    pt.year = et.year;
    pt.yday = et.doy;
    pt.hour = et.hour;
    pt.minute = et.minute;
    pt.second = et.second;
    pt.usec = et.usec;

    // Channel ID
    // Make sure you have "station NET.STA proc = ..." in seedlink.ini
    // and respective proc element in streams.xml!
    char channel[PLUGIN_CIDLEN + 1];
    snprintf(channel, PLUGIN_CIDLEN + 1, "%s.%s", hdr->location_id,
      hdr->channel_id);
    
    if(send_raw3(station, channel, &pt, hdr->num_ticks_correction,
      timing_quality, data, nsamples) <= 0)
      {
        free_data_hdr(hdr);
        return WRITE_ERROR;
      }

    free_data_hdr(hdr);
    return recsize;
  }

} // end unnamed namespace

//*****************************************************************************
// Java interface
//*****************************************************************************

namespace edu {
namespace iris {
namespace dmc {
namespace LISService {
namespace BudNetSocket {

jint send_packet(jbyteArray data)
  {
    return do_send_packet((void *)elements(data), data->length);
  }

} } } } } // end namespace edu::iris::dmc::LISService::BudNetSocket

