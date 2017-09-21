/*
 * File     :
 *   CheckPCQS.C
 *
 * Purpose  :
 *  This is the routines which checks to see if packet queues
 *   have current data in them, and if so, compresss and send it.
 *
 * Author   :
 *  Phil Maechling
 *
 * Mod Date :
 *   26 May 2002
 *   15 September 2003, Chad Trabant
 *   1 October 2004, Andres Heinloo
 *
 * This program is free software; you can redistribute it and/or modify
 * it with the sole restriction that:
 * You must cause any work that you distribute or publish, that in
 * whole or in part contains or is derived from the Program or any
 * part thereof, to be licensed as a whole at no charge to all third parties.
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 *
 */
#include <iostream>
#include <stdio.h>
#include "QmaTypes.h"
#include "QmaDiag.h"
#include "CheckPCQS.h"
#include "global.h"
#include "PCQ.h"
#include "Verbose.h"
#include "Cleanup.h"
#include "qmacserv.h"

extern Verbose g_verbosity;

void comlink_send(void *pseed, int recsize, int rectype)
{
  if(sendRecord(pseed, recsize) < 0)
    AbortQMA(1);
}

bool check_pcqs()
{
  if(g_outputPacketsQueued)
  {
    //
    // Walk through the digPCSs
    //  
    bool packetFound = true;
    while(packetFound)
    {
      packetFound = false;
      for(int i=0;i<g_number_of_diglcqs;i++)
      {
         if(g_digPCQ_list[i].packetReady())
         {
           g_digPCQ_list[i].compressPacket();
           char* pkt_ptr = g_digPCQ_list[i].getPacket();
           comlink_send(pkt_ptr,
		      BYTES_IN_MINISEED_PACKET,
		      DATA_PACKET);
	   if(g_verbosity.show(D_MINOR,D_COMSERV))
	   {
	     std::cout << "--- Data record sent to SeedLink :" 
		       << g_digPCQ_list[i].getLCQVO().getSEEDName() << ":"
		       << g_digPCQ_list[i].getLCQVO().getStationCode() << ":"
		       << g_digPCQ_list[i].getLCQVO().getNetworkCode()
		       << std::endl;
	   }
           packetFound = true;
	 }
      }
    }

    //
    // Now work through the mainPCQS
    //
    packetFound = true;
    while(packetFound)
    {
      packetFound = false;
      for(int i=0;i<g_number_of_mainlcqs;i++)
      {
         if(g_mainPCQ_list[i].packetReady())
         {
           g_mainPCQ_list[i].compressPacket();
           char* pkt_ptr = g_mainPCQ_list[i].getPacket();
           comlink_send(pkt_ptr,
		      BYTES_IN_MINISEED_PACKET,
		      DATA_PACKET);
	   if(g_verbosity.show(D_MINOR,D_COMSERV))
	   {
	     std::cout << "--- Data record sent to SeedLink :" 
		       << g_mainPCQ_list[i].getLCQVO().getSEEDName() << ":"
		       << g_mainPCQ_list[i].getLCQVO().getStationCode() << ":"
		       << g_mainPCQ_list[i].getLCQVO().getNetworkCode()
		       << std::endl;
	   }
           packetFound = true;
	 }
      }
    }
  }
  g_outputPacketsQueued = false;
  return true;
}


void empty_pcqs()
{
  bool res = check_pcqs();
  bool packetFound = true;
  while(packetFound)
  {
      packetFound = false;
      for(int i=0;i<g_number_of_diglcqs;i++)
      {
         if(g_digPCQ_list[i].packetRemaining())
         {
           g_digPCQ_list[i].compressPacket();
           char* pkt_ptr = g_digPCQ_list[i].getPacket();
           comlink_send(pkt_ptr,
		      BYTES_IN_MINISEED_PACKET,
		      DATA_PACKET);
	   if(g_verbosity.show(D_MINOR,D_PARTIALPACKETS))
	   {
	     std::cout << "--- Partially filled data record sent to SeedLink :" 
		       << g_digPCQ_list[i].getLCQVO().getSEEDName() << ":"
		       << g_digPCQ_list[i].getLCQVO().getStationCode() << ":"
		       << g_digPCQ_list[i].getLCQVO().getNetworkCode()
		       << std::endl;
	   }
           packetFound = true;
	 }
      }
  }

  //
  // Now write out the mainPCQs
  //
  packetFound = true;
  while(packetFound)
  {
      packetFound = false;
      for(int i=0;i<g_number_of_mainlcqs;i++)
      {
         if(g_mainPCQ_list[i].packetRemaining())
         {
           g_mainPCQ_list[i].compressPacket();
           char* pkt_ptr = g_mainPCQ_list[i].getPacket();
           comlink_send(pkt_ptr,
		      BYTES_IN_MINISEED_PACKET,
		      DATA_PACKET);
	   if(g_verbosity.show(D_MINOR,D_PARTIALPACKETS))
	   {
	     std::cout << "--- Partially filled data record sent to SeedLink :" 
		       << g_mainPCQ_list[i].getLCQVO().getSEEDName() << ":"
		       << g_mainPCQ_list[i].getLCQVO().getStationCode() << ":"
		       << g_mainPCQ_list[i].getLCQVO().getNetworkCode()
		       << std::endl;
	   }
           packetFound = true;
	 }
      }
  }
}
