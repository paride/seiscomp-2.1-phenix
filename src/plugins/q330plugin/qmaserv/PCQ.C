/*
 * File     :
 *   Packet Compression Queue
 *
 * Purpose  :
 *  This is the program that returns the compressed packets.
 *  Once returned, the packets are handed off to comserv.
 *
 * Author   :
 *  Phil Maechling
 *
 * Mod Date :
 *  25 May 2002
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
 */
#include <iostream>
#include <string.h>
#include <stdio.h>
#include "QmaTypes.h"
#include "QmaLimits.h"
#include "QmaDiag.h"
#include "SecondOfData.h"
#include "PCQ.h"
#include "global.h"
#include "qma_mseed.h"
#include "UnpackComp.h"
#include "Cleanup.h"
#include "CharUtils.h"
#include "CreatePacket.h"

PCQ::PCQ()
{
  p_seconds_to_packetize = 0;
  p_previous_last_sample_in_packet = 0;
  p_seqno = 0;
  p_seconds_in_list = 0;
}


bool PCQ::setLCQVO(LCQVO vo)
{
  p_lcqvo = vo;
  p_seconds_to_packetize = 0;
  p_previous_last_sample_in_packet = 0;
  p_seqno = 0;
  p_seconds_in_list = 0;
  return true;
}


LCQVO PCQ::getLCQVO() const
{
  return p_lcqvo;
}

bool PCQ::addDataToQueue(SecondOfData& ds)
{
  if(!true)
  {
    std::cout << "--- Adding Second of Data to Packet Queue : " 
	<< p_seconds_in_list << " : " <<  p_lcqvo.getSEEDName() << std::endl;
  }
  if(p_seconds_in_list <= MAX_SECONDS_IN_COMPRESSION_QUEUE) 
  {
      p_list[p_seconds_in_list] = ds;
      ++p_seconds_in_list;
      g_outputPacketsQueued = true;  // set global flag used 
                                     //to reduce polling when queue is empty
      if(p_seconds_in_list > 1)
      {
        int res = ds.getBlocketteTimeInfo().drsn -
           p_list[p_seconds_in_list - 2].getBlocketteTimeInfo().drsn;

         int blockDiff = 0;
         double freq = p_lcqvo.getFrequencyHertz();
         if(freq < 1.0)
         {
	   // this triggers a rounding error (blockDiff becomes 9) on linux
	   //blockDiff = (int) (1.0/freq);

	   // Doing this prevents the rounding error
	   //double tmpDouble = 1.0/freq;
	   //blockDiff = (int) tmpDouble;

           blockDiff = -p_lcqvo.getRate();
	  

         }
         else
         {
          blockDiff = 1;
         }

         if(res != blockDiff)
         {
           if((res == 0) && (ds.completionPacket()))
           {
		// it was a completionPacket that was queued
           }
	   else
           {
             std::cout << "xxx Continuity Queueing blockette " <<
	       p_lcqvo.getSEEDName() << " Timestamp Diff = " << res 
		<< std::endl;
	     std::cout << "xxx blockDiff: " << blockDiff << std::endl;
	     std::cout << "xxx SecondsInList: " << p_seconds_in_list 
		<< std::endl;
             std::cout << "xxx DRSN: " << ds.getBlocketteTimeInfo().drsn 
		<< std::endl;
             std::cout << "xxx p_list[p_seconds_in_list]: " << 
		p_list[p_seconds_in_list].getBlocketteTimeInfo().drsn << 
		std::endl;
             return false; 
           }
         }
       }
      return true;
  }
  else
  {
      std::cout << "xxx Compression Queue is full :" << std::endl;
      std::cout << "xxx Channel : " << p_lcqvo.getSEEDName() << std::endl;
      return false;
  }
}


bool  PCQ::packetReady()
{  
  bool retVal = false;
  int secondsToPack = 0;
  int wordcount = 0;

  if(p_seconds_in_list == 0)
  {
    return retVal;
  }
  else if(p_list[0].completionPacket())
  {
    p_seconds_to_packetize = 1;
    wordcount = p_list[0].getNumberOfDataWords();
    if(g_verbosity.show(D_EVERYTHING,D_SPLITS))
    {
      std::cout << "+++ Completion Packet info - words: " << wordcount <<
	" blockettes: " << p_list[0].getNumberOfBlockettes() << 
        " type: " << p_list[0].getBlocketteType() << std::endl;
    }
    retVal = true;
  }
  else
  {
    for (int i = 0; i<p_seconds_in_list;i++)
    {
      wordcount = wordcount + p_list[i].getNumberOfDataWords();
      ++secondsToPack;
      if(wordcount == MAX_WORDS_IN_PACKET)
      {
        p_seconds_to_packetize = secondsToPack;
        retVal = true;
        if(!true)
        {
          std::cout << "--- Found exactly enough packets to compress : "
	  << p_lcqvo.getSEEDName() << std::endl;
          std::cout << "--- seconds in outputQueue " <<
	  p_seconds_in_list << std::endl;
          std::cout << "--- secondsToPack " << secondsToPack << std::endl;
        }
        break;
      }
      else if(wordcount > MAX_WORDS_IN_PACKET)
      {
        --secondsToPack;
        p_seconds_to_packetize = secondsToPack;
        retVal = true;
        if(!true)
        {
          std::cout << "--- Found more than required packets in Queue : " 
	  << p_lcqvo.getSEEDName() << std::endl;
          std::cout << "--- seconds in outputQueue " <<
	  p_seconds_in_list << std::endl;
          std::cout <<  "--- secondsToPack " << secondsToPack << std::endl;
        }
        break;
      }
    }
  }
  //
  // if it works through whole list without finding enough
  // blockettes return false
  //
  return retVal;
}


bool PCQ::compressPacket()
{
  bool res = createPacket(p_lcqvo,
	                  p_seconds_to_packetize,
                          p_previous_last_sample_in_packet,
                          p_seconds_in_list,
                          p_seqno,
                          p_miniseed_packet,
	                  p_list);
 
 return res;
}

char* PCQ::getPacket()
{
  return ((char*)&p_miniseed_packet[0]);
}

int PCQ::getSecondsInList()
{
  return p_seconds_in_list;
}

bool  PCQ::packetRemaining()
{  
  bool retVal = false;
  int secondsToPack = 0;
  int wordcount = 0;
  if(p_seconds_in_list == 0)
  {
    return retVal;
  }
  else if(p_list[0].completionPacket())
  {
    p_seconds_to_packetize = 1;
    wordcount = p_list[0].getNumberOfDataWords();
    if(g_verbosity.show(D_EVERYTHING,D_SPLITS))
    {
      std::cout << "+++ Completion Packet info - words: " << wordcount <<
	" blockettes: " << p_list[0].getNumberOfBlockettes() << 
        " type: " << p_list[0].getBlocketteType() << std::endl;
    }
    retVal = true;
  }
  else
  {
    for (int i = 0; i<p_seconds_in_list;i++)
    {
      wordcount = wordcount + p_list[i].getNumberOfDataWords();
      ++secondsToPack;
      p_seconds_to_packetize = secondsToPack;
      retVal = true;
      if(wordcount > MAX_WORDS_IN_PACKET)
      {
        --secondsToPack;
        p_seconds_to_packetize = secondsToPack;
        retVal = true;
	std::cout << "--- Found more than a full packet on packet remaining"
		  << std::endl;
        break;
      }
    }
    if(g_verbosity.show(D_EVERYTHING,D_PARTIALPACKETS))
    {
      std::cout << "--- Packet remaining found packets to write seqno: " <<
        p_seqno << " seconds of data " <<
	p_seconds_to_packetize << " for " <<
	getLCQVO().getSEEDName() << std::endl;
    }
  }
  //
  // if it works through whole list without finding enough
  // blockettes return false
  //
  return retVal;
}
