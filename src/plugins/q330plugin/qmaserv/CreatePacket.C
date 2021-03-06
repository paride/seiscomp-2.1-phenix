/*
 * File     :
 *  CreatePacket.h
 *
 * Purpose  :
 *   Create a packet to hand to comserv
 *
 * Author   :
 *  Phil Maechling
 *
 * Mod Date :
 *  14 July 2002
 *  15 September 2003, Chad Trabant
 *  1 October 2004, Andres Heinloo
 *  28 April 2005, Andres Heinloo
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
#include <string>
#include <map>
#include <string.h>
#include "CreatePacket.h"
#include "QmaLimits.h"
#include "QmaTypes.h"
#include "CharUtils.h"
#include "qma_mseed.h"
#include "Cleanup.h"
#include "UnpackComp.h"
#include "TimeServer.h"
#include "global.h"
#include "FrameUtils.h"
#include "QmaDiag.h"
#include "ClockUtils.h"
#include "qmaswap.h"

bool createPacket(const LCQVO& vo,
                  int          seconds_to_packetize,
                  qma_int32&   p_previous_last_sample_in_packet,
                  int&         p_seconds_in_list,
                  qma_uint16&  p_seqno,
                  char         p_miniseed_packet[],
                  SecondOfData p_list[])
{

  //
  // The assumption when this routine is called is that there is data in
  // p_list which should be packetized.
  //

  QMAMINISEED packet;
  qma_int32 packetType;
  qma_int32 mapArray[7];
  qma_uint32 x0val,xnval;
  char header[64];
  char *hptr;
  hptr = (char*) &header;
  
  char *ptr;

  ptr = vo.getLocationCode();
  std::string locationCode;
  for(int n = 2; n > 0; --n)
    {
      if(ptr[n - 1] != ' ')
        {
          locationCode = std::string(ptr, 0, n);
          break;
        }
    }

  ptr = vo.getSEEDName();
  std::string SEEDName;
  for(int n = 3; n > 0; --n)
    {
      if(ptr[n - 1] != ' ')
        {
          SEEDName = std::string(ptr, 0, n);
          break;
        }
    }

  StreamDescriptor sd(locationCode, SEEDName, SLDATA);
  BTI bti = p_list[0].p_blockette_array[0].getBlocketteTime();
  
  std::map<StreamDescriptor, BTI>::iterator streamTimeIter;
  if((streamTimeIter = g_streamTimeMap.find(sd)) != g_streamTimeMap.end())
  {
    SecondOfData* p_list1 = p_list;
    
    while(seconds_to_packetize > 0)
    {
      if(bti.drsn > streamTimeIter->second.drsn)
        break;
        
      if(streamTimeIter->second.drsn - bti.drsn > 100)
      {
        std::cout << "--- DRSN difference is too large, resetting DRSN for"
          " stream " << sd.to_string() << std::endl;
        break;
      }
      
      std::cout << "--- Removing 1 second from stream " << sd.to_string() <<
       " due to overlap" << std::endl;

      ++p_list1;
      --seconds_to_packetize;
      --p_seconds_in_list;
      bti = p_list1[0].p_blockette_array[0].getBlocketteTime();
    }

    if(p_list != p_list1)
    {
      for(int z=0;z<p_seconds_in_list;z++)
        p_list[z] = p_list1[z];
    }
  }
  
  if(seconds_to_packetize == 0)
  {
    memset(p_miniseed_packet, 0, 512);
    return true;
  }
  
  g_streamTimeMap[sd] = bti;

  SecondOfData firstsod = p_list[0];
  packetType = firstsod.getBlocketteType();

  //
  // results are based on ORING so reset all to 0 to start.
  //
  memset(&packet.data[64],0,4);
  memset(&packet.data[128],0,4);
  memset(&packet.data[192],0,4);
  memset(&packet.data[256],0,4);
  memset(&packet.data[320],0,4);
  memset(&packet.data[384],0,4);
  memset(&packet.data[448],0,4);

  bool retValue = false;

  if(seconds_to_packetize > p_seconds_in_list)
  {
    if(true)
    {
      std::cout <<
      "xxx Number of Words to compress exceed number of words in queue."
        << std::endl;
      std::cout << "xxx Resetting Acquisition due to this error." << std::endl;
        g_stateMachine.setState(Resetting);
     return retValue;
    }
  } 
  //
  // Determine number of diffs to pack

  int samplesInPacket = 0;
  int framesInPacket = 0;
  if(packetType == DC_D32)
  {
    retValue = packDC32(vo,
                        seconds_to_packetize,
                        p_previous_last_sample_in_packet,
                        p_seconds_in_list,
                        samplesInPacket,
                        framesInPacket,
                        p_seqno,
                        p_miniseed_packet,
                        p_list,
	                mapArray,
	                packet);

    createHeader(vo,
               firstsod.p_blockette_array[0],
               seconds_to_packetize,
               samplesInPacket,
	       framesInPacket, 
	       p_seqno,
               hptr);

    packHeader(packet,header,p_miniseed_packet);

    retValue = true;

  }
  else // All SOD converted to DC_COMPS at this point
  {
    //
    // Determine number of diffs to pack
    //
    int totalWordsInPacket = 0;
    for(int z=0;z<seconds_to_packetize;z++)
    {
      int dataWords = p_list[z].getNumberOfDataWords();
      totalWordsInPacket = totalWordsInPacket + dataWords;
    }

    if(totalWordsInPacket <= MAX_WORDS_IN_PACKET)
    {
      retValue = packDCComp(vo,
                            seconds_to_packetize,
                            p_previous_last_sample_in_packet,
                            p_seconds_in_list,
			    samplesInPacket,
                            framesInPacket,	
                            p_seqno,
                            p_miniseed_packet,
                            p_list,
	                    mapArray,
	                    packet);

      createHeader(vo,
                   firstsod.p_blockette_array[0],
                   seconds_to_packetize,
                   samplesInPacket,
                   framesInPacket,
	           p_seqno,
                   hptr);
      
      packHeader(packet,header,p_miniseed_packet);

      retValue = true;
    }
    else
    {
      std::cout << "xxx Unexpected need to split packet." << std::endl;
      retValue = false;
    }// Done with if Words > MAX_WORDS_IN_Packet
  } // Done with If DC32 or DCComp
  return retValue;
}


//
// Create a 64-byte, network byte ordered, data record header including
// a 1000 and 1001 blockette.
//

void createHeader(const       LCQVO& vo,
		              Blockette& b,
                  const       int& p_seconds_to_packetize,
                  const       int& samplesInPacket,
                  const       int& framesInPacket,
                  qma_uint16& p_seqno,
		  char*       inframe)
{

  //
  // Clear header frame for incoming data
  //
  memset(inframe,0,64);

  //
  // Next seqno
  //
  ++p_seqno;
  capnint((char*)&inframe[0],p_seqno,6);
  // Based on Solaris compilers, use char stings in capnstr calls
  // rather than literals
  char letterD[2] = "D";
  char blankSpace[2] = " ";
  capnstr((char*)&inframe[6],letterD,1);
  capnstr((char*)&inframe[7],blankSpace,1);
  capnstr((char*)&inframe[8],vo.getStationCode(),5);
  capnstr((char*)&inframe[13],vo.getLocationCode(),2);
  capnstr((char*)&inframe[15],vo.getSEEDName(),3);
  capnstr((char*)&inframe[18],vo.getNetworkCode(),2);

  b.setFilterDelay(vo.getFilterDelay());
  timeval tt = g_timeServer.convertBTI2Timeval(b.getBlocketteTime());

  char btime[10];
  g_timeServer.timeval2sdr(tt,btime);
  memcpy((char*)&inframe[20],btime,10);

  if(!true)
  {
    time_t val = tt.tv_sec;
    std::cout << "--- Packet time: " << ctime(&val);
  }

  //
  // Set number of samples
  //

  qma_uint16 nsamps = (qma_uint16) samplesInPacket;
  qma_uint8 fcount = (qma_uint8) framesInPacket;

  if(g_verbosity.show(D_EVERYTHING,D_FRAME_COUNT))
  {
    std::cout << "--- Packet Samples: " << (qma_uint16) samplesInPacket << 
     " in Frames: " << (qma_uint16) fcount << std::endl;
  }

  qma_uint16 trate = vo.getSamplesPerBlockette();
  qma_uint16 nsamps2 = p_seconds_to_packetize * trate;

  if(nsamps != nsamps2)
  {
    if(g_verbosity.show(D_EVERYTHING,D_SPLITS))
    {
      std::cout << "+++ Packing a Split packet with samples in Packet: " <<
	nsamps << " and number in this blockette: " << nsamps2 << std::endl;
      std::cout << "+++ for channel: " << vo.getSEEDName() << std::endl; 
    }
  }

  qma_uint16 tmp_nsamps = qma_htons(nsamps);
  memcpy((char*)&inframe[30],&tmp_nsamps,2);

  //
  // set rate
  //
  if(vo.getFrequencyHertz() < 1.0)
  {
    qma_int16 signed_rate = qma_htons(vo.getRate());
    memcpy((char*)&inframe[32],&signed_rate,2);
    // set mult factor
    qma_int16 multfac = qma_htons(1);
    memcpy((char*)&inframe[34],&multfac,2);
  }
  else
  {
    qma_uint16 tmp_trate = qma_htons(trate);
    memcpy((char*)&inframe[32],&tmp_trate,2);
    // set mult factor
    qma_uint16 multfac = qma_htons(1);
    memcpy((char*)&inframe[34],&multfac,2);
  }

  //
  // Call to find Timing Quality done here
  // for use in IOFlags flags
  //

  qma_uint8 tqual = translate_clock(g_tvo.getClockProcVO(),
                                    g_timeServer.getClockQuality(),
                                    g_timeServer.getMinutesSinceLoss());

  //
  // Activity flag
  //
  qma_uint8 actflag = 0;
  qma_uint16 calStat = g_timeServer.getCalibrationStatus();
  if(calStat != 0)
  {
   actflag = CALIBRATION_ON;
  }
  else
  {
   actflag = CALIBRATION_OFF;
  }
  memcpy((char*)&inframe[36],&actflag,1);

  //
  // IO Flag
  qma_uint8 ioflag = 0;
  if(tqual >= g_tvo.getClockProcVO().getPLLOffQuality())
  {
    ioflag = SIF_LOCKED;
  }
  else
  {
    ioflag = 0;
  }
  memcpy((char*)&inframe[37],&ioflag,1);

  //
  // SEED Quality
  qma_uint8 dqualflag = 0;
  if(tqual < g_tvo.getClockProcVO().getLowestHasBeenLocked())
  {
    dqualflag = SQF_QUESTIONABLE_TIMETAG;
  }
  else
  {
    dqualflag = 0;
  }
  memcpy((char*)&inframe[38],&dqualflag,1);

  //
  // Number of blockettes
  //
  qma_uint8 numblockettes =2;
  memcpy((char*)&inframe[39],&numblockettes,1);

  //
  // Time correction
  //
  qma_uint32 correction = qma_htonl(0);
  memcpy((char*)&inframe[40],&correction,4);

  //
  // Beginning of Data
  //
  qma_uint16 datastart = qma_htons(64);
  memcpy((char*)&inframe[44],&datastart,2);

  //
  // Beginning of HeaderData
  //
  qma_uint16 blockstart = qma_htons(48);
  memcpy((char*)&inframe[46],&blockstart,2);

  //
  // Add blockette 1000
  //

  qma_uint16 blocktype = qma_htons(1000);
  memcpy((char*)&inframe[48],&blocktype,2);

  blocktype = qma_htons(56);
  memcpy((char*)&inframe[50],&blocktype,2);

  qma_uint8 dformat = 11;
  memcpy((char*)&inframe[52],&dformat,1);

  qma_uint8 worder = 1;
  memcpy((char*)&inframe[53],&worder,1);

  qma_uint8 drelen = 9;
  memcpy((char*)&inframe[54],&drelen,1);

  qma_uint8 pad = 0;
  memcpy((char*)&inframe[55],&pad,1);


  // Add blockette 1001

  blocktype = qma_htons(1001);
  memcpy((char*)&inframe[56],&blocktype,2);

  blocktype = qma_htons(0);
  memcpy((char*)&inframe[58],&blocktype,2);

  memcpy((char*)&inframe[60],&tqual,1);

  // Find usec value for blockette 1001
  qma_uint8 usec = g_timeServer.getUsecsFromTimeval(tt);
  memcpy((char*)&inframe[61],&usec,1);

  qma_uint8 reservedID = QMA_BLOCKETTE_1001_ID;
  memcpy((char*)&inframe[62],&reservedID,1);

  memcpy((char*)&inframe[63],&fcount,1);
  
  return; 
}



void packHeader(QMAMINISEED& packet,
	        char header[],
	        char p_miniseed_packet[])
{
  qma_int32 temp;
  qma_int32 temp2;
  qma_int32 t2; 
  qma_int32 t3;

  memcpy((char*)&packet.data[0],(char*)&header[0],64);

  memcpy((char*)&temp,(char*)&packet.data[68],4);
  memcpy((char*)&temp2,(char*)&packet.data[72],4);
    memcpy((char*)&p_miniseed_packet[0],(char*)&packet.data[0],
	BYTES_IN_MINISEED_PACKET);

  memcpy((char*)&t2,(char*)&p_miniseed_packet[68],4);
  memcpy((char*)&t3,(char*)&p_miniseed_packet[72],4);
  if(temp != t2)
  {
      std::cout << "--- x0 in packet does not equal x0 in : " << temp << 
	" " << t2 << std::endl;
  }
  return;
}


bool packDCComp(const LCQVO& vo,
                const int    seconds_to_packetize,
                qma_int32&   p_previous_last_sample_in_packet,
                int&         p_seconds_in_list,
                int&         samplesInPacket,
                int&         framesInPacket,
                qma_uint16&  p_seqno,
                char         p_miniseed_packet[],
                SecondOfData p_list[],
                qma_int32    mapArray[],
	        QMAMINISEED& packet)
{

  qma_int32 x0,xn;
  SecondOfData sod;

  int currentWordsInPacket = 0;
  int currentWordsInFrame = 0;
  int totalWordsInPacket = 0;
  int destFrameNumber = 0;

  int numberOfSecondsToPack = seconds_to_packetize;

  //
  // Determine number of diffs to pack
  //
  for(int z=0;z<numberOfSecondsToPack;z++)
  {
      int dataWords = p_list[z].getNumberOfDataWords();
      totalWordsInPacket = totalWordsInPacket + dataWords;
  }

  qma_int32 diffbuf[532],*diffptr;
  qma_int32 databuf[532],*dataptr;
  diffptr = diffbuf;
  dataptr = databuf;

  SecondOfData firstsod = p_list[0];
  int totalWordsInSecond = 0;
  int totalSamplesInPacket = 0;

  for(int xy=0;xy<numberOfSecondsToPack;xy++)
  {
      SecondOfData sod = p_list[xy];
      qma_int32 packetType = sod.getBlocketteType();
      totalWordsInSecond = sod.getNumberOfDataWords();	

      QMABLOCK dblock;

      //
      // Before processing each second of data, check if it
      // is the first or last
      // blockette and get x0, or xn out of the blockette
      //

      if(xy==0)
      {

	if(packetType == DC_MULT)
	{
	  std::cout << "xxx Found MULT PACKET to pack" << std::endl;
          return false;
	}
	else
	{
          dblock = sod.p_blockette_array[0].getBlockette();
          int res = unpack_DCComp(&dblock,
                                   diffptr,
                                   dataptr);
           if(res > 0)
           {
	      x0 = qma_htonl(databuf[0]);
           }
           else
           {
		std::cout << "xxx Error unpacking blockette" << std::endl;
           }
        }
      }

      //
      // Check each time through, including the first packet, 
      // in case the first blockette is also the last blockette.

      if(xy == (numberOfSecondsToPack-1))
      {
           dblock = sod.p_blockette_array[0].getBlockette();
           int num = sod.p_blockette_array[0].getBlockette().numberOfSamples;
           int res = unpack_DCComp(&dblock,
                                   diffptr,
                                   dataptr);
           if(res > 0)
           {
	     xn = qma_htonl(databuf[(num-1)]);
           }
           else
           {
		std::cout << "xxx Error unpacking blockette" << std::endl;
           }
      }

      //
      // Now for each word in the current second, 
      // transfer a word and maps to the packet
      //
      int currentWordsInSecond = 0;
      int srcWordPosition=0;
      int srcMapPosition=0;
          destFrameNumber=0;
      int destWordPosition=0;
      qma_uint32 destMap = 0;

  
      while(currentWordsInSecond < totalWordsInSecond)
      {
	dblock = sod.p_blockette_array[0].getBlockette();
	findFrameMapAndWord(vo,
                            currentWordsInSecond,
			    currentWordsInPacket,
			    dblock.map,	
			    dblock.mapLengthInBytes,
			    destFrameNumber,
			    destWordPosition,
			    destMap);

        if(destFrameNumber == 1)
        {
	  qma_uint32 curMap;
          qma_uint32 tempMap =0;
	  memcpy((char*)&curMap,(char*)&packet.data[64],4);
	  curMap = qma_ntohl(curMap);

	  tempMap = curMap | destMap;       
	  tempMap = qma_htonl(tempMap);

          memcpy((char*)&packet.data[64],(char*)&tempMap,4);

	  qma_int32 tmpWord;
	  memcpy(&tmpWord, (char *)&dblock.w[srcWordPosition], 4);
	  //tmpWord = qma_ntohl(tmpWord);
	  memcpy((char *)&packet.data[64+(destWordPosition * 4)], &tmpWord, 4);
	    
        }
        else
        { 

	  qma_uint32 curMap;
          qma_uint32 tempMap =0;
	  memcpy((char*)&curMap,
                 (char*)&packet.data[128+((destFrameNumber-2)*64)],4);

	  curMap = qma_ntohl(curMap);

	  tempMap = curMap | destMap;       
	  tempMap = qma_htonl(tempMap);

          memcpy((char*)&packet.data[128+((destFrameNumber-2)*64)],
                 (char*)&tempMap,4);

	  qma_uint32 tmpWord;
	  memcpy(&tmpWord, (char *)&dblock.w[srcWordPosition], 4);
	  memcpy((char*)&packet.data[128+((destFrameNumber-2)*64)+(destWordPosition*4)], &tmpWord, 4);
        } 

        //
        // Keep running total of number of data words packed
        // for entire packet
        //
	++srcWordPosition;
	++currentWordsInSecond;
	++currentWordsInPacket;

      } // End while packing data words in a second of data

      totalSamplesInPacket = dblock.numberOfSamples + totalSamplesInPacket;

    } // End packing all seconds

    //
    // Set the return value;
    samplesInPacket = totalSamplesInPacket;
    framesInPacket = destFrameNumber;
    //
    // Consistency check. Should exit when enough words are packed
    //
    if (currentWordsInPacket != totalWordsInPacket)
    {
	std::cout << "xxx Did not pack all expected data workds : actual : " 
	  << currentWordsInPacket << " expected " << totalWordsInPacket << 
		std::endl;
    }

    //
    // Complete filling out the packet with x0,xn values;
    //

    memcpy((char*)&packet.data[68],&x0,4);
    memcpy((char*)&packet.data[72],&xn,4);
    //
    //
    // Remove sent blockettes from p_list;
    //
    //

    int unsent = p_seconds_in_list - numberOfSecondsToPack;

    for(int x=0;x<unsent;x++)
    {
      p_list[x] = p_list[numberOfSecondsToPack+x];
    }

    p_seconds_in_list = unsent;

    return true;
}

bool packDC32(const LCQVO& vo,
              const int    seconds_to_packetize,
              qma_int32&   p_previous_last_sample_in_packet,
              int&         p_seconds_in_list,
              int&         samplesInPacket,
              int&         framesInPacket,
              qma_uint16&  p_seqno,
              char         p_miniseed_packet[],
              SecondOfData p_list[],
              qma_int32    mapArray[],
	      QMAMINISEED& packet)
{


  qma_int32 x0,xn;
  SecondOfData sod;

  int currentWordsInPacket = 0;
  int destFrameNumber = 0;
  int currentWordsInFrame = 0;
  int totalWordsInPacket = 0;

  int numberOfSecondsToPack = seconds_to_packetize;
  int totalSamplesInPacket = 0;

  qma_int32 diffs[103];
  qma_int32 prevsample = 0;
  for(int i=0;i<numberOfSecondsToPack;i++)
  {
      totalSamplesInPacket = 
        p_list[i].p_blockette_array[0].getBlockette().numberOfSamples + 
          totalSamplesInPacket;

      if(i==0)
      {
        x0 = p_list[i].p_blockette_array[0].getBlockette().w[0];
	prevsample = p_previous_last_sample_in_packet;
      }
      else if (i == (numberOfSecondsToPack -1))
      {
        xn = p_list[i].p_blockette_array[0].getBlockette().w[0];
	prevsample =  p_list[i-1].p_blockette_array[0].getBlockette().w[0];
	p_previous_last_sample_in_packet = xn;
      }
      else
      {	
	prevsample =  p_list[i-1].p_blockette_array[0].getBlockette().w[0];
      }
        
      qma_int32 val = p_list[i].p_blockette_array[0].getBlockette().w[0];
      diffs[i] = val - prevsample;
      if(!true)
      {
        if(i==0)
        {
          std::cout << "+++ Channel "<< vo.getSEEDName() 
	  << " x0 = x0-1 + d0 " << (prevsample + diffs[0]) << " should equal " 
	  << val << " or " << x0 << std::endl;
        }
      }
  }

  if(!true)
  {
      std::cout << "--- Samples for channel: " << vo.getSEEDName() << std::endl;
      for(int x=0;x<numberOfSecondsToPack;x++)
      {
	std::cout << "--- Diff number: " << x << " " << diffs[x] << std::endl;
      }
  }

    //
    // For DC_D32 blockettes, the blockettes to packetize and the
    // the seconds to packetize will be equal.
    //
  for(int i = 0;i<numberOfSecondsToPack;i++)
  {    
      qma_uint32 uval = 0; 
      qma_uint32 sval = (qma_uint32) diffs[i];
      uval = (sval & 0x3fffffff);
      uval = uval | 0x40000000;

      if(currentWordsInPacket == 0)
      {
	destFrameNumber = 1;
        currentWordsInFrame = 0;
      }
      else if(currentWordsInPacket == 13)
      {
	destFrameNumber = 2;
        currentWordsInFrame = 0;
      }
      else if(currentWordsInPacket == 28)
      {	
 	destFrameNumber = 3;
        currentWordsInFrame = 0;
      }
      else if(currentWordsInPacket == 43)
      {	
 	destFrameNumber = 4;
        currentWordsInFrame = 0;
      }
      else if(currentWordsInPacket == 58)
      {	
 	destFrameNumber = 5;
        currentWordsInFrame = 0;
      }
      else if(currentWordsInPacket == 73)
      {	
 	destFrameNumber = 6;
        currentWordsInFrame = 0;
      }
      else if(currentWordsInPacket == 88)
      {	
 	destFrameNumber = 7;
        currentWordsInFrame = 0;
      }
      else if(currentWordsInPacket >=103)
      {
	std::cout << "xxx Error more than 103 words in packet" << std::endl;
      }

      if(destFrameNumber == 1)
      {
	//memcpy((char*)&packet.data[64+12+(currentWordsInPacket*4)],
	//(char*)&uval,4);

	qma_uint32 tmpWord = qma_htonl(uval);
	memcpy((char*)&packet.data[64+12+(currentWordsInPacket*4)], &tmpWord, 4);

	++currentWordsInPacket;
        ++currentWordsInFrame;
      }
      else
      {
	//memcpy((char*)&packet.data[128+4+((destFrameNumber-2)*64) + 
	//		  (currentWordsInFrame*4)],(char*)&uval,4);

	qma_uint32 tmpWord = qma_htonl(uval);
	memcpy((char*)&packet.data[128+4+((destFrameNumber-2)*64) + 
			  (currentWordsInFrame*4)], &tmpWord, 4);

	++currentWordsInPacket;
	++currentWordsInFrame;
      }

      //
      // Check to see if we are done
      //
      if(currentWordsInPacket == 1)
      {
        qma_int32 fdpoint = p_previous_last_sample_in_packet + diffs[0];
        if(!true)
        {
          std::cout << "+++ Channel "<< vo.getSEEDName() 
	  << " x0 = x0-1 + d0 " << x0 <<
		" should equal " << 
		(p_previous_last_sample_in_packet + diffs[0]) 
		<< " and " << fdpoint << std::endl;
        }
        if(!true)
        {
          qma_uint32 tval = uval | 0xc0000000;
          qma_int32 sval = (qma_int32) tval;
          qma_int32 fdpoint = p_previous_last_sample_in_packet + sval;
          std::cout << "+++ Channel "<< vo.getSEEDName() 
	  << " x0 = x0-1 + d0 " << x0 <<
		" should equal " << 
		(p_previous_last_sample_in_packet + sval) 
		<< " and " << fdpoint << std::endl;
        }

      }
      if(currentWordsInPacket == numberOfSecondsToPack)
      {
	break; // exits list iteration
      }

  } // End iter through packets.

  // Assign the return value
  //
  samplesInPacket = totalSamplesInPacket;
  framesInPacket = destFrameNumber;

  //
  // Complete filling out the packet with x0,xn values and maps;
  // These are order dependent copies. Adding x0 and x0 earlier
  // caused data overwrites.
  //

  x0 = qma_htonl(x0);
  xn = qma_htonl(xn);
  memcpy((char*)&packet.data[68],&x0,4);
  memcpy((char*)&packet.data[72],&xn,4);

  getAllIntMap(numberOfSecondsToPack,mapArray);
  
  qma_uint32 mapval;
  for(int x=0;x<7;x++)
  {
      mapval = qma_htonl (mapArray[x]);
      memcpy((char*)&packet.data[(x*64)+64], (char*)&mapval,4);
  } 

  //
  // Delete the packetized word from compression queue;
  //
  int unsent = p_seconds_in_list - currentWordsInPacket;
  for(int x=0;x<unsent;x++)
  {
      p_list[x] = p_list[currentWordsInPacket+x];
  }
  p_seconds_in_list = unsent;
  if(currentWordsInPacket != seconds_to_packetize)
  {
      std::cout << "xxx Words in packet do not equal seconds to packetize" <<
	std::endl;
  }
  return true;
}
