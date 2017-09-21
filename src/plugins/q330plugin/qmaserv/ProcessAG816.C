/*
 * File     :
 *   processAG816.C
 *
 * Purpose  :
 *   Process an DC_AG816 packet
 *
 * Author   :
 *   Phil Maechling
 *
 * Mod Date :
 *  3 August 2003
 *  1 October 2004, Andres Heinloo
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

#include "ProcessAG816.h"
#include "QmaTypes.h"
#include "QmaLimits.h"
#include "QmaDiag.h"
#include "global.h"
#include "DC.h"
#include "msgs.h"
#include "Blockette.h"
#include "Verbose.h"
#include "qmaswap.h"

extern Verbose g_verbosity;

void processAG816(const int       curposition,
                 const qma_uint8 chan,
	         dt_data         &dt,
	         const BTI&      curtime)
{

  if(!g_startingDRSNNeeded)
  {
    qma_uint8 mainType = (chan & 0x7f); // mask out the max bit
    mainType = mainType >> 3;  // Rotate right to get number 0-15
    qma_uint8 chanindex = (chan & 0x07);

    if(chanindex == 0)
    {
      std::cout << "--- Found Calibration Status Blockette" << std::endl;

      qma_int16 ldbyte = 0;
      //
      // copy data into first word of data structure. Skip 8 byte value
      //
      memcpy((char*)&ldbyte,(char*)dt.getOffsetBitString(curposition+1),2);
      reportCalibration(ldbyte);
    }
    else if(chanindex == 1)
    {
      // std::cout << "--- Found Analog Power Positive Blockette" << std::endl;
      //
      // Sample 0 (8 bit byte) is used for status so don't try to map it to
      // LCQ.
      //
      // Process First byte (16 bit byte) in packet
      //
      qma_uint8 queuepos = g_mainMap_list.retrieveQueuePosition(mainType,
                                                    chanindex,
                                                    1);

      if(queuepos >= g_number_of_mainlcqs)
      {
        //std::cout << " No analog powre LCQ found" << std::endl;
        return;
      }
      else
      {
        Blockette curBlock;
        QMABLOCK  dcblock;

        //
        // Fill in fixed values in dcblock for dc_comp blockette
        //
        dcblock.blocketteType = (qma_uint32) DC_D32;
        dcblock.numberOfSamples = 
          g_mainLCQ_list[queuepos].getLCQVO().getSamplesPerBlockette();
        //
        // Some values in a QMABLOCK are not valid for a DC_D32 blockette
        // Fill them in with placeholder, default values
        //
        dcblock.previousSample = 0;
        dcblock.segmentNumber = 0;
        dcblock.finalSegment = true;
        dcblock.mapLengthInBytes = 0;
        dcblock.dataLengthInBytes = 4;
        qma_int16 ldbyte = 0;
        qma_int32 intval = 0;
        //
        // copy data into first word of data structure. Skip 8 byte value
        //
        memcpy((char*)&ldbyte,(char*)dt.getOffsetBitString(curposition+1),2);
        intval = (qma_int32) qma_ntohs(ldbyte);
        memcpy((char*)&dcblock.w[0],(char*)&intval,4);
        //
        // Finally, data and time complete, insert them into blockette
        //    
        curBlock.setBlockette(dcblock);
        curBlock.setBlocketteTime(curtime);

        g_mainLCQ_list[queuepos].insertBlockette(curBlock);
      }
    }
    else if(chanindex == 2)
    {
       //std::cout << "--- Found Analog Power Negative Blockette" << std::endl;
    //
    // Process Second byte (16 bit byte) in packet
    //
    qma_uint8 queuepos = g_mainMap_list.retrieveQueuePosition(mainType,
                                                    chanindex,
                                                    2);

    if(queuepos >= g_number_of_mainlcqs)
    {
       return;
    }
    else
    {
      Blockette curBlock;
      QMABLOCK  dcblock;

      //
      // Fill in fixed values in dcblock for dc_comp blockette
      //
      dcblock.blocketteType = (qma_uint32) DC_D32;
      dcblock.numberOfSamples = 
        g_mainLCQ_list[queuepos].getLCQVO().getSamplesPerBlockette();
      //
      // Some values in a QMABLOCK are not valid for a DC_D32 blockette
      // Fill them in with placeholder, default values
      //
      dcblock.previousSample = 0;
      dcblock.segmentNumber = 0;
      dcblock.finalSegment = true;
      dcblock.mapLengthInBytes = 0;
      dcblock.dataLengthInBytes = 4;
      qma_int16 ldbyte = 0;
      qma_int32 intval = 0;
      //
      // copy data into first word of data structure. Skip 8 byte value
      //
      memcpy((char*)&ldbyte,(char*)dt.getOffsetBitString(curposition+1),2);
      intval = (qma_int32) qma_ntohs(ldbyte);
      memcpy((char*)&dcblock.w[0],(char*)&intval,4);
      //
      // Finally, data and time complete, insert them into blockette
      //    
      curBlock.setBlockette(dcblock);
      curBlock.setBlocketteTime(curtime);

      g_mainLCQ_list[queuepos].insertBlockette(curBlock);
      }
    }
    else
    {
       std::cout << "xxx Error. Invalid chanNum in Analog 816 LCQ : " <<
                      chanindex << std::endl;
    }
  }
}

void reportCalibration(qma_uint16 val)
{
 return;
}
