/*
 * File     :
 *  Cleanup.C
 *
 * Purpose  :
 *  This is the cleanup routine which is called to exit the program.
 *  The signal number parameter is used when registering as a signal
 *  handler callback routine.
 *
 * Author   :
 *  Phil Maechling
 *
 * Mod Date :
 *  8 July 2002
 *  15 September 2003, Chad Trabant
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
 *
 */
#include <cstdlib>
#include <iostream>
#include <errno.h>
#include <unistd.h>
#include <time.h>
#include "Cleanup.h"
#include "QmaTypes.h"
#include "QmaLimits.h"
#include "StateMachine.h"
#include "global.h"
#include "CheckPCQS.h"
#include "QMA_Version.h"
#include "qmautils.h"
#include "q330_plugin.h"

void ResetQMA(bool flush_queues)
{
  std::cout << "+++ Setting q330_plugin to starting state" << std::endl;
  
  MainStates curstate = g_stateMachine.getState();

  g_cmdTimer.stop();
  g_dataPortTimer.stop();
  g_statusTimer.stop();

  if(curstate > RequestingServerChallenge)
  {
    if(flush_queues)
    {
      std::cout << "+++ Writing Data in compression queues to disk with DRSN : " 
         << g_currentTimeInfo.drsn << std::endl;

      empty_pcqs();
    }

    //
    // Send disconnect message
    //

    c1_dsrv   dsrv;
    QDPHeader out_qdp;
    Packet    out_p;

    dsrv.setSerialNumber(g_cvo.getQ330SerialNumber());

    out_qdp.setCommand(C1_DSRV_VAL);
    out_qdp.setVersion(QDP_VERSION);
    out_qdp.setDataLengthInBytes(dsrv.getLengthInBytes());
    out_qdp.setPacketSequence(g_cmdPacketSeq.next());
    out_qdp.setAckNumber(0x00);

    out_p.setDataBitString(dsrv.getBitString(),dsrv.getLengthInBytes());
    out_p.setQDPHeaderBitString(out_qdp.getBitString());
    out_p.setCRC();

    bool send_ok = g_cmdPort.write((char*)
                                    out_p.getBitString(),
                                    out_p.getLengthInBytes());
    if(!send_ok)
    {
          std::cout << "xxx Error sending exiting disconnect with errno :"
                << errno << std::endl;
    }
    std::cout << "+++ Sent disconnect message while resetting." << std::endl;
  }
  g_cmdPort.closePort();
  g_dataPort.closePort();

  //
  // Reset the global variables to a starting value
  //
  g_segmentsReceived = 0;
  g_nextTokenAddress = 0;
  g_bytesInBuffer = 0;
  g_totalSegments = 0;
  g_totalFillPackets = 0;
  g_number_of_diglcqs = 0;
  g_number_of_mainlcqs = 0;
  g_startingDRSNNeeded = true;
  g_nextPacketSeqno = 0;
  g_reset = true;
  g_outputPacketsQueued = false;

  //
  // Free the dynamic array lists
  //
  if(!g_lcq_freed)
  {
    delete [] g_digLCQ_list;
    delete [] g_digPCQ_list;

    delete [] g_mainLCQ_list;
    delete [] g_mainPCQ_list;

    // These are freed on creation of LCQs. Don't
    // free them again here.
    // delete [] g_digLCQVO_list;
    // delete [] g_mainLCQVO_list; 

    g_lcq_freed = true;
  }
  g_digMap_list.reset();
  g_mainMap_list.reset();

  //
  // This statement will drop messages in the input queue on reset.
  // Believed to introduce datagaps becuase ack'd packets are dropped.
  //
  // clearInputQueue();

  g_stateMachine.setState(OpeningLocalPorts);
  
  if(g_streamTimeFile.length() != 0)
      saveTimetable();

  return;
}

void AbortQMA(int sig)
{
  std::cout << "+++ Aborting q330_plugin" << std::endl;
  ResetQMA(false);
  exit(sig);
}

//
// Use cleanup to exit on fatal errors
//
void CleanQMA(int sig)
{
  std::cout << "+++ Exiting q330_plugin" << std::endl;
  ResetQMA(true);
  exit(sig);
}

//
// Use shutdown when you've received a signal from netmon
//
void ShutdownQMA(int sig)
{
  g_reset = true;
  g_done = true;
}

void clearInputQueue()
{
  for(int i=0;i<MAX_SLIDING_WINDOW_SIZE;i++)
  {
    g_inputQueue[i].full = false;
  }
}
