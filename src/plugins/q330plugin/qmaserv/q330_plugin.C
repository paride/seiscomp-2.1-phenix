/*
 * Program: q330_plugin, a SeedLink plugin for the Quantera 330
 *
 * File:
 *  q330_plugin
 *
 * Purpose:
 *  This is the top level program that collects data from a Q330
 *  and delivers it to a SeedLink server.
 *
 * Author:
 *  Phil Maechling
 *  Chad Trabant (q330_plugin version)
 *
 * Modifications:
 *  12 September 2003: Re-fitting of Mountainair for a SeedLink plugin
 *  17 June 2004: Upgrading to Mountainair 1.1.12
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
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <time.h>
#include <string.h>

#include "q330_plugin.h"
#include "qmautils.h"
#include "Verbose.h"
#include "StateMachine.h"
#include "Cleanup.h"
#include "QMA_Port.h"
#include "ModuloCounter.h"
#include "QDPHeader.h"
#include "Packet.h"
#include "ConfigVO.h"
#include "ReadConfig.h"
#include "TxCmd.h"
#include "CountDownTimer.h"
#include "TimeServer.h"
#include "AckCounter.h"
#include "TokenVO.h"
#include "NetStationVO.h"
#include "LogTimingVO.h"
#include "ClockProcVO.h"
#include "LCQ.h"
#include "PCQ.h"
#include "ChanFreqMap.h"
#include "TypeChanFreqMap.h"
#include "QMA_Version.h"
#include "Q330Version.h"
#include "global.h"

#include "qmacserv.h"
#include "ConfigInfoVO.h"
#include "LCQVO.h"
#include "CheckLCQS.h"
#include "CheckPCQS.h"
#include "msgs.h"
#include "InputPacket.h"
#include "cppstreams.h"

#ifdef LINUX
#include "linuxtools.h"
#endif

using namespace std;
using namespace CPPStreams;

//
// These are the instantiations of the variable declared in the global.h
//
Q330Version      g_Q330Version;
Verbose          g_verbosity;
StateMachine     g_stateMachine;
ConfigVO         g_cvo;
QMA_Port 	 g_cmdPort;
QMA_Port         g_dataPort;
ModuloCounter    g_cmdPacketSeq;
CountDownTimer   g_cmdTimer;
CountDownTimer   g_dataPortTimer;
CountDownTimer   g_statusTimer;
bool             g_reset;
bool             g_done;
Verbose*         g_verbList; // Not needed. Just for testing
Packet           g_packet_for_tx;
TimeServer 	 g_timeServer;
AckCounter       g_ackCounter;
TokenVO          g_tvo;
int              g_nextTokenAddress;
int              g_segmentsReceived;
int              g_bytesInBuffer;
char 		 g_tokenBuffer[C1_MAXCFG];
int              g_totalSegments;
qma_uint16       g_nextPacketSeqno;
int              g_totalFillPackets;
char             g_configFile[MAX_CHARS_IN_CONFIG_FILE+1];
//
// These 4 are related to the number of digital LCQ's in the system.
// The LCQVO_List is used to moderate the startup grap for free memory.
//
int              g_number_of_diglcqs;
LCQ*             g_digLCQ_list;
PCQ* 		 g_digPCQ_list;
LCQVO*           g_digLCQVO_list;
bool             g_lcq_freed;
//
// These 4 are related to the number of main LCQ's in the system.
// The LCQVO_List is used to moderate the startup grap for free memory.
//
int              g_number_of_mainlcqs;
LCQ*             g_mainLCQ_list;
PCQ* 		 g_mainPCQ_list;
LCQVO*           g_mainLCQVO_list;
bool             g_main_lcq_freed;

ChanFreqMap 	 g_digMap_list;
TypeChanFreqMap  g_mainMap_list;

ModuloCounter    g_dataPacketSeq;
TimeOfDay        g_curTime;
bool             g_startingDRSNNeeded;
c1_srvch         g_c1; 
MainStates       g_curState;
bool             g_outputPacketsQueued;
BTI		 g_currentTimeInfo;
InputPacket      g_inputQueue[MAX_SLIDING_WINDOW_SIZE];

std::string                     g_streamTimeFile;
std::map<StreamDescriptor, BTI> g_streamTimeMap;

// Local global variables
static char      l_pluginName[MAX_CHARS_IN_PLUGIN_NAME+1];

//*****************************************************************************
// SystemLog
//*****************************************************************************

class SystemLog
{
public:
  enum { msglen = 200 };
  
  void operator()(int priority, const string &msg)
  {
    time_t t = time(NULL);
    char *p = asctime(localtime(&t));
    string msgout = string(p, strlen(p) - 1) + " - " + l_pluginName + ": " + msg;
    write(STDOUT_FILENO, msgout.c_str(), msgout.length());
  }
};

namespace CPPStreams {

Stream logs = make_stream(SystemLog());

}

//
//*****************************************************************************
// This is the main routine which receives data from the q330.
//*****************************************************************************
//

int main(int argc, char *argv[])
{
  struct timespec req, rem;
  int verbosity = 0;
  int c;

  if(argc < 2)
  {
    Usage();
    return(0);
  }

  strncpy(g_configFile, CONFIG_FILE, MAX_CHARS_IN_CONFIG_FILE);
  g_configFile[MAX_CHARS_IN_CONFIG_FILE] = '\0';

  while((c = getopt(argc, argv, "h?vVf:x:")) != EOF)
  {
    switch(c)
      {
      case 'h':
      case '?': Usage(); exit(0); break;
      case 'v': verbosity++; break;
      case 'V': showVersion(); exit(0); break;
      case 'f':
        strncpy(g_configFile, optarg, MAX_CHARS_IN_CONFIG_FILE);
        g_configFile[MAX_CHARS_IN_CONFIG_FILE] = '\0';
        break;
      case 'x':
        g_streamTimeFile = string(optarg);
        break;
      }
  }

  if ( optind != (argc-1) ) {
    cout << "plugin name incorrectly specified" << endl << endl;
    Usage();
    exit(1);
  }

  strncpy(l_pluginName, argv[optind], MAX_CHARS_IN_PLUGIN_NAME);
  l_pluginName[MAX_CHARS_IN_PLUGIN_NAME] = '\0';

  redirect_ostream(cout, SystemLog(), LOG_INFO);
  redirect_ostream(cerr, SystemLog(), LOG_ERR);
  redirect_ostream(clog, SystemLog(), LOG_ERR);

  initializeSignalHandlers();

  if(g_streamTimeFile.length() != 0)
      loadTimetable();

  g_done  = false;
  g_reset = false;
  g_lcq_freed = true;
  g_verbosity.setVerbosity(verbosity);
  g_verbosity.setDiagnostic(0);

  showVersion();

  req.tv_sec  = (time_t) 0;
  req.tv_nsec = (long int) 50000000;

  bool recvd = false;

  while(!g_done)
  {
    readOrRereadConfigFile();
    ResetQMA(true);
    g_reset = false;
    openLocalIPPorts();
    while(!g_reset)
    {
      tx_cmd();
      recvd = rx_msg();
      assembleBlockettesIntoSecond();
      assembleSecondsIntoPacket();
      check_state();

      // Throttle the loop if no messages were received during
      // the previous loop, sleep for 0.05 seconds.
      if (!recvd)
        nanosleep (&req, &rem);
    }
  }
  
  std::cout << "+++ Shutting down q330_plugin" << std::endl;
  ResetQMA(true);
  return 0;
}

//
// Initialize the signal handlers and the variables
//

void initializeSignalHandlers()
{
  //
  // Ignore hangup.
  //
  signal(SIGHUP, SIG_IGN);

  //
  // Abort on SIGPIPE.
  //
  signal(SIGPIPE, AbortQMA);

  //
  // Normal shutdown request.
  //
  signal(SIGINT, ShutdownQMA);
  signal(SIGTERM, ShutdownQMA);
}


void readOrRereadConfigFile()
{
  bool res = readConfigFile(l_pluginName);

  if(res)
  {
    // Only set the verbosity if not given on the command line
    if ( ! g_verbosity.showVerbosity() )
      g_verbosity.setVerbosity(g_cvo.getVerbosity());

    g_verbosity.setDiagnostic(g_cvo.getDiagnostic());
    g_stateMachine.setState(RequestingServerChallenge);
  }
  else
  {
     std::cout << 
        "xxx Error reading q330_plugin configuration values for: " 
        << l_pluginName << std::endl;
     CleanQMA(12);
  }
}

void assembleBlockettesIntoSecond()
{
  
  if(g_stateMachine.getState() == AcquiringData)
  {
    check_lcqs();
  }
  return;
}
    
void assembleSecondsIntoPacket()
{
  if(g_stateMachine.getState() == AcquiringData)
  {
    check_pcqs();
  }
  return;
}
    
void check_state()
{

  MainStates mstate = g_stateMachine.getState();
  if(mstate == Resetting)
  {
    ResetQMA(true);
  }
  else if(mstate == Exitting)
  {
    CleanQMA(12);
  }
  return;
}
