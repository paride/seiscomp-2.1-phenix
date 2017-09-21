/*
 * Program: Mountainair
 *
 * File:
 *  qlobal.h
 *
 * Purpose:
 *  These are "global" objects used by the qmaserv routine. This file
 *   is include by any routine that needs access to the central hub,
 *   the globally updated objects.
 *
 * Author:
 *   Phil Maechling
 *
 * Created:
 *   4 April 2002
 *
 * Modifications:
 *   1 October 2004, Andres Heinloo
 *
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

#include <map>
#include <string>

#include "QDPHeader.h"
#include "Packet.h"
#include "ConfigVO.h"
#include "ModuloCounter.h"
#include "QMA_Port.h"
#include "StateMachine.h"
#include "AckCounter.h"
#include "qmacserv.h"
#include "TokenVO.h"
#include "TimeServer.h"
#include "ChanFreqMap.h"
#include "TypeChanFreqMap.h"
#include "LCQVO.h"
#include "LCQ.h"
#include "PCQ.h"
#include "Verbose.h"
#include "CountDownTimer.h"
#include "msgs.h"
#include "InputPacket.h"
#include "Q330Version.h"
#include "BTI.h"

/* SeedLink packet types */
#define SLDATA 0     /* waveform data record */
#define SLDET  1     /* detection record */
#define SLCAL  2     /* calibration record */
#define SLTIM  3     /* timing record */
#define SLMSG  4     /* message record */
#define SLBLK  5     /* general record */
#define SLNUM  6

#include "descriptor.h"

using Utilities::StreamDescriptor;

//
// Set a Configuration Values, input/output Ports, SeqnoCounters,
// and state machine, at a global level 
// so the signal handler can set an exit state
//
extern Verbose          g_verbosity; 
extern StateMachine     g_stateMachine;
extern ConfigVO         g_cvo;
extern QMA_Port         g_cmdPort;
extern QMA_Port         g_dataPort;
extern ModuloCounter    g_cmdPacketSeq;
extern CountDownTimer   g_cmdTimer;
extern CountDownTimer   g_dataPortTimer;
extern CountDownTimer   g_statusTimer;
extern bool             g_reset;
extern bool             g_done;
extern Packet           g_packet_for_tx;
extern TimeServer       g_timeServer;
extern AckCounter       g_ackCounter;
extern TokenVO          g_tvo;
extern int              g_nextTokenAddress;
extern int              g_segmentsReceived;
extern int              g_bytesInBuffer;
extern char             g_tokenBuffer[C1_MAXCFG];
extern int              g_totalSegments;
extern int              g_totalFillPackets;
extern char             g_configFile[MAX_CHARS_IN_CONFIG_FILE+1];

extern int		g_number_of_diglcqs; 
extern LCQ*    		g_digLCQ_list;
extern PCQ*   		g_digPCQ_list;
extern LCQVO*           g_digLCQVO_list;


extern int		g_number_of_mainlcqs; 
extern LCQ*    		g_mainLCQ_list;
extern PCQ*   		g_mainPCQ_list;
extern LCQVO*           g_mainLCQVO_list;

extern ChanFreqMap 	g_digMap_list;
extern TypeChanFreqMap  g_mainMap_list;

extern ModuloCounter    g_dataPacketSeq;
extern TimeOfDay        g_curTime;
extern double           g_timeOfLastScan;
extern bool             g_startingDRSNNeeded;
extern bool 		g_outputPacketsQueued;
extern BTI		g_currentTimeInfo;
extern InputPacket      g_inputQueue[MAX_SLIDING_WINDOW_SIZE];

extern qma_uint16       g_nextPacketSeqno;
extern Q330Version      g_Q330Version;
extern bool             g_lcq_freed;

extern std::string                     g_streamTimeFile;
extern std::map<StreamDescriptor, BTI> g_streamTimeMap;

