/*
 * File     :
 *  qmautils.C
 *
 * Purpose  :
 *  Simple utility routines for use in qmaserv.
 *
 * Author   :
 *  Phil Maechling
 *
 * Mod Date :
 *  26 July 2002
 *  12 September 2003, Chad Trabant
 *  1 October 2004, Andres Heinloo
 *  24 May 2005, Andres Heinloo
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
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "qmautils.h"
#include "QmaTypes.h"
#include "QmaDiag.h"
#include "Verbose.h"
#include "global.h"
#include "QMA_Port.h"
#include "Cleanup.h"
#include "QMA_Version.h"

void Usage()
{
  showVersion();
  std::cout << "Usage: q330_plugin [options] plugin_name" << std::endl << std::endl;

  std::cout << "'plugin_name' is the section name in config file; it is also used" << std::endl;
  std::cout << "as a signature in log messages" << std::endl << std::endl;

  std::cout << "-h -?                Print this usage message" << std::endl;
  std::cout << "-v                   Increase verbosity, overrides value in config file" << std::endl;
  std::cout << "-V                   Print version and exit" << std::endl;
  std::cout << "-f config_file       Specify configuration file" << std::endl << std::endl;
}

//
// Routine to initialize the data and cmd ports
//

bool openLocalIPPorts()
{  

  if(g_stateMachine.getState() != OpeningLocalPorts)
  {
    CleanQMA(12);
  }

  bool initialized = false;
  bool result = false;
  if(g_verbosity.show(D_MINOR,D_OPEN_LOCAL_PORT))
  {
    std::cout << "--- QMA IPPort : " << 
      (qma_uint32) g_cvo.getQMAIPPort() << std::endl;
    std::cout << "--- Q330 Data Port : " <<
      (qma_uint32) g_cvo.getQ330DataPortAddress() << std::endl;
    struct sockaddr_in temp;
    temp.sin_family = AF_INET;
    temp.sin_addr.s_addr = g_cvo.getQ330UdpAddr();
    memset(&(temp.sin_zero),'\0',8); 
    std::cout << "--- UdpAddr : " <<
	    inet_ntoa(temp.sin_addr) << std::endl;
    std::cout << "--- Serial Number : " << std::hex <<  
	g_cvo.getQ330SerialNumber() << std::endl;
  }

  int retryLimit = 100; // Just a large number of retries before exiting
  while(!initialized)
  {
      --retryLimit;
       bool result = g_cmdPort.initialize(g_cvo.getQMAIPPort(),
		                        g_cvo.getQ330DataPortAddress(),
                                        g_cvo.getQ330UdpAddr());
       if(result)
       {
         initialized = true;
         if(g_verbosity.show(D_MAJOR,D_OPEN_LOCAL_PORT))
         {
           std::cout << "+++ DP CmdPort Initialized: " << 
		g_cvo.getQMAIPPort() << std::endl;
         }
       }
       else if(retryLimit < 1)
       {
          std::cout << "xxx Exiting after 100 retries initializing cmdPort" << 
		std::endl;
          initialized = false;
       }
       else
       {
         std::cout << "--- Retrying cmdPort Initializaion" << std::endl;
         sleep(10); // sleep in seconds
       }
    } // end while

    initialized = false;
    result = false;
    retryLimit = 100;
    while(!initialized)
    {
      --retryLimit;
      result = g_dataPort.initialize((g_cvo.getQMAIPPort()+1),
                                    (g_cvo.getQ330DataPortAddress()+1),
                                     g_cvo.getQ330UdpAddr(),
				     g_cvo.getStatusInterval(),
                                     g_cvo.getDataRateInterval());
      if(result)
      {
	    initialized = true;

        if(g_verbosity.show(D_MAJOR,D_OPEN_LOCAL_PORT))
        {
            std::cout << "+++ DP DataPort Initialized: " << 
		(g_cvo.getQMAIPPort() + 1) << std::endl;
        }
      }
      else if (retryLimit < 1)
      {
            std::cout << "xxx Exiting after 100 retries initializing dataPort" 
		<< std::endl;
	return false;
      }
      else
      {
            sleep(10);
            std::cout << "xxx Retrying dataPort Initializaion" << std::endl;
      }
  }
  g_stateMachine.setState(RequestingServerChallenge);
  return true;
}

//
// Validation current consist of only one checks.
// A new version will generate a warning only.
void validateTokens()
{
  char staCode[STATION_CODE_LEN +1];
  memset(staCode,0,STATION_CODE_LEN +1);
  memcpy((char*)&staCode[0],
         g_tvo.getNetStationVO().getStationCode(),
         strlen(g_tvo.getNetStationVO().getStationCode()));
  memset((char*)&staCode[STATION_CODE_LEN],0,1);
  //
  // There is a Token Version which is not useful
  // More importantly there is a system version. This version
  // Info is in the fixed flags, not the tokens. None the less.
  // check the System Version here before starting acquisition.
  //
  if(g_Q330Version.getVersionRelease() > QMA_VERSION_RELEASE)
  {
    std::cout << "+++ Important. This Q330 is using a new System Version." 
     << std::endl;
    std::cout << "+++ q330_plugin may not be tested with this System Version."
     << std::endl;
  }
          std::cout << std::endl;
          std::cout << "+++ Summary of Token Information : " << std::endl;
	  std::cout << "+++ Q330 System Version : " 
                << std::dec << g_Q330Version.getVersionRelease() 
		<< "." << std::dec << g_Q330Version.getVersionFeature() 
                   << std::endl;
          NetStationVO nsvo = g_tvo.getNetStationVO(); 
          std::cout << "+++ Token Version : " << g_tvo.getVersion() 
                << std::endl;
          std::cout << "+++ Network : " << nsvo.getNetworkCode() << std::endl;
          std::cout << "+++ Station : " << nsvo.getStationCode() << std::endl;
          std::cout << "+++ DPNetserver : " << g_tvo.getDPNetServerPortNumber() 
		<< std::endl;
          std::cout << "+++ DPWebserver : " << g_tvo.getDPWebServerPortNumber()
		<< std::endl;

          //
          // ClockProc info
          //
          qma_uint16 temp;
	  ClockProcVO cpvo = g_tvo.getClockProcVO();

	  temp = cpvo.getTimeZoneOffset(); 
	  std::cout << "+++ TimeZone offset in Minutes : " <<
		temp << std::endl;

	  temp = cpvo.getLossInMinutes();
	  std::cout << "+++ Loss in Minutes : " <<
		temp << std::endl;

	  temp = cpvo.getPLLLockQuality();
	  std::cout << "+++ PLL Lock Quality : " << std::hex <<
		temp << std::endl;

          temp = cpvo.getPLLTrackingQuality();
	  std::cout << "+++ PLL Track Quality : " << std::hex <<
		temp << std::endl;

	  temp = cpvo.getPLLHoldQuality();
	  std::cout << "+++ PLL Hold Quality : " << std::hex <<
		temp << std::endl;

          temp = cpvo.getPLLOffQuality();
	  std::cout << "+++ PLL Off Quality : " << std::hex <<
		temp << std::endl;

	  temp = cpvo.getHighestHasBeenLocked();
	  std::cout << "+++ Highest Has Been Locked Clock Quality : " <<
		temp << std::endl;

	  temp = cpvo.getLowestHasBeenLocked();
	  std::cout << "+++ Lowest Has Been Locked Clock Quality : " <<
		temp << std::endl;

	  temp = cpvo.getClockQualityFilter();
	  std::cout << "+++ Clock Quality Filter : " <<
		temp << std::endl;

	  //
          // Now Timing log info
          //
	  LogTimingVO ltvo = g_tvo.getLogTimingVO();
	  std::cout << "+++ MessageLogLocation : " << 
		ltvo.getMessageLogLocation() << std::endl;
	  std::cout << "+++ MessageLogSEEDName : " << 
		ltvo.getMessageLogName() << std::endl;
	  std::cout << "+++ TimingLogLocation : " << 
		ltvo.getTimingLogLocation() << std::endl;
	  std::cout << "+++ TimingLogSEEDName : " << 
		ltvo.getTimingLogName() << std::endl;

	  //
          //
          //
	  ConfigInfoVO civo = g_tvo.getConfigInfoVO();
	  std::cout << "+++ ConfigStreamLocation : " <<
	    civo.getStreamSEEDLocation() << std::endl;
	  std::cout << "+++ ConfigStreamName : " <<
	    civo.getStreamSEEDName() << std::endl;
	  std::cout << "+++ ConfigStreamRequestOption : " <<
	    civo.getConfigOption() << std::endl;
	  if(civo.getConfigOption() == Periodic)
	    {
	      std::cout << "+++ ConfigRequestInterval : " <<
		civo.getConfigInterval() << std::endl;
	    }
         
           std::cout << "+++ DataServerPort : " <<
		g_tvo.getDataServerPort() << std::endl;

 	   std::cout << "+++ Number of 24 bit digitizer LCQ's" 
		<< " for this Data port : " <<
		std::dec << g_number_of_diglcqs << std::endl;
           for(int i=0;i<g_number_of_diglcqs;i++)
           {
             LCQVO temp = g_digLCQ_list[i].getLCQVO();

             std::cout << "+++ LCQ Network Code : " << temp.getNetworkCode() 
	        <<	std::endl;
             std::cout << "+++ LCQ Station Code : " << temp.getStationCode()
		<<	std::endl;
             std::cout << "+++ LCQ Location Code : " << temp.getLocationCode() 
	        <<	std::endl;
             std::cout << "+++ LCQ SEED Name : " << temp.getSEEDName()
		<<	std::endl;

             std::cout << "+++ LCQ Ref Number : " << 
		(qma_uint16) temp.getLCQReferenceNumber() << std::endl;

             std::cout << "+++ LCQ Channel Number : " <<
		(qma_uint16) temp.getChannel() << std::endl;

             std::cout << "+++ LCQ Channel Byte   : " <<
		(qma_uint16) temp.getChannelByte() << std::endl;

             std::cout << "+++ LCQ Frequency (Hertz) : " <<
		 temp.getFrequencyHertz() << std::endl;

             std::cout << "+++ LCQ FreqBit Number : " <<
		(qma_uint16) temp.getFrequencyBit() << std::endl;

             std::cout << "+++ LCQ Filter Delay : " <<
		(qma_int32) temp.getFilterDelay() << std::endl;

             std::cout << "+++ LCQ Source Parameter : " << 
		(qma_uint16) temp.getSourceParameter() << std::endl;

             std::cout << "+++ LCQ Source : " << (qma_uint16) temp.getSource()
                << std::endl;

             std::cout << "+++ LCQ Parameter : " << 
		(qma_uint16) temp.getParameterNumber() << std::endl;

             std::cout << "+++ LCQ Option Bits : " << 
			temp.getOptionBitsFlag() << std::endl;
             std::cout << "+++ LCQ Samples per Blockette : " <<
                        temp.getSamplesPerBlockette() << std::endl;
             std::cout << "+++ LCQ Rate : " << temp.getRate() <<
			std::endl << std::endl;
	   }


 	   std::cout << "+++ Number of main and analog LCQ's" 
		<< " for this Data port : " <<
		std::dec << g_number_of_mainlcqs << std::endl;

           for(int i=0;i<g_number_of_mainlcqs;i++)
           {

             LCQVO temp = g_mainLCQ_list[i].getLCQVO();

             std::cout << "+++ LCQ Network Code : " << temp.getNetworkCode() 
	        <<	std::endl;
             std::cout << "+++ LCQ Station Code : " << temp.getStationCode()
		<<	std::endl;
             std::cout << "+++ LCQ Location Code : " << temp.getLocationCode() 
	        <<	std::endl;
             std::cout << "+++ LCQ SEED Name : " << temp.getSEEDName()
		<<	std::endl;

             std::cout << "+++ LCQ Ref Number : " << 
		(qma_uint16) temp.getLCQReferenceNumber() << std::endl;

             std::cout << "+++ LCQ Channel Number : " <<
		(qma_uint16) temp.getChannel() << std::endl;

             std::cout << "+++ LCQ Channel Byte   : " <<
		(qma_uint16) temp.getChannelByte() << std::endl;

             std::cout << "+++ LCQ Frequency (Hertz) : " <<
		 temp.getFrequencyHertz() << std::endl;

             std::cout << "+++ LCQ FreqBit Number : " <<
		(qma_uint16) temp.getFrequencyBit() << std::endl;

             std::cout << "+++ LCQ Filter Delay : " <<
		(qma_int32) temp.getFilterDelay() << std::endl;

             std::cout << "+++ LCQ Source Parameter : " << 
		(qma_uint16) temp.getSourceParameter() << std::endl;

             std::cout << "+++ LCQ Source : " << (qma_uint16) temp.getSource()
                << std::endl;

             std::cout << "+++ LCQ Parameter : " << 
		(qma_uint16) temp.getParameterNumber() << std::endl;

             std::cout << "+++ LCQ Option Bits : " << 
			temp.getOptionBitsFlag() << std::endl;

             std::cout << "+++ LCQ Samples per Blockette : " <<
                        temp.getSamplesPerBlockette() << std::endl;

             std::cout << "+++ LCQ Rate : " << temp.getRate() <<
			std::endl << std::endl;
	   }
}

void showVersion()
{
  std::cout << "q330_plugin version "
            << QMA_VERSION_RELEASE << "."
            << QMA_VERSION_FEATURE << "."
            << QMA_VERSION_FIX 
            << " (" PLUGIN_RELEASE_DATE ")"
            << std::endl;
}

int packet_type2int(const char *type)
  {
    if(type == NULL) return SLNUM;
    else if(strlen(type) != 1) return -1;
    
    switch(toupper(*type))
      {
        case 'D': return SLDATA;
        case 'E': return SLDET;
        case 'T': return SLTIM;
        case 'C': return SLCAL;
        case 'L': return SLMSG;
        case 'O': return SLBLK;
      }

    return -1;
  }

void loadTimetable()
  {
    std::cout << "+++ Loading initial DRSNs from " << g_streamTimeFile << std::endl;

    FILE *fp;
    if((fp = fopen(g_streamTimeFile.c_str(), "r")) == NULL)
      {
        std::cout << "--- Cannot open " << g_streamTimeFile << std::endl;
        return;
      }

    char stream[9], *loc, *chn, *stype;
    int type, drsn;
    
    int r;
    while((r = fscanf(fp, "%8s %d\n", stream, &drsn)) == 2)
      {
        loc = strdup(stream);

        if((chn = strchr(loc, '.')) == NULL)
          {
            std::cout << "--- Invalid stream: " << stream << std::endl;
            free(loc);
            continue;
          }

        *(chn++) = 0;

        if((stype = strchr(chn, '.')) == NULL)
          {
            std::cout << "--- Invalid stream: " << stream << std::endl;
            free(loc);
            continue;
          }

        *(stype++) = 0;

        if((type = packet_type2int(stype)) == -1)
          {
            std::cout << "--- Invalid stream: " << stream << std::endl;
            free(loc);
            continue;
          }
            
        StreamDescriptor sd(loc, chn, type);
        
        BTI bti;
        memset(&bti, 0, sizeof(BTI));
        bti.drsn = drsn;
        g_streamTimeMap[sd] = bti;

        free(loc);
      }

    fclose(fp);
  }

void saveTimetable()
  {
    std::cout << "+++ Saving current DRSNs to " << g_streamTimeFile << std::endl;

    FILE *fp;
    if((fp = fopen(g_streamTimeFile.c_str(), "w")) == NULL)
      {
        std::cout << "--- Cannot open " << g_streamTimeFile << std::endl;
        return;
      }

    std::map<StreamDescriptor, BTI>::const_iterator p;
    for(p = g_streamTimeMap.begin(); p != g_streamTimeMap.end(); ++p)
        fprintf(fp, "%s %d\n", p->first.to_string().c_str(), p->second.drsn);

    fclose(fp);
  }
 
