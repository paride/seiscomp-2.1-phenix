/*
 * Program: 
 *   ReadConfig.C
 *
 * Purpose:
 *   These is processing routine is used by the qmaserv main routine
 *   to read in configuration information. It uses the comserv configuration
 *   routines to read the config file 
 *
 * Author:
 *   Phil Maechling
 *
 * Created:
 *   4 April 2002
 *
 * Modifications:
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
 */
#include <iostream>
#include <string.h>
#include "ReadConfig.h"
#include "Verbose.h"
#include "QmaTypes.h"
#include "QmaDiag.h"
#include "StateMachine.h"
#include "ConfigVO.h"
#include "qmacserv.h"

extern Verbose      g_verbosity;
extern StateMachine g_stateMachine;
extern ConfigVO     g_cvo;
extern char         g_configFile[];


bool readConfigFile(char* plugin_name)
{
  if(g_verbosity.show(D_MAJOR,D_QMA_CONFIG))
  {
          std::cout << "+++ Entering State : " << g_stateMachine.getState() 
		<< std::endl;
  }

  if(g_verbosity.show(D_MAJOR,D_SHOW_STATE))
  {
	  std::cout << "+++ Configuring using plugin name : " << plugin_name
		<< std::endl;
  }
  struct qma_cfg qmacfg;

  // This makes a call to comserv routines
  // to retrieve [<plugin_name>] data in the
  // configuration file (i.e. plugins.ini)

  int res = getQmacfg(&qmacfg, g_configFile, plugin_name);

  if(res != QMA_SUCCESS)
  {
    std::cout << "xxx Error initializing from config file using section : " << 
	plugin_name << std::endl;
        return false;
  }


  //
  // Call to check that all required config values are set
  //
  res = validateQmaConfig(qmacfg);

  if(res == false)
  {
    return false;
  } 

  //
  // This will convert string values to int, IP, and other usable
  // values, and put them into a ConfigVO.
  //
  ConfigVO tcvo(qmacfg.udpaddr,
		qmacfg.baseport,
		qmacfg.dataport,
		qmacfg.serialnumber,
		qmacfg.authcode,
		qmacfg.ipport,
                qmacfg.verbosity,
	        qmacfg.diagnostic,
                qmacfg.startmsg,
		qmacfg.statusinterval,
		qmacfg.datarateinterval);

  if(g_verbosity.show(D_MAJOR,D_QMA_CONFIG))
  {
    std::cout << "--- Initialized configuration for : " << plugin_name
		<< std::endl;
  }

  g_cvo = tcvo; // Move local ConfigVO to global position

  //
  // When this is run, verbosity object not yet set. So specify
  // here the qma target
  //
  if((g_cvo.getVerbosity() >= D_MAJOR ) || 
      (g_cvo.getDiagnostic() == D_QMA_CONFIG))
  { 
            std::cout << "--- Read config for " <<
	        plugin_name << std::endl;
            std::cout << "--- Q330 UDPAddr : " <<
	        qmacfg.udpaddr << std::endl;
	    std::cout << "--- Q330 control Port : " <<
		qmacfg.baseport << std::endl;
	    std::cout << "--- Q330 Data Port : " <<
		qmacfg.dataport << std::endl;
	    std::cout << "--- Q330 Serial number : " <<
	        qmacfg.serialnumber << std::endl;
	    std::cout << "--- Authcode : " << 
	        qmacfg.authcode << std::endl;
	    std::cout << "--- DP IPPort : " <<
                qmacfg.ipport << std::endl;
	    std::cout << "--- Verbosity : " <<
                qmacfg.verbosity << std::endl;
	    std::cout << "--- Diagnostic : " <<
                qmacfg.diagnostic << std::endl;
	    std::cout << "--- Start Message : " <<
                qmacfg.startmsg << std::endl;
	    std::cout << "--- Status Interval : " <<
                qmacfg.statusinterval << std::endl;
	    std::cout << "--- Data Rate Interval : " <<
                qmacfg.datarateinterval << std::endl;
  }
  return true;
}

bool validateQmaConfig(const struct qma_cfg& aCfg)
{

 //
 // Required fields are 
 // udpaddr
 // ipport
 // baseport
 // dataport
 // serial number
 // authcode
 
 // Optional fields are
 // verbosity
 // diagnostic
 // startmsg
 // status interval
 // min clock quality

 int len = strlen(aCfg.udpaddr);
 if(len < 1)
 {
   std::cout << 
     "xxx Configuration file is missing value for udpaddr:" << std::endl;
  return false;
 }

 len = strlen (aCfg.baseport);
 if(len < 1)
 {
   std::cout << 
     "xxx - Configuration file is missing value for baseport:" << std::endl;
   return false;
 }

 len = strlen (aCfg.dataport);
 if(len < 1)
 {
   std::cout << 
     "xxx - Configuration file is missing value for dataport:" << std::endl;
   return false;
 }

 len = strlen (aCfg.serialnumber);
 if(len < 1)
 {
   std::cout << 
     "xxx - Configuration file is missing value for serialnumber:" 
	     << std::endl;
   return false;
 }

 len = strlen (aCfg.authcode);
 if(len < 1)
 {
   std::cout << 
     "xxx - Configuration file is missing value for authcode:" << std::endl;
   return false;
 }

 return true;
}
