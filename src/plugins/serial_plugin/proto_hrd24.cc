/***************************************************************************** 
 * proto_hdr24.cc
 *
 * HRD24 protocol implementation
 *
 * (c) 2004 Recai YALGIN, Kandilli Observatory
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

#include <iomanip>
#include <vector>
#include <algorithm>
#include <cstdlib>
#include <cstddef>
#include <fstream>
#include <iostream>
//#include <string>

#include <unistd.h>
#include <time.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "qtime.h"

#include "utils.h"
#include "cppstreams.h"
#include "serial_plugin.h"
#include "plugin_channel.h"
#include "diag.h"

#include "little-endian.h"

#define PACKED __attribute__ ((packed))

using namespace std;
using namespace Utilities;
using namespace CPPStreams;
using namespace SeedlinkPlugin;

namespace {

const int DATA_CHANNELS     = 16;                               // 0...15
const int SOH_CHANNELS      = 16;                               // 16...31
const int NCHAN             = DATA_CHANNELS + SOH_CHANNELS;     // 32
const int SOH_SAMPLE_TIME   = 10;
const int RECVBUFSIZE       = 5120;
const int SENDBUFSIZE       = 256;
const int READ_TIMEOUT      = 10;

const int COMPRESSED_DATA_PACKET_FIRST_TIME		= 0x01;
const int COMPRESSED_DATA_PACKET_RETRANSMITTED	= 0x21;
const int STATUS_OF_HEALTH_PACKET				= 0x02;

struct DataBundle
  {
    u_int8_t desc;
    union { int8_t d8[4]; le_int16_t d16[2]; le_int32_t d32; } w[4];
  } PACKED;

//*****************************************************************************
// Data Structures
//*****************************************************************************


//*****************************************************************************
// HRD24Protocol
//*****************************************************************************
class HRD24Protocol: public Proto
  {
  private:
    int fd;
	string stationName;
    unsigned char recvbuf[RECVBUFSIZE], sendbuf[SENDBUFSIZE];
    map<string, rc_ptr<OutputChannel> > hrd24_channels;

    long sampleData[8000];
	static short sampleRateTable[32];	

    void do_start();
	void processData(const unsigned char *buf, int len);	
	unsigned short checkCRC(const unsigned char *buf, int len)
	{
		static unsigned short crctable[256] = {   /* CCITT crc */
			0x0000, 0x1189, 0x2312, 0x329b, 0x4624, 0x57ad, 0x6536, 0x74bf,
			0x8c48, 0x9dc1, 0xaf5a, 0xbed3, 0xca6c, 0xdbe5, 0xe97e, 0xf8f7,
			0x1081, 0x0108, 0x3393, 0x221a, 0x56a5, 0x472c, 0x75b7, 0x643e,
			0x9cc9, 0x8d40, 0xbfdb, 0xae52, 0xdaed, 0xcb64, 0xf9ff, 0xe876,
			0x2102, 0x308b, 0x0210, 0x1399, 0x6726, 0x76af, 0x4434, 0x55bd,
			0xad4a, 0xbcc3, 0x8e58, 0x9fd1, 0xeb6e, 0xfae7, 0xc87c, 0xd9f5,
			0x3183, 0x200a, 0x1291, 0x0318, 0x77a7, 0x662e, 0x54b5, 0x453c,
			0xbdcb, 0xac42, 0x9ed9, 0x8f50, 0xfbef, 0xea66, 0xd8fd, 0xc974,
			0x4204, 0x538d, 0x6116, 0x709f, 0x0420, 0x15a9, 0x2732, 0x36bb,
			0xce4c, 0xdfc5, 0xed5e, 0xfcd7, 0x8868, 0x99e1, 0xab7a, 0xbaf3,
			0x5285, 0x430c, 0x7197, 0x601e, 0x14a1, 0x0528, 0x37b3, 0x263a,
			0xdecd, 0xcf44, 0xfddf, 0xec56, 0x98e9, 0x8960, 0xbbfb, 0xaa72,
			0x6306, 0x728f, 0x4014, 0x519d, 0x2522, 0x34ab, 0x0630, 0x17b9,
			0xef4e, 0xfec7, 0xcc5c, 0xddd5, 0xa96a, 0xb8e3, 0x8a78, 0x9bf1,
			0x7387, 0x620e, 0x5095, 0x411c, 0x35a3, 0x242a, 0x16b1, 0x0738,
			0xffcf, 0xee46, 0xdcdd, 0xcd54, 0xb9eb, 0xa862, 0x9af9, 0x8b70,
			0x8408, 0x9581, 0xa71a, 0xb693, 0xc22c, 0xd3a5, 0xe13e, 0xf0b7,
			0x0840, 0x19c9, 0x2b52, 0x3adb, 0x4e64, 0x5fed, 0x6d76, 0x7cff,
			0x9489, 0x8500, 0xb79b, 0xa612, 0xd2ad, 0xc324, 0xf1bf, 0xe036,
			0x18c1, 0x0948, 0x3bd3, 0x2a5a, 0x5ee5, 0x4f6c, 0x7df7, 0x6c7e,
			0xa50a, 0xb483, 0x8618, 0x9791, 0xe32e, 0xf2a7, 0xc03c, 0xd1b5,
			0x2942, 0x38cb, 0x0a50, 0x1bd9, 0x6f66, 0x7eef, 0x4c74, 0x5dfd,
			0xb58b, 0xa402, 0x9699, 0x8710, 0xf3af, 0xe226, 0xd0bd, 0xc134,
			0x39c3, 0x284a, 0x1ad1, 0x0b58, 0x7fe7, 0x6e6e, 0x5cf5, 0x4d7c,
			0xc60c, 0xd785, 0xe51e, 0xf497, 0x8028, 0x91a1, 0xa33a, 0xb2b3,
			0x4a44, 0x5bcd, 0x6956, 0x78df, 0x0c60, 0x1de9, 0x2f72, 0x3efb,
			0xd68d, 0xc704, 0xf59f, 0xe416, 0x90a9, 0x8120, 0xb3bb, 0xa232,
			0x5ac5, 0x4b4c, 0x79d7, 0x685e, 0x1ce1, 0x0d68, 0x3ff3, 0x2e7a,
			0xe70e, 0xf687, 0xc41c, 0xd595, 0xa12a, 0xb0a3, 0x8238, 0x93b1,
			0x6b46, 0x7acf, 0x4854, 0x59dd, 0x2d62, 0x3ceb, 0x0e70, 0x1ff9,
			0xf78f, 0xe606, 0xd49d, 0xc514, 0xb1ab, 0xa022, 0x92b9, 0x8330,
			0x7bc7, 0x6a4e, 0x58d5, 0x495c, 0x3de3, 0x2c6a, 0x1ef1, 0x0f78
			};

		unsigned short c, crc;
		int i;

		crc = 0;
		for (i = 0; i < len; i++){
			c = buf[i];
			crc = crctable [ ( c ^ crc ) & 0xff ] ^ ( crc >> 8 );
		}
		return ( crc);
	}

 
  public:
    const string myname;
    
    HRD24Protocol(const string &myname_init)
      {
      }

    void attach_output_channel(const string &source_id,
    const string &channel_name, const string &station_name);
    void flush_channels();
    void start();
	void processVCXOCalibration(unsigned char *bundle, const string &station, const unsigned short instrumentId);
	void processSlowInternalSOH(unsigned char *bundle, const string &station, const unsigned short instrumentId);
	void processGPSLocation(unsigned char *bundle, const string &station, const unsigned short instrumentId);
	void processGPSSatelliteStatus(unsigned char *bundle, const string &station, const unsigned short instrumentId);
  };

short HRD24Protocol::sampleRateTable[] = {
	0, 1, 2, 5, 10, 20, 40, 50, 80, 100, 125, 200, 250, 500, 1000, 25, 
	120, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};
void HRD24Protocol::attach_output_channel(const string &source_id,
  const string &channel_name, const string &station_name)
  {

    map<string, rc_ptr<OutputChannel> >::iterator p;

    if((p = hrd24_channels.find(source_id)) != hrd24_channels.end())
        throw PluginADInUse(source_id, p->second->channel_name);
    
    hrd24_channels[source_id] = new OutputChannel(channel_name, station_name,
      dconf.zero_sample_limit);
	
	stationName = station_name;

  }

void HRD24Protocol::flush_channels()
  {

    map<string, rc_ptr<OutputChannel> >::iterator p;
    for(p = hrd24_channels.begin(); p != hrd24_channels.end(); ++p)
        if(p->second != NULL) p->second->flush_streams();

  }

void HRD24Protocol::start()
  {
    fd = open_port(O_RDWR);
	//Recai:Test
	//cout << "Port fd:" << fd << endl;
    try
      {
        do_start();
      }
    catch(PluginError &e)
      {
        seed_log << "closing device" << endl;
        close(fd);
        throw;
      }

    seed_log << "closing device" << endl;
    close(fd);
  }


/*
 *  processVCXOCalibration  still has problems. In documentation and according to example SOH file "VcxoValue,TimeDiffAtLock,TimeError,FreqError"
 *  values should be floating point numbers. However in the packet these are just 2 byte values. The program saves these as integer at the momenet
 */
void HRD24Protocol::processVCXOCalibration(unsigned char *bundle, const string &station, const unsigned short instrumentId){
	FILE		*soh_out = NULL;

	unsigned long	seconds;	
	float			vcxoValue;
	float			timeDiffAtLock;
	int				timeError = 0;
	int				freqError = 0;
	int				crystalTemp = 411;
	int				pllStatus = 21;
	int				gpsStatus = 0;
	char			datetime[32];
	char			tFile1[32];
	char			tFile2[32];


	struct tm tnow;
	

	memcpy(&seconds, &bundle[1], 4);
	memcpy(&vcxoValue, &bundle[5], 2);
	memcpy(&timeDiffAtLock, &bundle[7], 2);
	memcpy(&timeError, &bundle[9], 2);
	memcpy(&freqError, &bundle[11], 2);
	memcpy(&crystalTemp, &bundle[13], 2);
	memcpy(&pllStatus, &bundle[15], 1);
	memcpy(&gpsStatus, &bundle[16], 1);

	tnow = *gmtime((time_t *)&seconds);
	strftime(datetime,32,"%Y-%m-%d %H:%M:%S",&tnow);

	sprintf(tFile1, "%s.HRD24-%d.", station.c_str(), instrumentId);
	strftime(tFile2, 32, "%b.%d.%Y.vcx", &tnow);

	string filename = "/home/sysop/hrd24-SOH-files/";	
	filename.append(tFile1);
	filename.append(tFile2);
	
	if(soh_out == NULL){
		soh_out = fopen(filename.c_str(), "r+");
		if(soh_out == NULL){
			soh_out = fopen(filename.c_str(), "w+");
			fprintf(soh_out, "Vcxo SOH for Instrument %d\n", instrumentId);
			fprintf(soh_out, " Time(secs),          Time(date-time), VcxoValue, TimeDiffAtLock, TimeError, FreqError, CrystalTemp, PLLStatus, GPSStatus");
			fprintf(soh_out, "\n");
		} else {
			fseek(soh_out, 0, SEEK_END);
		}
	}

	if(soh_out != NULL){
		fprintf(soh_out, "%11ld,", seconds);
		fprintf(soh_out, "%25s,", datetime);
		fprintf(soh_out, "%10.2f,", (float)vcxoValue);	
		fprintf(soh_out, "%15.2f,", (float)timeDiffAtLock);	
		fprintf(soh_out, "%10.2f,", (float)timeError);
		fprintf(soh_out, "%10.2f,", (float)freqError);
		fprintf(soh_out, "%12d,", crystalTemp);
		fprintf(soh_out, "%10d,", pllStatus);
		fprintf(soh_out, "%10d", gpsStatus);
		fprintf(soh_out, "\n");
		fflush(soh_out);
	}
	fclose(soh_out);
}

/*
 *  processSlowInternalSOH is fully functional
 */
void HRD24Protocol::processSlowInternalSOH(unsigned char *bundle, const string &station, const unsigned short instrumentId){
	FILE *soh_out = NULL;
	
	unsigned long seconds;
	float	battVoltage;
	float	vcxoTemp;
	float	radioSNR;
	char datetime[32];
	char tFile1[32];
	char tFile2[32];

	struct tm tnow;

	memcpy(&seconds,		&bundle[1],		4);
	memcpy(&battVoltage,	&bundle[5],		4);
	memcpy(&vcxoTemp,		&bundle[9],		4);
	memcpy(&radioSNR,		&bundle[13],	4);

	tnow = *gmtime((time_t *)&seconds);
	strftime(datetime,32,"%Y-%m-%d %H:%M:%S",&tnow);

	sprintf(tFile1, "%s.HRD24-%d.", station.c_str(), instrumentId);	
	strftime(tFile2, 32, "%b.%d.%Y.hrd", &tnow);

	string filename = "/home/sysop/hrd24-SOH-files/";	
	filename.append(tFile1);
	filename.append(tFile2);

	if(soh_out == NULL){
		soh_out = fopen(filename.c_str(), "r+");
		if(soh_out == NULL){
			soh_out = fopen(filename.c_str(), "w+");
			fprintf(soh_out, "HRD Slow Internal SOH for Instrument %d\n", instrumentId);
			fprintf(soh_out, " Time(secs),          Time(date-time), BattVoltage,    VCXOTemp,    RadioSNR");
			fprintf(soh_out, "\n");
		} else {
			fseek(soh_out, 0, SEEK_END);
		}
	}

	if(soh_out != NULL){
		fprintf(soh_out, "%11ld,", seconds);
		fprintf(soh_out, "%25s,", datetime);
		fprintf(soh_out, "%12.4f,", battVoltage);	
		fprintf(soh_out, "%12.4f,", vcxoTemp);	
		fprintf(soh_out, "%12.4f", radioSNR);
		fprintf(soh_out, "\n");
		fflush(soh_out);
	}
	fclose(soh_out);
}

/*
 *  processGPSLocation is fully functional
 */
void HRD24Protocol::processGPSLocation(unsigned char *bundle, const string &station, const unsigned short instrumentId){
	FILE *soh_out = NULL;
	
	unsigned long seconds;
	float	latitude;
	float	longtitude;
	float	elevation;
	char datetime[32];
	char			tFile1[32];
	char			tFile2[32];
	
	struct tm tnow;

	
	memcpy(&seconds,	&bundle[1],		4);
	memcpy(&latitude,	&bundle[5],		4);
	memcpy(&longtitude, &bundle[9],		4);
	memcpy(&elevation,	&bundle[13],	4);
	
	tnow = *gmtime((time_t *)&seconds);
	strftime(datetime,32,"%Y-%m-%d %H:%M:%S",&tnow);

	sprintf(tFile1, "%s.HRD24-%d.", station.c_str(), instrumentId);	
	strftime(tFile2, 32, "%b.%d.%Y.loc", &tnow);

	string filename = "/home/sysop/hrd24-SOH-files/";	
	filename.append(tFile1);
	filename.append(tFile2);

	if(soh_out == NULL){
		soh_out = fopen(filename.c_str(), "r+");
		if(soh_out == NULL){
			soh_out = fopen(filename.c_str(), "w+");
			fprintf(soh_out, "Gps Location SOH for Instrument %d\n", instrumentId);
			fprintf(soh_out, " Time(secs),          Time(date-time),    Latitude,  Longtitude, Elevation\n");
		} else {
			fseek(soh_out, 0, SEEK_END);
		}
	}

	if(soh_out != NULL){
		fprintf(soh_out, "%11ld,", seconds);
		fprintf(soh_out, "%25s,", datetime);
		fprintf(soh_out, "%12.4f,", latitude);	
		fprintf(soh_out, "%12.4f,", longtitude);	
		fprintf(soh_out, "%10.2f", elevation);
		fprintf(soh_out, "\n");
		fflush(soh_out);
	}
	fclose(soh_out);
}

/*
 *  processGPSSatelliteStatus still has problems. No information about how to find out "SolnState,FigMerit,NSatForSoln" values
 */
void HRD24Protocol::processGPSSatelliteStatus(unsigned char *bundle, const string &station, const unsigned short instrumentId){
	FILE *soh_out = NULL;
	
	unsigned long seconds;
	char datetime[32];
	char tFile1[32];
	char tFile2[32];

	char PRNCodes[5];
	char NoiseRatio[5];
	char Activity[5];
	int NSatTracked = 0;
	int NSatForSoln = 0;
	int FigureOfMerit = 0;
	int SolnState = 0;

	struct tm tnow;

	
	memcpy(&seconds,	&bundle[1],		4);
	
	for(int i = 0; i < 5; i++)
	{
		PRNCodes[i]   = bundle[7 + i*2] & 0x1F;
		NoiseRatio[i] = bundle[8 + i*2] & 0x3F;
		Activity[i]   = (bundle[8 + i*2] >> 6) & 0x03;
		if (Activity[i] == 3)
			NSatTracked++;
	}

	
	tnow = *gmtime((time_t *)&seconds);
	strftime(datetime,32,"%Y-%m-%d %H:%M:%S",&tnow);

	sprintf(tFile1, "%s.HRD24-%d.", station.c_str(), instrumentId);	
	strftime(tFile2, 32, "%b.%d.%Y.gst", &tnow);

	string filename = "/home/sysop/hrd24-SOH-files/";	
	filename.append(tFile1);
	filename.append(tFile2);



	if(soh_out == NULL){
		soh_out = fopen(filename.c_str(), "r+");
		if(soh_out == NULL){
			soh_out = fopen(filename.c_str(), "w+");
			fprintf(soh_out, "Gps Channel SOH for Instrument %d\n", instrumentId);
			fprintf(soh_out, " Time(secs),          Time(date-time),SolnState,FigMerit,NSatForSoln,NSatTracked,Act1,Act2,Act3,Act4,Act5,SNR1,SNR2,SNR3,SNR4,SNR5,PRN1,PRN2,PRN3,PRN4,PRN5\n");
		} else {
			fseek(soh_out, 0, SEEK_END);
		}
	}

	

	if(soh_out != NULL){

		fprintf(soh_out, "%11ld,", seconds);
		fprintf(soh_out, "%25s,", datetime);

		fprintf(soh_out, "%9d,", SolnState);
		fprintf(soh_out, "%8d,", FigureOfMerit);		
		fprintf(soh_out, "%11d,", NSatForSoln);		
		fprintf(soh_out, "%11d,", NSatTracked);		

		for(int i = 0; i < 5; i++)
		{
			fprintf(soh_out, "%4d,", Activity[i]);
		}
		for(int i = 0; i < 5; i++)
		{
			fprintf(soh_out, "%4d,", NoiseRatio[i]);
		}
		for(int i = 0; i < 5; i++)
		{
			fprintf(soh_out, "%4d,", PRNCodes[i]);
		}

		
		fprintf(soh_out, "\n");

		
		fflush(soh_out);
	}
	fclose(soh_out);
}

void HRD24Protocol::processData(const unsigned char *buf, int len)
{
	int   bundles    = (len - 25) / 17;
	int   type       = buf[6];
	char stationID[2];
	int   oldestSeq  = 0;
	int   sequence   = 0;
	int   timeSecs   = 0;
	int	  pNSamp     = 0;
	int   lastSamp   = 0;
	short timeFrac   = 0;
	short instrument = 0;
	short channel    = 0;
	short sampleRate = 0;
	short tempShort  = 0;
	char  byteDiff   = 0;
	short wordDiff   = 0;
	unsigned short recvCRC    = 0;
	unsigned short calcCRC    = 0;
	int   dwordDiff   = 0;
	int   sPtr = 0;
	int i;

	unsigned char bundleFrame[17];
	unsigned char compressionType;

	double pktTime = 0;
  
	char  *cbyte;	
	

	
	memcpy(&recvCRC,  &buf[len-2], 2);
	// copy the header contents into local fields
	// note these are little endian (LSB first)
	memcpy(&oldestSeq,  &buf[2], 4);
	memcpy(&timeSecs,   &buf[7], 4);
	memcpy(&timeFrac,   &buf[11], 2);
	memcpy(&instrument, &buf[13], 2);
	memcpy(&sequence,   &buf[15], 4);
	memcpy(&tempShort,    &buf[19], 1);

	channel = tempShort & 0x07;
	sampleRate = (tempShort & 0xF8) >> 3;

	
	pktTime = timeSecs + 0.0001 * timeFrac;

	//make CRC check
	calcCRC = checkCRC(buf,len-2);			
	if (recvCRC ^ calcCRC){		
		return;
	}

	
	if(type == COMPRESSED_DATA_PACKET_FIRST_TIME || type == COMPRESSED_DATA_PACKET_RETRANSMITTED){
		pNSamp = sampleRateTable[sampleRate];
		if (pNSamp == 0){
			return;
		}

		cbyte = (char *) &lastSamp;
		memcpy(cbyte, buf+20, 3);

		if(cbyte[2]&0x80) cbyte[3]=0xff;      /* sign-extend 4th byte       */		
			for(i=0;i < bundles; i++){
			memcpy(bundleFrame, &buf[23+(i*17)], 17);
			compressionType = bundleFrame[0];
			if (compressionType == 9)
				break;
			for(int j=0;j < 4; j++){
				switch(compressionType & 0xC0){
				case 0x00:
					memcpy(&sampleData[sPtr++], &bundleFrame[j*4+1], 4);
					break;
				case 0x40:				
					byteDiff = bundleFrame[j*4+1];
					sampleData[sPtr++] = (lastSamp+=byteDiff);
					byteDiff = bundleFrame[j*4+2];
					sampleData[sPtr++] = (lastSamp+=byteDiff);
					byteDiff = bundleFrame[j*4+3];
					sampleData[sPtr++] = (lastSamp+=byteDiff);
					byteDiff = bundleFrame[j*4+4];
					sampleData[sPtr++] = (lastSamp+=byteDiff);
					break;
				case 0x80:
					memcpy(&wordDiff, &bundleFrame[j*4+1], 2);
					sampleData[sPtr++] = (lastSamp+=wordDiff);
					memcpy(&wordDiff, &bundleFrame[j*4+3], 2);
					sampleData[sPtr++] = (lastSamp+=wordDiff);
					break;
				case 0xC0:
					memcpy(&dwordDiff, &bundleFrame[j*4+1], 4);
					sampleData[sPtr++] = (lastSamp+=dwordDiff);
					break;
				}
				compressionType <<= 2;
			}
		}

		stationID[0] = channel + 0x30;
		stationID[1] = 0;
		string s(stationID);

		map<string, rc_ptr<OutputChannel> >::iterator p;		
		p = hrd24_channels.find(s);

		if (p->second == NULL){
			return;
		}
		
		send_raw_depoch(p->second->station_name.c_str(), p->second->channel_name.c_str(), pktTime, 0, pNSamp , (int32_t*)sampleData, sPtr);
	}
	else if(type == STATUS_OF_HEALTH_PACKET){

		memcpy(&instrument, &buf[13], 2);
		instrument &= 0x07FF;

		for(i=0;i < bundles; i++){
			memcpy(bundleFrame, &buf[23+(i*17)], 17);
			switch(bundleFrame[0]){
				case 0x22:	/*type 34, HRD Slow Internal SOH Bundle*/
					processSlowInternalSOH(bundleFrame, stationName, instrument);
					break;
				case 0x0D:	/*type 13, GPS Location Bundle*/
					processGPSLocation(bundleFrame, stationName, instrument);
					break;
				case 0x07: /*type 7, VCXO Calibration Bundle*/
					processVCXOCalibration(bundleFrame, stationName, instrument);
					break;
				case 0x0F: /*type 15, GPS Satellite Status/Reference Time Error Bundle*/
					processGPSSatelliteStatus(bundleFrame, stationName, instrument);
					break;
			}
		}

	}

}
void HRD24Protocol::do_start()
{
    //start_acquisition();
    int bPtr = 0;	
	int bof = -1;
	int eof = -1;

	int packetSize = 0;
	unsigned char *compPacket = new unsigned char[1200];

	while(!terminate_proc)
    {
        fd_set read_set, write_set;
        FD_ZERO(&read_set);
        FD_ZERO(&write_set);
        FD_SET(fd, &read_set);

	if(FD_ISSET(fd, &read_set))
        {
            int nread;
            if((nread = read_port(fd, &recvbuf[bPtr], 1)) < 0)
                throw PluginLibraryError("read error");
			if((bPtr > 0) && (recvbuf[bPtr-1] == 0xAA) && (recvbuf[bPtr] == 0xBB) ){
				if(bof != -1){					
					eof = bPtr - 1;
				}else{
					bof = bPtr -1;
				}
			}
			if((eof != -1) && (bof != -1)){ //different
				if ( (((eof - bof - 8) % 17 ) == 0) && (recvbuf[bPtr-1] == 0xAA) && (recvbuf[bPtr] == 0xBB) ){
					packetSize = eof - bof;
					memcpy(compPacket, &recvbuf[bof], packetSize);
					processData(compPacket, packetSize);
					eof = -1;
					bof = -1;					
					if((recvbuf[bPtr-1] == 0xAA) && (recvbuf[bPtr] == 0xBB)){
						recvbuf[0] = 0xAA;
						recvbuf[1] = 0xBB;
						bof = 0;
						bPtr= 2;
					}else{
						bPtr = 0;
					}
					continue;
				}else{
					//eof = bPtr - 1;
					eof = - 1;
				}
				//}
			}
			bPtr++;
			if (bPtr >= RECVBUFSIZE-1){
				eof = -1;
				bof = -1;
				bPtr= 0;
			}
		}

	}
}

RegisterProto<HRD24Protocol> proto("hrd24");

} // unnamed namespace
