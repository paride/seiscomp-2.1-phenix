/****************************************************************/
/*								*/
/* LogAnalyzer 	- shows event detection				*/
/*		- shows Clock Information			*/
/*		- shows the hole LogFile			*/
/*								*/
/* Version:		0.92					*/
/* Date:		2000/05/31				*/
/* Author:		Ronny Kopischke            		*/
/*			former version by W. Ruesing		*/
/* EMail:		roko@gfz-potsdam.de			*/
/* Organization:        GFZ Potsdam				*/
/*								*/
/****************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <math.h>
#include <time.h>

double log2(double);

off_t lseek (int fd, off_t offset, int whence);

#include "qlib2.h"
#include "dpstruc.h"
#include "timeutil.h"
#include "service.h"
#include "datalog.h"
#include "FileStr.h"

#define MAX_BLKSIZE			4096
#define	DEFAULT_BLKSIZE		512
#define VOLLEN				(8 + 3+4+4+2+5+2+3+23+23+1+1+2)
#define BEGTIME_OFFSET		(8 + 3+4+4+2+5+2+3)
#define VOL_TIMELEN			23
#define ENDTIME_OFFSET  	(BEGTIME_OFFSET+VOL_TIMELEN)
#define NULL_STR			"(null)"
#define TIME_FMT			1

FINFO 		*find_finfo 		(DATA_HDR *hdr);
char 		*datafile_path  	(FINFO *fip);
char 		*datadir_path 		(FINFO *fip);
char 		*date_string		(INT_TIME time, char *dflt_str);
EXT_TIME 	file_time_limit 	(EXT_TIME file_time, char *duration);
char 		*channel_duration 	(DURLIST *head, char *channel);
char 		*build_filename 	(FINFO *fip);
void 		boolean_mask 		(short *ps, short bit_mask, char *str);
char 		*downshift			(char *);

typedef struct _sel {			/* Structure for storing selectors.	     */
    int nselectors;				/* Number of selectors.			         */
    seltype *selectors;			/* Ptr to selector array.		         */
} SEL;

char 	BeginTime[17],
	EndTime[17],
	LookPattern[100],
	OutDir[256];
    
int iCanScan     = FALSE,
    iStation     = FALSE,
    iFilePattern = FALSE,
    iBeginTime   = FALSE,
    iEndTime     = FALSE,
    iOutDir      = FALSE,
    iStatList	 = FALSE;

int PrintClock   = FALSE,
    PrintPick    = FALSE,
    PrintTape    = FALSE,
    PrintPattern = FALSE,
    DetectedPick = FALSE;
    
double 	aT,eT,aF,eF;	

/*********************************************************************** */
/*  Externals required in multiple files.				                 */
/*********************************************************************** */
extern DURLIST  durhead[5];		/* data, detection, timing, calib, log	 */
extern char 	*extension[5];	/* data, detection, timing, calib, log	 */
extern char 	pidfile[1024];	/* pid file.				             */
extern short 	data_mask;		/* data mask for cs_setup.		         */


/* Convert blanks to 0 character */

void ConvTo0( char cpStr[] ) {
int iSize = strlen( cpStr );

	while( iSize > 0 ){
		if( cpStr[iSize] == ' ' )
			cpStr[iSize] = '0';
		iSize--;
	}
}

/* change YYYYMMDDhhmmss -> YYYYMMDD_hhmmss */

void ToFlagTime( char InTime[], char OutTime[] ) {

	strncpy( OutTime, InTime, 8 );
	OutTime[8] = '_';
	strcpy( &OutTime[9], &InTime[8] );

}

/* *********************************************************************** */
/* read volume header from comserv data file                               */
/* *********************************************************************** */
int read_vol(FINFO *fip, char* vol, char* file)
{
    int nscan;
    int lrl;
    INT_TIME *p_it;
    char tmpstr[512];
    char *p;
    long lStartTime, lEndTime;
    int bticks, eticks;
/*
    strcpy (vol,"000001V 0080073 2.309MCC    CLZ1994,285,17:34:29.0000~1994,285,19:14:58.4993~~~BK");
*/
/*
	nscan = sscanf(vol,"*%06d%*c%*c%*03d%*04d%*4.1f%02d%-5.5s%-2.2s%-3.3s%04d,%03d,%02d:%02d:%02d.%04d~%04d,%03d,%02d:%02d:%02d.%04d~~~%-2.2s",
*/


	nscan = sscanf(vol,"%*06d%*c%*03d%*04d%*4f%02d%5s%2s%3s%04d,%03d,%02d:%02d:%02d.%04d~%04d,%03d,%02d:%02d:%02d.%04d~~~%2s",
	       &lrl,
	       fip->station, fip->location, fip->channel, 
	       &fip->begtime.year,   &fip->begtime.doy,    &fip->begtime.hour,
	       &fip->begtime.minute, &fip->begtime.second, &bticks,
	       &fip->endtime.year,   &fip->endtime.doy,    &fip->endtime.hour,
	       &fip->endtime.minute, &fip->endtime.second, &eticks,
	       fip->network);

    fip->begtime.usec = bticks * 100;
    fip->endtime.usec = eticks * 100;
	       
	nscan = sscanf (&vol[19],"%02d",&lrl);
	strncpy (fip->station,&vol[21],5);
	strncpy (fip->location,&vol[26],2);
	strncpy (fip->channel,&vol[28],3);

	p = &vol[31];
	(void)charvncpy (tmpstr, p, 22, 0);
	p += strlen(tmpstr) + 1;
	if ((p_it = parse_date (tmpstr)) == NULL) {
	    fprintf (stderr, "Error reading starttime on volume hdr in file %s\n%s\n",
		     file,vol);
	    return(-1);
	}
	fip->begtime = int_to_ext(*p_it);

	charvncpy (tmpstr, p, 22, 0);
	p += strlen(tmpstr) + 1;
	if ((p_it = parse_date (tmpstr)) == NULL) {
	    fprintf (stderr, "Error reading endtime on volume hdr in file %s\n%s\n",
		     file,vol);
	    return(-1);
	}
	fip->endtime = int_to_ext(*p_it);
	
	/* Skip over next 2 variable fields. */
	charvncpy (tmpstr, p, 22, 0);
	p += strlen(tmpstr) + 1;
	charvncpy (tmpstr, p, 22, 0);
	p += strlen(tmpstr) + 1;

	strncpy (fip->network,p,2);
	 
	lStartTime = jconv(fip->begtime.year,fip->begtime.doy);
	lStartTime += fip->begtime.hour * 3600 + fip->begtime.minute * 60 + fip->begtime.second;
	lEndTime   = jconv(fip->endtime.year,fip->endtime.doy);
	lEndTime   += fip->endtime.hour * 3600 + fip->endtime.minute * 60 + fip->endtime.second;

	{
	char    caStartYYYYMMDD[9]        = "00000000", 							/* argv times */	
			caStarthhmmss[7]        = "000000",
			caStartYYYYMMDDhhmmss[15] = "00000000000000",
			caEndYYYYMMDD[9]          = "00000000",
			caEndhhmmss[7]          = "000000",
			caEndYYYYMMDDhhmmss[15]   = "00000000000000",
			
			caFStartYYYYMMDD[9]        = "00000000", 							/* file times */
			caFStarthhmmss[7]        = "000000",
			caFStartYYYYMMDDhhmmss[15] = "00000000000000",
			caFEndYYYYMMDD[9]          = "00000000",
			caFEndhhmmss[7]          = "000000",
			caFEndYYYYMMDDhhmmss[17]   = "00000000000000";
	
		/* convert YY.DOY.hh.mm.ss -> YY.MM.DD.hh.mm.ss */
		sprintf( caFStartYYYYMMDD, "%4d%2d%2d",	fip->begtime.year,
							fip->begtime.month,
							fip->begtime.day );
		sprintf( caFStarthhmmss, "%2d%2d%2d",   fip->begtime.hour,
							fip->begtime.minute,
							fip->begtime.second );
											
		strcpy( caFStartYYYYMMDDhhmmss,caFStartYYYYMMDD );
		strcat( caFStartYYYYMMDDhhmmss,caFStarthhmmss );
		ConvTo0( caFStartYYYYMMDDhhmmss );

	
		sprintf( caFEndYYYYMMDD, "%4d%2d%2d", 	fip->endtime.year,
												fip->endtime.month,
												fip->endtime.day );
		sprintf( caFEndhhmmss, "%2d%2d%2d", 	fip->endtime.hour,
												fip->endtime.minute,
												fip->endtime.second );
		
		
		strcpy( caFEndYYYYMMDDhhmmss,caFEndYYYYMMDD );
		strcat( caFEndYYYYMMDDhhmmss,caFEndhhmmss );
		ConvTo0( caFEndYYYYMMDDhhmmss );		
				
		/* copy argv times to local variables */
	
		if( iBeginTime ) {
			strncpy(caStartYYYYMMDD,BeginTime,8);
			caStartYYYYMMDD[8] = 0;
			strcpy(caStarthhmmss,&BeginTime[8]);
			strcpy(caStartYYYYMMDDhhmmss,BeginTime);
		} else
			strcpy(caStartYYYYMMDDhhmmss,caFStartYYYYMMDDhhmmss );
	
		if( iEndTime ) {
			strncpy(caEndYYYYMMDD,EndTime,8);
			caEndYYYYMMDD[8] = 0;
			strcpy(caEndhhmmss,&EndTime[8]);
			strcpy(caEndYYYYMMDDhhmmss,EndTime);
		} else
			strcpy(caEndYYYYMMDDhhmmss,caFEndYYYYMMDDhhmmss );		
	
		/* check for start and end times in data file */
		
		aT = atof(caStartYYYYMMDDhhmmss);
		eT = atof(caEndYYYYMMDDhhmmss);
		aF = atof(caFStartYYYYMMDDhhmmss);
		eF = atof(caFEndYYYYMMDDhhmmss);


/* printf( "\nWerte: aT %f eT %f  - aF %f eF %f",aT, eT, aF, eF ); */

		if( eT < aF )
			return(1);
			
		if( aT > eF )
			return(1);
		
/*		if( (aF >= aT && aF <= eT) || (eF >= aT && eF <= eT) ){ */
		if( (aF <= eT) || (eF >= aT) ){

/* printf( "\nWerte: FILE : %s\n", file ); */

			{
			FILE *ThisFile;
			void read_data();
			
				if( (ThisFile = fopen( file, "r" )) == NULL ) {
					fprintf( stderr, "\nCan't open file %s", file );
					return(2);
				}
				(void)read_data(ThisFile);
				fclose( ThisFile );
			}  
			return(1);
		}
	}
	
	return(0);
}
/* ************************************************************************ */
/*                          flip short and integer                          */
/* ************************************************************************ */ 
short Flip2( short shToFlip ) {
 short shSave1, shSave2;
 
        shSave1 = ((shToFlip & 0xFF00) >> 8);
        shSave2 = ((shToFlip & 0x00FF) << 8);
 
        return( shSave1 | shSave2 );
}
 
int Flip4( int iToFlip ) {
int iSave1, iSave2, iSave3, iSave4;
 
        iSave1 = ((iToFlip & 0xFF000000) >> 24);
        iSave2 = ((iToFlip & 0x00FF0000) >> 8);
        iSave3 = ((iToFlip & 0x0000FF00) << 8);
        iSave4 = ((iToFlip & 0x000000FF) << 24);
 
        return( iSave1 | iSave2 | iSave3 | iSave4 );
}

/* ************************************************************************ */
/*                    convert julday -> calday                              */
/* ************************************************************************ */

static char daytab[2][13] = {
    {0,31,28,31,30,31,30,31,31,30,31,30,31},
    {0,31,29,31,30,31,30,31,31,30,31,30,31}};


void calday( int doy, int year, char *result ) {
   int yearday;
   int pmonth;
   int pday;
   int i, leap;

	/* convert ascii day */
	yearday  = doy ;

	/* calculate day and month */
     leap = ((year%4 == 0) && ((year%100 != 0) || (year%400 == 0)));

	/* check for error in days */
	if((yearday < 0) || ((yearday-leap)>365))
	{
		fprintf(stderr,"ERROR Year %d does not have %d days\n", year, yearday) ;
		exit(1) ;
	}

     for(i =1;yearday > daytab[leap][i];i++)
          yearday -= daytab[leap][i];
     pmonth = i;
     pday = yearday;

	/* return results */
	/* sprintf(result, "%.4d %.2d=%.2d/%.2d", year, doy, pmonth, pday) ; */
	sprintf(result, "%.4d/%.2d/%.2d", year, pmonth, pday) ;
	return;

}

void calday2( int doy, int year, char *result ) {
   int yearday;
   int pmonth;
   int pday;
   int i, leap;

	/* convert ascii day */
	yearday  = doy ;

	/* calculate day and month */
     leap = ((year%4 == 0) && ((year%100 != 0) || (year%400 == 0)));

	/* check for error in days */
	if((yearday < 0) || ((yearday-leap)>365))
	{
		fprintf(stderr,"ERROR Year %d does not have %d days\n", year, yearday) ;
		exit(1) ;
	}

     for(i =1;yearday > daytab[leap][i];i++)
          yearday -= daytab[leap][i];
     pmonth = i;
     pday = yearday;

	/* return results */
	/* sprintf(result, "%.4d %.2d=%.2d/%.2d", year, doy, pmonth, pday) ; */
	sprintf(result, "%.4d%.2d%.2d", year, pmonth, pday) ;
	return;

}

/* ************************************************************************ */
/*  						        read Logs        	  				    */
/* ************************************************************************ */

#include "dmc_steim.h"


#define DEFAULT_BSIZE 512

void read_data(istream)
FILE *istream;
{
	char 			block[8*DEFAULT_BSIZE];	/* will handle up to 32K */
	MINISEEDHDR 	*mseedhdr=(MINISEEDHDR *) &block[0];
	DATA_HEADER 	*rechdr=(DATA_HEADER *) &mseedhdr->data_hdr;
	SDR_HDR			*SDR = (SDR_HDR *)&block[0]; 
	int 		num;
	static int	blocksize=DEFAULT_BSIZE;
	int 		total_samples = 0;
	int 		samples_in_block;
	int 		block_num = 0;	
	
	/* print out comserv volume header 512Bytes */
	fread(&block[0], sizeof(char), 512, istream);

	num = fread(&block[0], sizeof(char), blocksize, istream);
	if (num != blocksize) return;
	
	while (num == blocksize) {

		block_num++;
		samples_in_block = Flip2(rechdr->Number_of_samples);
		total_samples += samples_in_block;
		
		{
		char logline[512],
		     station[6],
		     channel[3],
		     CalDate[32],
                     TempTime[32];

/* STATION */
		strncpy(&station[0],SDR->station_id,5);
		station[5] = 0;

/* CHANNEL COMPONENT */
		strncpy(&channel[0],(char*)SDR->channel_id,3);
		channel[3] = 0;								
		
		strncpy( logline,&block[56],samples_in_block);
		logline[samples_in_block] = 0;
		  
		calday(Flip2(SDR->time.day),Flip2(SDR->time.year),CalDate);
		calday2(Flip2(SDR->time.day),Flip2(SDR->time.year),TempTime);
		aF = atof(TempTime)*100+SDR->time.hour;
		aF = aF*100+SDR->time.minute;
		aF = aF*100+SDR->time.second;
/*                printf( "\nWerte: aT %f aF %f \n", aT, aF );				*/
/*		printf( "\n%s %-15s %.2d:%.2d:%.2d.%d %s\n  %s",
							station,
							CalDate,
							SDR->time.hour,
							SDR->time.minute,
							SDR->time.second,
							Flip2(SDR->time.ticks),
							channel,
							logline ); */
		if(aF >= aT && aF <= eT)
		{
                  if (DetectedPick)
  		  {
		     strncpy( logline,&block[70],samples_in_block);
		     logline[samples_in_block] = 0;
		     printf( "%s", logline );
		     DetectedPick=FALSE;
		  }
                  if (PrintPattern)
		  {
		     if(strstr(logline,LookPattern)!=NULL) {
		        printf( "%s %-9s  %.2d:%.2d:%.2d: %s", 		station,
									CalDate,
									SDR->time.hour,
									SDR->time.minute,
									SDR->time.second,
									logline );
		     }
		  }
                  if (PrintClock)
		  {
		     if((strstr(logline,"FROM CLOCK")!=NULL) || (strstr(logline,"Clock drift")!=NULL)) {
		        if(strstr(logline,"Clock drift")!=NULL) {
		           strncpy( logline,&block[70],samples_in_block);
		           logline[samples_in_block] = 0;
		        } else {
		           strncpy( logline,&block[67],samples_in_block);
		           logline[samples_in_block] = 0;
		        }
		        printf( "%s %-9s %.2d:%.2d:%.2d  CLOCK: %s", 	station,
									CalDate, 
									SDR->time.hour,
									SDR->time.minute,
									SDR->time.second,
									logline );
		     }

		  }
                  if (PrintPick)
		  {
		     if(strstr(logline,"detector pick")!=NULL) {
		        DetectedPick=TRUE;
		     }
		  }
                  if (PrintTape)
		  {
		     if(strstr(logline,"FROM TAPE")!=NULL) {
		        strncpy( logline,&block[67],samples_in_block);
		        logline[samples_in_block] = 0;
		        printf( "%s %-9s  %.2d:%.2d:%.2d  TAPE: %s", 	station,
									CalDate,
									SDR->time.hour,
									SDR->time.minute,
									SDR->time.second,
									logline );
		     }
		  }
		}
	   }		
	   num = fread(&block[0], sizeof(char), blocksize, istream);
	}
}





/* ************************************************************************ */
/*  						print usage 									*/
/* ************************************************************************ */
void PrintUsage ( void )
{
	fprintf(stderr, "\n\nLogAnalyzer -c show clock messages");
	fprintf(stderr, "\n            -p show detections");
	fprintf(stderr, "\n            -t show tape messages");
	fprintf(stderr, "\n            -l <Searchstring>");
	fprintf(stderr, "\n            -d <StartDirectory>");
	fprintf(stderr, "\n           [-s <Station  Name>]");
	fprintf(stderr, "\n          [[-f <File Pattern>]..]");
	fprintf(stderr, "\n           [-b <Begin time to look for YYMMDDHHMMSS>]");
	fprintf(stderr, "\n           [-e <End time to look for YYMMDDHHMMSS>]");
	fprintf(stderr, "\n\nVersion 0.92 2000/05/31\n\n");
	exit(1);
}

/* ************************************************************************ */
/*                                Main										*/
/* ************************************************************************ */
FINFO fip;
int iJoinedPath = TRUE;
#define BLK_SIZE	(512)

int main(int argc, char** argv )
{
int c,i,
    iPatCount = 0,
    iRetCode  = 0,
    iFile;
    	
char DirName[256],				/* start directory for scannig  		*/
     Station[256],				/* directory pattern to look for 		*/
     *FilePattern[256],			/* file pattern to look for 			*/
     caFile[512],				/* file found by serach 				*/
     caDir[256],				/* directory found by search			*/
     caBuf[512];				/* data buffer  						*/
        
	if( argc <= 1 )
		PrintUsage();
		
	while(1) {
		c = getopt( argc, argv, "cptl:b:d:e:f:s:" );
		if( c == -1 )
			break;
			
		switch(c) {
			case 'b':
					strcpy(BeginTime, optarg);
					iBeginTime = TRUE;
					break;
			case 'e':
					strcpy(EndTime, optarg);
					iEndTime = TRUE;
					break;
			case 'l':
					strcpy(LookPattern, optarg);
					PrintPattern = TRUE;
					break;
			case 'c':
					PrintClock = TRUE;
					break;
			case 'p':
					PrintPick = TRUE;
					break;
			case 't':
					PrintTape = TRUE;
					break;
			case 'd':
					strcpy(DirName, optarg);
					iCanScan = TRUE;
#ifdef DEBUG
	printf( "DirName\t\t%s\n", DirName);
#endif		
					break;
					
			case 's':
					strcpy(Station, optarg);
					iStation = TRUE;
#ifdef DEBUG
	printf( "Station\t\t%s\n", Station);
#endif
					break;
					
			case 'f':
					FilePattern[iPatCount] = (char*)malloc(sizeof(optarg));
					strcpy(FilePattern[iPatCount], optarg);
#ifdef DEBUG
	printf( "FilePattern[%d]\t%s\n",iPatCount,FilePattern[iPatCount]);
#endif
					iPatCount++;
					iFilePattern = iPatCount;
					break;
					
			case '?':
				default:
					fprintf(stderr, "\nUnknown option ...");
					PrintUsage();
		}
	}

	if( ! iCanScan )
		PrintUsage();
			
	strcpy( caDir , DirName );
	strcpy( caFile, "*" );
	if( iStation ) {
		strcat( caDir , "/" );
		strcat( caDir, Station );
	}

	iRetCode = FileFind( caDir, caFile , "rdbj" );
		
	if( iRetCode < 0 ) 
		exit(1);	
		
	for( i = 0; i < iPatCount; i++ ) {
		FileFind( NULL, NULL, NULL );
		strcpy( caDir, DirName );
		strcpy( caFile, FilePattern[i] );
		while( FileFind(caDir,caFile , "#") > 0 ) {
			if( (iFile = open(caFile,O_RDONLY)) > 0 ) {
				 if( read(iFile, caBuf, BLK_SIZE) != BLK_SIZE ){
				 	fprintf( stderr, "\nCan not read from file %s",(char*)caFile );
				 }
				 read_vol( &fip, caBuf, caFile );
				 close( iFile );
			}
		}
	} 
	 
    exit(0);
}

