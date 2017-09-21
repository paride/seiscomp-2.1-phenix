#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "stuff.h"
#include "timeutil.h"
#include "cfgutil.h"
#include "Tape.h"

static char time_str[256];

/* ************************************************************************** */
/* Get configuration file name for this station                               */
/* ************************************************************************** */
char* GetInitFileName( char *station, char* filename ){

char    station_dir[128] = "\0";
config_struc    cfg;

    /* Look for station entry in master station file */
    if (open_cfg(&cfg, stations_ini, sname)) {
        fprintf (stderr,"Could not find station %s\n", sname);
        TerminateProgram(0);
    }
    
    strcpy(time_str,localtime_string(dtime()));
    /* Look for station directory */
    while(1) {
        read_cfg(&cfg, str1, str2);
        if (str1[0] == '\0') break;
        if (strcmp(str1, "DIR") == 0){
            strcpy(station_dir, str2);
            fprintf (stderr,"%s - Lookup fo %s in %s\n",time_str, sname, station_dir );
        }
    }
    if( !strlen(station_dir) ) {
        fprintf (stderr,"%s - Could not find configuration directory for %s\n",time_str, sname);
        TerminateProgram(0);
    } else {
       	fprintf (stderr,"%s - Datadump startup for station %s \n",time_str, sname);
    } 	
    close_cfg(&cfg);
    sprintf( filename, "%s/station.ini", station_dir );
     

    return( filename );
}

                        
                

/* ************************************************************************** */
/* Read tape configuration from init file                                     */
/* ************************************************************************** */
int ReadConf( LINFO* loginfo, DINFO* diskinfo ) {
char    stemp[128];
short   size_count = -1;
short   dump_count = -1;
        
    diskinfo->dump_stream_count = 0;
    GetInitFileName( sname, loginfo->initfile); 

    if(open_cfg(&cfg, loginfo->initfile, name)) {
        fprintf (stderr, "Could not open %s or section not found\n",loginfo->initfile);
        TerminateProgram(0);
    }
    loginfo->sleep = 1;              /* default, sleep 1 minute between loops */   
    do { 
        read_cfg( &cfg, str1, str2 );
 #ifdef DEBUG
    fprintf( stderr, "\nreading conf line [%s] ", str1 );
 #endif              
        if( strcmp(str1, "LOCKFILE") == 0 )
            strcpy(loginfo->lockfile, str2 );
        if( strcmp(str1, "DEVICE") == 0 )
            strcpy(loginfo->device, str2 );
        if( strcmp(str1, "LOGDIR") == 0 )
            strcpy(loginfo->logdir, str2 );
        if( strcmp(str1, "DATADIR") == 0 )
            strcpy(loginfo->datadir, str2 );
        if( strcmp(str1, "TAPESIZE") == 0 )
            loginfo->tapesize = atoi( str2 );
        if( strcmp(str1, "RETRY") == 0 )
            loginfo->retry = atoi( str2 );
        if( strcmp(str1, "DEFDISKSIZE") == 0 )
            loginfo->defdisksize = atof( str2 );
         if( strcmp(str1, "SLEEP") == 0 )
            loginfo->sleep = atoi( str2 );   
        if( strcmp(str1, "DISKSIZE") == 0 ) {
            strcpy( stemp, str2 );
            stemp[5] = 0;
        if( size_count < 0 ) size_count = 0;
            if( (diskinfo->max_dir_size[size_count] = 
                (SIZE*)malloc(sizeof(SIZE))) == NULL ){
                fprintf (stderr, "Could not malloc memory\n");
                TerminateProgram(0);
            } else {
                strcpy(diskinfo->max_dir_size[size_count]->stream, stemp );
                diskinfo->max_dir_size[size_count]->size = atof(&stemp[6]);
                size_count++;
            }
        }                       
        if( strcmp(str1, "DUMPSTREAM") == 0 ) {                 
            strcpy( stemp, str2 );
            stemp[5] = 0;
            if( dump_count < 0 ) dump_count = 0;
            if( (diskinfo->dump_streams[dump_count] = 
                (char*)malloc(32 *sizeof(char))) == NULL ){
                fprintf (stderr, "Could not malloc memory\n");
                TerminateProgram(0);
            } else {
                strcpy(diskinfo->dump_streams[dump_count], stemp );
                diskinfo->dump_stream_count = dump_count;
                dump_count++;
            }
        }                               
    } while( str1[0] != '\0' ); 
    close_cfg( &cfg );

    return( TRUE );
}
                        
                

        
