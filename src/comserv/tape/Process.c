#include <stdio.h>
#include <unistd.h>
#include "stuff.h"
#include "timeutil.h"
#include "cfgutil.h"
#include "lockutil.h"
#include "Tape.h"

/* ************************************************************************** */
/*                         Globals for initialisation                         */
/* ************************************************************************** */
static LINFO dump_info;
static DINFO disk_info;

/* ************************************************************************** */
/*                         Cleanup logs and eject tape                        */
/* ************************************************************************** */
void MvLogAndEject(  ) {
char time_str[256];
    
    MvLogName( dump_info.logdir );
    
    strcpy(time_str, localtime_string(dtime()));
    printf ("%s - Ejecting tape.\n", time_str);
    fflush (stdout);
    
    TapeOpen( &dump_info );
    EjectTape();
    TapeClose();
    return;
}

/* ************************************************************************** */
/* starts the whole dump process and disk control                             */
/* ************************************************************************** */

int DumpMainLoop( int* InitFlag ) {

int i, status,
    iRetCode = SUCCESS;

    if( *InitFlag == FALSE ) {
        if( ReadConf(&dump_info, &disk_info) == FAILURE )
            return( FAILURE );
        else if ((status = acquire_lock (dump_info.lockfile)) < 0) {
            fprintf(stderr, "Unable to lock lockfile: "
                "%s status=%d errno=%d\n", dump_info.lockfile, status, errno);
            return( FAILURE );
        }
        else
            *InitFlag = TRUE;                       
    } else {
        if( StrMatch(dump_info.device, "[N,n][O,o][N,n][E,e]") != TRUE ) {           
            if( CheckDiskUsage(&disk_info, &dump_info, DO_NOT_DELETE ) == DUMP_TO_TAPE ) {           
#ifdef DEBUG
            fprintf( stderr, "\nDump forced ...." );
#endif
                if( TapeLock(&dump_info) == FALSE ){
                    sleep( (unsigned int)10 );
                    return( FAILURE );
                }
                if( TapeOpen( &dump_info ) != SUCCESS ){
                    return( FAILURE );
                }          
                if( disk_info.dump_stream_count >= 0 ) {
                    for( i=0; i <= disk_info.dump_stream_count; i++ ) {
                        if( GetDumpFiles(disk_info.dump_streams[i] , &disk_info, &dump_info) > 0 ) {
                            if( DumpFiles(&disk_info, &dump_info) != SUCCESS ){
                                iRetCode = FAILURE;
                            }
                        }   
                    }
                } else
                    fprintf( stderr, "\nNo stream for dumping selected ...");

                TapeClose();
                TapeUnlock( &dump_info );
            }
            
        } /* End of CheckDiskUsage */       
        CheckDiskUsage(&disk_info, &dump_info, FORCE_DELETE );
        
#ifndef NO_SLEEP
        sleep( dump_info.sleep * 60 );
#endif        
    }
    return( iRetCode );              
}
