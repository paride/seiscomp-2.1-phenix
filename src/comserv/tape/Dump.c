#include <stdio.h>
#include <stdlib.h>
#include <search.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include "stuff.h"
#include "timeutil.h"
#include "cfgutil.h"
#include "Tape.h"

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   Get all files for dumping to tape
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
int DumpFiles( DINFO *diskinfo, LINFO *dumpinfo ) {
extern  char caDir[],
        caFile[];
int     rc, iInitCount = 2,
        initstatus;
char    caPath[PATH_LEN],
        caLogPath[PATH_LEN];        

    OpenLog( ActiveLogName(dumpinfo->logdir, caLogPath) );
    if( SyncRecording(dumpinfo) != SUCCESS ) {
        LogError( dumpinfo, "Can't sync active_log with tape in drive or tape in drive is full ", dumpinfo->device );
        CloseLog( fdLog );
        return( FAILURE );
    }
    CloseLog( fdLog );
                
    while( (rc = FileFind( caDir, caFile, "#")) > 0 ) {

        /* no #D_ postfix */
        if( StrMatch(caFile, DONE_PAT) || 
            StrMatch(caFile, ERR_PAT) )
            continue;
            
        strcpy( caPath, caDir );
        addslash( caPath );
        strcat( caPath, caFile );
        do { 
           initstatus = GetTapeStatus( dumpinfo );
           if( initstatus != TAPE_READY ) 
               iInitCount--;
            else
                break;
        } while( iInitCount );
                
        if( !iInitCount ) {
            LogError( dumpinfo, "Can't get tape status ... rewinding ",dumpinfo->device );
            return( FAILURE );
        }
                                
#ifdef DEBUG
        fprintf( stderr, "\nDumping .. [%s]##F[%s]",caDir, caFile );
        fprintf( stderr,"\nSTATUS %d BLK No : %d FILE No : %d", initstatus, 
                                                               dumpinfo->tape_record_no,
                                                               dumpinfo->tape_file_no );
#endif
        switch( initstatus ) {
            case TAPE_READY :
#ifdef DEBUG
        fprintf( stderr, "\nTape ready ...");
        fprintf( stderr, "\nDumping %s ...", caPath );
#endif
                strcpy( dumpinfo->dumpfile, caFile );
                strcpy( dumpinfo->ch_dir, caDir );
                if( CopyFileToTape( dumpinfo, caPath ) == FAILURE ) {
                    LogError( dumpinfo, "Error dumping file to tape ", dumpinfo->device );
                    if( RenameFile(caPath, ERR_POST ) != SUCCESS ){
                        LogError( dumpinfo, "Can't rename bad file ", caPath ); 
                    } 
                    return( FAILURE );
                } else {
                    if( RenameFile(caPath, DONE_POST) != SUCCESS ){
                        LogError( dumpinfo, "Can't rename dumped file ", caPath );
                        return( FAILURE ); 
                    }
                }
                break;
            default:
                LogError( dumpinfo, "Unknown tape status on ",dumpinfo->device );
        }
    }
    return( SUCCESS );  
}
