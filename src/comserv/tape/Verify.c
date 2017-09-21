#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include "stuff.h"
#include "timeutil.h"
#include "cfgutil.h"
/* #include "timedef.h" */
#include "Tape.h"

extern int fdLog;

/* ==========================================================================
   Check if the log is synchronous with the tape data
   ========================================================================== */
int SyncRecording( LINFO *dinfo ) {
char caLogLine[LOG_SIZE],
     caTapeBuf[BLK_SIZE],
     caTmp1[LOG_SIZE],
     caTmp2[LOG_SIZE];
int  iIsOk, 
     iLogOk, 
     iTapeOk,
     iNext,
     iLastFile  = 0,
     iHeadFiles = 0,
     iFileCount = 0;

    if( IsMaxTapeSize(dinfo) != SUCCESS )
        return( FAILURE );
        
    if( GetTapeStatus( dinfo) == TAPE_NO_TAPE ) {
        LogError( dinfo, "No tape in drive, or drive offline for ", dinfo->device );
        return( FAILURE );
    }
         
    if( IsEOF() != SUCCESS && IsBOT() != SUCCESS ) {
        LogError( dinfo, "Can't stat tape position, rewinding and syncing ", dinfo->device );
        TapeRewind();
        SyncRecording( dinfo );
    }
         
    if( IsEOF() == SUCCESS && IsActiveLog(dinfo->logdir) == SUCCESS ) {
        ReadLastLogLn( caLogLine );
        GetPrevFirstRec(caTapeBuf, BLK_SIZE);

        strncpy( caTmp1, &caTapeBuf[TAPE_OFFSET], CMP_LEN );
        caTmp1[CMP_LEN] = 0;
        strncpy( caTmp2, &caLogLine[LOG_OFFSET], CMP_LEN );
        caTmp2[CMP_LEN] = 0;
#ifdef DEBUG_VERIFY
    fprintf( stderr, "\nEnd LOG >%s<\nEnd TAP >%s<\nEnd", caTmp2, caTmp1 );
#endif  
        if( strcmp( caTmp1, caTmp2 ) == 0 ) {
            TapeSkipToEOM();
            return( SUCCESS );
        } else {
            return( FAILURE );
        }
    }
    
    if( IsEOF() == SUCCESS && IsActiveLog(dinfo->logdir) == FAILURE ) {
        LogError( dinfo, "No active log file found, rewinding and syncing ", dinfo->device );
        TapeRewind();
        SyncRecording( dinfo );
    }        
    
                 
    if( IsBOT() == SUCCESS && IsActiveLog(dinfo->logdir) == FAILURE ) {
#ifdef DEBUG_VERIFY
    fprintf( stderr, "\nIsBOT() == SUCCESS && IsActiveLog(%s) == FAILURE", dinfo->logdir );
#endif
        if( IsDataTape( dinfo ) == SUCCESS ) {
            LogError( dinfo, "Found a used Comserv Data Tape in Tape Drive ", dinfo->device );
            return( FAILURE );
        } else {
            return( SUCCESS );
        }
    } 
    
    if( IsBOT() == SUCCESS && IsActiveLog(dinfo->logdir) == SUCCESS ) {
#ifdef DEBUG_VERIFY
    fprintf( stderr, "\nIsBOT() == SUCCESS && IsActiveLog(%s) == SUCCESS", dinfo->logdir );
#endif
        RewindLog();
        if( ReadLastLogLn(caLogLine) == SUCCESS ) {
           caLogLine[7] = 0;
           iLastFile = atoi( caLogLine );
           if( iLastFile > 4 )
                iHeadFiles = 4;
           else
                iHeadFiles = 0;
        }
        RewindLog(); 
        if( TapeRead(caTapeBuf, BLK_SIZE) == SUCCESS &&
            ReadLogLn(caLogLine) == SUCCESS ) {
            iFileCount = 1;    
            iNext = TRUE;
            do {
                strncpy( caTmp1, &caTapeBuf[TAPE_OFFSET], CMP_LEN );
                caTmp1[CMP_LEN] = 0;
                strncpy( caTmp2, &caLogLine[LOG_OFFSET], CMP_LEN );
                caTmp2[CMP_LEN] = 0;   
                iIsOk = strcmp( caTmp1, caTmp2 );
#ifdef DEBUG_VERIFY
    fprintf( stderr, "\nLOG >%s<\nTAP >%s<\nCMP %d", caTmp2, caTmp1, iIsOk );
#endif
                if( iIsOk )
                    return( DIFFER );
                        
                iTapeOk = GetFirstDataRec(caTapeBuf, BLK_SIZE);
                iLogOk = ReadLogLn(caLogLine);
                    
                if( iLogOk == SUCCESS &&  iTapeOk == SUCCESS ){
                    iFileCount++;
                    if( iHeadFiles )
                        if( iFileCount >= (iHeadFiles - 1) ) {
                            SkipLastLogLn();
                            if( TapeSkipEOF(iLastFile - iHeadFiles) == FAILURE )
                                return( FAILURE );
                        }
                    iNext = TRUE;
                } else
                    iNext = FALSE;
                
            } while( iNext );
            TapeSkipToEOM();
        }
       return( SUCCESS ); 
    }

    TapeRewind();
    return( DIFFER );
}       
