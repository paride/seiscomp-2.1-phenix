#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#include "stuff.h"
#include "timeutil.h"
#include "cfgutil.h"
/* #include "timedef.h" */
#include "Tape.h"

static char time_str[256];

/* ==========================================================================
   Open dump logfile for verifying
   ========================================================================== */
int OpenLog( char *cpLogPath ) {

    strcpy(time_str,localtime_string(dtime()));   
    if( (fdLog = open(cpLogPath, O_RDWR|O_CREAT|O_SYNC, 0644 )) < 0 ) {
        fprintf( stderr, "\n%s - Can't open/create logfile %s ...",time_str, cpLogPath );
        return(LOG_ERROR);
    } else {
        return(SUCCESS);
    }
}

/* ==========================================================================
   Open dump logfile
   ========================================================================== */
int OpenDumpLog( char *cpLogPath ) {

    strcpy(time_str,localtime_string(dtime()));    
    if( (fdDLog = open(cpLogPath, O_RDWR|O_CREAT|O_SYNC, 0644 )) < 0 ) {
        fprintf( stderr, "\n%s - Can't open/create dump logfile %s ...",time_str, cpLogPath );
        return(LOG_ERROR);
    } else {
        return(SUCCESS);
    }
}


/* ==========================================================================
   Open error logfile
   ========================================================================== */
int OpenErrorLog( char *cpLogPath ) {

    strcpy(time_str,localtime_string(dtime()));    
    if( (fpErLog = fopen( cpLogPath, "a+" )) == NULL ) {
        fprintf( stderr, "\n%s - Can't open/create error logfile %s ...",time_str, cpLogPath );
        return(LOG_ERROR);
    } else 
        return(SUCCESS);
}

/* ==========================================================================
   Close logfile
   ========================================================================== */
int CloseLog( int fd ) {

    if( close( fd ) ) {
         return( FAILURE );
    } else {
         return(SUCCESS);
    }
}

/* ==========================================================================
   Rewind logfile
   ========================================================================== */
void RewindLog( void ) {

    lseek( fdLog, 0L, SEEK_SET );
}

/* ==========================================================================
   Read next line in log file
   ========================================================================== */
int ReadLogLn( char *cpBuffer ) {

    strcpy(time_str,localtime_string(dtime()));
    if( read( fdLog, cpBuffer, LOG_SIZE ) != LOG_SIZE ) {
        fprintf( stderr, "\n%s - Can't read log line ...", time_str );
        return( FAILURE );
    } else {
        return( SUCCESS );
    }
}

/* ==========================================================================
   Read last log line in file
   ========================================================================== */
int ReadLastLogLn( char *cpBuffer ) {

long lPos;
    
    if( (lPos = lseek(fdLog, -LOG_SIZE, SEEK_END)) < 0 ){
        return( FAILURE );
    } else if( lPos >= 0 ){
        if( read( fdLog, cpBuffer, LOG_SIZE ) != LOG_SIZE )
            return( FAILURE );
        else
            return( SUCCESS );                  
     } else
        return( FAILURE );
}

/* ==========================================================================
   Skip to last log line in file
   ========================================================================== */
int SkipLastLogLn( void ) {

long lPos;
    
    if( (lPos = lseek(fdLog, -LOG_SIZE, SEEK_END)) < 0 ){
        return( FAILURE );                 
     } else
        return( SUCCESS );
}
/* ==========================================================================
   Check if logfile exists
   ========================================================================== */
int IsActiveLog( char *cpLogPath ) {

char caLogPath[PATH_LEN];
struct stat statbuf;
    
    ActiveLogName(cpLogPath, caLogPath);
#ifdef DEBUG_VERIFY
    fprintf( stderr, "\nIsActiveLog(%s)", caLogPath );
#endif    
    if( stat( caLogPath, &statbuf ) == 0 ) {
        if( S_ISREG((mode_t)statbuf.st_mode) && statbuf.st_size ) 
            return( SUCCESS );
        else
            return( FAILURE );
    } else 
        return( FAILURE );
}
        
/* ==========================================================================
   Build active logname
   ========================================================================== */
char *ActiveLogName( char *cpLogPath, char *cpName ) {
char caActiveName[PATH_LEN];

    strcpy( caActiveName, cpLogPath );
    addslash( caActiveName );
    strcat( caActiveName, "active_log" );
    strcpy( cpName, caActiveName );
    return( cpName );
}

/* ==========================================================================
   Build finished logname
   ========================================================================== */
char   *FinishLogName( char *cpLogPath, char *cpName ) {
char   caFinishName[PATH_LEN];
extern char* localtime_string( double );
time_t tTime;
int    i;

    strcpy( caFinishName, cpLogPath );
    addslash( caFinishName );
    strcat( caFinishName, localtime_string((double)time(&tTime)) );
    
    for( i = 0; i < strlen(caFinishName); i++)
        if( caFinishName[i] == ' ' )
            caFinishName[i] = '_';
            
    strcat( caFinishName, "_log" );
    strcpy( cpName, caFinishName );
    return( cpName );
}


/* ==========================================================================
   Move active log to fineshed status
   ========================================================================== */
int MvLogName( char *cpLogPath ){
char caLogFileNameAct[PATH_LEN];
char caLogFileNameOld[PATH_LEN];

    ActiveLogName(cpLogPath, caLogFileNameAct);
    FinishLogName(cpLogPath, caLogFileNameOld);
    return( rename(caLogFileNameAct, caLogFileNameOld) );        
}

/* ==========================================================================
   Write log if successfull
   ========================================================================== */
void LogSuccess( LINFO *dinfo ) {
char caLogPath[PATH_LEN];
char caLine[LINE_LEN];
char caLogLine[LOG_SIZE];
long count,
     size,
     total;

#ifdef DEBUG
        fprintf( stderr, "\nlogging success for %s ...", &(dinfo->HDR)[21] );
#endif
    strcpy( caLogPath, dinfo->logdir );
    addslash( caLogPath );
    strcat(caLogPath, "active_log");
    OpenDumpLog( caLogPath );
    if( ReadLastLogLn(caLine) == SUCCESS ) {
        caLine[7]  = 0;
        caLine[20] = 0;
        caLine[32] = 0;
        count = atol( caLine );
        size  = atol( &caLine[8]);
        total = atol( &caLine[21]);
        count++;
        size  =  dinfo->dumpsize;
        total =  total + size;
    } else {
        count =  1;
        size  =  dinfo->dumpsize;
        total =  size;
    }
    memset( caLogLine, 0, LOG_SIZE );
    sprintf( caLogLine , "%-7ld %-12ld %-12ld %s\n", count, size, total, &(dinfo->HDR)[21] );
    if( write( fdLog, caLogLine, LOG_SIZE ) != LOG_SIZE )
        fprintf( stderr,"\nError writing to file %s !!!", caLogPath);
    CloseLog( fdDLog );   
}

/* ==========================================================================
   Write log if error
   ========================================================================== */
void LogError( LINFO *dinfo, char *cpErrLine, char *cpAppend ) {

char caLogPath[PATH_LEN];
char caLogLine[LINE_LEN];

    strcpy(time_str,localtime_string(dtime()));
#ifdef DEBUG
        fprintf( stderr, "\nlogging error for %s ...", &(dinfo->HDR)[TAPE_OFFSET] );
#endif

    strcpy( caLogPath, dinfo->logdir );
    addslash( caLogPath );
    strcat(caLogPath, "error_log");
    OpenErrorLog( caLogPath );
    
    sprintf( caLogLine, "%s - ", time_str );
    strcat( caLogLine, cpErrLine );
    if( cpAppend )
        strcat( caLogLine, cpAppend );
    fprintf( fpErLog, "%s\n", caLogLine );
    fclose( fpErLog );
}

/* ==========================================================================
   Read volume header from tape
   ========================================================================== */
int ReadTapeVolHdr( LINFO *dinfo ) {

    if( TapeRead(dinfo->block, BLK_SIZE) == READ_ERR ) {
        return(  FAILURE );
    }
        
    if( strcpy(dinfo->HDR, &(dinfo->block)[HDR_OFFSET]) == NULL )
        return( FAILURE );
        
    return( SUCCESS );
}


        
