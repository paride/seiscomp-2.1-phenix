#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mtio.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include "dpstruc.h"
#include "stuff.h"
#include "timeutil.h"
#include "cfgutil.h"
#include "Tape.h"

extern void perror();

static int   NextRecord     ( char*, int);
static int   NextEofMark    ( void );
static int   CheckAnswer    ( int );

struct mtop  sMaTapeCom;        /* tape command structure                    */
struct mtget sMaTapeStat;       /* tape status structure                     */

static int  fdTape;             /* tape file descritor                       */

#ifdef DEBUG
static int  iGBlocks  = 0L,
            iPBlocks  = 0L;
#endif

static int  iNewFile  = TRUE;
static int  iEofCount = 0L; 


/* ==========================================================================
   Rewind the tape
   ========================================================================== */

int TapeRewind( void )
{

int iAnswer;

    sMaTapeCom.mt_op    = MTREW;
    sMaTapeCom.mt_count = 1;

    iAnswer = ioctl( fdTape, MTIOCTOP, &sMaTapeCom );
    if( iAnswer == 0 ) {
#ifdef DEBUG
        fprintf( stderr, "\n#Tape rewinded ...\n" );
#endif  
        iEofCount = 0L;
        return( (int)SUCCESS );
     } else {
        fprintf( stderr, "\nCan't rewind Tape ...\n" );
        return( (int)FAILURE );
    }
}

/* ==========================================================================
   Eject the tape
   ========================================================================== */

int EjectTape( void ) {

int iAnswer;

    sMaTapeCom.mt_op    = MTOFFL;
    sMaTapeCom.mt_count = 1;

    iAnswer = ioctl( fdTape, MTIOCTOP, &sMaTapeCom );
    if( iAnswer == 0 ) {
#ifdef DEBUG
        fprintf( stderr, "\n#Tape ejected ...\n" );
#endif  
        iEofCount = 0L;
        return( (int)SUCCESS );
     } else {
        fprintf( stderr, "\nCan't eject Tape ...\n" );
        return( (int)FAILURE );
    }
}

/* ==========================================================================
   Close the tape
   ========================================================================== */
int  TapeClose( void ) {

int iRetry = 5;
#ifdef DEBUG
        fprintf( stderr, "\n#Closing tape device ...\n" );
#endif
    iEofCount = 0L;
    while(close( fdTape ) != 0 && iRetry > 0 ){
    	sleep(10);
	iRetry--;
    }
    if( iRetry )
    	return( SUCCESS );
    else
        return( FAILURE );
}

/* ==========================================================================
   Open the tape
   ========================================================================== */
int TapeOpen( LINFO *dinfo ) {
        
    if( (fdTape = open( dinfo->device, O_RDWR|O_SYNC )) >= 0 ) {
        iEofCount = 0L;
        iNewFile = TRUE;
#ifdef DEBUG
        fprintf(stderr,"\nOpening Tape device [%s]", dinfo->device);
#endif
        return( SUCCESS );
    } else {
#ifdef DEBUG
        char caTmp[PATH_LEN];
        sprintf(caTmp,"\nERROR: open error on device [%s](ret %d)", dinfo->device, fdTape );
        perror( caTmp );
#endif
    }
    return( TAPE_ERROR );
}

/* ==========================================================================
   Read next block on tape
   ========================================================================== */
int TapeRead( char*cpBuffer, int iSizeOfBuf ) {

int iAnswer;


    if( iNewFile == FALSE ) {
#ifdef DEBUG
        fprintf( stderr, "\n<TapeRead> read blocks (%d + %d)\n",iGBlocks
                                                           ,iPBlocks );
        iPBlocks = 0;
        iGBlocks = 0;
#endif
        iNewFile = TRUE;
    }
    iAnswer = NextRecord( cpBuffer, iSizeOfBuf );

    if( iAnswer == 0 ) {
        iEofCount++;
        iNewFile = FALSE;
        if( iEofCount == 1 )
            return( (int)EOF_MARK );
        else
            return( (int)EOT_MARK );
    } else if( iAnswer == iSizeOfBuf ) {
#ifdef DEBUG
        iGBlocks++;
#endif
        iEofCount = 0L;         
        return( (int)SUCCESS );         
    } else if( iAnswer > 0 && iAnswer != iSizeOfBuf ) {
#ifdef DEBUG
        iPBlocks++;
#endif          
        iEofCount++;
        iNewFile = FALSE;
        return( (int)EOF_MARK );
    } else {
#ifdef DEBUG
        fprintf( stderr, "\nREAD ERROR: <TapeRead> read blocks (%d + %d)\n",iGBlocks
                                                           ,iPBlocks );
#endif
        iNewFile = FALSE;
        return( (int)READ_ERR );
    }
}

/* ==========================================================================
   Read the block via read() on tape
   ========================================================================== */
static int NextRecord( char* cpBuffer, int iSizeOfBuf ) {

int  iAnswer;

    return( iAnswer = read(fdTape, cpBuffer, iSizeOfBuf) );
}

/* ==========================================================================
   write buffer write() on tape
   ========================================================================== */
int TapeWriteBlk( char* cpBuffer, int iSizeOfBuf ) {

int  iAnswer;
 
    return( iAnswer = write(fdTape, cpBuffer, iSizeOfBuf) );
}


/* ==========================================================================
   Skip over EOF mark to continue reading  with next file 
   ========================================================================== */
static int NextEofMark( void ) {

int iAnswer;

    sMaTapeCom.mt_op    = MTFSF;
    sMaTapeCom.mt_count = 1;

    iAnswer = ioctl( fdTape, MTIOCTOP, &sMaTapeCom );
    ioctl( fdTape, MTIOCGET, (char*)&sMaTapeStat);  

    if( CheckAnswer(iAnswer) ) {
        perror( "\nioctl ERROR in NextEofMark ..." );
        return( FAILURE );
    } else {
#ifdef DEBUG
        fprintf( stderr, "\nNextEofMark skipped over next EOF mark\n" );
#endif
        return( SUCCESS );
    } 
}

/* ==========================================================================
   Skip over n EOF marks 
   ========================================================================== */
int TapeSkipEOF( int iEofMarks ) {

int iAnswer;

    sMaTapeCom.mt_op    = MTFSF;
    sMaTapeCom.mt_count = iEofMarks;

    iAnswer = ioctl( fdTape, MTIOCTOP, &sMaTapeCom );
    ioctl( fdTape, MTIOCGET, (char*)&sMaTapeStat);  

    if( CheckAnswer(iAnswer) ) {
         perror( "\n#ioctl ERROR" );
         return( FAILURE );
    } else {
#ifdef DEBUG
        fprintf( stderr, "\nTapeSkipEOF skipped over %d EOF marks ...\n", iEofMarks );
#endif
        return( SUCCESS );
    } 
}

/* ==========================================================================
   Go to EOM of tape 
   ========================================================================== */
int GoToEOM( void ) {

int iAnswer;

    sMaTapeCom.mt_op    = MTEOM;
    sMaTapeCom.mt_count = 1;

    iAnswer = ioctl( fdTape, MTIOCTOP, &sMaTapeCom );
    ioctl( fdTape, MTIOCGET, (char*)&sMaTapeStat);  

    if( CheckAnswer(iAnswer) ) {
         perror( "\n#ioctl ERROR" );
         return( FAILURE );
    } else {
#ifdef DEBUG
        fprintf( stderr, "\nGoToEOD ...\n" );
#endif
        return( SUCCESS );
    } 
}

/* =========================================================================
   Read first block after next EOF mark
   ========================================================================= */
int GetFirstDataRec( char* cpBuffer, int iSizeOfBuf ) {

int skipstat,
    readstat;

    if( (skipstat = NextEofMark( )) != SUCCESS )
        return( FAILURE );
    if( (readstat = TapeRead(cpBuffer, iSizeOfBuf)) != SUCCESS )
        return( FAILURE );
    else
        return( SUCCESS );
}

/* ==========================================================================
   Skip in front of previopus file mark 
   ========================================================================== */
static int PrevEofMark( void ) {

int iAnswer1,
    iAnswer2;

    sMaTapeCom.mt_op    = MTBSF;
    sMaTapeCom.mt_count = 1;
    iAnswer1 = ioctl( fdTape, MTIOCTOP, &sMaTapeCom );

    sMaTapeCom.mt_op    = MTBSFM;
    sMaTapeCom.mt_count = 1;
    iAnswer2 = ioctl( fdTape, MTIOCTOP, &sMaTapeCom );
    
    ioctl( fdTape, MTIOCGET, (char*)&sMaTapeStat);
    
    if( CheckAnswer(iAnswer1) || CheckAnswer(iAnswer2)) {
        perror( "\nioctl ERROR in PrevEofMark ..." );
        return( FAILURE );
    } else {
#ifdef DEBUG
        fprintf( stderr, "\nPrevEofMark skipped over prev EOF mark\n" );
#endif
        return( SUCCESS );
    }
    
     
}

/* =========================================================================
   Read first block after previous EOF mark
   ========================================================================= */
int GetPrevFirstRec( char* cpBuffer, int iSizeOfBuf ) {

int skipstat,
    readstat,
    iAnswer;

    if( (skipstat = PrevEofMark( )) != SUCCESS )
        return( FAILURE );
    if( (readstat = TapeRead(cpBuffer, iSizeOfBuf)) != SUCCESS )
        return( FAILURE );
    else {
        sMaTapeCom.mt_op    = MTBSR;
        sMaTapeCom.mt_count = 1;
        iAnswer = ioctl( fdTape, MTIOCTOP, &sMaTapeCom );    
        ioctl( fdTape, MTIOCGET, (char*)&sMaTapeStat);
    
        if( CheckAnswer(iAnswer) ) {
            return( FAILURE );
        } else {
#ifdef DEBUG
        fprintf( stderr, "\nSkipped to PrevRec ...\n" );
#endif
            return( SUCCESS );
        }
    }
}


/* =========================================================================
   Skip to end of media
   ========================================================================= */    
int TapeSkipToEOM( void ) {

int iAnswer;

    sMaTapeCom.mt_op    = MTEOM;
    sMaTapeCom.mt_count = 1;

    iAnswer = ioctl( fdTape, MTIOCTOP, &sMaTapeCom );
    ioctl( fdTape, MTIOCGET, (char*)&sMaTapeStat);  

    if( CheckAnswer(iAnswer) ) {
        perror( "\n#ioctl ERROR" );
        return( FAILURE );
    } else {
#ifdef DEBUG
        fprintf( stderr, "\nTapeSkipEOM skipped to EOM ...\n" );
#endif
        return( SUCCESS );
    }
        
}


/* =========================================================================
   Write a EOF mark
   ========================================================================= */    
int TapeWriteEOF( void ) {

int iAnswer;

    sMaTapeCom.mt_op    = MTWEOF;
    sMaTapeCom.mt_count = 1;

    iAnswer = ioctl( fdTape, MTIOCTOP, &sMaTapeCom );
    ioctl( fdTape, MTIOCGET, (char*)&sMaTapeStat);  

    if( CheckAnswer(iAnswer) ) {
        perror( "\n#ioctl ERROR" );
        return( FAILURE );
    } else {
#ifdef DEBUG
        fprintf( stderr, "\nTapeWriteEOF writing EOF mark ...\n" );
#endif
        return( SUCCESS );
    }    
}

/* =========================================================================
   Check if tape has reached max size
   ========================================================================= */
int IsMaxTapeSize( LINFO *dinfo ) {

char caLogPath[PATH_LEN],
     caLine[LINE_LEN];
long total;
struct stat statbuf;
int  iStat,
     iRet;
    
    strcpy( caLogPath, dinfo->logdir );
    addslash( caLogPath );
    strcat(caLogPath, "active_log");   
    iStat = stat( caLogPath, &statbuf );
    
    if( statbuf.st_size == 0 || iStat )
        return( SUCCESS );
        
    if( ReadLastLogLn(caLine) == SUCCESS ) {
        caLine[32] = 0;
        total = atol(&caLine[21]);
        if( total < (dinfo->tapesize * MB) )
            iRet = SUCCESS;
        else {
            MvLogName( dinfo->logdir );
#ifdef EJECT
            EjectTape();
#endif
            iRet = FAILURE;
        }
        return( iRet );
    }
    LogError( dinfo, "Can't get tape size from ", caLogPath );
    return( FAILURE );
}
   
/* =========================================================================
   Check return code form IOCTL
   ========================================================================= */
int CheckAnswer( int code ) {

    switch( code ) {
        case EIO:
        case ENOSPC:
        case EACCES:
        case ENXIO:
        case EBUSY:
        case EOVERFLOW:
        case EINVAL:
        case ENOSYS:
            return( FAILURE );
            break;
        default:
            return( SUCCESS );
    }
}

/* =========================================================================
   Create tape lock file
   ========================================================================= */
void LockTape( char *cpLockPath ) {

char caCmd[LINE_LEN];

    strcpy( caCmd, "touch " );
    strcat( caCmd, cpLockPath );
    addslash( caCmd );
    strcat(  caCmd, "TAPE..LCK" );
    system( caCmd );
}
  
/* ==========================================================================
   Get status of Tape device
   
   returns - TAPE_BUSY       tape device already in use
             TAPE_READY      tape is ready for writing
             TAPE_BOM        tape is at begin of data medium
             TAPE_EOM        tape is at end of media
             TAPE_NO_TAPE    no tape in place
             TAPE_NO_WRITE   tape is write protected
             TAPE_ERROR      tape is positioned at the beginning
               
   ========================================================================== */
int GetTapeStatus( LINFO *dumpinfo ) {

int iAnswer;

    /* flush the driver buffer */
    sMaTapeCom.mt_op    = MTNOP;
    sMaTapeCom.mt_count = 1;
    ioctl( fdTape, MTIOCTOP, &sMaTapeCom );
    
    iAnswer = ioctl( fdTape, MTIOCGET, &sMaTapeStat );
        
    dumpinfo->tape_record_no = sMaTapeStat.mt_blkno;
    dumpinfo->tape_file_no   = sMaTapeStat.mt_fileno;
    dumpinfo->tape_status    = sMaTapeStat.mt_gstat;        

#ifdef DEBUG
        fprintf( stderr, "\nioctl()     (%d)",iAnswer);
        fprintf( stderr, "\nGMT_EOF     (%ld)",GMT_EOF(sMaTapeStat.mt_gstat));
        fprintf( stderr, "\nGMT_EOT     (%ld)",GMT_EOT(sMaTapeStat.mt_gstat));
        fprintf( stderr, "\nGMT_BOT     (%ld)",GMT_BOT(sMaTapeStat.mt_gstat));
        fprintf( stderr, "\nGMT_WR_PROT (%ld)",GMT_WR_PROT(sMaTapeStat.mt_gstat));
        fprintf( stderr, "\nGMT_ONLINE  (%ld)",GMT_ONLINE(sMaTapeStat.mt_gstat));
        fprintf( stderr, "\nGMT_DR_OPEN (%ld)",GMT_DR_OPEN(sMaTapeStat.mt_gstat));
        fprintf( stderr, "\nGMT_SM      (%ld)",GMT_SM(sMaTapeStat.mt_gstat));
        fprintf( stderr, "\nGMT_EOD     (%ld)",GMT_EOD(sMaTapeStat.mt_gstat));
        fprintf( stderr, "\nblkno :%d  fileno: %d",sMaTapeStat.mt_blkno, sMaTapeStat.mt_fileno ); 
#endif
                
    if( iAnswer == EBUSY )  
        return(TAPE_BUSY);
    else if( GMT_DR_OPEN(sMaTapeStat.mt_gstat) )  
        return(TAPE_NO_TAPE); 
    else if( GMT_WR_PROT(sMaTapeStat.mt_gstat) ) 
        return(TAPE_NO_WRITE);
    else if( (GMT_BOT(sMaTapeStat.mt_gstat)   || 
              GMT_EOF(sMaTapeStat.mt_gstat))  &&
              GMT_ONLINE(sMaTapeStat.mt_gstat)) {
        return(TAPE_READY);
    } else {
        return(TAPE_ERROR);
    }
}

/* =========================================================================
   Get EOD status for tape device
   ========================================================================= */
int IsEOD( void ) {

int iAnswer;

        iAnswer = ioctl( fdTape, MTIOCGET, &sMaTapeStat );
        if( GMT_EOD(sMaTapeStat.mt_gstat) )
            return( SUCCESS );
        else
            return( FAILURE );
}

/* =========================================================================
   Get BOT status for tape device
   ========================================================================= */
int IsBOT( void ) {

int iAnswer;

        iAnswer = ioctl( fdTape, MTIOCGET, &sMaTapeStat );
        if( GMT_BOT(sMaTapeStat.mt_gstat) )
            return( SUCCESS );
        else
            return( FAILURE );
}

/* =========================================================================
   Get EOF status for tape device
   ========================================================================= */
int IsEOF( void ) {

int iAnswer;

        iAnswer = ioctl( fdTape, MTIOCGET, &sMaTapeStat );
        if( GMT_EOF(sMaTapeStat.mt_gstat) )
            return( SUCCESS );
        else
            return( FAILURE );
}

/* =========================================================================
   Check if tape is a data tape
   ========================================================================= */
int IsDataTape( LINFO *dinfo ) {

int  iBytes;
int  iRetCode = FAILURE;
char caBuf[BLK_SIZE];

    if( TapeRewind() != SUCCESS ) {
        LogError( dinfo, "Can't rewind device ", dinfo->device );
        return( TAPE_ERROR );
    }
    
    if( (iBytes = NextRecord( caBuf, BLK_SIZE )) != BLK_SIZE ) {
        LogError( dinfo, "Can't read data record from tape ", dinfo->device );
        iRetCode = FAILURE;
    } else {
        caBuf[HDR_LEN] = 0;
        if( StrMatch(caBuf, HDR_PAT) )
            iRetCode = SUCCESS;
        else
            iRetCode = FAILURE;
    }

    if( TapeRewind() != SUCCESS ) {
        LogError( dinfo, "Can't rewind device ", dinfo->device );
        return( TAPE_ERROR );
    }
    return( iRetCode );
}

/* =========================================================================
   Copy a file from disk to tape
   ========================================================================= */
int CopyFileToTape ( LINFO *dumpinfo, char* cpFileName ) {

long    lFileSize;
int     fdSrc,
        iReadBytes;
struct  stat statbuf;
char    caBuf[WR_BLK_SIZE];

    if( (fdSrc = open(cpFileName, O_RDONLY)) < 0 ) {
        LogError( dumpinfo, "Can't open source file ", cpFileName );
        return( FAILURE );
    } else 
        if( read(fdSrc, dumpinfo->HDR, HDR_LEN) < 0 ){
            close(fdSrc);
            return( FAILURE );
        } else {
           dumpinfo->HDR[HDR_LEN] = 0;
           lseek( fdSrc, 0L, SEEK_SET );
        }      
        
    if( stat( cpFileName, &statbuf ) == 0 ) {
        lFileSize = statbuf.st_size;
        dumpinfo->dumpsize = lFileSize;
    } else {
        LogError( dumpinfo, "Can't get status for ", cpFileName );
        close(fdSrc);
        return( FAILURE );
    }
        
    do {
        memset( &caBuf[0], 32, WR_BLK_SIZE );
        iReadBytes = read( fdSrc, &caBuf[0], WR_BLK_SIZE );
        if( iReadBytes < 0 ) {
            LogError( dumpinfo, "Can't read data from file ", cpFileName );
            close(fdSrc);
            return( FAILURE );
        } else if( iReadBytes > 0 ) {
            if( TapeWriteBlk( &caBuf[0], WR_BLK_SIZE) == FAILURE ) {
                LogError( dumpinfo, "Can't dump data record to ", dumpinfo->device );
                close(fdSrc);
                return(FAILURE);
            }
            lFileSize -= iReadBytes;
        }
    } while( iReadBytes != 0 );
        
    close(fdSrc);
    if( lFileSize != 0 ){
        LogError( dumpinfo , "Not all data copied to tape from ", cpFileName );
        return( FAILURE );
    } else {
        LogSuccess( dumpinfo ); 
        return( TapeWriteEOF() );
    }       
}



