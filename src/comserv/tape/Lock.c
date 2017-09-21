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

static void  CheckStaleLock( LINFO*, char* );
static int  LockFileName  ( LINFO*, char*, char* );

extern char sname[];

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   Remove stale lock files
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
static void CheckStaleLock( LINFO *dinfo, char *cpStation ) {

struct dirent **nameList;
int iCnt;
char caMatch[PATH_LEN],
     caDelFile[PATH_LEN];

    iCnt = scandir(dinfo->logdir, &nameList, 0, alphasort);
    if (iCnt < 0)
        return;
    else
        while(iCnt--) {
            sprintf( caMatch, "*.%s.[0-9]*", cpStation );
            if( StrMatch(nameList[iCnt]->d_name, caMatch) )
                sprintf( caDelFile, "%s/%s", dinfo->logdir, nameList[iCnt]->d_name );
                unlink( caDelFile );
            }
} 

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   Compose lock file name
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
static int LockFileName( LINFO* dinfo, char *cpLink, char *cpLock ) {

char *cpSub;

    if( (cpSub = strrchr(dinfo->device, 47)) == NULL )
        return( FALSE );
    
    sprintf( cpLock, "%s%s..LCK.%s", dinfo->logdir, 
                                        cpSub, 
                                        sname ); 
    
    sprintf( cpLink, "%s%s..LCK.%s.%d", dinfo->logdir, 
                                        cpSub, 
                                        sname,
                                        getpid() );
    return( TRUE );

}

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   Lock tape device
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
int TapeLock( LINFO *dinfo ) {

char caLinkName[PATH_LEN],
     caLockName[PATH_LEN],
     caCmd[LINE_LEN];
struct stat statbuf;
int  iRet;

    if( !LockFileName(dinfo, caLinkName, caLockName) ){
        LogError( dinfo, "Can't create logfile name ", caLockName );
        return( FALSE );
    }
     
    stat( caLockName, &statbuf );
    if( S_ISREG((mode_t)statbuf.st_mode) ){
        stat( caLinkName, &statbuf );
        CheckStaleLock( dinfo, sname );
        if( statbuf.st_nlink >= 2 ) {
            return( FALSE );
        } else {
            link( caLockName, caLinkName );
            return( TRUE );
        }         
    } else {
        sprintf( caCmd, "touch %s", caLockName );
        iRet = system( caCmd );
        if( iRet == 127 || iRet == -1 ){
            LogError( dinfo, "Can't create logfile ", caLockName );
            return( FALSE );
        } else {        
            link( caLockName, caLinkName );
            return( TRUE );
        }
    }
}


/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   Unlock tape device
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
int TapeUnlock( LINFO *dinfo ) {

char caLockPath[PATH_LEN],
     caLinkName[PATH_LEN];
struct stat     statbuf;
    
    if( !LockFileName(dinfo, caLinkName, caLockPath) ){
        LogError( dinfo, "Can't find lock file ", caLockPath );
        return( FALSE );
    }
                            
    stat( caLinkName, &statbuf );
    if( statbuf.st_nlink == 2 )
        if( unlink(caLinkName) == 0 ){
            return( TRUE );
        } else {
            LogError( dinfo, "Can't unlink ", caLinkName );
            return( FALSE );
        }
    else {
        LogError( dinfo, "Lock count mismatch on ", caLinkName ); 
        return( FALSE );
    }
}
