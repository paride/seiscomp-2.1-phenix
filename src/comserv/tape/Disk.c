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

char    caDir[128],                       /* start directory and pattern for dump loop */
        caFile[128];
static  DIRENT **dirlist;
static  DIRENT **namelist;
static  char *except_file_list[5] = { "..", ".", "active", ".*", "" };

typedef struct _rem_file{
    char caFileName[PATH_LEN];
    int  iSize;
    int  iTime;
    struct _rem_file *next;
    struct _rem_file *prev;
}FILELIST;

static  FILELIST *FileList,*FirstFile,*ListRoot;        /* files to erase              */     
static  void AddToRemoveList( char*, int, int );        /* add filename to remove list */
static  void InitList( void );                          /* init remove file list       */
static  int  dumpstatus = NONE;
static  void FreeScanList( int, DIRENT** );
static  void FreeDelList ( FILELIST* );

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   Free scandir memory
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
void FreeScanList( int iCount, DIRENT** spList) {
    
    if( iCount <= 0 )
        return;
            
    while( iCount--){
        free( spList[iCount] );
    }
    free( spList );
   
    return;
}

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   create a list of all file in parent data dir
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
int GetDirFileList( char* cpStartDir, DINFO *dinfo ) {
int filecount;
        
    if((filecount = scandir( cpStartDir, &dirlist, 0, alphasort)) < 0 ){
        perror("scandir");
        return( 0L );
    } else {
        return( filecount );
    }
}

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   Rename dumped file
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
int RenameFile( char *cpPath, char *cpPost ) {

int i,
    iLen = strlen(cpPath),
    iFound = FALSE;
char caFile[LINE_LEN],
     caPath[LINE_LEN],
     caNewName[LINE_LEN],
     caOldName[LINE_LEN];
     
    if( iLen < 2 ) 
        return( FAILURE );
    
    strcpy( caOldName, cpPath );
    for( i = iLen; i >= 0; i-- ) {
        if( *(cpPath+i) == '/' ) {
            *(cpPath+i) = 0;
            iFound = TRUE;
            break;
        }
    }
        
    if( iFound == FALSE ) 
        return( FAILURE );
        
    strcpy( caPath, cpPath );
    strcpy( caFile, cpPath + i + 1 );
    strcpy( caNewName, caPath );
    addslash( caNewName );
    strcat( caNewName, cpPost );
    strcat( caNewName, caFile );
#ifdef DEBUG_DISK
    fprintf( stderr,"\nRename %s\nto %s", caOldName, caNewName );
#endif
    if( rename( caOldName, caNewName ) != 0 )
        return( FAILURE );
    else
        return( SUCCESS );
}

   

int GetDataFileList( char* startdir, DINFO *dinfo ) {
int filecount;

    if((filecount = scandir( startdir, &namelist, 0, alphasort )) < 0 ){
        perror("scandir");
        return( 0L );
    } else {
        return( filecount );
    }
}

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   Don't look at this files
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
int  ExceptFile( char *filename ) {
int i;

    for( i = 0; strlen(except_file_list[i]) > 0; i++ ) 
        if( StrMatch(filename, (char*)except_file_list[i]) )
            return( TRUE );
        
    return( FALSE );
}   

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   Is this a _D__ file ?
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
int  ForceDump( char *filename ) {
        
    if( StrMatch(filename, (char*)DUMP_PAT ) ) { 
        return( TRUE );
    }
                     
    return( FALSE );
}

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   Check total bytes for all streams
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
int CheckDiskUsage( DINFO *diskinfo, LINFO *dumpinfo, int iForceDelete ) {

struct stat     statbuf;
int     filecount = 0, 
        delcount;
int     i;
BYTE    pathname[128];
int     dirdefault = TRUE;
float   real_size = 0;
char    remove = FALSE;

    dumpstatus = NONE;
    for( i = 0; diskinfo->max_dir_size[i]; i++ )
        diskinfo->max_dir_size[i]->real_size = 0;
                
    delcount = filecount = GetDirFileList(dumpinfo->datadir, diskinfo ); 
    while( filecount-- ) {
        if( ExceptFile(dirlist[filecount]->d_name) ) 
            continue;                
        strcpy( pathname, dumpinfo->datadir);
        addslash( pathname );
        strcat(pathname, dirlist[filecount]->d_name );
        stat( pathname, &statbuf );   
        if( S_ISDIR((mode_t)statbuf.st_mode) ) { 
            dirdefault = TRUE;
            for( i = 0; diskinfo->max_dir_size[i]; i++ ) {
                if( StrMatch(dirlist[filecount]->d_name, 
                    diskinfo->max_dir_size[i]->stream) ){
#ifdef DEBUG_DISK               
        fprintf(stderr,"\n%s matches %s",diskinfo->max_dir_size[i]->stream,dirlist[filecount]->d_name);
        fprintf(stderr,"\nchecking dir -> %s size %f", pathname, diskinfo->max_dir_size[i]->size );
#endif
                    diskinfo->max_dir_size[i]->real_size = 0;
                    InitList();
                    remove = CheckDir(  pathname, 
                                        diskinfo, 
                                        dumpinfo, 
                                        &(diskinfo->max_dir_size[i])->real_size,
                                        diskinfo->max_dir_size[i]->size );
                    if( remove && iForceDelete ) 
                        RemoveFiles( pathname,
                                     diskinfo->max_dir_size[i]->size,
                                     diskinfo->max_dir_size[i]->real_size);
                    dirdefault = FALSE;
                    FreeDelList( ListRoot );  
                } 
            }
            if( dirdefault ) {
#ifdef DEBUG_DISK               
        fprintf(stderr,"\nchecking dir -> %s size %f", pathname, dumpinfo->defdisksize);
#endif
                real_size = 0;
                InitList();
                remove = CheckDir( pathname,
                                   diskinfo, 
                                   dumpinfo, 
                                   &real_size,
                                   dumpinfo->defdisksize );
                if( remove && iForceDelete ) 
                    RemoveFiles( pathname, dumpinfo->defdisksize, real_size );
                FreeDelList( ListRoot );
            }       
        }
    }
    FreeScanList( delcount, dirlist );

    return( dumpstatus );   
}

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   Check disk space limit
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
int CheckDir( char *dirname, DINFO *diskinfo, LINFO *dumpinfo, float *real_size, float max_size ){

struct stat  statbuf;
int     max_bytes;
int     filecount = 0,
        delcount;
BYTE    pathname[PATH_LEN];
int     stat0 = 1;
   
    max_bytes = MB * max_size;
    delcount = filecount = GetDataFileList(dirname, diskinfo );
    while( filecount-- ) {
        if( ExceptFile(namelist[filecount]->d_name) )
            continue;
        if( ForceDump(namelist[filecount]->d_name) )
            dumpstatus = DUMP_TO_TAPE; 
                
        strcpy( pathname, dirname);
        addslash( pathname );
        strcat(pathname, namelist[filecount]->d_name );
        
        stat0 = stat( pathname, &statbuf );             
        if( stat0 == 0 ) {  
            if( S_ISREG((mode_t)statbuf.st_mode) ){
                *real_size += statbuf.st_size;
#ifdef CLEAN_ANY_FILE
                AddToRemoveList( pathname, (int)statbuf.st_mtime, (int)statbuf.st_size );
#else                
                if( StrMatch(namelist[filecount]->d_name, DONE_PAT) ){ 
                    AddToRemoveList( pathname, (int)statbuf.st_mtime, (int)statbuf.st_size );
                }
#endif
            }
        } 

#ifdef DEBUG_DISK        
        if( *real_size > (int)max_bytes )
            fprintf(stderr,"\n\tMAX %d  REAL %f", max_bytes, *real_size );
#endif
    }
    FreeScanList( delcount, namelist );

    if( *real_size > (int)max_bytes )
        return( TRUE );
    else
        return( FALSE );
} 

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   Init the remove list
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
static void InitList( void ){
FILELIST *New;
    
    if( (New = (FILELIST*)malloc(sizeof(FILELIST))) == NULL ){
        perror("MALLOC");
        return;
    }
    strcpy( New->caFileName, "root" );
    New->iSize = 0;
    New->iTime = 0;
    New->next  = NULL;
    New->prev  = NULL;
    FileList = New;
    ListRoot = FirstFile = FileList;
    return;  
}

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   Free delete file list
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
void FreeDelList( FILELIST* spList ) {

    if( spList ) {
        FreeDelList( spList->next );
        free( spList );
    }
}

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   Add file name to remove list
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
static void AddToRemoveList( char *cpFileName, int iTime, int iSize ) {
FILELIST *New;
            

    if( strcmp(FileList->caFileName, "root") == 0 ) {
         strcpy( FileList->caFileName, cpFileName );
         FileList->iSize = iSize;
         FileList->iTime = iTime;
#ifdef DEBUG_DISK
    fprintf( stderr, "\nAdding %s to list ...",FileList->caFileName );
#endif
    } else {
        if( (New = (FILELIST*)malloc(sizeof(FILELIST))) == NULL ){
            perror("MALLOC");
            return;
        }    
        strcpy( New->caFileName, cpFileName );
        New->iSize = iSize;
        New->iTime = iTime;
        New->next  = NULL;
        New->prev  = FileList;
        FileList->next = New;
        FileList = FileList->next;
#ifdef DEBUG_DISK
    fprintf( stderr, "\nAdding %s to list ...",New->caFileName );
#endif
    }
    
    return;  
}

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   Remove named file
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
void RemoveFiles( char *cpPath, float fMaxSize, float fRealSize ) {

int iMaxSize,
    iRealSize;

    iMaxSize = (int)(fMaxSize * MB);
    iRealSize = (int)(fRealSize);
    
    if( FileList->prev == NULL )
        return;
        
#ifdef DEBUG_DISK
    fprintf( stderr, "\nremoving file %s ... [%d %d %d]", 
                    FileList->caFileName, iMaxSize, iRealSize, FileList->iSize );
#endif

#ifndef NO_DELETE
    if( remove(FileList->caFileName) )
        perror("unlink");
    else 
        iRealSize -= FileList->iSize;
#else
        iRealSize -= FileList->iSize;
#endif
           
    while( iRealSize > iMaxSize ) {
        if( FileList->prev == NULL )
            break;
        FileList = FileList->prev;

#ifndef NO_DELETE
        if( remove(FileList->caFileName) )
            perror("unlink");
        else
            iRealSize -= FileList->iSize;
#else
        iRealSize -= FileList->iSize;
#endif

#ifdef DEBUG_DISK
    fprintf( stderr, "\nremoving file %s ...[%d %d %d]", 
                    FileList->caFileName, iMaxSize, iRealSize, FileList->iSize );
#endif            
    }    
    return; 
}

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   Get all files for dumping to tape
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
int GetDumpFiles( char* pattern, DINFO *diskinfo, LINFO *dumpinfo ) {
char    newpattern[128];
int     retcode;

    strcpy( newpattern, "*" );  
    strcat( newpattern, sname );
    strcat( newpattern, ".??.");
    strcat( newpattern, pattern );
    strcat( newpattern, ".????.???.????");

#ifdef DEBUG_DISK
        fprintf( stderr,"\nStart directory %s Pattern %s", dumpinfo->datadir, newpattern );
#endif

    FileFind( NULL, NULL, "f" );    
    retcode = FileFind( dumpinfo->datadir, newpattern, "bdr" );
    strcpy(caDir,dumpinfo->datadir);
    strcpy(caFile, newpattern );
        
    return( retcode );       
} 
