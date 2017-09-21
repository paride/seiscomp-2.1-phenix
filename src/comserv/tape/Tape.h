#define FAILURE         (-1)
#define SUCCESS         (0)
#define EOF_MARK        (1)
#define EOT_MARK        (2)
#define READ_ERR        (4)
#define TAPE_BUSY       (5)
#define TAPE_READY      (6)
#define TAPE_BOT        (7)
#define TAPE_EOT        (8)
#define TAPE_NO_TAPE    (9)
#define TAPE_NO_WRITE   (10)
#define TAPE_ERROR      (11)
#define LOG_ERROR       (12)
#define LOG_EOF         (13)
#define DIFFER          (14)

#define DUMP_TO_TAPE    (100)
#define NONE            (101)

#define FORCE_DELETE    (1)
#define DO_NOT_DELETE   (0)

#define ALL              "*"

#define LOG_SIZE        (95)
#define LINE_LEN        (128)
#define PATH_LEN        (128)
#define WR_BLK_SIZE     (4096)
#define BLK_SIZE        (512)
#define HDR_LEN         (81)
#define HDR_OFFSET      (54)
#define LOG_OFFSET      (34)
#define TAPE_OFFSET     (21)
#define CMP_LEN         (60)


#ifndef TRUE
#define TRUE            (1)
#endif
#ifndef FALSE
#define FALSE           (0)
#endif

extern  FILE    *fpTape;
extern  int     fdLog,
                fdDLog;
extern  FILE*   fpErLog;

typedef char            BYTE;   /* signed byte                  */
typedef unsigned char   UBYTE;  /* unsigned byte                */
typedef short           WORD;   /* 16 bit signed                */
typedef unsigned short  UWORD;  /* 16 bit unsigned              */
typedef int             LONG;   /* 32 bit signed                */
typedef unsigned int    ULONG;  /* 32 bit unsigned              */
typedef char            CHAR;   /* 7 bit character, high bit 0  */
typedef unsigned char   UCHAR;  /* 8 bit character, unsigned.   */
typedef signed char     SCHAR;  /* 8 bit character, signed.     */
typedef float           FLOAT;  /* IEEE floating point          */

typedef struct _sdr_time {
    UWORD   year;
    UWORD   day;
    UBYTE   hour;
    UBYTE   minute;
    UBYTE   second;
    UBYTE   pad;
    UWORD   ticks;
} LOG_TIME;

typedef struct _loginfo {
        BYTE     initfile[128];
        BYTE     lockfile[128];
        BYTE     device[128];
        BYTE     logdir[128];
        BYTE     datadir[128];
        ULONG    tapesize;
        ULONG    retry;
        ULONG    sleep;
        FLOAT    defdisksize;
        BYTE     station[6];
        BYTE     channel[4];
        BYTE     network[3];
        BYTE     location[3];
        BYTE     ch_dir[128];
        ULONG    filecount;
        ULONG    bytecount;
        BYTE     dumpfile[128];
        LONG     dumpsize;
        LONG     tape_record_no;
        LONG     tape_file_no;
        long     tape_status;
        BYTE     block[BLK_SIZE];
        ULONG    chkfilecount;
        ULONG    chkfilesize;
        ULONG    chkfilestotal;
        BYTE     HDR[HDR_LEN+1];

} LINFO;

#define MAX_STREAM      (100)
#define MB              (1000000)
#define DONE_PAT        "#D_*"
#define ERR_PAT         "#E_*"
#define DONE_POST       "#D_"
#define ERR_POST	"#E_"
#define DUMP_PAT        "[A-Z][A-Z]*.??.???.?.????.???.????"
#define HDR_PAT         "??????? ?????????.?????????????????,???,??:??:??.????~????,???,??:??:??.????~~~??"

typedef struct _dsize {
        BYTE    stream[32];
        FLOAT   size;
        FLOAT   real_size;
} SIZE;

typedef struct _dinfo {
        BYTE    initfile[128];
        SIZE    *max_dir_size[MAX_STREAM];
        BYTE    *dump_streams[MAX_STREAM];
        WORD    dump_stream_count;
        ULONG   is_dir_size;
        ULONG   delete_size;
} DINFO;

typedef struct dirent DIRENT;

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   Configuration pathes and global vars
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
extern FILE *fpLog;
extern char sname[5];
extern char name[9];
extern char *network_ini;
extern char *stations_ini;

char   str2[128];
char   str1[128];
config_struc    cfg;

   
/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   function prototypes
   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
   
/* tape functions */

extern int   GetTapeStatus  ( LINFO* );
extern int   TapeRewind     ( void ); 
extern int   TapeOpen       ( LINFO* ); 
extern int   TapeRead       ( char*, int ); 
extern int   TapeClose      ( void ); 
extern int   TapeWriteBlk   ( char*, int);
extern int   TapeSkipEOF    ( int );
extern int   TapeSkipToEOM  ( void );
extern int   TapeWriteEOF   ( void );
extern int   GetFirstDataRec( char*, int );
extern int   CopyFileToTape ( LINFO*, char* );
extern int   IsEOD          ( void );
extern int   IsBOT          ( void );
extern int   IsEOF          ( void );
extern int   IsMaxTapeSize  ( LINFO* );
extern void  LockTape       ( char* );
extern int   EjectTape      ( void );
extern int   IsDataTape     ( LINFO* );
extern int   GoToEOM        ( void );
extern void  MvLogAndEject  ( void );
extern int   TapeLock       ( LINFO* );
extern int   TapeUnlock     ( LINFO* );

/* log functions */

extern int   OpenDumpLog    ( char* );
extern int   OpenLog        ( char* );
extern int   CloseLog       ( int );
extern int   OpenErrorLog   ( char* );
extern int   ReadLogLn      ( char* );
extern char* WriteLogLn     ( char* );
extern char* ReadLogHdr     ( char* );
extern int   ReadTapeVolHdr ( LINFO* );
extern char* WriteLogHdr    ( char* );
extern int   IsActiveLog    ( char* );
extern char* ActiveLogName  ( char*, char* );
extern char* FinishLogName  ( char*, char* );
extern int   MvLogName      ( char* );
extern int   SyncRecording  ( LINFO* );
extern void  LogError       ( LINFO* , char*, char* );
extern void  LogSuccess     ( LINFO* );
extern void  RewindLog      ( void );
extern int   SkipLastLogLn  ( void );
extern int   ReadLastLogLn  ( char* );
extern int   GetPrevFirstRec( char*, int );


/* functions to verify data written on tape */
extern int   VerifyTapeData( void );

/* file functions */

extern int  CheckDiskUsage ( DINFO*, LINFO*, int );
extern int  CheckDir       ( char*, DINFO*, LINFO*, float*, float );
extern int  GetDirFileList ( char*, DINFO*); 
extern int  GetNextFileName( char* );
extern void RemoveFiles    ( char*, float, float );
extern int  RenameFile     ( char* , char* );

extern int  GetDumpFiles   ( char*, DINFO*, LINFO* );
extern int  DumpFiles      ( DINFO*, LINFO* );

/* configure functions */

extern int   ReadConf       ( LINFO*, DINFO* );
extern char* GetInitFileName( char*, char* );


/* Dump main loop */

extern int DumpMainLoop( int* );

/* Functions in main */

extern int TerminateProgram( int );

/* misc functions */

extern int    StrMatch      ( char*, char* );
extern int    ExceptFile    ( char* );
extern int    ForceDump     ( char* );
extern double dtime         ( void );
extern int    FileFind      ( char*, char*, char* );


