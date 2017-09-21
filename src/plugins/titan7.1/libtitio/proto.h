/*=========================================================================
        proto.h
 *========================================================================*/
#ifndef _proto_h
#define _proto_h

#include "titanio.h"

#ifdef ANSI_C
/* libutil.c */
void find_wordorder  (char *);
void swap_4byte      (void *);
void time_asc4       (char*, double);
int  bytes2int4      (char, char, char, char);
int  sparse          (char *, char **, char *, int);

/* titanio.c */
TITFILE *topen(char *, int);
TITFILE *topenGuess(char *);
void    tclose(TITFILE *);
int     tSetPart(TITFILE *,int);
int     tGetSize(TITFILE *);
FILE    *tFILE(TITFILE*);
int     teof(TITFILE *);
void    trewind(TITFILE*);
int     tread(void*,int,int,TITFILE*);
int     ttell(TITFILE*);
int     tseek(TITFILE *, int, int);
char    ttype();
int     testByteSwap();
int     isDir(char*);
int     isFile(char*);
int     isCharDev(char*);
int     hasIndex(TITFILE*);
int     getEventOfs(int, TITFILE*);


/* fileio.c */
int       file_fseek(TITFILE*,int,int);
int       file_fread(char*,int,int,TITFILE*);
int       file_ftell(TITFILE*);
void      file_rewind(TITFILE*);
int       file_feof(TITFILE*);
FILE*     file_tfile(TITFILE*);
TITFILE  *file_fopen(char*);

/* diskio.c */
int       disk_fseek(TITFILE*,int,int);
int       disk_fread(char*,int,int,TITFILE*);
int       disk_ftell(TITFILE*);
void      disk_rewind(TITFILE*);
int       disk_feof(TITFILE*);
TITFILE  *disk_fopen(char*);
int       isTitanDisk(char*);
int       disk_getIndex(TITFILE*,int,TITINDEX*,int,char*);

/* datio.c */
int       dat_fseek(TITFILE*,int,int);
int       dat_fread(char*,int,int,TITFILE*);
int       dat_ftell(TITFILE*);
void      dat_rewind(TITFILE*);
int       dat_feof(TITFILE*);
FILE*     dat_tfile(TITFILE*);
TITFILE  *dat_fopen(char*);
int       dat_getIndex(TITFILE*,int,TITINDEX*);

/* libst.c */
int stSetDriver(int);
int stSetPartition(int,int);
int stFormat(int,int,int);
int stSetPos(int,int,int);
int stGetPos(int);

/*  prototypes */
int  copen(char*,int,int);
int  cread(int,char**);
void cclose(int);
void creadNext(int);
int  cseekBlock(int,int,int);
void crewind(int);
int  cfileno(int);

#else
/* libutil.c */
void find_wordorder  ();
void swap_4byte      ();
void time_asc4       ();
int  bytes2int4      ();
int  sparse          ();

/* titanio.c */
TITFILE *topen();
TITFILE *topenGuess();
void    tclose();
int     tSetPart();
int     tGetSize();
FILE    *tFILE();
int     teof();
void    trewind();
int     tread();
int     ttell();
int     tseek();
char    ttype();
int     testByteSwap();
int     isDir();
int     isFile();
int     isCharDev();
int     hasIndex();
int     getEventOfs();

/* fileio.c */
int       file_fseek();
int       file_fread();
int       file_ftell();
void      file_rewind();
int       file_feof();
FILE*     file_tfile();
TITFILE  *file_fopen();

/* diskio.c */
int       disk_fseek();
int       disk_fread();
int       disk_ftell();
void      disk_rewind();
int       disk_feof();
TITFILE  *disk_fopen();
int       isTitanDisk();
int       disk_getIndex();

/* datio.c */
int       dat_fseek();
int       dat_fread();
int       dat_ftell();
void      dat_rewind();
int       dat_feof();
FILE*     dat_tfile();
TITFILE  *dat_fopen();
int       dat_getIndex();

/* libst.c */
int stSetDriver();
int stSetPartition();
int stFormat();
int stSetPos();
int stGetPos();

/*  prototypes */
int  copen();
int  cread();
void cclose();
void creadNext();
int  cseekBlock();
void crewind();
int  cfileno();

#endif

#endif
