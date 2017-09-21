#ifndef _titantio_h
#define _titantio_h

/*
 * O. Coutant, 1997, Observatoire de Grenoble, UJF
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <malloc.h>
#include <signal.h>
#include <unistd.h>
#include <pwd.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <dirent.h>


#ifdef AIX
#include <sys/tape.h>
#else
#include <sys/mtio.h>
typedef unsigned char uchar;
#endif


#ifdef MT_ST_CAN_PARTITIONS
#define ST_HAS_PARTITION
#else
#undef  ST_HAS_PARTITION
#endif

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

typedef int   (*PFI)();
typedef void  (*PFV)();
typedef FILE* (*PFF)();

#define TFILE 1
#define TDAT  2
#define TDISK 3

/*========================================================================
                   Titan Indexes
  First sixteen bytes
    b0-b3     current systeme time, MSB first.
    b4        channel num on which trigger was detected (triplet).
    b5        index type
    b6        index size: 0 if "true" trigger index, 1 if "filling" index.
    b7        unused.
    b8-b11    end RAM offset
                b8       number of RAM cycles for end offset.
                b9-b11   end RAM offset, MSB first.
    b12-15    beg RAM offset
                b12      number of RAM cycles for beg offset.
                b13-b15  beg RAM offset, MSB first.
  Second sixteen bytes, except for DAT
    b16       info flag: if 1, next following 15 bytes are not significant.
    b17-b18   unused.
    b19       alert.
    b20-b22   duration.
    b23-b25   channel 1 max value, MSB first.
    b26-b28   channel 2 max value, MSB first.
    b29-b31   channel 3 max value, MSB first.

NOTES
-----
Index type:
    - if b5 = 0, index is a trigger index and all subsequent info is valid.
    - if b5 = 1, index is a "filling" index, and the only siginificant
                 fields are the time end the beg_ofs (b12-b15). 
                 This type of "false" index is used on TITAN-DISK to 
                 give the user some indication between the relationship
                 between time and the state of writing on the hard drive.

Recording mode:
    - For Titan station recording in trigger mode, all fields are filled.
    - For Titan station recording continuously, the only siginificant
                 fields are the time end the beg_ofs (b12-b15).

For TITAN-DISK:
    -  "RAM" must be replaced by "DISK" in the above.
    -  number of cycles has no meaning and beg_offset, end_offset must
       be read on b12-b15 and b8-b11, respectively.

======================================================================*/

/* -------------------- structrure TITINDEX -----------------------*/
/*                    ------ OLD --------
typedef struct {
    int     time;        * first sixteen bytes *
    uchar   triplet;
    uchar   indexType;
    uchar   indexSize;
    uchar   dum0;
    int     endAddr;
    int     beginAddr;
    uchar   info[16];    * second sixteen bytes except for DAT *
} TITINDEX;
*/

typedef struct {
    int     time;
    int     chan;
    int     index_type;
    int     index_size;
    int     beg_ofs;
    int     beg_ofs_ncycle;
    int     end_ofs;
    int     end_ofs_ncycle;
    int     info_flag;
    int     alert;
    int     duration;
    int     max_chan1;
    int     max_chan2;
    int     max_chan3;
} TITINDEX;

#define    INDEX_SIZE       32
#define    DATINDEX_SIZE    16


/*
 * Structure titanio pour maintenir l'abstraction "fichier titan"
 * autant que possible independante du support (DAT, Fichier, 
 * disque dur)
 */
/*
 * --------------------- structrure TITFILE -----------------------
 */
#define TITFILE_HEADER \
    short  type;     /*type de source, FILE, DAT, DISK*/ \
    int    fd;       /*pointeur fichier, depend du type (FILE*, int,...)*/\
    int    byteswap; /*ordre stockage interne (Big Endian ...)*/         \
    char  *name;     /*nom du fichier/device ouvert*/ \
/* procedures io */ \
    PFI    tread;   \
    PFI    tseek;   \
    PFI    ttell;   \
    PFI    teof;    \
    PFF    tfile;   \
    PFV    trewind

typedef struct {
    TITFILE_HEADER;
} TITFILE;

/*
 * -------------------- Fichier titan -------------------------
 */
typedef struct {
    TITFILE_HEADER;
} TF_FILE;

/*
 * --------------------- Cassette DAT -------------------------
 */
typedef struct {
    TITFILE_HEADER;
/* partie reservee au dat */
    char     *buffer;
    char     *pt;
    char     *bufend;
    int       bufsize;
    int       nblock;
    int       nindex;    /* nombre d'index */
    TITINDEX *index;     /* tableau des index */
} TF_DAT;
/*
 * --------------------- Disque minititan -----------------------
 */
typedef struct {
    TITFILE_HEADER;
/* partie reservee au disk */
    char     *buffer;
    char     *pt;
    char     *bufend;
    int       bufsize;
    int       nblock;
    int       nindex;
    TITINDEX *index;
    int       eof;    /* position de fin du fichier */
} TF_DISK;


#ifdef USE_CACHE
#include "cache.h"
#define FD(fp) (CACHE(fp->fd)->fd)
#define CACHE_SIZE 32768*8
#else
#define FD(fp) (fp->fd)
#endif

#define    DAT_BLOCKSIZE 32768


/*
 * TOUTES les definitions sous AIX deviennent st au lieu de mt.....
 */
#ifdef AIX
#define mtop stop
#define MTIOCTOP        STIOCTOP
#define MTIOCMD         STIOCMD
#define MTIOCHGP        STIOCHGP
#define STOFFL          STOFFL
#define MTREW           STREW
#define MTERASE         STERASE
#define MTRETEN         STRETEN
#define MTWEOF          STWEOF
#define MTFSF           STFSF
#define MTRSF           STRSF
#define MTFSR           STFSR
#define MTRSR           STRSR
#define MTINSRT         STINSRT
#define MTEJECT         STEJECT
#endif


/* cache.c */

#define MAXCSIZE 32780*8
#define MAXCACHE 5
/*
 * La structure CACHE est partagee entre le processus de lecture
 * sur DAT et le processus de traitement donnees/titan via la memoire
 * partagee
 * 
 * "active_buffer" pointe sur le buffer qui vient d'etre rempli
 * si "active_buffer" vaut 0, on est en train de lire
 * si "active_buffer" vaut -1, il y a une erreur de lecture
 * si "active_buffer" vaut -2, c'est la fin de fichier
 * sinon, active_buffer pointe sur buffer1 ou buffer2
 */
typedef struct {
    int  stopflag;             /* indicate stop time to subprocess */
    int  bufsize;              /* bufsize to read from device */
    char *active_buffer;       /* points toward active buffer */
    char *active_buffer_end;   /* end of active buffer */
    char buffer[3][MAXCSIZE];  /* 3 io buffers */
    int  ibuf;                 /* index of active buffer in buffer */
    int  rpid;                 /* pid of reading subprocess */
    int  fd;                   /* file descriptor */
    int  shmid;                /* shared memory identifier */
    int  semid;                /* semaphore(s) identifier */
    int  dblock;               /* next block to be read from tape */
    int  cblock;               /* next block to be read from cache */
    int  closeDelay;           /* delay before closing */
} Cache;

#define CACHE(i) ((Cache*)i)
#define CACHE_READING ((char*) 0)
#define CACHE_EOF     ((char*)-1)
#define CACHE_ERROR   ((char*)-2)

/* pour pallier les deficiences Linux ... */
#ifndef SHM_W
#define SHM_W 200
#endif
#ifndef SHM_R
#define SHM_R 400
#endif
#ifndef SEM_A
#define SEM_A SHM_W
#endif
#ifndef SEM_R
#define SEM_R SHM_R
#endif

#endif
