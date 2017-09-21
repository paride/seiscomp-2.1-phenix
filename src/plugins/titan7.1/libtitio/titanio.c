/*
 * O. Coutant, 1997, Observatoire de Grenoble, UJF
 */

#include "titanio.h" 
#include "proto.h"
#include "../titan.h"

/* type du dernier device titan accede */
static int devtype;

/**************************** PUBLIC **********************************/
/*
 * topen: ouverture d'un fichier titan avec test sur le nom passe en argument
 *          pour TFILE, doit etre un fichier normal
 *          pour TDISK, doit etre un directory
 *          pour TDAT, doit etre un character device
 * entree:
 *    name= nom du fichier ou du device
 *    type= type du fichier, TDAT,TFILE,TDISK
 * sortie:
 * retour:
 *    pointeur TITFILE ou NULL si erreur
 */

TITFILE *topen(name, type)
char *name;
int type;
{    
TITFILE    *tfp;

    switch (type)
    {
/*
 * Open a regular titan file with no index information
 * Must be a regular file in Unix sense
 */
    case TFILE:
        tfp=file_fopen(name);
        break;
/*
 * Open titan disk. Disk contain data file and index file
 * Must be a directory name, we then open the data file
 */
    case TDISK:
        tfp=disk_fopen(name);
        break;
/*
 * Open titan dat. 
 * Must be a character device
 */
    case TDAT:
        tfp=dat_fopen(name);
        break;

    default:
        return(NULL);
    }

    if (tfp)
    {
        tfp->byteswap=testByteSwap();
        devtype=tfp->type;
    }
    return(tfp);
}


/*
 * topenGuess: ouverture d'un fichier titan avec determination du type
 *          pour TFILE, doit etre un fichier normal
 *          pour TDISK, doit etre un directory
 *          pour TDAT, doit etre un block device
 * entree:
 *    name= nom du fichier ou du device
 * retour:
 *    pointeur TITFILE ou NULL si erreur
 */
TITFILE* topenGuess(name)
char *name;
{    
extern FILE *Fp_log;

  fprintf(Fp_log,"\n  RUNNING CVTIT Rev %s (Fels,Coutant)\n", REVISION);
  if (isFile(name))
  {
    fprintf(Fp_log,"  titan file %s\n",name);
    return(topen(name,TFILE));
  }
  else if (isDir(name))
  {
    fprintf(Fp_log,"  titan disk %s\n",name);
    return(topen(name,TDISK));
  }
  else if (isCharDev(name))
  {
    fprintf(Fp_log,"  titan dat  %s\n",name);
    return(topen(name,TDAT));
  }
  else
  {
    fprintf(Fp_log,"can't find %s\n",name);
    fprintf(Fp_log,"type (file, disk or tape??)\n");
    return(NULL);
  }
}


/*
 * fermeture d'un fichier titan
 */
void tclose(fp)
TITFILE *fp;
{
    switch (fp->type) {
    case TFILE:
        fclose(tFILE(fp));
        break;
    case TDISK:
#ifdef USE_CACHE
        cclose(fp->fd);
#else
        fclose(tFILE(fp));
#endif
        if (((TF_DISK*)fp)->index)
            free(((TF_DISK*)fp)->index);
        break;
    case TDAT:
        if (((TF_DAT*)fp)->index)
            free(((TF_DAT*)fp)->index);
#ifdef USE_CACHE
        cclose(fp->fd);
#else
        free(((TF_DAT*)fp)->buffer);
        close(fp->fd);
#endif
    }
    free(fp->name);
    free(fp);
}


#ifdef ST_HAS_PARTITION
/*
 * tSetPart: selectionne la partition courante, pour un
 * fichier DAT uniquement
 * entree:
 *    fp= pointeur fichier titan
 *    part= numero partition (0= donnees, 1= index)
 * sortie:
 * retour:
 *    1 en cas de succes, 0 sinon
 */
int tSetPart(fp, part)
TITFILE *fp;
int part;
{
    int ier;

    switch (fp->type)
    {
    case TFILE:
    case TDISK:
        return(0);
    case TDAT:
        /*fpurge(tFILE(fp));*/
        ier=stSetPartition(fp->fd,part);
        if (!ier)
            return(ier);
        ier=stSetPos(fp->fd,MTREW,0);
        if (!ier)
            return(ier);
        return(1);
    }
    return(0);
}
#endif /* ST_HAS_PARTITION */


/*
 * tGetSize: recupere la taille d'un fichier titan
 *
 * entree: 
 *    fp= pointeur fichier titan
 * sortie:
 * retour:
 *    taille du fichier ou -1 si erreur
 */
int tGetSize(fp)
TITFILE *fp;
{
    int    taille;
    struct stat    fs;

    switch (fp->type) {
    case TFILE:
    case TDISK:
        stat(fp->name,&fs);
            taille=fs.st_size;
        break;
#ifndef AIX
    case TDAT:
        stSetPos(FD(fp),MTEOM,0);
        taille=stGetPos(FD(fp))*32768;
        break;
#else
        return(0);
#endif
    default:
        return(-1);
    }
    return(taille);
}



/*
 * teof: test sur fin de bande idem feof()
 */
int teof(fp)
TITFILE *fp;
{
    return(fp->teof(fp));
}



/*
 * tFILE: retourne le pointeur de fichier FILE* associe
 *          a un pointeur de fichier titan ou NULL si erreur
 */
FILE* tFILE(fp)
TITFILE *fp;
{
        return(fp->tfile(fp));
}


/*
 * trewind: positionne en debut de fichier titan, idem rewind()
 */
void trewind(fp)
TITFILE *fp;
{
    fp->trewind(fp);
}


/*
 * tread: lecture de donnees binaire bufferises sur unnn fichier titan,
 *           idem fread()
 */
int tread(buffer, size, elmt, fp)
void *buffer;
int size;
int elmt;
TITFILE* fp;
{
    devtype=fp->type;
    return(fp->tread(buffer,size,elmt,fp));
}


/*
 * tseek: positionnement dans un fichier titan, argument idem a fseek
 */
int tseek(fp, n, whence)
TITFILE *fp;
int n;
int whence;
{
    devtype=fp->type;
    return(fp->tseek(fp,n,whence));
}


/*
 * ttell: renvoie la position dans un fichier titan.
 *            Pour un fichier de type TDISK ou TFILE, position en octet
 *            Pour un fichier de type TDAT ?, position en block de 32768?
 */
int ttell(fp)
TITFILE *fp;
{
    devtype=fp->type;
    return(fp->ttell(fp));
}


char ttype()
{
    switch (devtype) {
    case TDAT:
        return('D');
    case TFILE:
        return('F');
    case TDISK:
        return('H');
    default:
        return(' ');
    }
}

/*
 **************************** PRIVEE *********************************
 */

int testByteSwap()
{
char        temp[40];
    find_wordorder(temp);
    if (!strncmp(temp, "3210", 4)) return(TRUE);
    else                           return(FALSE);
}


/*
 * isFile: test if regular file. Allows 1 level of symbolic link
 */
int isFile(name)
char *name;
{
struct stat fs;
char   link[512];

    memset(link,0,512);
    stat(name,&fs);
    if (S_ISREG(fs.st_mode))
        return(1);
    else if (S_ISLNK(fs.st_mode)) {
        readlink(name,link,512);
        stat(link,&fs);
        if (S_ISREG(fs.st_mode))
            return(1);
        else
            return(0);
    } else
        return(0);
}



/*
 * isDir: test if directory . Allows 1 level of symbolic link
 */
int isDir(name)
char *name;
{
struct stat fs;
char   link[512];

    memset(link,0,512);
    stat(name,&fs);
    if (S_ISDIR(fs.st_mode))
    {
        return(1);
    }
    else if (S_ISLNK(fs.st_mode))
    {
        readlink(name,link,512);
        stat(link,&fs);
        if (S_ISDIR(fs.st_mode))
            return(1);
        else
            return(0);
    }
    else
        return(0);
}



/*
 * isCharDev: test if character device . Allows 1 level of symbolic link
 */
int isCharDev(name)
char *name;
{
struct stat fs;
char   link[512];

    memset(link,0,512);
    stat(name,&fs);
    if (S_ISCHR(fs.st_mode))
        return(1);
    else if (S_ISLNK(fs.st_mode)) {
        readlink(name,link,512);
        stat(link,&fs);
        if (S_ISCHR(fs.st_mode))
            return(1);
        else
            return(0);
    } else
        return(0);
}


/*
 * hasIndex: return 1 if titan file has a trigger index array
 *           which is not empty, 0 otherwise
 */
int hasIndex(tfp)
TITFILE *tfp;
{
    switch (tfp->type) {
    case TFILE:
        return(0);
    case TDISK:
        if (((TF_DISK*)tfp)->nindex)
            return(1);
        else
            return(0);
    case TDAT:
        if (((TF_DAT*)tfp)->nindex)
            return(1);
        else
            return(0);
    }
    return(0);
}


/*
 * getEventOfs: return the position in byte where
 *              to start to look for an event
 * (should do a linear interpolation, but...)
 */
int getEventOfs(time, tfp)
int time;
TITFILE *tfp;
{
long       nindex,i,addrUnit;
TITINDEX  *index;

    index = NULL;
    nindex = 0;
    addrUnit = 0;
    switch (tfp->type)
    {
      case TFILE:
        return(0);
      case TDISK:
        nindex=((TF_DISK*)tfp)->nindex;
        index=((TF_DISK*)tfp)->index;
    addrUnit=1;    /* address given in byte */
        break;
      case TDAT:
        nindex=((TF_DISK*)tfp)->nindex;
        index=((TF_DISK*)tfp)->index;
        addrUnit=DAT_BLOCKSIZE; /* address given in block */
        break;
    }
/*
    printf("getEventOfs: time=%ld  otime=%ld \n",
        time, index[0].time);
*/

    if (!nindex)             return(0);
    if (time<index[0].time)  return(0);

    for (i=1; i<nindex; i++)
    {
        if (time >= index[i-1].time && time < index[i].time) break;
    }
    if (i==nindex) {
        printf("CHIOTTE");
        return(index[nindex-1].beg_ofs*addrUnit);
    } else
/* beg_ofs - 10 par securite */
      return((index[i-1].beg_ofs)*addrUnit-10*DAT_BLOCKSIZE);

/*
    printf("getEventOfs: %ld>=%ld  pos=%ld\n",
        time, index[i-1].time, index[i-1].beg_ofs);
*/
}

int getEventOfs_(time, tfp)
int time;
TITFILE *tfp;
{
    int        nindex,i,pos,otime;
    TITINDEX    *index;

    nindex = 0;
    index = NULL;
    switch (tfp->type)
    {
    case TFILE:
        return(0);
    case TDISK:
        nindex=((TF_DISK*)tfp)->nindex;
        index=((TF_DISK*)tfp)->index;
        break;
    case TDAT:
        nindex=((TF_DISK*)tfp)->nindex;
        index=((TF_DISK*)tfp)->index;
        break;
    }
    if (!nindex)
        return(0);

    otime=index[0].time;
    pos=index[0].beg_ofs;
    if (otime>time)
        return(0);
    for (i=1;i<nindex && time>index[i].time ;i++)
    {
        otime=index[i].time;
        pos=index[i].beg_ofs;
    }
    return(pos);
}

