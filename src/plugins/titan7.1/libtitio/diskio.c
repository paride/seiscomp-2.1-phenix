/*
 * O. Coutant, 1997, Observatoire de Grenoble, UJF
 */
#include "titanio.h" 
#include "proto.h"

/*
 * content:
 *    disk_fread
 *    disk_fseek
 *    disk_feof
 *    disk_rewind
 *    disk_ftell
 *    disk_tfile
 *    isTitanDisk
 * disk_fread, disk_feof, disk_fseek, disk_ftell, take the same arguments
 * than un'disk_' versions,  except for FILE* pointer replaced by TITFILE*
 */

static char pointer[MAXPATHLEN];
static char data[MAXPATHLEN];
static char indexname[MAXPATHLEN];

extern FILE *Fp_log;


/*==================================================================
                              fread
 *=================================================================*/
int disk_fread(buf, elmsize, nelm, tfp)
char *buf;
int elmsize;
int nelm;
TITFILE *tfp;
{
#ifdef USE_CACHE
int     readier,lus,alire;
TF_DISK *fp=(TF_DISK*)tfp;

    lus=0;
    alire=elmsize*nelm;
    while ((lus!=alire)&&(!disk_feof(tfp)))
    {

/* read from buffer */
        for (;(fp->pt<fp->bufend)&&(lus<alire);fp->pt++,lus++)
            buf[lus]=*(fp->pt);

/* fill buffer */
        if (lus!=alire)
        {
            readier=cread(fp->fd,&fp->buffer);
/* fin de fichier, on reste positionne au bloc courant */
            if (readier==0)
                    return(lus/elmsize);
            if (readier==-1)
                    return(-1);
            fp->nblock++;
            fp->pt=fp->buffer;
            fp->bufend=fp->buffer+readier;
        }
    }
    return(lus/elmsize);

#else
    return(fread(buf,elmsize,nelm,tFILE(tfp)));

#endif
}


/*==================================================================
                              feof
 *=================================================================*/
int disk_feof(tfp)
TITFILE *tfp;
{
    int    pos;
    TF_DISK    *fp;
    fp=(TF_DISK*)tfp;
#ifdef USE_CACHE
    pos=disk_ftell(tfp);
    return ((pos>((TF_DISK*)fp)->eof) || ceof(fp->fd) );
#else
    pos=ftell(fp->tfile(fp));
    return ((pos>((TF_DISK*)fp)->eof) || feof(fp->tfile(fp)) );
#endif
}


/*==================================================================
                              fseek
 *=================================================================*/
int disk_fseek(tfp, n, where)
TITFILE *tfp;
int n;
int where;
{
TF_DISK *fp=(TF_DISK*)tfp;

#ifdef USE_CACHE
int    npt,nback,nforward,nbufback,nbufforward,i,ier;
int    readier;

    switch (where)
    {

        case SEEK_CUR:

/* backward motion */
            if (n<0)
            {
                n=-n;
                nback=fp->pt-fp->buffer; 
                if (n<=nback)
                    fp->pt-= n;
                else
                {
                    n-=nback;
                    nbufback=n/fp->bufsize+2;
                    n-=(nbufback-2)*fp->bufsize;
                    ier=cseekBlock(fp->fd,fp->nblock,-nbufback);
                    if (ier==-1) 
                        return(-1);
                    fp->nblock-=nbufback;
                    readier=cread(fp->fd,&fp->buffer);
                    if (readier!=fp->bufsize)
                        return(-1);
                    fp->nblock++;
                    fp->pt=fp->buffer+readier-n;
                    fp->bufend=fp->buffer+readier;
                }

/* forward motion */
            }
            else if (n>0)
            {
                    nforward=fp->bufend-fp->pt;
                    if (n<nforward)
                    {
                        fp->pt+=n;
                    }
                    else
                    {
                        n-=nforward;
                        nbufforward=n/fp->bufsize;
                        n-=nbufforward*fp->bufsize;
                        ier=cseekBlock(fp->fd,fp->nblock,nbufforward);
                        if (ier==-1)
                            return(-1);
                        fp->nblock+=nbufforward;
                        readier=cread(fp->fd,&fp->buffer);
                        if (readier!=fp->bufsize)
                            return(-1);
                        fp->nblock++;
                        fp->pt=fp->buffer+readier-n;
                        fp->bufend=fp->buffer+readier;
                    }
            }
            return(0);
            break;


        case SEEK_SET:
            if (n<0)
                return(-1);
            else
            {
                return(disk_fseek(tfp,n-disk_ftell(tfp),SEEK_CUR));
            }
            break;


        case SEEK_END:
            fprintf(stderr,
                "fseek(..,SEEK_END,..) not implemented for titan disk\n");
            return(-1);

        default:
            return(-1);

    } /* end switch */

#else
    return(fseek(fp->tfile(fp),n,where));
#endif
}


/*==================================================================
                              rewind
 *=================================================================*/
void disk_rewind(tfp)
TITFILE *tfp;
{
TF_DISK    *fp=(TF_DISK*)tfp;

#ifdef USE_CACHE
    int    readier;
    crewind(fp->fd);

    fp->pt=fp->bufend=fp->buffer=NULL;
    fp->nblock=0;
    readier=cread(fp->fd,&fp->buffer);
    if (readier==-1)
        return;
    fp->nblock=1;
    fp->pt=fp->buffer;
    fp->bufend=fp->buffer+readier;

#else
    rewind(fp->tfile(fp));
#endif
}


/*==================================================================
                              ftell
 * return the stream position with respect to the origin "fp->offset"
 * in the file. If fp->offset=0, then disk_ftell return the same as
 * ftell.
 *=================================================================*/
int disk_ftell(tfp)
TITFILE *tfp;
{
TF_DISK *fp=(TF_DISK*)tfp;
    
#ifdef USE_CACHE  
    int    pos;
    pos=(fp->nblock-1)*fp->bufsize+(int)(fp->pt-fp->buffer);
    return((pos>=0)? pos: 0);
#else
    return(ftell(fp->tfile(fp)));
#endif
}


/*==================================================================
                              tfile
 *=================================================================*/
FILE* disk_tfile(fp)
TITFILE *fp;
{
#ifdef USE_CACHE
    return(NULL);
#else
    return((FILE*)fp->fd);
#endif
}


/*==================================================================
                              fopen
 *=================================================================*/
TITFILE *disk_fopen(name)
char *name;
{
int      offset1,offset2;
FILE     *fp;
TF_DISK  *tfp;
struct stat  fstat;

/*
 * check if name is a titan disk or cdrom
 * This also set static names 'data, pointer and index'
 */
    if (!isTitanDisk(name))
    {
        fprintf(Fp_log,
           "disk_fopen: error: opening %s as a titan disk or cdrom\n",name);
        return(NULL);
    }

/*
 * if cdrom, pointer file should not exist. We determine 'offset'
 * by reading data file size
 */
    if (strlen(pointer)==0)
    {
        stat(data,&fstat);
        offset1=fstat.st_size;
        stat(indexname,&fstat);
        offset2=fstat.st_size;
        testByteSwap();
    }

/*
 * else read 'offset1' pointing to the end of the data file.
 * and offset2 pointing to the end of index file
 */
    else
    {
        fp=fopen(pointer,"r");
        fread(&offset1,sizeof(int),1,fp);
        if (testByteSwap())
            swap_4byte(&offset1);
        fread(&offset2,sizeof(int),1,fp);
        fclose(fp);
        if (testByteSwap())
            swap_4byte(&offset2);
    }
    tfp=(TF_DISK*)malloc(sizeof(TF_DISK));
    tfp->eof=offset1;
    fprintf(Fp_log,"  disk_fopen: titan disk end of data: %d\n", offset1);
    tfp->name=(char*)malloc(strlen(name)+1);
    strcpy(tfp->name,name);
    tfp->byteswap=testByteSwap();

/*
 * read the list of indexes
 */
    fprintf(Fp_log,"  disk_fopen: titan disk end of index: %d\n", offset2);
    tfp->nindex=offset2/INDEX_SIZE;
    tfp->index=(TITINDEX*)malloc(sizeof(TITINDEX)*tfp->nindex);
    fprintf(Fp_log,"  disk_fopen: reading %d index(es)...",tfp->nindex);
    tfp->nindex=
      disk_getIndex((TITFILE*)tfp,tfp->nindex,tfp->index,offset2,indexname);
    fprintf(Fp_log,"done, %d indexes effectively read\n",tfp->nindex);


#ifdef USE_CACHE
    tfp->fd=copen(data,CACHE_SIZE,1);
    tfp->bufsize=CACHE_SIZE;
    tfp->nblock=0;
#else
    tfp->fd=(int)fopen(data,"r");
    if (tfp->fd==0) {
        free(tfp);
        return(NULL);
    }
#endif
    tfp->type = TDISK;
    tfp->tread   = disk_fread;
    tfp->ttell   = disk_ftell;
    tfp->tseek   = disk_fseek;
    tfp->trewind = disk_rewind;
    tfp->teof    = disk_feof;
    tfp->tfile   = disk_tfile;

    return((TITFILE*)tfp);
}


/*==================================================================
                         isTitanDisk
  NOUVEAU (Feb 2000): un directory quelconque ou un cdrom contenant
  un nombre quelconque de fichiers parmi lesquels 3 fichiers de type:
       name.ndx
       name.siz our size.siz
       name     
  sera traite comme un titandisk.
  Avant, la condition pour qu'un repertoire soit traite comme un 
  titandisk etait qu'il devait contenir 5 fichiers et seulement 5:
       .
       ..
       xxx.ndx
       yyy.siz
       zzz
  ou zzz est le fichier titan.
  L'une des raisons de ce changement est que le logiciel Agecodagis
  TSPLIT cree des fichiers de type name.ndx, name.siz et name.
  Les cdrom sont alors equivalents a des titandisk.
  A la suite d'une proposition faite l'an passe, on rapellera que
  l'on peut maintenant partitionner un titandisk en n partitions
  de tailles quelconques (superieure a au moins 60 MB quand meme!)
  ce qui permet d'utiliser tout l'espace disque, qui est actuellement
  de 4 GB, et de faire des partitions d'environ 650 MB qui peuvent
  etre directement recopiee su cdrom.

 * Cherche si un directory correspond a un disque titan
 * ou a un cdrom titan.
 * Un disque titan contient 3 fichiers:
 *     data:    data
 *     index:   data.ndx
 *     pointer: size.siz
 * Un cdrom peut contenir 2 fichiers:
 *     data:    prefix
 *     index:   prefix.ndx
 * ou 3 fichiers:
 *     data:    prefix
 *     index:   prefix.ndx
 *     pointer: prefix.siz
 *=================================================================*/
int isTitanDisk(name)
char *name;
{
int    datalu;
DIR    *mdir;
struct dirent *fl;
char   prefix[20];
char   sizename[20];
char   *token[10];
int    ntok;

    if (!isDir(name))
        return 0;

    datalu       = 0;
    data[0]      = '\0';
    pointer[0]   = '\0';
    indexname[0] = '\0';

    if((mdir=opendir(name)) == NULL)
        return 0;

/*
 * First pass: look for index name and set prefix
 */
    while((fl = readdir(mdir)))
    {
        if (strstr(fl->d_name,".ndx"))
        {
            sprintf(indexname,"%s/%s",name,fl->d_name);
            ntok = sparse(fl->d_name, token, ".", 2);
            sprintf(prefix, "%s", token[0]);
            sprintf(sizename, "%s.siz", prefix);
            break;
        }
    }
    closedir(mdir);
    if (!strlen(indexname))
        return 0;

/*
 * Second pass: look for dataname and xxx.siz
 */
    mdir=opendir(name);
    while((fl = readdir(mdir)))
    {
/* Find data filename */
        if (!strcmp(fl->d_name,prefix))
        {
            sprintf(data,"%s/%s",name,prefix);
        }
/* Find datasize filename */
        else if (!strcmp(fl->d_name,"size.siz"))
        {
            sprintf(pointer,"%s/%s",name,fl->d_name);
        }
        else if (!strcmp(fl->d_name,sizename))
        {
            sprintf(pointer,"%s/%s",name,sizename);
        }
        else
            continue;
    }
    closedir(mdir);

    if (strlen(data) && strlen(indexname) && strlen(pointer))
         return(1);
    else
         return(0);
}

/*==================================================================
                        disk_getIndex
               See index description in titanio.h
 *=================================================================*/
int disk_getIndex(tfp, nindex, index, offset, indexfile)
TITFILE *tfp;
int nindex;
TITINDEX *index;
int offset;
char* indexfile;
{
char       date[32];
int        ix,lus,pos;
FILE       *fp,*fp2;
TITINDEX   *ipt;
char       str[INDEX_SIZE+10];
time_t     cdate;

    time(&cdate);

/* open index file */
    fp=fopen(indexfile,"r");
    fp2=fopen("index.asc","w");

    ipt=index;
    ix=0;
    while ( (ix<nindex) && ((pos=ftell(fp))<=offset) )
    {
        lus = fread(str,1,INDEX_SIZE,fp);
        if (lus==0)
            break;
        if (lus==-1)
        {
            fclose(fp);
            return(0);
        }
        if (lus<INDEX_SIZE)
        {
            fclose(fp);
            return(ix);
        }
        ipt->index_type = str[5];
        ipt->time       = bytes2int4(str[0],str[1],str[2],str[3]);
        ipt->beg_ofs    = bytes2int4(str[12],str[13],str[14],str[15]);

/* on elimine les dates > date courante */
        if (ipt->time>cdate)
                break;
        time_asc4(date, (double) ipt->time);
        fprintf(fp2,"%s %d %d\n", date, ipt->index_type, ipt->beg_ofs);
        if (ipt->time<cdate)
        {
            ipt++;
            ix++;
        }
    }
    fclose(fp);
    fclose(fp2);
    /* unlink("index.asc"); */
    return(ix);
}

