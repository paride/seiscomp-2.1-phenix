/*
 * O. Coutant, 1997, Observatoire de Grenoble, UJF
 */
#include "titanio.h" 
#include "proto.h" 

/*
 * contents:
 *    dat_fread
 *    dat_fseek
 *    dat_feof
 *    dat_rewind
 *    dat_ftell
 *
 * dat_fread, dat_feof, dat_fseek, dat_ftell, take the same arguments than
 * un'dat_' versions,  except for FILE* pointer replaced by TITFILE*
 */

static int readier;
static int badeof=0;

#ifndef USE_CACHE
/* chance is, if we got trapped in this 
 * alarm interrupt that we have reached end of tape.
 * Unfortunately, we have no way to check it,
 * So it's better to trust 'badeof' flag
 */
static void sigalrm_hand(dum)
int dum;
{
    readier=0;
    badeof=1;
    signal(SIGALRM,sigalrm_hand);
}


/*==================================================================
                              fread
 *=================================================================*/
int dat_fread(buf, elmsize, nelm, tfp)
char *buf;
int elmsize;
int nelm;
TITFILE *tfp;
{
int    alire,lus;
TF_DAT *fp;

    alire=elmsize*nelm;
    lus=0;
    fp=(TF_DAT*)tfp;

#ifndef USE_CACHE
    signal(SIGALRM,sigalrm_hand);
#endif

    while ((lus!=alire)&&(!dat_feof(tfp)))
    {

/* read from buffer */
        for (;(fp->pt<fp->bufend)&&(lus<alire);fp->pt++,lus++)
            buf[lus]=*(fp->pt);

/* fill buffer */
        if (lus!=alire)
        {
#ifdef USE_CACHE
            readier=cread(fp->fd,&fp->buffer);
#else
            alarm(30);
            readier=read(fp->fd,fp->buffer,fp->bufsize);
            alarm(0);
#endif
/* fin de fichier, on reste positionne au bloc courant */
            if (readier==0)
            {
                badeof=1;
                return(lus/elmsize);
            }
            if (readier==-1)
            {
                fprintf(stderr,
                    "dat_fread: error reading data, errno=%d\n",errno);
                return(0);
            }
            fp->nblock++;
            fp->pt=fp->buffer;
            fp->bufend=fp->buffer+readier;
        }
    }
    return(lus/elmsize);
}



/*==================================================================
                              feof
 *=================================================================*/
int dat_feof(tfp)
TITFILE *tfp;
{
#ifdef USE_CACHE
    TF_DAT    *fp;
    fp=(TF_DAT*)tfp;
    return(ceof(fp->fd));
#else
#ifdef linux
    TF_DAT    *fp;
struct    mtget    mget;
    fp=(TF_DAT*)tfp;
    ioctl(fp->fd,MTIOCGET,&mget);
    return ((GMT_EOF(mget.mt_gstat) && (fp->pt==fp->bufend)) || (badeof==1));
#else
    TF_DAT    *fp;
    fp=(TF_DAT*)tfp;
    return ((fp->pt==fp->bufend) && (badeof==1));

#endif
#endif
}


/*==================================================================
                              fseek
 *=================================================================*/
int dat_fseek(tfp, n, where)
TITFILE *tfp;
int n;
int where;
{
struct mtop    smtop;
int     nback,nforward,nbufback,nbufforward,ier;    
TF_DAT  *fp;

    fp=(TF_DAT*)tfp;
    switch (where)
    {

      case SEEK_CUR:
/* backward motion */
        if (n<0)
        {
            n=-n;
            nback=fp->pt-fp->buffer; 
            if (n<=nback)
            {
                fp->pt-= n;
            }
            else
            {
                n-=nback;
                nbufback=n/fp->bufsize+2;
                nbufback=n/fp->bufsize+1; /* ModifOC*/
                n-=(nbufback-2)*fp->bufsize;

#ifdef USE_CACHE
                ier=cseekBlock(fp->fd,fp->nblock,-nbufback);
                if (ier==-1) 
                    return(-1);
#else
                smtop.mt_op=MTBSR;
                smtop.mt_count=nbufback;
                ier=ioctl(fp->fd,MTIOCTOP,&smtop);
                if (ier==-1)
                    return(-1);
#endif

                fp->nblock-=nbufback;

#ifdef USE_CACHE
                readier=cread(fp->fd,&fp->buffer);
#else
                alarm(30);
                readier=read(fp->fd,fp->buffer,fp->bufsize);
                alarm(0);
#endif

                if (readier!=fp->bufsize)
                    return(-1);
                fp->nblock++;
                fp->pt=fp->buffer+readier-n;
                fp->bufend=fp->buffer+readier;
            }
            badeof=0;
        }

/* forward motion */
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
            
#ifdef USE_CACHE
               ier=cseekBlock(fp->fd,fp->nblock,nbufforward);
               if (ier==-1)
                   return(-1);
#else
               smtop.mt_op=MTFSR;
               smtop.mt_count=nbufforward;
               ier=ioctl(fp->fd,MTIOCTOP,&smtop);
               if (ier==-1)
                   return(-1);
#endif
                fp->nblock+=nbufforward;

#ifdef USE_CACHE
                readier=cread(fp->fd,&fp->buffer);
#else
                alarm(30);
                readier=read(fp->fd,fp->buffer,fp->bufsize);
                alarm(0);
#endif
                if (readier<=0)
                    return(-1);
                fp->pt=fp->buffer+n;
                fp->bufend=fp->buffer+readier;
            }
        }
        return(0);
        break;
    

    case SEEK_SET:
        if (n<0)
        {
            return(-1);
        }
        else
        {
/*
            dat_rewind(fp);
            return(dat_fseek(fp,n,SEEK_CUR));
*/
            return(dat_fseek(tfp,n-dat_ftell(tfp),SEEK_CUR));
        }
        break;


    case SEEK_END:
        fprintf(stderr,
            "fseek(..,SEEK_END,..) not implemented for dat drive\n");
        return(-1);


    default:
        return(-1);
    }
}

/*==================================================================
                              rewind
 *=================================================================*/
void dat_rewind(tfp)
TITFILE *tfp;
{
TF_DAT    *fp;

#ifdef USE_CACHE
    fp=(TF_DAT*)tfp;

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

struct  mtop smtop;
    fp=(TF_DAT*)tfp;

    smtop.mt_op=MTREW;
    smtop.mt_count=1;
    readier=ioctl(fp->fd,MTIOCTOP,&smtop);
    fp->pt=fp->bufend=fp->buffer;
    fp->nblock=0;
    alarm(30);
    readier=read(fp->fd,fp->buffer,fp->bufsize);
    alarm(0);
    if (readier==-1)
        return;
    fp->nblock=1;
    fp->bufend+=readier;
#endif

    badeof=0;
}


/*==================================================================
                              ftell
 *=================================================================*/
int dat_ftell(tfp)
TITFILE *tfp;
{
    int    pos;
    TF_DAT    *fp;
    fp=(TF_DAT*)tfp;
    pos=(fp->nblock-1)*fp->bufsize+(int)(fp->pt-fp->buffer);
/*
    fprintf(stderr,"dat_ftell: nblock=%d bufsize=%d ioffset= %d pos=%d\n",
                fp->nblock,fp->bufsize,(int)(fp->pt-fp->buffer),pos);
*/
    return((pos>=0)? pos: 0);
}


/*==================================================================
                              fopen
 *=================================================================*/
TITFILE *dat_fopen(name)
char *name;
{
    extern FILE *Fp_log;
    TF_DAT      *tfp;
    int    fd;

    if (!isCharDev(name))
    {
        fprintf(Fp_log,
            "dat_fopen: error: opening %s as a block device\n",name);
        return(NULL);
    }
#ifdef USE_CACHE
    fd=copen(name,DAT_BLOCKSIZE,5);
#else
    fd=open(name,O_RDONLY|O_NONBLOCK);
#endif
    if (fd==-1) {
        perror("dat_fopen: ");
        return(NULL);
    }

    tfp=(TF_DAT*)malloc(sizeof(TF_DAT));
    tfp->fd=fd; 
    tfp->bufsize=32768;
#ifdef USE_CACHE
    tfp->bufend=tfp->pt=tfp->buffer=NULL;
#else
    tfp->bufend=tfp->pt=tfp->buffer=(char*)malloc(tfp->bufsize);
#endif
    tfp->nblock=0;
    tfp->type=TDAT;
#ifdef ST_HAS_PARTITION
    tSetPart((TITFILE*)tfp,0);
#endif
    tfp->byteswap=testByteSwap();
    tfp->tread=dat_fread;
    tfp->ttell=dat_ftell;
    tfp->tseek=dat_fseek;
    tfp->trewind=dat_rewind;
    tfp->teof=dat_feof;
    tfp->tfile=dat_tfile;
    tfp->name=(char*)malloc(strlen(name)+1);
    strcpy(tfp->name,name);

    /* 
     * lecture des index de declenchement
     */
#ifdef ST_HAS_PARTITION
    tfp->nindex=65536;/* partition 1 is 1Mo with 16bytes indexes */
    tfp->index=(TITINDEX*)malloc(sizeof(TITINDEX)*tfp->nindex);
    fprintf(Fp_log,"topen: reading at most %d index(es)...",tfp->nindex);
    tfp->nindex=dat_getIndex((TITFILE*)tfp,tfp->nindex,tfp->index);
    fprintf(Fp_log,"done, %d indexes effectively read\n",tfp->nindex);
#else
    tfp->nindex=0;
    tfp->index=NULL;
#endif

    return((TITFILE*)tfp);
}


/*==================================================================
                              tfile
 *=================================================================*/
FILE *dat_tfile(fp)
TITFILE *fp;
{
    return(NULL);
}


#ifdef ST_HAS_PARTITION

/*==================================================================
                       dat_getIndex
 * Decode la liste des index de declenchements d'une
 * cassette DAT.
 * entree:
 *      fp= TITAN file pointer
 *      nindex= nombre max d'index pouvant etre lus
 * sortie:
 *      index= pointeur pointer sur un tableau d'index
 * retour:
 *      nombre d'index lus en cas de succes, 0 sinon
 *=================================================================*/
int dat_getIndex(fp, nindex, index)
TITFILE *fp;
int nindex;
TITINDEX *index;
{
char       date[32];
int        fd,lus,ix,ier;
TITINDEX   *ipt;
FILE       *fp2;
time_t     cdate;
char       str[DATINDEX_SIZE+10];

#ifdef USE_CACHE
        fd=cfileno(fp->fd);
#else
        fd=fp->fd;
#endif

    time(&cdate);
    ier=stSetPartition(fd,1);
    if (!ier)
    {
        fprintf(stderr,
            "Error: Cannot set partition 1, maybe setdrv was not run?\n");
        return(0);
    }

    ier=stSetPos(fd,MTREW,0);
    fp2=fopen("index.asc","w");


    ipt=index;
    ix=0;
    while (ix<nindex)
    {
        lus = read(fd,str,DATINDEX_SIZE);
        if (lus==-1)
        {
            stSetPartition(fd,0);
            stSetPos(fd,MTREW,0);
            return(ix);
        }
        if (lus<DATINDEX_SIZE)
        {
            stSetPartition(fd,0);
            stSetPos(fd,MTREW,0);
            return(ix);
        }
        ipt->time       = bytes2int4(str[0],str[1],str[2],str[3]);
        ipt->index_type = str[5];
        ipt->beg_ofs    = bytes2int4(str[12],str[13],str[14],str[15]);

        ipt->beg_ofs /= DAT_BLOCKSIZE;
        time_asc4(date, (double) ipt->time);
        fprintf(fp2,"%s %d %d\n", date, ipt->index_type, ipt->beg_ofs);
        if (ipt->time<=cdate)
        {
            ipt++;
            ix++;
        }
    }
    stSetPartition(fd,0);
    stSetPos(fd,MTREW,0);
    fclose(fp2);
    /* unlink("index.asc"); */
    return(ix);
}
#endif

#endif   /* end ifdef ST_HAS_PARTITION */

