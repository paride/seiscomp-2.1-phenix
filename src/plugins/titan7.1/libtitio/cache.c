/*
 * O. Coutant, 1997, Observatoire de Grenoble, UJF
 */

#ifdef USE_CACHE

#include "titanio.h"
#include "proto.h"

/* 
 * Bibliotheque de lecture cache:
 *
 * principe:  la lecture est bufferisee avec une taille 'buffersize'
 *      determinee a l'ouverture du fichier. Pendant que l'on 
 *      utilise les donnees du buffer courant, un sous-process
 *      lit les donnees du buffer suivant 
 *      Si une partie du temps de lecture est perdu sur le bus scsi, on gagne
 *      un peu de temps cpu en calculant dans le meme temps.
 *
 * contenu public:
 *    copen()
 *    cread()
 *    cclose()
 *    cseekBlock()
 *    ceof()
 */

/* Prototypes, private modules */

static void   readprocess(Cache *);
static void   cclose_exit(int,int);
static void   cclose_intr(int);

static int    lus;
static char   shmproj=26;
static char   semproj=27;
static int    *cacheList=NULL;
static int    ncache=0;
static int    creadier=1;
static int    badeof=0;


extern FILE   *Fp_log;



/*====================================================================*/
static void sigalrm_hand(int dum)
{
    creadier=0;
    badeof=1;
    signal(SIGALRM,sigalrm_hand);
}


/*====================================================================*/
int copen(char *name,int bufsize, int closeDelay)
{
int     fdin,shmid,semid,pid,i,un=1,zero=0;
Cache   *cache;
FILE    *fp;
static struct    sembuf waitop={2,0,0};

/* test sur la taille demandee */
    if (bufsize>MAXCSIZE)
    {
        fprintf(Fp_log,"copen: error: buffer size is larger than static shared memory size\n");
        fprintf(Fp_log,"              please change MAXCSIZE in cache.h and recompile\n");
        exit(-1);
    }
    /* initialisation du cache liste */
    if (cacheList==NULL)
    {
        cacheList=(int*)malloc(sizeof(int)*MAXCACHE);
        memset(cacheList,0,sizeof(int)*MAXCACHE);
    }

    /* test nombre de cache */
    if (ncache+1==MAXCACHE)
    {
        fprintf(stderr,"ERROR: copen: cannot open more than %d cached io\n",MAXCACHE);
        return(0);
    } else
        ncache++;

    /* ouverture du fichier dat */
    fdin=open(name,O_RDONLY|O_NONBLOCK);
    if (fdin==-1)
        return(-1);

    /* creation d'un segment de memoire partagee */
    shmid=shmget(ftok(name,shmproj),sizeof(Cache),IPC_CREAT|SHM_W|SHM_R);
    if (shmid==-1)
    {
        fprintf(Fp_log,"copen: error: could not allocate shared memory\n");
        perror("");
        close(fdin);
        return(-1);
    }

    /* creation de 3 semaphores */
    semid=semget(ftok(name,semproj),3,IPC_CREAT|SEM_A|SEM_R);
    if (semid==-1)
    {
        fprintf(Fp_log,"dat_open: error: could not allocate semaphores \n");
        perror("");
        close(fdin);
        return(-1);
    }

    /* attachement du segment */
    cache=(Cache*)shmat(shmid,NULL,0);
    if (cache==(Cache*)-1)
    {
        fprintf(Fp_log,"dat_open: error: could not attach shared memory\n");
        close(fdin);
        return(-1);
    }
    for (i=0;i<MAXCACHE;i++)
        if (cacheList[i]==0)
            break;
    cacheList[i]=(int)cache;

    /* initialisation de la structure cache */
    cache->bufsize=bufsize;
    cache->active_buffer=CACHE_READING;
    cache->stopflag=0;
    cache->fd=fdin;
    cache->shmid=shmid;
    cache->semid=semid;
    cache->ibuf=0;
    cache->cblock=0;
    cache->dblock=0;
    cache->closeDelay=closeDelay;

    /* initilisation des semaphores: 
     * semaphore 0: device libre si a 0, occupe si a 1 
     * semaphore 1: lecture du buffer suivant demandee si 1, attente si 0
     * semaphore 2: buffer disponible si 0, buffer en lecture si 1
     */
    semctl(semid,0,SETVAL,un);
    semctl(semid,1,SETVAL,zero);
    semctl(semid,2,SETVAL,un);

    /* creation du sous process de lecture */
    pid=fork();
    if (pid==-1)
    {
        fprintf(Fp_log,"dat_open: error: could not fork\n");
        shmdt((char*)cache);
        close(fdin);
        return(-1);
    }
    
    /* 
     * I) sous-process 
     *    Lecture du cache
     */
    if (pid==0)
    {
        readprocess(cache);
        exit(0);
    }

    /* 
     * II) process principal 
     */
    /* En cas de sortie, nettoyage */
#ifndef AIX
    on_exit(cclose_exit,cache);
#else
    atexit(cclose_exit);
#endif
    /* En cas de ^C (INTR), nettoyage */
    signal(SIGINT,cclose_intr);
    signal(SIGHUP,cclose_intr);
    signal(SIGQUIT,cclose_intr);
    signal(SIGTERM,cclose_intr);

    /* attente de fin d'initialisation de readprocess 
     * il faut qu'un buffer soit charge
     */
    semop(CACHE(cache)->semid,&waitop,1);
    
    cache->rpid=pid;
    fp=fopen("cleancache","w");
    fprintf(fp,"#!/bin/sh\nkill -9 %d\nipcrm shm %d \nipcrm sem %d \n",pid,shmid,semid);
    fclose(fp);

    return((int)cache);
}

/*====================================================================*
 * cread: remplace la procedure int read(fd,buffer,size)
 * entree:     cache: pointeur sur le cache (passe comme entier)
 * sortie:     buf: pointeur du buffer d'entree
 * retour:     nombre de caractere lus ou 0 si EOF ou -1 si erreur
 *====================================================================*/
int cread(int cache, char **buf)
{
int    lus;
static struct  sembuf waitop={2,0,0};
static struct  sembuf    freeop[2]=    {{1,1,0},{2,1,0}};

    /* On attend si on est en train de lire qu'un buffer
     * soit disponible (sem 2 a 0)
     */
    semop(CACHE(cache)->semid,&waitop,1);

    /* on regarde d'eventuels codes d'erreur */
    if (CACHE(cache)->active_buffer==CACHE_EOF)
    {
        fprintf(stderr,"cread: active_buffer:%d (CACHE_EOF:%d)\n",
            CACHE(cache)->active_buffer,
            CACHE_EOF);
        return(0);
    }
    if (CACHE(cache)->active_buffer==CACHE_ERROR)
        return(-1);

    /* 'recopie' du buffer courant */
    *buf=CACHE(cache)->active_buffer;
    lus=(int)(CACHE(cache)->active_buffer_end-CACHE(cache)->active_buffer);

    /* lecture du buffer suivant */
    semop(CACHE(cache)->semid,freeop,2);

    return(lus);
}


/*====================================================================*/
void cclose(int cache)
{
int    i;
struct shmid_ds *sds;
static struct    sembuf    waitandset[3]={{0,-1,0},{2,0,0},{1,1,0}};

    CACHE(cache)->stopflag=1;
    /*
     * attente de liberation du DAT et envoie d'une demande
     * de lecture
     */
    semop(CACHE(cache)->semid,waitandset,3);
    sleep(CACHE(cache)->closeDelay);
    close(CACHE(cache)->fd);
    shmctl(CACHE(cache)->shmid,IPC_RMID,sds);
    semctl(CACHE(cache)->semid,0,IPC_RMID,0);
    shmdt((char*)cache);
    for (i=0;i<MAXCACHE;i++)
        if (cacheList[i]==cache)
        {
            cacheList[i]=0;
            break;
        }
    ncache--;
}


/*====================================================================*
 * idem cclose except arg passed by address
 *====================================================================*/
static void cclose_exit(int status,int cache)
{
int    i;
struct shmid_ds *sds;
static struct  sembuf  waitandset[3]={{0,-1,0},{2,0,0},{1,1,0}};

    for (i=0;i<MAXCACHE;i++)
        if (cacheList[i]==cache)
        {
            cacheList[i]=0;
            break;
        }
    if (i==MAXCACHE)
        return;

    ncache--;
    CACHE(cache)->stopflag=1;
    /*
     * attente de liberation du DAT et envoie d'une demande
     * de lecture
     */
    semop(CACHE(cache)->semid,waitandset,3);
    sleep(CACHE(cache)->closeDelay);
    close(CACHE(cache)->fd);
    shmctl(CACHE(cache)->shmid,IPC_RMID,sds);
    semctl(CACHE(cache)->semid,0,IPC_RMID,0);
    shmdt((char*)cache);
}


/*====================================================================*
 * idem cclose except arg passed by address
 *====================================================================*/
static void cclose_intr(int intr)
{
int    i,cache;
struct shmid_ds *sds;
static struct  sembuf  waitandset[3]={{0,-1,0},{2,0,0},{1,1,0}};

    for (i=0;i<MAXCACHE;i++)
    {
        if (cacheList[i]==0) 
            continue;
        cache=cacheList[i];
        CACHE(cache)->stopflag=1;
    /*
     * attente de liberation du DAT et envoie d'une demande
     * de lecture
     */
        semop(CACHE(cache)->semid,waitandset,3);
        sleep(CACHE(cache)->closeDelay);
        close(CACHE(cache)->fd);
        shmctl(CACHE(cache)->shmid,IPC_RMID,sds);
        semctl(CACHE(cache)->semid,0,IPC_RMID,0);
        shmdt((char*)cache);
    }
}


/*====================================================================*
 * Ce process lit sur le dat et met les donnees
 * dans un buffer situe en memoire partagee.
 * La lecture d'un buffer est declenchee par 
 * semaphore
 * La sortie de la procedure est declenchee par
 * la mise a 1 du drapeau cache->stopflag
 *====================================================================*/
static void readprocess(Cache *cache)
{
char    *buffin;
int    i;
static struct  sembuf    freeop={2,-1,0};
static struct  sembuf    waitop={1,-1,0};
static struct  sembuf    waitDat={0,-1,0};
static struct  sembuf    freeDat={0,1,0};

    cache->ibuf=-1;
    signal(SIGALRM,sigalrm_hand);
    while(cache->stopflag==0)
    {
        cache->ibuf=(cache->ibuf+1)%3;
        buffin=&(cache->buffer[cache->ibuf][0]);
        if (cache->cblock!=cache->dblock)
        {
            cache->cblock++;
            cache->active_buffer_end=buffin+cache->bufsize;
        }
        else
        {
            semop(CACHE(cache)->semid,&waitDat,1);
read_again0:        lus=0;
read_again1:        alarm(30);
            lus+=read(cache->fd,buffin+lus,cache->bufsize);
            alarm(0);
/* il y a eu interruption Alarm en cas de fin de bande */
            if ((lus==-1) && (creadier==1))
                if ((errno==EAGAIN)||(errno==EINTR))
                    goto read_again0;
                else
                    buffin=CACHE_ERROR;
            else if ((lus==0) || (creadier==0))
                buffin=CACHE_EOF;
            else if (lus<cache->bufsize)
                goto read_again1;
            else {
                cache->dblock++;
                cache->cblock++;
                cache->active_buffer_end=buffin+lus;
            }
            semop(CACHE(cache)->semid,&freeDat,1);
        }

        /* On a fini l'operation de lecture,
         * buffer disponible sem2 -> -1
         */
        cache->active_buffer=buffin;
        semop(cache->semid,&freeop,1);

        /*
         * On attend une demande (sem1 -> -1) 
         */
        semop(cache->semid,&waitop,1);

    }
    semop(cache->semid,&freeop,1);
    semctl(cache->semid,0,IPC_RMID,0);
    shmdt((char*)cache);
    for (i=0;i<MAXCACHE;i++)
        if (cacheList[i]==(int)cache)
        {
           cacheList[i]=0;
           break;
        }
    ncache--;
}

/*====================================================================*
 * positionnement du pointeur 'bande' au debut du bloc
 * where + offset.
 * offset est donne en unite de block (pas en octets!)
 * La taille de bloc est celle declaree a l'ouverture du cache
 * (cache->bufsize)
 *
 * rappel: cache->dblock est le prochain bloc a lire sur bande
 *         cache->cblock est le prochain bloc a lire sur le cache
 *====================================================================*/
int cseekBlock(int cache,int where, int offset)
{
int     i,j,ier;
struct  mtop mtop;
static  struct  sembuf    waitBuf={2,0,0};
static  struct  sembuf    waitDat={0,-1,0};
static  struct  sembuf    freeDat={0,1,0};
static  struct  sembuf    setaskBuf[2]={{1,1,0},{2,1,0}};

    /* pour attendre que dblock soit a jour d'une eventuelle lecture 
     * sur le peripherique
     */
    semop(CACHE(cache)->semid,&waitBuf,1);

    /* offset par rapport a la position sur bande */
    offset+=where-CACHE(cache)->dblock;


    /* cas du bloc precedant le bloc courant */    
    if ((offset==-3)&&(CACHE(cache)->dblock==CACHE(cache)->cblock)) 
        CACHE(cache)->cblock-=3;
    else
    {
      /* on attend que le dat soit libre */
        semop(CACHE(cache)->semid,&waitDat,1);
        mtop.mt_op=(offset>0)? MTFSR: MTBSR;
        j=(offset>0)? 1: -1;
        mtop.mt_count=j*offset;
        ier=ioctl(CACHE(cache)->fd,MTIOCTOP,&mtop);
        if (ier==-1)
        {
            semop(CACHE(cache)->semid,&freeDat,1);
            perror("cseekBlock: ");
            return(-1);
        }
        CACHE(cache)->dblock+=offset;
        CACHE(cache)->cblock+=offset;
        /* on libere le dat */
        semop(CACHE(cache)->semid,&freeDat,1);
    }
    semop(CACHE(cache)->semid,setaskBuf,2);
    return(1);
}


/*====================================================================*/
void crewind(int cache)
{
struct mtop mtop;
static struct  sembuf    waitandset[3]={{0,-1,0},{2,0,0},{2,1,0}};
static struct  sembuf    freeop[2]={{0,1,0},{1,1,0}};

    /* 
     * on attend que le dat soit libre et qu'un eventuelle
     * lecture de readprocess soit finie
     */
/*
fprintf(stderr,"crewind [0]:%d [2]:%d\n",
    semctl(CACHE(cache)->semid,0,GETVAL,0),
    semctl(CACHE(cache)->semid,2,GETVAL,0));
*/
    semop(CACHE(cache)->semid,waitandset,3);

    mtop.mt_op=MTREW;
    mtop.mt_count=1;
    ioctl(CACHE(cache)->fd,MTIOCTOP,&mtop);
    CACHE(cache)->dblock=0;
    CACHE(cache)->cblock=0;

    /* on libere le dat et on demande une mise a jour du buffer */
    semop(CACHE(cache)->semid,freeop,2);
}

/*====================================================================*/
int ceof(int cache)
{
    return( (CACHE(cache)->active_buffer==CACHE_EOF) || (badeof==1));
}

/*====================================================================*/
int cfileno(int cache)
{
    return(CACHE(cache)->fd);
}

#endif
