/*
 * O. Coutant. 1997, Observatoire de Grenoble, UJF Grenoble
 *
 * Manipulation d'une cassette DAT formattee en 2 partitions
 * Les appels ioctl sont decrits par un fichier README.st dans
 * l'arborescence Linux: </usr/src/>linux/drivers/scsi
 * Code compilable a condition que les ioctl permette la manipulation
 * de cassette partionable
 *
 * Petite interface pour un lecteur bandes/DAT/...
 *
 * stSetDriver():     initialise le driver en mode 'partition'
 *                    appelable uniquement sous root
 * stSetPartition():  selectionne la partition courante
 * stFormat():        formatte une cassette en 1 ou 2 partition
 * stSetPos():        positionnement sur la bande
 * stGetPos():        recupere la position
 *
 */
#include "titanio.h"
#include "proto.h"

extern FILE   *Fp_log;


#ifdef ST_HAS_PARTITION
/*
 * stSetDriver: autorise la manipulation de 2 partitions sur le driver
 * entree:
 *    fd= file descriptor
 * retour:
 *    1 en cas de succes, 0 sinon
 */
int stSetDriver(fd)
int fd;
{
    int    ier;
    struct mtop mt_cmd;
    struct passwd *pswd;

    /* check for root id */
    pswd=getpwuid(geteuid());
    if (strcmp(pswd->pw_name,"root")!=0) {
        fprintf(Fp_log,"stSetDriver: error: must be root\n");
        return(0);
    }
    
    /* autorise le multi-partition */
    mt_cmd.mt_op=MTSETDRVBUFFER;
    mt_cmd.mt_count=MT_ST_BOOLEANS | MT_ST_CAN_PARTITIONS;
    ier=ioctl(fd,MTIOCTOP,&mt_cmd);
    if (ier==-1) 
        return(0);
    else
        return(1);
}
    
/*
 * stSetPartition: positionne sur la partition 1 ou 2
 * entree: 
 *    fd= file descriptor du device
 *    num= partition 1 ou 2
 * sortie:
 * retour: 1 en cas de succes, 0 sinon
 */
int stSetPartition(fd, num)
int fd;
int num;
{
    int    ier;
    struct mtop mt_cmd;

    num=(num>=1)? 1: 0;


    /* positionnement sur la partition demandee */
    mt_cmd.mt_op=MTSETPART;
    mt_cmd.mt_count=num;
    ier=ioctl(fd,MTIOCTOP,&mt_cmd);
    if (ier==-1)
        return(0);
    return(1);
}
/*
 * stFormat: formatte une cassette avec 1 ou deux partitions
 *
 * entree:
 *    fd= file descriptor du device
 *    num= nombre de partitions (1 ou 2)
 *    size= (facultatif) taille de la premiere partition en Moctet
 * sortie:
 * retour: 1 en cas de succes, 0 sinon
 */
int stFormat(fd, num, size)
int fd;
int num;
int size;
{
    int    ier;
    struct mtop mt_cmd;

    num=(num>=2)? size: 0;

    /* autorise le multi-partition */
/*    mt_cmd.mt_op=MTSETDRVBUFFER;
    mt_cmd.mt_count=MT_ST_BOOLEANS | MT_ST_CAN_PARTITIONS;
    ier=ioctl(fd,MTIOCTOP,&mt_cmd);
    if (ier==-1) 
        return(0);*/

    /* formatte la cassette */
    mt_cmd.mt_op=MTMKPART;
    mt_cmd.mt_count=num;
    ier=ioctl(fd,MTIOCTOP,&mt_cmd);
    if (ier==-1) {
    perror("stFormat: ");
        return(0);
    }
    return(1);
}
#endif /* ST_HAS_PARTITION */
/*
 * stSetPos: positionnement sur la bande
 * entree:
 *    fd= file descriptor du device
 *    pos= MTFSF,MTFSR,MTBSF,MTBSR,MTFSFM,MTBSFM,MTREW,MTEOM (voir mtio.h)
 *    count= (facult.) nombre d'operations
 * sortie:
 * retour: 1 en cas de succes, 0 sinon
 */
int stSetPos(fd, pos, count)
int fd;
int pos;
int count;
{
    int    ier;
    struct mtop mtcmd;

    switch (pos) {
    case MTFSF:    
    case MTFSR:    
    case MTBSF:    
    case MTBSR:    
#ifdef MTFSFM
    case MTFSFM:    
    case MTBSFM:    
#endif
        mtcmd.mt_op=pos;
        mtcmd.mt_count=count;
        break;
    case MTREW:
    case MTEOM:
        mtcmd.mt_op=pos;
        break;
    default:
        return(0);
    }
    ier=ioctl(fd,MTIOCTOP,&mtcmd);
    if (ier==-1) 
        return(0);
    return(1);
}
/*
 * retourne la position en unite block
 */
int stGetPos(fd)
int fd;
{
    int    ier;
#ifdef MTIOCPOS
    struct    mtpos    mt_pos;

    ier=ioctl(fd,MTIOCPOS,&mt_pos);
    if (ier==-1)
        return(-1);
    else
        return(mt_pos.mt_blkno);
#else
        struct  mtget   mt_get;

        ier=ioctl(fd,MTIOCGET,&mt_get);
        if (ier==0) 
              return(mt_get.mt_blkno);
        fprintf(Fp_log,"stGetPos: could not get tape position\n");
        return(100);

#endif
}

