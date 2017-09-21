#include <stdio.h>
#include <unistd.h>
#include "libst.h"
#include "titanio.h"
#include "diskio.h"
#include "datio.h"

FILE *Fp_log=stderr;

main(int argc, char **argv)
{
    char    resp[10];
    char    buffer[512];
    int    ier,fd,zero=0;
    FILE    *fp;    

    if (argc<2) {
        fprintf(stderr,"usage: %s device\n",argv[0]);
        fprintf(stderr," %s /export/titan (for titan disk)\n",argv[0]);
        fprintf(stderr," %s /dev/st0      (for titan dat)\n",argv[0]);
        exit(0);
    }

    /* 
     * titan DAT case 
     */
    if (isCharDev(argv[1])) {
        fprintf(stdout,"formatting titan DAT %s, confirm (y/n)?",argv[1]);
        fscanf(stdin,"%s",resp);
        if (resp[0]!='y')
            exit(0);
        fprintf(stdout,"do you REALLY want to erase the tape (y/n)?");
        fscanf(stdin,"%s",resp);
        if (resp[0]!='y')
            exit(0);

        fd=open(argv[1],O_RDWR);
        ier=stFormat(fd,2,1);
        if (!ier) {
            fprintf(stderr,"fmttitan: error: maybe you didn't run <setdrv> ?\n");
            fprintf(stderr,"  <setdrv> is necessary to use dual DAT partitions\n");
            exit(-1);
        }
        close(fd);

    /* 
     * titan disk case 
     */
    } else if (isDir(argv[1])) {
        fprintf(stdout,"formatting titan DISK %s, confirm (y/n)?");
        fscanf(stdin,"%s",resp);
        if (resp[0]!='y')
            exit(0);
        fprintf(stdout,"do you REALLY want to erase the disk (y/n)?");
        fscanf(stdin,"%s",resp);
        if (resp[0]!='y')
            exit(0);

        chdir(argv[1]);
        fp=fopen(POINTER,"w");
        memset(buffer,zero,512);
        fwrite(buffer,1,512,fp);
        fclose(fp);
    }
}
