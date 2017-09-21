/*
 * This program allows the tape driver to partition the tape
 * It must be run by root, the best time to run it is at boot time
 * e.g. in /etc/rc.d/rc.local
 */
#include <stdio.h>
#include "titanio.h"
FILE    *Fp_log=stderr;
main(int argc, char** argv)
{
    int    fd;
    if (argc<2) {
        fprintf(stderr,"usage: %s device_name\n",argv[0]);
        fprintf(stderr,"       (SOUS ROOT!)\n");
        exit(-1);
    }
    fd=open(argv[1],O_RDONLY);
    if (fd==-1) {
        fprintf(stderr,"usage: %s device_name\n",argv[0]);
        exit(-1);
    }
    stSetDriver(fd);
    close(fd);
}
