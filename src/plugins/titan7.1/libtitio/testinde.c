#include <stdio.h>
#include "titanio.h"

FILE *Fp_log=stderr;
int    verb=1;
#define NINDEX 100000
main()
{
static    char    name[]="/export/jaz";
    int    nindex,i;
    TITFILE    *fp;
    TITINDEX index[NINDEX];

    fp=topenGuess(name);


    nindex=tGetIndex(fp,NINDEX,index);
    if (nindex) {
        for (i=0;i<nindex;i++)
                fprintf(stderr,"index %d block %d date %s",i,
                index[i].beginAddr,ctime(&index[i].time));
    }
    tclose(fp);
}
