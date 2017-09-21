/*
 * O. Coutant, 1997, Observatoire de Grenoble, UJF
 */
#include "titanio.h" 
#include "proto.h" 

/*
 * content:
 *    file_fread
 *    file_fseek
 *    file_feof
 *    file_rewind
 *    file_ftell
 * file_fread, file_feof, file_fseek, file_ftell, take the same arguments
 * than * un'file_' versions,  except for FILE* pointer replaced by TITFILE*
 */

int file_fread(buf, elmsize, nelm, fp)
char *buf;
int elmsize;
int nelm;
TITFILE *fp;
{
    return(fread(buf,elmsize,nelm,tFILE(fp)));
}


int file_feof(fp)
TITFILE *fp;
{
    return(feof(fp->tfile(fp)));
}


int file_fseek(fp, n, where)
TITFILE *fp;
int n;
int where;
{
    return(fseek(fp->tfile(fp),n,where));
}


/*
 * rewind set position before the first synchronized
 * frame, or, at the beginning of the file if syncOffset is null
 */
void file_rewind(fp)
TITFILE *fp;
{
    rewind(fp->tfile(fp));
}


int file_ftell(fp)
TITFILE *fp;
{
    return(ftell(fp->tfile(fp)));
}


FILE* file_tfile(fp)
TITFILE *fp;
{
    return((FILE*)fp->fd);
}


TITFILE* file_fopen(name)
char *name;
{
    extern FILE *Fp_log;
    FILE    *fp;
    TF_FILE *tfp;

    if (!isFile(name)) {
        fprintf(Fp_log,"titOpen: error: openning %s as a regular titan file\n",name);
        return(NULL);
    }
    fp=fopen(name,"r");
    if (fp==NULL) 
        return(NULL);
    tfp=(TF_FILE*)malloc(sizeof(TF_FILE));
    tfp->fd=(int)fp; 
    tfp->name=(char*)malloc(strlen(name)+1);
    strcpy(tfp->name,name);
    tfp->type=TFILE;

    tfp->tread=file_fread;
    tfp->ttell=file_ftell;
    tfp->tseek=file_fseek;
    tfp->trewind=file_rewind;
    tfp->teof=file_feof;
    tfp->tfile=file_tfile;

    return((TITFILE*)tfp);
}
