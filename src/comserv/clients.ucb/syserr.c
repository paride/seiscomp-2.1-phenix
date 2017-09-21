#include <stdio.h>
#include <errno.h>

#if !defined(__GNU_LIBRARY__) && !defined(__GLIBC__)
extern int errno, sys_nerr;
extern char *sys_errlist[];
#endif

int syserr(char *msg)
{
  fprintf(stdout,"ERROR: %s ( errno: %d",msg,errno);
  if (errno > 0 && errno < sys_nerr)
    fprintf(stdout,"; Description: %s)\n",sys_errlist[errno]);
  else
    fprintf(stdout,")\n");  
  fflush(stdout);
  return(1);
}

void fatalsyserr(char *msg)
{
  fprintf(stdout,"FATAL ERROR: %s ( errno: %d",msg,errno);
  if (errno > 0 && errno < sys_nerr)
    fprintf(stdout,"; Description: %s)\n",sys_errlist[errno]);
  else
    fprintf(stdout,")\n");  
  fflush(stdout);
  exit(1);
}

