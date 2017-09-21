/*====================================================================
    mklst.c


 *===================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/types.h>


FILE *Fp_err;

/*===================================================================*/
main(argc, argv)
int     argc;
char    *argv[];
{
int     i, j, n, k;
char    ttime[40];
char    tzstr[16];
time_t  tt;
double  dbltime, secs, prev, diff;
char    str[256];
char    date[12], tim[14];
int     day, month, first;
int     tzone   = 0;
struct tm *tms;
int period;
int duration;
int ntimes;
int ntraces = 1;

    Fp_err = stderr;
    if (argc < 5)
    {
help:

printf("Make test event time list\n");
printf("Usage:\n");
printf("    mklst  start_time period (sec) duration (sec) ntimes\n");
printf("    mklst  1998.04.15-07.48.00 3600 200 100\n");

/* Get current time */
time(&tt);
strftime(ttime,40,"%Y.%m.%d-%H.%M.%S", gmtime(&tt));
printf("\nToday: %s %ld\n", ttime, tt);
tms = gmtime(&tt);

    exit(1);
    }

/* Get options */

    for (i=5; i<argc; i++)
    {
        {
            fprintf(stderr, "\n unsupported option '%s'\n", argv[i]);
            exit(1);
        }
    }


/*==== Argument is a time string yyyy.mm.dd-hh.mm.ss ====*/

    if (strchr(argv[1], ':') ||
        strchr(argv[1], '-') ||
        strchr(argv[1], '.'))
    {
      char   *token[10];
      int     ntok;
      static struct tm tm;
      int year;

        ntok = sparse(argv[1], token, ":-.", 6);

        sscanf(token[0], "%d", &year);       tm.tm_year = year - 1900;
        sscanf(token[1], "%d", &tm.tm_mon);  tm.tm_mon  -= 1;
        sscanf(token[2], "%d", &tm.tm_mday);
        sscanf(token[3], "%d", &tm.tm_hour);
        sscanf(token[4], "%d", &tm.tm_min);
        sscanf(token[5], "%d", &tm.tm_sec);
        if (tm.tm_mon < 0 || tm.tm_mon > 11 ||
            tm.tm_mday < 1 || tm.tm_mday > 31 ||
            tm.tm_hour < 0 || tm.tm_hour > 23 ||
            tm.tm_min  < 0 || tm.tm_min  > 59 ||
            tm.tm_sec  < 0 || tm.tm_sec  > 59)
        {
            printf("\n  Wrong input time\n\n");
            goto help;
        }
/*
printf("\n    %s %s %s %s %s %s\n",
    token[0],
    token[1],
    token[2],
    token[3],
    token[4],
    token[5]);
printf("\n    %d %d %d %d %d %d\n",
tm.tm_year,
tm.tm_mon,
tm.tm_mday,
tm.tm_hour,
tm.tm_min,
tm.tm_sec);

tt  = mktime(&tm);

strftime(ttime,40,"%Y.%m.%d-%H.%M.%S", gmtime(&tt));
printf("%s\n", ttime);
exit(0);
*/
        tt  = mktime(&tm);

        period   = atoi(argv[2]);
        duration = atoi(argv[3]);
        ntimes   = atoi(argv[4]);
        for (i=0; i<ntimes; i++)
        {
            strftime(ttime,40,"%Y.%m.%d-%H.%M.%S", gmtime(&tt));
            printf("%s %d %d\n", ttime, duration, ntraces);
            tt += period;
        }
        return;
    } 
}

/*===================================================================*
 *  Parse the given string.
 *===================================================================*/
int sparse(input, argv, delimiters, max_tokens)
char *input;
char *argv[];
char *delimiters;
int  max_tokens;
{
extern char *strtok();
int i = 0;

    if (max_tokens < 1) {
        fprintf(stderr,"parse: illegal 'max_tokens'\n");
        return -1;
    }

    i = 0;
    if ((argv[i] = strtok(input, delimiters)) == NULL) return 0;
    for (i = 1; i < max_tokens; i++) {
        if ((argv[i] = strtok(NULL, delimiters)) == NULL) return i;
    }

    return i;
}

