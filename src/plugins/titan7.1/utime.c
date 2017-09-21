/*====================================================================
    utime.c

 *===================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/types.h>

typedef struct
{   
    int    year;
    int    month;
    int    day;
    int    hour;
    int    min;
    float  sec;
} Tstruct;

#define leap_year(i) ((i % 4 == 0 && i % 100 != 0) || i % 400 == 0)

#ifdef ANSI_C
void str2utime1(char*, double*);
char *check_tstruct(Tstruct);
#else
void str2utime1();
char *check_tstruct();
#endif

int sparse();

/*===================================================================*/
void main(argc, argv)
int     argc;
char    *argv[];
{
int     i;
char    ttime[40];
time_t  tt;
char    str[256];
int     tzone   = 0;
struct tm *tms;

    if (argc < 2)
    {
printf("Time conversion\n");
printf("Usage:\n");
printf("    utime  860000000           -> Unix to date-time conversion\n");
printf("    utime  1998.04.15-07:48:00 -> date-time to Unix conversion\n");

/* Print current time */
        time(&tt);
        strftime(ttime,40,"%Y.%m.%d-%H:%M:%S %Z", gmtime(&tt));
        printf("    TODAY: %s %ld\n", ttime, tt);
        tms = gmtime(&tt);
        exit(1);
    }

if (1)
{
int year;

    for (year=1970; year<2030; year++)
    {
        if (!(year & 3) != leap_year(year))
        {
            printf("  %d  leap %d %d ", year, !(year & 3), leap_year(year));
            printf(" algo disagree\n");
        }
    }
}

/* Get options */

    for (i=2; i<argc; i++)
    {
        if (!strncmp(argv[i], "tz=",    3))
            tzone = atoi(&argv[i][3]);
        else
        {
            fprintf(stderr, "\n unsupported option '%s'\n", argv[i]);
            exit(1);
        }
    }

    if (argc < 2) return;

/*==== Argument is a time string yyyy.mm.dd-hh.mm.ss ====*/
/*==== Incomplete time string accepted ====*/

    if (strchr(argv[1], ':') ||
        strchr(argv[1], '-') ||
        strchr(argv[1], '.'))
    {
      char   *token[10];
      int     ntok;
      double  dbtime;
      static struct tm *tm;
      int sec;
      double day;
      char   dayasc[40];

        strncpy(ttime, argv[1], strlen(argv[1]));
        ntok = sparse(ttime, token, ":-.", 6);
        if (ntok <= 2)
            goto utime;

        sprintf(ttime, "1970.01.01-00.00.00.000");
        strncpy(ttime, argv[1], strlen(argv[1]));
        sprintf(str, "%s", ttime);
        if (0) printf("==== %s", ttime);
        if (0) printf("==== %s", str);
        ntok = sparse(ttime, token, ":-.", 6);

        str2utime1(str, &dbtime);
        if (dbtime == 0.0)
            exit(1);
        tt = (time_t) dbtime;
        tm = gmtime(&tt);
        sec = tm->tm_hour*3600 + tm->tm_min*60 + tm->tm_sec;
        day = (double) (tm->tm_yday+1) + (double) sec / 86400.0;
        sprintf(dayasc, "%.8f", day);
        ntok = sparse(dayasc, token, ".", 2);

        if ((int) ((dbtime - (double) tt) * 10000) != 0)
            printf("%.4f  day=%d.%.3s  \(", dbtime, (int)day, token[1]);
        else
            printf("%d  day=%d.%.3s  (", (int) dbtime, (int)day, token[1]);
        printf("%s)\n", str);

        return;
    } 


/*==== Argument is Unix time ====*/

    else
utime:
    {
      char   *token[10];
      int     ntok;
      static struct tm *tm;
      int    sec;
      double dbtime;
      double day;
      float  fract_sec;
      char   dayasc[40];


        dbtime = atof(argv[1]);
        tt = (time_t) atol(argv[1]);
        tt = (time_t) dbtime;
        fract_sec = (dbtime - (double) tt) * 10000;
        strftime(ttime,40,"%Y.%m.%d-%H:%M:%S", gmtime(&tt));
        sprintf(&ttime[strlen(ttime)], ".%04d", (int) rint(fract_sec));
        tm = gmtime(&tt);
        sec = tm->tm_hour*3600 + tm->tm_min*60 + tm->tm_sec;
        day = (double) (tm->tm_yday+1) + (double) sec / 86400.0;
        sprintf(dayasc, "%.8f", day);
        ntok = sparse(dayasc, token, ".", 2);
        printf("%s day=%d.%.3s (%.4f)\n", ttime, (int)day, token[1], dbtime);
        return;
    } 
}

/*===================================================================*/
/* Convert time string into double UNIX time                         */
/* Input format: yyyy.mm.dd-hh.mm.ss.sss                             */
/* Example       1995.02.12-11:34:45.123                             */
void str2utime1(str, dbltime)
char      *str;
double    *dbltime;
{
static int daysInMonth[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
char temp[5];
Tstruct ts;
int     sec;
int     msec;
char    *pmesg;
int     year;
int     month;
int     day;
double  dbsec;


    if (strlen(str) < 23)
    {
        printf("str2utime1: time string too short: '%s'\n", str);
        *dbltime = 0.;
        return;
    }
    sprintf(temp, "%4.4s", &str[ 0]); ts.year  = atoi(temp);
    sprintf(temp, "%2.2s", &str[ 5]); ts.month = atoi(temp);
    sprintf(temp, "%2.2s", &str[ 8]); ts.day   = atoi(temp);
    sprintf(temp, "%2.2s", &str[11]); ts.hour  = atoi(temp);
    sprintf(temp, "%2.2s", &str[14]); ts.min   = atoi(temp);
    sprintf(temp, "%2.2s", &str[17]); sec   = atoi(temp);
    sprintf(temp, "%3.3s", &str[20]); msec  = atoi(temp);
    ts.sec = (float) sec + (float) msec / 1000.0;
/*
    printf("==== str2utime1: %4d %02d %02d %02d %02d %.4f\n",
        ts.year, ts.month, ts.day, ts.hour, ts.min, ts.sec);
*/
    if ((pmesg = check_tstruct(ts)) != NULL)
    {
        printf("%s\n", pmesg);
        return;
    }

    ts.month -= 1;
    dbsec = 0.0;
    for (year=1970; year<ts.year; year++)
    {
        if (((year & 3) == 0) && (leap_year(year) == 0))
        {
            fprintf(stderr,"str2utime1: leap year algo disagree\n");
        }
        if ((leap_year(year) == 0) && ((year & 3) == 0))
        {
            fprintf(stderr,"str2utime1: leap year algo disagree\n");
        }
        
        if (leap_year(year)) daysInMonth[1] = 29;
        else                 daysInMonth[1] = 28;
        for (month=0; month<12; month++)
        {
            dbsec += (double) daysInMonth[month] * 86400.0;
        }
    }

    if (leap_year(year)) daysInMonth[1] = 29;
    else                 daysInMonth[1] = 28;

    for (month=0; month<ts.month; month++)
    {
        dbsec += (double) daysInMonth[month] * 86400.0;
    }

    for (day=1; day<ts.day; day++)
    {
        dbsec += 86400.0;
    }

    dbsec += (double) ts.hour * 3600.0;
    dbsec += (double) ts.min  *   60.0;
    dbsec += (double) ts.sec;

    *dbltime = dbsec;
    return;
}


/*===================================================================*/
char *check_tstruct(ts)
Tstruct ts;
{
static char mesg[255];

    if (ts.year  <= 0 ||
        ts.month <  1 ||
        ts.day   <  1 ||
        ts.hour  <  0 ||
        ts.min   <  0 ||
        ts.sec   <  0.0)
    {
        sprintf(mesg,"check_tstruct: got zero or negative number\n");
        return mesg;
    }

    if (ts.year < 1950)
    {
        sprintf(mesg,"check_tstruct: illegal year %d\n", ts.year);
        return mesg;
    }
    if (ts.hour > 23)
    {
        sprintf(mesg,"check_tstruct: illegal hour %d\n", ts.hour);
        return mesg;
    }
    if (ts.min > 59)
    {
        sprintf(mesg,"check_tstruct: illegal minute %d\n", ts.min);
        return mesg;
    }
    if (ts.sec > 59.99999)
    {
        sprintf(mesg,"check_tstruct: illegal second %.4f\n", ts.sec);
        return mesg;
    }

    switch (ts.month)
    {
      case  1: case  3: case  5: case  7: case  8: case 10: case 12:
        if (ts.day > 31)
        {
            sprintf(mesg,"check_tstruct: illegal day %d for month %d\n",
                 ts.day, ts.month);
            return mesg;
        }
        break;

      case  4: case  6: case  9: case  11:
        if (ts.day > 30)
        {
            sprintf(mesg,"check_tstruct: illegal day %d for month %d\n",
                 ts.day, ts.month);
            return mesg;
        }
        break;

      case  2:
        if (leap_year(ts.year))
        {
          if (ts.day > 29)
          {
           sprintf(mesg,
           "check_tstruct: year %d is leap: illegal day: %d for month %d\n",
            ts.year, ts.day, ts.month);
           return mesg;
          }
        }
        else if (ts.day > 28)
        {
         sprintf(mesg,
           "check_tstruct: year %d not leap: illegal day %d for month %d\n",
            ts.year, ts.day, ts.month);
         return mesg;
        }
        break;
      default:
        sprintf(mesg,"check_tstruct: illegal month %d\n", ts.month);
        return mesg;
    }
    return NULL;
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

    if (max_tokens < 1)
    {
        fprintf(stderr,"parse: illegal 'max_tokens'\n");
        return -1;
    }

    i = 0;
    if ((argv[i] = strtok(input, delimiters)) == NULL) return 0;
    for (i = 1; i < max_tokens; i++)
    {
        if ((argv[i] = strtok(NULL, delimiters)) == NULL) return i;
    }

    return i;
}

