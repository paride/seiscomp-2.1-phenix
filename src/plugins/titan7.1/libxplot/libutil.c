/*====================================================================
         libutil.c
 *===================================================================*/
#include "xplot.h"
#include "proto.h"

typedef struct
{
    int    year;
    int    month;
    int    day;
    int    hour;
    int    imin;
    float  sec;
} Tstruct;

#ifdef ANSI_C
static char *check_tstruct(Tstruct, char*);
static char *mem_alloc(int, char*);
#else
static char *check_tstruct();
static char *mem_alloc();
#endif



/*===================================================================*/
int getyear(dbtime)
double dbtime;
{
char date[40];
long lt = (long) dbtime;

    strftime(date,40,"%Y", gmtime((time_t*)&lt));
    return (atoi(date));
}


/*===================================================================*/
double dbtime2dbday(dbtime, year)
double dbtime;
int    year;
{
char date[40];
double dbsecs;

    sprintf(date, "%04d.01.01-00:00:00.000", year);
    get_dbltime4(date, &dbsecs);
    return (double) ((dbtime - dbsecs) / (double) DAY + (double) 1.0);
}


/*===================================================================*/
double dbday2dbtime(day, year)
double day;
int year;
{
char date[40];
double dbsecs;
int year2;

    if (year < 2000) year2 = year-1900;
    else             year2 = year-2000;
    sprintf(date, "%02d.01.01-00:00:00.000", year2);
    get_dbltime2(date, &dbsecs);
    return (double) (((double) day - (double) 1.0) * (double) DAY + dbsecs);
}

/*===================================================================*/
static char *check_tstruct(ts, from)
Tstruct ts;
char *from;
{
static char mesg[255];

    if (ts.year  <= 0 ||
        ts.month <  1 ||
        ts.day   <  1 ||
        ts.hour  <  0 ||
        ts.imin  <  0 ||
        ts.sec   <  0.0)
    {
        sprintf(mesg,"check_tstruct: got zero or negative number");
        goto end;
    }

    if (ts.year < 1950)
    {
        sprintf(mesg,"check_tstruct: illegal year %d", ts.year);
        goto end;
    }
    if (ts.hour > 23)
    {
        sprintf(mesg,"check_tstruct: illegal hour %d", ts.hour);
        goto end;
    }
    if (ts.imin > 59)
    {
        sprintf(mesg,"check_tstruct: illegal minute %d", ts.imin);
        goto end;
    }
    if (ts.sec > 59.99999)
    {
        sprintf(mesg,"check_tstruct: illegal second %.4f", ts.sec);
        goto end;
    }

    switch (ts.month)
    {
      case  1: case  3: case  5: case  7: case  8: case 10: case 12:
        if (ts.day > 31)
        {
            sprintf(mesg,"check_tstruct: illegal day %d for month %d",
                 ts.day, ts.month);
            goto end;
        }
        break;

      case  4: case  6: case  9: case  11:
        if (ts.day > 30)
        {
            sprintf(mesg,"check_tstruct: illegal day %d for month %d",
                 ts.day, ts.month);
            goto end;
        }
        break;

      case  2:
        if (leap_year(ts.year))
        {
          if (ts.day > 29)
          {
           sprintf(mesg,
           "check_tstruct: year %d is leap: illegal day: %d for month %d",
            ts.year, ts.day, ts.month);
           goto end;
          }
        }
        else if (ts.day > 28)
        {
         sprintf(mesg,
           "check_tstruct: year %d not leap: illegal day %d for month %d",
            ts.year, ts.day, ts.month);
         goto end;
        }
        break;
      default:
        sprintf(mesg,"check_tstruct: illegal month %d", ts.month);
        goto end;
    }
    return NULL;

end:
    if (from) sprintf(&mesg[strlen(mesg)], " (%s)", from);
    sprintf(&mesg[strlen(mesg)], "\n");
    return mesg;
}


/*===================================================================*/
/* Convert time string into double UNIX time                         */
/* Input format: yyyy.mm.dd-hh.mm.ss.sss                             */
/* Example       1995.02.12-11:34:45.123                             */
int str2utime1(str, dbltime)
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
        return 1;
    }

    sprintf(temp, "%4.4s", &str[ 0]); ts.year  = atoi(temp);
    sprintf(temp, "%2.2s", &str[ 5]); ts.month = atoi(temp);
    sprintf(temp, "%2.2s", &str[ 8]); ts.day   = atoi(temp);
    sprintf(temp, "%2.2s", &str[11]); ts.hour  = atoi(temp);
    sprintf(temp, "%2.2s", &str[14]); ts.imin  = atoi(temp);
    sprintf(temp, "%2.2s", &str[17]); sec   = atoi(temp);
    sprintf(temp, "%3.3s", &str[20]); msec  = atoi(temp);
    ts.sec = (float) sec + (float) msec / 1000.0;
/*
    printf("==== str2utime1: %4d %02d %02d %02d %02d %.4f\n",
        ts.year, ts.month, ts.day, ts.hour, ts.imin, ts.sec);
*/
    if ((pmesg = check_tstruct(ts, "str2utime1")) != NULL)
    {
        printf("%s\n", pmesg);
        return 1;
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
    dbsec += (double) ts.imin *   60.0;
    dbsec += (double) ts.sec;

    *dbltime = dbsec;
    return 0;
}


/*===================================================================*/
/* Convert SISMALP format time string into double UNIX time          */
/* Input format: dd.mm.yyyy hh.mm.ss.sss                             */
/* Example       02.04.1995 18:20:27.950                             */
int str2utime2(date, time, dbltime)
char   *date;
char   *time;
double *dbltime;
{
char    str[40];

    if (strlen(date) < 10)
    {
        printf("str2utime2: date string too short: '%s'\n", date);
        *dbltime = 0.;
        return 1;
    }

    if (strlen(time) < 12)
    {
        printf("str2utime2: time string too short: '%s'\n", time);
        *dbltime = 0.;
        return 1;
    }

    sprintf(str, "%4.4s.%2.2s.%2.2s-%2.2s.%2.2s.%2.2s.%3.3s",
        &date[ 6],
        &date[ 3],
        &date[ 0],
        &time[ 0],
        &time[ 3],
        &time[ 6],
        &time[ 9]);

    return (str2utime1(str, dbltime));
}


/*===================================================================*/
/* Convert time string into double UNIX time                         */
/* Input format: yy.mm.dd-hh.mm.ss.sss                               */
/* Example       95.02.12-11:34:45.123                               */
int str2utime3(str, dbltime)
char      *str;
double    *dbltime;
{
char temp[5];
int  year;
char str2[40];

    if (strlen(str) < 21)
    {
        printf("str2utime3: time string too short: '%s'\n", str);
        *dbltime = 0.;
        return 1;
    }

    sprintf(temp, "%2.2s", &str[0]); year  = atoi(temp);
    if (year > 50) year += 1900;
    else           year += 2000;

    sprintf(str2, "%4d.%2.2s.%2.2s-%2.2s.%2.2s.%2.2s.%3.3s",
        year,
        &str[ 3],
        &str[ 6],
        &str[ 9],
        &str[12],
        &str[15],
        &str[18]);

    return (str2utime1(str2, dbltime));
}


/*===================================================================*/
/* Convert time string into double UNIX time                         */
/* Input format: dd.mm.yy-hh.mm.ss.sss                               */
/* Example       12.02.95-11:34:45.123                               */
int str2utime4(str, dbltime)
char      *str;
double    *dbltime;
{
char temp[5];
int  year;
char str2[40];

    if (strlen(str) < 21)
    {
        printf("str2utime4: time string too short: '%s'\n", str);
        *dbltime = 0.;
        return 1;
    }

    sprintf(temp, "%2.2s", &str[6]); year  = atoi(temp);
    if (year > 50) year += 1900;
    else           year += 2000;

    sprintf(str2, "%4d.%2.2s.%2.2s-%2.2s.%2.2s.%2.2s.%3.3s",
        year,
        &str[ 3],
        &str[ 0],
        &str[ 9],
        &str[12],
        &str[15],
        &str[18]);

    return (str2utime1(str2, dbltime));
}


/*===================================================================*/
/* Convert SISMALP format time string into double UNIX time          */
/* Input format: dd.mm.yyyy hh.mm.ss.sss                             */
/* Example       02.04.1995 18:20:27.950                             */
void get_dbltime1(date, time, dbltime)
char *date, *time;
double *dbltime;
{
static int daysInMonth[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
static struct tm tim;
int year, month, mday;
int i;
double nsec;
char temp[5];
int sec, msec;
Tstruct ts;
char    *pmesg;


/* Check time string */

    sprintf(temp, "%4.4s", &date[ 6]); ts.year  = atoi(temp);
    sprintf(temp, "%2.2s", &date[ 3]); ts.month = atoi(temp);
    sprintf(temp, "%2.2s", &date[ 0]); ts.day   = atoi(temp);
    sprintf(temp, "%2.2s", &time[ 0]); ts.hour  = atoi(temp);
    sprintf(temp, "%2.2s", &time[ 3]); ts.imin  = atoi(temp);
    sprintf(temp, "%2.2s", &time[ 6]); sec   = atoi(temp);
    sprintf(temp, "%3.3s", &time[ 9]); msec  = atoi(temp);
    ts.sec = ((double) sec + (double) msec / (double) 1000.0);
/*
    printf("==== get_dbltime1: %4d %2d %2d %2d %2d %.3f\n",
        ts.year, ts.month, ts.day, ts.hour, ts.imin, ts.sec);
*/
    if ((pmesg = check_tstruct(ts, "get_dbltime1")) != NULL)
    {
        printf("%s\n", pmesg);
        return;
    }

    tim.tm_year = ts.year - 1900;
    tim.tm_mon  = ts.month - 1;
    tim.tm_mday = ts.day;
    tim.tm_hour = ts.hour;
    tim.tm_min  = ts.imin;
    tim.tm_sec  = sec;

    nsec = 0.0;
    for (year=1970; year<(tim.tm_year + 1900); year++)
    {
        if (((year & 3) == 0) && (leap_year(year) == 0))
        {
            fprintf(stderr,"get_dbltime1: leap year algo disagree\n");
        }
        if ((leap_year(year) == 0) && ((year & 3) == 0))
        {
            fprintf(stderr,"get_dbltime1: leap year algo disagree\n");
        }
        
        if (leap_year(year)) daysInMonth[1] = 29;
        else                 daysInMonth[1] = 28;
        for (i=0; i<12; i++)
        {
            nsec += (double) daysInMonth[i] * 86400.0;
        }
    }

    if (leap_year(year)) daysInMonth[1] = 29;
    else                 daysInMonth[1] = 28;

    for (month=0; month<tim.tm_mon; month++)
    {
        nsec += (double) daysInMonth[month] * 86400.0;
    }

    for (mday=1; mday<tim.tm_mday; mday++)
    {
        nsec += 86400.0;
    }

    nsec += (double) tim.tm_hour * 3600.0;
    nsec += (double) tim.tm_min  *   60.0;
    nsec += (double) tim.tm_sec;
    nsec += (double) msec / 1000.0;

    *dbltime = nsec;
    return;
}


/*===================================================================*/
/* Convert time string into double UNIX time                         */
/* Input format: yy.mm.dd-hh.mm.ss.sss                               */
/* Example       95.02.12-11:34:45.123                               */
void get_dbltime2(str, dbltime)
char      *str;
double    *dbltime;
{
char date[12], time[14];
char temp[5];
int sec, msec;
Tstruct ts;
char    *pmesg;
char year_asc[3];
int  year;

    if (strlen(str) < 21)
    {
        printf("get_dbltime2: time string too short: '%s'\n", str);
        *dbltime = 0.;
        return;
    }

/* Check time string */

    sprintf(year_asc, "%2.2s", &str[0]);
    year = atoi(year_asc);
    if (year < 70) year += 2000;
    else           year += 1900;
    ts.year  = year;
    sprintf(temp, "%2.2s", &str[ 3]); ts.month = atoi(temp);
    sprintf(temp, "%2.2s", &str[ 6]); ts.day   = atoi(temp);
    sprintf(temp, "%2.2s", &str[ 9]); ts.hour  = atoi(temp);
    sprintf(temp, "%2.2s", &str[12]); ts.imin  = atoi(temp);
    sprintf(temp, "%2.2s", &str[15]); sec   = atoi(temp);
    sprintf(temp, "%3.3s", &str[18]); msec  = atoi(temp);
    ts.sec = (double) sec + (double) msec / (double) 1000.0;
/*
    printf("==== get_dbltime2: %4d %2d %2d %2d %2d %.3f\n",
        ts.year, ts.month, ts.day, ts.hour, ts.imin, ts.sec);
*/
    if ((pmesg = check_tstruct(ts, "get_dbltime2")) != NULL)
    {
        printf("%s\n", pmesg);
        return;
    }

    sprintf(date, "%2.2s.%2.2s.%4d", &str[6], &str[3], year);
    sprintf(time, "%2.2s:%2.2s:%2.2s.%3.3s",
         &str[9], &str[12], &str[15], &str[18]);
/*
printf("==== get_dbltime2: %s %s\n", date, time);
*/
    get_dbltime1(date, time, dbltime);
    return;
}


/*===================================================================*/
/* Convert time string into double UNIX time                         */
/* Input format: dd.mm.yy-hh.mm.ss.sss                               */
/* Example       12.02.95-11:34:45.123                               */
void get_dbltime3(str, dbltime)
char      *str;
double    *dbltime;
{
char date[12], time[14];
char temp[5];
int sec, msec;
Tstruct ts;
char    *pmesg;

    if (strlen(str) < 21)
    {
        printf("get_dbltime3: time string too short: '%s'\n", str);
        *dbltime = 0.;
        return;
    }
/* Check time string */

    sprintf(temp, "%2.2s", &str[ 6]); ts.year  = atoi(temp) + 1900;
    sprintf(temp, "%2.2s", &str[ 3]); ts.month = atoi(temp);
    sprintf(temp, "%2.2s", &str[ 0]); ts.day   = atoi(temp);
    sprintf(temp, "%2.2s", &str[ 9]); ts.hour  = atoi(temp);
    sprintf(temp, "%2.2s", &str[12]); ts.imin  = atoi(temp);
    sprintf(temp, "%2.2s", &str[15]); sec   = atoi(temp);
    sprintf(temp, "%3.3s", &str[18]); msec  = atoi(temp);
    ts.sec = (float) sec + (float) msec / 1000.0;
/*
    printf("==== get_dbltime3: %4d %2d %2d %2d %2d %.3f\n",
        ts.year, ts.month, ts.day, ts.hour, ts.imin, ts.sec);
*/
    if ((pmesg = check_tstruct(ts, "get_dbltime3")) != NULL)
    {
        printf("%s\n", pmesg);
        return;
    }

    sprintf(date, "%2.2s.%2.2s.19%2.2s", &str[0], &str[3], &str[6]);
    sprintf(time, "%2.2s:%2.2s:%2.2s.%3.3s",
         &str[9], &str[12], &str[15], &str[18]);

    get_dbltime1(date, time, dbltime);
    return;
}

/*===================================================================*/
/* Convert time string into double UNIX time                         */
/* Input format: yyyy.mm.dd-hh.mm.ss.sss                             */
/* Example       1995.02.12-11:34:45.123                             */
void get_dbltime4(str, dbltime)
char      *str;
double    *dbltime;
{
char date[12], time[14];
char temp[5];
int sec, msec;
Tstruct ts;
char    *pmesg;


    if (strlen(str) < 23)
    {
        printf("get_dbltime4: time string too short: '%s'\n", str);
        *dbltime = 0.;
        return;
    }
/*
    printf("==== get_dbltime4: %s\n", str);
*/
    sprintf(temp, "%4.4s", &str[ 0]); ts.year  = atoi(temp);
    sprintf(temp, "%2.2s", &str[ 5]); ts.month = atoi(temp);
    sprintf(temp, "%2.2s", &str[ 8]); ts.day   = atoi(temp);
    sprintf(temp, "%2.2s", &str[11]); ts.hour  = atoi(temp);
    sprintf(temp, "%2.2s", &str[14]); ts.imin  = atoi(temp);
    sprintf(temp, "%2.2s", &str[17]); sec   = atoi(temp);
    sprintf(temp, "%3.3s", &str[20]); msec  = atoi(temp);
    ts.sec = (float) sec + (float) msec / 1000.0;
/*
    printf("==== get_dbltime4: %4d %2d %2d %2d %2d %.3f\n",
        ts.year, ts.month, ts.day, ts.hour, ts.imin, ts.sec);
*/
    if ((pmesg = check_tstruct(ts, "get_dbltime4")) != NULL)
    {
        printf("%s\n", pmesg);
        return;
    }
    sprintf(date, "%2.2s.%2.2s.%4.4s", &str[8], &str[5], &str[0]);
    sprintf(time, "%2.2s:%2.2s:%2.2s.%3.3s",
         &str[11], &str[14], &str[17], &str[20]);
/*
printf("==== get_dbltime4: %s %s\n", date, time);
*/
    get_dbltime1(date, time, dbltime);
    return;
}



/*===================================================================*/
/*  Get second number of calendar time from UNIX time                */
/*  Example: Unix time: 796869468.404  (03.04.1995 00:37:48.404)     */
/*  Seconds return value:  48.404                                    */
void get_secs(dbltime, secs)
double dbltime, *secs;
{
long hour, lmin;

    hour = (long)(dbltime / 3600);
    lmin = (long)((dbltime - hour * 3600) / 60);
    *secs = dbltime - hour * 3600 - lmin * 60;
    return;
}


/*===================================================================*/
/* Convert Unix double time into date and time strings (SISMALP format  */
/* Get date and time string from Unix double time                       */
/* Output format: DATE: dd.mm.yyyy  TIME: hh.mm.ss.mmm                  */
/* Example:             05.05.1995        13:20:35.000                  */
void time_asc0(date, time, dbltime)
char *date, *time;
double dbltime;
{
struct tm *tms;
long ltime = (long) dbltime;
double msec;
int year;

  tms = gmtime(&ltime);
  msec = dbltime - (double) ltime;
  year = tms->tm_year + 1900;
  sprintf(date, "%02d.%02d.%04d",
     tms->tm_mday,tms->tm_mon+1,year);
  sprintf(time, "%02d:%02d:%02d.%03d",
     tms->tm_hour,tms->tm_min,tms->tm_sec, (int)(1000.0*msec));
  return;
}

/*===================================================================*/
/* Convert Unix double time into date-time string    */
/* Output format: yy.mm.dd-hh.mm.ss.mmm              */
/* Example:       95.05.05-13:20:35.000              */
void time_asc1(str, dbltime)
char    *str;
double   dbltime;
{
static char date[20], time[20];

    time_asc0(date, time, dbltime);
    sprintf(str,"%2.2s.%2.2s.%2.2s-%s",&date[8],&date[3],&date[0],time);
    return;
}


/*===================================================================*/
/* Convert Unix double time into date-time string    */
/* Output format: dd.mm.yy-hh.mm.ss.mmm              */
/* Example:       05.11.94-13:20:35.000              */
void time_asc2(str, dbltime)
char    *str;
double   dbltime;
{
static char date[20], time[20];

    time_asc0(date, time, dbltime);
    sprintf(str,"%2.2s.%2.2s.%2.2s-%s",&date[0],&date[3],&date[8],time);
    return;
}


/*===================================================================*/
void time_asc3(str, dbltime)
char *str;
double dbltime;
{
struct tm *tms;
long ltime = (long) dbltime;

    tms = gmtime(&ltime);
    sprintf(str,"%02d%02d%02d %02d:%02d:%02d",
        tms->tm_year, tms->tm_mon+1, tms->tm_mday,
        tms->tm_hour, tms->tm_min, tms->tm_sec);
    return;
}


/*===================================================================*/
/* Convert Unix double time into date-time string    */
/* Output format: yyyy.mm.dd-hh.mm.ss.mmm              */
/* Example:       1995.05.05-13:20:35.000              */
void time_asc4(str, dbltime)
char    *str;
double   dbltime;
{
static char date[20], time[20];

    time_asc0(date, time, dbltime);
    sprintf(str,"%4.4s.%2.2s.%2.2s-%2.2s.%2.2s.%2.2s.%s",
        &date[6],&date[3],&date[0],
        &time[0],&time[3],&time[6],&time[9]);
    return;
}


/*===================================================================*/
long open_Frd(fn, fp)
char  *fn;
FILE **fp;
{
long size;
    if ((*fp = fopen(fn, "rb")) == NULL) {
        fprintf(stderr, "\nERROR: open_Frd: can't open %s\n", fn);
        exit(1);
    }
    fseek(*fp, 0L, SEEK_END);
    size = ftell(*fp);
    rewind(*fp);
    return size;
}

/*===================================================================*/
long open_Frw(fn, fp)
char *fn;
FILE **fp;
{
long size;

    if ((*fp = fopen(fn, "r+b")) == NULL) {
        fprintf(stderr, "\nERROR: open_Frw: can't open %s\n", fn);
        exit(1);
    }
    fseek(*fp, 0L, SEEK_END);
    size = ftell(*fp);
    rewind(*fp);
    return size;
}

/*===================================================================*/
long open_Frd_noex(fname, Fp)
char  *fname;
FILE **Fp;
{
long size;

    if (*Fp != NULL)
    {
        fprintf(stderr,"open_noex: file pointer not null for '%s'\n", fname);
    }
    if ((*Fp = fopen(fname, "rb")) == NULL)
    {
        return -1;
    }
    fseek(*Fp, 0L, SEEK_END);
    size = ftell(*Fp);
    rewind(*Fp);
    return size;
}


/*===================================================================*/
long open_Fapp(fname, Fp)
char  *fname;
FILE **Fp;
{
long size;

    if ((*Fp = fopen(fname, "r+b")) == NULL) {
        fprintf(stderr, "\nERROR: open_Fapp: can't open %s\n", fname);
        exit(1);
    }
    fseek(*Fp, 0L, SEEK_END);
    size = ftell(*Fp);
    rewind(*Fp);
    return size;
}



/*===================================================================*/
void open_Fwr(fn, fp)
char *fn;
FILE **fp;
{

    if ((*fp = fopen(fn, "w+b")) == NULL) {
        fprintf(stderr, "\nERROR: open_Fwr: can't open %s\n", fn);
	perror("");
        exit(1);
    }
    return;
}


/*===================================================================*/
long fcreat_append(name, Fp)
char *name;
FILE **Fp;
{
long len;

    if (open_Frd_noex(name, Fp) < 0)
        open_Fwr(name, Fp);
    fclose(*Fp); *Fp = NULL;
    len = open_Fapp(name, Fp);
    return (len);
}


/*===================================================================*/
int getline(fp, line)
FILE *fp;
char *line;
{
    while (1)
    {
        if (!fgets(line,255,fp)) return FALSE;
        if (line[0] == '#')   continue;
        if (strlen(line) < 3) continue;
        return TRUE;
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


/*===================================================================*
    Remove trailing carriage return and trailing blanks.
 *===================================================================*/
int trim(input)
char *input;
{
int n;

  n = strlen(input);
  if (input[n-1] == '\n') input[--n] = 0;
  if (input[n-1] == 0x0d) input[--n] = 0;
  --n;
  while (n >= 0 && input[n] == ' ') --n;
  n++;
  input[n] = '\0';
  return n;
}


/*===================================================================*/
void ucase(c)
char *c;
{
int i;
    for (i=0;i<strlen(c);i++) if (islower(c[i])) c[i] = toupper(c[i]);
    return;
}

/*===================================================================*/
void lcase(c)
char *c;
{
int i;

    for (i=0;i<strlen(c);i++) if (isupper(c[i])) c[i] = tolower(c[i]);
    return;
}



/*
static int sort_strings(a, b) char *a, *b; { return (strcmp(a,b)); }
*/
static int sort_strings(a, b) const void *a, *b; { return (strcmp(a,b)); }


/*==================================================================*
    sort_table() sort ascii table and overwrite input file.
    All commented lines are copied on the top of the file.
 *==================================================================*/
void sort_table(fname, linelen)
char *fname;
short linelen;
{
FILE  *Fp_tbl;
FILE  *Fp_tmp;
char  line[255], prev_line[255];
char  *table;
int   i, nlines;
char  tmp_fname[255];

/* Open table file */

    open_Frd(fname, &Fp_tbl);

/* Open temp file and save commented lines */

    sprintf(tmp_fname, "%s.%d", fname, getpid());
    open_Fwr(tmp_fname, &Fp_tmp);
    while(fgets(line, 255, Fp_tbl))
    {
        if (line[0] == '#')
            fprintf(Fp_tmp, "%s", line);
    }
    rewind(Fp_tbl);
    fclose(Fp_tmp);

/* Find number of valid entries */

    nlines = 0;
    while (getline(Fp_tbl, line))
    {
        ++nlines;
    }
    rewind(Fp_tbl);

    printf("  sort_table: file '%s' %ld char linelen=%d\n",
           fname, (long) ftell(Fp_tbl), linelen);

/* Allocate memory */

    table = (char*) mem_alloc((nlines*linelen), "sort_table");
    memset(table, 0, (nlines*linelen));

/* Store entries into table */

    nlines = 0;
    while (getline(Fp_tbl, line))
    {
        strcpy(&table[nlines*linelen], line);
        ++nlines;
    }
    fclose(Fp_tbl);

/* Sort lines */

    qsort(table, nlines, linelen, sort_strings);

/* Open table for overwritting */

    open_Fwr(fname, &Fp_tbl);

/* Open tmp file and copy commented lines into output file */

    open_Frd(tmp_fname, &Fp_tmp);
    if (1) while(fgets(line, 255, Fp_tmp))
    {
        fprintf(Fp_tbl, "%s", line);
    }
    fclose(Fp_tmp);
    unlink(tmp_fname);

/* Overwrite table to disk, discarding duplicates */

    strncpy(prev_line, &table[0], linelen);
    trim(prev_line);
if (0) printf("--%s--\n", prev_line);
    fprintf(Fp_tbl, "%s\n", prev_line);
    for (i=1; i<nlines; i++)
    {
        strncpy(line, &table[i*linelen], linelen);
        trim(line);
        if (strcmp(line, prev_line))
        {
if (0) printf("--%s--\n", line);
            fprintf(Fp_tbl, "%s\n", line);
        }
        strcpy(prev_line, line);
    }
    fclose(Fp_tbl);
    free(table);
}

/*=====================================================================
 *  mem()   Call memory allocator
 *          calloc is supposed to initialize the memory allocated
 *====================================================================*/
static char *mem_alloc(nbytes, caller)
int  nbytes;
char *caller;
{
static int alloc_bcnt;
char *p;

    if ((p = (char *) calloc(nbytes, 1L)) == NULL)
    {
        fprintf(stderr,"\n\tERROR: mem_alloc: calloc failed; ");
        fprintf(stderr,"(%s)\n", caller);
        fprintf(stderr,"\tThere was %d Bytes allocated\n", alloc_bcnt);
        exit(1);
    }
    alloc_bcnt += nbytes;
    return p;
}

