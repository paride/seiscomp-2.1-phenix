/*====================================================================
         libutil.c
 *===================================================================*/
#include "titan.h"
#include "proto.h"

#ifdef ANSI_C
static char *check_tstruct(Tstruct, char*);
#else
static char *check_tstruct();
#endif


Paths db_paths;
char  netname[PATHLEN];
char  seed_organization[100];
char  seed_label[100];
char  seed_scalefactor[100];


/*===================================================================* 
                    paths_init()

structure Paths:
char   db_paths.cvtit_conf
char   db_paths.stations[255];
char   db_paths.timedt[255];
char   db_paths.timedft[255];
char   db_paths.extra_tcorr[255];
char   db_paths.timedb[255];
char   db_paths.events[255];
char   db_paths.events_lst[255];
char   db_paths.events_tbl[255];
char   db_paths.meteosat_lst[255];

Examples:

PYREN_STATIONS /home/fels/sismnet/
PYREN_DT       /home/fels/sismnet/timedb/dt
PYREN_DFT      /home/fels/sismnet/timedb/dft
PYREN_TC       /home/fels/sismnet/timedb/dft

SISMALP_TIMEDB /home/fels/sismnet/timedb
SISMALP_EVNDIR /home/fels/sismnet/events
SISMALP_EVNLST /home/fels/sismnet/events/events.lst
SISMALP_EVNTBL /home/fels/sismnet/events/events.tbl
METEOSAT_LST   /home/fels/sismnet/msat.lst

 *===================================================================*/

void paths_init()
{
#define MAXTOKEN 4
#define pr(m) fprintf(stderr,"%s", m);
FILE   *Fp;
char    line[LINELEN];
char    line_save[LINELEN];
char   *token[MAXTOKEN];
int     ntok;
int     len;
int     foundNetname = 0;

/* This module should be call just once.  */

    db_paths.timedt[0]      = '\0';
    db_paths.timedft[0]     = '\0';
    db_paths.extra_tcorr[0] = '\0';
    db_paths.stations[0]    = '\0';

    if (!(db_paths.cvtit_conf = getenv("CVTIT_CONF")))
    {
        pr("\n\tpaths_init(): ENVIRONMENTAL VARIABLE ");
        pr("\"CVTIT_CONF\" NOT FOUND\n");
        pr("\tExample: setenv CVTIT_CONF ");
        pr("/home/fels/sismnet/cvtit.conf\n\n");
        exit(1);
    }
    if (!(Fp = fopen(db_paths.cvtit_conf,"r")))
    {
        pr("\n\tpaths_init(): CONFIG FILE ");
        pr(db_paths.cvtit_conf);
        pr(" NOT FOUND\n\n");
        exit(1);
    }

    ucase(netname);
    len = strlen(netname);
    while (getline(Fp, line))
    {
        trim(line);
        sprintf(line_save, "%s", line);
        ntok = sparse(line, token, " \t", MAXTOKEN);
        if (ntok < 2) continue;
        ucase(token[0]);

        if (!strncmp(token[0], netname, strlen(netname)))
        {
            foundNetname = 1;
            if (!strcmp(&token[0][len+1], "DT"))
            {
                sprintf(db_paths.timedt, "%s", token[1]);
            }
            if (!strcmp(&token[0][len+1], "DFT"))
            {
                sprintf(db_paths.timedft, "%s", token[1]);
            }
            if (!strcmp(&token[0][len+1], "TC"))
            {
                sprintf(db_paths.extra_tcorr, "%s", token[1]);
            }
            if (!strcmp(&token[0][len+1], "STATIONS"))
            {
                sprintf(db_paths.stations, "%s", token[1]);
            }
        }
        if (!strncmp(token[0], "SISMALP", 7))
        {
            if (!strcmp(&token[0][8], "EVNDIR"))
            {
                sprintf(db_paths.events, "%s", token[1]);
            }
            if (!strcmp(&token[0][8], "EVNLST"))
            {
                sprintf(db_paths.events_lst, "%s", token[1]);
            }
            if (!strcmp(&token[0][8], "EVNTBL"))
            {
                sprintf(db_paths.events_tbl, "%s", token[1]);
            }
        }
        if (!strcmp(token[0], "SEED_ORGANIZATION"))
        {
            int len = strlen("SEED_ORGANIZATION");
            char *pt = &line_save[len];
            int i = 0;
            while (pt[i++] == ' '); --i;
            sprintf(seed_organization, "%s", &line_save[len+i]);
        }
        if (!strcmp(token[0], "SEED_LABEL"))
        {
            int len = strlen("SEED_LABEL");
            char *pt = &line_save[len];
            int i = 0;
            while (pt[i++] == ' '); --i;
            sprintf(seed_label, "%s", &line_save[len+i]);
        }
        if (!strcmp(token[0], "SEED_DATAFORMAT"))
        {
            int len = strlen("SEED_DATAFORMAT");
            char *pt = &line_save[len];
            int i = 0;
            while (pt[i++] == ' '); --i;
            sprintf(seed_scalefactor, "%s", &line_save[len+i]);
        }
    }
    fclose(Fp);

    if (foundNetname == FALSE)
    {
       fprintf(stderr,
               "\n\tpaths_init(): can't find network name '%s'\n", netname);
       fprintf(stderr,
               "\tin cvtit config file '%s'\n\n", db_paths.cvtit_conf);
       exit(1);
    }

    if (/* !strlen(db_paths.stations) || */
        !strlen(db_paths.timedt) ||
        !strlen(db_paths.timedft) /* ||
        !strlen(db_paths.extra_tcorr) */)
    {
       pr("\n\tpaths_init(): wrong syntax or missing path \n");
       pr("\tin cvtit config file  ");
       pr(db_paths.cvtit_conf);
       pr("\n");
       pr("\tKey words:\n");
       pr("\t  NETWORK_DT path        or  network_dt path\n");
       pr("\t  NETWORK_DFT path       or  network_dft path\n");
       pr("\t  NETWORK_TC path        or  network_tc path\n");
       pr("\t  NETWORK_STATIONS path  or  network_stations path\n");
       pr("\n");
       exit(1);
    }

    if (1)
    {
        printf("\n");
        printf("  paths_init: stations:     '%s'\n", db_paths.stations);
        printf("  paths_init: timedt:       '%s'\n", db_paths.timedt);
        printf("  paths_init: timedft       '%s'\n", db_paths.timedft);
        printf("  paths_init: extra_tcorr   '%s'\n", db_paths.extra_tcorr);
        printf("  paths_init: events:       '%s'\n", db_paths.events);
        printf("  paths_init: events_lst:   '%s'\n", db_paths.events_lst);
        printf("  paths_init: events_tbl:   '%s'\n", db_paths.events_tbl);
        if (strlen(seed_organization))
        printf("  paths_init: seed_organization: `%s`\n", seed_organization);
        if (strlen(seed_label))
        printf("  paths_init: seed_label       : `%s`\n", seed_label);
        if (strlen(seed_scalefactor))
        printf("  paths_init: seed_dataformat  : `%s`\n", seed_scalefactor);
    }

    return;
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
        ts.min   <  0 ||
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
    if (ts.min > 59)
    {
        sprintf(mesg,"check_tstruct: illegal minute %d", ts.min);
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


/*
void get_dbltime1(date, time, dbltime)   02.04.1995 18:20:27.950
void get_dbltime2(str, dbltime)            95.02.12-11:34:45.123
void get_dbltime3(str, dbltime)            12.02.95-11:34:45.123
void get_dbltime4(str, dbltime)          1995.02.12-11:34:45.123

void str2utime2(date, time, dbltime)   02.04.1995 18:20:27.950
void str2utime3(str, dbltime)            95.02.12-11:34:45.123
void str2utime4(str, dbltime)            12.02.95-11:34:45.123
void str2utime1(str, dbltime)          1995.02.12-11:34:45.123
*/


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
    sprintf(temp, "%2.2s", &str[14]); ts.min   = atoi(temp);
    sprintf(temp, "%2.2s", &str[17]); sec   = atoi(temp);
    sprintf(temp, "%3.3s", &str[20]); msec  = atoi(temp);
    ts.sec = (float) sec + (float) msec / 1000.0;
/*
    printf("==== str2utime1: %4d %02d %02d %02d %02d %.4f\n",
        ts.year, ts.month, ts.day, ts.hour, ts.min, ts.sec);
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
    dbsec += (double) ts.min  *   60.0;
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

    return str2utime1(str2, dbltime);
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

    return str2utime1(str2, dbltime);
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
    sprintf(temp, "%2.2s", &time[ 3]); ts.min   = atoi(temp);
    sprintf(temp, "%2.2s", &time[ 6]); sec   = atoi(temp);
    sprintf(temp, "%3.3s", &time[ 9]); msec  = atoi(temp);
    ts.sec = ((double) sec + (double) msec / (double) 1000.0);
/*
    printf("==== get_dbltime1: %4d %2d %2d %2d %2d %.3f\n",
        ts.year, ts.month, ts.day, ts.hour, ts.min, ts.sec);
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
    tim.tm_min  = ts.min;
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
    sprintf(temp, "%2.2s", &str[12]); ts.min   = atoi(temp);
    sprintf(temp, "%2.2s", &str[15]); sec   = atoi(temp);
    sprintf(temp, "%3.3s", &str[18]); msec  = atoi(temp);
    ts.sec = (double) sec + (double) msec / (double) 1000.0;
/*
    printf("==== get_dbltime2: %4d %2d %2d %2d %2d %.3f\n",
        ts.year, ts.month, ts.day, ts.hour, ts.min, ts.sec);
*/
    if ((pmesg = check_tstruct(ts, "get_dbltime2")) != NULL)
    {
        printf("%s\n", pmesg);
        return;
    }

    sprintf(date, "%2.2s.%2.2s.%4d", &str[6], &str[3], year);
    sprintf(time, "%2.2s:%2.2s:%2.2s.%3.3s",
         &str[9], &str[12], &str[15], &str[18]);

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
    sprintf(temp, "%2.2s", &str[12]); ts.min   = atoi(temp);
    sprintf(temp, "%2.2s", &str[15]); sec   = atoi(temp);
    sprintf(temp, "%3.3s", &str[18]); msec  = atoi(temp);
    ts.sec = (float) sec + (float) msec / 1000.0;
/*
    printf("==== get_dbltime3: %4d %2d %2d %2d %2d %.3f\n",
        ts.year, ts.month, ts.day, ts.hour, ts.min, ts.sec);
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
    sprintf(temp, "%4.4s", &str[ 0]); ts.year  = atoi(temp);
    sprintf(temp, "%2.2s", &str[ 5]); ts.month = atoi(temp);
    sprintf(temp, "%2.2s", &str[ 8]); ts.day   = atoi(temp);
    sprintf(temp, "%2.2s", &str[11]); ts.hour  = atoi(temp);
    sprintf(temp, "%2.2s", &str[14]); ts.min   = atoi(temp);
    sprintf(temp, "%2.2s", &str[17]); sec   = atoi(temp);
    sprintf(temp, "%3.3s", &str[20]); msec  = atoi(temp);
    ts.sec = (float) sec + (float) msec / 1000.0;
/*
    printf("==== get_dbltime4: %4d %2d %2d %2d %2d %.3f\n",
        ts.year, ts.month, ts.day, ts.hour, ts.min, ts.sec);
*/
    if ((pmesg = check_tstruct(ts, "get_dbltime4")) != NULL)
    {
        printf("%s\n", pmesg);
        return;
    }
    sprintf(date, "%2.2s.%2.2s.%4.4s", &str[8], &str[5], &str[0]);
    sprintf(time, "%2.2s:%2.2s:%2.2s.%3.3s",
         &str[11], &str[14], &str[17], &str[20]);

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



/*====================================================================*
 *  find_wordorder
 *====================================================================*/
void find_wordorder(str)
char *str;
{
#define MSB_FIRST 0x76543210         /* MC680x0 word order */
#define MSB_LAST  0x10325476         /* VAX, 80x86 word order */
union
{
    unsigned char character[4];
    unsigned long int integer;
} test4;                              /* holds test 4-byte word */

/* Construct a 4-byte word of known contents - 0x76543210 */
    test4.character[0] = 0x76;
    test4.character[1] = 0x54;
    test4.character[2] = 0x32;
    test4.character[3] = 0x10;

/* determine the 4-byte word order of this machine */
    if      (test4.integer == MSB_FIRST) sprintf(str, "321010");
    else if (test4.integer == MSB_LAST)  sprintf(str, "012301");
    else
    {
        fprintf(stderr,"find_wordorder:  machine word order, ");
        fprintf(stderr,"%ld, not treated by byte swappers.\n",
            test4.integer);
        exit (-1);
    }
}


/*======================================================================*
 * Convert 4 bytes into integer nomber. First byte in MSB.
 *======================================================================*/
int bytes2int4(b1,b2,b3,b4)
char b1,b2,b3,b4;
{
int n = 0;

    n = n | (uchar) b1;
    n = n << 8;
    n = n | (uchar) b2;
    n = n << 8;
    n = n | (uchar) b3;
    n = n << 8;
    n = n | (uchar) b4;
/*
printf("======== %0X %0X %0X %0X   %0X\n",
        (uchar)b1, (uchar)b2, (uchar)b3, (uchar)b4,  nombre);
*/
    return n;
}


/*================================================================*/
void swap_4byte(input)
void *input;
{
union four_bytes *word=input;
char temp;
    temp = word->c[0];
    word->c[0] = word->c[3];
    word->c[3] = temp;
    temp = word->c[1];
    word->c[1] = word->c[2];
    word->c[2] = temp;
}

/*===================================================================*/
void swap_2byte_array(input, number)
short *input;
int number;
{
char tmp[2],*byte;
int i;

    for (i=0,byte=(char*)input; i<number; i++,byte+=2)
    {
        tmp[0] = byte[1];
        tmp[1] = byte[0];
        memcpy(input + i, tmp, 2);
    }
}

/*================================================================*/
void swap_4byte_array(array, number)
void *array;
int number;
{
char tmp[4],*word;
int  *input=(int*)array,i;
    for (i=0,word=(char*)input; i<number; i++,word+=4) {
        tmp[0]=word[3];
        tmp[1]=word[2];
        tmp[2]=word[1];
        tmp[3]=word[0];
        memcpy(input + i, tmp,4);
    }
}


/*===================================================================*/
long open_Frd(fn, fp)
char  *fn;
FILE **fp;
{
long size;
    *fp = NULL;
    if ((*fp = fopen(fn, "r")) == NULL) {
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
    *fp = NULL;
    if ((*fp = fopen(fn, "r+")) == NULL) {
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
    if ((*Fp = fopen(fname, "r")) == NULL)
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
    *Fp = NULL;
    if ((*Fp = fopen(fname, "r+")) == NULL) {
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
    *fp = NULL;
    if ((*fp = fopen(fn, "w+")) == NULL) {
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
        fprintf(stderr,"sparse: illegal 'max_tokens'\n");
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

/*===================================================================*
    Remove trailing carriage return but not trailing blanks.
 *===================================================================*/
int trim_cr(input)
char *input;
{
int n;

  n = strlen(input);
  if (input[n-1] == '\n') input[--n] = 0;
  if (input[n-1] == 0x0d) input[--n] = 0;
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


/*===================================================================*/
void min_max(data, n, smin, smax)
short *data;
int n;
short *smin, *smax;
{
int i;

    *smax = -32768;
    *smin = 32765;
    for (i=0; i<n; i++) {
        if (data[i] > *smax) *smax = data[i];
        if (data[i] < *smin) *smin = data[i];
    }
    return;
}


/*===================================================================*/
int isdir(path)
char *path;
{
struct stat statbuf;

    if (stat(path, &statbuf) != 0)
    {
        return FALSE;
    }
    if ((statbuf.st_mode & S_IFMT) == S_IFDIR)
    {
        return TRUE;
    }
    return FALSE;

/*
    printf("%s %d bytes\n", path, statbuf.st_size);
    printf("st_mode %0lX\n", statbuf.st_mode);
    printf("st_dev %d\n", statbuf.st_dev);
    printf("st_ino %d\n", statbuf.st_ino);
    printf("st_nlink %d\n", statbuf.st_nlink);
    printf("st_rdev %d\n", statbuf.st_rdev);
    printf("st_size %d\n", statbuf.st_size);
*/
}


/*===================================================================*/
void mk_year_day_dir(base, year, jday)
char *base;
char *year;
char *jday;
{
char dir[255];

    sprintf(dir, "%s/%s", base, year);
    if (!isdir(dir))
    {
        if (mkdir(dir, 0755) < 0)
        {
            fprintf(stderr,"ERROR: ckt: can't create dir %s\n", dir);
            exit(1);
        }
    }
    sprintf(dir, "%s/%s/%s", base, year, jday);
    if (!isdir(dir))
    {
        if (mkdir(dir, 0755) < 0)
        {
            fprintf(stderr,"ERROR: ckt: can't create dir %s\n", dir);
            exit(1);
        }
    }
}


/*===================================================================*/
int isfile(path)
char *path;
{
struct stat statbuf;

    if (stat(path, &statbuf) != 0)
    {
        return FALSE;
    }
    if ((statbuf.st_mode & S_IFMT) == S_IFREG)
    {
        return TRUE;
    }
    return FALSE;

/*
    printf("%s %d bytes\n", path, statbuf.st_size);
    printf("st_mode %0lX\n", statbuf.st_mode);
    printf("st_dev %d\n", statbuf.st_dev);
    printf("st_ino %d\n", statbuf.st_ino);
    printf("st_nlink %d\n", statbuf.st_nlink);
    printf("st_rdev %d\n", statbuf.st_rdev);
    printf("st_size %d\n", statbuf.st_size);
*/
}

/*===================================================================*/
int read_stadir(dir, fname)
char *dir;
char *fname;
{
static DIR           *dfd;
static struct dirent *dp;
static int            first = TRUE;
char                  str[256];
int                   found = FALSE;

    if (first)
    {
        FILE *Fp = NULL;
        if (!(Fp = fopen(dir, "r")))
        {
            fprintf(stderr,"read_stadir: can't find dir '%s'\n", dir);
            exit(1);
        }
        fclose(Fp);
        if ((dfd = opendir(dir)) == NULL)
        {
            fprintf(stderr,"read_stadir: can't open dir '%s'\n", dir);
            exit(1);
        }
        while ((dp = readdir(dfd)))
        {
            if (dp->d_name[0]=='.')
                continue;
            sprintf(str, "%s", dp->d_name);
            found = TRUE;
            break;
        }
        if (!found)
        {
            fprintf(stderr,"ERROR: read_stadir: no file found\n");
            exit(1);
        }
        sprintf(fname, "%s/%s", dir, dp->d_name);
        first = FALSE;
        return TRUE;
    }

    while ((dp = readdir(dfd)))
    {
        if (dp->d_name[0]=='.')
                continue;
        sprintf(str, "%s", dp->d_name);
        break;
    }
    if (!dp)
    {
        closedir(dfd);
        first = TRUE;
        return FALSE;
    }
    sprintf(fname, "%s/%s", dir, dp->d_name);
    return TRUE;
}

#ifdef DOS
#include <dir.h>

/*===================================================================*/
int read_dir(dir, fname, pattern)
char *dir;
char *fname;
char *pattern;
{
static struct ffblk ffb;
static int    first = TRUE;
static char   path[256];

    if (first) {
        sprintf(path, "%s\\*%s*", dir, pattern);
        if (findfirst(path, &ffb, 0)) {
            fprintf(stderr,"ERROR: read_dir: No entry found in '%s'\n", dir);
            exit(1);
        }
        sprintf(fname, "%s", ffb.ff_name);
        lcase(fname);
        first = FALSE;
        return TRUE;
    }
    if (findnext(&ffb)) {
        first = TRUE;
        return FALSE;
    }
    sprintf(fname, "%s", ffb.ff_name);
    lcase(fname);
    return TRUE;
}



#else
#include <dirent.h>


/*===================================================================*/
int read_dir(dir, fname, pattern)
char *dir;
char *fname;
char *pattern;
{
static DIR           *dfd;
static struct dirent *dp;
static int            first = TRUE;
int                   found = FALSE;

    fname[0] = '\0';
    if (first) {
        if ((dfd = opendir(dir)) == NULL) {
            fprintf(stderr,"ERROR: read_dir: can't open dir '%s'\n", dir);
            exit(1);
        }
        while ((dp = readdir(dfd))) {
            if (!strstr(dp->d_name, pattern)) continue;
            found = TRUE;
            break;
        }
        if (!found)
        {
            closedir(dfd);
            first = TRUE;
            return FALSE;
        }
        sprintf(fname, "%s", dp->d_name);
        first = FALSE;
        return TRUE;
    }

    while ((dp = readdir(dfd))) {
        if (!strstr(dp->d_name, pattern)) continue;
        break;
    }
    if (!dp) {
        closedir(dfd);
        first = TRUE;
        return FALSE;
    }
    sprintf(fname, "%s", dp->d_name);
    return TRUE;
}
#endif



/*===================================================================
  Sorts x numerically by the combsort, a souped up version of
  bubble sort (Lacey & Box, Byte 16, p315, 1991).
  Almost as fast as quicksort.
*===================================================================*/
void sort(ndt, x)
long ndt;
long *x;
{
#define maxi(a,b) ((a) >= (b) ? (a) : (b))
int i, j, isw, ngap;
long temp;

    ngap = ndt;

again:

    ngap = maxi((int)(ngap/1.3), 1);
    if (ngap == 9 || ngap == 10) ngap = 11;
    isw = 0;

    for (i = 0; i < (ndt-ngap); ++i) {
        j = i + ngap;
        if (x[i] <= x[j]) continue;
        temp = x[i];
        x[i] = x[j];
        x[j] = temp;
        isw = 1;
    }
    if (isw == 1 || ngap > 1) goto again;
    return;
}


static int sort_strings(a, b) char *a, *b; { return (strcmp(a,b)); }


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
    printf("  sort_table: file '%s' (%ld bytes) using linelen=%d\n",
           fname, (long) ftell(Fp_tbl), linelen);

    rewind(Fp_tbl);

/* Allocate memory */

    table = mem_alloc((nlines*linelen), "sort_table");
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


/*===================================================================*/
int worder(word_order)
int    word_order;
{
#define MSB_FIRST 0x76543210            /* MC680x0 Sun word order */
#define MSB_LAST  0x10325476            /* VAX, 80x86 word order */
union {      /* holds test 4-byte word */
    unsigned char character[4];
    unsigned long int integer;
} test4;

union {      /* holds test 2-byte word */
    unsigned char character[2];
    unsigned short int integer;
} test2;


    /* Construct a 4-byte word of known contents - 0x76543210 */
    test4.character[0] = 0x76;
    test4.character[1] = 0x54;
    test4.character[2] = 0x32;
    test4.character[3] = 0x10;

    /* Construct a 2-byte word of known contents - 0xDCBA */
    test2.character[0] = 0xDC;
    test2.character[1] = 0xBA;
/*
    printf("worder: long: %lX  short %X\n",
            test4.integer, (test2.integer & 0x0000FFFF));
*/
    /* determine the 4-byte word order of this machine */
    if (test4.integer == MSB_FIRST) {
        if (word_order == 10) return (FALSE);
        return(TRUE);
    }
    else if (test4.integer == MSB_LAST) {
        if (word_order == 1) return (FALSE);
        return (TRUE);
    }
    else
    {
        fprintf(stderr,"ERROR (worder):  machine word order, ");
        fprintf(stderr,"%ld, not treated by byte swappers.\n",
            test4.integer);
        return -1;
    }
}

/*======================================================================
 *
 *  getLine.c
 *
 *  Read a single line from the given file, stripping out comments and
 *  blank lines.
 *
 *  The processed line will be a NULL terminated string and without
 *  the trailing newline.
 *
 *  Return values: 0 => success
 *                 1 => EOF
 *                 2 => read error
 *                 3 => other error
 *
 *====================================================================*/

int getLine(fp, buffer, buflen, comment, lineno)
FILE *fp;         /* input stream                   */
char *buffer;     /* buffer to hold line            */
int  buflen;      /* length of buffer               */
char comment;     /* comment character              */
int  *lineno;     /* line number of line in buffer  */
{
int i;

    clearerr(fp);

    buffer[0] = 0;
    do {

        /*  Read the next line in the file  */

        if (fgets(buffer, buflen-1, fp) == NULL)
        {
            buffer[0] = 0;
            if (feof(fp))
            {
                return 1;
            }
            else if (ferror(fp))
            {
                return 2;
            }
            else
            {
                return 3;
            }
        }
        ++*lineno;
        
        /*  Truncate everything after comment token  */

        if (comment != 0)
        {
            i = 0;
            while (i < strlen(buffer) && buffer[i++] != comment);
            buffer[--i] = 0;
        }

        /*  Remove trailing blanks  */

        i = strlen(buffer) - 1;
        while (i >= 0 && (buffer[i] == ' ' || buffer[i] == '\n')) --i;
        buffer[++i] = 0;
        
    } while (strlen(buffer) <= 0);

    return 0;
}



/*======================================================================
 *  isinteger.c
 *  Test to see if a string represents a legal decimal integer.
 *====================================================================*/
int isinteger(string)
char *string;
{
int i, ok;

    for (i = 0; i < strlen(string); i++) {
        ok = (isdigit(string[i]) || string[i] == '+' || string[i] == '-');
        if (!ok) return 0;
    }
    return 1;
}


/*=====================================================================
 *  mem_alloc()   Call memory allocator
 *          calloc is supposed to initialize the memory allocated
 *====================================================================*/
char *mem_alloc(nbytes, caller)
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

