/*=====================================================================
  
    mkseed.c

    Input: data in miniseed format.
           Station-channel responses: 2 schemes
                1- "PZ" directory + dbird file
                2- "RESPDB" directory

    Output: seed volume

    Algorithm:

      Header Phase: build SEED headers
        - build Stations header logical records.
        - build Abbreviations header logical records.
        - build Time header logical records.

      Data Phase: write the SEED volume:
        - write Volume header logical records.
        - write Abbreviations header logical records.
        - write Stations header logical records.
        - write Time header logical records.
        - write Data records:
            - for each data segment:
                - copy SEED data logical records from disk to SEED volume.


    Seed Volume Header:
      The lenght of the Volume Header depends upon the number of
      entries in the Station Header (blk 011) and the number of
      entries in the Time Header (blk 012). By counting the number of
      stations and the number of time entries we can predict the
      number of logical records of the Volume Header. In turn, we
      need this number to build blockettes 011 and 012, and to set
      properly the record numbers in the time span blockette 074. 
      See MakeTimeHeaders().

    Linked lists:
      The code makes a frequent use of linked list to store objects
      whose number is unkwon a priori. The scheme is the following:
      define the structure, initialize the linked list head and tail,
      and define a macro to link new elements into the list.

      struct some_struct
      {
          ...
          struct some_struct *next;
      }
      struct some_struct *list_head = NULL;
      struct some_struct *list_tail = NULL;

          #define append_linklist_element(new, head, tail) \
                  if (head != NULL) tail->next = new; \
                  tail = new; \
                  if (head == NULL) head = tail;

      In the caller to this procedure, allocate space for a new
      element in the list and link it to the list with:

          append_linklist_element(structure, list_head, list_tail);

      The structure may be filled in before or after linking it.


    UPDATES:
    28 March 1997: Fixed problems when a data file time span
        crosses a change in station response.

 *====================================================================*/
#include "seed.h"
#include "proto.h"
#include "proto_seed.h"
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <sys/types.h>
#include <dirent.h>

#ifdef ANSI_C
void MakeStationHeaders       (struct station*);
void blk_030                  ();
void blk_031_S                ();
void blk_031_C                ();
void blk_033                  ();
void blk_034                  ();
void add_blockette            (char*, char);
static struct station *find_sta_resp (struct data_segment*);
static void MakeVolumeHeaders ();
static void MakeAbbrevHeaders ();
static void MakeTimeHeaders   (int);
static int find_nVH           ();
static void blk_010           (char*, char*);
static void blk_011           (int);
static void blk_012           (int);
static void blk_070           (double, double);
static void blk_074           (struct data_segment*);
static void write_logrec      (char*, int);
static void free_VH           ();
static void free_AH           ();
static void free_SH           ();
static void free_TH           ();
#else
void MakeStationHeaders       ();
void blk_030                  ();
void blk_031_S                ();
void blk_031_C                ();
void blk_033                  ();
void blk_034                  ();
void add_blockette            ();
static struct station *find_sta_resp ();
static void MakeVolumeHeaders ();
static void MakeAbbrevHeaders ();
static void MakeTimeHeaders   ();
static int find_nVH           ();
static void blk_010           ();
static void blk_011           ();
static void blk_012           ();
static void blk_070           ();
static void blk_074           ();
static void write_logrec      ();
static void free_VH           ();
static void free_AH           ();
static void free_SH           ();
static void free_TH           ();
#endif



FILE     *Fp_volume;     /* file ptr to output SEED volume       */
FILE     *Fp_err;        /* file ptr to output SEED log file     */
FILE     *Fp_tmp;        /* file ptr to temporary data files     */
char     vol_fname[255]; /* SEED volume file name                */

char     *PZdir;         /* PZ     response directory (env "PZ") */
char     *RESPDBdir;     /* RESPDB response directory (env "RESPDB") */
char     *cmts_dir;      /* Stations-Channels Comments directory name */

int     tspan_break;

/* Blockettes and logical records stuff */
char      blk[10000];
char      logrec[5000];
int       logrec_num;
int       nc;

/* Dynamically allocated linked lists of structures */
struct station      *stalist_head;
struct station      *stalist_tail;
struct data_segment *dsegm_head;
struct data_segment *dsegm_tail;
struct timehead     *tsegm_head;
struct timehead     *tsegm_tail;
struct lrec         *VH_list_head;
struct lrec         *VH_list_tail;
struct lrec         *AH_list_head;
struct lrec         *AH_list_tail;
struct lrec         *SH_list_head;
struct lrec         *SH_list_tail;
struct lrec         *TH_list_head;
struct lrec         *TH_list_tail;
int                  nVH, nAH, nSH, nTH;
char                *blk010;
char                *blk011;
char                *blk012;

/* String parsing stuff */
char      *token[MAXTOKENS];
int        ntokens;

char  **resplist = NULL;
int   nrespfiles;

/* Flags */
int    do_write    = TRUE;
int    dbug;
double SeedClockTolerance;
int    SeedDataformat;
double SeedDataScaleFactor;
char   Network[3];

extern FILE   *Fp_log;
extern char   Station[8];
extern int    use_PZ;


/*==================================================================*/
void mkseed()
{
struct station *sta;
struct station *psta;
struct lrec    *pt;
struct stat    filestat;
struct data_segment *segm;
int    i, j;
char   temp[10];
int    calculated_nVH;
double data_begtime, data_endtime;
FILE   *Fp_mseed;
char   s1[24], s2[24];


    printf("\tSEED WRITER Rev %s (J.-F. Fels, OMP Toulouse)\n", MKSEED_REV);
    tspan_break  = 86400;
    logrec_num   = 0;
/* Default SEED volume */
    sprintf(vol_fname, VOLFNAME);
    Fp_err = Fp_log;

    printf("  Time span break  %d secs\n", tspan_break);
    printf("\n");

/* Open SEED volume name */

    if (!(Fp_volume = fopen(vol_fname, "w")))
    {
        fprintf(Fp_err,"ERROR: mkseed: can't open %s\n", vol_fname);
        exit(1);
        setbuf(Fp_volume, NULL);
    }

/*==========================================================* 
 * Header phase: 
 *    Read comments
 *    Build headers
 *        - MakeStationHeaders
 *        - MakeAbbrevHeaders
 *        - Make Volume header blockettes b010, b011
 *==========================================================*/

/* Read station comments */

/*
    read_comments();
*/

/* Build Stations Headers */

    for (segm=dsegm_head; segm!=NULL; segm=segm->next)
    {
       if ((psta = find_sta_resp(segm)) == NULL)
       {
          fprintf(stderr, "ERROR: mkseed failed: (find_sta_resp)\n");
          exit(1);
       }
       for (sta=stalist_head; sta!=NULL; sta=sta->next)
       {
          if (psta == sta)
             MakeStationHeaders(sta);
       }
    }

/* Build Abbreviations Headers */

    MakeAbbrevHeaders();

/* Get data beg and end time */

    data_endtime = -1.0;
    for (segm=dsegm_head; segm!=NULL; segm=segm->next)
        if (segm->endtime > data_endtime) data_endtime = segm->endtime;
    data_begtime = data_endtime;
    for (segm=dsegm_head; segm!=NULL; segm=segm->next)
        if (segm->begtime < data_begtime) data_begtime = segm->begtime;

    dbt_to_asc(data_begtime, s1);
    dbt_to_asc(data_endtime, s2);

/* Make Volume header blockettes b010, b011 */

    blk_010(s1, s2);
    calculated_nVH = find_nVH();
    blk_011(calculated_nVH + nAH + 1); 

if (1)
{
    printf(" %3d  Volume  Header Records\n", calculated_nVH);
    printf(" %3d  Abbrev  Header Records\n", nAH);
    printf(" %3d  Station Header Records\n", nSH);
}

/* Build Time span Headers and Volume header blockette b012 */

    MakeTimeHeaders(calculated_nVH + nAH + nSH + 1);


/*==========================================================* 
 *   Data phase:
 *       Write SEED volume
 *==========================================================*/


/*====  Volume control header ====*/

    logrec_num = 0;
    MakeVolumeHeaders();
    for (i=0,pt=VH_list_head; i<nVH; i++,pt=pt->next)
    {
        sprintf(temp, "%06d", ++logrec_num);
        strncpy(pt->rec, temp, 6);
        write_logrec(pt->rec, logrec_num);
    }

    /* If expected nVH greater than actual nVH, write blank */
    /* Volumes logical records */

    if (calculated_nVH > nVH)
    {
        for (nc=0; nc<LRECL; nc++) logrec[nc] = ' ';
        while (nVH++ < calculated_nVH)
        {
            sprintf(temp, "%06dV ", ++logrec_num);
            strncpy(logrec, temp, 8);
            write_logrec(logrec, logrec_num);
        }
    }


/*====  Abbreviation Dictionary Control Headers ====*/

    MakeAbbrevHeaders();
    for (i=0,pt=AH_list_head; i<nAH; i++,pt=pt->next)
    {
        sprintf(temp, "%06d", ++logrec_num);
        strncpy(pt->rec, temp, 6);
        write_logrec(pt->rec, logrec_num);
    }

/*====  Station control header ====*/

    for (i=0,pt=SH_list_head; i<nSH; i++,pt=pt->next)
    {
        sprintf(temp, "%06d", ++logrec_num);
        strncpy(pt->rec, temp, 6);
        write_logrec(pt->rec, logrec_num);
    }

/*====  Time span control header ====*/

    for (i=0,pt=TH_list_head; i<nTH; i++,pt=pt->next)
    {
        sprintf(temp, "%06d", ++logrec_num);
        strncpy(pt->rec, temp, 6);
        write_logrec(pt->rec, logrec_num);
    }

/*==== Data records ====*/
 
    for (segm=dsegm_head; segm!=NULL; i++,segm=segm->next)
    {
        dbt_to_asc(segm->begtime, s1);
        dbt_to_asc(segm->endtime, s2);
        Fp_mseed = fopen(segm->fname, "r");
        stat(segm->fname, &filestat);

        if (do_write)
        {
          int nnn;
          if (filestat.st_size % LRECL)
          {
            fprintf(stderr,"ERROR: mkseed: wrong size for '%s'\n", segm->fname);
            fprintf(stderr,"size = %d\n", (int) filestat.st_size);
            exit(1);
          }
          for (j=0; j<(filestat.st_size / LRECL); j++)
          {
            if ((nnn=fread(logrec, 1, LRECL, Fp_mseed)) != LRECL)
            {
                fprintf(stderr, "ERROR: mkseed: read failed: n=%d\n", nnn);
                exit(1);
            }
            sprintf(temp, "%06d", ++logrec_num);
            strncpy(logrec, temp, 6);
            write_logrec(logrec, logrec_num);
          }
        }
        fclose(Fp_mseed); Fp_mseed = NULL;

    /*
     * Delete miniseed file; we don't need it anymore
     */
/*
        unlink(segm->fname);
*/
    }


/*==== Fill up and flush out last physical record ====*/

    for (i=0; i<LRECL; i++)    logrec[i] = ' ';
    while ((logrec_num++ % 8)) write_logrec(logrec, logrec_num);
    ftell(Fp_volume);
    stat(vol_fname, &filestat);
    if ((filestat.st_size % PRECL) != 0)
    {
        fprintf(stderr,"ERROR: mkseed: volume has a wrong size\n");
        fprintf(stderr,"size = %d\n", (int) filestat.st_size);
        exit(1);
    }

/*==== Close files, free memory... */

    fclose(Fp_volume);
    free_VH();
    free_AH();
    free_SH();
    free_TH();
    fprintf(Fp_err,"\tmkseed completed\n");
    fprintf(Fp_err,
       "  Output Seed volume name is '%s'. See also .info file\n", VOLFNAME);
    if (SeedDataScaleFactor != 0.0)
        fprintf(Fp_err,
       "  Seed data (uncompressed) were scaled a by factor %E\n",
                SeedDataScaleFactor);
    fprintf(Fp_err,"\n");
}



/*==================================================================*/
struct station *find_sta_resp(segm)
struct data_segment *segm;
{
struct list {
    struct station *sta;
    struct list *next;
};
static struct list *list_head;
static struct list *list_tail;
struct list        *pt;

FILE     *Fperr;
double   sta_begtime, sta_endtime;
int      found_sta   = FALSE;
int      found_dates = FALSE;
int      found_resp  = FALSE;
struct station *sta;
static struct station *sel_sta = NULL;
struct channel *chan;
int j;

    Fperr = (Fp_err != NULL) ? Fp_err : stderr;
    sta_begtime = sta_endtime = 0.0;

/* Loop thru all entries in stations responses database */

    for (sta=stalist_head; sta!=NULL; sta=sta->next)
    {
       if (!strcmp(sta->name, segm->sta))
       {
          found_sta = TRUE;
          for (j=0; j<sta->nchannel; j++)
          {
             chan = sta->channel[j];
             if (!strcmp(chan->name, segm->cha))
             {
                sta_begtime = chan->beg_utime;
                if (chan->end_utime == 0.0) sta_endtime = END_OF_WORLD;
                else                        sta_endtime = chan->end_utime;

if (0) printf("find_sta_resp: .... %s %s %.0f %.0f\n",
       sta->name, chan->name, sta_begtime, sta_endtime);

                if (segm->begtime >= sta_begtime &&
                    segm->endtime <= sta_endtime)
                {
                    found_dates = TRUE;
                    sel_sta = sta;
                    break;
                }
                if (sta_begtime >= segm->begtime &&
                    sta_begtime <= segm->endtime)
                {
                    fprintf(Fperr,"#### WARNING: find_sta_resp: data file ");
                    fprintf(Fperr,"crosses change of response:\n");
                }
                if (sta_endtime >= segm->begtime &&
                    sta_endtime <= segm->endtime)
                {
                    fprintf(Fperr,"#### WARNING: find_sta_resp: data file ");
                    fprintf(Fperr,"crosses change of response:\n");
                }
             }  /* end if chan->name == segm->cha     */
          }  /* end for sta->nchannel              */
       }  /* end if sta->name == segm->sta      */
    }  /* end for (n station)                */

/* If station not found, issue error message and exit */

    if (found_sta == FALSE)
    {
        fprintf(stderr, "ERROR: find_sta_resp: can't find station ");
        fprintf(stderr, "'%s' in responses files\n", segm->sta);
        exit(1);
    }

/* If dates not found, issue warning but do not exit */

    if (found_dates == FALSE)
    {
        char     s1[40], s2[40];
        dbt_to_asc(segm->begtime, s1);
        dbt_to_asc(segm->endtime, s2);
        fprintf(Fperr,"WARNING: find_sta_resp: found sta %s but ",
           segm->sta);
        fprintf(Fperr,"can't find dates\n%s %s in responses ",s1,s2);
        fprintf(Fperr,"files\n");
        return NULL;
    }

/* Have we seen this response before ? */

    for (pt=list_head; pt!=NULL; pt=pt->next)
    {
        if (pt->sta == sel_sta)
        {
/*
printf("find_sta_resp: already found this station response %p\n", sel_sta);
*/
            found_resp = TRUE;
            return (struct station *) -1;
        }
    }

/* First time we see this response file */

    if (found_resp == FALSE)
    {
        pt = (struct list *) mem(sizeof(struct station *));
        append_linklist_element(pt, list_head, list_tail);
        pt->sta = sel_sta;
        return sel_sta;
    }

    return NULL;
}


/*==================================================================*/
static void MakeVolumeHeaders()
{
    nVH = 0;
    free_VH(0);
    add_blockette("init", 'V');
    add_blockette(blk010,  'V');
    add_blockette(blk011,  'V');
    add_blockette(blk012,  'V');
    add_blockette("end",  'V');
}

/*==================================================================*/
static void MakeAbbrevHeaders()
{
    nAH = 0;
    free_AH();
    add_blockette("init", 'A');
    blk_030();
    blk_031_S();
    blk_031_C();
    blk_033();
    blk_034();
    add_blockette("end",  'A');
}

/*==================================================================*/
static int find_nVH()
{
struct station *sta;
struct data_segment *segm;
int    nsta;
int    ntheader;
int    nbytes;
char   prev_sta[6];

    prev_sta[0] = 0;
    nsta     = 0;
    ntheader = 0;

    for (sta=stalist_head; sta!=NULL; sta=sta->next)
        ++nsta;

    for (segm=dsegm_head; segm!=NULL; segm=segm->next)
    {
        if (strncmp(prev_sta, segm->sta, 6))
            ++ntheader;
        sprintf(prev_sta, "%s", segm->sta);
    }
    nbytes = 8;
    nbytes += strlen(blk010);
    nbytes += 10 + nsta * 11;
    nbytes += 11 + ntheader * 52;
    return (nbytes / (4096 - 8 - 7) + 1);
}


/*==================================================================*/
static void MakeTimeHeaders(lrec_num)
int lrec_num;
{
#define TIME1      &th->b070[ 8]
#define TIME2      &th->b070[31]
#define REC1(n)    &th->b074[n][40]
#define REC2(n)    &th->b074[n][71]
int    i, j, n1, n2;
int    D_recnum;
char   temp[10];
struct data_segment *ss;
struct timehead     *th;
struct s_b074       *pb;
int    ntheader;
char prev_sta[6];
double begtime;
double endtime;

FILE *Fp_mseed;
struct stat  filestat;
char s1[24], s2[24];
int lrecnum;

/*========
    There is one timehead structure per station.
struct timehead
{
    char            *b070;
    struct s_b074   *b074_head;
    struct s_b074   *b074_tail;
    int              logrec;
    struct timehead *next;
};
==========*/

    prev_sta[0] = 0;
    ntheader    = 0;
    begtime =  2000000000.0;
    endtime = -2000000000.0;
    th = (struct timehead *) mem(sizeof(struct timehead));
    append_linklist_element(th, tsegm_head, tsegm_tail);

/*
   Loop thru all data segments found in data().
   Log time segments into blk 074.
   Find min-max times and log into blk 070.
   If this is a new station, start a new time header:
     - make blockette blk_070
     - open a new time header
*/

    lrecnum = 0;
    for (i=0,ss=dsegm_head; ss!=NULL; i++,ss=ss->next)
    {
        dbt_to_asc(ss->begtime, s1);
        dbt_to_asc(ss->endtime, s2);
if (0)  printf("==== MakeTimeHeaders: opening %s\n", ss->fname);
        Fp_mseed = fopen(ss->fname, "r");
        stat(ss->fname, &filestat);

        if (filestat.st_size % LRECL)
        {
            fprintf(stderr,"ERROR: MakeT: wrong size for '%s'\n", ss->fname);
            exit(1);
        }
        if (do_write && filestat.st_size == 0)
        {
            fprintf(stderr,"WARNING: No data for %.5s %.3s %.17s-%.17s\n",
              ss->sta, ss->cha, s1, s2);
            fprintf(Fp_err,"WARNING: No data for %.5s %.3s %.17s-%.17s\n",
              ss->sta, ss->cha, s1, s2);
            ss->beg_rec = -1;
            exit(1);
        }

        ss->beg_rec = lrecnum;
        lrecnum += ss->nrec;
        fclose(Fp_mseed);
    }


    for (i=0,ss=dsegm_head; ss!=NULL; i++,ss=ss->next)
    {
        if (strlen(prev_sta) && strncmp(prev_sta, ss->sta, 6))
        {
            blk_070(begtime, endtime);
            ++ntheader;
            begtime =  2000000000.0;
            endtime = -2000000000.0;
            th = (struct timehead *) mem(sizeof(struct timehead));
            append_linklist_element(th, tsegm_head, tsegm_tail);
        }
        sprintf(prev_sta, "%s", ss->sta);

    /*==== make time span blockette 074 ====*/
        if (ss->beg_rec >= 0)
            blk_074(ss);

    /*==== find min-max times ====*/
        if (ss->begtime < begtime) begtime = ss->begtime;
        if (ss->endtime > endtime) endtime = ss->endtime;
    }

/*==== Make last blockette 070 ====*/

    blk_070(begtime, endtime);
    ++ntheader;


    if (0) printf("MakeTimeHeaders: end of loop 1, %d data segm, %d theader\n",
         i, ntheader);

    if (0) for (i=0,th=tsegm_head; th!=NULL; i++,th=th->next)
    {
        for(j=0,pb=th->b074_head; pb!=NULL; j++, pb=pb->next)
            ;
        printf("MakeTimeHeaders: %3d %.23s %.23s %3d b074\n",i,TIME1,TIME2,j);
    }


/* Find Time headers record number and log it into structure */

    nTH = 0;
    for (th=tsegm_head; th!=NULL; th=th->next)
    {
        th->logrec = lrec_num + nTH;

        add_blockette("init", 'T');
        add_blockette(th->b070, 'T');
        for(pb=th->b074_head; pb!=NULL; pb=pb->next)
            add_blockette(pb->b74, 'T');
        add_blockette("end", 'T');

    }
    if (0) printf("Time header: end of loop 3\n");

/* Make blockette 012 */

    blk_012(ntheader);

    printf(" %3d  Time    Header Records\n", nTH);
    D_recnum = lrec_num + nTH;


/* Now that we know how many Time headers there are, */
/* set data record number into blockettes 74 */

    for (th=tsegm_head; th!=NULL; th=th->next)
    {
        for(pb=th->b074_head; pb!=NULL; pb=pb->next)
        {
/* get relative value of data record number in blockettes */
            sprintf(temp, "%.6s", &pb->b74[40]);
            n1 = atoi(temp);
            sprintf(temp, "%.6s", &pb->b74[71]);
            n2 = atoi(temp);

/* calculate abolute value by adding offset */
            n1 += D_recnum;
            n2 += D_recnum;

/* write result back into blockette */
            sprintf(temp, "%06d", n1);
            strncpy(&pb->b74[40], temp, 6);
            sprintf(temp, "%06d", n2);
            strncpy(&pb->b74[71], temp, 6);
        }
    }

/* Save new Time headers, removing previous first */

    free_TH();
    nTH = 0;
    for (i=0,th=tsegm_head; th!=NULL; i++,th=th->next)
    {
if (0)  printf("MakeTimeHeaders: %3d %.23s %.23s rec %d, ",
             i, TIME1, TIME2, lrec_num+nTH);
        add_blockette("init", 'T');
        add_blockette(th->b070, 'T');
        for(j=0,pb=th->b074_head; pb!=NULL; j++,pb=pb->next)
            add_blockette(pb->b74, 'T');
        add_blockette("end", 'T');
if (0)  printf("%3d b074\n", j);
    }

}

/*===================================================================*
 *  Build blockette 12
 *===================================================================*/
static void blk_012(ntheader)
int ntheader;
{
int j;
struct timehead *th;
/*
012011500021994,099,00:00:00.8066~1994,161,23:59:50.9978~000010
b012 lenght: 11 + ntheader * 52
*/

    blk012 = mem(11 + ntheader * (23+23+6+1));

    j = 0;
    sprintf(&blk012[j], "012");                        j += 3;
    sprintf(&blk012[j], "%04d",0);                     j += 4;
    sprintf(&blk012[j], "%04d",ntheader);              j += 4;
    
    for (th=tsegm_head; th!=NULL; th=th->next)
    {
        sprintf(&blk012[j], "%.23s", &th->b070[ 8]);   j += 23;
        sprintf(&blk012[j], "%.23s", &th->b070[31]);   j += 23;
        sprintf(&blk012[j], "%06d", th->logrec);       j += 6;
if (0)  printf("             %d bytes in b012\n", strlen(blk012));
    }


}

/*===================================================================*
 *  Build blockette 10
 *===================================================================*/
static void blk_010(vol_beg, vol_end)
char *vol_beg;
char *vol_end;
{
int    j, jj, len;
long   time_now;
char   temp[100];
extern char seed_organization[100];
extern char seed_label[100];

    j = 0;
    sprintf(&blk[j], "010");               j += 3;
    sprintf(&blk[j], "%4d",0);             j += 4;
    sprintf(&blk[j], "%4.1f", SEED_REV);   j += 4;
    sprintf(&blk[j], "%.2s",  LREC_LEN);   j += 2;
    sprintf(&blk[j], "%s~",   vol_beg);    j += strlen(vol_beg)+1;
    sprintf(&blk[j], "%s~",   vol_end);    j += strlen(vol_end)+1;
    time_now = time(NULL);
    dbt_to_asc((double) time_now, temp);
    sprintf(&blk[j], "%s~", temp);         j += strlen(temp)+1;

    if ((len = strlen(seed_organization)) != 0)
    {
        sprintf(temp, "%40.40s", seed_organization);
        sprintf(&blk[j], "%s~", temp); j += strlen(temp)+1;
    }
    else
    {
        sprintf(&blk[j], "%s~", ORGANIZATION); j += strlen(ORGANIZATION)+1;
    }
    
    if ((len = strlen(seed_label)) != 0)
    {
        sprintf(temp, "%40.40s", seed_label);
        sprintf(&blk[j], "%s~", temp); j += strlen(temp)+1;
    }
    else
    {
        sprintf(&blk[j], "%s~", LABEL);        j += strlen(LABEL)+1;
    }
    
    sprintf(temp, "%04d", strlen(blk));
    strncpy(&blk[3], temp, 4);

    jj = strlen(blk) + 2;
    blk010 = mem(jj);
    sprintf(blk010, "%s", blk);
}


/*===================================================================*
 *  Build blockette 11
 *===================================================================*/
static void blk_011(recn_ofs)
int recn_ofs;
{
struct station *sta;
int      nsta;
char     temp[10];
/* Template: 0110032002SSB  000003BNG  000006...*/

/* Find number of stations */

    for (nsta=0,sta=stalist_head; sta!=NULL; sta=sta->next) ++nsta;

/* Allocate memory */

    blk011 = mem(10 + nsta * (5+6+1));

    sprintf(blk011, "0110000");

/* Write number of stations */
    sprintf(temp, "%03d", nsta);
    strncpy(&blk011[strlen(blk011)], temp, 3);

    for (sta=stalist_head; sta!=NULL; sta=sta->next)
    {

/* Write station code */
        sprintf(temp, "     ");
        strncpy(temp, sta->name, strlen(sta->name));
        sprintf(&blk011[strlen(blk011)], "%5s", temp);

if (0)  printf("blk_011:  %s %5d\n", sta->name, (recn_ofs + sta->lrec_num));

/* Write corresponding station headers logical record number */
        sprintf(temp, "%06d", (recn_ofs + sta->lrec_num));
        sprintf(&blk011[strlen(blk011)], "%.6s", temp);
    }
}


/*===================================================================*
 *  Build blockette 70
 *===================================================================*/
static void blk_070(begtime, endtime)
double begtime;
double endtime;
{
int j;
char temp[10];
char *pt;

/* allocate memory for blockette 070 */
    tsegm_tail->b070 = mem(100);
    pt = tsegm_tail->b070;

/* fill-up blockette 070 */
    j = 0;
    sprintf(&pt[j], "070");                  j += 3;
    sprintf(&pt[j], "0000");                 j += 4;
    sprintf(&pt[j], "P");                    j += 1;
    dbt_to_asc(begtime, &pt[j]);             j += 22;
    sprintf(&pt[j], "~");                    j += 1;
    dbt_to_asc(endtime, &pt[j]);             j += 22;
    sprintf(&pt[j], "~");                    j += 1;

    sprintf(temp, "%04d", strlen(pt));
    strncpy(&pt[3], temp, 4);

}


/*===================================================================*
 *  Build blockette 74
 *===================================================================*/
static void blk_074(ds)
struct data_segment *ds;
{
#define B074LEN 90            /* Blockette 074 is 84 bytes long */
int            j;
char           temp[10];
struct s_b074 *ps_b074;
char          *pt;

/*=====   B074 template ====*/
/*
static char *b074 = "\
0740000STA    CHA\
YYYY,DDD,HH:MM:SS.mmmm~......01\
YYYY,DDD,HH:MM:SS.mmmm~......01\
000G ";
*/

/* Allocate memory for a new element */
    ps_b074 = (struct s_b074 *) mem(sizeof(struct s_b074));
/* Allocate memory or a new blockette 074 */
    ps_b074->b74 = mem(B074LEN);
    pt = ps_b074->b74;

    j = 0;
    sprintf(&pt[j], "074");                           j += 3;
    sprintf(&pt[j], "    ");                          j += 4;
    sprintf(&pt[j], "     ");
    strncpy(&pt[j], ds->sta, strlen(ds->sta));        j += 5;
    sprintf(&pt[j], "  ");                            j += 2;
    sprintf(&pt[j], "%s", ds->cha);                   j += 3;
    dbt_to_asc(ds->begtime, &pt[j]);                  j += 22;
    sprintf(&pt[j], "~");                             j += 1;
    sprintf(&pt[j], "%06d", ds->beg_rec);             j += 6;
    sprintf(&pt[j], "01");                            j += 2;
    dbt_to_asc(ds->endtime, &pt[j]);                  j += 22;
    sprintf(&pt[j], "~");                             j += 1;
    sprintf(&pt[j], "%06d", ds->beg_rec+ds->nrec-1);  j += 6;
    sprintf(&pt[j], "01000");                         j += 5;
    strncpy(&pt[j], "  ", 2);
    strncpy(&pt[j], NETWORK, strlen(NETWORK));
    strncpy(&pt[j], Network, strlen(Network));
    if (strlen(pt) != 84)
    {
        fprintf(Fp_err, "ERROR: blk_074: wrong lenght\n");
        fprintf(Fp_err, "==%s==\n", pt);
        exit(1);
    }
    sprintf(temp, "%04d", strlen(pt));
    strncpy(&pt[3], temp, 4);

    append_linklist_element(ps_b074,
                            (tsegm_tail->b074_head),
                            (tsegm_tail->b074_tail));
}



/*==================================================================*/
static void write_logrec(buf, lrecnum)
char *buf;
int lrecnum;
{
int fd;
struct stat  filestat;
static struct mtop mtfunc;            /* For writing eofs */

    fd = fileno(Fp_volume);
    if (fwrite(buf, 1, LRECL, Fp_volume) != LRECL)
    {
        fprintf(Fp_err, "ERROR: write_logrec: write failed\n");
        exit(1);
    }
    stat(vol_fname, &filestat);
    if (!(filestat.st_size % 32768))
    {
if (0)  printf("==== %d %d\n", lrecnum, (int) filestat.st_size);
        mtfunc.mt_op = MTWEOF;
        mtfunc.mt_count = 1;
        ioctl(fd, MTIOCTOP, &mtfunc);
    }
}


/*==================================================================*/
static void free_VH()
{
struct lrec  *prec;
struct lrec  *t;
int i = 0;
    prec = VH_list_head;
    while (prec != NULL)
    {
        ++i;
        t = prec->next;
        free((char *) prec);
        prec = t;
    }
    VH_list_head = VH_list_tail = NULL;
    if (0) printf("free %d VH pointers\n", i);
}

/*==================================================================*/
static void free_AH()
{
struct lrec  *prec;
struct lrec  *t;
int i = 0;
    prec = AH_list_head;
    while (prec != NULL)
    {
        ++i;
        t = prec->next;
        free((char *) prec);
        prec = t;
    }
    AH_list_head = AH_list_tail = NULL;
    if (0) printf("free %d AH pointers\n", i);
}

/*==================================================================*/
static void free_SH()
{
struct lrec  *prec;
struct lrec  *t;
int i = 0;
    prec = SH_list_head;
    while (prec != NULL)
    {
        ++i;
        t = prec->next;
        free((char *) prec);
        prec = t;
    }
    SH_list_head = SH_list_tail = NULL;
    if (0) printf("free %d SH pointers\n", i);
}

/*==================================================================*/
static void free_TH()
{
struct lrec  *prec;
struct lrec  *t;
int i = 0;
    prec = TH_list_head;
    while (prec != NULL)
    {
        ++i;
        t = prec->next;
        free((char *) prec);
        prec = t;
    }
    TH_list_head = TH_list_tail = NULL;
    if (0) printf("free %d TH pointers\n", i);
}

static int bytesAlloc;

/*======================================================================
 *  mem()   Call memory allocator
 *          calloc is supposed to initialize the memory
 *====================================================================*/
char *mem(nbytes)
int nbytes;
{
static int bytesAlloc;
char *p;

    if ((p = (char *) calloc(nbytes, 1L)) == NULL)
    {
        fprintf(Fp_err,"ERROR: mem: calloc failed for %d bytes; ",nbytes);
        fprintf(Fp_err,"there was %d bytes allocated\n", bytesAlloc);
        exit(1);
    }
    bytesAlloc += nbytes;
    return p;
}

/*==================================================================
===================================================================*/
int mem_bytes()
{
   return bytesAlloc;
}

