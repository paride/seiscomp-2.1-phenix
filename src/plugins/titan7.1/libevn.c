/*====================================================================
         libevn.c            4 April 1995
 *===================================================================*/
#include "titan.h"
#include "proto.h"


/*===================================================================*
    Read events ascii list.
    Format:   date-time duration [ntra]
      token[0]    event time: yyyy.mm.dd-hh.mm.ss
      token[1]    time duration to extract, secs
      token[2] (optional)  number of sites where the event was recorded
 *===================================================================*/
int read_event_list(maxevn)
int      maxevn;
{
FILE    *Fpe = NULL;
char    *token[4];
int      ntok;
char     line[256];
char     line_cp[256];
char     req_asctm[23];
double   dbltime;
int      duration;
int      ntra;
int      nevn;
int      linenum;
extern struct option opt;
extern Event  evn;
char     str[23];
char     *ttoken[8];
int      nttok;
int      year;

    printf("\n");
    printf("  read_event_list: re-order times and remove duplicate lines\n");
    printf("                   WARNING: file will be overwritten\n");
    sort_table(opt.event_list, 50);

    open_Frd(opt.event_list, &Fpe);
 
    nevn = 0;
    linenum = 0;
    while (getline(Fpe, line))
    {
      ++linenum;
      ntok = sparse(line, token, " ", 4);
      if (ntok > 0)
      {
        if (opt.num_traces && ntok < 3)
        {
          fprintf(stderr,"\n  ERROR: read_event_list: ");
          fprintf(stderr,"number of traces specified\n");
          fprintf(stderr,"         but not found in %s line %d\n",
              opt.event_list, linenum);
          fprintf(stderr,"         Recompile with opt.num_traces=0 if you");
          fprintf(stderr," don't use this option.\n");
          fprintf(stderr,"\n");
          exit(1);
        }
        ntra = 0;
        if (ntok > 2) ntra = atoi(token[2]);
        if (ntra && (ntra < opt.num_traces)) continue;
      }
      ++nevn;
    }
    if (nevn == 0)
    {
       fclose(Fpe);
       fprintf(stderr,"\n  ERROR: read_event_list: no events in ");
       fprintf(stderr,"%s for %d+ traces\n\n",opt.event_list,opt.num_traces);
       evn.num_events = 0;
       return 0;
    }

    if (!(evn.evn_time = (int *) m_alloc((nevn+2)*sizeof(int))))
    {
        fprintf(stderr, "  ERROR: read_event_list: m_alloc failed\n");
        exit(1);
    }
    if (!(evn.evn_duration = (int *) m_alloc((nevn+2)*sizeof(int))))
    {
        fprintf(stderr, "  ERROR: read_event_list: m_alloc failed\n");
        exit(1);
    }
    if (!(evn.found = (int *) m_alloc((nevn+2)*sizeof(int))))
    {
        fprintf(stderr, "  ERROR: read_event_list: m_alloc failed\n");
        exit(1);
    }
    if (!(evn.file_offset = (int *) m_alloc((nevn+2)*sizeof(int))))
    {
        fprintf(stderr, "  ERROR: read_event_list: m_alloc failed\n");
        exit(1);
    }
    memset(evn.found, 0, (nevn+2)*sizeof(int));
    memset(evn.file_offset, 0, (nevn+2)*sizeof(int));

    rewind(Fpe);
    nevn = 0;
    while (getline(Fpe, line))
    {
      trim(line);
      sprintf(line_cp, "%s", line);
if (0) printf("==== %s ====\n", line);
      ntok = sparse(line, token, " ", 4);
      if (ntok > 0)
      {
        if (opt.num_traces && ntok < 3)
        {
          fprintf(stderr,"ERROR: read_event_list: number of traces specified\n");
          fprintf(stderr,"but not found in %s\n", opt.event_list);
          exit(1);
        }
    /* inits */
        sprintf(req_asctm, "1994.01.01-00.00.00.000");
        duration = 0;
        ntra     = 0;

    /* Convert time string according to year format */
        sprintf(str, "%s", line);
        nttok = sparse(str, ttoken, ".:-", 7);
        year = atoi(ttoken[0]);
        if (year > 99)
            sprintf(req_asctm, "%2.2s.%2.2s.%2.2s-%8.8s.000",
                 &token[0][2],&token[0][5],&token[0][8], &token[0][11]);
        else
            sprintf(req_asctm, "%2.2s.%2.2s.%2.2s-%8.8s.000",
                 &token[0][0],&token[0][3],&token[0][6], &token[0][9]);

        if (ntok > 1) duration = atoi(token[1]);
        if (ntok > 2) ntra     = atoi(token[2]);

        if (ntra && (ntra < opt.num_traces)) continue;

        dbltime = 0;
        if (str2utime3(req_asctm, &dbltime) ||
            !TIME_OK(dbltime))
        {
          printf("  WARNING: wrong time n file %s line %d\n",
                 opt.event_list, linenum);
          printf("  '%s' (discarded)\n", line_cp);
          continue;
        }
        evn.evn_time[nevn]     = dbltime;
      /*
       * If duration is missing, set evn_duration to default
       */
        if (duration == 0) evn.evn_duration[nevn] = opt.evn_duration;
        else               evn.evn_duration[nevn] = duration;
        ++nevn;
      }
    }
    fclose(Fpe);
    if (nevn == 0)
    {
       fprintf(stderr,"\n\tERROR: read_event_list: no events in ");
       fprintf(stderr,"%s for %d+ traces\n\n",opt.event_list,opt.num_traces);
    }
    printf("  read_event_list: %d times in %s\n", nevn, opt.event_list);
    evn.num_events = nevn;
    return nevn;
}

