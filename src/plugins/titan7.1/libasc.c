/*======================================================================
    libasc.c

    Library for Titan to ASCII converter

    Author: J.-F. Fels, OMP, Toulouse

    REMEMBER to set #define HDLEN xxx where xxx is the actual size
    of the header + about 44 bytes. These last bytes will be replaced
    with "====".

*======================================================================*/
#include "titan.h"
#include "proto.h"

#define HDLEN  520

/* Prototypes, private */
#ifdef ANSI_C
static void  openFiles      (int);
static void  closeFiles     (int);
static void  writeHead      (int);
static void  writeData      (int);
#else
static void  openFiles      ();
static void  closeFiles     ();
static void  writeHead      ();
static void  writeData      ();
#endif


extern TITFILE *Fp_tit;
extern int     inp_data[NCHAN][NCOMP][NINP];
extern struct  Channel Channel[NCHAN];
extern outData OutData[NCHAN];
extern struct  option opt;
extern Event   evn;
extern FILE    *Fp_log;
extern int     totOutSamples;
extern char    Station[8];
extern double  SystTime;
extern double  Observ_dt;
extern double  ExtraTcorr;


static FILE  *Fp_asc[NCHAN][NCOMP];
static char   fname[NCHAN][NCOMP][PATHLEN];

static double EvnTime;


/*===================================================================*/
void output_asc(last, time_jump)
int last;
int time_jump;
{
char     str[40];
int      chan, comp;

  if (evn.evn_time != NULL)
  {
     flushInputData(last, time_jump);
     Event2TitanSeg(last, time_jump);
     return;
  }

  for (chan=0; chan<NCHAN; chan++) if (Channel[chan].numcomp)
  {

      if (opt.chan >= 0 && (chan != opt.chan))
      {
          Channel[chan].ninp = 0;
          continue;
      }

/*========== Beginning of process or re-initialization ==============*/

    if (Channel[chan].new == TRUE && Channel[chan].ninp > 0)
    {
        openFiles(chan);
        saveOutputParms(chan);
        Channel[chan].new = FALSE;
    }


/*==================  Case: continuous input data ===================*/

    if (time_jump == 0 && last == FALSE)
    {
        writeData(chan);
        Channel[chan].nout += Channel[chan].ninp;
        Channel[chan].ninp = 0;
    }



/*================== Case: discontinuous input data =================*/

    if (time_jump)
    {
        OutData[chan].nsamples = Channel[chan].nout;
        totOutSamples += OutData[chan].nsamples;

        time_asc4(str, OutData[chan].uncorrected_time);
        fprintf(Fp_log,"  %s: chan %d %5d samples %s tot %d\n",
            Station, chan, Channel[chan].nout, str, totOutSamples);

        for (comp=0; comp<Channel[chan].numcomp; comp++)
        {
            if (opt.comp >= 0 && (comp != opt.comp)) continue;
            rewind(Fp_asc[chan][comp]);
        }
        writeHead(chan);
        closeFiles(chan);



        openFiles(chan);
        saveOutputParms(chan);

/* Write pending input data */

        writeData(chan);
        Channel[chan].nout = Channel[chan].ninp;
        Channel[chan].ninp = 0;
    }



/*================== Case: end of input data ======================*/

    if (last == LAST)
    {
        writeData(chan);
        Channel[chan].nout += Channel[chan].ninp;
        Channel[chan].ninp = 0;

        OutData[chan].nsamples = Channel[chan].nout;
        totOutSamples += OutData[chan].nsamples;

        time_asc4(str, OutData[chan].uncorrected_time);
        fprintf(Fp_log,"  %s: chan %d %5d samples %s tot %d\n",
            Station, chan, Channel[chan].nout, str, totOutSamples);

        for (comp=0; comp<Channel[chan].numcomp; comp++)
        {
            if (opt.comp >= 0 && (comp != opt.comp)) continue;
            rewind(Fp_asc[chan][comp]);
        }
        writeHead(chan);
        closeFiles(chan);
    }
  }
  return;
}


/*==================================================================*/
static void openFiles(chan)
int chan;
{
int    i, comp;
char   name[255];
char   prefix[255];
char   head[HDLEN];
int    nc;

    for (i=0; i<HDLEN; i++)
    {
        head[i] = '=';
        if (i && !(i%60)) head[i] = '\n';
    }
    head[HDLEN-1] = '\n';

    if (EvnTime != 0.0)
        nc = get_output_name(EvnTime, prefix);
    else
        nc = get_output_name(Channel[chan].uncorrected_time, prefix);

    sprintf(prefix, "%s.%d", prefix, chan);

/*
 * Check if new name is different from previous. If identical,
 * issue a warning. This may happend if the start time differ 
 * by less than 1 second.
 */
    if (!strncmp(prefix, fname[chan][0], nc))
    {
      fprintf(Fp_log,"  WARNING: open_data: file name already in use:\n");
      fprintf(Fp_log,"      previous: %.25s (overwritten)\n",fname[chan][0]);
      fprintf(Fp_log,"      current : %.25s\n",prefix);
    }

    for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
        if (opt.comp >= 0 && (comp != opt.comp)) continue;

        sprintf(fname[chan][comp], "%s-%d", prefix, comp);

        if (Fp_asc[chan][comp] != NULL)
        {
          fprintf(Fp_log,"  WARNING: openFiles: file pointer ");
          fprintf(Fp_log,"Fp_asc[%d][%d] not null\n", chan,comp);
        }
        sprintf(name, "%s.asc", fname[chan][comp]);
        open_Fwr(name, &Fp_asc[chan][comp]);
        fwrite(head, 1, HDLEN, Fp_asc[chan][comp]);
/*
printf("++++ openFiles: %s head_len=%d  file ofs=%d\n",
    name, HDLEN, (int)ftell(Fp_asc[chan][comp]));
*/
    }
}


/*==================================================================*/
static void writeHead(chan)
int chan;
{
int    comp;
char   *dftfile;
double startime;
int    i;

    startime = GetOutputDataTime(opt.tcorr_mode, Station, chan, &dftfile);

    if (dftfile) sprintf(OutData[chan].dftfile, "%s", dftfile);
    OutData[chan].start_time = startime;

    for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
        if (opt.comp >= 0 && (comp != opt.comp)) continue;
        wrt_info(chan, comp, startime, Fp_asc[chan][comp]);

        if ((int)ftell(Fp_asc[chan][comp]) >= (HDLEN-1))
        {
          fprintf(Fp_log,"  ERROR: writeHead: header length ");
          fprintf(Fp_log,"(%d) exceeds max: %d\n",
              (int)ftell(Fp_asc[chan][comp]), HDLEN);
          fprintf(Fp_log,"  Please recompile cvtit with larger HDLEN\n");
          exit(1);
        }
/*
printf("++++ writeHead: head_len=%d  file ofs=%d\n",
    HDLEN, (int)ftell(Fp_asc[chan][comp]));
*/
        for (i=0; ; i++)
        {
            if (i && !(i%60))
            {
                fprintf(Fp_asc[chan][comp], "\n");
                if ((int)ftell(Fp_asc[chan][comp]) >= (HDLEN-1)) break;

            }
            fprintf(Fp_asc[chan][comp], "-");
            if ((int)ftell(Fp_asc[chan][comp]) >= (HDLEN-1)) break;
        }
/*
printf("++++ writeHead: head_len=%d  file ofs=%d\n",
    HDLEN, (int)ftell(Fp_asc[chan][comp]));
*/
    }
}



/*==================================================================*/
static void closeFiles(chan)
int chan;
{
int comp;

/*
 * Reset all variables in structure OutData.
 */

    clearOutputParms(chan);


    for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
        if (opt.comp >= 0 && (comp != opt.comp)) continue;

if (comp == -1) printf("  closing %.19s for chan %d  %d samples\n",
    fname[chan][comp], chan, OutData[chan].nsamples);


        if (Fp_asc[chan][comp] == NULL)
        {
          fprintf(Fp_log,"  WARNING: closeFiles: null file pointer ...\n");
        }
        else
        {
            fclose(Fp_asc[chan][comp]); Fp_asc[chan][comp] = NULL;
        }
    }
}


/*==================================================================*/
static void writeData(chan)
int chan;
{
int i;
int data;
int comp;

    for (comp=0; comp<Channel[chan].numcomp; comp++)
    {
      if (opt.comp >= 0 && (comp != opt.comp)) continue;

        for (i=0; i<Channel[chan].ninp; i++)
        {
            data = inp_data[chan][comp][i];
            fprintf(Fp_asc[chan][comp], "%d\n", data);
        }
    }
}

