/*======================================================================
    Main program   titchan.c

    Dump titan file

    Author: J.-F. Fels, OMP, Toulouse


*======================================================================*/
#include "titan.h"
#include "proto.h"

#ifdef ANSI_C
static void  help         (void);
static int   parse_arg    (int, char**);
static void  titchan    (int);
extern int   sync_phase1  (int);
extern int   sync_phase2  (void);
static void  print_byte_cnt (void);
#else
static void  help         ();
static int   parse_arg    ();
static void  titchan    (int);
extern int   sync_phase1  ();
extern int   sync_phase2  ();
static void  print_byte_cnt ();
#endif


struct option opt;
TITFILE *Fp_tit;
FILE    *Fp_log;
int     byteswap;

/* Unused, but for compatibility : */
struct         acq acq;
struct         Channel Channel[NCHAN];
int            DataOffsetCorr;
double         BasicSamprate;
typedef struct {int dummy;} STA_INFO;
STA_INFO       STA_infos;
static  int    selchan;

/*==================================================================*/
int main(argc, argv)
int    argc;
char **argv;
{
int  titfile_ofs;
char temp[40];


    if (argc < 2)
    {
        help();
    }

    Fp_log              = stdout;
    opt.beg_offset      = 0;
    opt.end_offset      = -1;
    find_wordorder(temp);
    if (!strncmp(temp, "3210", 4)) byteswap = TRUE;
    else                           byteswap = FALSE;

    if (argc < 3)
    {
        fprintf(Fp_log,"\n\ttitchan: arg 'chan=' missing\n");
        help();
    }
    selchan = atoi(&argv[2][5]);
    if (selchan < 0 || selchan > 3)
    {
        fprintf(Fp_log,"\n\ttitchan: unsupported chan number %d\n", selchan);
        help();
    }

    if (!parse_arg(argc,argv))
    {
        help();
        exit(0);
    }

    if ((Fp_tit = topenGuess(argv[1])) == NULL)
    {
        fprintf(Fp_log,"titchan: error opening file %s\n",argv[1]);
        return 1;
    }


    titfile_ofs = opt.beg_offset;

    titchan(titfile_ofs);

    exit(0);
}


/*==================================================================*/
static void help()
{
printf("\n");
printf("                 Titan file channel select %s\n", REVISION);
printf("                 J.-F. Fels, OMP Toulouse\n");
printf("\n");
printf("Usage:   titchan input chan=n [options]\n");
printf("\n");
printf("input:   Titan-file  or  Titan-disk  or Titan-DAT tape.\n");
printf("\n");
printf("options:\n");
printf("  bof=   Start processing input Titan file at specified offset (bytes).\n");
printf("  eof=   Stop process input Titan file past the specified offset (bytes).\n");
printf("\n");
exit(1);
}


/*==================================================================*/
static int parse_arg(argc, argv)
int  argc;
char **argv;
{
int i;


    for (i=3; i<argc; i++)
    {
        if (!strncmp(argv[i],"bof=",  4))
        {
            opt.beg_offset = atol(&argv[i][4]);
        }
        else if (!strncmp(argv[i],"eof=",  4))
        {
            opt.end_offset = atol(&argv[i][4]);
        }
        else
        {
            printf("\nUnsupported option '%s'\n", argv[i]);
            return 0;
        }
    }
    return 1;
}


/*===================================================================*/
static void titchan(initial_file_ofs)
int initial_file_ofs;
{
int   file_ofs;
int   sync;
int   frame_type;
char  frame[20];
char  *f = frame;
int   cha;
FILE *Fp_out;
static int next_sync;
static int first = 1;


    sprintf(frame, "chan.%d.tit", selchan);
    if (!(Fp_out = fopen(frame, "w")))
    {
        fprintf(Fp_log,"\n\ttitchan: can't open file '%s'\n", frame);
        exit(1);
    }

    file_ofs = initial_file_ofs;
    sync_phase1(file_ofs);
    sync_phase2();

    file_ofs = ttell(Fp_tit);
    fprintf(Fp_log,"titchan: start ofs %d\n", file_ofs);

    while (tread(frame, 1, 12, Fp_tit))
    {
        GET_SYNC;
        GET_FRAME_TYPE;
        if (SYNC_KO)
        {
          fprintf(Fp_log,"  titchan: lost frame synchro ");
          fprintf(Fp_log,"@ ofs %d\n", ttell(Fp_tit));
          sync_phase1(file_ofs);
          sync_phase2();
        }
        

        if (first)
        {
if (0) printf("%02X %02X\n", (0XF0 & sync), (0XF0 & ~sync));
            next_sync = sync;
            first = 0;
        }
        if (opt.end_offset > 0 && ttell(Fp_tit) >= opt.end_offset) break;

        switch (frame_type)
        {

          case DATA_PRE_TRIG:
          case DATA_POST_TRIG:
              cha = (short) (f[9] >> 4) & 0XF;
              if (cha == selchan)
              {
                  frame[11] = (0XF0 & next_sync) | (0X0F & frame[11]);
                  next_sync = ~sync;
                  fwrite(frame, 1, 12, Fp_out);
              }
              break;

          case INFO:
              frame[11] = (0XF0 & next_sync) | (0X0F & frame[11]);
              next_sync = ~sync;
              fwrite(frame, 1, 12, Fp_out);
              print_byte_cnt();
              break;

          case TIME: 
              frame[11] = (0XF0 & next_sync) | (0X0F & frame[11]);
              next_sync = ~sync;
              fwrite(frame, 1, 12, Fp_out);
              break;

          case OFFSET:
              frame[11] = (0XF0 & next_sync) | (0X0F & frame[11]);
              next_sync = ~sync;
              fwrite(frame, 1, 12, Fp_out);
              break;
              
          case MISC:
              frame[11] = (0XF0 & next_sync) | (0X0F & frame[11]);
              next_sync = ~sync;
              fwrite(frame, 1, 12, Fp_out);
              break;

          case TIME_CORRECTED:
          case FILLING:
          default:
              fprintf(Fp_log,"  titchan: frame type not supported %d\n",
                      frame_type);
        }
        file_ofs = ttell(Fp_tit);
    }
    return;
}

/*==================================================================*/
static void print_byte_cnt()
{
static int  kkk;

  if (++kkk > 80000)
  {
    printf("  process_titan: ................   %7.2f MBytes read\n",
              (double)(ttell(Fp_tit)) / 1000000.0);
    kkk = 0;
  }
}


void processOffsetFrame(frame, dummy)
char *frame;
int dummy;
{
    return;
}

int processInfoFrames(frame, str)
char *frame;
char *str;
{
    return 0;
}
