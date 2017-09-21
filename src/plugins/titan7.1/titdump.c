/*======================================================================
    Main program   dumptit.c

    Dump titan file

    Author: J.-F. Fels, OMP, Toulouse


*======================================================================*/
#include "titan.h"
#include "proto.h"

#ifdef ANSI_C
static void  help         (void);
static int   parse_arg    (int, char**);
static void  dumpTitan    (int);
extern int   sync_phase1  (int);
extern int   sync_phase2  (void);
#else
static void  help         ();
static int   parse_arg    ();
static void  dumpTitan    (int);
extern int   sync_phase1  ();
extern int   sync_phase2  ();
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
double         SystTime = UNKNOWN;

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

    if (!parse_arg(argc,argv))
    {
        help();
        exit(0);
    }

    if ((Fp_tit = topenGuess(argv[1])) == NULL)
    {
        fprintf(Fp_log,"process_titan: error opening file %s\n",argv[1]);
        return 1;
    }

    titfile_ofs = opt.beg_offset;

    dumpTitan(titfile_ofs);

    exit(0);
}


/*==================================================================*/
static void help()
{
printf("\n");
printf("                 Titan file dump %s\n", REVISION);
printf("                 J.-F. Fels, OMP Toulouse\n");
printf("\n");
printf("Usage:   dumptit input [options]\n");
printf("\n");
printf("input:   Titan-file  or  Titan-disk  or Titan-DAT tape.\n");
printf("\n");
printf("options:\n");
printf("  bof=  Start processing input Titan file at specified offset (bytes).\n");
printf("  eof=  Stop process input Titan file past the specified offset (bytes).\n");
printf("\n");
exit(1);
}


/*==================================================================*/
static int parse_arg(argc, argv)
int  argc;
char **argv;
{
int i;


    for (i=2; i<argc; i++)
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
static void dumpTitan(initial_file_ofs)
int initial_file_ofs;
{
int   file_ofs;
int   i;
int   sync;
int   frame_type;
char  frame[20];
char  *f = frame;
int nn = 0;
 
int          srcode1, srcode2;
int          cha, ncomp, compress;
static int   prev_ncomp, prev_srcode1;
int          systtime, exttime;
int          systmsec, extmsec;
int          n;

    file_ofs = initial_file_ofs;
    sync_phase1(file_ofs);
    sync_phase2();

    file_ofs = ttell(Fp_tit);
    fprintf(Fp_log,"dumpTitan: start ofs %d\n", file_ofs);

    while (tread(frame, 1, 12, Fp_tit))
    {
        GET_SYNC;
        GET_FRAME_TYPE;
        if (SYNC_KO)
        {
          fprintf(Fp_log,"  dumpTitan: lost frame synchro ");
          fprintf(Fp_log,"@ ofs %d\n", ttell(Fp_tit));
          sync_phase1(file_ofs);
          sync_phase2();
        }

        if (opt.end_offset > 0 && ttell(Fp_tit) >= opt.end_offset) break;

        printf("%010d  ", file_ofs);
        switch (frame_type)
        {

          case DATA_PRE_TRIG:
          case DATA_POST_TRIG:
              cha = (short) (f[9] >> 4) & 0XF;
              ncomp = ((f[10] >> 4) & 0X01) ? 1 : 3;
              srcode1 = (f[10] >> 5) & 0X03;
              srcode2 = (f[9] & 0x0F);
              if (srcode2 > 0X7) srcode2 -= 0x10;
              compress  = (int)(f[10] & 0X0F);

              printf("%02X%02X%02X ", f[0]&0XFF,f[1]&0XFF,f[2]&0XFF); 
              printf("%02X%02X%02X ", f[3]&0XFF,f[4]&0XFF,f[5]&0XFF); 
              printf("%02X%02X%02X ", f[6]&0XFF,f[7]&0XFF,f[8]&0XFF); 
              printf("%02X %02X  %02X", f[9]&0XFF, f[10]&0XFF, f[11]&0XFF);
              printf(" -- ");
              printf("ch=%2d nc=%d r=%d c=%d", cha,ncomp,srcode1,compress);
              if (cha < 13)
              { 
                if (ncomp != prev_ncomp || srcode1 != prev_srcode1)
                printf(" NEW");
                prev_ncomp = ncomp;
                prev_srcode1 = srcode1;
              }
              fprintf(Fp_log,"\n");
              break;


          case INFO:
              for (i=0; i<10; i++)
              {
                  if (i && !(i%4)) printf(" ");
                  printf("%02X",f[i]&0XFF);
              }
              printf("  %02X  %02X", f[10]&0XFF,f[11]&0XFF);
              printf(" -- ");
              fprintf(Fp_log,"INFO    num %d\n", frame[10]);

              if (frame[10] != nn)
              {
                fprintf(Fp_log,"  WARNING: suspiscious INFO ");
                fprintf(Fp_log,"frames (skipped) @ ofs %d\n", ttell(Fp_tit));
                while (tread(frame, 1, 12, Fp_tit))
                {
                  fprintf(Fp_log,"====== info %d %d (%d) ofs=%d\n",
                      frame_type,frame[10], nn, ttell(Fp_tit));
                  GET_FRAME_TYPE;
                  if (frame_type != INFO) break;
                }
                nn = 0;
                tseek(Fp_tit, -12, SEEK_CUR);
                continue;
              }
              if (++nn >= 32) nn = 0;
              break;

          case TIME: 
              systtime = bytes2int4(frame[0],frame[1],frame[2],frame[3]);
              exttime =  bytes2int4(frame[4],frame[5],frame[6],frame[7]);
              n = bytes2int4(0,frame[8],frame[9],frame[10]);
              systmsec =  n        & 0X3FF;
              extmsec  = (n >> 10) & 0X3FF;


              printf("%02X%02X%02X%02X ",
                     f[0]&0XFF,f[1]&0XFF,f[2]&0XFF,f[3]&0XFF); 
              printf("%02X%02X%02X%02X ",
                     f[4]&0XFF,f[5]&0XFF,f[6]&0XFF,f[7]&0XFF); 
              printf("%02X%02X%02X ",
                     f[8]&0XFF,f[9]&0XFF,f[10]&0XFF); 
              printf("   %02X", f[11]&0XFF);
              printf(" -- ");
              fprintf(Fp_log,"TIME %d.%03d  %d.%03d\n",
                      systtime,systmsec,exttime,extmsec);
              break;

          case OFFSET:
              for (i=0; i<10; i++)
              {
                  if (i && !(i%4)) printf(" ");
                  printf("%02X",f[i]&0XFF);
              }
              printf(" %02X  %02X", f[10]&0XFF,f[11]&0XFF);
              printf(" -- ");
              fprintf(Fp_log,"        OFST\n");
                            break;
              
          case MISC:
              for (i=0; i<10; i++)
              {
                  if (i && !(i%4)) printf(" ");
                  printf("%02X",f[i]&0XFF);
              }
              printf(" %02X  %02X", f[10]&0XFF,f[11]&0XFF);
              printf(" -- ");
              fprintf(Fp_log,"MISC\n");
              break;

          case TIME_CORRECTED:
          case FILLING:
          default:
              fprintf(Fp_log,"  dumpTitan: frame type not supported %d\n",
                      frame_type);
        }
        file_ofs = ttell(Fp_tit);
    }
    return;
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
