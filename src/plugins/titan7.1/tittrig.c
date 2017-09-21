/*======================================================================
    Main program tittrig.c

    Author: J.-F. Fels, OMP, Toulouse

*======================================================================*/
#include "titan.h"
#include "proto.h"
#include "libxplot/proto.h"

struct tit_inpfile_list
{
     char path[511];
     struct tit_inpfile_list *next;
};

#ifdef ANSI_C
static void  init_cvtit         (FILE*);
static void  end_cvtit          (void);
static void  help               (void);
static void  prompt             (void);
static int   parse_arg          (int, char**);
#else
static void  init_cvtit         ();
static void  end_cvtit          ();
static void  help               ();
static void  prompt             ();
static int   parse_arg          ();
#endif


struct option opt;
Trigparm    trig;
FILE       *Fp_log;
int         byteswap;
Event       evn;
char  suffix[32];

extern struct data_list *list_head;
extern char   netname[PATHLEN];
extern char   dtfname[PATHLEN];
extern short  *out_data[][NCOMP];
extern Paths  paths;
extern int    nduplic, nnew;
extern char   Station[8];
extern Paths  db_paths;

int   datalist;
int   nodata = FALSE;
int   dbug   = FALSE;
float plot_scale;
float timeWindowSec;
int   opt_decim;
int   removeFile = FALSE;
extern float Xmargin, Ymargin;

static struct tit_inpfile_list  *TitFileListHead;
static struct tit_inpfile_list  *TitFileListTail;


/*==================================================================*/
int main(argc, argv)
int    argc;
char **argv;
{
struct  tit_inpfile_list *titfile;
struct  data_list *pl;
char    str[40];

    Xmargin = 0.10;
    Ymargin = 0.10;
    plot_scale = 1000.0;
    opt_decim = 1;

    trig.sta = 1.0;
    trig.lta = 50.0;
    trig.ratio = 2.0;
    trig.plot = FALSE;

    if (argc < 2) help();

    init_cvtit(stdout);

    if (!parse_arg(argc,argv))
    {
        help();
        exit(0);
    }

/* Load event list from disk */

    if (strlen(opt.event_list))
    {
       if (read_event_list(0) == 0)
       {
          exit(1);
       }
    }

/* Create or get process paths */

    if (opt.use_database == TRUE) paths_init();


    if (opt.verb > 1 && opt.verb <= 2)
    {
        fprintf(Fp_log,"  Machine word order %s\n",
            byteswap ? "3210 (swap bytes)" : "0123 (no byte swap)");
    }

/*======== Process Titan disk or Titan files ========*/

    for (titfile=TitFileListHead; titfile!=NULL; titfile=titfile->next)
    {
        process_titan(titfile->path);
        if (removeFile == TRUE)
        {
            printf("\n    deleting %s\n", titfile->path);
            unlink(titfile->path);
        }
        removeFile = FALSE;
    }

    for (pl=list_head; pl!=NULL; pl=pl->next)
    {
        time_asc4(str, pl->header->uncorrected_time);
        printf("  %s chan %d comp %d %s %s %d\n",
            pl->station, pl->chan, pl->comp,
            pl->data_fname, str, pl->header->nsamples);
    }

    end_cvtit();
    exit(0);
}


/*==================================================================*/
static void help()
{
printf("\n");
printf("                 TITAN TRIGGER Rev %s\n", REVISION);
printf("                 J.-F. Fels, OMP Toulouse\n");
printf("\n");
printf("Usage:   tittrig input [options]\n");
printf("         tittrig -f list [options]\n");
printf("\n");
printf("input:\n");
printf("    Titan-file  or  Titan-disk  or Titan-DAT tape.\n");
printf("    With a leading \"-f\", input can be a list of files and/or directories.\n");
printf("\n");
printf("options:\n");
printf("    trig=sta,lta,ratio[,flow,fhigh]\n");
printf("        Default values: sta=1 lta=60 ratio=2\n");
printf("        Units of short term and long term average: sta,lta, is secs)\n");
printf("        A front-end bandpass filter is provided, whose default corner freq.\n");
printf("        are flow=0.05, fhigh=20 Hz. Default values can be changed with the\n");
printf("        optional parameters flow,fhigh (Hz).\n");
printf("    +plot    --> plot data to xwindow while running trigger.\n");
printf("                 Data on component 0 are bandpass filtered and mean removed.\n");
printf("    d=       --> plot duration (seconds)\n");
printf("    y=       --> y scale plot (counts)\n");
printf("    -v       --> verbose.\n");
printf("\n");
printf(" Examples:\n");
printf("     tittrig -f /b6/titan/data trig=1,50,2 d=2000 +plot\n");
printf("     tittrig -f /b6/titan/data trig=0.5,50,1.5,0.1,10\n");
printf("\n");

prompt();
printf("                          OPTIONS DESCRIPTION\n");
printf("\n");
printf("-f\n");
printf("    Process the list following \"-f\".\n");
printf("    Examples:\n");
printf("        tittrig7.0 OMP.data chan=0 +plot\n");
printf("        tittrig -f file1 file2 file3 dir1/file\n");
printf("\n");
prompt();
printf("sta=\n");
printf("    Force station name as specified. For sismalp, only 4 chars are used.\n");
printf("    Characters accepted: alphanumerical upper and lower case,");
printf(" plus '_', '-', '.'\n");
printf("\n");
printf("db=\n");
printf("    Use the titan database to access the time correction files.\n");
printf("    Example:  db=pyren    where 'pyren' is a network name.\n");
printf("    This option db= requires the environment variable \"CVTIT_CONF\"\n");
printf("    to be set to the full path of titan database.\n");
printf("    Example:\n");
printf("        setenv CVTIT_CONF /home/fels/sismnet/cvtit.conf\n"); 
printf("    The 2 following paths tell the program where are:\n");
printf("      - the .dft files:   estimated delta_t files\n");
printf("      - the extra_tcorr file:  extra time correction associated\n");
printf("                          to a .dft file\n");
printf("\n");
printf("    Example for the /home/fels/sismnet/cvtit.conf file:\n");
printf("        pyren_dft      /home/fels/sismnet/dft\n");
printf("        pyren_tc       /home/fels/sismnet/xtc\n");
printf("\n");
printf("    The \"dft\" directory contents station sub-directories where\n");
printf("             the .dft files are stored.\n");
printf("    The \"tc\" directory contents station sub-directories with\n");
printf("             a file called \"extra_tcorr\".\n");
printf("    Note that, for convenience, the \"tc\" path can be set equal\n");
printf("    to the \"dft\" path.\n");
printf("    The \"extra_tcorr\" file looks like (times are in secs):\n");
printf("        1999.10.07-17.45.35.OMP.dft 20\n");
printf("        1999.12.31-06.49.36.OMP.dft -12\n");
printf("        2000.08.23-13.44.31.OMP.dft 5\n");
printf("\n");
prompt();
printf("chan=\n");
printf("    Process data only for the specified channel.\n");
printf("\n");
printf("comp=\n");
printf("    Apply trigger to the specified component.\n");
printf("\n");
printf("bof=\n");
printf("    Start processing input Titan file at specified offset (bytes).\n");
printf("\n");
printf("eof=\n");
printf("    Stop process input Titan file past the specified offset (bytes).\n");
printf("\n");
printf("o=\n");
printf("    o=R Apply the relative offset to the data series.\n");
printf("    o=A Apply the absolute offset to the data series.\n");
printf("    o=N Do not apply any offset correction (default).\n");
printf("\n");
printf("-v\n");
printf("    Set the verbose flag.\n");
printf("\n");
printf("\n");
exit(1);
}


/*==================================================================*/
static void prompt()
{
  char line[100];
  printf("MORE: RET   QUIT: q ");
  while ((line[0] = getc(stdin)) != EOF)
  {
    if (line[0]=='q') exit(0);
    return;
  }
}


/*==================================================================*/
static int parse_arg(argc, argv)
int  argc;
char **argv;
{
struct tit_inpfile_list  *tit;
static int multiple_files = FALSE;
int i, list_end;


/*
 * First, if arg "-f", get multiple titan input files
 */
    for (i=1; i<argc; i++)
    {
        if (!strcmp(argv[i], "-f"))
        {
            multiple_files = TRUE;
            list_end = FALSE;
            while((++i) < argc)
            {
               if (!isfile(argv[i]) &&
                   !isTitanDisk(argv[i]) &&
                   !isDir(argv[i]))
               {
                   list_end = TRUE;
                   break;
               }

               tit = (struct tit_inpfile_list *)
                   mem_alloc(sizeof(struct tit_inpfile_list), "parse_arg");
               append_linklist_element(tit, TitFileListHead,TitFileListTail);
               sprintf(tit->path, "%s", argv[i]);
               if (0) printf("==== %s\n", tit->path);
            }
            if (list_end == TRUE) break;
        }
    }

    if (multiple_files == FALSE)
    {
        tit = (struct tit_inpfile_list *)
            mem_alloc(sizeof(struct tit_inpfile_list), "parse_arg");
        append_linklist_element(tit, TitFileListHead, TitFileListTail);
        sprintf(tit->path, "%s", argv[1]);
    }

    if (multiple_files == FALSE) i = 2;
    else                         i = 1;

    for ( ; i<argc; i++)
    {

      if (multiple_files == TRUE)
      {
          if (!strcmp(argv[i], "-f"))
              continue;

          if (isfile(argv[i]) ||
              isTitanDisk(argv[i]) ||
              isDir(argv[i]))
              continue;
      }

      if (!strncmp(argv[i], "sta=", 4))
      {
          sprintf(opt.station, "%.8s", &argv[i][4]);

          if (strlen(opt.station) > 8)
          {
              fprintf(stderr,"\nERROR: station name '%s' ", opt.station);
              fprintf(stderr,"too long (8 char max)\n");
              exit(1);
          }
          if (isdir(argv[1])  && !isTitanDisk(argv[1]))
          {
              fprintf(stderr,"\nERROR: use option 'sta=' only for ");
              fprintf(stderr,"single input file (not input directory)\n");
              exit(1);
          }
      }

      else if (!strncmp(argv[i], "db=",   3))
      {
          sprintf(netname, "%s", &argv[i][3]);
          if (!strlen(netname))
          {
              fprintf(Fp_log,"\n  Please use db=YOUR_NETWORK_NAME.\n\n");
              help();
          }
          ucase(netname);
          opt.use_database = TRUE;
      }

      else if (!strncmp(argv[i], "-v", 2))
      {
          if (strlen(argv[i]) == 2) opt.verb = 1;
          else opt.verb = atoi(&argv[i][2]);
      }
      else if (!strncmp(argv[i], "dec=", 4))
      {
          opt_decim = atoi(&argv[i][4]);
          if (opt_decim <= 2) help();
      }
      else if (!strncmp(argv[i], "trig=", 5))
      {
        char    *token[6];
        int     ntok, j;
      
          ntok = sparse(&argv[i][5], token, ",", 6);
          if (0) for (j=0; j<ntok; j++) printf("---- %s\n", token[j]);
          if ((ntok < 3) || ((ntok > 3) && (ntok != 5)))
          {
              help();
          }
          trig.sta = atof(token[0]);
          trig.lta = atof(token[1]);
          trig.ratio = atof(token[2]);
          if (trig.sta <= 0.0 || trig.sta > 10.0)      help();
          if (trig.lta <= 0.0 || trig.lta > 2000.0)    help();
          if (trig.ratio <= 0.0 || trig.ratio > 100.0) help();
          if (ntok == 5)
          {
              trig.fl = atof(token[3]);
              trig.fh = atof(token[4]);
              if (trig.fl <= 0.0 || trig.fl > 10.0)    help();
              if (trig.fh <= 0.0 || trig.fh > 200.0)   help();
              if (trig.fl > trig.fh)                   help();
          }
          if (0)printf("==== sta=%.1f lta=%.1f ratio=%.1f fl=%.1f fh=%.1f\n",
                  trig.sta, trig.lta, trig.ratio, trig.fl, trig.fh);
      }
      else if (!strncmp(argv[i],"+plot",  5)) 
      {
          trig.plot = TRUE;
      }
      else if (!strncmp(argv[i], "o=", 2))
      {
          if      (argv[i][2] == 'R') opt.do_offset = REL_OFS;
          else if (argv[i][2] == 'A') opt.do_offset = ABS_OFS;
          else if (argv[i][2] == 'N') opt.do_offset = NO_OFS;
          else
          {
              fprintf(stderr,"\n\nERROR:  use option o=R/A/N\n\n");
              return(0);
          }
      }
      else if (!strncmp(argv[i],"tl=",   3))
      {
          sprintf(opt.event_list,"%s",&argv[i][3]);
      }
      else if (!strncmp(argv[i],"y=",    2))
      {
          plot_scale       = atof(&argv[i][2]);
      }
      else if (!strncmp(argv[i],"d=",    2))
      {
          timeWindowSec    = atof(&argv[i][2]);
      }
      else if (!strncmp(argv[i],"chan=", 5))
      {
          opt.chan         = atoi(&argv[i][5]);
      }
      else if (!strncmp(argv[i],"comp=", 5))
      {
          opt.comp         = atoi(&argv[i][5]);
      }
      else if (!strncmp(argv[i],"bof=",  4))
      {
          opt.beg_offset   = atol(&argv[i][4]);
      }
      else if (!strncmp(argv[i],"eof=",  4))
      {
          opt.end_offset   = atol(&argv[i][4]);
      }
      else if (!strncmp(argv[i],"len=",  4))
      {
          opt.evn_duration = atol(&argv[i][4]);
      }
      else if (!strncmp(argv[i],"-shortblk",8))
      {
          opt.discard_short_blk = TRUE;
      }
      else
      {
            fprintf(stderr,"\n\n option %s not supported\n\n", argv[i]);
            return(0);
      }
    }
/*
    if (trig.sta <= 0.0)
    {
        fprintf(stderr,"\n\n argument trig=params is mandatory\n\n");
        help();
    }
*/    
    return(1);
}




/*==================================================================*
    init_cvtit().
    Les options sont initialisees dans ce module.
    Elles sont modifiees par les arguments de la ligne de commande
    de cvtit.
 *==================================================================*/
void init_cvtit(Fp)
FILE *Fp;
{
char        temp[40];

    Fp_log             = Fp;
    evn.evn_time       = NULL;
    evn.evn_duration   = NULL;
    evn.cur_event      = 0;
    evn.Fp_sism_evntbl = NULL;
    sprintf(suffix,".tit");
    opt.station[0]      = '\0';
    opt.chan            = -1;
    opt.comp            = -1;
    opt.verb            = FALSE;
    opt.do_sac          = FALSE;
    opt.sacsun          = FALSE;
    opt.do_ah           = FALSE;
    opt.do_segy         = FALSE;
    opt.do_bindata      = FALSE;
    opt.do_sismalp      = FALSE;
    opt.do_asc          = FALSE;
    opt.use_database    = FALSE;
    opt.do_offset       = 0;
    opt.timespan        = 0.0;
    opt.event_list[0]   = '\0';
    opt.evn_duration    = 180;
    opt.num_traces      = 0;
    opt.daydir          = FALSE;
    opt.beg_offset      = 0;
    opt.end_offset      = -1;
    opt.do_time         = FALSE;
    opt.output_delta_t  = TRUE;
    opt.tcorr_mode      = CORRECT_DFT;
    opt.dtfile          = SMOOTHED;
    opt.noinfo          = FALSE;
    opt.gain_range      = TRUE;
    opt.discard_short_blk = TRUE;

/* Determine machine word order */

    find_wordorder(temp);
    if (!strncmp(temp, "3210", 4)) byteswap = TRUE;
    else                           byteswap = FALSE;
}


/*==================================================================*/
void end_cvtit()
{
int chan, comp;

/* Free memory */

    for (chan=0; chan<NCHAN; chan++) {
        for (comp=0; comp<NCOMP; comp++) {
          if (out_data[chan][comp] != NULL)
              m_free((char *) out_data[chan][comp]);
        }
    }
    if (evn.evn_time      != NULL) m_free((char *) evn.evn_time);
    if (evn.evn_duration  != NULL) m_free((char *) evn.evn_duration);

/*==== Sismalp: if event extraction and use_database turned on, ====*/
/*==== sort event and create events tables used by PICKEV.      ====*/

    free_data_list();
    fprintf(Fp_log,"\n  process titan completed\n\n");
    return;
}
