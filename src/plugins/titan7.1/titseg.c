/*======================================================================
    Program main.c

    Titan reader

    Author: J.-F. Fels, OMP, Toulouse


   ALGORITHM
   ---------
   - init_cvtit()                        Initializations
   - parse_arg(argc,argv)                Get arguments and set options
   - read optional input files
   - run optional tests
   - for each titan input                Process titan file
        process_titan(tit->path);
   - end_cvtit()                         Finish: free allocated variables

*======================================================================*/
#include "titan.h"
#include "proto.h"

struct tit_inpfile_list
{
     char path[511];
     struct tit_inpfile_list *next;
};

#ifdef ANSI_C
static void  init_cvtit  (FILE *);
static void  end_cvtit   (void);
static void  help        (void);
static int   parse_arg   (int, char**);
static void  prompt      (void);
#else
static void  init_cvtit  ();
static void  end_cvtit   ();
static void  help        ();
static int   parse_arg   ();
static void  prompt      ();
#endif

struct option opt;
FILE   *Fp_log;
int    byteswap;
Event  evn;
char   suffix[32];
int nodata = FALSE;

static struct tit_inpfile_list  *TitFileListHead;
static struct tit_inpfile_list  *TitFileListTail;


extern struct dft_list *dft_head;
extern struct data_list *list_head;
extern char   netname[PATHLEN];
extern char   dtfname[PATHLEN];
extern short  *out_data[][NCOMP];
extern char   Station[8];
extern Titseg TitSegment[2048];
extern int    NTitSegment;
extern int    TitSegmentNum;
extern Paths  db_paths;



/*==================================================================*/
int main(argc, argv)
int    argc;
char **argv;
{
struct  tit_inpfile_list *titfile;
struct  data_list *pl;
char    str[40];


    if (argc < 2) help();

    init_cvtit(stdout);

    if (!parse_arg(argc,argv))
    {
        help();
        exit(0);
    }

/*
 * Load event list from disk ascii file, if any
 */
    if (strlen(opt.event_list))
    {
       if (read_event_list(0) == 0)
       {
          exit(1);
       }
    }

/*
 * Create or get process paths
 */
    if (opt.use_database == TRUE) paths_init();


/*======== Process Titan disk or Titan files ========*/

    for (titfile=TitFileListHead; titfile!=NULL; titfile=titfile->next)
    {
        process_titan(titfile->path);
    }


/*
 * Events extraction.
 * Perform n passes to extract events starting a beg_offset and ending
 * at end_offset.
 */

    if (evn.evn_time != NULL && opt.titseg == FALSE)
    {
        int i;

fprintf(Fp_log, "\n  SEGMENTS EXTRACTION PASS\n");

        opt.event_list[0]   = '\0';
        evn.evn_time = NULL;
        TitSegmentNum = 0;
        titfile = TitFileListHead;
        for (i=0; i<NTitSegment; i++)
        {
            opt.beg_offset = TitSegment[i].beg_offset;
            opt.end_offset = TitSegment[i].end_offset;
            process_titan(titfile->path);
            ++TitSegmentNum;
        }
    }

/*
 * Print out output data list
 */

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
printf("                 TITAN READER Rev %s\n", REVISION);
printf("                 J.-F. Fels, OMP Toulouse\n");
printf("                 O. Coutant, IRIGM Grenoble\n");
printf("\n");
printf("Usage:   titseg input seg=opt [options]\n");
printf("         titseg -f list seg=tjump [options]\n");
printf("\n");
printf("input:\n");
printf("    Titan-file  or  Titan-disk  or Titan-DAT tape.\n");
printf("    With a leading \"-f\", input can be a list of files and/or directories.\n");
printf("    Titan-disk must be specified by the directory over which\n");
printf("    the MSDOS disk is mounted.\n");
printf("    Titan-DAT tape must be specified by the device.\n");
printf("\n");
printf("argument seg=opt:\n");
printf("    Segment input Titan file in several pieces, according to 4 optional schemes:\n");
printf("        seg=time=input_time_list\n");
printf("        seg=offset=input_time_list\n");
printf("        seg=titndx=input_index_list\n");
printf("        seg=tjump\n");
printf("    Example:\n");
printf("        titseg 1214_chi seg=titndx=1214_chi.ndx sta=CHI\n");
printf("        titseg OMP.data seg=time=list sta=CHI db=sismnet\n");
printf("\n");
prompt();
printf("                          OPTIONS DESCRIPTION\n");
printf("\n");
printf("-f\n");
printf("    Process the list following \"-f\".\n");
printf("    Examples:\n");
printf("        titseg -f file1 file2 file3 dir1/file seg=tjump\n");
printf("\n");
printf("sta=\n");
printf("    Set station name as specified.\n");
printf("    Characters accepted: alphanumerical upper and lower case,");
printf(" plus '_', '-', '.'\n");
printf("\n");
prompt();
printf("\n");
printf("db=\n");
printf("    Use the titan database to access the station parameters file,\n");
printf("    the time correction files.\n");
printf("    Example:  'db=pyren' where 'pyren' is a network name.\n");
printf("    The 'sta=STA' option must be associated to the 'db=' option\n");
printf("    in order to get the full time correction, using 'delta_t'\n");
printf("    and 'extra_tcorr'.\n");
printf("    The db= option requires the environment variable \"CVTIT_CONF\"\n");
printf("    to be set to the full path of titan database.\n");
printf("    Example:\n");
printf("        setenv CVTIT_CONF /home/fels/sismnet/cvtit.conf\n");
printf("    The config file must contain 3 paths identified by 4 keywords\n");
printf("    starting with the network name specified above.\n");
printf("    The 3 paths tell the program where are:\n");
printf("      - the .dt files:    observed delta_t files\n");
printf("      - the .dft files:   estimated delta_t files\n");
printf("      - the extra_tcorr file:  extra time correction associated\n");
printf("                          to a .dft file\n");
printf("\n");
printf("    Example for the /home/fels/sismnet/cvtit.conf file:\n");
printf("        pyren_dt       /home/fels/sismnet/dt\n");
printf("        pyren_dft      /home/fels/sismnet/dft\n");
printf("        pyren_tc       /home/fels/sismnet/xtc\n");
printf("\n");
prompt();
printf("\n");
printf("db= (continuation)\n");
printf("    The \"dt\" directory contents station sub-directories where\n");
printf("             the .dt files are stored.\n");
printf("    The \"dft\" directory contents station sub-directories where\n");
printf("             the .dft files are stored.\n");
printf("    The \"tc\" directory contents station sub-directories where\n");
printf("             a file called \"extra_tcorr\" is stored.\n");
printf("    Note that, for convenience, the \"tc\" path can be set equal\n");
printf("    to the \"dft\" path.\n");
printf("    The \"extra_tcorr\" file looks like (times are in secs):\n");
printf("        1999.10.07-17.45.35.OMP.dft 20\n");
printf("        1999.12.31-06.49.36.OMP.dft -12\n");
printf("        2000.08.23-13.44.31.OMP.dft 5\n");
printf("\n");
prompt();
printf("\n");
printf("len=\n");
printf("    Set the event duration to extract in case this variable is missing in\n");
printf("    the above \"input_time_list\". Example: len=400 (seconds).\n");
printf("\n");
printf("bof=\n");
printf("    Start processing input Titan file at specified offset (bytes).\n");
printf("\n");
printf("eof=\n");
printf("    Stop process input Titan file past the specified offset (bytes).\n");
printf("\n");
printf("-tcorr\n");
printf("    Do not correct the time for the delta between the system time and UTC.\n");
printf("    Just apply the AD converter delay and the anti-aliasing filter delay.\n");
printf("\n");
printf("-v\n");
printf("    Set the verbose flag.\n");
exit(1);
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

      if (!strncmp(argv[i],"seg=",4))
      {
          char *token[10];
          int  ntok, j;

          ntok = sparse(&argv[i][4], token, "=", 3);

if (0) for (j=0; j<ntok; j++) printf("=== %s\n", token[j]);

          if (!strcmp(token[0],"time"))
          {
              sprintf(opt.event_list,"%s",token[1]);
          }
          else if (!strcmp(token[0],"offset"))
          {
              sprintf(opt.offset_list,"%s",token[1]);
          }
          else if (!strcmp(token[0],"titndx"))
          {
              sprintf(opt.index_list,"%s",token[1]);
          }
          else if (!strcmp(token[0],"tjump"))
          {
              opt.tjump_seg = TRUE;
          }
          else
          {
             fprintf(stderr,
                     "\n\nERROR:  'seg=': unsupported option '%s'\n\n",
                     token[0]);
             return FALSE;
          }

          opt.titseg = TRUE;
      } 

      else if (!strncmp(argv[i], "sta=", 4))
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

      else if (!strncmp(argv[i],"db=",   3))
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

      else if (!strncmp(argv[i],"len=",  4))
      {
          opt.evn_duration = atol(&argv[i][4]);
      }
      else if (!strncmp(argv[i],"bof=",  4))
      {
          opt.beg_offset = atol(&argv[i][4]);
      }
      else if (!strncmp(argv[i],"eof=",  4))
      {
          opt.end_offset = atol(&argv[i][4]);
      }
      else if (!strncmp(argv[i],"-tcorr",6))
      {
          opt.tcorr_mode = NOCORRECTION;
      }
      else
      {
        fprintf(stderr,"\n\nERROR: option '%s' not supported\n\n", argv[i]);
        return(0);
      }
    }
    if (opt.titseg == FALSE)
    {
      fprintf(stderr,"\n\nERROR: argument 'seg=' missing\n\n");
      return FALSE;
    }

    return TRUE;
}


/*==================================================================*
    init_cvtit().
    Les options sont initialisees dans ce module.
    Elles sont modifiees par les arguments de la ligne de commande
    de cvtit.
 *==================================================================*/
static void init_cvtit(Fp)
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
    opt.do_sac          = TRUE;
    opt.sacsun          = FALSE;
    opt.do_ah           = FALSE;
    opt.do_mseed        = 0;
    opt.do_segy         = FALSE;
    opt.do_bindata      = FALSE;
    opt.do_sismalp      = FALSE;
    opt.do_asc          = FALSE;
    opt.titseg          = FALSE;
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
    opt.do_coord        = FALSE;
    opt.output_delta_t  = TRUE;
    opt.tcorr_mode      = CORRECT_DFT;
    opt.dtfile          = SMOOTHED;
    opt.noinfo          = FALSE;
    opt.gain_range      = TRUE;
    opt.discard_short_blk = TRUE;
    opt.info.on         = FALSE;

/* Determine machine word order */

    find_wordorder(temp);
    if (!strncmp(temp, "3210", 4)) byteswap = TRUE;
    else                           byteswap = FALSE;
}


/*==================================================================*/
static void end_cvtit()
{

/* Free memory */

    if (evn.evn_time      != NULL) m_free((char *) evn.evn_time);
    if (evn.evn_duration  != NULL) m_free((char *) evn.evn_duration);

    fprintf(Fp_log,"\n  process titan completed\n\n");
    return;
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

