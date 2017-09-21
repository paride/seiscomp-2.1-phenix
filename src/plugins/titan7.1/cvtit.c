/*======================================================================
    Main program  cvtit.c

    Titan reader-converter

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
static void  init_cvtit         (FILE*);
static void  end_cvtit          (void);
static void  help               (void);
static void  prompt             (void);
static int   parse_arg          (int, char**);
static void  check_inp_datalist (void);
static void  test_dft_files     (void);
#else
static void  init_cvtit         ();
static void  end_cvtit          ();
static void  help               ();
static void  prompt             ();
static int   parse_arg          ();
static void  check_inp_datalist ();
static void  test_dft_files     ();
#endif

struct option opt;
FILE   *Fp_log;
int    byteswap;
Event  evn;
char   suffix[32];
int    nodata = FALSE;

static struct tit_inpfile_list  *TitFileListHead;
static struct tit_inpfile_list  *TitFileListTail;


extern struct dft_list *dft_head;
extern struct data_list *list_head;
extern char   netname[PATHLEN];
extern char   dtfname[PATHLEN];
extern short  *out_data[][NCOMP];
extern int    nduplic, nnew;
extern char   Station[8];
extern Titseg TitSegment[2048];
extern int    NTitSegment;
extern int    TitSegmentNum;
extern Paths  db_paths;

struct data_segment *dsegm_head;
struct data_segment *dsegm_tail;
struct station *stalist_head;
char   Network[3];

/* For compatibility */
FILE   *Fp_err; /* file ptr to output SEED log file */



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


/*
 * Print out machine word order. Test is in init_cvtit()
 */
    if (opt.verb > 1 && opt.verb <= 2)
    {
        fprintf(Fp_log,"  Machine word order %s\n",
            byteswap ? "3210 (swap bytes)" : "0123 (no byte swap)");
    }

/* Just a debug test */

    if (0) test_dft_files();


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

    if (evn.evn_time != NULL)
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


/*
 * Post decim output data
 */

    if (opt.postdecim.decim_fact > 1)
    {
      for (pl=list_head; pl!=NULL; pl=pl->next)
      {
        if (pl->chan == 0 || pl->chan == 1)
        {
          if (opt.postdecim.chan == pl->chan)
          {
            if (pl->comp == 0) postDecimate(pl);
            if (pl->comp == 1) postDecimate(pl);
            if (pl->comp == 2) postDecimate(pl);
          }
        }
      }
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
printf("Usage:   cvtit input [options]\n");
printf("         cvtit -f list [options]\n");
printf("\n");
printf("input:\n");
printf("    Titan-file  or  Titan-disk  or Titan-DAT tape.\n");
printf("    With a leading \"-f\", input can be a list of files and/or directories.\n");
printf("    Titan-disk must be specified by the directory over which\n");
printf("    the MSDOS disk is mounted.\n");
printf("    Titan-DAT tape must be specified by the device.\n");
printf("\n");
printf("main options:\n");
printf("    -sac      --> output data format: SAC with native byte ordering  (default)\n");
printf("    -sacsun   --> output data format: SAC with SUN byte ordering\n");
printf("    -sis      --> output data format: SISMALP (gain-ranging)\n");
printf("    -ah       --> output data format: AH compressed\n");
printf("    -segy     --> output data format: SEGY\n");
printf("    -mseed    --> output data format: MINISEED\n");
printf("    -bin      --> output data format: BINARY (real_4)\n");
printf("    -time     --> process time; no data extraction.\n");
printf("    -tcorr    --> do not apply delta-t time correction\n");
printf("     tl=fname --> extract events for time listed in specified file\n");
printf("     ts=n     --> chunk output data into n secs long segments.\n");
printf("    -v        --> verbose.\n");
printf("\n");
prompt();
printf("\n");
printf("Examples:\n");
printf("    cvtit data/field\n");
printf("    cvtit t091413.lar -time sta=xxx\n");
printf("    cvtit -time -f Inter/1998.09.03/d.* sta=MLS -v\n");
printf("    cvtit /export/titan -time     (Titan disk mounted on \"/export/titan\")\n");
printf("    cvtit /dev/st0 -time          (Titan DAT tape)\n");
printf("\n");

prompt();
printf("                          OPTIONS DESCRIPTION\n");
printf("\n");
printf("-f\n");
printf("    Process the list following \"-f\".\n");
printf("    Examples:\n");
printf("        cvtit -time -f dir1/d.* dir2/*MLS* -v\n");
printf("        cvtit -sis -tcorr -f file1 file2 file3 dir1/file\n");
printf("\n");
prompt();
printf("sta=\n");
printf("    Set station name as specified. For sismalp, only 4 chars are used.\n");
printf("    Characters accepted: alphanumerical upper and lower case,");
printf(" plus '_', '-', '.'\n");
printf("\n");
printf("    When the option sta= is used, cvtit will look thru all .dft files\n");
printf("    containing the specified station name to correct the time\n");
printf("\n");

printf("    Further more, if a file whose name is the same as the specified\n");
printf("    station name is found, cvtit will consult this file to get station\n");
printf("    informations.\n");
printf("\n");
printf("    Station files content parameters relative to the coordinates,\n");
printf("    the acquisition system input channels, including the sensor\n");
printf("    model and its orientation.\n");
printf("\n");
printf("    Station info file format:\n");
printf("    ------------------------\n");
printf("    Example for the /home/fels/sismnet/OMP station file:\n");
printf("\n");
printf("        coordinates  42.76010 1.189400 100.0 0\n");
printf("        chan 0 comp 0 STS2 Z 0.00 -90.00\n");
printf("        chan 0 comp 1 STS2 N 0.00  0.00\n");
printf("        chan 0 comp 2 STS2 E 90.00  0.00\n");
printf("        chan 1 comp 0 CMG5 Z 0.00 -90.00\n");
printf("        chan 1 comp 1 CMG5 N 0.00  0.00\n");
printf("        chan 1 comp 2 CMG5 E 90.00  0.00\n");
printf("\n");
printf("    Note that there are 4 parameters for the location coordinates:\n");
printf("        latitude, longitude, elevation, depth.\n");
printf("    Channels are described according to the Agecodagis channel\n");
printf("    conventions. Only \"primary channels\" are entered\n");
printf("    Sensor components orientation follow the SEED rules (not SAC)\n");
printf("\n");
prompt();
printf("sta= (continuated)\n");
printf("    None of the lines are mandatory (any line can be omitted)\n");
printf("    except if the output format is MINISEED. In that case, the\n");
printf("    sensor model is needed to determine the Seed channel name.\n");
printf("    Sensor code is 3 to 8 char long.\n");
printf("    Sensors supported in the station-response database are:\n");
printf("       STS2 CMG3 CMG5 CMG40 L4C L22 LEN5 LEN20\n");
printf("\n");
prompt();
printf("db=\n");
printf("    Use the titan database to access the station parameters file,\n");
printf("    the time correction files and events tables.\n");
printf("    Example:  db=pyren    where 'pyren' is a network name.\n");
printf("    This option overrides the contents of the file 'STA' of the\n");
printf("    above sta=STA option.\n");
printf("    The option db= requires the environment variable \"CVTIT_CONF\"\n");
printf("    to be set to the full path of titan database.\n");
printf("    Example:\n");
printf("        setenv CVTIT_CONF /home/fels/sismnet/cvtit.conf\n"); 
printf("    The config file must contain 4 paths identified by 4 keywords\n");
printf("    starting with the network name specified above.\n");
printf("    The 4 paths tell the program where are:\n");
printf("      - the .dt files:    observed delta_t files\n");
printf("      - the .dft files:   estimated delta_t files\n");
printf("      - the extra_tcorr file:  extra time correction associated\n");
printf("                          to a .dft file\n");
printf("      - the station parameter file\n");
printf("\n");
printf("    Example for the /home/fels/sismnet/cvtit.conf file:\n");
printf("        pyren_dt       /home/fels/sismnet/dt\n");
printf("        pyren_dft      /home/fels/sismnet/dft\n");
printf("        pyren_tc       /home/fels/sismnet/xtc\n");
printf("        pyren_stations /home/fels/sismnet\n");
printf("\n");
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
printf("    Stations parameters files are named after the sta=STA option\n");
printf("    argument and are located in the \"stations\" directory above.\n");
printf("    Station files content parameters relative to the coordinates,\n");
printf("    the acquisition system input channels, including the sensor\n");
printf("    model and its orientation.\n");
printf("    The station file format is the same then the one describe above.\n");
printf("\n");
printf("    Example for the /home/fels/sismnet/OMP station file:\n");
printf("\n");
printf("        coordinates  42.76010 1.189400 100.0 0\n");
printf("        chan 0 comp 0 STS2 Z 0.00 -90.00\n");
printf("        chan 0 comp 1 STS2 N 0.00  0.00\n");
printf("        chan 0 comp 2 STS2 E 90.00  0.00\n");
printf("        chan 1 comp 0 CMG5 Z 0.00 -90.00\n");
printf("        chan 1 comp 1 CMG5 N 0.00  0.00\n");
printf("        chan 1 comp 2 CMG5 E 90.00  0.00\n");
printf("\n");
printf("    Note that there are 4 parameters for the location coordinates:\n");
printf("        latitude, longitude, elevation, depth.\n");
printf("    Channels are described according to the Agecodagis channel\n");
printf("    conventions. Only \"primary channels\" are entered\n");
printf("    Sensor components orientation follow the SEED rules (not SAC)\n");
printf("    Sensors supported in the station-response database are:\n");
printf("       STS2 CMG3 CMG5 CMG40 L4C L22 LEN5 LEN20\n");
printf("\n");
prompt();
printf("-time\n");
printf("    Process time only, no data extraction. This is the \"Time Phase\",\n");
printf("    normally the first pass run on the Titan file. An ASCII file\n");
printf("    'yyyy.mm.dd-hh.mm.ss.STA.dt' is written to disk.\n");
printf("\n");
printf("-pos\n");
printf("    Process GPS position only, no data extraction. An ACSII file\n");
printf("    'yyyy.mm.dd-hh.mm.ss.STA.pos' containing station coordinates\n");
printf("    and elevevation is written to disk.\n");
printf("\n");
printf("info=opt1[,opt2,...,optn]\n");
printf("    Process informations frames only, no data extraction.\n");
printf("    Options:\n");
printf("        all     print out complete information.\n");
printf("        diff    print out complete information only if different from previous.\n");
printf("        decim=n print out complete information 1 over n info frames.\n");
printf("        media   print info relative to media only.\n");
printf("        batt    print info relative to battery voltage only.\n");
printf("        pos     print info relative to GPS position only.\n");
printf("\n");
printf("-tcorr\n");
printf("    Do not correct the time for the delta between the system time and UTC.\n");
printf("    Just apply the AD converter delay and the anti-aliasing filter delay.\n");
printf("\n");
prompt();
printf("chan=\n");
printf("    Extract data only for the specified channel.\n");
printf("\n");
printf("comp=\n");
printf("    Extract data only for the specified component.\n");
printf("\n");
printf("o=\n");
printf("    o=R Apply the relative offset to the data series.\n");
printf("    o=A Apply the absolute offset to the data series.\n");
printf("    o=N Do not apply any offset correction (default).\n");
printf("\n");
printf("tl=\n");
printf("    Extract events from a date-time list read in the specified file name.\n");
printf("    Time list file format: n lines 'yyyy.mm.dd-hh.mm.ss duration'.\n");
printf("    Year may have 2 digits. Duration to extract is in seconds.\n");
printf("    Example:   1997.02.04-09:34:51 200   or   97.02.04-09:34:51 200\n");
printf("    If the difference between 2 consecutive times is less than\n");
printf("    'duration', events can be skipped.\n");
printf("\n");
printf("len=\n");
printf("    Set the event duration to extract in case this variable is missing in\n");
printf("    the date-time event list. Example: len=400 (seconds).\n");
printf("\n");
printf("ts=\n");
printf("    Segment output data using the specified duration in seconds.\n");
printf("    Segments start time occur at multiple of the specified duration.\n");
printf("\n");
prompt();
printf("bof=\n");
printf("    Start processing input Titan file at specified offset (bytes).\n");
printf("\n");
printf("eof=\n");
printf("    Stop process input Titan file past the specified offset (bytes).\n");
printf("\n");
printf("-sis\n");
printf("     Output data format is SISMALP\n");
printf("\n");
printf("-bin\n");
printf("    Output data format is pure binary files (reals).\n");
printf("\n");
printf("-sac\n");
printf("    Output data format is SAC\n");
printf("\n");
printf("-sacsun\n");
printf("    Output data format is SAC with byte swapping for SUN machines, in the\n");
printf("    case the Titan file was processed on a PC.\n");
printf("\n");
printf("-segy\n");
printf("    Output data format is SEGY\n");
printf("\n");
printf("-ah\n");
printf("    Output data format is AH\n");
printf("\n");
printf("-mseed\n");
printf("    Output data format is MINISEED\n");
printf("    Note that:\n");
printf("      - a station parameters file is required, containing the\n");
printf("        type of sensors connected to the acquisition system.\n");
printf("        See above help on 'sta='.\n");
printf("      - option 'net=XX' is required to set the network name.\n");
printf("      - the ESTIMATED delta_t (smoothed) is required for\n");
printf("        calculation of the corrected time.\n");
printf("        This assume that a valid .dft file is found.\n");
printf("        Else, time will be calculated with the starting OBSERVED delta_t.\n");
printf("    Example: cvtit SAOF.tit sta=SAOF chan=0 db=tgrs -mseed net=TG\n");
printf("\n");
printf("-asc\n");
printf("    Output data format is ASCII\n");
printf("\n");
prompt();
printf("-daydir\n");
printf("    Output data are written to disk into day directories with\n");
printf("    names of the form Rnnn where nnn is the julian day.\n");
printf("\n");
printf("postdecim=dec=n[,chan=n]\n");
printf("    Decimate output data series by a factor of \"n\".\n");
printf("    Optional primary channel to decimate can be specified with the\n");
printf("    option chan= (default is chan=0).\n");
printf("    Decimation is performed with the same FIR filter that the one\n");
printf("    used in the Titan acquisition system (symetric 128-coef FIR)\n"); 
printf("\n");
printf("-altname <<<<<<<<<<<<THIS OPTION HAS BEEN REMOVED>>>>>>>>>>\n");
printf("    Use alternate naming rule for event output files. File names\n");
printf("    usually have the form YYYY.MM.DD-HH:MM:SS.C-c.xxx, where date-time\n");
printf("    corresponds to the data start time, C is the channel and c the component.\n");
printf("    For the alternate naming rule, date-time correspond to the event\n");
printf("    time specified in the event time list.\n"); 
printf("\n");
printf("-shortblk\n");
printf("    Discard the first samples at the beginning of the Titan file, in case\n");
printf("    the number is less than expected (%d).\n", PRIM_NUM_DATA);

printf("\n");
printf("-v\n");
printf("    Set the verbose flag.\n");
printf("\n");
printf("\n");
prompt();
printf("Other Internal Options\n");
printf("----------------------\n");
printf("\n");
printf("opt.noinfo\n");
printf("    If TRUE, do not write out .info files (default is FALSE).\n");
printf("\n");
printf("opt.num_traces\n");
printf("    Extract event only if the event has been seen in a number of\n");
printf("    sites greater or equal to the specified number. Default: 0.\n");
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

      else if (!strncmp(argv[i], "net=", 4))
      {
          sprintf(Network, "%.2s", &argv[i][4]);
      }

      else if (!strncmp(argv[i], "-v", 2))
      {
          if (strlen(argv[i]) == 2) opt.verb = 1;
          else opt.verb = atoi(&argv[i][2]);
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

      else if (!strncmp(argv[i],"-sacsun",7))
      {
          opt.do_sac     = TRUE;
          opt.sacsun     = TRUE;
          opt.do_ah      = FALSE;
          opt.do_mseed   = FALSE;
          opt.do_segy    = FALSE;
          opt.do_bindata = FALSE;
          opt.do_sismalp = FALSE;
          opt.do_asc     = FALSE;
      }
      else if (!strncmp(argv[i],"-sac",  4))
      {
          opt.do_sac     = TRUE;
          opt.do_ah      = FALSE;
          opt.do_mseed   = FALSE;
          opt.do_segy    = FALSE;
          opt.do_bindata = FALSE;
          opt.do_sismalp = FALSE;
          opt.do_asc     = FALSE;
      }
      else if (!strncmp(argv[i],"-ah",   3))
      {
          opt.do_sac     = FALSE;
          opt.do_ah      = TRUE;
          opt.do_mseed   = FALSE;
          opt.do_segy    = FALSE;
          opt.do_bindata = FALSE;
          opt.do_sismalp = FALSE;
          opt.do_asc     = FALSE;
      }
      else if (!strncmp(argv[i],"-mseed", 6))
      {
          opt.do_sac     = FALSE;
          opt.do_ah      = FALSE;
          opt.do_mseed   = TRUE;
          opt.do_segy    = FALSE;
          opt.do_bindata = FALSE;
          opt.do_sismalp = FALSE;
          opt.do_asc     = FALSE;
      }
      else if (!strncmp(argv[i],"-segy", 5))
      {
          opt.do_sac     = FALSE;
          opt.do_ah      = FALSE;
          opt.do_mseed   = FALSE;
          opt.do_segy    = TRUE;
          opt.do_bindata = FALSE;
          opt.do_sismalp = FALSE;
          opt.do_asc     = FALSE;
      }
      else if (!strncmp(argv[i],"-bin",  4))
      {
          opt.do_sac     = FALSE;
          opt.do_ah      = FALSE;
          opt.do_mseed   = FALSE;
          opt.do_segy    = FALSE;
          opt.do_bindata = TRUE;
          opt.do_sismalp = FALSE;
          opt.do_asc     = FALSE;
      }
      else if (!strncmp(argv[i],"-sis",  4))
      {
          opt.do_sac     = FALSE;
          opt.do_ah      = FALSE;
          opt.do_mseed   = FALSE;
          opt.do_segy    = FALSE;
          opt.do_bindata = FALSE;
          opt.do_sismalp = TRUE;
          opt.do_asc     = FALSE;
      }
      else if (!strncmp(argv[i],"-asc",  4))
      {
          opt.do_sac     = FALSE;
          opt.do_ah      = FALSE;
          opt.do_mseed   = FALSE;
          opt.do_segy    = FALSE;
          opt.do_bindata = FALSE;
          opt.do_sismalp = FALSE;
          opt.do_asc     = TRUE;
      }

      else if (!strncmp(argv[i],"postdecim=", 10))
      {
          char *token[10];
          int  ntok, j;

          opt.postdecim.chan = 0;
          opt.postdecim.decim_fact = 1;
          ntok = sparse(&argv[i][10], token, ",", 8);
          if (!ntok)
          {
              fprintf(stderr,"\n\nERROR: missing options for postdecim=\n\n");
              return(0);
          }
          for (j=0; j<ntok; j++)
          {
              if (!strncmp(token[j],"chan=", 5))
              {
                  opt.postdecim.chan = atoi(&token[j][5]);
              }
              else if (!strncmp(token[j],"dec=",4))
              {
                  opt.postdecim.decim_fact = atoi(&token[j][4]);
              }
              else
              {
                fprintf(stderr,
                   "\n\nERROR:  'postdecim=': unsupported option '%s'\n\n",
                   token[j]);
                return(0);
              }
          }
/*
printf("===== postdecim chan=%d dec=%d\n",
          opt.postdecim.chan,
          opt.postdecim.decim_fact);
*/
      }

      else if (!strncmp(argv[i],"info=",  5))
      {
          char *token[10];
          int  ntok, j;

          opt.info.decim = 1;
          ntok = sparse(&argv[i][5], token, ",", 8);
          if (!ntok)
          {
              fprintf(stderr,"\n\nERROR: missing options for info=\n\n");
              return(0);
          }
          for (j=0; j<ntok; j++)
          {
              if (!strncmp(token[j],"all", 3))
              {
                  opt.info.all = TRUE;
              }
              else if (!strncmp(token[j],"diff",4))
              {
                 opt.info.diff = TRUE;
              }
              else if (!strncmp(token[j],"decim=", 6))
              {
                  opt.info.decim = atoi(&token[j][6]);
                  if ((opt.info.decim < 1) || (opt.info.decim > 4000))
                      opt.info.decim = 1;
              }
              else if (!strncmp(token[j],"media", 5))
              {
                  opt.info.media = TRUE;
              }
              else if (!strncmp(token[j],"batt", 4))
              {
                  opt.info.batt = TRUE;
              }
              else if (!strncmp(token[j],"pos", 3))
              {
                  opt.info.coord = TRUE;
              }
              else
              {
                fprintf(stderr,
                   "\n\nERROR:  'info=': unsupported option '%s'\n\n",
                   token[j]);
                return(0);
              }
          }
          opt.info.on = TRUE;
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

      else if (!strncmp(argv[i],"-time", 5))
      {
          opt.do_time = TRUE;
      }
      else if (!strncmp(argv[i],"-pos",  4))
      {
          opt.do_coord = TRUE;
      }
      else if (!strncmp(argv[i],"chan=", 5))
      {
          opt.chan = atoi(&argv[i][5]);
      }
      else if (!strncmp(argv[i],"comp=", 5))
      {
          opt.comp = atoi(&argv[i][5]);
      }
      else if (!strncmp(argv[i],"tl=",3))
      {
          sprintf(opt.event_list,"%s",&argv[i][3]);
      }
      else if (!strncmp(argv[i],"len=",  4))
      {
          opt.evn_duration = atol(&argv[i][4]);
      }
      else if (!strncmp(argv[i],"ts=",   3))
      {
          opt.timespan = atof(&argv[i][3]);
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
      else if (!strncmp(argv[i],"-gr",   3))
      {
          opt.gain_range = FALSE;
      }
      else if (!strncmp(argv[i],"-shortblk",8))
      {
          opt.discard_short_blk = TRUE; 
      }
      else if (!strcmp(argv[i],"-data"))
      {
          nodata = TRUE;
      }
      else if (!strncmp(argv[i],"-daydir", 7))
      {
          opt.daydir = TRUE;
      }
      else if (!strncmp(argv[i],"srate=", 6))
      {
          opt.srate = atof(&argv[i][6]);
      }
      else
      {
          fprintf(stderr,"\n\n option '%s' not supported\n\n", argv[i]);
          return(0);
      }
    }

    if (opt.do_mseed)
    {
       opt.daydir = FALSE;
       if (strlen(Network) != 2)
       {
          fprintf(stderr,
              "\n\n option 'net=XX' is mandatory for miniseed output\n\n");
          return(0);
       }
    }
    
/*
 * Test input titan data list
 */
    if (0) check_inp_datalist();

    return(1);
}


/*==================================================================*/
static void check_inp_datalist()
{
int ninp;
struct tit_inpfile_list *titfile;

    printf("\n");
    printf("==== main.c: check_inp_datalist:\n");
    ninp = 0;
    for (titfile=TitFileListHead; titfile!=NULL; titfile=titfile->next)
    {
        if (!isfile(titfile->path) &&
            !isTitanDisk(titfile->path) &&
            !isDir(titfile->path))
        {
            fprintf(stderr,"ERROR: parse_arg: unknown arg : `%s`\n",
                titfile->path);
            fprintf(stderr,"       Check command line syntax.\n\n");
            help();
        }
        fprintf(Fp_log,"    %s\n", titfile->path);
        ++ninp;
    }
    fprintf(Fp_log,"    %d inputs\n", ninp);
    printf("  ==== end check_inp_datalist ====\n");
    printf("\n");
}


/*==================================================================*/
static void test_dft_files()
{
    struct dft_list *pdft;

    sprintf(Station, "MLS");

    printf("\n");
    printf("  ==== main.c: test_dft_files: looking for %s .dft files\n",
           Station);
   
    readDftFiles();

    for (pdft=dft_head; pdft!=NULL; pdft=pdft->next)
    {
        printf("    %s reset=%d beg=%d end=%d\n",
            pdft->dft_name, pdft->time_reset,
            pdft->beg_systime, pdft->end_systime);
    }
    printf("  ==== end test_dft_files ====\n");
    printf("\n");

    exit(0);
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
    Network[0]          = '\0';
    opt.chan            = -1;
    opt.comp            = -1;
    opt.verb            = FALSE;
    opt.do_sac          = TRUE;
    opt.sacsun          = FALSE;
    opt.do_ah           = FALSE;
    opt.do_mseed        = FALSE;
    opt.do_seed         = FALSE;
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
    opt.srate           = 0.0;

/* Determine machine word order */

    find_wordorder(temp);
    if (!strncmp(temp, "3210", 4)) byteswap = TRUE;
    else                           byteswap = FALSE;
}


/*==================================================================*/
static void end_cvtit()
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

    if (opt.do_sismalp)
    {
        if (evn.Fp_sism_evntbl != NULL)
        {
             fclose(evn.Fp_sism_evntbl);
             evn.Fp_sism_evntbl = NULL;
             sort_table(db_paths.events_tbl, 20);
/* Create Sismalp events table in each events/YYMM directory */
             sism_month_events_tbl(db_paths);
        }
        fprintf(Fp_log,"\n  process titan completed; %d new traces, ",nnew);
        fprintf(Fp_log,"%d duplicates\n\n", nduplic);
        return;
    }

    free_data_list();
    fprintf(Fp_log,"\n  process titan completed\n\n");
    return;
}

