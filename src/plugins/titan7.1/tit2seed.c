/*======================================================================
    Program main.c

    Titan reader

    Author: J.-F. Fels, OMP, Toulouse


   ALGORITHM
   ---------
   - init_cvtit()                        Initializations
   - parse_arg(argc,argv)                Get arguments and set options
   - read optional input files
   - for each titan input file           Process titan file
        process_titan(tit->path);
   - end_cvtit()                         Finish: free allocated variables

Station-channels response
-------------------------

                        tit2seed (main)



                      StaChanResponse
                            |
                            |
          -------------------------------------------
         |                  |                        |
  StaChanResponse     process_titan               mkseed
                            |                        |
                            |                        |
                      readTitanLoop                  | 
                            |                  --------------  
                            |                 |              |
                       output_data     find_sta_resp   MakeStationHeaders
                            |
                            |
                      output_miniseed
                            |
                            |
                      closeDataFiles
                            |
                            |
                        seed_data
                            |
                            |
                   -------------------
                  |                   |
             Steim_comp   (or)    writeRecords
                  |                   |
                   -----        ------
                        |      |
               ---------------------------
             |        seed data            |
             | (compressed or IEEE_Floats) |
               ---------------------------



*======================================================================*/
#include "titan.h"
#include "seed.h"
#include "proto.h"
#include "proto_seed.h"

struct tit_inpfile_list
{
     char path[511];
     struct tit_inpfile_list *next;
};

#ifdef ANSI_C
static int   GetNetwork       (void);
static void  StaChanResponse  (char*);
static void  init_cvtit  (FILE *);
static void  end_cvtit   (void);
static void  help        (void);
static void  prompt      (void);
static int   parse_arg   (int, char**);
#else
static int   GetNetwork       ();
static void  StaChanResponse  ();
static void  init_cvtit  ();
static void  end_cvtit   ();
static void  help        ();
static void  prompt      ();
static int   parse_arg   ();
#endif

struct option opt;
FILE   *Fp_log;
int    byteswap;
Event  evn;
char   suffix[32];
int    nodata = FALSE;
int    use_PZ;

static struct tit_inpfile_list  *TitFileListHead;
static struct tit_inpfile_list  *TitFileListTail;


extern struct dft_list *dft_head;
extern struct data_list *list_head;
extern char   netname[PATHLEN];
extern char   dtfname[PATHLEN];
extern char   Station[8];
extern Titseg TitSegment[2048];
extern int    NTitSegment;
extern int    TitSegmentNum;
extern Paths  db_paths;
extern char   **resplist;
extern int    nrespfiles;

extern struct station *stalist_head;
extern struct station *stalist_tail;
extern struct data_segment *dsegm_head;

extern FILE   *Fp_err;
extern char   *PZdir;         /* PZ     response directory (env "PZ") */
extern char   *RESPDBdir;     /* RESPDB response directory (env "RESPDB") */
extern int    tspan_break;

char         Network[3];
static char  opt_network[3];
static char  Networklist[10][3];
static int   nNetwork;




/*==================================================================*/
int main(argc, argv)
int    argc;
char **argv;
{
struct  tit_inpfile_list *titfile;
struct  data_list *pl;
struct  data_segment *segm;
char    str[40];
int     jj;


    Fp_err = stdout;
    if (argc < 2) help();

    init_cvtit(stdout);

    if (!parse_arg(argc,argv))
    {
        help();
        exit(0);
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

/*==========================================================*/
/* Phase 1:                                                 */
/*       "PZ" option: find network in station-channel files.*/
/*       Look for a line "network_list" or for the          */
/*       the first network name found in channels.           */
/*==========================================================*/

    if (GetNetwork() == 0)
    {
       fprintf(Fp_err,"ERROR: tit2seed: can't find network name in %s\n",
               "station-channel files");
       exit(1);
    }

    if (nNetwork > 1)
    {
       fprintf(Fp_err,"\n");
       fprintf(Fp_err,
          "  ==========================================================\n");
       fprintf(Fp_err,
          "  WARNING: 'PZ' response database: ");
       fprintf(Fp_err,"more than 1 network found\n");
       fprintf(Fp_err,"  in station-channel files. ");
       fprintf(Fp_err,"We will use the first one: '%s'\n", Networklist[0]);
       fprintf(Fp_err,
          "  ==========================================================\n");
       fprintf(Fp_err,"\n");
    }
    sprintf(Network, "%.2s", Networklist[0]);


    StaChanResponse(Network);

    if (0) for (jj=0; jj<nNetwork; jj++)
    {
       sprintf(Network, "%.2s", Networklist[jj]);
       StaChanResponse(Network);
    }




/*======== Process Titan disk or Titan files ========*/

    for (titfile=TitFileListHead; titfile!=NULL; titfile=titfile->next)
    {
        process_titan(titfile->path);
    }

    mkseed();

/*
 * Delete miniseed files
 */
    for (segm=dsegm_head; segm!=NULL; segm=segm->next)
    {
        unlink(segm->fname);
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


/*==================================================================*
 *       find network in station-channel files
 *==================================================================*/
static int GetNetwork()
{
FILE    *Fp;
char    line[255];
char    line_cp[255];
int     lineno;
char    *token[MAXTOKENS];
int     ntokens;
int     i, j, k;
char    net[3], prev_net[3];
int     found_net;

    if (use_PZ == TRUE)
    {
       if(!(PZdir = getenv("PZ")))
       {
          fprintf(stderr,"\tGetNetwork: Can't get environment variable. ");
          fprintf(stderr,"The variable 'PZ' \n\tmust be set to the ");
          fprintf(stderr,"directory containing responses informations.\n");
          fprintf(stderr,"\tExample: setenv PZ /home/fels/respdb/PZ\n\n");
          exit(1);
       }
       if (!isdir(PZdir))
       {
          fprintf(stderr,"\tGetNetwork: directory '%s' not found\n", PZdir);
          exit(1);
       }


   /*
    * Look for a line "network_list"
    */
       for (i=0; i<nrespfiles; i++)
       {
          if (!(Fp = fopen(resplist[i], "r")))
          {
              fprintf(stderr,"ERROR: GetNetwork: can't open '%s'\n",
                      resplist[i]);
              exit(1);
          }
          while (1)
          {
             if ((getLine(Fp, line, 250, '#', &lineno) == 1)) break;
             sprintf(line_cp, "%s", line);
             ntokens = sparse(line_cp, token, " \t", MAXTOKENS);
             ucase(token[0]);
             if (!strcmp(token[0], "NETWORK_LIST"))
             {
                for (j=1; j<ntokens; j++)
                {
                   sprintf(net, "%.2s", token[j]);
if (0) printf("++++++ GetNetwork %s\n", net);
                   found_net = 0;
                   for (k=0; k<nNetwork; k++)
                   {
                     if (!strncmp(net, Networklist[k], 2))
                     {
                        found_net = 1;
                        break;
                     }
                   }
                   if (found_net) continue;
                   if (strncmp(net, prev_net, 2))
                   {
                      sprintf(Networklist[nNetwork], "%.2s", net);
if (0) printf("====== GetNetwork %d %s\n", nNetwork, Networklist[nNetwork]);
                      ++nNetwork;
                   }
                }
             }
             sprintf(prev_net, "%.2s", net);
          }
          fclose(Fp); Fp = NULL;
       }

       if (nNetwork != 0)
       {
          return nNetwork;
       }

   /*
    * Look for the the first network name found in channel
    */
       for (i=0; i<nrespfiles; i++)
       {
          if (!(Fp = fopen(resplist[i], "r")))
          {
              fprintf(stderr,"ERROR: GetNetwork: can't open '%s'\n",
                      resplist[i]);
              exit(1);
          }
          while (1)
          {
             if ((getLine(Fp, line, 250, '#', &lineno) == 1)) break;
             sprintf(line_cp, "%s", line);
             ntokens = sparse(line_cp, token, " \t", MAXTOKENS);
             ucase(token[0]);
             if (!strcmp(token[0], "BEGIN_CHANNEL"))
             {
                if ((getLine(Fp, line, 250, '#', &lineno) == 1)) break;
                sprintf(line_cp, "%s", line);
                ntokens = sparse(line_cp, token, " \t", MAXTOKENS);
                if (ntokens != 9)
                {
                   fprintf(Fp_err,
                       "ERROR: read_channel: Expect 9 tokens, got %d.\n",
                       ntokens);
                   break;
                }

                sprintf(Networklist[nNetwork], "%.2s", token[8]);

if (0) printf("====== GetNetwork %d %s\n", nNetwork, Networklist[nNetwork]);

                ++nNetwork;
                fclose(Fp); Fp = NULL;
                return nNetwork;
             }
          }
       }
    }

/*
 * RESPDB
 */
    else
    {
       if (strlen(opt_network) != 2)
       {
          fprintf(stderr,
              "\n  ERROR: GetNetwork: option 'net=XX' missing\n\n");
          exit(1);
       }
       sprintf(Networklist[nNetwork], "%.2s", opt_network);
       ++nNetwork;
       return nNetwork;
    }
    return 0;
}



/*==================================================================*/
static void StaChanResponse(network)
char *network;
{
struct station *sta;
char   respdir[255];
char   fname[255];
int    error = 0;

    if (use_PZ == TRUE)
    {
       if(!(PZdir = getenv("PZ")))
       {
          fprintf(stderr,
              "\tStaChanResponse: Can't get environment variable. ");
          fprintf(stderr,"The variable 'PZ' \n\tmust be set to the ");
          fprintf(stderr,"directory containing responses informations.\n");
          fprintf(stderr,"\tExample: setenv PZ /home/fels/respdb/PZ\n\n");
          exit(1);
       }
       if (!isdir(PZdir))
       {
          fprintf(stderr,
              "ERROR: StaChanResponse: directory '%s' not found\n", PZdir);
          exit(1);
       }

       while (1)
       {
          if ((error =
               LoadDBsisResponses(resplist, nrespfiles, network)) != TRUE)
          {
              break;
          }
       }
       if (error != FALSE)
       {
          fprintf(stderr,
              "ERROR: StaChanResponse: loading PZ response failed\n");
          exit(1);
       }

if (1) printf("  StaChanResponse: %s network '%s': success\n",
              PZdir, network);

    }
    else
    {
       if (!(RESPDBdir = getenv("RESPDB")))
       {
          fprintf(Fp_err,
              "\tStaChanResponse: Can't get environment variable. ");
          fprintf(Fp_err,"The variable 'RESPDB' \n\tmust be set to the ");
          fprintf(Fp_err,"directory containing responses informations.\n");
          fprintf(Fp_err,"\tExample: setenv RESPDB /home/fels/respdb\n\n");
          exit(1);
       }
       if (!isdir(RESPDBdir))
       {
          fprintf(stderr,"\tStaChanResponse: directory '%s' not found\n", RESPDBdir);
          exit(1);
       }

       sprintf(respdir, "%s/%s", RESPDBdir, STATIONSDIR);

       while (read_stadir(respdir, fname))
       {
          if (strstr(fname, "~")) continue;
          sta = (struct station *) mem(sizeof(struct station));
          append_linklist_element(sta, stalist_head, stalist_tail);
   
          if ((error = LoadStationResponses(fname, sta)) != TRUE)
          {
              break;
          }
       }
       if (error != TRUE)
       {
          fprintf(Fp_err, "ERROR: StaChanResponse: loading RESPDB failed\n");
          exit(1);
       }

if (1) printf("  StaChanResponse: RESPDB %s network '%s': success\n",
              respdir, network);

    }
}


/*==================================================================*/
static void help()
{
printf("\n");
printf("                 TITAN READER Rev %s\n", REVISION);
printf("                 J.-F. Fels, OMP Toulouse\n");
printf("                 O. Coutant, IRIGM Grenoble\n");
printf("\n");
printf("Usage:   tit2seed input [options]\n");
printf("\n");
printf("input:\n");
printf("    Titan-file  or  Titan-disk  or Titan-DAT tape.\n");
printf("    Titan-disk must be specified by the directory over which\n");
printf("    the MSDOS disk is mounted.\n");
printf("    Titan-DAT tape must be specified by the device.\n");
printf("\n");
printf("Examples:\n");
printf("    tit2seed data/field\n");
prompt();
printf("                          OPTIONS DESCRIPTION\n");
printf("sta=\n");
printf("    Set station name as specified. For sismalp, only 4 chars are used.\n");
printf("    Characters accepted: alphanumerical upper and lower case,");
printf(" plus '_', '-', '.'\n");
printf("\n");
printf("    When the option sta= is used, tit2seed will look thru all .dft files\n");
printf("    containing the specified station name to correct the time\n");
printf("\n");

printf("    Further more, if a file whose name is the same as the specified\n");
printf("    station name is found, tit2seed will consult this file to get station\n");
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
printf("    The \"extra_tcorr\" file looks like (time are in secs):\n");
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
printf("bof=\n");
printf("    Start processing input Titan file at specified offset (bytes).\n");
printf("\n");
printf("eof=\n");
printf("    Stop process input Titan file past the specified offset (bytes).\n");
printf("\n");
printf("-shortblk\n");
printf("    Discard the first samples at the beginning of the Titan file, in case\n");
printf("    the number is less than expected (%d).\n", PRIM_NUM_DATA);

printf("\n");
printf("-v\n");
printf("    Set the verbose flag.\n");
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

/* PZ response input files */
      if (!strcmp(argv[i], "-r"))
      {
         use_PZ = TRUE;
         resplist = &argv[i+1];
         nrespfiles = 0;
         while (1)
         {
             if (++i >= argc || argv[i][0] == '-') break;
             ++nrespfiles;
         }
         --i;
      }


/* Timespan break */
      else if (!strcmp(argv[i], "-ts") && (i+1) < argc)
      {
         ++i;
         if (argv[i][0] != '-')
             tspan_break = atoi(argv[i]);
         else help();
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

/* RESPDB response: Network name */
      else if (!strncmp(argv[i], "net=", 4))
      {
          sprintf(opt_network, "%.2s", &argv[i][4]);
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

      else if (!strncmp(argv[i],"chan=", 5))
      {
          opt.chan = atoi(&argv[i][5]);
      }
      else if (!strncmp(argv[i],"comp=", 5))
      {
          opt.comp = atoi(&argv[i][5]);
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
      else if (!strcmp(argv[i],"-data"))
      {
          nodata = TRUE;
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

    return(1);
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
    opt.do_mseed        = TRUE;
    opt.do_seed         = TRUE;
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

    if (evn.evn_time      != NULL) m_free((char *) evn.evn_time);
    if (evn.evn_duration  != NULL) m_free((char *) evn.evn_duration);

    free_data_list();
    fprintf(Fp_log,"\n  process titan completed\n\n");
    return;
}

