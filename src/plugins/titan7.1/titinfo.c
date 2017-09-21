/*======================================================================
    Main program  titinfo.c

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
#else
static void  init_cvtit         ();
static void  end_cvtit          ();
static void  help               ();
static void  prompt             ();
static int   parse_arg          ();
#endif


struct option opt;
FILE   *Fp_log;
int    byteswap;
Event  evn;
char   suffix[32];
int datalist;
int nodata = FALSE;

static struct tit_inpfile_list  *TitFileListHead;
static struct tit_inpfile_list  *TitFileListTail;


extern char   netname[PATHLEN];
extern char   dtfname[PATHLEN];
extern short  *out_data[][NCOMP];
extern int    nduplic, nnew;
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

    if (argc < 2) help();

    init_cvtit(stdout);

    if (parse_arg(argc,argv) == FALSE)
    {
        help();
        exit(0);
    }

/*
 * Print out machine word order. Test is in init_cvtit()
 */
    if (opt.verb > 1 && opt.verb <= 2)
    {
        fprintf(Fp_log,"  Machine word order %s\n",
            byteswap ? "3210 (swap bytes)" : "0123 (no byte swap)");
    }


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
printf("Usage:   titinfo input [options]\n");
printf("         titinfo -f list [options]\n");
printf("\n");
printf("input:\n");
printf("    Titan-file  or  Titan-disk  or Titan-DAT tape.\n");
printf("    With a leading \"-f\", input can be a list of files and/or directories.\n");
printf("\n");
printf("main options:\n");
printf("     info=[opts] --> print selected infos: media, battery, pos...\n");
printf("    -v        --> verbose.\n");
printf("\n");
prompt();
printf("\n");
printf("Examples:\n");
printf("    cvtit /h1 info=all  or info=batt,media,pos,decim=500\n");
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
prompt();
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
prompt();
printf("bof=\n");
printf("    Start processing input Titan file at specified offset (bytes).\n");
printf("\n");
printf("eof=\n");
printf("    Stop process input Titan file past the specified offset (bytes).\n");
printf("\n");
printf("-v\n");
printf("    Set the verbose flag.\n");
printf("\n");
prompt();
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

      else if (!strncmp(argv[i], "-v", 2))
      {
          if (strlen(argv[i]) == 2) opt.verb = 1;
          else opt.verb = atoi(&argv[i][2]);
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
              return FALSE;
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
                return FALSE;
              }
          }
          opt.info.on = TRUE;
      }

      else if (!strncmp(argv[i],"-pos",  4))
      {
          opt.do_coord = TRUE;
      }
      else if (!strncmp(argv[i],"bof=",  4))
      {
          opt.beg_offset = atol(&argv[i][4]);
      }
      else if (!strncmp(argv[i],"eof=",  4))
      {
          opt.end_offset = atol(&argv[i][4]);
      }
      else
      {
            fprintf(stderr,"\n\n option '%s' not supported\n\n", argv[i]);
            return FALSE;
      }
    }

    if (opt.info.on == FALSE)
    {
        fprintf(stderr,"\n\n argument 'info=' missing\n\n");
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
    opt.verb            = FALSE;
    opt.do_offset       = 0;
    opt.beg_offset      = 0;
    opt.end_offset      = -1;
    opt.do_coord        = FALSE;
    opt.discard_short_blk = TRUE;
    opt.info.on         = FALSE;

/* Determine machine word order */

    find_wordorder(temp);
    if (!strncmp(temp, "3210", 4)) byteswap = TRUE;
    else                           byteswap = FALSE;
}


/*==================================================================*/
void end_cvtit()
{
    fprintf(Fp_log,"\n  process titan completed\n\n");
    return;
}


