/***************************************************************************** 
 * qpsetup.c
 *
 * Config file and command line processing for slqplot
 *
 * (c) 1998, 2003 Andres Heinloo, GFZ Potsdam
 * 
 * Modified: 2003.6.11 - Ported to be a SeedLink client, Chad
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>

#if defined(__GNU_LIBRARY__) || defined(__GLIBC__)
#include <getopt.h>
#endif

#include <libslink.h>

#include "slqplot.h"
#include "confparse.h"

#ifndef CONFIG_FILE
#define CONFIG_FILE  "/home/sysop/config/slqplot.ini"
#endif

#ifndef FILTER_CONFIG_FILE
#define FILTER_CONFIG_FILE  "/home/sysop/config/slqplot.coef"
#endif

const char *config_file = CONFIG_FILE;
const char *filter_config_file = FILTER_CONFIG_FILE;
const char *state_file = NULL;

int chcount = 0;
int          c_traces = 8, c_tracelen = 180, c_scroll_step = 1;
int          c_channel_mag;
int          c_verbosity = -1;
unsigned int c_flags = FL_INTERACTIVE;
char         c_channel_filter;
char         station_name[SSTNAME];
struct       c_channel_struct c_channel[NCHANS];
char         c_description[DESCLINES][DESCLINELEN];

static const char *config_section = NULL;
static const char *version_message = "slqplot v" SLQP_VERSION "\n";

#if defined(__GNU_LIBRARY__) || defined(__GLIBC__)
static const char *opterr_message = "Try `%s --help' for more information\n";
static const char *help_message = 
    "Usage: %s [options] <[address]:port>\n"
    "\n"
    "-s, --selectors               specify SeedLink selectors for uni-station mode\n"
    "-S, --streams                 specify SeedLink streams for multi-station mode\n"
    "-p, --print-packets           Write packet information to stderr\n"
    "-v                            Increase verbosity level\n"
    "    --verbosity=LEVEL         Set verbosity level\n"
    "-i, --noninteractive          Disable interactivity\n"
    "-F, --filter-config-file=FILE Alternative filter configuration file\n"
    "-f, --config-file=FILE        Alternative configuration file\n"
    "-c, --config-section=SECTION  Alternative configuration section\n"
    "-x, --state-file=FILE         Use state file\n"
    "-V, --version                 Show version information\n"
    "-h, --help                    Show this help message\n"
    "\n"
    "<[address]:port> The SeedLink server to connect to, if 'address'\n"
    "                   is omitted 'localhost' will be assumed.\n";
#else
const char *const opterr_message = "Try `%s -h' for more information\n";
const char *const help_message =
    "Usage: %s [options] <[address]:port>\n"
    "\n"
    "-s             specify SeedLink selectors for uni-station mode\n"
    "-S             specify SeedLink streams for multi-station mode\n"
    "-p             Write packet information to stderr\n"
    "-v             Increase verbosity level\n"
    "-i             Disable interactivity\n"
    "-F FILE        Alternative filter configuration file\n"
    "-f FILE        Alternative configuration file\n"
    "-c SECTION     Alternative configuration section\n"
    "-x FILE        Use state file\n"
    "-V             Show version information\n"
    "-h             Show this help message\n"
    "\n"
    "<[address]:port> The SeedLink server to connect to, if 'address'\n"
    "                   is omitted 'localhost' will be assumed.\n";
#endif

static int prm_strcpy(void *dest, char *src, int n);
static int prm_p_int(void *dest, char *src, int n);
static int prm_yesno(void *dest, char *src, int n);
static int prm_chan(void *dest, char *src, int n);

static void qpcoefsetup(void);

void qpsetup(int argc, char **argv, SLCD *slconn)
  {
    int c;
    int tmp_complete_pages = 0;
    int tmp_coloured_traces = 0;
    char *p, *progname;
    char *selectors = NULL;
    char *streams = NULL;
    static char statenv[20];
    struct cfg_struct conf[] = 
      { 
        { "tracelen",        prm_p_int, &c_tracelen,          1440, 0        },
        { "traces",          prm_p_int, &c_traces,            100,  0        },
        { "scroll_step",     prm_p_int, &c_scroll_step,       100,  0        },
        { "complete_pages",  prm_yesno, &tmp_complete_pages,  0,    0        },
        { "coloured_traces", prm_yesno, &tmp_coloured_traces, 0,    0        },
        { "desc1",           prm_strcpy, &c_description[0],   DESCLINELEN, 0 },
        { "desc2",           prm_strcpy, &c_description[1],   DESCLINELEN, 0 },
        { "channel",         prm_chan,  NULL,                 0,    CFG_SUBS },
        { "plot",            prm_plot,  NULL,                 0,    CFG_SUBS },
        { NULL }
      };

#if defined(__GNU_LIBRARY__) || defined(__GLIBC__)
    struct option ops[] = 
      {
        { "selectors",          no_argument,       NULL, 's' },
        { "streams",            no_argument,       NULL, 'S' },
        { "print-packets",      no_argument,       NULL, 'p' },
        { "noninteractive",     no_argument,       NULL, 'i' },
        { "verbosity",          required_argument, NULL, 'X' },
        { "filter-config-file", required_argument, NULL, 'F' },
        { "config-file",        required_argument, NULL, 'f' },
        { "config-section",     required_argument, NULL, 'c' },
        { "state-file",         required_argument, NULL, 'x' },
        { "version",            no_argument,       NULL, 'V' },
        { "help",               no_argument,       NULL, 'h' },
        { NULL }
      };
#endif

    for(progname = argv[0]; (p = strchr(progname, '/')) != NULL; progname = p + 1);
    
#if defined(__GNU_LIBRARY__) || defined(__GLIBC__)
    while((c = getopt_long(argc, argv, "pviF:f:c:x:Vhs:S:", ops, NULL)) != EOF)
#else
    while((c = getopt(argc, argv, "pviF:f:c:x:Vhs:S:")) != EOF)
#endif
      {
        switch(c)
          {
          case 's': P(selectors = strdup(optarg)); break;
          case 'S': P(streams = strdup(optarg)); break;
          case 'p': c_flags |= FL_SHOW_PACKETS; break;
          case 'i': c_flags &= ~FL_INTERACTIVE; break;
          case 'v': ++c_verbosity; break;
          case 'X': c_verbosity = atoi(optarg); break;
          case 'F': P(filter_config_file = strdup(optarg)); break;
          case 'f': P(config_file = strdup(optarg)); break;
          case 'c': P(config_section = strdup(optarg)); break;
          case 'x': P(state_file = strdup(optarg)); break;
          case 'V': fprintf(stdout, version_message); exit(0);
          case 'h': fprintf(stdout, help_message, progname); exit(0);
          case '?': fprintf(stderr, opterr_message, progname); exit(1);
          }
      }
 
    if(optind != argc - 1 || argv[optind][0] == '*')
      {
        fprintf(stderr, "%s: invalid syntax\n", argv[0]);
        fprintf(stderr, opterr_message, progname);
        exit(1);
      }
    
    slconn->sladdr = strdup(argv[optind]);

    /* If no host is given for the SeedLink server, add 'localhost' */
    if (*slconn->sladdr == ':')
      {
	p = (char *) malloc (strlen (slconn->sladdr) + 10);
	sprintf (p, "localhost%s", slconn->sladdr);
	slconn->sladdr = p;
      }

    if ( ! selectors && ! streams )
      {
	fprintf(stderr, "Either selectors (-s) or streams (-S) must be specified\n");
	exit(1);
      }

    if ( streams )
      {
	int scnt;

	scnt = sl_parse_streamlist (slconn, streams, selectors);
	if ( scnt < 0 )
	  {
	    fprintf(stderr, "Error parsing stream list: %s\n", streams);
	    exit(1);
	  }
	else if ( scnt != 1 )
	  {
	    fprintf(stderr, "For slqplot a single stream must be specified\n");
	    exit(1);
	  }
      }
    else
      {
	sl_setuniparams (slconn, selectors, -1, NULL);
      }

    strncpclean (station_name, slconn->streams->sta, SSTNAME - 1);

    sprintf(statenv, "STATION=%s", station_name);
    putenv(statenv);
    
    if(!config_section) config_section = progname;
    
    if((yyin = fopen(config_file, "r")) == NULL)
      {
        fprintf(stderr, "Cannot find config file %s\n", config_file);
        exit(1);
      }
    
    if(read_config(config_section, conf) < 0) 
        exit(1);

    if(tmp_complete_pages)
        c_flags |= FL_COMPLETE_PAGES;
    
    if(tmp_coloured_traces)
        c_flags |= FL_COLOURED_TRACES;
    
    if(c_scroll_step > c_traces)
        c_scroll_step = c_traces;
    
    if(!chcount)
      {
        fprintf(stderr, "No channels defined\n");
        exit(1);
      }
    
    if(!plcount)
      {
        fprintf(stderr, "No plots defined\n");
        exit(1);
      }
    
    qpcoefsetup();
  }

int prm_strcpy(void *dest, char *src, int n)
  {
    strncpy(dest, src, n - 1);
    *(char *)(dest + n - 1) = 0;
    return 0;
  }

int prm_p_int(void *dest, char *src, int n)
  {
    int arg;
    char *tail;

    arg = strtoul(src, &tail, 0);
    if(*tail) 
      {
        config_message("%s is not an integer\n", src);
        return -1;
      }

    if(arg <= 0)
      {
        config_message("%d is out of bounds, using 1 instead\n", arg);
        *(int *)dest = 1;
        return 0;
      }

    if(arg > n)
      {
        config_message("%d is out of bounds, using %d instead\n", arg, n);
        *(int *)dest = n;
        return 0;
      }

    *(int *)dest = arg;
    return 0;
  }

int prm_yesno(void *dest, char *src, int n)
  {
    if(!strncasecmp(src, "yes", n))
      {
        *(int *)dest = 1;
        return 0;
      }
    else if(!strncasecmp(src, "no", n))
      {
        *(int *)dest = 0;
        return 0;
      }

    config_message("value should be \"yes\" or \"no\"; using \"no\"\n");
    *(int *)dest = 0;
    return 0;
  }

static inline void zero_flags(struct cfg_struct *cfg)
  {
    struct cfg_struct *cfgp;
    
    for(cfgp = cfg; cfgp->param; ++cfgp) cfgp->flags = 0;
  }

int prm_chan(void *dest, char *src, int n)
  {
    static struct cfg_struct chan_parms[] =
      {
        { "location", prm_strcpy, NULL, SLOCNAME,  0 },
        { "mag",      prm_p_int,  NULL, (1 << 20), 0 },
        { "filter",   prm_strcpy, NULL, FILNAME,   0 },
        { "fmag",     prm_p_int,  NULL, (1 << 20), 0 },
        { NULL }
      };

    if(chcount == NCHANS)
      {
        config_message("maximum number of channels (%d) exceeded\n", NCHANS);
        return -1;
      }
      
    strncpy(c_channel[chcount].chname, src, SCHNAME - 1); 
    c_channel[chcount].chname[SCHNAME - 1] = 0;

    c_channel[chcount].location[0] = 0;
    c_channel[chcount].mag = DEFMAG;
    c_channel[chcount].filter.filname[0] = 0;
    c_channel[chcount].filter.fmag = 0;
    
    zero_flags(chan_parms);

    chan_parms[0].dataptr = &c_channel[chcount].location;
    chan_parms[1].dataptr = &c_channel[chcount].mag;
    chan_parms[2].dataptr = &c_channel[chcount].filter.filname;
    chan_parms[3].dataptr = &c_channel[chcount].filter.fmag;
    
    ++chcount;
    *(struct cfg_struct **)dest = chan_parms;
    
    return 0;
  }


void qpcoefsetup(void) 
  {
    int i,j;
    FILE *fp;
    char line[256];
    
    if((fp = fopen(filter_config_file, "r")) == NULL)
      {
        fprintf(stderr, "Cannot find filter config file %s\n", filter_config_file);
        exit(1);
      }
         
    for(i = 0; i <= chcount; i++)
      {
        rewind(fp);
        while(fgets(line, sizeof(line), fp))
          {
            if(line[0] != '[') continue;
                
            for(j=0; j<32 && line[j] != ']'; j++);
                
            c_channel[i].filter.nband = -1;
                    
            if(strlen(c_channel[i].filter.filname) &&
              (j-1) == strlen(c_channel[i].filter.filname) && 
              !strncasecmp(c_channel[i].filter.filname, &line[1], strlen(c_channel[i].filter.filname)))
              {
                fgets(line, sizeof(line), fp);
                if(sscanf(line,"%d",&c_channel[i].filter.nband) != 1)
                  {
                    fprintf(stderr,"\nCan't read nband in %s", filter_config_file);
                    exit(-1);
                  }
                         
                fgets(line, sizeof(line), fp);
                if(sscanf(line, "%f %f %f %f %f",&(c_channel[i].filter.w1)[0],
                                                  &(c_channel[i].filter.w1)[1],
                                                  &(c_channel[i].filter.w1)[2],
                                                  &(c_channel[i].filter.w1)[3],
                                                  &(c_channel[i].filter.w1)[4]) != 5) 
                  {
                    fprintf(stderr,"\nCan't read coef w1 in %s", filter_config_file);
                    exit(-1);
                  }
                        
                fgets(line, sizeof(line), fp);     
                if(sscanf(line, "%f %f %f",&(c_channel[i].filter.w2)[0],
                                            &(c_channel[i].filter.w2)[1],
                                            &(c_channel[i].filter.w2)[2]) != 3)
                  {
                    fprintf(stderr,"\nCan't read coef w2 in %s", filter_config_file);
                    exit(-1);  
                  }

                break;  
              }                        
          }
      }
      
    fclose(fp);
  }          

