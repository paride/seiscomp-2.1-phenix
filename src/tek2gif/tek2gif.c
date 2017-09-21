/***************************************************************************** 
 * tek2gif.c
 *
 * (c) 1996, 2002 Andres Heinloo, GFZ Potsdam
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/ioctl.h>

#if defined(__GNU_LIBRARY__) || defined(__GLIBC__)
#include <getopt.h>
#endif

#include "tek2gif.h"
#include "settings.h"

#define VERSION "1.0 (2005.212)"

#define HANDLER_BUFLEN 1024

struct modeinfo tek_info =
  {
    DEFAULT_FG_COLOR,
    DEFAULT_BG_COLOR,
    0,
    TEKMODE_ANSI,
    TEKMODE_ANSI,
    GRAPH_ALPHA,
    WRITE_OR,
    DEFAULT_LINESTYLE,
    DEFAULT_FONT,
    DEFAULT_FLAGS,
    0,
    DEFAULT_WINDOW,
    DEFAULT_ABOUNDS,
    DEFAULT_MAX_PHYS_X,
    DEFAULT_MAX_PHYS_Y,
    0,0,
    0,0,
    0,0
  };

struct font_params fontpar[5] =
  {
    { FONTPARAMS_SIZE1 },
    { FONTPARAMS_SIZE2 },
    { FONTPARAMS_SIZE3 },
    { FONTPARAMS_SIZE4 },
    { FONTPARAMS_USER }
};

unsigned long linestyle[6] =
  {
    LINESTYLE_SOLID,
    LINESTYLE_DOT,
    LINESTYLE_DADOT,
    LINESTYLE_SDASH,
    LINESTYLE_LDASH,
    LINESTYLE_USER
  };

#ifdef DEBUG
static FILE *debug;
#endif

static const char *const version = "tek2gif v" VERSION;

#if defined(__GNU_LIBRARY__) || defined(__GLIBC__)
static const char *const usage =
  "Usage: tekemu [options] [input_file] [output_file]\n"
  "output_file may contain a # for multi-page output\n"
  "\n"
  "Supported options:\n"
  "-r <X>x<Y>      Set graphic resolution\n"
  "-fg <color>     Set foreground color\n"
  "-bg <color>     Set background color\n"
  "   Color can be given as a numeric value (0..255) or one of the strings:\n"
  "   Black, Blue, Green, Cyan, Red, Purple, Brown, Grey, LightGrey, LightBlue,\n"
  "   LightGreen, LightCyan, LightRed, LightPurple, Yellow, White\n"
  "-t              Allow text output (if output_file != stdout)\n"
  "-g              Start in graphics mode\n"
  "-c              Enable \"clear screen\" command\n"
#ifdef DEBUG
  "-d <file>       Send debug information to file\n"
#endif
  "-V              Show version information\n";
#else
static const char *const usage =
  "Usage: tekemu [options] [input_file] [output_file]\n"
  "output_file may contain a # for multi-page output\n"
  "\n"
  "Supported options:\n"
  "-r <X>x<Y>      Set graphic resolution\n"
  "-F <color>      Set foreground color\n"
  "-B <color>      Set background color\n"
  "   Color can be given as a numeric value (0..255) or one of the strings:\n"
  "   Black, Blue, Green, Cyan, Red, Purple, Brown, Grey, LightGrey, LightBlue,\n"
  "   LightGreen, LightCyan, LightRed, LightPurple, Yellow, White\n"
  "-t              Allow text output (if output_file != stdout)\n"
  "-g              Start in graphics mode\n"
  "-c              Enable \"clear screen\" command\n"
#ifdef DEBUG
  "-d <file>       Send debug information to file\n"
#endif
  "-V              Show version information\n";
#endif

static void parse_reso(const char *str, int *x, int *y)
  {
    if(sscanf(str, "%ux%u%*c", x, y) != 2 || *x > 4096 || *y > 4096)
      {
        fprintf(stderr, "invalid resolution: %s\n", str);
        exit(1);
      }

    --(*x);
    --(*y);
  }

static int parse_color(const char *str)
  {
    static char *colors[] =
      {
        "Black", "Blue", "Green", "Cyan", "Red", "Purple", "Brown",
        "Grey", "LightGrey", "LightBlue", "LightGreen", "LightCyan",
        "LightRed", "LightPurple", "Yellow", "White"
      };

    int n;
    char *tail;

    for(n = 0; n <= sizeof(colors) / sizeof(char *) - 1; ++n)
        if(!strcasecmp(str, colors[n])) return n;
  
    n = strtol(str, &tail, 10);
    if(strlen(tail) != 0 || n < 0 || n > 255)
      {
        fprintf(stderr, "invalid color: %s\n", str);
        exit(1);
      }
    return n;
  }

const char *dumpfile;
int progflags;

int main(int argc, char **argv)
  {
    int c, in_fd, handler_pos, bytes_read, bytes_written;
    char handler_buf[HANDLER_BUFLEN + 1];  /* terminating 0 ! */
#ifdef DEBUG
    char *debugfile = NULL;
#endif
#if defined(__GNU_LIBRARY__) || defined(__GLIBC__)
    struct option op[] =
      {
        { "fg", required_argument, NULL, 'F' },
        { "bg", required_argument, NULL, 'B' },
        { NULL, 0, NULL, 0 }
      };
#endif

    opterr = FALSE;
#if defined(__GNU_LIBRARY__) || defined(__GLIBC__)
    while((c = getopt_long_only(argc, argv, "d:ctgr:V", op, NULL)) != EOF)
#else
    while((c = getopt(argc, argv, "d:ctgr:F:B:V")) != EOF)
#endif
      {
        switch(c)
          {
#ifdef DEBUG
          case 'd':
            progflags |= PF_DEBUG;
            debugfile = optarg;
            break;
#endif
          case 'c':
            progflags |= PF_CLEAR;
            break;
            
          case 't':
            progflags |= PF_TEXT;
            break;
            
          case 'g':
            tek_info.tek_mode = TEKMODE_ALPHA;
            break;
            
          case 'r':
            parse_reso(optarg, &tek_info.max_phys_x, &tek_info.max_phys_y);
            break;
            
          case 'F':
            tek_info.fg_color = parse_color(optarg);
            break;
            
          case 'B':
            tek_info.bg_color = parse_color(optarg);
            break;
            
          case 'V':
            fprintf(stderr, "%s\n", version);
            exit(0);
            
          default:
            fprintf(stderr, "%s\n", usage);
            exit(1);
          }
      }

    in_fd = STDIN_FILENO;
    dumpfile = "-";
    
    if(optind == argc - 2)
      {
        if(strchr(argv[argc - 1], '#')) progflags |= PF_MULTI_PAGE;
        dumpfile = argv[argc - 1];
        --argc;
      }
          
    if(optind == argc - 1)
      {
        if(strcmp(argv[argc - 1], "-") &&
          (in_fd = open(argv[argc - 1], O_RDONLY)) < 0)
          {
            fprintf(stderr, "could not open file '%s'\n", argv[argc - 1]);
            exit(1);
          }

        --argc;
      }

    if(optind != argc)
      {
        fputs(usage, stderr);
        exit(1);
      }

    tek_info.xor_color = tek_info.fg_color ^ tek_info.bg_color;

#ifdef DEBUG
    if(progflags & PF_DEBUG)
      if((debug = fopen(debugfile, "w")) == NULL)
        {
          fprintf(stderr, "%s: %s\n", debugfile, strerror(errno));
          exit(1);
        }
#endif

    graph_setup();
    handler_setup();

    handler_pos = 0;
    
    while(1)
      {
        if((bytes_read = read(in_fd, &handler_buf[handler_pos],
          HANDLER_BUFLEN - handler_pos)) < 0)
          {
            perror("read");
            exit(1);
          }

        if(bytes_read == 0) break;

        handler_pos += bytes_read;
        handler_buf[handler_pos] = 0;

        if((bytes_written = tek_handler_entry(handler_buf, handler_pos)) > 0)
          {
            memmove(handler_buf, &handler_buf[bytes_written],
              handler_pos - bytes_written);
            handler_pos -= bytes_written;
          }

        if(handler_pos == HANDLER_BUFLEN)
          {
            fprintf(stderr, "internal error\n");
            exit(1);
          }
      }

    graph_dump();
    graph_cleanup();
    return 0;
  }

int scrsend(const char *buffer, int len)
  {
    if((progflags & PF_TEXT) && (write(STDOUT_FILENO, buffer, len) < len))
      {
        perror("write");
        exit(1);
      }

    return len;
  }

#ifdef DEBUG
int debug_printf(const char *fmt, ...)
  {
    va_list argptr;
    int retval;

    if(progflags & PF_DEBUG)
      {
        va_start(argptr, fmt);
        if((retval = vfprintf(debug, fmt, argptr)) < 0)
          {
            perror("vfprintf");
            exit(1);
          }
          
        return retval;
      }

    return 0;
  }
#endif

