/***************************************************************************** 
 * qpgraph.c
 *
 * GNU libplot interface for slqplot
 *
 * (c) 1998, 2003 Andres Heinloo, GFZ Potsdam
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include <plot.h>

#ifdef LIBPLOT_VERSION
  #include <plotcompat.h>
#endif
#ifdef PL_LIBPLOT_VER
  #include <plotcompat.h>
#endif

#include "slqplot.h"
#include "confparse.h"

#define NA  (char *)(-1)

#define PLF_OPEN       0x01
#define PLF_MULTIPART  0x02
#define PLF_PIPE       0x04
#define PLF_NOSTREAM   0x08
#define PLF_LANDSCAPE  0x10

int plcount = 0;

enum plparms 
  { 
    PP_DISPLAY, PP_TERM, PP_FILE, PP_FG, PP_BG, PP_SIZE, PP_PAPER, 
    PP_ORIENTATION, PP_SCALE, PP_OFFSET,
    NPLPARMS 
  };

enum plotters 
  { 
    PL_X, PL_PS, PL_HPGL, PL_TEK, PL_META, 
    NPLOTTERS 
  };

static char *plparmstr[NPLPARMS] =
  { 
    "display", "term", "file", "fg", "bg", "size", "paper", "orientation", 
    "scale", "offset"
  };

static char *plstr[NPLOTTERS] = 
  {
    "x", "ps", "hpgl", "tek", "meta"
  };

static char *default_plparm[NPLOTTERS][NPLPARMS] = 
  { 
    { NULL, NA,   NA,     "black", "white", "1024x682", NA,   "portrait",  NA,      NA        },
    { NA,   NA,   "|lpr", "black", "white", NA,         "a4", "landscape", "1.5;1", "-1250;0" },
    { NA,   NA,   "|lpr", "black", "white", NA,         "a4", "landscape", "1.5;1", "-1250;0" },
    { NA,   NULL, NULL,   "black", "white", NA,         NA,   "portrait",  "1.4;1", "-1060;0" },
    { NA,   NA,   "qp*",  "black", "white", NA,         NA,   "portrait",  "1;1",   "0;0"     }
  };

struct c_plot_struct
  {
    int type;
    int pd;
    double xscale, yscale;
    double xoffset, yoffset;
    char *parm[NPLPARMS];
    unsigned int flags;
  };

static struct c_plot_struct c_plot[NPLOTS];

static int prm_setpl(void *dest, char *src, int n);

static void setplparm(int i, char *s, enum plparms n) 
  { 
    if(c_plot[i].parm[n] == NA)
      {
        if(parampl(s, "") < 0)
            fprintf(stderr, "Error setting parameter %s to \"\"\n", s);
        return;
      }
    
    if(parampl(s, c_plot[i].parm[n]) < 0)
        fprintf(stderr, "Error setting parameter %s to %s\n", s, 
            c_plot[i].parm[n]);
  }

static FILE *outfp = NULL;

FILE *_outfile(FILE *fp)
  {
    outfp = fp;
    return outfile(fp);
  }

void _setcolor(int n, const char *name)
  {
    if(c_plot[n].type == PL_TEK)
      {
        FILE* fp = outfp;

        flushpl();
        
        if(outfp == NULL)
          {
            fp = stdout;
          }

        /* this works only with the tek2gif program */
        
        if(!strcmp(name, "red"))                     /* color code 39 */
            fprintf(fp, "\x1b\x4f\x61\x20\x63\x60\x20\x49\x40");
        else if(!strcmp(name, "blue"))               /* color code 32 */
            fprintf(fp, "\x1b\x4f\x61\x20\x60\x60\x20\x48\x40");
        else                                         /* color code 0  */
            fprintf(fp, "\x1b\x4f\x61\x20\x60\x60\x20\x40\x40");
      }
    else
      {
        colorname(name);
      }
  }
            
static void setup_plotter(int i)
  {
    space(0, 0, XDISPSIZE, YDISPSIZE);
    
    if(c_plot[i].flags & PLF_LANDSCAPE)
      {
        fconcat(0, (double)YDISPSIZE/(double)XDISPSIZE, 
            -(double)XDISPSIZE/(double)YDISPSIZE, 0, XDISPSIZE, 0); 
      }

    fscale(c_plot[i].xscale, c_plot[i].yscale);
    ftranslate(c_plot[i].xoffset, c_plot[i].yoffset);
    bgcolorname(c_plot[i].parm[PP_BG]);
    colorname(c_plot[i].parm[PP_FG]);
    linewidth(1);
    // fontname(FNTNAME);
    // fontsize(FNTSIZE);
    erase();
  }

void create_plotters(void)
  {
    int i;
    FILE *fp = NULL;
    
    parampl("VANISH_ON_DELETE", "yes");
        
    for(i = 0; i < plcount; ++i)
      {
        setplparm(i, "DISPLAY", PP_DISPLAY);
        setplparm(i, "TERM", PP_TERM);
        setplparm(i, "BG_COLOR", PP_BG);
        setplparm(i, "BITMAPSIZE", PP_SIZE);
        setplparm(i, "PAGESIZE", PP_PAPER);
        
        if(c_plot[i].type == PL_X) c_plot[i].flags |= PLF_NOSTREAM;

        if(c_plot[i].parm[PP_ORIENTATION] && 
            c_plot[i].parm[PP_ORIENTATION] != NA)
          {
            if(!strcasecmp(c_plot[i].parm[PP_ORIENTATION], "landscape"))
              {
                c_plot[i].flags |= PLF_LANDSCAPE;
              }
            else if(strcasecmp(c_plot[i].parm[PP_ORIENTATION], "portrait"))
              {
                fprintf(stderr, "Unrecognized orientation: `%s', using "
                    "`portrait' instead\n", c_plot[i].parm[PP_ORIENTATION]);
              }
          }
        
        if(c_plot[i].parm[PP_SCALE] && c_plot[i].parm[PP_SCALE] != NA)
          {
            if(sscanf(c_plot[i].parm[PP_SCALE], "%lf;%lf",
                &c_plot[i].xscale, &c_plot[i].yscale) != 2)
              {
                fprintf(stderr, "Unrecognized scale: `%s', using "
                    "`1;1' instead\n", c_plot[i].parm[PP_SCALE]);
                c_plot[i].xscale = 1;
                c_plot[i].yscale = 1;
              }
          }
        else
          {
            c_plot[i].xscale = 1;
            c_plot[i].yscale = 1;
          }

        if(c_plot[i].parm[PP_OFFSET] && c_plot[i].parm[PP_OFFSET] != NA)
          {
            if(sscanf(c_plot[i].parm[PP_OFFSET], "%lf;%lf",
                &c_plot[i].xoffset, &c_plot[i].yoffset) != 2)
              {
                fprintf(stderr, "Unrecognized offset: `%s', using "
                    "`0;0' instead\n", c_plot[i].parm[PP_OFFSET]);
                c_plot[i].xoffset = 0;
                c_plot[i].yoffset = 0;
              }
          }
        else
          {
            c_plot[i].xoffset = 0;
            c_plot[i].yoffset = 0;
          }
        
        if(c_plot[i].parm[PP_FILE] && c_plot[i].parm[PP_FILE] != NA)
          {
            if(strchr(c_plot[i].parm[PP_FILE], '*')) 
                c_plot[i].flags |= PLF_MULTIPART;

            if(c_plot[i].parm[PP_FILE][0] == '|')
                c_plot[i].flags |= PLF_PIPE;

            if(c_plot[i].flags & (PLF_MULTIPART | PLF_PIPE))
              {
                fp = NULL;
              }
            else if((fp = fopen(c_plot[i].parm[PP_FILE], "w")) == NULL)
              {
                fprintf(stderr, "Cannot open %s for writing\n", 
                    c_plot[i].parm[PP_FILE]);
                exit(1);
              }
          }
        else if(!c_plot[i].parm[PP_FILE])
          {
            fp = stdout;
          }
        else
          {
            fp = NULL;
          }

        if((c_plot[i].pd = newpl(plstr[c_plot[i].type], NULL, fp, stderr)) < 0)
          {
            fprintf(stderr, "Error creating plotter %s\n", 
                plstr[c_plot[i].type]);
            exit(1);
          }
      }
  }
        
void destroy_plotters(void)
  {
    int i;

    for(i = 0; i < plcount; ++i)
      {
        if(c_plot[i].flags & PLF_OPEN)
          {
            selectpl(c_plot[i].pd);
            closepl();
            c_plot[i].flags &= ~PLF_OPEN;
          }
        selectpl(0);
        deletepl(c_plot[i].pd);
      }
  }

void flush_plotters(void)
  {
    int i;

    for(i = 0; i < plcount; ++i)
      {
        selectpl(c_plot[i].pd);
        flushpl();
      }
  }

void closepage(void)
  {
    int i;
    FILE *fp = NULL;

    for(i = 0; i < plcount; ++i)
      {
        selectpl(c_plot[i].pd);
        closepl();

        c_plot[i].flags &= ~PLF_OPEN;

        if((c_plot[i].flags & PLF_MULTIPART) &&
          (~c_plot[i].flags & PLF_PIPE))
          {
            if((fp = _outfile(NULL)) != NULL) fclose(fp);
          }
        else if(c_plot[i].flags & PLF_PIPE)
          {
            if((fp = _outfile(NULL)) != NULL) pclose(fp);
          }
      }
  }

void newpage(const char *page_id)
  {
    int i;
    char str[256], *ast, *p;
    FILE *fp = NULL;

    for(i = 0; i < plcount; ++i)
      {
        selectpl(c_plot[i].pd);
        p = c_plot[i].parm[PP_FILE];
        
        if(c_plot[i].flags & PLF_OPEN) 
          {
            if(c_plot[i].flags & PLF_NOSTREAM)
              {
                erase();
              }
            else
              {
                closepl();

                c_plot[i].flags &= ~PLF_OPEN;

                if((c_plot[i].flags & PLF_MULTIPART) &&
                  (~c_plot[i].flags & PLF_PIPE))
                  {
                    if((fp = _outfile(NULL)) != NULL) fclose(fp);
                  }
                else if(c_plot[i].flags & PLF_PIPE)
                  {
                    if((fp = _outfile(NULL)) != NULL) pclose(fp);
                  }
              }
          }
        
        if(c_plot[i].flags & PLF_MULTIPART)
          {
            P(ast = strchr(p, '*'));
            snprintf(str, 256, "%.*s%s%s", ast - p, p, page_id, ast + 1);
            p = str;
            
            if((~c_plot[i].flags & PLF_PIPE))
              {
                if((fp = fopen(p, "w")) == NULL)
                  {
                    fprintf(stderr, "Cannot open %s for writing\n", p);
                  }
                
                _outfile(fp);
              }
          }

        if(c_plot[i].flags & PLF_PIPE)
          {
            ++p;
            if((fp = popen(p, "w")) == NULL)
              {
                fprintf(stderr, "Cannot execute %s\n", p);
              }
            
            _outfile(fp);
          }
                
        if(~c_plot[i].flags & PLF_OPEN)
          {
            openpl();
            c_plot[i].flags |= PLF_OPEN;
            setup_plotter(i);
          }           
      }
  }
        
void moveto(double x, double y)
  {
    int i;

    for(i = 0; i < plcount; ++i)
      {
        selectpl(c_plot[i].pd);
        fmove(x, YDISPSIZE - y);
      }
  }

void lineto(double x, double y)
  {
    int i;

    for(i = 0; i < plcount; ++i)
      {
        selectpl(c_plot[i].pd);
        fcont(x, YDISPSIZE - y);
      }
  }

void htext(double x, double y, int h, int v, const char *fname, int fsize,
  const char *s)
  {
    int i;

    for(i = 0; i < plcount; ++i)
      {
        selectpl(c_plot[i].pd);
        fontname(fname);
        fontsize(fsize);
        fmove(x, YDISPSIZE - y);
        textangle(0);
        alabel(h, v, s);
      }
  }

void vtext(double x, double y, int h, int v, const char *fname, int fsize,
  const char *s)
  {
    int i;

    for(i = 0; i < plcount; ++i)
      {
        selectpl(c_plot[i].pd);
        fontname(fname);
        fontsize(fsize);
        fmove(x, YDISPSIZE - y);
        textangle(90);
        alabel(h, v, s);
      }
  }

void setcolor(const char *name)
  {
    int i;

    for(i = 0; i < plcount; ++i)
      {
        selectpl(c_plot[i].pd);
        _setcolor(i, name);
      }
  }

int prm_plot(void *dest, char *src, int n)
  {
    int i;
    static struct cfg_struct plot_parms[NPLPARMS + 1]; 

    if(plcount == NPLOTS)
      {
        config_message("maximum number of plots (%d) exceeded\n", NPLOTS);
        return -1;
      }

    for(i = 0; i < NPLOTTERS; ++i)
      {
        if(!strcasecmp(plstr[i], src)) break;
      }
    
    if(i == NPLOTTERS)
      {
        config_message("unknown plot type: %s\n", src);
        return -1;
      }

    c_plot[plcount].type = i;
    
    for(i = 0; i < NPLPARMS; ++i)
      {
        c_plot[plcount].parm[i] = default_plparm[c_plot[plcount].type][i];
        plot_parms[i].param = plparmstr[i];
        plot_parms[i].convert = prm_setpl;
        plot_parms[i].n = i;
        plot_parms[i].flags = 0;
      }
    
    ++plcount;
    *(struct cfg_struct **)dest = plot_parms;
    return 0;
  }

int prm_setpl(void *dest, char *src, int n)
  {
    if(c_plot[plcount - 1].parm[n] == NA)
      {
        config_message("parameter %s is not applicable to plot type %s\n",
            plparmstr[n], plstr[n]);
        return -1;
      }
    
    if((c_plot[plcount - 1].parm[n] = strdup(src)) == NULL)
      {
        config_message("memory allocation error\n");
        return -1;
      }
    return 0;
  }

