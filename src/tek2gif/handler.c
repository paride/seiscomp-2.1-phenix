/***************************************************************************** 
 * handler.c
 *
 * (c) 1996, 2002 Andres Heinloo, GFZ Potsdam
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <sys/time.h>

#include "tek2gif.h"
#include "settings.h"
#include "font.h"

#define TEK_DRIVERS 7

static char *tek_sequence[] =
  {
    "\0332", "\030", "\0331",
    "\037", "\034", "\035",
    "\037", "\034", "\035",
    "\036", "\033\032", "\033\030", "\033O", "\033L",
    "\033\\", "\033e", "\033f", "\033g",
    "\033a", "\033b", "\033c", "\033d", "\033x",
    "\033\014", "\007", "\033\005", "\033\027",
    " ", "\010", "\011", "\012", "\013", "\015",
    "\0338", "\0339", "\033:", "\033;"
  };

static struct converter_state cs;
static struct submode_driver tekdriver[TEK_DRIVERS];

static int driver_write(struct submode_driver *drv, char *seq[],
  const char *buffer, int len);
static void tek_command(int command);
static void native_command(char command, int x1, int y1, int x2, int y2);
static int ansi_handler(const char *dataptr, int len);
static int graph_handler(const char *dataptr, int len);
static int alpha_handler(const char *dataptr, int len);
static int iplot_handler(const char *dataptr, int len);
static int bypass_handler(const char *dataptr, int len);
static int native_handler(const char *dataptr, int len);
static int define_linestyle(const char *dataptr, int len);

static int convert(char src, int *x, int *y);
static void convreset(void);
static void alpha_adjust(void);
static void alpha_move(int direction);
static void alpha_clear_cell(void);

void handler_setup(void)
  {
    static int ansi_commands[] =
      {
        TEK_GRAPHICS, TEK_GR_ALPHA, TEK_GR_POINTPLOT, TEK_GR_VECTORGRAPH, -1
      };

    static int graph_commands[] =
      {
        TEK_ANSI, TEK_ANSI_ALT, TEK_ALPHA, TEK_GRAPHICS, TEK_DEFINE_LINESTYLE,
        TEK_POINTPLOT, TEK_VECTORGRAPH, TEK_IPLOT, TEK_CROSSHAIR, TEK_BYPASS,
        TEK_NATIVE, TEK_GIN, TEK_PRINTSCREEN, TEK_LINE_SOLID, TEK_LINE_SOLID1,
        TEK_LINE_SOLID2, TEK_LINE_SOLID3, TEK_LINE_DOT, TEK_LINE_DASHDOT,
        TEK_LINE_SDASH, TEK_LINE_LDASH, TEK_LINE_USER, TEK_ERASE_SCREEN,
        TEK_BEEP, ALPHA_SIZE1, ALPHA_SIZE2, ALPHA_SIZE3, ALPHA_SIZE4, -1
      };

    static int alpha_commands[] =
      {
        TEK_ANSI, TEK_ANSI_ALT, TEK_ALPHA, TEK_GRAPHICS, TEK_DEFINE_LINESTYLE,
        TEK_POINTPLOT, TEK_VECTORGRAPH, TEK_IPLOT, TEK_CROSSHAIR, TEK_BYPASS,
        TEK_NATIVE, TEK_GIN, TEK_PRINTSCREEN, TEK_LINE_SOLID, TEK_LINE_SOLID1,
        TEK_LINE_SOLID2, TEK_LINE_SOLID3, TEK_LINE_DOT, TEK_LINE_DASHDOT,
        TEK_LINE_SDASH, TEK_LINE_LDASH, TEK_LINE_USER, TEK_ERASE_SCREEN,
        TEK_BEEP, ALPHA_SIZE1, ALPHA_SIZE2, ALPHA_SIZE3, ALPHA_SIZE4,
        ALPHA_SPACE, ALPHA_RIGHT, ALPHA_LEFT, ALPHA_DOWN, ALPHA_UP, ALPHA_CR,
        -1
      };

    static int no_commands[] = { -1 };

    tekdriver[TEKMODE_ANSI].c_list = ansi_commands;
    tekdriver[TEKMODE_ANSI].c_handler = tek_command;
    tekdriver[TEKMODE_ANSI].d_handler = ansi_handler;

    tekdriver[TEKMODE_GRAPHICS].c_list = graph_commands;
    tekdriver[TEKMODE_GRAPHICS].c_handler = tek_command;
    tekdriver[TEKMODE_GRAPHICS].d_handler = graph_handler;

    tekdriver[TEKMODE_ALPHA].c_list = alpha_commands;
    tekdriver[TEKMODE_ALPHA].c_handler = tek_command;
    tekdriver[TEKMODE_ALPHA].d_handler = alpha_handler;

    tekdriver[TEKMODE_IPLOT].c_list = graph_commands;
    tekdriver[TEKMODE_IPLOT].c_handler = tek_command;
    tekdriver[TEKMODE_IPLOT].d_handler = iplot_handler;

    tekdriver[TEKMODE_BYPASS].c_list = no_commands;
    tekdriver[TEKMODE_BYPASS].c_handler = NULL;
    tekdriver[TEKMODE_BYPASS].d_handler = bypass_handler;

    tekdriver[TEKMODE_NATIVE].c_list = no_commands;
    tekdriver[TEKMODE_NATIVE].c_handler = NULL;
    tekdriver[TEKMODE_NATIVE].d_handler = native_handler;

    tekdriver[TEKMODE_DEFINE_LINESTYLE].c_list = no_commands;
    tekdriver[TEKMODE_DEFINE_LINESTYLE].c_handler = NULL;
    tekdriver[TEKMODE_DEFINE_LINESTYLE].d_handler = define_linestyle;

    alpha_adjust();
  }

int tek_handler_entry(const char *buffer, int len)
  {
    int handler_result, bytes_handled = 0;

    while(bytes_handled < len)
      {
        if((handler_result = driver_write(&tekdriver[tek_info.tek_mode],
          tek_sequence, &buffer[bytes_handled], len - bytes_handled)) == 0)
          break;
        
        bytes_handled += handler_result;
      }
    
    return bytes_handled;
  }

int driver_write(struct submode_driver *drv, char *seq[], const char *buffer,
  int len)
  {
    int c_count, bytes_to_compare, bytes_handled = 0;
    char inconsistent;

    for(c_count = 0; drv->c_list[c_count] != -1; ++c_count)
      {
        bytes_to_compare = strlen(seq[drv->c_list[c_count]]);
      
        if(len - bytes_handled < bytes_to_compare)
          {
            inconsistent = TRUE;
            bytes_to_compare = len - bytes_handled;
          }
        else
          {
            inconsistent = FALSE;
          }

        if(!strncmp(&buffer[bytes_handled], seq[drv->c_list[c_count]],
          bytes_to_compare))
          {
            if(!inconsistent)
              {
                (*drv->c_handler)(drv->c_list[c_count]);
                bytes_handled += bytes_to_compare;
              }

            break;
          }
      }

    if(drv->c_list[c_count] == -1)
      {
        bytes_handled += (*drv->d_handler)(&buffer[bytes_handled],
          len - bytes_handled);
      }

    return bytes_handled;
  }

void tek_command(int command)
  {
    DPRINTF("tek_command(%d)  [tek_mode=%d, graph_mode=%d]\n", command,
      tek_info.tek_mode, tek_info.graph_mode);

    switch(command)
      {
      case TEK_ANSI_ALT:
      case TEK_ANSI:
        tek_info.tek_mode = TEKMODE_ANSI;
        tek_info.graph_mode = GRAPH_ALPHA;
        tek_info.write_mode = WRITE_OR;
        tek_info.linestyle = 0;
        break;
        
      case TEK_GRAPHICS:
        tek_info.tek_mode = TEKMODE_GRAPHICS;
        
        if(tek_info.graph_mode == GRAPH_ALPHA)
          {
            tek_command(TEK_ALPHA);
          }
          
        break;
        
      case TEK_ALPHA:
        tek_info.tek_mode = TEKMODE_ALPHA;
        tek_info.graph_mode = GRAPH_ALPHA;
        alpha_adjust();
        break;
        
      case TEK_IPLOT:
        tek_info.tek_mode = TEKMODE_IPLOT;
        tek_info.graph_mode = GRAPH_IPLOT;
        tek_info.status &= ~ST_IPLOT_PENDOWN;
        break;
        
      case TEK_NATIVE:
        tek_info.tek_mode_save = tek_info.tek_mode;
        tek_info.tek_mode = TEKMODE_NATIVE;
        break;
        
      case TEK_GIN:
      case TEK_CROSSHAIR:
      case TEK_BYPASS:
        tek_info.tek_mode_save = tek_info.tek_mode;
        tek_info.tek_mode = TEKMODE_BYPASS;
        break;
        
      case TEK_DEFINE_LINESTYLE:
        tek_info.tek_mode_save = tek_info.tek_mode;
        tek_info.tek_mode = TEKMODE_DEFINE_LINESTYLE;
        break;
        
      case TEK_POINTPLOT:
        tek_info.tek_mode = TEKMODE_GRAPHICS;
        tek_info.graph_mode = GRAPH_POINTPLOT;
        break;
        
      case TEK_VECTORGRAPH:
        tek_info.tek_mode = TEKMODE_GRAPHICS;
        tek_info.graph_mode = GRAPH_VECTORGRAPH;
        tek_info.status &= ~ST_HAVESENTLINE;
        break;
        
      case TEK_GR_ALPHA:
        tek_info.tek_mode = TEKMODE_GRAPHICS;
        tek_command(TEK_ALPHA);
        break;
        
      case TEK_GR_POINTPLOT:
        tek_info.tek_mode = TEKMODE_GRAPHICS;
        tek_command(TEK_POINTPLOT);
        break;
        
      case TEK_GR_VECTORGRAPH:
        tek_info.tek_mode = TEKMODE_GRAPHICS;
        tek_command(TEK_VECTORGRAPH);
        break;
        
      case TEK_PRINTSCREEN:
        if(progflags & PF_MULTI_PAGE) graph_dump();
        
        break;
        
      case TEK_LINE_SOLID:
      case TEK_LINE_SOLID1:
      case TEK_LINE_SOLID2:
      case TEK_LINE_SOLID3:
        tek_info.linestyle = 0;
        break;
        
      case TEK_LINE_DOT:
        tek_info.linestyle = 1;
        break;
        
      case TEK_LINE_DASHDOT:
        tek_info.linestyle = 2;
        break;
        
      case TEK_LINE_SDASH:
        tek_info.linestyle = 3;
        break;
        
      case TEK_LINE_LDASH:
        tek_info.linestyle = 4;
        break;
        
      case TEK_LINE_USER:
        tek_info.linestyle = 5;
        break;
        
      case TEK_ERASE_SCREEN:
        tek_info.status &= ~ST_HAVESENTPOINT;
        tek_info.status &= ~ST_HAVESENTALPHA;

        if(progflags & PF_CLEAR)
          {
            if(progflags & PF_MULTI_PAGE) graph_dump();
            graph_clear();
          }
          
        tek_command(TEK_ALPHA);
        break;
        
      case TEK_BEEP:
        scrsend("\007", 1);
        break;
        
      case ALPHA_SPACE:
        if(tek_info.flags & F_SPACE_CLEAR)
          alpha_clear_cell();
          
      case ALPHA_RIGHT:
        alpha_move(RIGHT);
        break;
        
      case ALPHA_LEFT:
        alpha_move(LEFT);
        break;
        
      case ALPHA_DOWN:
        alpha_move(DOWN);
        break;
        
      case ALPHA_UP:
        alpha_move(UP);
        break;
        
      case ALPHA_CR:
        alpha_move(CR);
        break;
        
      case ALPHA_SIZE1:
        tek_info.font = 0;
        break;
        
      case ALPHA_SIZE2:
        tek_info.font = 1;
        break;
        
      case ALPHA_SIZE3:
        tek_info.font = 2;
        break;
        
      case ALPHA_SIZE4:
        tek_info.font = 3;
        break;
      }
  }

void native_command(char command, int x1, int y1, int x2, int y2)
  {
    DPRINTF("native_command(%c) [x1=%d, y1=%d, x2=%d, y2=%d]\n", command, x1,
      y1, x2, y2);

    switch(command)
      {
      case SET_WRITE_MODE:
        if(x1 >= 0 && x1 <= 2) tek_info.write_mode = x1;
        break;
        
      case SET_FONT:
        tek_info.font = 4;               /* User-defined font */
        if(x1 == 0 || y1 == 0) break;    /* Select predefined values */
        fontpar[tek_info.font].xmag = x1;
        fontpar[tek_info.font].ymag = y1;
        fontpar[tek_info.font].hspace = x2;
        fontpar[tek_info.font].vspace = y2;
        break;
        
      case SET_ALPHA_PAGE_BOUNDARIES:
        tek_info.abound_x1 = x1;
        tek_info.abound_y1 = y1;
        tek_info.abound_x2 = x2;
        tek_info.abound_y2 = y2;
        alpha_adjust();
        break;
        
      case SET_FGCOLOR:
        tek_info.fg_color = x1 & 0xff;
        break;
        
      case SET_BGCOLOR:
        tek_info.bg_color = x1 & 0xff;
        tek_info.status &= ~ST_HAVESENTPOINT;
        tek_info.status &= ~ST_HAVESENTALPHA;
        graph_clear();
        tek_command(TEK_ALPHA);
        break;
        
      case SET_XORCOLOR:
        if(x1) tek_info.xor_color = x1 & 0xff;
        else tek_info.xor_color = tek_info.fg_color ^ tek_info.bg_color;
        
        break;
        
      case SET_FLAGS:
        if(y1 == 1) tek_info.flags |= x1;
        else if(y1 == 0) tek_info.flags &= ~x1;
        else tek_info.flags = x1;
        
        break;
        
      case SET_WINDOW:
        if(x2 > x1 && y2 > y1)
          {
            tek_info.win_x1 = x1;
            tek_info.win_x2 = x2;
            tek_info.win_y1 = y1;
            tek_info.win_y2 = y2;
            tek_info.status &= ~ST_HAVECROSSHAIR;
          }
      }
  }

int ansi_handler(const char *dataptr, int len)
  {
    int textlen = 0;

    while((unsigned char)(dataptr[++textlen]) >= 32);

    scrsend(dataptr, textlen);
    return textlen;
  }

int alpha_handler(const char *dataptr, int len)
  {
    int x1 = 0, y1 = 0, x2 = 0, y2 = 0, writemode_save, penpos, offset = 0;
    unsigned char pen;

    DPRINTF("alpha_handler()\n");
    writemode_save = tek_info.write_mode;

    if(tek_info.write_mode == WRITE_XOR) tek_info.write_mode = WRITE_OR;

    while(offset < len)
      {
        if(dataptr[offset] < 33 || dataptr[offset] > 126)
          {
            if(!offset)
              {
                DPRINTF("skipping unexpected code %d\n", dataptr[0]);
                ++offset;
                continue;
              }

            break;
          }

        DPRINTF("'%c'", dataptr[offset]);
        penpos = 0;
        while((pen = alpha_font[dataptr[offset] - 33][penpos]) != 0)
          {
            x1 = x2;
            y1 = y2;
            x2 = tek_info.alpha_x + ((pen >> 3) & 7) * fontpar[tek_info.font].xmag;
            y2 = tek_info.alpha_y + (pen & 7) * fontpar[tek_info.font].ymag;
            DPRINTF("%c(%d,%d)", (pen & 0x80) ? ' ' : '-', x2, y2);
            if(!(pen & 0x80)) graph_drawline(x1, y1, x2, y2);
            else graph_drawpixel(x2, y2);
            ++penpos;
          }

        DPRINTF(" ");
        alpha_move(RIGHT);
        ++offset;
      }

    tek_info.write_mode = writemode_save;
    return offset;
  }

void alpha_adjust(void)
  {
    DPRINTF("alpha_adjust()\n");

    if(!(tek_info.status & ST_HAVESENTPOINT)
      && !(tek_info.status & ST_HAVESENTALPHA))
      {
        alpha_move(HOME);
        tek_info.status |= ST_HAVESENTALPHA;
        return;
      }

    if(tek_info.alpha_x < tek_info.abound_x1)
      tek_info.alpha_x = tek_info.abound_x1;

    if(tek_info.alpha_x + CHARWIDTH * fontpar[tek_info.font].xmag >
      tek_info.abound_x2)
      {
        tek_info.alpha_x = tek_info.abound_x1;
        alpha_move(DOWN);
      }

    if(tek_info.alpha_y < tek_info.abound_y1)
        tek_info.alpha_y = tek_info.abound_y1;

    if(tek_info.alpha_y + CHARHEIGHT * fontpar[tek_info.font].ymag >
      tek_info.abound_y2)
        tek_info.alpha_y = tek_info.abound_y2 - CHARHEIGHT * fontpar[tek_info.font].ymag;
  }

void alpha_move(int direction)
  {
    DPRINTF("alpha_move(%d)\n", direction);

    switch(direction)
      {
      case HOME:
        tek_info.alpha_x = tek_info.abound_x1;
        tek_info.alpha_y = tek_info.abound_y2 - CHARHEIGHT * fontpar[tek_info.font].ymag;
        break;
        
      case CR:
        tek_info.alpha_x = tek_info.abound_x1;
        break;
        
      case DOWN:
        tek_info.alpha_y -= CHARHEIGHT * fontpar[tek_info.font].ymag +
          fontpar[tek_info.font].vspace;

        if(tek_info.alpha_y < tek_info.abound_y1)
          {
            if(tek_info.flags & F_PAGE_FULL_CLEAR)
                tek_command(TEK_ERASE_SCREEN);

            if(tek_info.flags & F_NLCR) alpha_move(CR);

            tek_info.alpha_y = tek_info.abound_y2 - CHARHEIGHT * fontpar[tek_info.font].ymag;
          }

        break;
        
      case UP:
        tek_info.alpha_y += CHARHEIGHT * fontpar[tek_info.font].ymag +
          fontpar[tek_info.font].vspace;

        if(tek_info.alpha_y + CHARHEIGHT * fontpar[tek_info.font].ymag >
          tek_info.abound_y2)
            tek_info.alpha_y = tek_info.abound_y2 - CHARHEIGHT * fontpar[tek_info.font].ymag;

        break;
        
      case RIGHT:
        tek_info.alpha_x += CHARWIDTH * fontpar[tek_info.font].xmag +
          fontpar[tek_info.font].hspace;

        if(tek_info.alpha_x + CHARWIDTH * fontpar[tek_info.font].xmag >
          tek_info.abound_x2)
          {
            tek_info.alpha_x = tek_info.abound_x1;
            alpha_move(DOWN);
          }

        break;

      case LEFT:
        tek_info.alpha_x -= CHARWIDTH * fontpar[tek_info.font].xmag +
          fontpar[tek_info.font].hspace;

        if(tek_info.alpha_x < tek_info.abound_x1)
            tek_info.alpha_x = tek_info.abound_x2 - CHARWIDTH * fontpar[tek_info.font].xmag;

        break;
      }
  }

void alpha_clear_cell()
  {
    int x1, y1, x2, y2;

    x1 = tek_info.alpha_x - fontpar[tek_info.font].hspace / 2;
    y1 = tek_info.alpha_y - fontpar[tek_info.font].vspace / 2;
    x2 = tek_info.alpha_x + fontpar[tek_info.font].hspace / 2 +
      CHARWIDTH * fontpar[tek_info.font].xmag;
    y2 = tek_info.alpha_y + fontpar[tek_info.font].vspace / 2 +
      CHARHEIGHT * fontpar[tek_info.font].ymag;
    graph_clear_region(x1, y1, x2, y2);
  }

int iplot_handler(const char *dataptr, int len)
  {
    int offset = 0, writemode_save;

    DPRINTF("iplot_handler()\n");
    writemode_save = tek_info.write_mode;
    if(tek_info.write_mode == WRITE_XOR)
      tek_info.write_mode = WRITE_OR;

    while(offset < len)
      {
        switch(dataptr[offset])
          {
          case 32:
            tek_info.status &= ~ST_IPLOT_PENDOWN;
            break;
            
          case 'P':
            tek_info.status |= ST_IPLOT_PENDOWN;
            break;
            
          case 'A':
            ++tek_info.iplot_x;
            break;
            
          case 'B':
            --tek_info.iplot_x;
            break;
            
          case 'D':
            ++tek_info.iplot_y;
            break;
            
          case 'E':
            ++tek_info.iplot_y;
            ++tek_info.iplot_x;
            break;
            
          case 'F':
            ++tek_info.iplot_y;
            --tek_info.iplot_x;
            break;
            
          case 'H':
            --tek_info.iplot_y;
            break;
            
          case 'I':
            --tek_info.iplot_y;
            ++tek_info.iplot_x;
            break;
            
          case 'J':
            --tek_info.iplot_y;
            --tek_info.iplot_x;
            break;
            
          default:
            if(offset)
              {
                tek_info.write_mode = writemode_save;
                return offset;
              }
            else
              {
                DPRINTF("skipping unexpected code %d\n", dataptr[0]);
                ++offset;
                continue;
              }
          }

        if(tek_info.iplot_x < tek_info.win_x1)
          {
            tek_info.iplot_x = tek_info.win_x1;
            ++offset;
            continue;
          }
          
        if(tek_info.iplot_x > tek_info.win_x2)
          {
            tek_info.iplot_x = tek_info.win_x2;
            ++offset;
            continue;
          }
          
        if(tek_info.iplot_y < tek_info.win_y1)
          {
            tek_info.iplot_y = tek_info.win_y1;
            ++offset;
            continue;
          }
          
        if(tek_info.iplot_y > tek_info.win_y2)
          {
            tek_info.iplot_y = tek_info.win_y2;
            ++offset;
            continue;
          }
          
        tek_info.alpha_x = tek_info.iplot_x;
        tek_info.alpha_y = tek_info.iplot_y;

        if(tek_info.status & ST_IPLOT_PENDOWN)
          graph_drawpixel(tek_info.iplot_x, tek_info.iplot_y);

        ++offset;
      }

    tek_info.write_mode = writemode_save;
    return offset;
  }

int bypass_handler(const char *dataptr, int len)
  {
    char *ptr;

    DPRINTF("bypass_handler()\n");

    if((ptr = strpbrk(dataptr, "\007\012\015\030\033\034\035\036\037")) != NULL)
      {
        tek_info.tek_mode = tek_info.tek_mode_save;
        DPRINTF("discarded %d bytes\n", ptr - dataptr);
        return (ptr - dataptr);
      }

    DPRINTF("discarded %d bytes\n", len);
    return len;
  }

int graph_handler(const char *dataptr, int len)
  {
    int offset = 0, x, y;

    DPRINTF("graph_handler()\n");

    do
      {
        while(1)
          {
            if(len == offset) return offset;

            if(dataptr[offset] < 32)
              {
                if(offset)
                  {
                    DPRINTF("unexpected code %d, exiting graph_handler()\n",
                      dataptr[offset]);
                    return offset;
                  }

                DPRINTF("skipping unexpected code %d\n", dataptr[0]);
                ++offset;
                continue;
              }

            if(convert(dataptr[offset++], &x, &y)) break;
          }

        if(tek_info.graph_mode == GRAPH_POINTPLOT)
          {
            DPRINTF("(%d,%d) ", x, y);
            graph_drawpixel(x, y);
          }
        else
          {
            DPRINTF("%s(%d,%d)", (!(tek_info.status & ST_HAVESENTLINE)) ? "" : "-", x, y);
          
            if(tek_info.status & ST_HAVESENTLINE)
                graph_drawline(tek_info.graph_x, tek_info.graph_y, x, y);
            tek_info.status |= ST_HAVESENTLINE;
          }

        tek_info.graph_x = tek_info.alpha_x = tek_info.iplot_x = x;
        tek_info.graph_y = tek_info.alpha_y = tek_info.iplot_y = y;
        tek_info.status |= ST_HAVESENTPOINT;
      }
    while(dataptr[offset] >= 32);

    DPRINTF("\n");
    return offset;
  }

int native_handler(const char *dataptr, int len)
  {
    int offset = 0;
    static int x[2], y[2], cnt = 0, command = 0;
    struct converter_state cs_save;

    DPRINTF("native_handler()\n");

    if(command == 0)
      {
        command = dataptr[offset++];
        cs_save = cs;  /* save current converter state and reset converter */
        convreset();
      }

    for(; cnt <= 1; ++cnt)
      while(1)
        {
          if(len == offset) return offset;

          if(dataptr[offset] < 32)
            {
              DPRINTF("unexpected code %d, exiting native_handler()\n",
                dataptr[offset]);
              tek_info.tek_mode = tek_info.tek_mode_save;
              return offset;
            }

          if(convert(dataptr[offset++], &x[cnt], &y[cnt])) break;
        }

    native_command(command, x[0], y[0], x[1], y[1]);
    command = 0;
    cnt = 0;
    cs = cs_save;      /* restore converter state */
    tek_info.tek_mode = tek_info.tek_mode_save;
    return offset;
  }

int convert(char src, int *x, int *y)
  {
    if((src & 32) && (src & 64))
      {
        if(cs.dataidx > 1) return 0;
        cs.dataidx = 1;
        if(cs.havesentloy) cs.extra = cs.data[cs.dataidx];
        cs.havesentloy = TRUE;
      }
    else if(src & 32)
      {
        if(cs.dataidx > 2) return 0;
        if(cs.dataidx > 0) cs.dataidx = 2;
      }
    else if(src & 64)
      {
        cs.dataidx = 3;
      }

    cs.data[cs.dataidx] = src & 31;
    if(cs.dataidx == 3)
      {
        *x = (cs.data[2] << 7) + (cs.data[3] << 2) + (cs.extra & 0x03);
        *y = (cs.data[0] << 7) + (cs.data[1] << 2) + ((cs.extra >> 2) & 0x03);
        cs.havesentloy = FALSE;
        cs.dataidx = 0;
        return 1;
      }
    return 0;
  }

void convreset(void)
  {
    memset(&cs, 0, sizeof(cs));
  }

int define_linestyle(const char *dataptr, int len)
  {
    int offset;
    unsigned long pattern = 0;

    DPRINTF("define_linestyle(");
    if(len < 8)
      {
        DPRINTF(")\n");
        return 0;
      }

    for(offset = 0; offset <= 7; ++offset)
      {
        if(dataptr[offset] < '0' || dataptr[offset] > '?')
          {
            DPRINTF(")\nunexpected code %d, exiting define_linestyle()\n",
              dataptr[offset]);
            tek_info.tek_mode = tek_info.tek_mode_save;
            return offset;
          }
        pattern = (pattern << 4) + (dataptr[offset] - '0');
      }

    linestyle[5] = pattern;
    tek_info.tek_mode = tek_info.tek_mode_save;

    DPRINTF("%x)\n", pattern);
    return 8;
  }

