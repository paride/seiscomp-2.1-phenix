/***************************************************************************** 
 * tek2gif.h
 *
 * (c) 1996, 2002 Andres Heinloo, GFZ Potsdam
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/time.h>

#define PF_DEBUG      0x01
#define PF_CLEAR      0x02
#define PF_TEXT       0x04
#define PF_MULTI_PAGE 0x08

#define ST_HAVESENTPOINT 0x01
#define ST_HAVESENTLINE  0x02
#define ST_HAVESENTALPHA 0x04
#define ST_HAVECROSSHAIR 0x08
#define ST_IPLOT_PENDOWN 0x10

#define TRUE  1
#define FALSE 0

#ifdef DEBUG
#define DPRINTF(fmt,arg...) debug_printf(fmt,##arg)
#else
#define DPRINTF(fmt,arg...) do { } while (0)
#endif

enum alpha_motions { UP, DOWN, LEFT, RIGHT, CR, HOME };

struct modeinfo
  {
    int fg_color;
    int bg_color;
    int xor_color;
    int tek_mode;
    int tek_mode_save;
    int graph_mode;
    int write_mode;
    int linestyle;
    int font;
    int flags;
    int status;
    int win_x1, win_y1, win_x2, win_y2;
    int abound_x1, abound_y1, abound_x2, abound_y2;
    int max_phys_x, max_phys_y;
    int graph_x, graph_y;
    int alpha_x, alpha_y;
    int iplot_x, iplot_y;
  };

struct submode_driver
  {
    int *c_list;
    void (*c_handler) (int command);
    int (*d_handler) (const char *dataptr, int len);
  };

struct font_params
  {
    int xmag, ymag, hspace, vspace;
  };

struct converter_state
  {
    unsigned char data[4];
    unsigned char extra;
    char havesentloy;
    int dataidx;
  };

enum tek_modes
  {
    TEKMODE_ANSI,
    TEKMODE_GRAPHICS,
    TEKMODE_ALPHA,
    TEKMODE_IPLOT,
    TEKMODE_BYPASS,
    TEKMODE_NATIVE,
    TEKMODE_DEFINE_LINESTYLE
  };

enum tek_commands
  {
    TEK_ANSI, TEK_ANSI_ALT, TEK_GRAPHICS,
    TEK_ALPHA, TEK_POINTPLOT, TEK_VECTORGRAPH,
    TEK_GR_ALPHA, TEK_GR_POINTPLOT, TEK_GR_VECTORGRAPH,
    TEK_IPLOT, TEK_CROSSHAIR, TEK_BYPASS, TEK_NATIVE, TEK_DEFINE_LINESTYLE,
    TEK_LINE_SOLID, TEK_LINE_SOLID1, TEK_LINE_SOLID2, TEK_LINE_SOLID3,
    TEK_LINE_DOT, TEK_LINE_DASHDOT, TEK_LINE_SDASH, TEK_LINE_LDASH,
    TEK_LINE_USER, TEK_ERASE_SCREEN, TEK_BEEP, TEK_GIN, TEK_PRINTSCREEN,
    ALPHA_SPACE, ALPHA_LEFT, ALPHA_RIGHT, ALPHA_DOWN, ALPHA_UP, ALPHA_CR,
    ALPHA_SIZE1, ALPHA_SIZE2, ALPHA_SIZE3, ALPHA_SIZE4
  };

enum native_commands
  {
    SET_WRITE_MODE = 'W',
    SET_FGCOLOR = 'a',
    SET_BGCOLOR = 'b',
    SET_XORCOLOR = 'c',
    SET_FONT = 'd',
    SET_FLAGS = 'e',
    SET_WINDOW = 'f',
    SET_ALPHA_PAGE_BOUNDARIES = 'g'
  };

enum graph_modes
  {
    GRAPH_ALPHA,
    GRAPH_POINTPLOT,
    GRAPH_VECTORGRAPH,
    GRAPH_IPLOT
  };

enum write_modes
  {
    WRITE_OR,
    WRITE_XOR,
    WRITE_CLEAR
  };

enum tek_flags
  {
    F_PAGE_FULL_CLEAR = 0x01,
    F_NLCR = 0x02,
    F_SPACE_CLEAR = 0x04

/* not used in tek2gif
    F_ALPHA_CURSOR = 0x08,
    F_GIN_TERMINATOR = 0x10,
    F_GIN_TERMINATOR_EOT = 0x20,
    F_PRINT_KEY = 0x40
*/
  };

  /* tek2gif.c */

extern int progflags;
extern const char *dumpfile;
extern struct modeinfo tek_info;
extern struct font_params fontpar[5];
extern unsigned long linestyle[6];
#if DEBUG
int debug_printf(const char *fmt, ...);
#endif
int scrsend(const char *buffer, int len);

  /* handler.c */

void handler_setup(void);
int tek_handler_entry(const char *buffer, int len);

  /* graph.c */

void graph_setup(void);
void graph_cleanup(void);
void graph_clear(void);
void graph_clear_region(int x1, int y1, int x2, int y2);
void graph_drawpixel(int x, int y);
void graph_drawline(int x1, int y1, int x2, int y2);
void graph_dump(void);

  /* transform.c */

int make_physical_line(int *x1, int *y1, int *x2, int *y2);
int make_physical_point(int *x, int *y);

