/***************************************************************************** 
 * graph.c
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
#include <string.h>
#include <sys/stat.h>

#include <gd.h>

#include "tek2gif.h"
#include "palette.h"

static void sty_drawline(int x1, int y1, int x2, int y2, int color);

static gdImagePtr gd_image;

void graph_setup(void)
  {
    int c;

    if((gd_image = gdImageCreate(tek_info.max_phys_x + 1, tek_info.max_phys_y + 1)) == NULL)
      {
        fprintf(stderr, "gdImageCreate() failed\n");
        exit(1);
      }

    for(c = 0; c <256; ++c)
        gdImageColorAllocate(gd_image, 4 * default_red[c],
          4 * default_green[c], 4 * default_blue[c]);

    graph_clear();
  }

void graph_cleanup(void)
  {
    gdImageDestroy(gd_image);
  }

void graph_clear(void)
  {
    gdImageFilledRectangle(gd_image, 0, 0, tek_info.max_phys_x,
      tek_info.max_phys_y, tek_info.bg_color);
  }

void graph_clear_region(int x1, int y1, int x2, int y2)
  {
    if(x1 < tek_info.win_x1) x1 = tek_info.win_x1;
  
    if(x2 > tek_info.win_x2) x2 = tek_info.win_x2;
  
    if(y1 < tek_info.win_y1) y1 = tek_info.win_y1;
  
    if(y2 > tek_info.win_y2) y2 = tek_info.win_y2;
  
    if(make_physical_point(&x1, &y1) && make_physical_point(&x2, &y2))
        gdImageFilledRectangle(gd_image, x1, y1, x2, y2, tek_info.bg_color);
  }

void graph_drawpixel(int x, int y)
  {
    if(!make_physical_point(&x, &y)) return;

    switch(tek_info.write_mode)
      {
        case WRITE_OR:
          gdImageSetPixel(gd_image, x, y, tek_info.fg_color);
          break;

        case WRITE_CLEAR:
          gdImageSetPixel(gd_image, x, y, tek_info.bg_color);
          break;

        case WRITE_XOR:
          gdImageSetPixel(gd_image, x, y,
            tek_info.xor_color ^ gdImageGetPixel(gd_image, x, y));
          break;
      }
  }

void graph_drawline(int x1, int y1, int x2, int y2)
  {
    if(!make_physical_line(&x1, &y1, &x2, &y2)) return;

    if(tek_info.graph_mode == GRAPH_VECTORGRAPH
      && linestyle[tek_info.linestyle] != 0xffffffff)
      {
        sty_drawline(x1, y1, x2, y2, 0);
        return;
      }

    switch(tek_info.write_mode)
      {
        case WRITE_OR:
          gdImageLine(gd_image, x1, y1, x2, y2, tek_info.fg_color);
          break;

        case WRITE_CLEAR:
          gdImageLine(gd_image, x1, y1, x2, y2, tek_info.bg_color);
          break;

        case WRITE_XOR:
          sty_drawline(x1, y1, x2, y2, 0);
          break;
      }
  }

void graph_dump(void)
  {
    static int cnt = 0;
    char *filename, *ptr;
    FILE *file;
    struct stat buf;

    if(!strcmp(dumpfile, "-"))
      {
        filename = strdup("<stdout>");
        file = fdopen(STDOUT_FILENO, "w");
      }
    else
      {
        if((ptr = strchr(dumpfile, '#')) != NULL)
          {
            if((filename = malloc(strlen(dumpfile) + 4)) == NULL)
              {
                perror("malloc()");
                exit(1);
              }

            do sprintf(filename, "%.*s%03d%s", ptr - dumpfile, dumpfile, cnt, ptr + 1);
            while(++cnt < 1000 && stat(filename, &buf) >= 0);
          }
        else
          {
            if((filename = strdup(dumpfile)) == NULL)
              {
                perror("strdup()");
                exit(1);
              }
          }

        if((file = fopen(filename, "w")) == NULL)
          {
            fprintf(stderr, "failed to open %s\n", filename);
            return;
          }
      }

    DPRINTF("dump(%s)\n", filename);

    free(filename);
    gdImageGif(gd_image, file);
    fclose(file);
  }

#define ABS(a) (((a)<0) ? -(a) : (a))

static int linestylepos;

static void sty_pixel(int x, int y)
  {
    int color = 0;

    switch(tek_info.write_mode)
      {
        case WRITE_OR:
          color = tek_info.fg_color;
          break;

        case WRITE_CLEAR:
          color = tek_info.bg_color;
          break;

        case WRITE_XOR:
          color = tek_info.xor_color ^ gdImageGetPixel(gd_image, x, y);
          break;
      }

    if(tek_info.graph_mode == GRAPH_VECTORGRAPH)
      {
        if(linestyle[tek_info.linestyle] & (1 << (31 - linestylepos)))
            gdImageSetPixel(gd_image, x, y, color);

        linestylepos = (linestylepos + 1) & 0x1f;
      }
    else
        gdImageSetPixel(gd_image, x, y, color);
  }

/***************************************************************************** 
 * Taken from SVGALIB
 *
 * (c) 1993 Tommy Frandsen, Harm Hanemaayer, Hartmut Schirmer
 *****************************************************************************/

void sty_drawline(int x1, int y1, int x2, int y2, int color)
  {
    int x, y, dx, dy, ax, ay, sx, sy;


    dx = x2 - x1;
    dy = y2 - y1;
    ax = ABS(dx) << 1;
    ay = ABS(dy) << 1;
    sx = (dx >= 0) ? 1 : -1;
    sy = (dy >= 0) ? 1 : -1;

    x = x1;
    y = y1;

    linestylepos = 0;

    if(ax > ay)
      {
        int d = ay - (ax >> 1);

        while(x != x2)
          {
            sty_pixel(x, y);

            if(d > 0 || (d == 0 && sx == 1))
              {
                y += sy;
                d -= ax;
              }

            x += sx;
            d += ay;
          }
      }
    else
      {
        int d = ax - (ay >> 1);

        while(y != y2)
          {
            sty_pixel(x, y);

            if(d > 0 || (d == 0 && sy == 1))
              {
                x += sx;
                d -= ay;
              }

            y += sy;
            d += ax;
          }
      }

    sty_pixel(x, y);
  }

