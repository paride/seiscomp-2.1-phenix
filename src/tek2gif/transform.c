/***************************************************************************** 
 * transform.c
 *
 * (c) 1996, 2002 Andres Heinloo, GFZ Potsdam
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

#include "tek2gif.h"

int make_physical_line(int *x1, int *y1, int *x2, int *y2)
  {
    int ptr = 0, lx[2], ly[2];

    if(*x1 >= tek_info.win_x1 && *x1 <= tek_info.win_x2
      && *y1 >= tek_info.win_y1 && *y1 <= tek_info.win_y2)
      {
        lx[ptr] = *x1;
        ly[ptr] = *y1;
        ++ptr;
      }

    if(*x2 >= tek_info.win_x1 && *x2 <= tek_info.win_x2
      && *y2 >= tek_info.win_y1 && *y2 <= tek_info.win_y2)
      {
        lx[ptr] = *x2;
        ly[ptr] = *y2;
        ++ptr;
      }

    if(ptr < 2 && *x1 != *x2)
      {
        lx[ptr] = tek_info.win_x1;
        ly[ptr] = (lx[ptr] - *x1) * (*y2 - *y1) / (*x2 - *x1) + *y1;
        if(ly[ptr] >= tek_info.win_y1 && ly[ptr] <= tek_info.win_y2
          && !((*x1 < lx[ptr] && *x2 < lx[ptr])
          || (*x1 > lx[ptr] && *x2 > lx[ptr])))
            ++ptr;
      }

    if(ptr < 2 && *x1 != *x2)
      {
        lx[ptr] = tek_info.win_x2;
        ly[ptr] = (lx[ptr] - *x1) * (*y2 - *y1) / (*x2 - *x1) + *y1;
        if(ly[ptr] >= tek_info.win_y1 && ly[ptr] <= tek_info.win_y2
          && !((*x1 < lx[ptr] && *x2 < lx[ptr])
          || (*x1 > lx[ptr] && *x2 > lx[ptr])))
            ++ptr;
      }

    if(ptr < 2 && *y1 != *y2)
      {
        ly[ptr] = tek_info.win_y1;
        lx[ptr] = (ly[ptr] - *y1) * (*x2 - *x1) / (*y2 - *y1) + *x1;
        if(lx[ptr] >= tek_info.win_x1 && lx[ptr] <= tek_info.win_x2
          && !((*y1 < ly[ptr] && *y2 < ly[ptr])
          || (*y1 > ly[ptr] && *y2 > ly[ptr])))
            ++ptr;
      }

    if(ptr < 2 && *y1 != *y2)
      {
        ly[ptr] = tek_info.win_y2;
        lx[ptr] = (ly[ptr] - *y1) * (*x2 - *x1) / (*y2 - *y1) + *x1;
        if(lx[ptr] >= tek_info.win_x1 && lx[ptr] <= tek_info.win_x2
          && !((*y1 < ly[ptr] && *y2 < ly[ptr])
          || (*y1 > ly[ptr] && *y2 > ly[ptr])))
            ++ptr;
      }

    if(ptr < 2) return 0;

    *x1 = lx[0];
    *y1 = ly[0];
    *x2 = lx[1];
    *y2 = ly[1];
    make_physical_point(x1, y1);
    make_physical_point(x2, y2);

    return 1;
  }

int make_physical_point(int *x, int *y)
  {
    if(*x < tek_info.win_x1 || *x > tek_info.win_x2 ||
      *y < tek_info.win_y1 || *y > tek_info.win_y2)
        return 0;

    *x = (*x - tek_info.win_x1) * tek_info.max_phys_x /
      (tek_info.win_x2 - tek_info.win_x1);
    
    *y = (tek_info.win_y2 - *y) * tek_info.max_phys_y /
      (tek_info.win_y2 - tek_info.win_y1);

    return 1;
  }

