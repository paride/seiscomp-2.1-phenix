/***************************************************************************** 
 * confparse.h
 *
 * Config file parser
 *
 * (c) 1998 Andres Heinloo, GFZ Potsdam
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

#ifndef CONFPARSE_H
#define CONFPARSE_H

#define CFG_SET     0x01
#define CFG_SUBS    0x02
#define CFG_INVALID 0x04

#define config_message(fmt,arg...) \
    fprintf(stderr, "%s:%d: " fmt, config_file, config_lineno ,##arg)

extern int config_lineno;
extern const char *config_file;
extern const char *filter_config_file; 
extern FILE *yyin;

struct slist
  {
    struct slist *next;
    char *p;
  };

struct cfg_struct
  {
    char *param;
    int (*convert)(void *dest, char *src, int n);
    void *dataptr;
    int n;
    unsigned int flags;
    struct slist *arglist;
  };

int read_config(const char *section, struct cfg_struct *cfg);

#endif /* CONFPARSE_H */
