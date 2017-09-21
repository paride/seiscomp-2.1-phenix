/***************************************************************************** 
 * confparse.lex
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

%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "confparse.h"

#define P(arg) do { if((arg) == NULL) { perror(#arg); exit(1); } } while(0)

int config_lineno;

static int section_exists = 0;
static const char *section_name;
static struct cfg_struct *cfg_main, *cfg_current;

static void subsection(const char *paramstr, char *argument);
static void store(const char *paramstr, char *valuestr);

%}

%option noyywrap nounput nolex-compat noreject nointeractive 
%x SKIP SECTION DATA PARAM EQUALS QVALUE VALUE ARGUMENT RECOVERY

%%

%{

#define SKIPTO(state) do { nextstate = (state); BEGIN(SKIP); } while(0)
#define RECOVER(state) do { nextstate = (state); BEGIN(RECOVERY); } while(0)

int nextstate = 0, section_match = 0;
char *paramstr = NULL;

config_lineno = 1;

%}

[^\[\n]+        /* eat up bytes to `[' or to the end of line */
\n              ++config_lineno;
\[[ \t]*        BEGIN(SECTION);

<SKIP>{
[ \t]+          /* eat up whitespace */
\n              ++config_lineno;
^\*[^\n]*       /* comment */
^#[^\n]*        /* comment */
.               BEGIN(nextstate); yyless(0);
}

<SECTION>{
[^ \t\n\[\]]+   {
                section_match = 0;
                if(!strcasecmp(yytext, section_name))
                  {
                    if(section_exists)
                      {
                        config_message("duplicated section `%s'\n", section_name);
                        BEGIN(INITIAL);
                      }
                    section_exists = 1;
                    section_match = 1;
                  }
                }                

[ \t]*\]        if(section_match) SKIPTO(DATA); else BEGIN(INITIAL);
}

<DATA>{
\[              BEGIN(INITIAL); yyless(0);  /* end of section */
.               SKIPTO(PARAM); yyless(0);
}

<PARAM>{
[^ \t\n=]+      P(paramstr = strdup(yytext)); BEGIN(EQUALS);
}

<EQUALS>{
[ \t]*=[ \t]*\" BEGIN(QVALUE);
[ \t]*=[ \t]*   BEGIN(VALUE);
[ \t]+          BEGIN(ARGUMENT);
}

<QVALUE>{
.*[^\\]/\"      store(paramstr, yytext); free(paramstr);
\"              SKIPTO(DATA);
}

<VALUE>{
[^ \t\n=]+      store(paramstr, yytext); free(paramstr); SKIPTO(DATA);
}

<ARGUMENT>{
[^ \t\n=]+      subsection(paramstr, yytext); free(paramstr); SKIPTO(DATA);
}

<SECTION>{
.|\n            config_message("parse error\n"); RECOVER(INITIAL); yyless(0);
}

<PARAM,EQUALS,QVALUE,VALUE,ARGUMENT>{
.|\n            config_message("parse error\n"); RECOVER(DATA); yyless(0);
}

<RECOVERY>{
[^\n]+          /* eat up bytes to the end of line */
\n              SKIPTO(nextstate); yyless(0); 
}

<INITIAL,SKIP>{
<<EOF>>         yyterminate();
}

<<EOF>>         config_message("unexpected eof\n"); yyterminate();

%%

void subsection(const char *paramstr, char *argument)
  {
    struct cfg_struct *cd;
    struct slist *arg;

    for(cd = cfg_main; cd->param; ++cd)
      {
        if(!strcasecmp(paramstr, cd->param) && (cd->flags & CFG_SUBS)) break;
      }
        
    if(!cd->param)
      {
        config_message("parameter `%s' is not used\n", paramstr);
        return;
      }
    
    for(arg = cd->arglist; arg; arg = arg->next)
      {
/*
        if(!strcasecmp(argument, arg->p))
          {
            config_message("duplicated argument `%s'\n", argument);
            cfg_current = NULL;   * discard whole subsection *
            return;
          }
*/
      }
        
    if((*cd->convert)(&cfg_current, argument, cd->n) < 0)
      {
        cfg_current = NULL;       /* discard whole subsection */
        return;
      }
    
    P(arg = malloc(sizeof(struct slist)));
    P(arg->p = strdup(argument));
    arg->next = cd->arglist;
    cd->arglist = arg;
  }  

void store(const char *paramstr, char *valuestr)
  {
    struct cfg_struct *cd;

    if(!cfg_current) return;
    
    for(cd = cfg_current; cd->param; ++cd)
      {
        if(!strcasecmp(paramstr, cd->param) && (~cd->flags & CFG_SUBS)) break;
      }

    if(!cd->param)
      {
        config_message("parameter `%s' is not used\n", paramstr);
        return;
      }
      
    if(cd->flags & CFG_SET)
      {
        config_message("duplicated parameter `%s'\n", cd->param);
        return;
      }

    if((*cd->convert)(cd->dataptr, valuestr, cd->n) < 0)
      {
        cd->flags |= CFG_INVALID;
        return;
      }
          
    cd->flags |= CFG_SET;
  }
        
int read_config(const char *section, struct cfg_struct *cfg)
  {
    section_name = section;
    cfg_main = cfg;
    cfg_current = cfg;
    yylex();
    
    if(!section_exists)
      {
        fprintf(stderr, "Section `%s' doesn't exist in file `%s'\n",
            section, config_file);
        return -1;
      }

    return 0;
  }

