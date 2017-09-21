/*   General purpose utility routines
     Copyright 1994-1996 Quanterra, Inc.
     Written by Woodrow H. Owens

Edit History:
   Ed Date      By  Changes
   -- --------- --- ---------------------------------------------------
    0 30 Mar 94 WHO Hacked from various other files.
    1 30 May 94 WHO str_long and long_str changed to handle right
                    justified name instead of left justified (DSN). Add
                    downshift procedure (DSN). Add "void" to str_right (DSN).
    2  9 Jun 94 WHO Cleanup to avoid warnings.
    3 28 Feb 95 WHO Start of conversion to run on OS9.
    4 17 Oct 97 WHO Add VER_STUFF
*/
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#ifndef _OSK
#include <termio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/time.h>
#else
#include <time.h>
#include <types.h>
#endif
#include "dpstruc.h"
#include "stuff.h"
#include "pascal.h"

short VER_STUFF = 4 ;

/* Return seconds (and parts of a second) since 1970 */
  double dtime (void) 
    begin
#ifndef _OSK
    struct timeval tp;
    struct timezone tzp;
      
      gettimeofday (&tp, &tzp) ;
      return ((double) tp.tv_sec + ((double) tp.tv_usec / 1000000.0)) ;
#else
#define TIMEDIFF 2440587 /* difference between 1970 and dorky OS9 julian time */
      u_int32 time, date, ticks ;
      u_int16 day ;
      
      _os9_getime(3, &time, &date, &day, &ticks) ;
      date = date - TIMEDIFF ; /* change from 4513BC to 1980 */
      return (double) date * 86400.0 + (double) time + 
           (double) (ticks and 0xffff) / (double) (ticks >> 16) ;
#endif
    end

/* Convert C string to longinteger */
  long str_long (pchar name)
    begin
      short i ;
      complong temp ;
      
      temp.l = 0x20202020 ; /* all spaces */
      for (i = 0 ; i < 4 ; i++)
        if (i < strlen(name))
          then
            temp.b[i] = toupper(name[i]) ;
      return temp.l ;
    end

/* Convert longinteger to C string */
  pchar long_str (long name)
    begin
      short i, j ;
      static short k ;
      complong temp ;
      static char out[4][5] ;
      
      temp.l = name ;
      k = (k + 1) & 3 ;
      j = 0 ;
      for (i = 0 ; i < 4 ; i++)
        if (temp.b[i] != ' ')
          then
            out[k][j++] = temp.b[i] ;
      out[k][j] = '\0' ;
      return out[k] ;
    end
    
/* Convert pascal string to C string */
  void strpcopy (pchar outstring, pchar instring)
    begin
      short i ;
      
      for (i = 0 ; i < instring[0] ; i++)
        outstring[i] = instring[i + 1] ;
      outstring[i] = '\0' ;
    end

/* Convert C string to Pascal string */
  void strpas (pchar outstring, pchar instring)
    begin
      short i ;
      
      i = 0 ;
      while (instring[i])
        outstring[i + 1] = instring[i++] ;
      outstring[0] = i ;
    end

/* Set the bit in the mask pointed to by the first parameter */
  void set_bit (long *mask, short bit)
    begin
      *mask = *mask or (1 << (long)bit) ;
    end

/* Clear the bit in the mask pointed to by the first parameter */
  void clr_bit (long *mask, short bit)
    begin
      *mask = *mask and (not(1 << (long)bit)) ;
    end

/* Returns TRUE if the bit in set in the mask */
  boolean test_bit (long mask, short bit)
    begin
      return ((mask and (1 << (long)bit)) != 0) ;
    end

/* remove trailing spaces and control characters from a C string */
  void untrail (pchar s)
    begin
      while ((s[0] != '\0') land (s[strlen(s)-1] <= ' '))
        s[strlen(s)-1] = '\0' ;
    end
    
/* upshift a C string */
  void upshift (pchar s)
    begin
      short i ;
      
      for (i = 0 ; i < strlen(s) ; i++)
        s[i] = toupper (s[i]) ;
    end

/* downshift a C string */
  void downshift (pchar s)
    begin
      short i ;
      
      for (i = 0 ; i < strlen(s) ; i++)
        s[i] = tolower (s[i]) ;
    end

/* add a directory separator slash to the end of a C string if there isn't one */
  void addslash (pchar s)
    begin
      if ((s[0] != '\0') land (s[strlen(s)-1] != '/'))
        then
          strcat(s, "/") ;
    end

/* Start at ptr+1 and copy characters into dest up to and including the
   terminator */
  void str_right (pchar dest, pchar ptr)
    begin
      do
        *(dest++) = *(++ptr) ;
      while (*ptr != '\0') ;
    end

/* Return longinteger representation of a byte, making sure it is not sign extended */
  long longhex (byte b)
    begin
      return ((long) b) and 255 ;
    end

/* Return integer representation of a byte, making sure it is not sign extended */
  short ord (byte b)
    begin
      return ((short) b) and 255 ;
    end

