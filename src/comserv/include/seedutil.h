#ifndef SEEDUTIL_H
#define SEEDUTIL_H

#include "quanstrc.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Take a C string, move it into a fixed length array, and pad
   on the right with spaces to fill the array */
  void cpadright (pchar s, pchar b, short fld);

/* Take a Pascal string, move it into a fixed length array, and pad
   on the right with spaces to fill the array */
  void padright (pchar s, pchar b, short fld);

/* Passed the address of a quanterra header, and the record size, will convert the
   header to SEED format.
*/
  double seedheader (header_type *h, seed_fixed_data_record_header *sh);

/* Converts the sequence number into a string with leading zeroes */
  void seedsequence (seed_record_header *seed, long seq);

/* given the seed name and location, returns a string in the format LL-SSS */
  pchar seednamestring (seed_name_type *sd, location_type *loc);

/* 
   sl is the buffer where the SEED header and blockette will go. log is the address
   of the commo_record that contains either a commo_event or commo_comment structure.
*/
  double seedblocks (seed_record_header *sl, commo_record *log);

#ifdef __cplusplus
}
#endif

#endif /* SEEDUTIL_H */

