#ifndef _ah_h
#define _ah_h

#include "ahhead.h"
#include <rpc/rpc.h>
#include <rpc/xdr.h>

#ifdef mem_alloc 
#undef mem_alloc
#endif

typedef struct {
    FILE* fp;
    XDR  xdr;
} AHFILE;

#endif
