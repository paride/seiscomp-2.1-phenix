/*====================================================================
 *    writegif.c
 *    Handles writing of GIF files. Based on flgife.c and flgifc.c
 *    from the FBM Library, by Michael Maudlin
 *    Contains: 
 *    WriteGIF(fp, pic, w, h, rmap, gmap, bmap, numcols, colorstyle)
 *
 *    Note: slightly brain-damaged, in that it'll only write
 *    non-interlaced GIF files (in the interests of speed, or something)
 *
 Updates
    compress(): inside probe:
    if ((long)htab[i] > 0)  changed for
    if ((long)htab[i] >= 0), as in xv source code xvgifwr.c 
 ====================================================================*/
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <values.h>
#include <malloc.h>

typedef unsigned char BYTE;
#define BW  1
/* MONO returns total intensity of r,g,b components: 0.33R + 0.5G + 0.17B */
#define MONO(rd,gn,bl) (((rd)*11 + (gn)*16 + (bl)*5) >> 5)

static void putword();
static void compress();
static void output();
static void cl_block();
static void cl_hash();
static void char_init();
static void char_out();
static void flush_char();

extern int DEBUG;

static int    Width, Height;
static int    Interlace;
static BYTE   bw[2] = {0, 0xff};


/*===================================================================*/
int WriteGIF(fp, pic, w, h, inrmap, ingmap, inbmap, numcols, colorstyle)
FILE  *fp;
BYTE  *pic;
int    w,h;
int   *inrmap, *ingmap, *inbmap;
int    numcols, colorstyle;
{
int    RWidth, RHeight;
int    Width, Height;
int    LeftOfs, TopOfs;
int    Resolution, colorMapSize, InitCodeSize, Background, BitsPerPixel;
int    Interlace;
BYTE   bw[2] = {0, 0xff};
BYTE   *rmap, *gmap, *bmap;
BYTE   bigmap[768];
int    i,j;

/* Here we can overwrite pic_data with a w*h decompressed image from disk */

if (0)
{
FILE *Fp;
  printf("======= READING 'pic' file from disk\n");
  if (!(Fp=fopen("pic", "r")))
  {
      fprintf(stderr,"can't open 'pic'\n");
  }
  else
      fread(pic, 1, w*h, Fp); 
}
/*
  for (i=0; i<10000; i++) pic[i] =0;
*/

  DEBUG = 0;

  if (!fp)
  {
    fprintf(stderr, "WriteGIF: file not open for writing\n");
    return (1);
  }

/* If writing B/W stipple... */

  if (colorstyle == BW)
  {
    rmap = gmap = bmap = bw;
    numcols = 2;
  }

/* Full colors */
  else
  {
     rmap = bigmap;
     gmap = &bigmap[256];
     bmap = &bigmap[512];

     for (i=0; i<numcols; i++) 
     {
	rmap[i] = inrmap[i];
	gmap[i] = ingmap[i];
	bmap[i] = inbmap[i];
     }  
  }

  Interlace = 0;
  Background = 0;

/* Figure out 'BitsPerPixel' */
  for (i=1; i<8; i++)
    if ( (1<<i) >= numcols) break;
  
  BitsPerPixel = i;
  colorMapSize = 1 << BitsPerPixel;
	
  RWidth  = Width  = w;
  RHeight = Height = h;
  LeftOfs = TopOfs = 0;
  Resolution = BitsPerPixel;

  if (BitsPerPixel <= 1) InitCodeSize = 2;
  else                   InitCodeSize = BitsPerPixel;

  if (DEBUG) {
      printf("WriteGIF: picture size %dx%d, %d bits per pixel, ",
	    w, h, BitsPerPixel);
      printf("num of colors=%d, %s\n",
            numcols, colorstyle ? "black/white" : "full colors");
  }

/* The GIF magic number */
  fwrite("GIF87a", 1, 6, fp);

/* Screen descriptor */
  putword(RWidth,  fp);
  putword(RHeight, fp);

/* Yes, there is a color map */
  i = 0x80;
/* OR in the color resolution (hardwired 8)*/
  i |= (8-1)<<4;
/* OR in the # of bits per pixel */
  i |= (BitsPerPixel - 1);
  fputc(i,fp);

/* Background color */
  fputc(Background, fp);

/* Future expansion byte */
  fputc(0, fp);

/* Greyscale */
  if (colorstyle == BW)
  {
    for (i=0; i<colorMapSize; i++)
    {
      j = MONO(rmap[i], gmap[i], bmap[i]);
      if (DEBUG) printf("color num %d r=%3d g=%3d b=%3d -> %3d\n",
                     i, rmap[i], gmap[i], bmap[i], j);
      fputc(j, fp);
      fputc(j, fp);
      fputc(j, fp);
    }
  }
/* Write out Global colormap */
  else
  {
    for (i=0; i<colorMapSize; i++)
    {
      fputc(rmap[i], fp);
      fputc(gmap[i], fp);
      fputc(bmap[i], fp);
    }
  }

/* Image separator */
  fputc(',', fp);

/* Write the Image header */
  putword(LeftOfs, fp);
  putword(TopOfs,  fp);
  putword(Width,   fp);
  putword(Height,  fp);

/* Use Global Colormap, maybe Interlace */
  if (Interlace) fputc(0x40, fp);
  else           fputc(0x00, fp);

  fputc(InitCodeSize, fp);

  compress(InitCodeSize+1, fp, pic, w*h);

/* Write out a Zero-length packet (EOF) */
  fputc(0,fp);

/* Write GIF file terminator */
  fputc(';',fp);
  return (0);
}


#define min(a,b)      ((a>b) ? b : a)
#define NBITS	12   
#define HSIZE  5003                 /* 80% occupancy (hash table???) */

static int n_bits;                  /* number of bits/code */
static int maxbits = NBITS;         /* user settable max # bits/code */
static int maxcode;                 /* maximum code, given n_bits */
static int maxmaxcode = 1 << NBITS; /* NEVER generate this */

#define MAXCODE(n_bits)     ((1 << (n_bits)) - 1)

static  unsigned long    cur_accum = 0;
static  int              cur_bits = 0;
static  long             htab [HSIZE];
static  unsigned short   codetab [HSIZE];

static int hsize = HSIZE;        /* for dynamic table sizing */

/*
 * To save much memory, we overlay the table used by compress() with
 * those used by decompress().
 * The tab_prefix table is the same size and type as the codetab.
 * The tab_suffix table needs 2**NBITS characters.
 * We get this from the beginning of htab. The output stack uses the
 * rest of htab, and contains characters.
 * There is plenty of room for any possible stack (stack used to be
 * 8000 characters).
 */

static int free_ent = 0;         /* first unused entry */

/*
 * block compression parameters -- after all codes are used up,
 * and compression rate changes, start over.
 */
static int clear_flg = 0;

static long in_count = 1;    /* length of input */
static long out_count = 0;   /* # of codes output (for debugging) */

/*
 * Compression algorithm:
 * Use open addressing double hashing (no chaining) on the 
 * prefix code / next character combination.
 *
 * We do a variant of Knuth's algorithm D (vol. 3, sec. 6.4) along
 * with G. Knott's relatively-prime secondary probe.
 * Here, the modular division first probe is gives way to a faster
 * exclusive-or manipulation.
 *
 * Also do block compression with an adaptive reset, whereby the code
 * table is cleared when the compression ratio decreases, but after
 * the table fills.
 *
 * The variable-length output codes are re-sized at this point, and
 * a special CLEAR code is generated for the decompressor.
 *
 * Late addition:  construct the table according to file size for
 * noticeable speed improvement on small files.
 *
 * Please direct questions about this implementation to ames!jaw.
 */

static int g_init_bits;
static FILE *g_outfile;

static int ClearCode;
static int EOFCode;


/*===================================================================*/
static void compress(init_bits, outfile, data, len)
int     init_bits;
FILE   *outfile;
BYTE   *data;
int     len;
{
register long fcode;
register int i = 0;
register int c;
register int ent;
register int disp;
register int hsize_reg;
register int hshift;

/* Set up initial number of bits and pointer to output file */

  g_init_bits = init_bits;
  g_outfile   = outfile;

/* Initialize 'compress' globals */

  maxbits    = NBITS;
  maxmaxcode = 1<<NBITS;
  memset((char *)htab,    0, sizeof(htab));
  memset((char *)codetab, 0, sizeof(codetab));
  hsize = HSIZE;
  free_ent  = 0;
  clear_flg = 0;
  in_count  = 1;
  out_count = 0;
  cur_accum = 0;
  cur_bits  = 0;

/* Set up the necessary values */

  out_count = 0;
  clear_flg = 0;
  in_count  = 1;
  maxcode   = MAXCODE(n_bits = g_init_bits);

  ClearCode = (1 << (init_bits - 1));
  EOFCode   = ClearCode + 1;
  free_ent  = ClearCode + 2;

  char_init();
/*ATTENTION*/ if (0) *data = 16;
  ent = *data++;  len--;

  hshift = 0;
  for (fcode=(long)hsize; fcode < 65536L; fcode*=2L)  hshift++;
  hshift = 8 - hshift;                /* set hash code range bound */

  hsize_reg = hsize;
  cl_hash((long)hsize_reg);           /* clear hash table */

  output(ClearCode);
    
  while (len) {
/*ATTENTION*/ if (0) *data = 16;
    c = *data++;  len--;
    in_count++;

    fcode = (long)(((long)c << maxbits) + ent);
    i = (((int)c << hshift) ^ ent);   /* xor hashing */

    if ( htab[i] == fcode ) {
      ent = codetab[i];
      continue;
    }

    else if ((long)htab[i] < 0)      /* empty slot */
      goto nomatch;

    disp = hsize_reg - i;       /* secondary hash (after G. Knott) */
    if ( i == 0 )
      disp = 1;

probe:
    if ( (i -= disp) < 0 )
      i += hsize_reg;

    if ( htab[i] == fcode ) {
      ent = codetab[i];
      continue;
    }

    if ((long)htab[i] >= 0) 
      goto probe;

nomatch:
    output(ent);
    out_count++;
    ent = c;

    if ( free_ent < maxmaxcode ) {
      codetab[i] = free_ent++; /* code -> hashtable */
      htab[i] = fcode;
    }
    else
      cl_block();
  }

  /* Put out the final code */
  output(ent);
  out_count++;
  output(EOFCode);
}


/*===================================================================*/
/* Output the given code.
 * Inputs:
 *      code:   A n_bits-bit integer.  If == -1, then EOF.
 *              This assumes that n_bits =< (long)wordsize - 1.
 * Outputs:
 *      Outputs code to the file.
 * Assumptions:
 *      Chars are 8 bits long.
 * Algorithm:
 *      Maintain a NBITS character long buffer (so that 8 codes will
 * fit in it exactly).  Use the VAX insv instruction to insert each
 * code in turn.  When the buffer fills up empty it and start over.
 */

static
unsigned long masks[] = { 0x0000, 0x0001, 0x0003, 0x0007, 0x000F,
                                  0x001F, 0x003F, 0x007F, 0x00FF,
                                  0x01FF, 0x03FF, 0x07FF, 0x0FFF,
                                  0x1FFF, 0x3FFF, 0x7FFF, 0xFFFF };

static void output(code)
int code;
{
  cur_accum &= masks[cur_bits];

  if (cur_bits > 0) cur_accum |= ((long)code << cur_bits);
  else              cur_accum = code;
	
  cur_bits += n_bits;

  while( cur_bits >= 8 ) {
    char_out ((unsigned int)(cur_accum & 0xff));
    cur_accum >>= 8;
    cur_bits -= 8;
  }

/* If the next entry is going to be too big for the code size,
 * then increase it, if possible.
 */

  if (free_ent > maxcode || clear_flg) {

    if( clear_flg ) {
      maxcode = MAXCODE (n_bits = g_init_bits);
      clear_flg = 0;
    }
    else {
      n_bits++;
      if ( n_bits == maxbits ) maxcode = maxmaxcode;
      else                     maxcode = MAXCODE(n_bits);
    }
  }
	
/* At EOF, write the rest of the buffer */

  if (code == EOFCode) {

    while (cur_bits > 0) {
      char_out ((unsigned int)(cur_accum & 0xff));
      cur_accum >>= 8;
      cur_bits -= 8;
    }

    flush_char();
	
    fflush(g_outfile);

    if (ferror(g_outfile)) {
      fprintf(stderr,"unable to write GIF file\n");
      exit(1);
    }
  }
}


/*===================================================================*/
static void putword(w, fp)
int w;
FILE *fp;
{
  /* writes a 16-bit integer in GIF order (LSB first) */
  fputc(w & 0xff, fp);
  fputc((w>>8)&0xff, fp);
}


/*===================================================================*/
/* Hash table clear for block compress */
static void cl_block ()
{
  cl_hash((long)hsize);
  free_ent = ClearCode + 2;
  clear_flg = 1;

  output(ClearCode);
}


/*===================================================================*/
/* Reset code table */
static void cl_hash(hsize)
register long hsize;
{
register long    *htab_p = htab+hsize;
register long     i;
register long     m1 = -1;

  i = hsize - 16;
  do {                            /* might use Sys V memset(3) here */
    *(htab_p-16) = m1;
    *(htab_p-15) = m1;
    *(htab_p-14) = m1;
    *(htab_p-13) = m1;
    *(htab_p-12) = m1;
    *(htab_p-11) = m1;
    *(htab_p-10) = m1;
    *(htab_p-9) = m1;
    *(htab_p-8) = m1;
    *(htab_p-7) = m1;
    *(htab_p-6) = m1;
    *(htab_p-5) = m1;
    *(htab_p-4) = m1;
    *(htab_p-3) = m1;
    *(htab_p-2) = m1;
    *(htab_p-1) = m1;
    htab_p -= 16;
  } while ((i -= 16) >= 0);

  for ( i += 16; i > 0; i-- )
    *--htab_p = m1;
}


/******************************************************************************
 *
 * GIF Specific routines
 *
 ******************************************************************************/

/*===================================================================*/
/* Set up the 'byte output' routine */
/* Number of characters so far in this 'packet' */
static int a_count;

static void char_init()
{
	a_count = 0;
}


/*===================================================================*/
/* Add a character to the end of the current packet, and if it is 254
 * characters, flush the packet to disk.
 */
/* Define the storage for the packet accumulator */
static char accum[ 256 ];

static void char_out(c)
unsigned int c;
{
  accum[ a_count++ ] = c;
  if( a_count >= 254 ) 
    flush_char();
}

/*===================================================================*/
/*Flush the packet to disk, and reset the accumulator */
static void flush_char()
{
  if( a_count > 0 )
  {
    fputc( a_count, g_outfile );
    fwrite( accum, 1, a_count, g_outfile );
    a_count = 0;
  }
}	

/*===================================================================*/
/*Flush the packet to disk, and reset the accumulator */
static void flush_char_()
{
int i;

  if( a_count > 0 )
  {
printf("flush_char fputc %d to %p\n", a_count, g_outfile);
    fputc( a_count, g_outfile );
for (i=0; i<a_count; i++)
    printf("%d\n", accum[i]);
printf("flush_char fofs %ld\n", ftell(g_outfile));
    fwrite( accum, 1, a_count, g_outfile );
    a_count = 0;
  }
}

