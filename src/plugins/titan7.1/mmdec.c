/*====================================================================
         mmdec.c
 *===================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>


#define BSIZE 50000

/*===================================================================*/
void main(argc, argv)
int     argc;
char    *argv[];
{
FILE    *Fpinp;
FILE    *Fpout;
char    iname[255];
char    oname[255];
struct stat statbuf;
int     ndata, req_ndata;
float   fbuf[50000];
int     i, n, N;
int     decifac, ndec, nout;


    ndata = 0;
    nout  = 0;
    req_ndata = 0;

    if (argc < 4) {
        fprintf(stderr, "\n  Min-max decimation\n");
        fprintf(stderr, "  Usage: mmdec input dec output\n");
        fprintf(stderr, "         (input and output are reals)\n");
        fprintf(stderr, "  Examples:\n");
        fprintf(stderr, "     mmdec data 10 data.dec\n");
        fprintf(stderr, "\n");
        exit(1);
    }
    for (i=4; i<argc; i++) {
        if      (!strncmp(argv[i], "n=", 2)) req_ndata = atoi(&argv[i][2]);
    }

    sprintf(iname, "%s", argv[1]);
    decifac       = atoi(argv[2]);
    sprintf(oname, "%s", argv[3]);

    if (decifac <= 2)
    {
        fprintf(stderr,
            "\n\tmmdec: decimation factor < 2 doesn't make sense\n");
        exit(1);
    }
    if (!(Fpinp = fopen(iname, "r")))
    {
        fprintf(stderr,"\n\tmmdec: cant't open file %s\n", iname);
        exit(1);
    }
    if (!(Fpout = fopen(oname, "w")))
    {
        fprintf(stderr,"\n\tmmdec: cant't open file %s\n", oname);
        exit(1);
    }
    if (stat(iname, &statbuf) != 0)
    {
        fprintf(stderr, "\n\tmmdec: can't stat '%s'\n", iname);
        exit(1);
    }
    ndata = statbuf.st_size / sizeof(float);
    if (ndata < 100)
    {
        fprintf(stderr,
            "\n\tmmdec: Don't ask me to decimate less than 100 data\n");
        exit(1);
    }


if (0) printf("file %s, %d samples, decim=%d\n", iname, ndata, decifac);

    N = ndata / BSIZE;
    for (n=0; n<N; n++)
    {
        if (fread(fbuf, sizeof(float), BSIZE, Fpinp) != BSIZE)
        {
            fprintf(stderr, "\n\tmmdec: read '%s' failed\n", iname);
            exit(1);
        }
/* Min-max decimation */
        ndec = decimate(BSIZE, fbuf, fbuf, decifac);
        nout += ndec;
        if (fwrite(fbuf, sizeof(float), ndec, Fpout) != ndec)
        {
            fprintf(stderr, "\n\tmmdec: read '%s' failed\n", iname);
            exit(1);
        }
    }
    N = ndata % BSIZE;
    if (fread(fbuf, sizeof(float), N, Fpinp) != N)
    {
        fprintf(stderr, "\n\tmmdec: read '%s' failed\n", iname);
        exit(1);
    }
/* Min-max decimation */
    ndec = decimate(N, fbuf, fbuf, decifac);
    nout += ndec;
    if (fwrite(fbuf, sizeof(float), ndec, Fpout) != ndec)
    {
        fprintf(stderr, "\n\tmmdec: read '%s' failed\n", iname);
        exit(1);
    }

    fclose(Fpinp);
    fclose(Fpout);

    printf("ninp=%d nout=%d actual_dec=%d\n",
         ndata, nout, (int)rint(((double)ndata/(double)nout)));

    exit(0);
}


/*=====================================================================
 *  decimate.c
 *
 *  decimate an float array src, length npts, by a factor of decim.
 *  Return in float array dest (may be same as src)
 *
 *====================================================================*/
int decimate(npts, src, dest, decim)
int npts, decim;
float *src, *dest;
{
    int i, nout;
    float  min, max;

    if (decim <= 1) {
	for (i = 0; i < npts; i++) 
	  dest[i] = src[i];
	nout = npts;
	return nout;
    }

/* Do the decimation */

    max = min = src[0];
    nout = 0;
    for (i = 1; i < npts; i++) {
	if (src[i] < min) min = src[i];
	if (src[i] > max) max = src[i];
	if ( (i % decim) == 0 ) {
	    dest[nout] = min;
	    nout       = nout + 1;
	    dest[nout] = max;
	    nout       = nout + 1;
	    min = max  = src[i];
	}
    }

    dest[nout] = min;
    nout       = nout + 1;
    dest[nout] = max;
    nout       = nout + 1;

    return nout;
}


