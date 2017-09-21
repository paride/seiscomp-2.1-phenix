VERSION   = 7.1
CC        = gcc
INC       = -Ilibtitio
#
# options: USE_CACHE, AIX, HAS_NINT
CFLAG     = -O
CFLAG     = -g
CFLAG     =
CFLAGS    = $(CFLAG) $(INC) -DUSE_CACHE
CFLAGS    = $(CFLAG) $(INC) -DHAS_NINT
CFLAGS    = $(CFLAG) $(INC)
CFLAGS    = $(CFLAG)
CFLAGS    = $(CFLAG) -DANSI_C -Wall

LIBIO     = libtitio/libio.o
LIBX      = libxplot/libxplot.o

ALL       = cvtit titseg titinfo titdump tit2seed xtit tittrig xdft

.c.o:
	$(CC) -c $(CFLAGS) $<

all: $(ALL)

$(LIBIO): 
	cd libtitio; make libio.o CC="$(CC)" CFLAGS="$(CFLAGS)"

$(LIBX):
	cd libxplot; make libxplot.o CC="$(CC)" CFLAGS="$(CFLAGS)"

xdft:   xdft.c libcmd2.o libsmo.o \
	libxplot1.o libxplot2.o $(LIBX)
	cd libxplot; make libxplot.o CC="$(CC)" CFLAGS="$(CFLAGS)"
	$(CC) $(CFLAGS) -L/usr/X11R6/lib \
	xdft.c libcmd2.o libsmo.o \
	libxplot1.o libxplot2.o $(LIBX) \
	-lm -lX11 -lXt -lXext -o xdft$(VERSION)

xtit:   xtit.o readtitanx.o libtitan.o synchro.o \
	libtrigger.o libevn.o libinfo.o libutil.o libtime.o \
	libxplot0.o $(LIBIO) $(LIBX)
	cd libtitio; make libio.o CC="$(CC)" CFLAGS="$(CFLAGS)"
	cd libxplot; make libxplot.o CC="$(CC)" CFLAGS="$(CFLAGS)"
	$(CC) $(CFLAGS) -L/usr/X11R6/lib \
	xtit.o readtitanx.o libtitan.o synchro.o \
	libtrigger.o libevn.o libinfo.o libutil.o libtime.o \
	libxplot0.o $(LIBIO) $(LIBX) \
	-lm -lX11 -lXt -lXext -o xtit$(VERSION)

tittrig:   tittrig.o readtitanx.o libtitan.o synchro.o \
	libtrigger.o libevn.o libinfo.o libutil.o libtime.o \
	libxplot0.o $(LIBIO) $(LIBX)
	cd libtitio; make libio.o CC="$(CC)" CFLAGS="$(CFLAGS)"
	cd libxplot; make libxplot.o CC="$(CC)" CFLAGS="$(CFLAGS)"
	$(CC) $(CFLAGS) -L/usr/X11R6/lib \
	tittrig.o readtitanx.o libtitan.o synchro.o \
	libtrigger.o libevn.o libinfo.o libutil.o libtime.o \
	libxplot0.o $(LIBIO) $(LIBX) \
	-lm -lX11 -lXt -lXext -o tittrig$(VERSION)

cvtit:  cvtit.o readtitan.o libout.o libtitan.o \
	synchro.o libsac.o libahz.o libsegy.o ioroutin.o libasc.o \
	libinfo.o libsism.o libevn.o libutil.o libtime.o segtitan.o \
	libseedtit.o libseed.o seed_chan.o steim.o decimate.o $(LIBIO)
	cd libtitio; make libio.o CC="$(CC)" CFLAGS="$(CFLAGS)"
	$(CC) $(CFLAGS) cvtit.o readtitan.o libout.o libtitan.o \
	synchro.o libsac.o libahz.o libsegy.o ioroutin.o libasc.o \
	libinfo.o libsism.o libevn.o libutil.o libtime.o segtitan.o \
	libseedtit.o libseed.o seed_chan.o steim.o decimate.o $(LIBIO) \
	-lnsl -lm -o cvtit$(VERSION)

titinfo:  titinfo.o readtitan3.o libtitan.o \
	synchro.o ioroutin.o libinfo.o libutil.o libtime.o $(LIBIO)
	cd libtitio; make libio.o CC="$(CC)" CFLAGS="$(CFLAGS)"
	$(CC) $(CFLAGS) titinfo.o readtitan3.o libtitan.o \
	synchro.o ioroutin.o libinfo.o libutil.o libtime.o $(LIBIO) \
	-lnsl -lm -o titinfo$(VERSION)

titseg:  titseg.o readtitan1.o libtitan.o \
	synchro.o ioroutin.o \
	libinfo.o libevn.o libutil.o libtime.o segtitan.o $(LIBIO)
	cd libtitio; make libio.o CC="$(CC)" CFLAGS="$(CFLAGS)"
	$(CC) $(CFLAGS) titseg.o readtitan1.o libtitan.o \
	synchro.o ioroutin.o \
	libinfo.o libevn.o libutil.o libtime.o segtitan.o $(LIBIO) \
	-lnsl -lm -o titseg$(VERSION)

tit2seed:  tit2seed.o readtitan2.o libtitan.o \
	synchro.o ioroutin.o \
	libinfo.o libutil.o libtime.o \
	loadDBsis.o libseedtit.o libseed.o seed_chan.o steim.o \
	mkseed.o loadresp.o stahead.o $(LIBIO)
	cd libtitio; make libio.o CC="$(CC)" CFLAGS="$(CFLAGS)"
	$(CC) $(CFLAGS) tit2seed.o readtitan2.o libtitan.o \
	synchro.o ioroutin.o \
	libinfo.o libutil.o libtime.o \
	loadDBsis.o libseedtit.o libseed.o seed_chan.o steim.o \
	mkseed.o loadresp.o stahead.o $(LIBIO) \
	-lnsl -lm -o tit2seed$(VERSION)

titdump:  titdump.o synchro.o ioroutin.o libutil.o $(LIBIO)
	cd libtitio; make libio.o CC="$(CC)" CFLAGS="$(CFLAGS)"
	$(CC) $(CFLAGS) titdump.o synchro.o ioroutin.o libutil.o $(LIBIO) \
	-lnsl -lm -o titdump$(VERSION)

# SUN: add library -lnsl 

titdt:  titdt.o libutil.o
	$(CC) $(CFLAGS) titdt.o libutil.o -lm -o titdt

utime:  utime.o
	$(CC) $(CFLAGS) utime.c -lm -o utime

rsampsac:  rsampsac.o
	$(CC) $(CFLAGS) rsampsac.c -lm -o rsampsac

mmdec:  mmdec.c
	$(CC) mmdec.c -lm -o mmdec

cktit:  cktit.o
	$(CC) cktit.c -lm -o cktit

smo: smo.c libsmo.o libxplot/libutil.o
	$(CC) $(CFLAGS) smo.c libsmo.o libxplot/libutil.o -lm -o smo

setdrv: setdrv.o $(LIBIO)
	$(CC) -o setdrv setdrv.o $(LIBIO) $(INC)


fmttitan: format.o libutil.o $(LIBIO)
	$(CC) -o fmttitan format.o $(LIBIO) libutil.o  -lm

clean:
	@-/bin/rm *.o cvtit$(VERSION) titseg$(VERSION);
	@-/bin/rm titinfo$(VERSION) titdump$(VERSION) tit2seed$(VERSION)
	@-/bin/rm xtit$(VERSION) tittrig$(VERSION) xdft$(VERSION)
	cd libtitio; make clean;
	cd libxplot; make clean

