CC = g++
BINDIR	= ../../bin
INCL	= ../include
CFLAGS	= -O2 -fpack-struct -I$(INCL)
LDFLAGS	= 
CSUDIR	= ../util
CSULIB	= $(CSUDIR)/libutil.a
LDLIBS	=

########################################################################
# Solaris 2 definitions
#
#CC	= cc
#INCL	= ../include
#CFLAGS	= -I$(INCL) +w -DSOLARIS2 -g
#LDFLAGS	= -g
#CSUDIR	= ../util
#CSULIB	= $(CSUDIR)/libutil.a
#LDLIBS	= -lposix4 -lnsl -lsocket

########################################################################
# SunOS definitions
#
#CC	= acc
#INCL	= ../include
#CFLAGS	= -I$(INCL)
#LDFLAGS	=
#CSUDIR	= ../util
#CSULIB	= $(CSUDIR)/libutil.a
#LDLIBS	=

########################################################################

ALL =		$(BINDIR)/config $(BINDIR)/msgmon $(BINDIR)/dpda $(BINDIR)/dataread $(BINDIR)/cs_suspend

all:		$(ALL)

$(BINDIR)/config:		config.o $(CSULIB)
		$(CC) $(LDFLAGS) -o $@ $+ $(LDLIBS)

$(BINDIR)/msgmon:		msgmon.o $(CSULIB)
		$(CC) $(LDFLAGS) -o $@ $+ $(LDLIBS)

$(BINDIR)/dpda:		dpda.o $(CSULIB)
		$(CC) $(LDFLAGS) -o $@ $+ $(LDLIBS)

$(BINDIR)/dataread:	dataread.o $(CSULIB)
		$(CC) $(LDFLAGS) -o $@ $+ $(LDLIBS)

$(BINDIR)/cs_suspend:	cs_suspend.o $(CSULIB)
		$(CC) $(LDFLAGS) -o $@ $+ $(LDLIBS)

config.o:	$(INCL)/dpstruc.h $(INCL)/service.h \
		$(INCL)/cfgutil.h $(INCL)/stuff.h config.ii
		$(CC) -c $(CFLAGS) config.ii

msgmon.o:	$(INCL)/dpstruc.h $(INCL)/service.h \
		$(INCL)/cfgutil.h $(INCL)/stuff.h msgmon.ii
		$(CC) -c $(CFLAGS) msgmon.ii

dpda.o:		$(INCL)/dpstruc.h $(INCL)/service.h \
		$(INCL)/cfgutil.h $(INCL)/stuff.h dpda.ii
		$(CC) -c $(CFLAGS) dpda.ii

dataread.o:	$(INCL)/dpstruc.h $(INCL)/service.h \
		$(INCL)/seedstrc.h $(INCL)/stuff.h $(INCL)/timeutil.h dataread.ii
		$(CC) -c $(CFLAGS) dataread.ii

cs_suspend.o:	$(INCL)/dpstruc.h $(INCL)/service.h \
		$(INCL)/cfgutil.h $(INCL)/stuff.h cs_suspend.c
		$(CC) -c $(CFLAGS) cs_suspend.c

%.ii: %.c
		$(CPP) $(CFLAGS) -D__cplusplus -x c $< > $@

$(CSULIB):	FORCE
		$(MAKE) -C $(CSUDIR)

FORCE:

clean:
		$(RM) *.o *.ii

veryclean:
		$(RM) *.o *.ii $(ALL)

install:	$(ALL)
		cp -p $(ALL) $(BINDIR)

