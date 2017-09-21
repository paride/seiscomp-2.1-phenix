CC	= g++
BINDIR	= ../../bin
INCL	= ../include
CSUDIR	= ../util
CSULIB  = $(CSUDIR)/libutil.a
QDIR    = ../../qlib2
QLIB    = $(QDIR)/libqlib2.a
CFLAGS	= -O2 -fpack-struct -fpermissive -I$(INCL) -I$(QDIR)
LDFLAGS	=
LDLIBS	=

########################################################################
# Solaris 2 definitions
#
#CC	= cc
#INCL	= ../include
#CFLAGS	= -I$(INCL) +w -DSOLARIS2 -g
#LDFLAGS	= -g
#CSUDIR	= ../util
#CSULIB = $(CSUDIR)/libutil.a
#LDLIBS	= -lposix4 -lnsl -lsocket

########################################################################
# SunOS definitions
#
#CC	= acc
#INCL	= ../include
#CFLAGS	= -I$(INCL)
#LDFLAGS	=
#CSUDIR	= ../util
#CSULIB = $(CSUDIR)/libutil.a
#LDLIBS	=

########################################################################

ALL =	$(BINDIR)/comserv


all:		$(ALL)

$(BINDIR)/comserv:	server.o cscfg.o buffers.o comlink.o seedlink.o commands.o \
		schedule.o $(CSULIB) $(QLIB)
		$(CC) $(LDFLAGS) -o $@ $+ $(LDLIBS)

server.o:	$(INCL)/dpstruc.h $(INCL)/quanstrc.h $(INCL)/service.h \
		$(INCL)/cfgutil.h $(INCL)/stuff.h $(INCL)/lockutil.h schedule.h \
		bitstring.h server.h server.ii
		$(CC) -c $(CFLAGS) server.ii

cscfg.o:	$(INCL)/dpstruc.h $(INCL)/quanstrc.h $(INCL)/service.h \
		$(INCL)/cfgutil.h $(INCL)/stuff.h server.h cscfg.ii
		$(CC) -c $(CFLAGS) cscfg.ii

buffers.o:	$(INCL)/dpstruc.h $(INCL)/quanstrc.h $(INCL)/service.h \
		$(INCL)/stuff.h server.h buffers.ii
		$(CC) -c $(CFLAGS) buffers.ii

comlink.o:	$(INCL)/dpstruc.h $(INCL)/quanstrc.h $(INCL)/service.h \
		$(INCL)/stuff.h server.h comlink.ii
		$(CC) -c $(CFLAGS) comlink.ii

commands.o:	$(INCL)/dpstruc.h $(INCL)/quanstrc.h $(INCL)/service.h \
		$(INCL)/stuff.h server.h commands.ii
		$(CC) -c $(CFLAGS) commands.ii

seedlink.o:	$(INCL)/dpstruc.h $(INCL)/quanstrc.h $(INCL)/service.h \
		$(INCL)/stuff.h server.h seedlink.c
		$(CC) -c $(CFLAGS) seedlink.c

schedule.o:	bitstring.h schedule.h schedule.c
		$(CC) -c $(CFLAGS) schedule.c

%.ii: %.c
		$(CPP) $(CFLAGS) -D__cplusplus -x c $< > $@

$(CSULIB):	FORCE
		$(MAKE) -C $(CSUDIR)

$(QLIB):	FORCE
		$(MAKE) -C $(QDIR)

FORCE:

clean:
		$(RM) *.o *.ii

veryclean:
		$(RM) *.o *.ii $(ALL)

install:	$(ALL)
		cp -p $(ALL) $(BINDIR)

