CSDIR	= ..
BINDIR	= ../../bin
CSINCL	= $(CSDIR)/include
CSUDIR	= $(CSDIR)/util
CSULIB	= $(CSUDIR)/libutil.a
QDIR	= ../../qlib2
QLIB    = $(QDIR)/libqlib2.a

CC = g++
CFLAGS = -O2 -Wno-write-strings -fpack-struct -I$(CSINCL) -I$(QDIR) -DSOLARIS2
LDFLAGS =
LIBS =

#LD_RUN_PATH = /opt/SUNWspro:/misc/opt/SUNWspro:/usr/lib

########################################################################
# Solaris 2 definitions
#CC	= cc
#CFLAGS	= -g -I$(CSINCL) -I$(QDIR) -DSOLARIS2 -g
#LIBS	= $(QDIR)/libqlib2.a -lposix4 -lsunmath -lm -lnsl -lsocket -ldl
#LIBS	= -R$(LD_RUN_PATH) $(QDIR)/libqlib2.a -lposix4 -lsunmath -lm -lnsl -lsocket -ldl
#LIBS	= -R$(LD_RUN_PATH) $(QDIR)/libqlib2.a -lposix4 -lm -lnsl -lsocket -ldl

########################################################################
# SunOS definitions
#CC	= acc
#CFLAGS	= -g -I$(CSINCL) -I$(QDIR)
#LIBS	= $(QDIR)/libqlib2.a -lm

########################################################################

P1 = netmon
P2 = datalog
#P3 = cpick_card_server
#P4 = evtalarm
P5 = dataspy

SRCS1 	= $(P1).c # fast_cs_check.c
OBJS1	= $(SRCS1:.c=.o)

SRCS2	= $(P2).c datalog_utils.c
OBJS2	= $(SRCS2:.c=.o)

SRCS3 	= $(P3).c syserr.c
OBJS3	= $(SRCS3:.c=.o)

SRCS4 	= $(P4).c syserr.c
OBJS4	= $(SRCS4:.c=.o)

SRCS5 	= $(P5).c 
OBJS5	= $(SRCS5:.c=.o)

ALL	= $(P1) $(P2) $(P3) $(P4) $(P5)

all:	$(ALL:%=$(BINDIR)/%)

$(BINDIR)/$(P1):	$(OBJS1) $(CSULIB) $(QLIB)
	$(CC) $(LDFLAGS) -o $@ $+ $(LIBS)

$(BINDIR)/$(P2):	$(OBJS2) $(CSULIB) $(QLIB)
	$(CC) $(LDFLAGS) -o $@ $+ $(LIBS)

#$(BINDIR)/$(P3):	$(OBJS3) $(CSULIB) $(QLIB)
#	$(CC) $(LDFLAGS) -o $@ $+ $(LIBS)

#$(BINDIR)/$(P4):	$(OBJS4) $(CSULIB) $(QLIB)
#	$(CC) $(LDFLAGS) -o $@ $+ $(LIBS)

$(BINDIR)/$(P5):	$(OBJS5) $(CSULIB) $(QLIB)
	$(CC) $(LDFLAGS) -o $@ $+ $(LIBS)

netmon.o:	netmon.c \
		$(CSINCL)/dpstruc.h $(CSINCL)/stuff.h \
		$(CSINCL)/service.h $(CSINCL)/cfgutil.h 
		$(CC) $(CFLAGS) -DALLOW_RECONFIG -c $<

datalog.o:	datalog.c $(QDIR)/qlib2.h \
		$(CSINCL)/dpstruc.h $(CSINCL)/seedstrc.h $(CSINCL)/stuff.h \
		$(CSINCL)/timeutil.h $(CSINCL)/service.h $(CSINCL)/cfgutil.h 
		$(CC) $(CFLAGS) -c $<

datalog_utils.o: datalog_utils.c datalog.h $(QDIR)/qlib2.h \
		$(CSINCL)/dpstruc.h $(CSINCL)/seedstrc.h $(CSINCL)/stuff.h \
		$(CSINCL)/timeutil.h $(CSINCL)/service.h $(CSINCL)/cfgutil.h 
		$(CC) $(CFLAGS) -c $<

cpick_card_server.o:	cpick_card_server.c
		$(CC) $(CFLAGS) -c $<

evtalarm.o:	evtalarm.c
		$(CC) $(CFLAGS) -c $<

dataspy.o:	dataspy.c \
		$(CSINCL)/dpstruc.h $(CSINCL)/seedstrc.h $(CSINCL)/stuff.h \
		$(CSINCL)/timeutil.h $(CSINCL)/service.h 
		$(CC) $(CFLAGS) -c $<

fast_cs_check.o:	fast_cs_check.c \
		$(CSINCL)/dpstruc.h $(CSINCL)/stuff.h \
		$(CSINCL)/service.h $(CSINCL)/cfgutil.h 
		$(CC) $(CFLAGS) -DALLOW_RECONFIG -c $<

$(CSULIB):	FORCE
		$(MAKE) -C $(CSUDIR)

$(QLIB):	FORCE
		$(MAKE) -C $(QDIR)

FORCE:

clean:
		$(RM) *.o

veryclean:
		$(RM) *.o $(ALL:%=$(BINDIR)/%)

install:	$(ALL)
		cp -p $(ALL) $(BINDIR)
		chmod u+s $(BINDIR)/netmon

