CSDIR		= ..
BINDIR		= ../../bin
CSINCL		= $(CSDIR)/include
CSUDIR		= $(CSDIR)/util
CSULIB		= $(CSUDIR)/libutil.a
TOOLSDIR	= $(CSDIR)/file
TOOLSLIB    = $(TOOLSDIR)/ToolsLib.a

DEFS 	= -DLINUX -DEJECT -DCLEAN_ANY_FILE
CC		= gcc
CXX     = g++
CFLAGS	= -O2 -fwritable-strings -pedantic -Wall -I$(CSINCL) -I$(TOOLSDIR) $(DEFS)
LDFLAGS	=

SRCS	= Conf.c \
		Log.c \
		Main.c \
		Process.c \
		Tape.c \
		Disk.c	\
		Dump.c \
		Verify.c \
		Lock.c
		
OBJS	= Conf.o \
		Log.o \
		Main.o \
		Process.o \
		Tape.o \
		Disk.o \
		Dump.o \
		Verify.o \
		Lock.o

all:	$(BINDIR)/datadump

$(BINDIR)/datadump:	$(OBJS) $(CSULIB) $(TOOLSLIB)
		$(CXX) $(LDFLAGS) -o $@ $+

$(CSULIB):
		$(MAKE) -C $(CSUDIR)

$(TOOLSLIB):
		$(MAKE) -C $(TOOLSDIR)

clean:
	$(RM) *.o

veryclean:
	$(RM) *.o $(BINDIR)/datadump

