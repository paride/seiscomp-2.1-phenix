BASEDIR = ../..
CSDIR = $(BASEDIR)/comserv
BINDIR = $(BASEDIR)/bin
CSIDIR = $(CSDIR)/include
CSUDIR = $(CSDIR)/util
CSULIB = $(CSUDIR)/libutil.a
QDIR   = $(BASEDIR)/qlib2
QLIB   = $(QDIR)/libqlib2.a
TOOLSDIR = $(CSDIR)/file
TOOLSLIB = $(TOOLSDIR)/ToolsLib.a

CXX = g++
CXXFLAGS = -Wall -O2 -fno-operator-names
CC = gcc
CFLAGS = -Wall -O2
LDFLAGS =
LDLIBS =

OBJ = LogAnalyzer.o

all: $(BINDIR)/LogAnalyzer

.PHONY: all clean .depend

$(BINDIR)/LogAnalyzer: LogAnalyzer.o $(CSULIB) $(QLIB) $(TOOLSLIB)
	$(CC) $(LDFLAGS) $+ $(LDLIBS) -o $@

LogAnalyzer.o: LogAnalyzer.c
	$(CC) $(CXXFLAGS) -I$(QDIR) -I$(CSIDIR) -I$(TOOLSDIR) -I$(CSDIR)/clients.ucb -c $< -o $@

$(CSULIB):
	$(MAKE) -C $(CSUDIR)

$(QLIB):
	$(MAKE) -C $(QDIR)

$(TOOLSLIB):
	$(MAKE) -C $(TOOLSDIR)

depend:
	$(CC) -I$(QDIR) -I$(CSIDIR) -I$(TOOLSDIR) -MM *.c > .depend

clean:
	$(RM) $(OBJ) .depend
	
-include .depend

