BASEDIR = ../..
BINDIR = $(BASEDIR)/bin
SLPLUGINSDIR = $(BASEDIR)/libslplugin
SLPLUGINSLIB = $(SLPLUGINSDIR)/libslplugin.a
QDIR   = $(BASEDIR)/qlib2
QLIB   = $(QDIR)/libqlib2.a

CC = gcc
CFLAGS = -O2

CXX = g++
CXXFLAGS = -O2

JXX = gcj
JXXFLAGS = -O2

LDFLAGS =
LDLIBS  =

CXX_OBJS = java_plugin.o
JXX_OBJS = edu/iris/dmc/LogByDay/LogByDay.o \
           edu/iris/dmc/LISService/BudFilter.o \
           edu/iris/dmc/LISService/BudNetSocket.o

MAIN_CLASS = edu.iris.dmc.LISService.BudNetSocket

all: $(BINDIR)/bud_plugin

.PHONY: all depend clean

$(BINDIR)/bud_plugin: $(JXX_OBJS) $(CXX_OBJS) $(SLPLUGINSLIB) $(QLIB)
	$(JXX) $(LDFLAGS) --main=$(MAIN_CLASS) $+ $(LDLIBS) -o $@

$(CXX_OBJS): %.o: %.cc
	$(CXX) $(CXXFLAGS) -I$(SLPLUGINSDIR) -I$(QDIR) -c $< -o $@

$(JXX_OBJS): %.o: %.java
	$(JXX) $(JXXFLAGS) -c $< -o $@

$(SLPLUGINSLIB):
	$(MAKE) -C $(SLPLUGINSDIR)

$(QLIB):
	$(MAKE) -C $(QDIR)

depend:
	$(CC) $(CFLAGS) -I$(SLPLUGINSDIR) -I$(QDIR) -MM *.c *.cc > .depend

clean:
	$(RM) $(CXX_OBJS) $(JXX_OBJS) .depend

-include .depend

