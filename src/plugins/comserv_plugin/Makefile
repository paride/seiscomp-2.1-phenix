BASEDIR = ../..
BINDIR = $(BASEDIR)/bin
CSIDIR = $(BASEDIR)/comserv/include
CSUDIR = $(BASEDIR)/comserv/util
CSULIB = $(CSUDIR)/libutil.a
SLUTILSDIR = $(BASEDIR)/libslutils
SLUTILSLIB = $(SLUTILSDIR)/libslutils.a
SLPLUGINSDIR = $(BASEDIR)/libslplugin
SLPLUGINSLIB = $(SLPLUGINSDIR)/libslplugin.a
QDIR   = $(BASEDIR)/qlib2
QLIB   = $(QDIR)/libqlib2.a

CONFIG_DIR  = /home/sysop/config
CONFIG_FILE = $(CONFIG_DIR)/plugins.ini

OBJS = comserv_plugin.o

CXX = g++
CXXFLAGS = -Wall -O2 -fno-operator-names
LDFLAGS =
LDLIBS =
OPTIONS = -DSYSLOG_FACILITY=LOG_LOCAL0 -DCONFIG_FILE=\"$(CONFIG_FILE)\"

all: $(BINDIR)/comserv_plugin

.PHONY: all depend clean

$(BINDIR)/comserv_plugin: $(OBJS) $(SLPLUGINSLIB) $(SLUTILSLIB) $(CSULIB) \
    $(QLIB)
	$(CXX) $(LDFLAGS) $+ $(LDLIBS) -o $@

comserv_plugin.o: comserv_plugin.cc
	$(CXX) $(CXXFLAGS) $(OPTIONS) -I$(SLPLUGINSDIR) -I$(SLUTILSDIR) \
           -I$(CSIDIR) -I$(QDIR) -c $< -o $@

$(SLUTILSLIB):
	$(MAKE) -C $(SLUTILSDIR)

$(SLPLUGINSLIB):
	$(MAKE) -C $(SLPLUGINSDIR)

$(CSULIB):
	$(MAKE) -C $(CSUDIR)

$(QLIB):
	$(MAKE) -C $(QDIR)

depend:
	$(CXX) $(OPTIONS) -I$(SLPLUGINSDIR) -I$(SLUTILSDIR) -I$(CSIDIR) \
           -I$(QDIR) -MM *.cc > .depend

clean:
	$(RM) $(OBJS) .depend
	
-include .depend

