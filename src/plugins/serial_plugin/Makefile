BASEDIR = ../..
BINDIR = $(BASEDIR)/bin
SLUTILSDIR = $(BASEDIR)/libslutils
SLUTILSLIB = $(SLUTILSDIR)/libslutils.a
SLPLUGINSDIR = $(BASEDIR)/libslplugin
SLPLUGINSLIB = $(SLPLUGINSDIR)/libslplugin.a
QDIR   = $(BASEDIR)/qlib2
QLIB   = $(QDIR)/libqlib2.a
GCFDIR = ../gcflib
GCFINC = $(GCFDIR)/include
GCFLIB = $(GCFDIR)/lib/libgcf.a

CONFIG_DIR  = /home/sysop/config
CONFIG_FILE = $(CONFIG_DIR)/plugins.ini

OBJS = serial_plugin.o proto_edata.o proto_edata2.o proto_guralp.o \
    proto_dr24.o proto_prema.o proto_hrd24.o proto_hrd24alt.o

CXX = g++
CXXFLAGS = -Wall -O2
LDFLAGS =
LDLIBS =
OPTIONS = -DSYSLOG_FACILITY=LOG_LOCAL0 -DCONFIG_FILE=\"$(CONFIG_FILE)\"

all: $(BINDIR)/serial_plugin

.PHONY: all depend clean

$(BINDIR)/serial_plugin: $(OBJS) $(SLPLUGINSLIB) $(SLUTILSLIB) $(QLIB) \
    $(GCFLIB)
	$(CXX) $(LDFLAGS) $+ $(LDLIBS) -o $@

serial_plugin.o: serial_plugin.cc
	$(CXX) $(CXXFLAGS) $(OPTIONS) -I$(SLPLUGINSDIR) -I$(SLUTILSDIR) \
           -I$(QDIR) -c $< -o $@

proto_edata.o: proto_edata.cc
	$(CXX) $(CXXFLAGS) $(OPTIONS) -I$(SLPLUGINSDIR) -I$(SLUTILSDIR) \
           -I$(QDIR) -c $< -o $@

proto_edata2.o: proto_edata2.cc
	$(CXX) $(CXXFLAGS) $(OPTIONS) -I$(SLPLUGINSDIR) -I$(SLUTILSDIR) \
           -I$(QDIR) -c $< -o $@

proto_guralp.o: proto_guralp.cc
	$(CXX) $(CXXFLAGS) $(OPTIONS) -I$(SLPLUGINSDIR) -I$(SLUTILSDIR) \
           -I$(QDIR) -I$(GCFINC) -c $< -o $@

proto_dr24.o: proto_dr24.cc
	$(CXX) $(CXXFLAGS) $(OPTIONS) -I$(SLPLUGINSDIR) -I$(SLUTILSDIR) \
           -I$(QDIR) -c $< -o $@

proto_prema.o: proto_prema.cc
	$(CXX) $(CXXFLAGS) $(OPTIONS) -I$(SLPLUGINSDIR) -I$(SLUTILSDIR) \
           -I$(QDIR) -c $< -o $@

proto_hrd24.o: proto_hrd24.cc
	$(CXX) $(CXXFLAGS) $(OPTIONS) -I$(SLPLUGINSDIR) -I$(SLUTILSDIR) \
           -I$(QDIR) -c $< -o $@

proto_hrd24alt.o: proto_hrd24alt.cc
	$(CXX) $(CXXFLAGS) $(OPTIONS) -I$(SLPLUGINSDIR) -I$(SLUTILSDIR) \
           -I$(QDIR) -c $< -o $@

$(SLUTILSLIB):
	$(MAKE) -C $(SLUTILSDIR)

$(SLPLUGINSLIB):
	$(MAKE) -C $(SLPLUGINSDIR)

$(QLIB):
	$(MAKE) -C $(QDIR)

depend:
	$(CXX) $(OPTIONS) -I$(SLPLUGINSDIR) -I$(SLUTILSDIR) -I$(QDIR) \
           -I$(GCFINC) -MM *.cc > .depend

clean:
	$(RM) $(OBJS) .depend
	
-include .depend

