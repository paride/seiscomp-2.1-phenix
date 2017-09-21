BASEDIR = ..
BINDIR = $(BASEDIR)/bin
SLUTILSDIR = $(BASEDIR)/libslutils
SLUTILSLIB = $(SLUTILSDIR)/libslutils.a

XML_INCL := $(shell xml2-config --cflags)
XML_LIBS := $(shell xml2-config --libs)

OBJS = arclink.o

CXX = g++
CXXFLAGS = -Wall -g2
LDFLAGS =
LDLIBS =
OPTIONS = -DSYSLOG_FACILITY=LOG_LOCAL0

all: $(BINDIR)/arclink

.PHONY: all depend clean

$(BINDIR)/arclink: $(OBJS) $(SLUTILSLIB)
	$(CXX) $(LDFLAGS) $+ $(XML_LIBS) $(LDLIBS) -o $@

arclink.o: arclink.cc
	$(CXX) $(CXXFLAGS) $(OPTIONS) -I$(SLUTILSDIR) $(XML_INCL) -c $< -o $@

$(SLUTILSLIB):
	$(MAKE) -C $(SLUTILSDIR)

depend:
	$(CXX) $(OPTIONS) -I$(SLUTILSDIR) $(XML_INCL) -MM *.cc > .depend

clean:
	$(RM) $(OBJS) .depend
	
-include .depend

