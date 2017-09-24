SLUTILSDIR = ..
SLUTILSLIB = $(SLUTILSDIR)/libslutils.a

XML_INCL := $(shell xml2-config --cflags)
XML_LIBS := $(shell xml2-config --libs)

OBJS = read_xml.o read_ini_multi.o read_ini_single.o

CXX = g++
CXXFLAGS = -O2
LDFLAGS =
LDLIBS =

all: read_xml read_ini_multi read_ini_single

.PHONY: all depend clean

read_xml: read_xml.o $(SLUTILSLIB)
	$(CXX) $(LDFLAGS) $+ $(XML_LIBS) $(LDLIBS) -o $@

read_ini_multi: read_ini_multi.o $(SLUTILSLIB)
	$(CXX) $(LDFLAGS) $+ $(XML_LIBS) $(LDLIBS) -o $@

read_ini_single: read_ini_single.o $(SLUTILSLIB)
	$(CXX) $(LDFLAGS) $+ $(XML_LIBS) $(LDLIBS) -o $@

$(OBJS): %.o: %.cc
	$(CXX) $(CXXFLAGS) -I$(SLUTILSDIR) -c $< -o $@

$(SLUTILSLIB):
	$(MAKE) -C $(SLUTILSDIR)

depend:
	$(CXX) -I$(SLUTILSDIR) -MM *.cc > .depend

clean:
	$(RM) $(OBJS) read_xml read_ini read_ini_multi read_ini_single .depend

-include .depend

