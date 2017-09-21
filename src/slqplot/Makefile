BINDIR	= ../bin
SLLDIR	= ../libslink
SLLIB	= $(SLLDIR)/libslink.a

EXTRA_INCL = -I/opt/plotutils/include
EXTRA_LIBS = -L/opt/plotutils/lib

ifneq ($(INSTALL_HOME),Y)
INSTALL_HOME  = /home/sysop
endif

CONFIG_FILE   = $(INSTALL_HOME)/config/qplot.ini
FILTER_CONFIG_FILE = $(INSTALL_HOME)/config/qplot.coef

CC = gcc
CXX = g++
LEX = flex
CFLAGS = -O2 -Wall -Wstrict-prototypes $(EXTRA_INCL)
LDFLAGS = -L/usr/X11/lib $(EXTRA_LIBS)
LDLIBS = -lplot -lXaw -lXpm -lXmu -lXt -lSM -lICE -lXext -lX11 -lpthread -ldl -lm
MSEED_FLAGS =

OBJS = slqplot.o qpdisplay.o qpgraph.o qpsetup.o lex.yy.o WWSS_Filter.o

all: $(BINDIR)/slqplot

.PHONY: all clean

$(BINDIR)/slqplot: $(OBJS) $(SLLIB)
	$(CXX) $(LDFLAGS) $+ $(LDLIBS) -o $@

slqplot.o: slqplot.c slqplot.h
	$(CC) $(CFLAGS) -I$(SLLDIR) -c $< -o $@

qpdisplay.o qpgraph.o: %.o: %.c slqplot.h
	$(CC) $(CFLAGS) -I$(SLLDIR) -c $< -o $@

qpsetup.o: qpsetup.c confparse.h slqplot.h
	$(CC) $(CFLAGS) -I$(SLLDIR) -DQP_FILTER_CONFIG_FILE=\"$(FILTER_CONFIG_FILE)\" -DQP_CONFIG_FILE=\"$(CONFIG_FILE)\" -c $< -o $@

WWSS_Filter.o:	WWSS_Filter.c
	$(CC) $(CFLAGS) -I$(SLLDIR) -c $< -o $@

lex.yy.o: lex.yy.c confparse.h
	$(CC) $(CFLAGS) $(WARNFLAGS) -I$(SLLDIR) -c $< -o $@

lex.yy.c: confparse.lex
	$(LEX) confparse.lex

clean:
	$(RM) slqplot lex.yy.c $(OBJS)
	
$(SLLIB):
	$(MAKE) -C $(SLLDIR)

$(QLIB):
	$(MAKE) -C $(QDIR)

