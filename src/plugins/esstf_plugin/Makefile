SHELL = /bin/sh
CC = gcc
CFLAGS = -O -DPLUGIN_COMPATIBILITY
LDFLAGS = -L../../qlib2
LDLIBS = -lm
INCLUDE = -I..
QDIR = ../../qlib2
QLIB = $(QDIR)/libqlib2.a
OBJS = esstf_aux.o esstf_plugin.o plugin.o
PROG = ../../bin/esstf_plugin

$(PROG): $(OBJS) $(QLIB)
	$(CC) $(LDFLAGS) $+ $(QLIB) $(LDLIBS) -o $@

esstf_aux.o esstf_plugin.o: %.o: %.c esstf_aux.h esstf_config.h
	$(CC) -c $(CFLAGS) $(INCLUDE) -c $< -o $@

plugin.o: ../plugin.c
	$(CC) -c $(CFLAGS) $(INCLUDE) -c $< -o $@

$(QLIB):
	$(MAKE) -C $(QDIR)

clean:
	$(RM) $(OBJS)

