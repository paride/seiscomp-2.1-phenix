DIRS = utils msgs cserv qmaserv
BINDIR = ../../bin

OS=linux

all: $(DIRS)

.PHONY: $(DIRS) $(DIRS:%=depend_%) $(DIRS:%=clean_%) all depend clean

depend: $(DIRS:%=depend_%)

clean: $(DIRS:%=clean_%)

$(DIRS):
	$(MAKE) -C $@ -f Makefile.$(OS)

$(DIRS:%=depend_%):
	-$(MAKE) -C $(@:depend_%=%) -f Makefile.$(OS) depend

$(DIRS:%=clean_%):
	-$(MAKE) -C $(@:clean_%=%) -f Makefile.$(OS) clean

