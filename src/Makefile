DIRS = qlib2 libslink libslutils libslplugin seedlink plugins slarchive \
       slinktool arclink utils comserv

all: $(DIRS)

.PHONY: $(DIRS) $(DIRS:%=depend_%) $(DIRS:%=clean_%) all depend clean veryclean

depend: $(DIRS:%=depend_%)

clean: $(DIRS:%=clean_%)

veryclean: clean
	$(RM) bin/*

$(DIRS):
	$(MAKE) -C $@

$(DIRS:%=depend_%):
	-$(MAKE) -C $(@:depend_%=%) depend

$(DIRS:%=clean_%):
	-$(MAKE) -C $(@:clean_%=%) clean

