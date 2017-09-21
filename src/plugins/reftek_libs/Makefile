DIRS = archive msgq reftek rtp si util

PLATFORM = -DLINUX -Di386 -D_REENTRANT

all: $(DIRS)

.PHONY: $(DIRS) $(DIRS:%=clean_%) all clean

clean: $(DIRS:%=clean_%)

$(DIRS):
	$(MAKE) -C $@ PLATFORM="$(PLATFORM)"

$(DIRS:%=clean_%):
	-$(MAKE) -C $(@:clean_%=%) clean

