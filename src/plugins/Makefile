DIRS = comserv_plugin serial_plugin chain_plugin sockplugin \
       esstf_plugin naqsplugin q330plugin reftek_plugin ewexport_plugin \
       scream_plugin mseedscan_plugin win_plugin
LIBS = seisan titan7.1 reftek_libs/archive reftek_libs/msgq \
       reftek_libs/reftek reftek_libs/rtp reftek_libs/si reftek_libs/util

all: $(DIRS)

.PHONY: $(DIRS) $(DIRS:%=depend_%) $(DIRS:%=clean_%) $(LIBS:%=clean_%) all depend clean

depend: $(DIRS:%=depend_%)

clean: $(DIRS:%=clean_%) $(LIBS:%=clean_%)

$(DIRS):
	$(MAKE) -C $@

$(DIRS:%=depend_%):
	-$(MAKE) -C $(@:depend_%=%) depend

$(DIRS:%=clean_%) $(LIBS:%=clean_%):
	-$(MAKE) -C $(@:clean_%=%) clean

