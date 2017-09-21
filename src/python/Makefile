BASEDIR = ..
SLPLUGINSDIR = $(BASEDIR)/libslplugin
SLPLUGINSLIB = $(SLPLUGINSDIR)/libslplugin.a

all: build

.PHONY: all clean build install

build: seiscomp/plugin/pyxplugin.c $(SLPLUGINSLIB)
	python setup.py build_ext

install: build
	python setup.py install

seiscomp/plugin/pyxplugin.c: seiscomp/plugin/plugin.pyx
	pyrexc $< -o $@

$(SLPLUGINSLIB):
	$(MAKE) -C $(SLPLUGINSDIR)

clean:
	rm -rf build seiscomp/plugin/pyxplugin.c

