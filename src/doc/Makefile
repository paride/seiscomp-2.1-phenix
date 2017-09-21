all: seiscomp-2.1.pdf

.PHONY: all clean .depend

seiscomp-2.1.pdf: seiscomp-2.1.tex dmacros.sty SDS.definition *.eps *.dtd
	@echo Generating TOC and references...
	-@latex seiscomp-2.1.tex < /dev/null > /dev/null 2>&1
	-@latex seiscomp-2.1.tex < /dev/null > /dev/null 2>&1
	latex seiscomp-2.1.tex < /dev/null
	dvips -t a4 seiscomp-2.1.dvi -o seiscomp-2.1.ps
	ps2pdf -sPAPERSIZE=a4 seiscomp-2.1.ps

clean:
	$(RM) $(addprefix seiscomp-2.1, .aux .toc .log .dvi .ps .pdf)

