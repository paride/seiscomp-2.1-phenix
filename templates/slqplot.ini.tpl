[slqplot]

traces = 24
tracelen = 15

scroll_step = 4
complete_pages = no
coloured_traces = no

desc1 = "#desc#"
desc2 = "Applied filter: WWSSN-SP"

channel BHZ filter=#plot_filter# mag=#plot_magn#
channel BHN filter=#plot_filter# mag=#plot_magn#
channel BHE filter=#plot_filter# mag=#plot_magn#

plot tek term=xterm
*plot x bg=white fg=black size=1024x682 
*plot x bg=white fg=black size=750x550 

* First ocurrence of an asterisk is replaced by the beginning time of the
* plot. Qplot creates an environment variable STATION, containing the
* station name, which can be used in shell commands.
* plot meta file=qp*.meta 
* plot meta file="|cat > ${STATION}.*.meta"

* The display of 7500x5000 pixels is mapped to a square region on printer so 
* we have to stretch and shift it in the x direction. 
* Values used here are the default ones and can be omitted.
* plot ps file=|lpr paper=a4 orientation=landscape scale=1.5;1 offset=-1250;0
* plot ps file=qp*.ps
* plot hpgl file=qp*.hpgl

[xslqplot]

traces = 24
tracelen = 15

scroll_step = 4
complete_pages = no
coloured_traces = yes

desc1 = "#desc#"
desc2 = "Applied filter: WWSSN-SP"

channel BHZ filter=#plot_filter# mag=#plot_magn#
channel BHN filter=#plot_filter# mag=#plot_magn#
channel BHE filter=#plot_filter# mag=#plot_magn#

plot x bg=white fg=black size=1024x682 
*plot x bg=white fg=black size=750x550 
* plot tek term=xterm

[printer]
 
traces = 24
tracelen = 15
 
scroll_step = 24
complete_pages = yes
coloured_traces = yes

desc1 = "#desc#"
desc2 = "Applied filter: WWSSN-SP"

channel BHZ filter=#plot_filter# mag=#plot_magn#
channel BHN filter=#plot_filter# mag=#plot_magn#
channel BHE filter=#plot_filter# mag=#plot_magn#
 
plot ps file="|gzip -c > qp*.ps.gz"
*plot ps file="|lpr"
*plot meta file="|gzip -c > qp*.meta.gz"

[gif]

traces = 48
tracelen = 30

scroll_step = 48
complete_pages = yes
coloured_traces = yes

desc1 = "#desc#"
desc2 = "Applied filter: WWSSN-SP"

channel BHZ filter=#plot_filter# mag=#plot_magn#

plot tek file="|#home_sysop#/bin/slqplot2tek2gif * #statid#"

