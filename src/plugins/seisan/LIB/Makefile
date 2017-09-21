
##################################################
#
# SEISAN 7.0
#
# Makefile for SEISAN LIB directory
#
# changes:
#   05 10 99 lo: cleanup distribution
#   24 03 00 jh: add wave
#   05 05 00 lo: check if any include file is changed
#   15 05 00 jh: remove ttlsub
#    7 01 01 jh: add auto_amp
#   30 01 01 jh : add sig_spec
#   19 02 01 lo : add hypo71sub make_hypoc_brief 
#
##################################################

##################################################
# COMMENT OUT ACCEPT FOR YOUR PLATFORM, AND SEARCH
# FOR LINUX, SUNOS OR SOLARIS IF YOU ARE MOVING 
# BETWEEN PLATFORMS
##################################################

##################################################
# SOLARIS AND SUNOS
##################################################

# fc = f77 -g -c -I../INC
# cc = cc -c

##################################################
# LINUX
##################################################
fc = g77 -O2 -c -I../INC -fdollar-ok -fno-automatic
cc = gcc -O2 -c -I../INC
##################################################

# not included hypinv2.for

#
# depending on your os, keep the ONLY for your system and comment out the other
#
# SUN
#
# ONLY = comp_sun.o xview_lib.o
# 
# LINUX
#
ONLY = comp_linux.o

SUBS = abstim.o auto_tr.o \
  auto_amp.o autocoda.o autofil.o azibazi.o bcd.o bndpas.o check_s.o \
  compdecomp.o $(ONLY) \
  codecoutil.o \
  comp_unix.o componen.o conv_def.o fft.o  \
  eev_sub.o err_ellipse.o filename.o findchan.o \
  findevin.o ga_lib.o get_baz.o gse_io.o gserespl.o gsesei_lib.o \
  hypoloc.o hypoloc1.o hyposub1.o hyposub2.o \
  hyposub3.o hyposub4.o hyposub6.o \
  hypo71sub.o make_hypoc_brief.o \
  inc_id.o indata.o libsei.o lsqlin.o \
  maxlik.o mb_att.o merge_f.o  mfhead.o mul_spec.o \
  nortype.o picsub.o polos.o recfil.o \
  rea.o \
  removedc.o sei_mes.o seiplot.o seisinc.o sfilname.o shead.o sheads.o \
  sacsei_lib.o sig_spec.o \
  spec_dist.o stat_loc.o swap.o syntsel.o sys_resp.o systime.o \
  svd.o timerout.o xy_plot.o respfil.o \
  seis_unix.o sfil.o  tau.o wave.o xlib.o

#
# OTHERS
# 
OTHERS = sac_dummy.o 

#
# INCLUDE FILES
#
INCS = ../INC/autosig.inc ../INC/continuous.inc \
  ../INC/decode.inc ../INC/epimap.inc ../INC/hypcom.inc ../INC/hypcoma.inc \
  ../INC/hypcomc.inc ../INC/hypcomp.inc ../INC/hypcomt.inc ../INC/hypcomv.inc \
  ../INC/hypparm.inc ../INC/libsei.inc ../INC/limits.inc ../INC/mulplt.inc \
  ../INC/qlg.inc ../INC/rea.inc ../INC/seidim.inc ../INC/seiplot.inc \
  ../INC/seisan.inc ../INC/symbol.inc ../INC/trace_plot.inc ../INC/ttlim.inc \
  ../INC/version.inc ../INC/waveform.inc ../INC/winplot1.inc \
  ../INC/winplot2.inc

all: output $(SUBS) $(OTHERS) seisan.a seidim.inc 

output:
	@echo -------------------------------------------------
	@echo ---- CHECKING SEISAN 7 LIBRARIES ----------------
	@echo -------------------------------------------------

##################################################
# FOR SUNOS REMOVE # IN FOLLOWING LINES
# ONLY NEEDED WHEN COMPILING LIB DIRECTLY BEFORE PRO
seidim.inc:
        # cp ../INC/*.inc .
##################################################

#
# make fortran object files
#

abstim.o: abstim.for $(INCS)
	$(fc) abstim.for

auto_tr.o: auto_tr.for $(INCS)
	$(fc) auto_tr.for

auto_amp.o: auto_amp.for $(INCS)
	$(fc) auto_amp.for

autocoda.o: autocoda.for $(INCS)
	$(fc) autocoda.for

autofil.o: autofil.for $(INCS)
	$(fc) autofil.for

azibazi.o: azibazi.for $(INCS)
	$(fc) azibazi.for

bcd.o: bcd.for $(INCS)
	$(fc) bcd.for

bndpas.o: bndpas.for $(INCS)
	$(fc) bndpas.for

check_s.o: check_s.for $(INCS)
	$(fc) check_s.for

codecoutil.o: codecoutil.for $(INCS)
	$(fc) codecoutil.for

compdecomp.o: compdecomp.for $(INCS)
	$(fc) compdecomp.for

# LINUX
comp_linux.o: comp_linux.for $(INCS)
	$(fc) comp_linux.for


# SUNOS and SOLARIS
comp_sun.o: comp_sun.for $(INCS)
	$(fc) comp_sun.for

comp_unix.o: comp_unix.for $(INCS)
	$(fc) comp_unix.for

componen.o: componen.for $(INCS)
	$(fc) componen.for

conv_def.o: conv_def.for $(INCS)
	$(fc) conv_def.for

continuous.o: continuous.for $(INCS)
	$(fc) continuous.for

err_ellipse.o: err_ellipse.for $(INCS)
	$(fc) err_ellipse.for

eev_sub.o: eev_sub.for $(INCS)
	$(fc) eev_sub.for

fft.o: fft.for $(INCS)
	$(fc) fft.for

filename.o: filename.for $(INCS)
	$(fc) filename.for

findchan.o: findchan.for $(INCS)
	$(fc) findchan.for

findevin.o: findevin.for $(INCS)
	$(fc) findevin.for

ga_lib.o: ga_lib.for $(INCS)
	$(fc) ga_lib.for

get_baz.o: get_baz.for $(INCS)
	$(fc) get_baz.for

gse_io.o: gse_io.for $(INCS)
	$(fc) gse_io.for

gsesei_lib.o: gsesei_lib.for $(INCS)
	$(fc) gsesei_lib.for

gserespl.o: gserespl.for $(INCS)
	$(fc) gserespl.for

hypinv1.o: hypinv1.for $(INCS)
	$(fc) hypinv1.for

hypinv2.o: hypinv2.for $(INCS)
	$(fc) hypinv2.for

hypinv3.o: hypinv3.for $(INCS)
	$(fc) hypinv3.for

hypoloc.o: hypoloc.for $(INCS)
	$(fc) hypoloc.for

hypoloc1.o: hypoloc1.for $(INCS)
	$(fc) hypoloc1.for

hyposub1.o: hyposub1.for $(INCS)
	$(fc) hyposub1.for

hyposub2.o: hyposub2.for $(INCS)
	$(fc) hyposub2.for

hyposub3.o: hyposub3.for $(INCS)
	$(fc) hyposub3.for

hyposub4.o: hyposub4.for $(INCS)
	$(fc) hyposub4.for

hyposub6.o: hyposub6.for $(INCS)
	$(fc) hyposub6.for

hypo71sub.o: hypo71sub.for
	$(fc) hypo71sub.for

inc_id.o: inc_id.for $(INCS)
	$(fc) inc_id.for

indata.o: indata.for $(INCS)
	$(fc) indata.for

libsei.o: libsei.for $(INCS)
	$(fc) libsei.for

lsqlin.o: lsqlin.for $(INCS)
	$(fc) lsqlin.for

make_hypoc_brief.o: make_hypoc_brief.for $(INCS)
	$(fc) make_hypoc_brief.for

maxlik.o: maxlik.for $(INCS)
	$(fc) maxlik.for

mb_att.o: mb_att.for $(INCS)
	$(fc) mb_att.for

merge_f.o: merge_f.for $(INCS)
	$(fc) merge_f.for

mfhead.o: mfhead.for $(INCS)
	$(fc) mfhead.for

mul_spec.o: mul_spec.for $(INCS)
	$(fc) mul_spec.for

nortype.o: nortype.for $(INCS)
	$(fc) nortype.for

picsub.o: picsub.for $(INCS)
	$(fc) picsub.for

polos.o: polos.for $(INCS)
	$(fc) polos.for

rea.o: rea.for $(INCS)
	$(fc) rea.for

recfil.o: recfil.for $(INCS)
	$(fc) recfil.for

removedc.o: removedc.for $(INCS)
	$(fc) removedc.for

sac_dummy.o: sac_dummy.for
	$(fc) sac_dummy.for

sacsei_lib.o: sacsei_lib.for $(INCS)
	$(fc) sacsei_lib.for

sei_mes.o: sei_mes.for $(INCS)
	$(fc) sei_mes.for

sig_spec.o: sig_spec.for $(INCS)
	$(fc) sig_spec.for

seiplot.o: seiplot.for $(INCS)
	$(fc) seiplot.for

seisinc.o: seisinc.for $(INCS)
	$(fc) seisinc.for

sfilname.o: sfilname.for $(INCS)
	$(fc) sfilname.for

shead.o: shead.for $(INCS)
	$(fc) shead.for

sheads.o: sheads.for $(INCS)
	$(fc) sheads.for

spec_dist.o: spec_dist.for $(INCS)
	$(fc) spec_dist.for

stat_loc.o: stat_loc.for $(INCS)
	$(fc) stat_loc.for

svd.o: svd.for $(INCS)
	$(fc) svd.for

swap.o: swap.for $(INCS)
	$(fc) swap.for

syntsel.o: syntsel.for $(INCS)
	$(fc) syntsel.for

sys_resp.o: sys_resp.for $(INCS)
	$(fc) sys_resp.for

systime.o: systime.for $(INCS)
	$(fc) systime.for

tau.o: tau.for $(INCS)
	$(fc) tau.for

timerout.o: timerout.for $(INCS)
	$(fc) timerout.for

wave.o: wave.for $(INCS)
	$(fc) wave.for

xy_plot.o: xy_plot.for $(INCS)
	$(fc) xy_plot.for

 
#
# make c object files
#


respfil.o: respfil.c
	$(cc) respfil.c

seis_unix.o: seis_unix.c
	$(cc) seis_unix.c

sfil.o: sfil.c
	$(cc) sfil.c

xlib.o:xlib.c
	$(cc) xlib.c

xview_lib.o: xview_lib.c
	cc -g -c xview_lib.c -I/usr/openwin/include/

#
# make archive
#
seisan.a: $(SUBS) $(OTHERS) $(INCS)
	ar cr seisan.a $(SUBS) $(OTHERS)
	ranlib seisan.a

# 
# delete object files with option clean
#
clean:
	rm -f *.o *.a

