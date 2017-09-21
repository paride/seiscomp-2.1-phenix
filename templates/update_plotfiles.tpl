#!/bin/bash

CFGDIR=#home_sysop#/config
PLTDIR=#home_sysop#/plotfiles

PATH=#home_sysop#/bin:$PATH

for rc in $CFGDIR/rc_*; do
    station=${rc##*_}
    if [ -f $PLTDIR/$station.active ]; then
        source $rc
        tek2gif -r$PLOT_SIZE $PLTDIR/$station.active $PLTDIR/$station.active.gif
    fi
done

