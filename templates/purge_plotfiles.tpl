#!/bin/bash

CFGDIR=#home_sysop#/config
PLTDIR=#home_sysop#/plotfiles

for rc in $CFGDIR/rc_*; do
    station=${rc##*_}
    source $rc
    find $PLTDIR -type f -follow -name $station.\*.gif -mtime +$PLOT_KEEP \
        -exec rm -f '{}' \;
done

