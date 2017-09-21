#!/bin/bash

CFGDIR=#home_sysop#/config
PLTDIR=#home_sysop#/plotfiles

PATH=#home_sysop#/bin:$PATH

STATID="$2"

if [ -f $PLTDIR/$STATID.stamp ]; then
    TIMESTAMP=`cat $PLTDIR/$STATID.stamp`

    if [ "$TIMESTAMP" != "${1%????}" ]; then
        source $CFGDIR/rc_$STATID
        tek2gif -r$PLOT_SIZE $PLTDIR/$STATID.active $PLTDIR/$STATID.$TIMESTAMP.gif
        echo "${1%????}" > $PLTDIR/$STATID.stamp
        rm -f $PLTDIR/$STATID.active
    fi
else
    echo "${1%????}" > $PLTDIR/$STATID.stamp
fi < /dev/null

cat > $PLTDIR/$STATID.active

