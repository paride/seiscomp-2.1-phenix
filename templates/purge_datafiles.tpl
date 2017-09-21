#!/bin/sh

CFGDIR=#home_sysop#/config
DATADIR=#home_sysop#/data
ARCHIVE=#home_sysop#/archive

for rc in $CFGDIR/rc_*; do
    station=${rc##*_}
    source $rc
    find $DATADIR/$station $ARCHIVE/*/$NET/$STATION -type f -follow -not -name \*.ini -mtime +$DATA_KEEP \
        -exec rm -f '{}' \;
done

