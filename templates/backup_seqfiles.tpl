#!/bin/bash

KEEP=30

find #home_sysop#/seq-backup -type f -mtime +$KEEP -exec rm -f '{}' \;

cd #home_sysop#/status
tar czf #home_sysop#/seq-backup/seq-`date +'%Y%m%d'`.tgz *.seq *.state

