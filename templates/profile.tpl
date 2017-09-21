#!/bin/bash

PATH=#home_sysop#/bin:$(echo $PATH | sed -e "s%^#home_sysop#/bin:%%g")
export PATH
trap -

LANG=en_US
umask 002

if [ "`tty`" != "/dev/console" -a "`tty`" != "/dev/tty1" ]; then
	cd #home_sysop#/operator
	./oprshell
	exit 0
fi

