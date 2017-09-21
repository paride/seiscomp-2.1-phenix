#!/bin/bash

## Trap CTRL-C
trap 'echo "Aborted operation - Press RETURN"' 2

#########################################################################
#                         Procedures
#########################################################################
ShowBootLog() {

if [ -f /tmp/BOOT.log ]; then
	less  /tmp/BOOT.log
else
	echo "No boot logfile found ..."
fi

}

SelectConfs() {

CurDir=`pwd`
cd #home_sysop#/operator/download
List="`echo ??`"
cd $CurDir

}

SetXterm() {

#
# ------------------------------------------------------------------------------
# XTERM settings
# ------------------------------------------------------------------------------

export TERM=xterm
export TERMCAP=\
'{vs|xterm|vs100:cr=^M:do=^J:nl=^J:bl=^G:le=^H:ho=\E[H:co#80:li#65:cl=\E[H\E[2J:bs:am:cm=\E[%i%d;%dH:nd=\E[C:up=\E[A:ce=\E[K:cd=\E[J:so=\E[7m:se=\E[m:us=\E[4m:ue=\E[m:md=\E[1m:mr=\E[7m:me=\E[m:ku=\EOA:kd=\EOB:kr=\EOC:kl=\EOD:kb=^H:k1=\EOP:k2=\EOQ:k3=\EOR:k4=\EOS:ta=^I:pt:sf=\n:sr=\EM:al=\E[L:dl=\E[M:ic=\E[@:dc=\E[P:MT:ks=\E[?1h\E=:ke=\E[?1l\E>:is=\E[r\E[m\E[2J\E[H\E[?7h\E[?1;3;4;6l:rs=\E[r\E<\E[m\E[2J\E[H\E[?7h\E[?1;3;4;6l:xn:AL=\E[%dL:DL=\E[%dM:IC=\E[%d@:DC=\E[%dP:ti=\E7\E[?47h:te=\E[2J\E[?47l\E8:hs:ts=\E[?E\E[?%i%dT:fs=\E[?F:es:ds=\E[?E:}'

#

}

CountConf() {

SelectConfs

ConfCount=0
for stat in $List; do
	ConfCount=$[$ConfCount + 1]
done

}

ShutdownComserv() {

echo ""
echo "removing crontab entry ...\n `crontab -l`\n\n" 
echo ""
crontab -r

echo ""
echo "stopping all comserv processes ..."
echo ""

killall -2 netmon server datalog datadump > /dev/null 2>&1
sleep 10
killall -9 netmon server datalog datadump > /dev/null 2>&1

}

StartNetmon() {

echo ""
echo "installing crontab entry for Netmon ...."
echo ""
crontab #home_sysop#/operator/crontab_l
echo ""

}

#########################################################################
#                         Download Menu
#########################################################################

XTERM="n"
echo ""
echo -n 'Should i use xterm settings [y or n] ? '
read yn
echo ""
test -z "$yn" && yn=n
if test $yn = y || test $yn = yes
then
	SetXterm
	XTERM="y"
fi

CountConf

if [ "$XTERM" = "y" ]; then

#
# Use dialog to display the menu
#
MenuCount=$[$ConfCount + 4]

echo "dialog  --title \"Download Quanterra system\" --menu \\" > /tmp/XMenu
echo "\"\n Download System \n\"  15 76 $MenuCount \\"          >> /tmp/XMenu
for MLine in $List; do
echo "\"DOWNLOAD $MLine\"   \"`cat #home_sysop#/operator/download/$MLine/BOOT_MENU_LINE`\" \\" >> /tmp/XMenu
done
echo "\" \" \"\" \\" >> /tmp/XMenu
echo "\"SHOW LOG\"      \"Show boot logfile\" \\" >> /tmp/XMenu
echo "\"            \"  \"-----------------------------------------\" \\" >> /tmp/XMenu
echo "\"EXIT\"          \"Exit Program\" 2> /tmp/SeTmain"  >> /tmp/XMenu

while [ 0 ]; do
    
    source /tmp/XMenu

    if [ $? = 1 -o $? = 255 ]; then 	#Cancel choosen
        echo "EXIT" >/tmp/SeTmain		#Simulate EXIT
    fi
 
    MAINSELECT="`cat /tmp/SeTmain`"
    rm -f /tmp/SeTmain
    
    #----------------- Configuration 3g ---------------------
    for MCon in $List; do
    	if [ "$MAINSELECT" = "DOWNLOAD $MCon" ]; then
		ShutdownComserv
		echo ""
		clear
		cd #home_sysop#/operator/download/$MCon
		BOOT | tee /tmp/BOOT.log
		StartNetmon
    	fi
    done
    
    #-----------------   Show Boot Log  ---------------------
    if [ "$MAINSELECT" = "SHOW LOG" ]; then
	   ShowBootLog
    fi
    
    #---------EXIT----------
    if [ "$MAINSELECT" = "EXIT" ]; then
    	rm -f /tmp/SeT*
        break
    fi

done	

else
#
# Use line menu
#

MenuCount=$[$ConfCount + 4]
 
echo ""
echo "      Download Quanterra system"                      > /tmp/XMenu
echo "-----------------------------------------"           >> /tmp/XMenu
for MLine in $List; do
echo "$MLine    `cat #home_sysop#/operator/download/$MLine/BOOT_MENU_LINE`"          >> /tmp/XMenu
done
echo ""                                                    >> /tmp/XMenu
echo "s     Show boot logfile"                             >> /tmp/XMenu
echo "-----------------------------------------"           >> /tmp/XMenu
echo "e     Exit Program"                                  >> /tmp/XMenu
echo ""
 
while [ 0 ]; do
	cat /tmp/XMenu
	
	echo ""
	read -p "Enter selection: "  answer

	case "$answer" in
		e)
			exit
			;;
		s)
			ShowBootLog
			;;
		*)	
			for MCon in $List; do
				if [ "$answer" = "$MCon" ]; then
					ShutdownComserv
					echo ""
                			clear
                			cd #home_sysop#/operator/download/$MCon
                			BOOT | tee /tmp/BOOT.log
					StartNetmon
        			fi
    			done
	esac
done	

fi

exit
