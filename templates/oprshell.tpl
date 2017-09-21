#!/bin/bash

## Trap CTRL-C
trap 'exit' INT QUIT

VERSION=#version#

SYS_HOME=#home_sysop#

OPRPATH="$SYS_HOME/operator"
CFGPATH="$SYS_HOME/config"
BINPATH="$SYS_HOME/bin"
LOGPATH="$SYS_HOME/logs"
PIDPATH="$SYS_HOME/status"
DRMPATH="$SYS_HOME/operator"
OPRPATH="$SYS_HOME/operator"

PLOT="$BINPATH/slqplot"

DISPLAY=:0.0
export DISPLAY

#
# show stations configured for sysop
#

STATIONS=$(for f in $CFGPATH/rc_*; do echo "${f##*_}"; done)
STATID=""

ShowStations() {
    echo ""
    echo "Data for station(s)" $STATIONS "are available on this SeisComP system."
    echo ""
}

#
# Get Initials for logging purpose
#
GetInitials() {
    LOG_FILE="$OPRPATH/initials.log"
    echo ""
    echo -n "Please enter your initials ---> "
    read INITIALS
    echo "`date` - $INITIALS" >> "$LOGPATH/initials.log"
}

#
# Set station name we want to operate on
#
SelectStation() {
    if [ $(echo "$STATIONS" | wc -w) -eq 1 ]; then
        STATID="$STATIONS"
        source $SYS_HOME/config/rc_$STATID
        return
    fi
    
    echo

    empty_count=0
    while :; do
        echo -n "Select station ---> "
        read STATID
        echo
        
        if [ -z "$STATID" ]; then
            empty_count=$((empty_count + 1))
            test $empty_count -gt 9 && exit
            continue
        fi

        empty_count=0

        STATID="`echo $STATID | tr '[:lower:]' '[:upper:]'`"
        
        test $(echo $STATIONS | grep -i -c "\<$STATID\>") -eq 1 && break
        
        echo
        echo "Stations:" $STATIONS
    done

    source $SYS_HOME/config/rc_$STATID
}    

#
# remove crontab startup entry and shut down SeisComP
#

ShutdownSeiscomp() {
    echo
    echo "removing crontab entry ..."
    echo "`crontab -l`"
    echo
    crontab -r

    $BINPATH/seiscomp_ctrl stop
}

#
# Start SeisComP
#
 
StartSeiscomp() {
    echo
    $BINPATH/seiscomp_ctrl start
    echo
    echo "installing crontab entry ...."
    echo
    crontab $OPRPATH/crontab_l
}

#
# Start a single station
#
 
StartSingleStation() {
    echo
    echo "starting station $STATID...."
    echo
    $BINPATH/seiscomp_ctrl start $STATID
    echo
}

#
# Stop a single station
#
 
StopSingleStation() {
    echo
    echo "stopping station $STATID...."
    echo
    $BINPATH/seiscomp_ctrl stop $STATID
    echo
}

#TERM##
#TERM## Start kermit and connect to Quanterra master console
#TERM##
#TERM#
#TERM#ConnectToMasterTerm() {
#TERM#    kermit -l #termport# -b 9600 -C "connect,quit"
#TERM#}
#TERM#
#
# Escape to Linux shell
#

EscapeToShell() {
    bash
}

#DRM##
#DRM## Start Data Request Manager
#DRM##
#DRM#
#DRM#StartUserDrm() {
#DRM#    $DRMPATH/drm_usrshell    
#DRM#
#DRM#}
#DRM#
#TAPE##
#TAPE## Recycle old tape
#TAPE##
#TAPE#
#TAPE#RecycleTape() {
#TAPE#
#TAPE#DoIt="n"
#TAPE#TapeDevice="/dev/st0"
#TAPE#
#TAPE#echo ""
#TAPE#echo -n "Please enter tape device [return sets default: $TapeDevice] : "
#TAPE#read Device
#TAPE#
#TAPE#if [ "$Device" == "" ]; then
#TAPE#    Device="$TapeDevice"
#TAPE#else
#TAPE#    mt -f $Device status > /dev/null 2>&1
#TAPE#    if [ $? -ne 0 ]; then
#TAPE#        echo "Can't access tape device $Device ..."
#TAPE#        return
#TAPE#    fi
#TAPE#fi
#TAPE#
#TAPE#echo "do you really want to recycle this tape ?" 
#TAPE#echo "This will delete all data !!"
#TAPE#echo -n "Recycle tape y/(n) : "
#TAPE#read Answer
#TAPE#
#TAPE#case "$Answer" in
#TAPE#
#TAPE#    
#TAPE#    [yY])
#TAPE#        echo "Recyling tape in tape device $Device"
#TAPE#        echo "---------------- NULL Tape ---------------"  | dd of=$Device > /dev/null 2>&1
#TAPE#        echo "Tape recyled ..."
#TAPE#    ;;
#TAPE#    
#TAPE#    [nN])
#TAPE#        return
#TAPE#    ;;
#TAPE#    
#TAPE#    *)
#TAPE#        echo "Please answer y or n ..."
#TAPE#        return;
#TAPE#esac
#TAPE#
#TAPE#}
#TAPE#
#TAPE##
#TAPE## Eject tape in drive
#TAPE##
#TAPE#
#TAPE#EjectTape() {
#TAPE#
#TAPE#echo ""
#TAPE#echo -n "Do you really want to eject all tape(s) ? y/(n) :"
#TAPE#read Answer
#TAPE#
#TAPE#case "$Answer" in
#TAPE#
#TAPE#    [yY])
#TAPE#        echo "Ejecting all tape(s) in drive(s) .."
#TAPE#        isdump=`ps auxwww | grep -v grep | grep -c datadump`
#TAPE#        if [ $isdump -eq 0 ]; then
#TAPE#            echo "No datadump process is running, please eject the tape manually ..."
#TAPE#            return
#TAPE#        fi
#TAPE#        killall -HUP datadump > /dev/null 2>&1
#TAPE#        echo "Tape(s) will be ejected ..."
#TAPE#    ;;
#TAPE#    
#TAPE#    [nN])
#TAPE#        return
#TAPE#    ;;
#TAPE#    
#TAPE#    *)
#TAPE#        echo "Please answer y or n ..."
#TAPE#        return;
#TAPE#esac
#TAPE#
#TAPE#}
#TAPE#
#TAPE#TapeStatus() {
#TAPE#            
#TAPE#TapeDevice="/dev/nst0"
#TAPE#            
#TAPE#echo ""
#TAPE#echo -n "Please enter tape device [return sets default: $TapeDevice] : "
#TAPE#read Device
#TAPE# 
#TAPE#if [ "$Device" == "" ]; then
#TAPE#        Device="$TapeDevice"
#TAPE#fi
#TAPE# 
#TAPE#mt -f $Device status > /dev/null 2>&1
#TAPE#if [ $? -ne 0 ]; then
#TAPE#        echo "Can't access tape device $Device ..."
#TAPE#        return
#TAPE#fi
#TAPE#
#TAPE#if test -f $LOGPATH/active_log; then
#TAPE#    LOGCOUNT=`tail -1 $LOGPATH/active_log | tr -s ' ' ' ' | cut -d" " -f1`
#TAPE#    LOGSIZE=`tail -1 $LOGPATH/active_log | tr -s ' ' ' ' | cut -d" " -f3`
#TAPE#        LOGSIZE=`echo $LOGSIZE | awk -F: '{printf "%f", $1/1000000}'`
#TAPE#        
#TAPE#    Status=`mt -f $Device status`
#TAPE#    if [ $? -ne 0 ]; then
#TAPE#        echo "Drive busy or no tape in drive ..."
#TAPE#        return;
#TAPE#    fi
#TAPE#        echo "=-=-=-=-(TAPE STATUS)=-=-=-=-"
#TAPE#        echo "current file number: $LOGCOUNT"
#TAPE#        echo "written MB to tape : $LOGSIZE"
#TAPE#        echo ""
#TAPE#        mt -f $Device status
#TAPE#        echo ""
#TAPE#else
#TAPE#    echo ""
#TAPE#    echo "No data logfile found in log directory ..."
#TAPE#    echo ""
#TAPE#    echo "=-=-=-=-(TAPE STATUS)=-=-=-=-"
#TAPE#    echo ""
#TAPE#    mt -f $Device status
#TAPE#    echo ""
#TAPE#    echo ""
#TAPE#fi
#TAPE#
#TAPE#}
#TAPE#
#DOWN#DownLoadUSHEAR() {
#DOWN#    echo ""
#DOWN#    echo "Reset Quanterra..."
#DOWN#    $SYS_HOME/bin/QReset #termport#
#DOWN#    $SYS_HOME/bin/QReset #comport#
#DOWN#    $SYS_HOME/operator/DownLoad
#DOWN#}
#CDREC#CdrMenu() {
#CDREC#    echo ""
#CDREC#    $HOME/grfsoft/prog/cd_menu.csh
#CDREC#}
#MULT##
#MULT## Select an other station
#MULT##
#MULT#
#MULT#SwitchToOtherStation() {
#MULT#    ShowStations
#MULT#    SelectStation
#MULT#}
#MULT#
PlotXWindow() {
    $PLOT  -i -c xslqplot -f $SYS_HOME/config/slqplot_${STATID} -F $SYS_HOME/config/slqplot.coef -S ${NET}_${STATION} ${SRCADDR}:${SRCPORT}  &
    echo ""
    echo "plotting in X-Window started ..."
}

KillPid() {
    this_pid=`echo $this_line | tr -s ' ' ' ' | cut -d" " -f2`
    echo
    echo "killing PID $this_pid ..."
    kill $this_pid
}

KillXWindow() {
    ps auxwwww | grep "$PLOT" | grep ${NET}_${STATION} | grep -v grep | while read this_line; do
        if [ "`echo $this_line | grep -c xslqplot`" != "0" ]; then
            KillPid
        fi
    done
}

ChangeDisplay () {
    echo -n "Old DISPLAY host: $DISPLAY - new one: "
    read DISPLAY
}

welcome () {
    if test -f $OPRPATH/welcome; then
        clear
        echo
        echo "       ============( SeisComP Version $VERSION )============"
        echo
        cat $OPRPATH/welcome
        echo
        echo "       ================================================"
        echo
    fi
    ShowStations
    GetInitials
    SelectStation
}

#
# Start - Stop comserv processes menu
#

StartStopSeiscomp() {
    empty_count=0
    while :; do
        cat << END_MENU

    SOM Aquisition Control Menu
    
    s - Start data aquisition
    k - Stop data aquisition
    b - Start current station
    t - Stop current station
    
    q - Return to main menu
    
END_MENU

        echo -n "Command [$STATID] ---> "
        read MENU_COMMAND

        case "$MENU_COMMAND" in
            q)
                return
                ;;
    
            s)
                StartSeiscomp
                ;;
    
            k)
                ShutdownSeiscomp
                ;;
    

            b)
                StartSingleStation
                ;;

            t)
                StopSingleStation
                ;;

            "")
                empty_count=$((empty_count + 1))
                test $empty_count -gt 9 && exit
                continue
                ;;
            *)
                echo
                echo "Command [$MENU_COMMAND] not found ..."
                echo
        esac

        empty_count=0
    done
}

#TAPE##
#TAPE## Tape control options
#TAPE##
#TAPE#    
#TAPE#TapeControl() {
#TAPE#
#TAPE#empty_count=0
#TAPE#
#TAPE#while :; do
#TAPE#
#TAPE#cat << TAPE
#TAPE#
#TAPE#    SOM Tape Control Menu
#TAPE#    
#TAPE#    e - Eject tape
#TAPE#    r - Recycle tape
#TAPE#    s - Tape status
#TAPE#    
#TAPE#    q - Return to main menu
#TAPE#
#TAPE#TAPE
#TAPE#
#TAPE#echo -n "Command [$STATID] ---> "
#TAPE#read TAPE_COMMAND
#TAPE#
#TAPE#case "$TAPE_COMMAND" in
#TAPE#
#TAPE#    q)
#TAPE#        return
#TAPE#    ;;
#TAPE#    
#TAPE#    e)
#TAPE#        EjectTape
#TAPE#    ;;
#TAPE#    
#TAPE#    r)
#TAPE#        RecycleTape
#TAPE#    ;;
#TAPE#    
#TAPE#    s)
#TAPE#        TapeStatus
#TAPE#    ;;
#TAPE#        
#TAPE#  "")
#TAPE#      empty_count=$((empty_count + 1))
#TAPE#      test $empty_count -gt 9 && exit
#TAPE#      continue
#TAPE#  ;;
#TAPE#    
#TAPE#    *)
#TAPE#    echo ""
#TAPE#    echo "Command [$TAPE_COMMAND] not found ..."
#TAPE#    echo ""
#TAPE#esac
#TAPE#
#TAPE#empty_count=0
#TAPE#done
#TAPE#}
#TAPE#
#
# Plot menu
#

Plotting() {
    empty_count=0
    while :; do
        cat << END_MENU

    SOM Plot Menu
    
    x - Start slqplot in X-window
    X - Stop slqplot in X-window
    h - Change display host

    q - Return to main menu
   
END_MENU

        echo -n "Command [$STATID] ---> "
        read MENU_COMMAND

        case "$MENU_COMMAND" in

            q)
                return    
                ;;
    
            x)
                PlotXWindow
                ;;
    
            X)
                KillXWindow
                ;;
    
            h)
                ChangeDisplay
                ;;
    
            "")
                empty_count=$((empty_count + 1))
                test $empty_count -gt 9 && exit
                continue
                ;;
            *)
                echo
                echo "Command [$MENU_COMMAND] not found ..."
                echo
        esac

        empty_count=0
    done
}

# Show main menu

welcome

empty_count=0
while :; do
    cat << END_MENU

    SOM Main Menu
    
    a - Control data aquisition
    p - Control monitor plots
#TERM#    c - Connect to Quanterra master console
#DOWN#    d - Reset Quanterra and download system
    o - Escape to Linux shell
#TAPE#    t - Control tape recording
#CDREC#    r - CD Menu
#DRM#    u - Start user DRM
#MULT#    w - Switch to other station

    q - quit
    
END_MENU

    echo -n "Command [$STATID] ---> "
    read MENU_COMMAND

    case "$MENU_COMMAND" in
        a)
            StartStopSeiscomp
            ;;
#TERM#        c)
#TERM#            ConnectToMasterTerm
#TERM#            ;;
#DOWN#        d)
#DOWN#            DownLoadUSHEAR
#DOWN#            ;;
        o)
            EscapeToShell
            ;;
        p)    
            Plotting
            ;;
#TAPE#        t)
#TAPE#            TapeControl
#TAPE#            ;;
#CDREC#        r)
#CDREC#            CdrMenu
#CDREC#            ;;
#DRM#        u)
#DRM#            StartUserDrm
#DRM#            ;;
#MULT#        w)
#MULT#            SwitchToOtherStation
#MULT#            ;;
        q)
            exit 0
            ;;
        "")
            empty_count=$((empty_count + 1))
            test $empty_count -gt 9 && exit
            continue
            ;;
        *)
            echo ""
            echo "Command [$MENU_COMMAND] not found ..."
            echo ""
    esac

    empty_count=0
done

