#!/bin/bash

CFGDIR=#home_sysop#/config
LOGDIR=#home_sysop#/logs
PIDDIR=#home_sysop#/status

if [ "$SEISCOMP_CTRL" != locked ]; then
    while :; do
        SEISCOMP_CTRL=locked #home_sysop#/bin/run_with_lock \
            $PIDDIR/seiscomp_ctrl.pid bash -c "$0 $*"
        if [ $? -ne 255 ]; then exit $?; fi
        echo "...locked...trying again..."
        sleep 1
    done
fi

PATH=#home_sysop#/bin:$PATH

ACTION="$1"
shift

if [ $# -gt 0 ]; then
    STATIONS="$*"
else
    STATIONS=$(for f in $CFGDIR/rc_*; do echo "${f##*_}"; done)
fi

trap '' SIGHUP

case "$ACTION" in
    start | check)
        for s in $STATIONS; do
            if [ "$ACTION" = check -a ! -f $PIDDIR/$s.run ]; then
                continue
            fi

            touch $PIDDIR/$s.run
            
            if [ -f $CFGDIR/station_$s ]; then
                if trylock $PIDDIR/$s.comserv.pid; then
                    echo "starting comserv for station $s"
                    comserv $s >> $LOGDIR/$s.comserv.log 2>&1 &
                else
                    echo "comserv for station $s is running"
                fi
            fi

            if [ -f $CFGDIR/clients_$s ]; then
                cat $CFGDIR/clients_$s | while read line; do
                    name="${line%% *}"
                    cmd="${line#* }"
                    
                    test -z "$name" -o -z "$cmd" && continue
                    
                    if trylock $PIDDIR/$s.$name.pid; then
                        echo "starting client \"$name\" of station $s"
                        sh -c "$cmd" >> $LOGDIR/$s.$name.log 2>&1 &
                    else
                        echo "client \"$name\" of station $s is running"
                    fi
                done
            fi
        done

#DGS#        if trylock $PIDDIR/digiserv.pid; then
#DGS#            echo "starting digiserv"
#DGS#            digiserv -v \
#DGS#              -f $CFGDIR/digiserv.ini \
#DGS#              >> $LOGDIR/digiserv.log 2>&1 &
#DGS#        else
#DGS#            echo "digiserv is running"
#DGS#        fi
#DGS#
#SLK#        if trylock $PIDDIR/seedlink.pid; then
#SLK#            echo "starting seedlink"
#SLK#            seedlink -v \
#SLK#              -f $CFGDIR/seedlink.ini \
#SLK#              >> $LOGDIR/seedlink.log 2>&1 &
#SLK#        else
#SLK#            echo "seedlink is running"
#SLK#        fi
#SLK#
        ;;
    stop)
        comserv_is_running=false
        for s in $STATIONS; do
            if trylock $PIDDIR/$s.comserv.pid; then :; else
                echo "suspending link to $s"
                cs_suspend $s
                comserv_is_running=true
            fi
        done
        
        if $comserv_is_running; then
            echo -n "waiting 10 seconds "
            for x in . . . . . . . . . . ; do
                echo -n $x
                sleep 1
            done
            echo
        fi

#SLK#        if [ $# -eq 0 ]; then
#SLK#            if trylock $PIDDIR/seedlink.pid; then
#SLK#                echo "seedlink is not running"
#SLK#            else
#SLK#                echo "shutting down seedlink"
#SLK#                kill `cat $PIDDIR/seedlink.pid` >/dev/null 2>&1
#SLK#            fi
#SLK#            rm -f $PIDDIR/seedlink.pid
#SLK#        fi
#SLK#
#DGS#        if [ $# -eq 0 ]; then
#DGS#            if trylock $PIDDIR/digiserv.pid; then
#DGS#                echo "digiserv is not running"
#DGS#            else
#DGS#                echo "shutting down digiserv"
#DGS#                kill `cat $PIDDIR/digiserv.pid` >/dev/null 2>&1
#DGS#            fi
#DGS#            rm -f $PIDDIR/digiserv.pid
#DGS#        fi
#DGS#
        for s in $STATIONS; do
            if [ -f $CFGDIR/clients_$s ]; then
                cat $CFGDIR/clients_$s | while read line; do
                    name="${line%% *}"
                    cmd="${line#* }"
                    
                    test -z "$name" -o -z "$cmd" && continue
                    
                    if trylock $PIDDIR/$s.$name.pid; then
                        echo "client \"$name\" of station $s is not running"
                    else
                        echo "shutting down client \"$name\" of station $s"
                        kill `cat $PIDDIR/$s.$name.pid` >/dev/null 2>&1
                    fi
                    
                    rm -f $PIDDIR/$s.$cexec.pid
                done
            fi
            
            if [ -f $CFGDIR/station_$s ]; then
                if trylock $PIDDIR/$s.comserv.pid; then
                    echo "comserv for station $s is not running"
                else
                    echo "shutting down comserv for station $s"
                    kill `cat $PIDDIR/$s.comserv.pid` >/dev/null 2>&1
                fi
                
                rm -f $PIDDIR/$s.comserv.pid
            fi

            rm -f $PIDDIR/$s.run

        done
        ;;
    *)
        echo "Usage: $0 {start|stop|check}"
        exit 1
esac

