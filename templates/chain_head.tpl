<?xml version="1.0" standalone="yes"?>

<chain
#SLNK#  timetable_loader="#home_sysop#/bin/load_timetable 127.0.0.1:#localport#"
#SLNK#  overlap_removal="initial"
  multistation="yes"
  netto="900"
  netdly="10"
  standby="10"
  keepalive="0"
  seqsave="60">

#TRIG#  <extension
#TRIG#    name = "trigger"
#TRIG#    filter = ".*_HH._D"
#TRIG#    cmd = "python #home_sysop#/python/trigger_ext.py"
#TRIG#    recv_timeout = "0"
#TRIG#    send_timeout = "60"
#TRIG#    start_retry = "60"
#TRIG#    shutdown_wait = "10"/>
#TRIG#
