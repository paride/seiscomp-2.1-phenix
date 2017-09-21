[netm]
server=#home_sysop#/bin/comserv
* datalog may have -vN option for verbose mode.
* client1=dlog,#home_sysop#/bin/datalog 
#TAPE#* client2=tape,#home_sysop#/bin/datadump
* State: A=auto-restart S=start-once R=runable N=non-runable I=ignore
state=A

[tape]
device=#tape_device#
logdir=#home_sysop#/logs
datadir=#home_sysop#/data/#statid#
lockfile=#home_sysop#/status/#statid#.datadump.pid
* tapesize=[x MB]
tapesize=1800
* retries for writing data to tape
retry=5
* default size for all streams
defdisksize=#defsize#
* disk size for stream,[x MB]
*disksize=BH?.D,40
*disksize=LH?.D,10
* streams to dump (dumpstream=* for all streams)
*dumpstream=BHE.D
*dumpstream=BHZ.D
*dumpstream=B??.?
*dumpstream=LH?.?
dumpstream=*
sleep=1

*[shear]
*network= 'fill in your netwok code'
*format=Q512
*vbb=bhz,bhn,bhe
*lp=lhz,lhn,lhe,lx1,lx2,lx3,lx4,lx5,lx6
*vlp=vhz,vhn,vhe,vx1,vx2,vx3,vx4,vx5,vx6
*ulp=uhz,uhn,uhe,ux1,ux2,ux3,ux4,ux5,ux6

[dlog]
* Pathnames for data directory, program, and pid file.
dir=#home_sysop#/data/#statid#
program=#home_sysop#/bin/datalog
lockfile=#home_sysop#/status/#statid#.datalog.pid
* Selector specifies general selectors for data, detections, and calibrations.
* Specific type selector lines set the data mask (y|n) for that type,
* and optionally set specific selectors for that data type.
selector=???
data_selector=y
detection_selector=y
calibration_selector=y
timing_selector=y
log_selector=y
*
limit=1d
data_limit=1d,BH?
data_limit=1d,CL?,DP?
data_limit=1d,HH?,HL?
data_limit=1d,V??,U??
data_limit=1d,A??,
detection_limit=1d
calibration_limit=1d
timing_limit=1d
log_limit=1d
*
data_ext=D
detection_ext=E
calibration_ext=C
timing_ext=T
log_ext=L
* Format specifier for filenames:
* %S=STATION %s=station %N=NET %n=net %C=CHAN %c=chan %X=EXTENSION %x=extention
* %Y=year %y=yr %j=doy %m=month %d=day %H=hr %M=min
filename_format=%S.%N.%C.%X.%Y.%j.%H%M
