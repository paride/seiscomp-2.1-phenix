[comlink]
*   Parameters recognized when data source is comlink

*   Serial port, baud, parity
port=#comport#
baud=19200
parity=no

*   Enable RTS/CTS flow control
flow=no

*   TCP or UDP port accepting connections from the DA
* ipport=60000

*   IP address of the DA for UDP connections
* udpaddr=

*   Shared memory segment ID (must be unique for every station)
segid=#segid#

*   Lockfile prevents more than one copy of the program running for
*   a single station (mandatory for check_seiscomp)
lockfile=#home_sysop#/status/#statid#.comserv.pid

*   Verbosity controls how much information is logged:
*   0  error messages
*   1  informational messages
*   2  one line for each packet received from the DA
verbosity=1

*   Station name
station=#statid#

*   Override station name from the DA with the one given above or on the
*   command line
override=no

*   If "anystation" is on, don't check station name from the DA
anystation=yes

*   If "noultra" is on, don't poll for ultra packets
noultra=no

*   If "mshear" is on, use channel name and location in comment and clock
*   correction packet
mshear=no

*   Channel name and location of log records in [LOCATION-]CHANNEL format
*   (if mshear=no)
log_seed=LOG

*   Channel name and location of timing records in [LOCATION-]CHANNEL format
*   (if mshear=no)
timing_seed=ACE

*   Service clients and check input every "pollusec" usec
pollusec=50000

*   Number of buffers for data, detection, timing, calibration, message
*   and blockette records.
databufs=200
detbufs=20
timbufs=20
calbufs=20
msgbufs=20
blkbufs=20

*   Reconfigure link after "reconfig" sequence errors
reconfig=50

*   Network timeout in seconds
netto=120

*   Network reconnect delay in seconds
netdly=30

*   Maximum window size for outgoing packets
grpsize=1

*   Seconds to wait before sending window with less than "grpsize" packets
grptime=5

*   Link retry in seconds
linkretry=10

*   Stream renaming
* stream_renaming=BH?->SH?,MH?->BH?

*   List of reserved clients

*   There are four types of clients:
*   Reserved clients - clients which have a reserved slot in the client table
*     and service queue. Only one instance of a reserved client can run at one
*     time. Reserved clients must have the same UID as server.
*   Blocking clients - reserved clients with flow control turned on. Blocking
*     clients are unblocked (eg. made normal reserved clients) if they don't
*     bother the server in the specified timeout period.
*   Foreign clients - clients which have different UID than the server. Foreign
*     clients are removed from the client table if they don't bother the
*     server in 60 seconds.
*   Transient clients - clients which have the same UID as the server, but are
*     not listed as reserved clients. Transient clients are removed from the
*     client table when they exit.

*   Reserved clients are listed in the form client[N]=name[,timeout], where
*   N is an arbitrary number ([] denote optional parts) and "name" is
*   client name. Optional timeout value turns on flow control. Flow control
*   should not be turned on for clients which are not permanently running.
#DLOG#client1=dlog,2400
#SLNK#client2=slnk,2400

*   Privileges are given in the form uidN=MASK, where N is UID of the client
*   and MASK is privilege mask. Privileges are checked only if client UID
*   differs from server UID. Comserv commands and privileges are listed in the
*   file "commands.txt". Privileges should not be taken too seriously, because
*   it is trivial to fake the client UID.
uid0=32767

