[seedlink]
*   Parameters recognized when data source is seedlink

*   Address of SeedLink server in hostname:port format
port=#srcaddr#:#srcport#

*   How many seconds to keep network down before bringing it up and making
*   another connection
standby=10

*   Maximum time to keep connected (0: no limit, positive value turns on
*   dialup mode)
* uptime=600

*   Shell command to bring network interface up. Note that the Comserv
*   server assumes that it can make a connection after the script finishes,
*   so you should ensure that this is the case. If making a connection
*   fails, then Comserv server runs ifdown and goes to standby state
*   immediately.
* ifup=( echo -n "ifup   "; date ) >> #home_sysop#/logs/#statid#.iflog

*   Shell command to take network interface down
* ifdown=( echo -n "ifdown "; date ) >> #home_sysop#/logs/#statid#.iflog

*   File where sequence number is stored
seqfile=#home_sysop#/status/#statid#.seq

*   Interval of saving the sequence number in seconds
seqsave=60

*   This script tells the time of last record in all channels. The resulting
*   timetable is used to ensure that there no overlaps (in case the saved
*   sequence number is not current)
* timetable_loader=#home_sysop#/bin/load_timetable #statid#

*   The "schedule" parameter is used to specify when Comserv server should
*   make Seedlink connections in a more flexible way. It overrides the
*   "standby" parameter above and uses a syntax idendical to the one used
*   by the cron utility (some code from Vixie Cron was borrowed to
*   implement this).  The fields are minute, hour, day of month, month, day
*   of week; please take a look at the crontab(5) manpage for more details.
*   Schedule makes sense only if you have set uptime to some value to turn
*   on dialup mode (the uptime value should normally be smaller than the
*   scheduling period).
*
*   For example, the following tells to make connection every 0-th and
*   30-th minute of each hour:
* schedule=0,30 * * * *

*   The "selectors" parameter tells to request packets that match given
*   selectors. This helps to reduce network traffic. A packet is sent to
*   client if it matches any positive selector (without leading "!") and
*   doesn't match any negative selectors (with "!"). General format of
*   selectors is LLSSS.T, where LL is location, SSS is channel, and T is
*   type (one of DECOTL for data, event, calibration, blockette, timing,
*   and log records). "LL", ".T", and "LLSSS." can be omitted, meaning
*   "any". It is also possible to use "?" in place of L and S. 
*
*   Some examples:
*   BH?            - BHZ, BHN, BHE (all record types)
*   BH?.D          - BHZ, BHN, BHE (data records)
*   BH? !E         - BHZ, BHN, BHE (excluding detection records)
*   BH? E          - BHZ, BHN, BHE plus detection records of all channels
*   !LCQ !LEP      - exclude LCQ and LEP channels
*   !L !T          - exclude log and timing records
* selectors=???

* Fall back to uni-station mode, if multi-station mode is not supported
unistation=yes

* Network ID
network=#netid#

*   Dial lock prevents more than one server to establish a
*   connection at the same time
* dial_lock=#home_sysop#/status/dial.pid

*   Following parameters have the same meaning in comlink and seedlink
*   mode
station=#statid#
segid=#segid#
lockfile=#home_sysop#/status/#statid#.comserv.pid
verbosity=1
pollusec=50000
databufs=200
detbufs=20
timbufs=20
calbufs=20
msgbufs=20
blkbufs=20
netto=900
netdly=10

* stream_renaming=BH?->SH?,MH?->BH?

*   It is strongly suggested to turn on flow control when using SeedLink (flow
*   control should not be turned on for clients which are not permanently
*   running).
#DLOG#client1=dlog,2400
#SLNK#client2=slnk,2400

uid0=32767

