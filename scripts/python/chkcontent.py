#***************************************************************************** 
# chkcontent.py
#
# Checks SDS content files and sends a daily summary of data quality to
# specified e-mail address
#
# (c) 2004 Andres Heinloo, GFZ Potsdam
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any later
# version. For more information, see http://www.gnu.org/
#*****************************************************************************

import time
import sys
import os.path
import cPickle
from datetime import datetime, timedelta
from seiscomp.utils import *
from seiscomp.qc.metadata import *

SERVER = "st25"
EMAIL = "your@mail.here"
CONTENT_DIR = "/home/sysop/content"
STATUS_DIR = "/home/sysop/status"
TQ_STREAMS = ("BHZ",)
STREAMS = ("BHZ", "BHN", "BHE")
SCANDAYS = 365

class MyError(Exception):
    pass

class Content(object):
    pass

rx_meta = re.compile("([A-Z0-9]{1,2})\\.([A-Z0-9]{1,5})\\." \
    "([A-Z0-9]{0,2})\\.([A-Z0-9]{1,3})\\.([A-Z0-9])\\.([0-9]{4})\\." \
    "([0-9]{3})\.([a-z]*)\.xml$")

curtime = time.time()
expected_end = datetime(*time.gmtime(curtime)[:3])
expected_begin = expected_end - timedelta(days=1)

station_content = {}

def process_metafile(f):
    m = rx_meta.match(os.path.basename(f))
    if m == None:
        raise MyError, "invalid filename: " + f

    net, sta, loc, cha, stype, year, yday, ftype = m.groups()
    if ftype != "meta":
        return

    if (net, sta, None) in station_content:
        del station_content[(net, sta, None)]
    
    meta = read_metadata(f)

    c = station_content.get((net, sta, loc))
    if c == None:
        c = Content()
        c.begin = {}
        c.end = {}
        c.timespans = {}
        c.qcparams = {}
    
    for ts in meta.timespan:
        if c.begin.get(cha[2]) == None or c.begin.get(cha[2]) > ts.begin:
            c.begin[cha[2]] = ts.begin

        if c.end.get(cha[2]) == None or c.end.get(cha[2]) < ts.end:
            c.end[cha[2]] = ts.end
        
        alerts = []
        
        for sn in ts.signal:
            if sn.offset > 10000:
                alerts.append("offset>10000")
            elif sn.offset < -10000:
                alerts.append("offset<-10000")

            if sn.rms < 10:
                alerts.append("rms<10")
            elif sn.rms > 10000:
                alerts.append("rms>10000")

            if ts.end == c.end[cha[2]]:
                c.qcparams["offset_" + cha[2]] = sn.offset
                c.qcparams["rms_" + cha[2]] = sn.rms
                
        if cha in TQ_STREAMS:
            for tq in ts.timing:
		if ts.end == c.end[cha[2]] and tq.q_avg != 0:
                    c.qcparams["tq"] = tq.q_avg

        if cha in TQ_STREAMS or len(alerts) > 0:
            for tq in ts.timing:
                if tq.q_avg < 20:
                    alerts.append("tq<20%")
                elif tq.q_avg < 40:
                    alerts.append("tq<40%")

        if cha in TQ_STREAMS or len(alerts) > 0:
            c.timespans[(ts.begin, ts.end, cha)] = alerts

    station_content[(net, sta, loc)] = c

def scan_metafiles():
    def scan_stream(d):
        for file in os.listdir(d):
            p = d + "/" + file
            if os.path.getmtime(p) > curtime - SCANDAYS * 24 * 60 * 60:
                process_metafile(p)
        
    def scan_station(d):
        for stream in STREAMS:
            p = d + "/" + stream + ".D"
            if os.path.exists(p):
                scan_stream(p)
    
    def scan_net(d, net):
        for sta in os.listdir(d):
#           if (net, sta) not in station_content:
#               station_content[(net, sta, None)] = None

            scan_station(d + "/" + sta)
    
    def scan_year(d):
        for net in os.listdir(d):
            scan_net(d + "/" + net, net)
        
    for year in os.listdir(CONTENT_DIR):
        scan_year(CONTENT_DIR + "/" + year)

def report_status(ofd, net, sta, loc, content):
    if content == None or loc == None or content.end.get("Z") == None:
        print >>ofd, "%-2s %-5s no data" % (net, sta)
        return
        
    pickle_file = (STATUS_DIR + "/" + net + "." + sta + "." + loc + ".tt")
    if os.path.exists(pickle_file):
        fd = open(pickle_file, "r")
        saved_content = cPickle.load(fd)
        fd.close()

        content.begin = saved_content.begin
        if content.end == None:
            content.end = saved_content.end
    else:
        saved_content = Content()
        saved_content.begin = {}
        saved_content.end = {}
        saved_content.timespans = {}
        saved_content.qcparams = {}

    timespan_list = filter(lambda x: x[0] not in saved_content.timespans, \
        content.timespans.items())

    if loc == "":
        locstr = ""
    else:
        locstr = " (" + loc + ")"
                
    if len(timespan_list) == 0:
        print >>ofd, "%-2s %-5s%s stopped at %s" % (net, sta, locstr, \
            format_datetime(content.end["Z"]))
    else:
        timespan_list.sort()

        tq = content.qcparams.get("tq")
        if tq == None:
            tqstr = "N/A"
        else:
            tqstr = str(tq) + "%"
        
        ofs = (content.qcparams.get("offset_Z"), \
            content.qcparams.get("offset_N"), \
            content.qcparams.get("offset_E"))

        ofsstr = 3 * ["N/A"]
        for i, x in enumerate(ofs):
            if x != None:
                ofsstr[i] = str(x)

        rms = (content.qcparams.get("rms_Z"), \
            content.qcparams.get("rms_N"), \
            content.qcparams.get("rms_E"))

        rmsstr = 3 * ["N/A"]
        for i, x in enumerate(rms):
            if x != None:
                rmsstr[i] = str(x)

        have_incomplete_timespans = False
        have_alerts = False
        for (begin, end, cha), alerts in timespan_list:
            if abs(begin - expected_begin) > timedelta(minutes=1) or \
                abs(end - expected_end) > timedelta(minutes=1):
                have_incomplete_timespans = True
            if len(alerts) > 0:
                have_alerts = True
                
        if not have_incomplete_timespans and not have_alerts:
            print >>ofd, "%-2s %-5s%s OK (tq=%s, offset={%s}, " \
                "rms={%s})" % \
                (net, sta, locstr, tqstr, ",".join(ofsstr), \
                ",".join(rmsstr))
        else:
            if have_alerts:
                print >>ofd, "%-2s %-5s%s QC alert! (tq=%s, " \
                    "offset={%s}, rms={%s})" % \
                    (net, sta, locstr, tqstr, ",".join(ofsstr), \
                    ",".join(rmsstr))
            else:
                print >>ofd, "%-2s %-5s%s incomplete/delayed (tq=%s, " \
                    "offset={%s}, rms={%s})" % \
                    (net, sta, locstr, tqstr, ",".join(ofsstr),
                    ",".join(rmsstr))
                
            for (begin, end, cha), alerts in timespan_list:
                if len(alerts) > 0:
                    print >>ofd, "         %-3s %s - %s %s" % \
                        (cha, format_datetime(begin), \
                        format_datetime(end), ", ".join(alerts))
                else:
                    print >>ofd, "         %-3s %s - %s" % \
                        (cha, format_datetime(begin), format_datetime(end))

    fd = open(pickle_file, "w")
    cPickle.dump(content, fd, True)
    fd.close()

scan_metafiles()
station_list = station_content.items()
station_list.sort()

fd = os.popen("mail -s 'Daily network status report (" + SERVER + ")' " + \
    EMAIL, "w")

for (net, sta, loc), content in station_list:
    report_status(fd, net, sta, loc, content)
    print >>fd

fd.close()

