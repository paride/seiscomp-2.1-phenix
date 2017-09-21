#***************************************************************************** 
# chkdata.py
#
# Updates SDS content files
#
# (c) 2004 Andres Heinloo, GFZ Potsdam
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any later
# version. For more information, see http://www.gnu.org/
#*****************************************************************************

import time
import warnings
import os.path
from datetime import datetime, timedelta
from seiscomp import mseed
from seiscomp.utils import *
from seiscomp.qc.cc import *

DATA_DIR = "/home/sysop/archive"
META_DIR = "/home/sysop/content"
SCANMINS = 120

curtime = time.time()
dt_now = datetime.utcfromtimestamp(curtime)
dt_today = datetime(*time.gmtime(curtime)[:3])
dt_yesterday = dt_today - timedelta(days=1)

_rx_sds = re.compile("([A-Z0-9]{1,2})\\.([A-Z0-9]{1,5})\\." \
    "([A-Z0-9]{0,2})\\.([A-Z0-9]{1,3})\\.([A-Z0-9])\\.([0-9]{4})\\." \
    "([0-9]{3})$")

def MSeedInput(fpath):
    """hack to handle non-Mini-SEED data"""
    fd = open(fpath, "r")
    
    while True:
        buf = fd.read(512)
        if len(buf) == 0:
            fd.close()
            break
            
        if buf[6] != "D" or buf[54] != "\x09":
            warnings.warn(fpath + ": skipping SEED record")
            continue

        try:
            yield mseed.Record(buf)
        except (mseed.Exception, IOError, NameError), e:
            warnings.warn(fpath + ": error decoding record header")
            warnings.warn(str(e))

for data_path, dirs, files in os.walk(DATA_DIR):
    for fname in files:
        m = _rx_sds.match(os.path.basename(fname))
        if m == None:
            warnings.warn("invalid filename: " + fname)
            continue

        net, sta, loc, cha, stype, year, yday = m.groups()
        ftime = datetime(*time.strptime(year+"."+yday, "%Y.%j")[:3])

        if ftime == dt_today:
            continue

        fpath = data_path + "/" + fname
        if os.path.getmtime(fpath) > curtime - SCANMINS * 60 or \
            (dt_now.hour == 0 and ftime == dt_yesterday):
            cc = ContentChecker(META_DIR, (net, sta, loc, cha, stype), ftime)
            for rec in MSeedInput(fpath):
                try:
                    cc.feed(rec)
                except (mseed.Exception, IOError, NameError), e:
                    warnings.warn(fpath + ": error decoding data")
                    warnings.warn(str(e))

            cc.close()

