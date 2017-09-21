# Sample chain_plugin extension to calculate offset and rms

import sys
import warnings
import math
from datetime import datetime
from seiscomp import mseed
from seiscomp.plugin import *
from Numeric import array

fd = get_input()
if fd == None:
    print "missing input stream"
    sys.exit(1)

for rec in mseed.Input(fd):
    sec = int(rec.time.sec)
    usec = int((rec.time.sec - sec) * 1000000)
    hdrtime = datetime(*rec.time.asDate[:5] + (sec, usec))

    try:
        a = array(rec.data, "d")
        offset = sum(a) / rec.nsamp
        rms = math.sqrt(sum((a - offset)**2) / rec.nsamp)

    except mseed.Exception, e:
        warnings.warn(str(e))

    send_log(rec.sta, hdrtime, "%s.%s.%s.%s: offset = %f, rms = %f" % 
        (rec.net, rec.sta, rec.loc, rec.cha, offset, rms))

