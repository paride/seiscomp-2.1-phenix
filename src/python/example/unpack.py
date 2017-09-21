# Sample chain_plugin extension to demonstrate send_raw() usage.
# For a triggering example, see seiscomp/trigger.py

import sys
from datetime import datetime
from seiscomp import mseed
from seiscomp.plugin import *

fd = get_input()
if fd == None:
    print "missing input stream"
    sys.exit(1)

if not have_command_stream():
    print "missing command stream"
    sys.exit(1)

for rec in mseed.Input(fd):
    sec = int(rec.time.sec)
    usec = int((rec.time.sec - sec) * 1000000)
    hdrtime = datetime(*rec.time.asDate[:5] + (sec, usec))
    send_raw(rec.sta + "1", rec.loc + "." + rec.cha, hdrtime, 0,
        rec.time_quality, rec.data)

