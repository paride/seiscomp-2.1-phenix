#***************************************************************************** 
# cc.py
#
# A Python class for checking the content of Mini-SEED data
#
# (c) 2004 Andres Heinloo, GFZ Potsdam
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any later
# version. For more information, see http://www.gnu.org/
#*****************************************************************************

import math
import warnings
import os.path
from datetime import datetime, timedelta
from Numeric import array
from seiscomp import mseed
from seiscomp.utils import *
from seiscomp.qc.metadata import *

def min_gap(fsamp):
    return timedelta(microseconds=1000000/(fsamp * 10))

class ContentChecker(object):
    def __init__(self, basedir, stream_id, timestamp):
        self.basedir = basedir
        self.stream_id = stream_id
        self.timestamp = timestamp
        self.begin_segment = None
        self.end_segment = None
        self.tq_min = 100
        self.tq_max = 0
        self.tq_sum = 0
        self.tq_nrecords = 0
        self.sumx = 0.0
        self.sumd2 = 0.0
        self.nsamp = 0
        self.metadata = Metadata()

    def feed(self, rec):
        sec = int(rec.time.sec)
        usec = int((rec.time.sec - sec) * 1000000)
        try:
            hdrtime = datetime(*rec.time.asDate[:5] + (sec, usec))
        except ValueError, e:
            warnings.warn(str(rec.time) + ": " + str(e))
            return

        if self.begin_segment == None:
            self.begin_segment = hdrtime

        if self.stream_id[4] == "D":
            if self.end_segment != None and \
                abs(hdrtime - self.end_segment) > min_gap(rec.fsamp):
                ts = Timespan(self.begin_segment, self.end_segment)
                self.begin_segment = hdrtime

                if self.tq_nrecords != 0:
                    ts.add_qcobj(Timing(self.tq_min, self.tq_max, \
                        self.tq_sum/self.tq_nrecords))
                    self.tq_min = 100
                    self.tq_max = 0
                    self.tq_sum = 0
                    self.tq_nrecords = 0
                
                if self.nsamp != 0:
                    offset = self.sumx / self.nsamp
                    rms = math.sqrt(self.sumd2 / self.nsamp)
                    ts.add_qcobj(Signal(offset, rms))
                    self.sumx = 0.0
                    self.sumd2 = 0.0
                    self.nsamp = 0
                    
                self.metadata.add_timespan(ts)

            try:
                tq = rec.time_quality
                if tq != None:
                    if tq < self.tq_min:
                        self.tq_min = tq

                    if tq > self.tq_max:
                        self.tq_max = tq

                    self.tq_sum += tq
                    self.tq_nrecords += 1
            
            except AttributeError:
                pass

            try:
                a = array(rec.data, "d")
                sumx = sum(a)
                self.nsamp += rec.nsamp
                self.sumx += sumx
                self.sumd2 += sum((a - sumx / rec.nsamp)**2)
            except mseed.Exception, e:
                warnings.warn(str(e))
                
        if rec.nsamp != 0 and rec.fsamp != 0:
            self.end_segment = hdrtime + \
                timedelta(microseconds = 1000000 * rec.nsamp / rec.fsamp)
        else:
            self.end_segment = hdrtime
            
    def close(self):
        if self.begin_segment != None and self.end_segment != None:
            ts = Timespan(self.begin_segment, self.end_segment)
            if self.tq_nrecords != 0:
                ts.add_qcobj(Timing(self.tq_min, self.tq_max, \
                    self.tq_sum/self.tq_nrecords))
                self.tq_min = 100
                self.tq_max = 0
                self.tq_sum = 0
                self.tq_nrecords = 0
                

            if self.nsamp != 0:
                offset = self.sumx / self.nsamp
                rms = math.sqrt(self.sumd2 / self.nsamp)
                ts.add_qcobj(Signal(offset, rms))
                self.sumx = 0.0
                self.sumd2 = 0.0
                self.nsamp = 0

            self.metadata.add_timespan(ts)

        timetuple = self.timestamp.timetuple()
        metafile_dir = "%s/%04d/%s/%s/%s.%s" % (self.basedir, timetuple[0], \
            self.stream_id[0], self.stream_id[1], self.stream_id[3], \
            self.stream_id[4] )

        metafile_name = "%s.%s.%s.%s.%s.%04d.%03d.meta.xml" % \
            (self.stream_id +  (timetuple[0], timetuple[7]))

        if not os.path.exists(metafile_dir):
            os.makedirs(metafile_dir)

        write_metadata(metafile_dir + "/" + metafile_name, self.metadata)
 
