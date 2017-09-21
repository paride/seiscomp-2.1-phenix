#***************************************************************************** 
# metadata.py
#
# Routines for reading and writing content files
#
# (c) 2004 Andres Heinloo, GFZ Potsdam
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any later
# version. For more information, see http://www.gnu.org/
#*****************************************************************************

import sys
import xml.dom.minidom
from seiscomp.utils import *

class MetadataError(Exception):
    pass

class QCObject(object):
    def getxml(self, doc, e0):
        pass

class Timing(QCObject):
    def __init__(self, q_min, q_max, q_avg):
        self.q_min = q_min
        self.q_max = q_max
        self.q_avg = q_avg

    def getxml(self, doc, e0):
        e1 = doc.createElement("timing")
        e1.setAttribute("q_min", str(int(self.q_min)))
        e1.setAttribute("q_max", str(int(self.q_max)))
        e1.setAttribute("q_avg", str(int(self.q_avg)))
        e0.appendChild(e1)

class _TimingElement(XElement):
    def __init__(self):
        XElement.__init__(self, { "q_avg"    : (int,),
                                  "q_min"    : (int,),
                                  "q_max"    : (int,) })

class Signal(QCObject):
    def __init__(self, offset, rms):
        self.offset = offset
        self.rms = rms

    def getxml(self, doc, e0):
        e1 = doc.createElement("signal")
        e1.setAttribute("offset", str(int(self.offset)))
        e1.setAttribute("rms", str(int(self.rms)))
        e0.appendChild(e1)

class _SignalElement(XElement):
    def __init__(self):
        XElement.__init__(self, { "offset"   : (int,),
                                  "rms"      : (int,) })

class Timespan(object):
    def __init__(self, begin, end):
        self.begin = begin
        self.end = end
        self.qcobj = []

    def add_qcobj(self, a):
        if not isinstance(a, QCObject):
            raise TypeError, "QCObject expected"
            
        self.qcobj.append(a)

    def getxml(self, doc, e0):
        e1 = doc.createElement("timespan")
        e1.setAttribute("begin", format_datetime(self.begin))
        e1.setAttribute("end", format_datetime(self.end))

        for a in self.qcobj:
            a.getxml(doc, e1)
        
        e0.appendChild(e1)
 
class _TimespanElement(XElement):
    def __init__(self):
        XElement.__init__(self, { "signal"   : _SignalElement,
                                  "timing"   : _TimingElement,
                                  "begin"    : (make_datetime,),
                                  "end"      : (make_datetime,) })

class Metadata(object):
    def __init__(self):
        self.timespans = []

    def add_timespan(self, s):
        if not isinstance(s, Timespan):
            raise TypeError, "Timespan expected"

        self.timespans.append(s)

    def getxml(self, doc, e0):
        for s in self.timespans:
            s.getxml(doc, e0)

class _MetadataElement(XElement):
    def __init__(self):
        XElement.__init__(self, { "timespan" : _TimespanElement })

def _err_print(s):
    print >>sys.stderr, s
    sys.stderr.flush()

def read_metadata(source, err_print = _err_print):
    xmlroot = read_xml(source,  { "meta"     : _MetadataElement }, err_print)

    if(len(xmlroot.meta) != 1):
        raise MetadataError, "error reading XML"

    return xmlroot.meta[0]

_dom = xml.dom.getDOMImplementation("minidom")

def write_metadata(file, data):
    doc = _dom.createDocument(None, "meta", None)
    root = doc.documentElement
    data.getxml(doc, root)
    fd = open(file, "w")
    print >>fd, doc.toprettyxml("  ")
    fd.close()
    doc.unlink()

