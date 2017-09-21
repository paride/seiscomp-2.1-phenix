#***************************************************************************** 
# utils.py
#
# Some useful Python code
#
# (c) 2004 Andres Heinloo, GFZ Potsdam
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any later
# version. For more information, see http://www.gnu.org/
#*****************************************************************************

import sys
import re
import xml.sax
from datetime import datetime

class _XValueError(Exception):
    pass

class _MsgPass(object):
    def __init__(self):
        self._callbacks = []

    def __call__(self, *args, **kwds):
        for f in self._callbacks:
            f(*args, **kwds)
            
    def __nonzero__(self):
        return len(self._callbacks) > 0
    
    def __getattr__(self, name):
        obj = _MsgPass()
        self.__dict__[name] = obj
        return obj

class MsgInterface(object):
    def attach(self, name, method):
        dict = self.__dict__
        for k in name.split("."):
            if k in dict:
                obj = dict[k]
            else:
                obj = _MsgPass()
                dict[k] = obj

            dict = obj.__dict__

        obj._callbacks.append(method)

    def __getattr__(self, name):
        obj = _MsgPass()
        self.__dict__[name] = obj
        return obj

class XElement(object):
    def __init__(self, xdict = {}):
        self.__xdict = xdict
        
        for name, ctor in xdict.items():
            if isinstance(ctor, tuple):
                self.__dict__[name] = ctor[0](*ctor[1:])
            else:
                self.__dict__[name] = []
            
    def set_attr(self, name, value):
        ctor = self.__xdict.get(name)
        if ctor != None and isinstance(ctor, tuple):
            try:
                obj = ctor[0](value)
            except (TypeError, ValueError), e:
                raise _XValueError, e.args
                
            self.__dict__[name] = obj
            return True

        return False

    def add_child(self, name):
        ctor = self.__xdict.get(name)
        if ctor != None and not isinstance(ctor, tuple):
            obj = ctor()
            self.__dict__[name].append(obj)
            return obj

        return None
            
class _MyContentHandler(xml.sax.ContentHandler):
    def __init__(self, parser, element_stack, err_print, warn_unused):
        xml.sax.ContentHandler.__init__(self)
        self.__parser = parser
        self.__element_stack = element_stack
        self.__err_print = err_print
        self.__warn_unused = warn_unused

    def warning(self, s):
        self.__err_print(self.__parser.getSystemId() + ":" + \
            str(self.__parser.getLineNumber()) + ":" + \
            str(self.__parser.getColumnNumber()) + ": " + s)
    
    def startElement(self, tag, attributes):
        chd = self.__element_stack[-1].add_child(tag)
        if chd == None:
            if self.__warn_unused:
                self.warning("element '" + tag + "' ignored")

            self.__element_stack.append(XElement())
        else:
            for name, value in attributes.items():
                try:
                    if not chd.set_attr(name, value) and self.__warn_unused:
                        self.warning("attribute '" + name + "' ignored")
                except _XValueError, e:
                    self.warning(str(e))

            self.__element_stack.append(chd)

    def endElement(self, tag):
        self.__element_stack.pop()

def _err_print(s):
    print >>sys.stderr, s
    sys.stderr.flush()

def read_xml(source, xdict, err_print = _err_print, warn_unused = True):
    p = xml.sax.make_parser()
    element_stack = [XElement(xdict)]
    p.setContentHandler(_MyContentHandler(p, element_stack, err_print,
        warn_unused))
    
    try:
        p.parse(source)
    except xml.sax.SAXException, e:
        err_print(str(e))
    
    return element_stack[0]

_rx_datetime = re.compile("([0-9]*)/([0-9]*)/([0-9]*) " \
    "*([0-9]*):([0-9]*):([0-9]*).([0-9]*)")

def make_datetime(s = None):
    if s == None:
        return datetime(1970, 1, 1)
        
    m = _rx_datetime.match(s)
    if m == None:
        raise ValueError, "invalid datetime: " + s

    year, month, mday, hour, min, sec, sfract = m.groups()

    try:
        obj = datetime(int(year), int(month), int(mday),
            int(hour), int(min), int(sec), int((sfract + "000000")[:6]))
    except ValueError:
        raise ValueError, "invalid datetime: " + s

    return obj

def format_datetime(dt):
    return "%04d/%02d/%02d %02d:%02d:%02d.%04d" % \
        (dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second, \
        dt.microsecond / 100)

