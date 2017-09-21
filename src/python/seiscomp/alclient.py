#***************************************************************************** 
# alclient.py
#
# ArcLink client library
#
# (c) 2004 Andres Heinloo, GFZ Potsdam
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any later
# version. For more information, see http://www.gnu.org/
#*****************************************************************************

import socket
from getpass import getpass
from seiscomp.utils import _MyContentHandler
from seiscomp.utils import *

LINESIZE      = 100
BLOCKSIZE     = 512

STATUS_UNSET  = 0
STATUS_PROC   = 1
STATUS_SYNERR = 2
STATUS_CANCEL = 3
STATUS_OK     = 4
STATUS_WARN   = 5
STATUS_RETRY  = 6
STATUS_NODATA = 7

def arclink_status_string(s):
    if   s == STATUS_PROC:   return "PROCESSING"
    elif s == STATUS_SYNERR: return "SYNERR"
    elif s == STATUS_CANCEL: return "CANCELLED"
    elif s == STATUS_OK:     return "OK"
    elif s == STATUS_WARN:   return "WARNING"
    elif s == STATUS_RETRY:  return "RETRY"
    elif s == STATUS_NODATA: return "NODATA"
    elif s == STATUS_UNSET:  return "UNSET"

    return "UNKNOWN"

def _make_status(s = None):
    if   s == "PROCESSING":  return STATUS_PROC
    elif s == "SYNERR":      return STATUS_SYNERR
    elif s == "CANCELLED":   return STATUS_CANCEL
    elif s == "OK":          return STATUS_OK
    elif s == "WARNING":     return STATUS_WARN
    elif s == "RETRY":       return STATUS_RETRY
    elif s == "NODATA":      return STATUS_NODATA
    elif s == "UNSET":       return STATUS_UNSET
    elif s == None:          return STATUS_UNSET
    
    raise ValueError, "invalid status value: " + s

def _make_bool(s = None):
    if   s == "true":  return True
    elif s == "false": return False
    elif s == None:    return False
    
    raise ValueError, "invalid boolean: " + s
    
class _RequestLineElement(XElement):
    def __init__(self):
        XElement.__init__(self, { "content"     : (str,),
                                  "status"      : (_make_status,),
                                  "size"        : (int,),
                                  "message"     : (str,) })

class _RequestVolumeElement(XElement):
    def __init__(self):
        XElement.__init__(self, { "id"          : (str,),
                                  "status"      : (_make_status,),
                                  "size"        : (int,),
                                  "message"     : (str,),
                                  "line"        : _RequestLineElement })

class _RequestElement(XElement):
    def __init__(self):
        XElement.__init__(self, { "id"          : (str,),
                                  "user"        : (str,),
                                  "institution" : (str,),
                                  "type"        : (str,),
                                  "size"        : (int,),
                                  "ready"       : (_make_bool,),
                                  "volume"      : _RequestVolumeElement })

class _ArclinkElement(XElement):
    def __init__(self):
        XElement.__init__(self, { "request"     : _RequestElement })

class ArclinkError(Exception):
    pass

class ArclinkCommandNotAccepted(ArclinkError):
    pass

class ArclinkAuthFailed(ArclinkError):
    pass

def _err_print(s):
    print >>sys.stderr, s
    sys.stderr.flush()

class Arclink(object):
    def __init__(self, err_print = _err_print):
        self.__err_print = err_print
    
    def send_command(self, s, check_ok=True):
        self.__fd.write(s + "\r\n")
        self.__fd.flush()
        if not check_ok:
            return

        r = self.__fd.readline(LINESIZE)
        if r == "OK\r\n":
            return
        elif r == "ERROR\r\n":
            raise ArclinkCommandNotAccepted, "command not accepted"
        else:
            raise ArclinkError, "unexpected response: " + r.rstrip()

    def get_errmsg(self):
        self.send_command("SHOWERR", False)
        return self.__fd.readline(LINESIZE)[:-2]

    def open_connection(self, host, port, user, inst = None):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.connect((host, port))
        self.__fd = sock.makefile('r+')
        self.send_command("HELLO", False)
        self.software = self.__fd.readline(LINESIZE)[:-2]
        self.organization = self.__fd.readline(LINESIZE)[:-2]
        
        try:
            self.send_command("USER " + user)
        except ArclinkCommandNotAccepted:
            try:
                self.send_command("USER " + user + " " + getpass())
            except ArclinkCommandNotAccepted:
                raise ArclinkAuthFailed, "authentification failed"

        if inst != None:
            self.send_command("INSTITUTION " + inst)

    def close_connection(self):
        self.__fd.close()
    
    def get_status(self, req_id = "ALL"):
        self.send_command("STATUS " + req_id, False)
        p = xml.sax.make_parser()
        element_stack = [XElement({ "arclink" : _ArclinkElement })]
        p.setContentHandler(_MyContentHandler(p, element_stack,
          self.__err_print, True))
 
        try:
            line = self.__fd.readline(LINESIZE)
            while line != "END\r\n":
                if not line:
                    raise ArclinkError, "unexpected end of XML data"
                elif line == "ERROR\r\n":
                    raise ArclinkError, self.get_errmsg()

                p.feed(line)
                line = self.__fd.readline(LINESIZE)

            p.close()

        except xml.sax.SAXException, e:
            raise ArclinkError, str(e)
        
        return element_stack[0].arclink[0]

    def submit(self, reqf):
        line = reqf.readline(LINESIZE)
        while not line:
            line = reqf.readline(LINESIZE)
        
        if line.split()[0].upper() != "REQUEST":
            raise ArclinkError, "incorrectly formulated request"
            
        self.send_command(line)
        
        line = reqf.readline(LINESIZE)
        while line.split()[0].upper() != "END":
            self.send_command(line, False)
            line = reqf.readline(LINESIZE)

        if len(line.split()) != 1:
            raise ArclinkError, "incorrectly formulated request"
        
        line = reqf.readline(LINESIZE)
        while line:
            if line.strip():
                raise ArclinkError, "incorrectly formulated request"

            line = reqf.readline(LINESIZE)

        self.send_command("END", False)
        r = self.__fd.readline(LINESIZE)
        if r == "ERROR\r\n":
            raise ArclinkError, self.get_errmsg()

        return r.strip()

    def download(self, outfd, req_id, vol_id = None, pos = None):
        if vol_id == None:
            req_vol = req_id
        else:
            req_vol = req_id + "." + vol_id
        
        if pos == None:
            pos_ext = ""
        else:
            pos_ext = " " + pos
        
        self.send_command("DOWNLOAD " + req_vol + pos_ext, False)
        r = self.__fd.readline(LINESIZE)
        if r == "ERROR\r\n":
            raise ArclinkError, self.get_errmsg()

        try:
            size = int(r)
        except ValueError:
            raise ArclinkError, "unexpected response: " + r.rstrip()

        bytes_read = 0
        while bytes_read < size:
            buf = self.__fd.read(min(BLOCKSIZE, size - bytes_read))
            outfd.write(buf)
            bytes_read += len(buf)

        r = self.__fd.readline(LINESIZE)
        if r != "END\r\n":
            raise ArclinkError, "END not found"

    def purge(self, req_id):
        try:
            self.send_command("PURGE " + req_id)
        except ArclinkCommandNotAccepted:
            raise ArclinkError, self.get_errmsg()

