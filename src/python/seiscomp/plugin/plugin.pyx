#***************************************************************************** 
# plugin.pyx
#
# Python wrapper for SeedLink plugin interface and chain_plugin extensions
#
# (c) 2004 Andres Heinloo, GFZ Potsdam
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any later
# version. For more information, see http://www.gnu.org/
#*****************************************************************************

cdef os
os = __import__("os")

cdef datetime
datetime = __import__("datetime")

cdef extern from "stdlib.h":
    void *malloc(int size)
    void free(void *ptr)

cdef extern from "plugin.h":
    enum:
        PLUGIN_FD

    struct _ptime "ptime":
        int year
        int yday
        int hour
        int minute
        int second
        int usec

    ctypedef int int32_t

    int _send_raw3 "send_raw3" (char *station, char *channel,
        _ptime *pt, int usec_correction, int timing_quality,
        int32_t *dataptr, int number_of_samples)

    int _send_flush3 "send_flush3" (char *station, char *channel)

    int _send_log3 "send_log3" (char *station, _ptime *pt, char *fmt, ...)

    int _send_mseed "send_mseed" (char *station, char *dataptr,
        int packet_size)

cdef enum:
    input_fd   = PLUGIN_FD - 1
    command_fd = PLUGIN_FD - 2

cdef class ptime:
    cdef public int year, yday, hour, minute, second, usec

    def __init__(self, int year = 2000, int yday = 1, int hour = 0,
        int minute = 0, int second = 0, int usec = 0):
        self.year = year
        self.yday = yday
        self.hour = hour
        self.minute = minute
        self.second = second
        self.usec = usec
    
cdef input_stream
input_stream = None
try:
    input_stream = os.fdopen(input_fd, "r")
except OSError:
    pass

cdef cmd_stream
cmd_stream = None
try:
    cmd_stream = os.fdopen(command_fd, "w")
except OSError:
    pass

cdef _ptime cvttime(t):
    cdef _ptime pt

    if isinstance(t, ptime):
        pt.year = t.year
        pt.yday = t.yday
        pt.hour = t.hour
        pt.minute = t.minute
        pt.second = t.second
        pt.usec = t.usec
    elif isinstance(t, datetime.datetime):
        tt = t.timetuple()
        pt.year = tt[0]
        pt.yday = tt[7]
        pt.hour = tt[3]
        pt.minute = tt[4]
        pt.second = tt[5]
        pt.usec = t.microsecond
    else:
        raise TypeError, "invalid time"

    return pt

def get_input():
    return input_stream

def have_command_stream():
    return cmd_stream is not None

def send_raw(char *sta_id, char *cha_id, pkttime, int tcorr, int tqual,
    data_array):
    cdef int32_t *databuf
    cdef int nsamp
    cdef _ptime pt

    if data_array is None:
        nsamp = 0
        databuf = NULL
    else:
        nsamp = len(data_array)
        databuf = <int32_t *>malloc(4 * nsamp)
        if databuf == NULL:
            raise MemoryError, "out of memory"

    try:
        for i from 0 <= i < nsamp:
            databuf[i] = data_array[i]

        if pkttime is None:
            r = _send_raw3(sta_id, cha_id, NULL, tcorr, tqual, databuf, nsamp)
        else:
            pt = cvttime(pkttime)
            r = _send_raw3(sta_id, cha_id, &pt, tcorr, tqual, databuf, nsamp)

        if r < 0 or (r == 0 and nsamp != 0):
            raise IOError, "error sending data to SeedLink"

    except: # try/finally seems to cause segfault in Pyrex
        free(databuf)
        raise

    free(databuf)

def send_flush(char *sta_id, char *cha_id):
    if _send_flush3(sta_id, cha_id) < 0:
        raise IOError, "error sending data to SeedLink"

def send_mseed(char *sta_id, rawrec):
    if _send_mseed(sta_id, rawrec, len(rawrec)) <= 0:
        raise IOError, "error sending data to SeedLink"

def send_log(char *sta_id, pkttime, char *msg):
    cdef _ptime pt

    pt = cvttime(pkttime)

    if _send_log3(sta_id, &pt, "%s", msg) <= 0:
        raise IOError, "error sending data to SeedLink"

def send_command(cmd):
    if cmd_stream:
        cmd_stream.write(cmd + "\r")
        cmd_stream.flush()

