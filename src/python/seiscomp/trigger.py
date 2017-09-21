#***************************************************************************** 
# trigger.py
#
# SeisComP trigger module
#
# (c) 2004 Andres Heinloo, GFZ Potsdam
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any later
# version. For more information, see http://www.gnu.org/
#*****************************************************************************

import sys
from datetime import datetime
from seiscomp import mseed
from seiscomp.detector import Detector
from seiscomp.plugin import *
from seiscomp.iniparser import *

class TriggerError(Exception):
    pass

class _DummyDetector(object):
    def feed(self, rec):
        pass

class _MyDetector(Detector):
    def __init__(self, label, config, log_print):
        Detector.__init__(self, label, config)
        self.__log_print = log_print
        self.__received = False

    def time2string(self, t):
        return " ".join(map(str, t.asDate[:5] + (int(t.sec),)))

    def trigger_ON(self, t):
        send_command("trigger on " + self.label + " " + self.time2string(t))
        Detector.trigger_ON(self, t)

    def trigger_OFF(self, t):
        send_command("trigger off " + self.label + " " + self.time2string(t))
        Detector.trigger_OFF(self, t)

    def write_pick(self, trg):
        sec = int(trg.on.sec)
        usec = int((trg.on.sec - sec) * 1000000)
        try:
            ontime = datetime(*trg.on.asDate[:5] + (sec, usec))
        except ValueError, e:
            self.__log_print(str(rec.time) + ": " + str(e))
            return
        
        send_log(self.label, ontime, trg.print_autoloc())

    def feed(self, rec):
        if not self.__received:
            self.__log_print("%s.%s.%s.%s.%s received" % \
                (rec.net, rec.sta, rec.loc, rec.cha[0:2] + "?", rec.stype))
            self.__received = True
        
        Detector.feed(self, rec)

def _log_print(s):
    print s
    sys.stdout.flush()

def start(config_file, log_print = _log_print):
    fd = get_input()
    if fd == None:
        raise TriggerError, "missing input stream"

    if not have_command_stream():
        raise TriggerError, "missing command stream"

    detec = {}
    mandatory = ["net", "sta"]
    config = read_ini(config_file, log_print)
    default = config.get("default")
    if default == None:
        raise TriggerError, "%s has no default section" % (config_file,)

    for label, sect in config.iteritems():
        if label == "default":
            continue

        c = { "label": label }
        for k, v in default.iteritems():
            if isinstance(v, dict):
                # There are not supposed to be object definitions
                # in triggers.ini; probably some quotes are missing, etc.
                # Continuing in this condition does not make much sense,
                # because many assignments are possibly skipped.
                raise TriggerError, "[%s]: unexpected object '%s'" % \
                    (label, k)
            
            item = k.lower()
            c[item] = v

        for k, v in sect.iteritems():
            if isinstance(v, dict):
                raise TriggerError, "[%s]: unexpected object '%s'" % \
                    (label, k)
            
            item = k.lower()
            if item not in default and item not in mandatory:
                log_print("[%s]: unmatched item '%s'" % (label, item))

            c[item] = v

        for item in mandatory:
            if item not in c:
                raise TriggerError, "[%s]: missing item '%s'" % (label, item)

        detec[(c["net"], c["sta"], c["loc"], c["cha"][0:2])] = _MyDetector(label, c, log_print)

    for rec in mseed.Input(fd):
        d = detec.get((rec.net, rec.sta, rec.loc, rec.cha[0:2]))
        if d == None:
            log_print("%s.%s.%s.%s.%s not configured" % \
                (rec.net, rec.sta, rec.loc, rec.cha[0:2] + "?", rec.stype))
            detec[(rec.net, rec.sta, rec.loc, rec.cha[0:2])] = _DummyDetector()
        else:
            d.feed(rec)

