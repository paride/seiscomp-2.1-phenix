#***************************************************************************** 
# fseed.py
#
# SEED builder for SeisComP
#
# (c) 2005 Andres Heinloo, GFZ Potsdam
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any later
# version. For more information, see http://www.gnu.org/
#*****************************************************************************

import re
import StringIO
from datetime import date, time, datetime
from seiscomp import logs
from seiscomp.cfgdb.cfgtree import *

class SEEDError(Exception):
    pass

_rx_coeff = re.compile(r'\s*(\S+)\s*')

def _mkseedcoeff(ncoeff, s):
    pos = 0
    n = 0
    c = ""
    
    while pos < len(s):
        m = _rx_coeff.match(s, pos)
        if m == None:
            raise SEEDError, "error parsing FIR coefficients at '" + \
                s[pos:] + "'"

        try:
            v = float(m.group(1))
        except ValueError:
            raise SEEDError, "error parsing FIR coefficients at '" + \
                s[pos:] + "'"

        c += "%14.7E" % (v,)
        n += 1
        pos = m.end()
    
    if n != ncoeff:
        raise SEEDError, "expected %d coefficients, found %d" % (ncoeff, n)

    return c

_rx_paz = re.compile(r'\s*([0-9]*)\(\s*([^,]+),\s*([^)]+)\)\s*')

def _mkseedpaz(npaz, s):
    pos = 0
    n = 0
    c = ""
    
    while pos < len(s):
        m = _rx_paz.match(s, pos)
        if m == None:
            raise SEEDError, "error parsing PAZ at '" + s[pos:] + "'"

        try:
            if len(m.group(1)) > 0:
                x = int(m.group(1))
            else:
                x = 1
                
            rv = float(m.group(2))
            iv = float(m.group(3))

        except ValueError:
            raise SEEDError, "error parsing PAZ at '" + s[pos:] + "'"

        for i in xrange(0, x):
            c += "%12.5E%12.5E 0.00000E-00 0.00000E-00" % (rv, iv)
        
        n += x
        pos = m.end()
    
    if n != npaz:
        raise SEEDError, "expected %d PAZ, found %d" % (npaz, n)

    return c

def _mkseedstring(s, min_length, max_length, flags):
    U = L = N = P = S = X = False
    rx_list = []
    
    if flags.find("U") != -1:
        U = True
        rx_list.append("[A-Z]")
        
    if flags.find("L") != -1:
        L = True
        rx_list.append("[a-z]")
        
    if flags.find("N") != -1:
        N = True
        rx_list.append("[0-9]")
        
    if flags.find("P") != -1:
        P = True
        rx_list.append("[^A-Za-z0-9 ]")

    if flags.find("S") != -1:
        S = True
        rx_list.append(" ")
        
    if flags.find("_") != -1:
        X = True
        rx_list.append("_")
        
    sn = s.strip()[:max_length]

    if U and not L:
        sn = sn.upper()
    elif L and not U:
        sn = sn.lower()

    if S and not X:
        sn = sn.replace("_", " ")
    elif X and not S:
        sn = sn.replace(" ", "_")

    rx = "|".join(rx_list)
    sn = "".join(re.findall(rx, sn))

    if re.match("(" + rx + ")*$", sn) == None:
        raise SEEDError, "cannot convert string \"" + s + "\" with flags " + \
            flags

    if len(sn) < min_length:
        if min_length != max_length:
            raise SEEDError, "cannot extend string \"" + s + \
                "\" to minimum length " + str(min_length) + " with flags " + \
                    flags
        else:
            sn = (sn + min_length * " ")[:min_length]

    if min_length != max_length:
        sn += "~"

    return sn

def _mkseedtime(t):
    if t == None:
        return "~"
    
    if isinstance(t, datetime):
        tt = t.utctimetuple()
        return "%04d,%03d,%02d:%02d~" % (t.year, tt[7], t.hour, t.minute)
    elif isinstance(t, date):
        tt = datetime.combine(t, time(0, 0, 0)).utctimetuple()
        return "%04d,%03d~" % (t.year, tt[7])

    raise SEEDError, "invalid time object: " + str(t)

class _Blockette10(object):
    def __init__(self, record_length, start_time, end_time, vol_time,
        organization, label):
        self.__record_length = record_length
        self.__start_time = _mkseedtime(start_time)
        self.__end_time = _mkseedtime(end_time)
        self.__vol_time = _mkseedtime(vol_time)
        self.__organization = _mkseedstring(organization, 1, 80, "UNLPS_")
        self.__label = _mkseedstring(label, 1, 80, "UNLPS_")
        self.__len = 13 + len(self.__start_time) + len(self.__end_time) + \
            len(self.__vol_time) + len(self.__organization) + \
            len(self.__label)

    def output(self, f):
        blk = "010%4d 2.3%2d%s%s%s%s%s" % (self.__len, self.__record_length,
            self.__start_time, self.__end_time, self.__vol_time,
            self.__organization, self.__label)
        
        if len(blk) != self.__len:
            raise SEEDError, "blockette 10 has invalid length: " + str(len(blk))

        f.write_blk(blk)

class _Blockette11(object):
    def __init__(self):
        self.__nstations = 0
        self.__stat_rec = ()
        self.__len = 10

    def add_station(self, code, recno):
        self.__stat_rec += (_mkseedstring(code, 5, 5, "UN"), recno)
        self.__nstations += 1
        self.__len += 11

    def output(self, f):
        blk = ("011%4d%3d" + self.__nstations * "%s%6d") % \
            ((self.__len, self.__nstations) + self.__stat_rec)

        if len(blk) != self.__len:
            raise SEEDError, "blockette 11 has invalid length: " + str(len(blk))

        f.write_blk(blk)

class _Blockette30(object):
    def __init__(self, name, key, family, ddl):
        self.__name = _mkseedstring(name, 1, 50, "UNLPS")
        self.__key = key
        self.__family = family
        self.__ddl = "~".join(ddl) + "~"
        self.__nddl = len(ddl)
        self.__len = 16 + len(self.__name) + len(self.__ddl)

    def output(self, f):
        blk = "030%4d%s%4d%3d%2d%s" % (self.__len, self.__name,
            self.__key, self.__family, self.__nddl, self.__ddl)

        if len(blk) != self.__len:
            raise SEEDError, "blockette 30 has invalid length: " + str(len(blk))

        f.write_blk(blk)

class _Blockette33(object):
    def __init__(self, key, desc):
        self.__key = key
        self.__desc = _mkseedstring(desc, 1, 50, "UNLPS")
        self.__len = 10 + len(self.__desc)

    def output(self, f):
        blk = "033%4d%3d%s" % (self.__len, self.__key, self.__desc)

        if len(blk) != self.__len:
            raise SEEDError, "blockette 33 has invalid length: " + str(len(blk))

        f.write_blk(blk)

class _Blockette34(object):
    def __init__(self, key, name, desc):
        self.__key = key
        self.__name = _mkseedstring(name, 1, 20, "UNP")
        self.__desc = _mkseedstring(desc, 1, 50, "UNLPS")
        self.__len = 10 + len(self.__name) + len(self.__desc)

    def output(self, f):
        blk = "034%4d%3d%s%s" % (self.__len, self.__key, self.__name,
            self.__desc)

        if len(blk) != self.__len:
            raise SEEDError, "blockette 34 has invalid length: " + str(len(blk))

        f.write_blk(blk)

class _Blockette41(object):
    def __init__(self, key, name, symmetry, input_units, output_units, ncoeff,
        coeff):

        self.__key = key
        self.__name = _mkseedstring(name, 1, 25, "UN")
        self.__symmetry = _mkseedstring(symmetry, 1, 1, "U")
        self.__input_units = input_units
        self.__output_units = output_units
        self.__ncoeff = ncoeff
        self.__coeff = _mkseedcoeff(ncoeff, coeff)
        self.__len = 22 + 14 * ncoeff + len(self.__name) 

    def output(self, f):
        blk = "041%4d%4d%s%s%3d%3d%4d%s" % (self.__len, self.__key,
            self.__name, self.__symmetry, self.__input_units,
            self.__output_units, self.__ncoeff, self.__coeff)

        if len(blk) != self.__len:
            raise SEEDError, "blockette 41 has invalid length: " + str(len(blk))
        
        f.write_blk(blk)
        
class _Blockette43(object):
    def __init__(self, key, name, type, input_units, output_units, norm_fac,
        norm_freq, nzeros, zeros, npoles, poles):

        self.__key = key
        self.__name = _mkseedstring(name, 1, 25, "UN")
        self.__type = _mkseedstring(type, 1, 1, "U")
        self.__input_units = input_units
        self.__output_units = output_units
        self.__norm_fac = norm_fac
        self.__norm_freq = norm_freq
        self.__nzeros = nzeros
        self.__zeros = _mkseedpaz(nzeros, zeros)
        self.__npoles = npoles
        self.__poles = _mkseedpaz(npoles, poles)
        self.__len = 48 + 48 * (nzeros + npoles) + len(self.__name)

    def output(self, f):
        blk = "043%4d%4d%s%s%3d%3d%12.5E%12.5E%3d%s%3d%s" % \
            (self.__len, self.__key, self.__name, self.__type,
            self.__input_units, self.__output_units, self.__norm_fac,
            self.__norm_freq, self.__nzeros, self.__zeros, self.__npoles,
            self.__poles)
    
        if len(blk) != self.__len:
            raise SEEDError, "blockette 43 has invalid length: " + str(len(blk))
        
        f.write_blk(blk)
        
class _Blockette44(object):
    def __init__(self, key, name, type, input_units, output_units):

        self.__key = key
        self.__name = _mkseedstring(name, 1, 25, "UN")
        self.__type = _mkseedstring(type, 1, 1, "U")
        self.__input_units = input_units
        self.__output_units = output_units
        self.__len = 26 + len(self.__name)

    def output(self, f):
        blk = "044%4d%4d%s%s%3d%3d   0   0" % (self.__len, self.__key,
            self.__name, self.__type, self.__input_units,
            self.__output_units)

        if len(blk) != self.__len:
            raise SEEDError, "blockette 44 has invalid length: " + str(len(blk))
        
        f.write_blk(blk)
        
class _Blockette47(object):
    def __init__(self, key, name, input_rate, deci_fac, deci_offset,
        delay, correction):

        self.__key = key
        self.__name = _mkseedstring(name, 1, 25, "UN")
        self.__input_rate = input_rate
        self.__deci_fac = deci_fac
        self.__deci_offset = deci_offset
        self.__delay = delay
        self.__correction = correction
        self.__len = 53 + len(self.__name)

    def output(self, f):
        blk = "047%4d%4d%s%10.4E%5d%5d%11.4E%11.4E" % (self.__len,
            self.__key, self.__name, self.__input_rate, self.__deci_fac,
            self.__deci_offset, self.__delay, self.__correction)
            
        if len(blk) != self.__len:
            raise SEEDError, "blockette 47 has invalid length: " + str(len(blk))
        
        f.write_blk(blk)

class _Blockette48(object):
    def __init__(self, key, name, gain, gain_freq):
        self.__key = key
        self.__name = _mkseedstring(name, 1, 25, "UN")
        self.__gain = gain
        self.__gain_freq = gain_freq
        self.__len = 37 + len(self.__name)

    def output(self, f):
        blk = "048%4d%4d%s%12.5E%12.5E 0" % (self.__len, self.__key,
            self.__name, self.__gain, self.__gain_freq)
        
        if len(blk) != self.__len:
            raise SEEDError, "blockette 48 has invalid length: " + str(len(blk))
        
        f.write_blk(blk)

class _Blockette50(object):
    def __init__(self, stat_code, latitude, longitude, elevation,
        site_name, net_id, net_code, start_date, end_date):
        self.__stat_code = _mkseedstring(stat_code, 5, 5, "UN")
        self.__latitude = latitude
        self.__longitude = longitude
        self.__elevation = elevation
        self.__site_name = _mkseedstring(site_name, 1, 60, "UNLPS")
        self.__net_id = net_id
        self.__start_date = _mkseedtime(start_date)
        self.__end_date = _mkseedtime(end_date)
        self.__net_code = _mkseedstring(net_code, 2, 2, "UN")
        self.__len = 59 + len(self.__site_name) + len(self.__start_date) + \
            len(self.__end_date)

    def output(self, f):
        blk = "050%4d%s%10.6f%11.6f%7.1f       %s%3d321010%s%sN%s" % \
            (self.__len, self.__stat_code, self.__latitude, self.__longitude,
            self.__elevation, self.__site_name, self.__net_id,
            self.__start_date, self.__end_date, self.__net_code)

        if len(blk) != self.__len:
            raise SEEDError, "blockette 50 has invalid length: " + str(len(blk))
        
        f.write_blk(blk)

class _Blockette52(object):
    def __init__(self, loc_id, chan_id, instr_id, comment, signal_units,
        calibration_units, latitude, longitude, elevation, local_depth,
        azimuth, dip, data_format, record_length, sample_rate, clock_drift,
        flags, start_date, end_date):
        self.__loc_id = _mkseedstring(loc_id, 2, 2, "UN")
        self.__chan_id = _mkseedstring(chan_id, 3, 3, "UN")
        self.__instr_id = instr_id
        self.__comment = _mkseedstring(comment, 0, 30, "UNLPS")
        self.__signal_units = signal_units
        self.__calibration_units = calibration_units
        self.__latitude = latitude
        self.__longitude = longitude
        self.__elevation = elevation
        self.__local_depth = local_depth
        self.__azimuth = azimuth
        self.__dip = dip
        self.__data_format = data_format
        self.__record_length = record_length
        self.__sample_rate = sample_rate
        self.__clock_drift = clock_drift
        self.__flags = _mkseedstring(flags, 0, 26, "U")
        self.__start_date = _mkseedtime(start_date)
        self.__end_date = _mkseedtime(end_date)
        self.__len = 99 + len(self.__comment) + len(self.__flags) + \
            + len(self.__start_date) + len(self.__end_date)

    def output(self, f):
        blk = "052%4d%s%s   0%3d%s%3d%3d%10.6f%11.6f%7.1f%5.1f%5.1f%5.1f%4d%2d%10.4E%10.4E    %s%s%sN" % \
            (self.__len, self.__loc_id, self.__chan_id, self.__instr_id,
            self.__comment, self.__signal_units, self.__calibration_units,
            self.__latitude, self.__longitude, self.__elevation,
            self.__local_depth, self.__azimuth, self.__dip, self.__data_format,
            self.__record_length, self.__sample_rate, self.__clock_drift,
            self.__flags, self.__start_date, self.__end_date)

        if len(blk) != self.__len:
            raise SEEDError, "blockette 52 has invalid length: " + str(len(blk))
        
        f.write_blk(blk)

class _Blockette58(object):
    def __init__(self, stage, sensitivity, frequency):
        self.__stage = stage
        self.__sensitivity = sensitivity
        self.__frequency = frequency
        self.__len = 35

    def output(self, f):
        blk = "058%4d%2d%12.5E%12.5E 0" % (self.__len, self.__stage,
            self.__sensitivity, self.__frequency)
        
        if len(blk) != self.__len:
            raise SEEDError, "blockette 58 has invalid length: " + str(len(blk))
        
        f.write_blk(blk)

class _Blockette60(object):
    def __init__(self, start_stage):
        self.__start_stage = start_stage
        self.__ref_list = []
        self.__len = 9

    def add_stage(self, *keyref):
        self.__ref_list.append(keyref)
        self.__len += 4 + 4 * len(keyref)

    def output(self, f):
        blk = "060%4d%2d" % (self.__len, len(self.__ref_list))
        
        for (n, r) in enumerate(self.__ref_list):
            blk += ("%2d%2d" + len(r) * "%4d") % \
               ((self.__start_stage + n, len(r)) + r)
        
        if len(blk) != self.__len:
            raise SEEDError, "blockette 60 has invalid length: " + str(len(blk))
        
        f.write_blk(blk)

class _FormatDict(object):
    __formats = {
        "Steim1": ("Steim1 Integer Compression Format", 50,
            "F1 P4 W4 D C2 R1 P8 W4 D C2",
            "P0 W4 N15 S2,0,1",
            "T0 X W4",
            "T1 Y4 W7 D C2",
            "T2 Y2 W2 D C2",
            "T3 N0 W4 D C2"),
            
        "Steim2": ("Steim2 Integer Compression Format", 50,
            "F1 P4 W4 D C2 R1 P8 W4 D C2",
            "P0 W4 N15 S2,0,1",
            "T0 X W4",
            "T1 Y4 W1 D C2",
            "T2 W4 I D2",
            "K0 X D30",
            "K1 N0 D30 C2",
            "K2 Y2 D15 C2",
            "K3 Y3 D10 C2",
            "T3 W4 I D2",
            "K0 Y5 D6 C2",
            "K1 Y6 D5 C2",
            "K2 X D2 Y7 D4 C2",
            "K3 X D30"),

        "ASCII": ("ASCII console log", 80, "") }

    def __init__(self):
        self.__num = 0
        self.__used = {}
        self.__blk = []

    def lookup(self, name):
        k = self.__used.get(name)
        if k is not None:
            return k
            
        self.__num += 1
        k = self.__num
        self.__used[name] = k

        f = self.__formats.get(name)
        if f is None:
            raise SEEDError, "unknown data format: " + name

        b = _Blockette30(name = f[0], key = k, family = f[1], ddl = f[2:])
        self.__blk.append(b)
        return k

    def output(self, f):
        for b in self.__blk:
            b.output(f)
            
class _UnitDict(object):
    __units = {
        "COUNTS": "Digital Counts",
        "COUNTS/V": "Counts per Volt",
        "M/S": "Velocity in Meters per Second",
        "M/S**2": "Acceleration in Meters Per Second Per Second",
        "V": "Volts",
        "A": "Amperes" }

    def __init__(self):
        self.__num = 0
        self.__used = {}
        self.__blk = []

    def lookup(self, name):
        k = self.__used.get(name)
        if k is not None:
            return k
            
        self.__num += 1
        k = self.__num
        self.__used[name] = k

        desc = self.__units.get(name)
        if desc is None:
            raise SEEDError, "unknown unit: " + name

        b = _Blockette34(key = k, name = name, desc = desc)
        self.__blk.append(b)
        return k

    def output(self, f):
        for b in self.__blk:
            b.output(f)
            
class _GenericAbbreviationDict(object):
    def __init__(self, cfgroot):
        self.__cfgroot = cfgroot
        self.__num = 0
        self.__used_sensor = {}
        self.__used_network = {}
        self.__blk = []   # blk33

    def lookup_sensor(self, name):     # instrument id for blk52
        k = self.__used_sensor.get(name)
        if k is not None:
            return k

        self.__num += 1
        k = self.__num
        self.__used_sensor[name] = k

        sensor = self.__cfgroot.seismometer.get(name)
        if sensor is None:
            raise SEEDError, "unknown sensor: " + name

        self.__blk.append(_Blockette33(k, sensor.description))
        return k

    def lookup_network(self, name):    # network id for blk50
        k = self.__used_network.get(name)
        if k is not None:
            return k

        self.__num += 1
        k = self.__num
        self.__used_network[name] = k

        network = self.__cfgroot.network.get(name)
        if network is None:
            raise SEEDError, "unknown network: " + name

        self.__blk.append(_Blockette33(k, network.description))
        return k

    def output(self, f):
        for b in self.__blk:
            b.output(f)
            
class _ResponseDict(object):
    def __init__(self, cfgroot, unit_dict):
        self.__cfgroot = cfgroot
        self.__unit_dict = unit_dict
        self.__num = 0
        self.__used_sensor = {}
        self.__used_sensor_calib = {}
        self.__used_digitizer = {}
        self.__used_digitizer_calib = {}
        self.__used_analogue_paz = {}
        self.__used_digital_paz = {}
        self.__used_fir = {}
        self.__used_fir_deci = {}
        self.__blk41 = []
        self.__blk43 = []
        self.__blk44 = []
        self.__blk47 = []
        self.__blk48 = []

    def lookup_sensor(self, name, dev_id, compn):
        sensor = self.__cfgroot.seismometer.get(name)
        if sensor is None:
            raise SEEDError, "unknown sensor: " + name

        k1 = self.__used_sensor.get(name)
        if k1 is None:
            if sensor.nzeros == 0:       # check!
                input_units = self.__unit_dict.lookup("M/S**2")
            else:
                input_units = self.__unit_dict.lookup("M/S")
            
            k1 = self.__num + 1

            b1 = _Blockette43(key = k1,
                name = "RS" + name,
                type = "A",
                input_units = input_units,
                output_units = self.__unit_dict.lookup("V"),
                norm_fac = sensor.norm_fac,
                norm_freq = sensor.norm_freq,
                nzeros = sensor.nzeros,
                zeros = sensor.zeros,
                npoles = sensor.npoles,
                poles = sensor.poles)

            self.__blk43.append(b1)
            self.__num += 1
            self.__used_sensor[name] = k1

        calib = self.__cfgroot.seismometer[name].calib.get(dev_id)
        if calib is not None and calib.component[compn].gain is not None:
            resp_name = "GS" + name + "_" + dev_id
            gain = calib.component[compn].gain
        else:
            resp_name = "GS" + name
            gain = sensor.gain
            dev_id = None
            compn = None
        
        k2 = self.__used_sensor_calib.get((name, dev_id, compn))
        if k2 is not None:
            return (k1, k2, gain, sensor.norm_freq)
        
        k2 = self.__num + 1

        b2 = _Blockette48(key = k2,
            name = resp_name,
            gain = gain,
            gain_freq = sensor.norm_freq)

        self.__blk48.append(b2)
        self.__num += 1
        self.__used_sensor_calib[(name, dev_id, compn)] = k2
        return (k1, k2, gain, sensor.norm_freq)

    def lookup_analogue_paz(self, name):
        k = self.__used_analogue_paz.get(name)
        if k is not None:
            return k

        resp_paz = self.__cfgroot.resp_paz.get(name)
        if resp_paz is None:
            raise SEEDError, "unknown PAZ response: " + name

        if resp_paz.deci_fac is not None:
            raise SEEDError, "expected analogue response, found digital"
        
        k1 = self.__num + 1
        k2 = self.__num + 2

        b1 = _Blockette43(key = k1,
            name = "RA" + name,
            type = "A",
            input_units = self.__unit_dict.lookup("V"),
            output_units = self.__unit_dict.lookup("V"),
            norm_fac = resp_paz.norm_fac,
            norm_freq = resp_paz.norm_freq,
            nzeros = resp_paz.nzeros,
            zeros = resp_paz.zeros,
            npoles = resp_paz.npoles,
            poles = resp_paz.poles)

        b2 = _Blockette48(key = k2,
            name = "GA" + name,
            gain = resp_paz.gain,
            gain_freq = resp_paz.norm_freq)

        self.__blk43.append(b1)
        self.__blk48.append(b2)
        self.__num += 2
        self.__used_analogue_paz[name] = (k1, k2)
        return (k1, k2, resp_paz.gain)

    def lookup_digitizer(self, name, dev_id, compn):
        digi = self.__cfgroot.datalogger.get(name)
        if digi is None:
            raise SEEDError, "unknown datalogger: " + name

        k = self.__used_digitizer.get(name)
        if k is None:
            k1 = self.__num + 1
            k2 = self.__num + 2

            b1 = _Blockette44(key = k1,
                name = "RL" + name,
                type = "D",
                input_units = self.__unit_dict.lookup("V"),
                output_units = self.__unit_dict.lookup("COUNTS"))

            b2 = _Blockette47(key = k2,
                name = "DL" + name,
                input_rate = digi.primary_rate,
                deci_fac = 1,
                deci_offset = 0,
                delay = 0,
                correction = 0)

            self.__blk44.append(b1)
            self.__blk47.append(b2)
            self.__num += 2
            self.__used_digitizer[name] = (k1, k2)
        else:
            (k1, k2) = k

        calib = self.__cfgroot.datalogger[name].calib.get(dev_id)
        if calib is not None and calib.component[compn].gain is not None:
            resp_name = "GL" + name + "_" + dev_id
            gain = calib.component[compn].gain
        else:
            resp_name = "GL" + name
            gain = digi.gain
            dev_id = None
            compn = None
        
        k3 = self.__used_digitizer_calib.get((name, dev_id, compn))
        if k3 is not None:
            return (k1, k2, k3, digi.primary_rate, gain)

        k3 = self.__num + 1
        
        b3 = _Blockette48(key = k3,
            name = resp_name,
            gain = gain,
            gain_freq = 0)
        
        self.__blk48.append(b3)
        self.__num += 1
        self.__used_digitizer_calib[(name, dev_id, compn)] = k3
        return (k1, k2, k3, digi.primary_rate, gain)

    def lookup_digital_paz(self, name):
        k = self.__used_digital_paz.get(name)
        if k is not None:
            return k

        resp_paz = self.__cfgroot.resp_paz.get(name)
        if resp_paz is None:
            raise SEEDError, "unknown PAZ response: " + name

        if resp_paz.deci_fac is None:
            raise SEEDError, "expected digital response, found analogue"
        
        k1 = self.__num + 1
        k2 = self.__num + 2
        k3 = self.__num + 3

        b1 = _Blockette43(key = k1,
            name = "RD" + name,
            type = "D",
            input_units = self.__unit_dict.lookup("COUNTS"),
            output_units = self.__unit_dict.lookup("COUNTS"),
            norm_fac = resp_paz.norm_fac,
            norm_freq = resp_paz.norm_freq,
            nzeros = resp_paz.nzeros,
            zeros = resp_paz.zeros,
            npoles = resp_paz.npoles,
            poles = resp_paz.poles)

        b2 = _Blockette47(key = k2,
            name = "DD" + name,
            input_rate = input_rate,
            deci_fac = 1,
            deci_offset = 0,
            delay = 0,
            correction = 0)

        b3 = _Blockette48(key = k3,
            name = "GD" + name,
            gain = resp_paz.gain,
            gain_freq = resp_paz.norm_freq)

        self.__blk43.append(b1)
        self.__blk47.append(b2)
        self.__blk48.append(b3)
        self.__num += 3
        self.__used_digital_paz[name] = (k1, k2, k3)
        return (k1, k2, k3, resp_paz.gain)

    def lookup_fir(self, name, input_rate):
        resp_fir = self.__cfgroot.resp_fir.get(name)
        if resp_fir is None:
            raise SEEDError, "unknown FIR response: " + name

        k = self.__used_fir.get(name)
        if k is None:
            k1 = self.__num + 1
            k3 = self.__num + 2

            b1 = _Blockette41(key = k1,
                name = "RF" + name,
                symmetry = resp_fir.symmetry,
                input_units = self.__unit_dict.lookup("COUNTS"),
                output_units = self.__unit_dict.lookup("COUNTS"),
                ncoeff = resp_fir.ncoeff,
                coeff = resp_fir.coeff)
                
            b3 = _Blockette48(key = k3,
                name = "GF" + name,
                gain = resp_fir.gain,
                gain_freq = 0)

            self.__blk41.append(b1)
            self.__blk48.append(b3)
            self.__num += 2
            self.__used_fir[name] = (k1, k3)
        else:
            (k1, k3) = k

        k = self.__used_fir_deci.get((name, input_rate))
        if k is None:
            k2 = self.__num + 1
            b2 = _Blockette47(key = k2,
                name = "DF" + name + "_" + str(input_rate).replace(".", "_"),
                input_rate = input_rate,
                deci_fac = resp_fir.deci_fac,
                deci_offset = 0,
                delay = resp_fir.delay / input_rate,
                correction = resp_fir.correction / input_rate)

            self.__blk47.append(b2)
            self.__num += 1
            output_rate = input_rate / resp_fir.deci_fac
            self.__used_fir_deci[(name, input_rate)] = (k2, output_rate)
        else:
            (k2, output_rate) = k

        return (k1, k2, k3, output_rate, resp_fir.gain)

    def output(self, f):
        for b in self.__blk41:
            b.output(f)
            
        for b in self.__blk43:
            b.output(f)
            
        for b in self.__blk44:
            b.output(f)
            
        for b in self.__blk47:
            b.output(f)
            
        for b in self.__blk48:
            b.output(f)
            
class _Channel(object):   # blk 52
    def __init__(self, cfgroot, netcfg, tpcfg, strmcfg, compn,
        format_dict, unit_dict, gen_dict, resp_dict):

        self.__id = (strmcfg.loc_id, strmcfg.code, compn)
        self.__blk = []
        
        sensor = cfgroot.seismometer.get(strmcfg.seismometer)
        if sensor is None:
            raise SEEDError, "unknown sensor: " + strmcfg.seismometer

        digi = cfgroot.datalogger.get(strmcfg.datalogger)
        if digi is None:
            raise SEEDError, "unknown datalogger: " + strmcfg.datalogger

        stream_deci = digi.stream.get((strmcfg.sample_rate_num, strmcfg.sample_rate_denom))
        if stream_deci is None:
            raise SEEDError, "cannot find filter chain for stream " + \
                str(strmcfg.sample_rate_num) + "/" + \
                str(strmcfg.sample_rate_denom) + " of datalogger " + \
                strmcfg.datalogger
 
        if sensor.nzeros == 0:       # check!
            signal_units = unit_dict.lookup("M/S**2")
        else:
            signal_units = unit_dict.lookup("M/S")
        
        sample_rate = float(strmcfg.sample_rate_num) / \
            float(strmcfg.sample_rate_denom)
        
        chan_blk = _Blockette52(loc_id = strmcfg.loc_id,
            chan_id = strmcfg.code + strmcfg.component[compn].code,
            instr_id = gen_dict.lookup_sensor(strmcfg.seismometer),
            comment = "",
            signal_units = signal_units,
            calibration_units = unit_dict.lookup("A"),    # check!
            latitude = tpcfg.latitude,
            longitude = tpcfg.longitude,
            elevation = tpcfg.elevation,
            local_depth = strmcfg.depth,
            azimuth = strmcfg.component[compn].azimuth,
            dip = strmcfg.component[compn].dip,
            data_format = format_dict.lookup(strmcfg.format),
            record_length = 12,
            sample_rate = sample_rate,
            clock_drift = digi.max_clock_drift / sample_rate,
            flags = strmcfg.flags,
            start_date = tpcfg.start,
            end_date = tpcfg.end)

        resp_blk = _Blockette60(1)
        (k1, k2, sens, sens_freq) = resp_dict.lookup_sensor(strmcfg.seismometer,
            strmcfg.seismometer_sn, compn)
        resp_blk.add_stage(k1, k2)

        if len(stream_deci.analogue_filter_chain) > 0:
            for f in stream_deci.analogue_filter_chain.split(", "):
                f = f.strip()
                if f[0:4] == "PAZ_":
                    (k1, k2, gain) = resp_dict.lookup_analogue_paz(f[4:])
                    sens *= gain
                else:
                    raise SEEDError, "invalid filter type: " + f

                resp_blk.add_stage(k1, k2)

        (k1, k2, k3, rate, gain) = resp_dict.lookup_digitizer(strmcfg.datalogger,
            strmcfg.datalogger_sn, compn)
        sens *= gain
        resp_blk.add_stage(k1, k2, k3)
        
        if len(stream_deci.digital_filter_chain) > 0:
            for f in stream_deci.digital_filter_chain.split(", "):
                f = f.strip()
                if f[0:4] == "PAZ_":
                    (k1, k2, k3, gain) = resp_dict.lookup_digital_paz(f[4:])
                elif f[0:4] == "FIR_":
                    (k1, k2, k3, rate, gain) = resp_dict.lookup_fir(f[4:], rate)
                else:
                    raise SEEDError, "invalid filter type: " + f

                sens *= gain
                resp_blk.add_stage(k1, k2, k3)

        if sens_freq > rate / 5:
            sens_freq = rate / 5
        
        sens_blk = _Blockette58(stage = 0,
            sensitivity = sens,
            frequency = sens_freq)
        
        self.__blk.append(chan_blk)
        self.__blk.append(resp_blk)
        self.__blk.append(sens_blk)
 
    def __cmp__(self, other):
        if(self.__id < other.__id):
            return -1

        if(self.__id > other.__id):
            return 1

        return 0
    
    def output(self, f):
        for b in self.__blk:
            b.output(f)
            
class _StationTimePeriod(object):
    def __init__(self, cfgroot, netcfg, statcfg, tpcfg,
        format_dict, unit_dict, gen_dict, resp_dict):
        self.__cfgroot = cfgroot
        self.__netcfg = netcfg
        self.__tpcfg = tpcfg
        self.__format_dict = format_dict
        self.__unit_dict = unit_dict
        self.__gen_dict = gen_dict
        self.__resp_dict = resp_dict
        self.__recno = 0
        self.__channel = {}

        self.__stat_blk = _Blockette50(stat_code = statcfg.code,
            latitude = tpcfg.latitude,
            longitude = tpcfg.longitude,
            elevation = tpcfg.elevation,
            site_name = tpcfg.description,
            net_id = gen_dict.lookup_network(netcfg.code),
            net_code = netcfg.code,
            start_date = tpcfg.start,
            end_date = tpcfg.end)
            
    def __cmp__(self, other):
        if(self.__tpcfg.start < other.__tpcfg.start):
            return -1

        if(self.__tpcfg.start > other.__tpcfg.start):
            return 1

        return 0
    
    def add_chan(self, chan, loc):
        if self.__channel.get((chan, loc)) is not None:
            return

        strmcfg = self.__tpcfg.stream.get((chan[0:2], loc))
        if strmcfg is None:
            raise SEEDError, "unknown stream: " + loc + "." + chan

        for compn in range(0, 3):
            if strmcfg.component[compn].code == chan[2]:
                self.__channel[(chan, loc)] = _Channel(self.__cfgroot,
                    self.__netcfg, self.__tpcfg, strmcfg, compn,
                    self.__format_dict, self.__unit_dict, self.__gen_dict,
                    self.__resp_dict)

                return

        raise SEEDError, "unknown stream: " + loc + "." + chan
                
    def get_recno(self):
        return self.__recno
    
    def output(self, f):
        self.__recno = f.recno
        self.__stat_blk.output(f)
        chan_list = self.__channel.values()
        chan_list.sort()
        for c in chan_list:
            c.output(f)
            
class _Station(object):
    def __init__(self, cfgroot, netcfg, statcfg, format_dict, unit_dict,
        gen_dict, resp_dict):
        self.__cfgroot = cfgroot
        self.__netcfg = netcfg
        self.__statcfg = statcfg
        self.__format_dict = format_dict
        self.__unit_dict = unit_dict
        self.__gen_dict = gen_dict
        self.__resp_dict = resp_dict
        self.__stat_tp = {}

    def add_chan_tp(self, chan, loc, start_time, end_time):
        for tpcfg in self.__statcfg.time_period:
            if (tpcfg.end is None or start_time <= tpcfg.end) and \
                (end_time is None or end_time >= tpcfg.start):
                tp = self.__stat_tp.get((tpcfg.start, tpcfg.end))
                if tp is None:
                    tp = _StationTimePeriod(self.__cfgroot, self.__netcfg,
                        self.__statcfg, tpcfg, self.__format_dict,
                        self.__unit_dict, self.__gen_dict, self.__resp_dict)
                    self.__stat_tp[(tpcfg.start, tpcfg.end)] = tp

                tp.add_chan(chan, loc)

    def get_recno(self):
        tp_list = self.__stat_tp.values()
        tp_list.sort()
        return [ tp.get_recno() for tp in tp_list ]
    
    def output(self, f):
        tp_list = self.__stat_tp.values()
        tp_list.sort()
        for t in tp_list:
            t.output(f)
            f.flush()
            
class _RecordBuilder(object):
    def __init__(self, type, fd):
        self.recno = 1
        self.__type = type
        self.__fd = fd
        self.__buf = None

    def flush(self):
        if self.__buf != None:
            self.__buf += (4096 - len(self.__buf)) * " "
            self.__fd.write(self.__buf)
            self.__buf = None
    
    def reset(self, type, fd, recno = None):
        self.flush()
        self.__type = type
        self.__fd = fd
        if recno is not None:
            self.recno = recno
    
    def write_blk(self, s):
        if self.__buf == None:
            self.__buf = "%06d%c " % (self.recno, self.__type)
            self.recno += 1
        
        b = 0
        while len(s) - b > 4096 - len(self.__buf):
            e = b + 4096 - len(self.__buf)
            self.__buf += s[b:e]
            self.__fd.write(self.__buf)
                
            self.__buf = "%06d%c*" % (self.recno, self.__type)
            self.recno += 1
            b = e

        self.__buf += s[b:]
            
        if len(self.__buf) > 4088:
            self.flush()    

class SEEDVolume(object):
    def __init__(self, cfgroot, organization, label):
        self.__cfgroot = cfgroot
        self.__organization = organization
        self.__label = label
        self.__vol_start_time = None
        self.__vol_end_time = None
        self.__format_dict = _FormatDict()
        self.__unit_dict = _UnitDict()
        self.__gen_dict = _GenericAbbreviationDict(cfgroot)
        self.__resp_dict = _ResponseDict(cfgroot, self.__unit_dict)
        self.__station = {}

    def add_chan(self, net_code, stat_code, chan, loc, start_time, end_time):
        sta = self.__station.get((net_code, stat_code))
        if sta is None:
            netcfg = self.__cfgroot.network.get(net_code)
            if netcfg is None:
                raise SEEDError, "unknown network: " + net_code

            statcfg = netcfg.station.get(stat_code)
            if statcfg is None:
                raise SEEDError, "unknown station in net " + net_code + \
                    ": " + stat_code

            sta = _Station(self.__cfgroot, netcfg, statcfg, self.__format_dict,
                self.__unit_dict, self.__gen_dict, self.__resp_dict)
            self.__station[(net_code, stat_code)] = sta

        sta.add_chan_tp(chan, loc, start_time, end_time)
        if start_time is not None and (self.__vol_start_time is None or \
            start_time < self.__vol_start_time):
            self.__vol_start_time = start_time

        if end_time is not None and (self.__vol_end_time is None or
            end_time < self.__vol_end_time):
            self.__vol_end_time = end_time

    def add_file(self, fname):
        pass

    def add_station_comment(self, stat_code, begin_time, end_time, comment):
        pass
    
    def __output_vol(self, vol_creat_time, code_list, rb):
        b1 = _Blockette10(record_length = 12,
            start_time = self.__vol_start_time,
            end_time = self.__vol_end_time,
            vol_time = vol_creat_time,
            organization = self.__organization,
            label = self.__label)

        b2 = _Blockette11()

        for s in code_list:
            (net_code, stat_code) = s
            sta = self.__station[s]
            for recno in sta.get_recno():
                b2.add_station(stat_code, recno)
        
        b1.output(rb)
        b2.output(rb)
        rb.flush()
        
    def output(self, fname):
        vol_creat_time = datetime.utcnow()
        code_list = self.__station.keys()
        code_list.sort()

        fd = StringIO.StringIO()
        rb = _RecordBuilder("V", fd)
        self.__output_vol(vol_creat_time, code_list, rb)
        reserved_space = len(fd.getvalue())
        fd.close()

        fd = file(fname, "w")
        fd.seek(reserved_space, 0)

        rb.reset("A", fd)
        self.__format_dict.output(rb)
        self.__gen_dict.output(rb)
        self.__unit_dict.output(rb)
        self.__resp_dict.output(rb)
        rb.flush()

        rb.reset("S", fd)
        for s in code_list:
            sta = self.__station[s]
            sta.output(rb)

        fd.seek(0, 0)
        rb.reset("V", fd, 1)
        self.__output_vol(vol_creat_time, code_list, rb)

