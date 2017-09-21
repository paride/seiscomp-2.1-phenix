#***************************************************************************** 
# cfgtree.py
#
# Interface to SeisComP configuration database
#
# (c) 2005 Andres Heinloo, GFZ Potsdam
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any later
# version. For more information, see http://www.gnu.org/
#*****************************************************************************

from time import strptime
from datetime import date
from seiscomp.cfgdb.dbcore import *

#    type       name                     key  default  null

_tab_network = DBTable("network",
    (DBString,  "net_code",              True,  "",    False),
    (DBString,  "net_class",             False, None,  True),
    (DBString,  "description",           False, None,  True),
    (DBString,  "institutions",          False, None,  True),
    (DBString,  "region",                False, None,  True),
    (DBString,  "type",                  False, None,  True),
    (DBDecimal, "start_year",            False, None,  True),
    (DBDecimal, "start_month",           False, None,  True),
    (DBDecimal, "end_year",              False, None,  True),
    (DBDecimal, "end_month",             False, None,  True),
    (DBBoolean, "continuous",            False, True,  False),
    (DBBoolean, "available",             False, True,  False),
    (DBBoolean, "restricted",            False, False, False),
    (DBString,  "remark",                False, None,  True),
    (DBString,  "map_url",               False, None,  True),
    (DBString,  "info_url",              False, None,  True))

_tab_stat_info = DBTable("stat_info",
    (DBString,  "net_code",              True,  "",    False),
    (DBString,  "stat_code",             True,  "",    False),
    (DBDecimal, "start_year",            True,  0,     False),
    (DBDecimal, "start_day",             True,  0,     False),
    (DBInteger, "start_time",            False, None,  True),
    (DBDecimal, "end_year",              False, None,  True),
    (DBDecimal, "end_day",               False, None,  True),
    (DBInteger, "end_time",              False, None,  True),
    (DBDecimal, "latitude",              False, None,  True),
    (DBDecimal, "longitude",             False, None,  True),
    (DBDecimal, "elevation",             False, None,  True),
    (DBString,  "description",           False, None,  True),
    (DBString,  "place",                 False, None,  True),
    (DBString,  "country",               False, None,  True),
    (DBString,  "server_url",            False, None,  True))

_tab_stat_pos = DBTable("stat_pos",
    (DBString,  "net_code",              True,  "",    False),
    (DBString,  "stat_code",             True,  "",    False),
    (DBString,  "stream_code",           True,  "",    False),
    (DBString,  "loc_id",                True,  "",    False),
    (DBDecimal, "start_year",            True,  0,     False),
    (DBDecimal, "start_day",             True,  0,     False),
    (DBInteger, "start_time",            False, None,  True),
    (DBDecimal, "end_year",              False, None,  True),
    (DBDecimal, "end_day",               False, None,  True),
    (DBInteger, "end_time",              False, None,  True),
    (DBString,  "datalogger",            False, None,  True),
    (DBString,  "datalogger_sn",         False, None,  True),
    (DBString,  "seismometer",           False, None,  True),
    (DBString,  "seismometer_sn",        False, None,  True),
    (DBInteger, "sample_rate_num",       False, None,  True),
    (DBInteger, "sample_rate_denom",     False, None,  True),
    (DBString,  "comp1_code",            False, None,  True),
    (DBDecimal, "comp1_azimuth",         False, None,  True),
    (DBDecimal, "comp1_dip",             False, None,  True),
    (DBString,  "comp2_code",            False, None,  True),
    (DBDecimal, "comp2_azimuth",         False, None,  True),
    (DBDecimal, "comp2_dip",             False, None,  True),
    (DBString,  "comp3_code",            False, None,  True),
    (DBDecimal, "comp3_azimuth",         False, None,  True),
    (DBDecimal, "comp3_dip",             False, None,  True),
    (DBDecimal, "depth",                 False, None,  True),
    (DBString,  "format",                False, None,  True),
    (DBString,  "flags",                 False, "",    True),
    (DBBoolean, "hide",                  False, False, False))

_tab_calibration = DBTable("calibration",
    (DBString,  "device",                True,  "",    False),
    (DBString,  "sn",                    True,  "",    False),
    (DBFloat,   "comp1_gain",            False, None,  True),
    (DBFloat,   "comp2_gain",            False, None,  True),
    (DBFloat,   "comp3_gain",            False, None,  True))

_tab_resp_fir = DBTable("resp_fir",
    (DBString,  "name",                  True,  "",    False),
    (DBFloat,   "gain",                  False, None,  True),
    (DBInteger, "deci_fac",              False, None,  True),
    (DBFloat,   "delay",                 False, 0.0,   True),
    (DBFloat,   "correction",            False, 0.0,   True),
    (DBString,  "symmetry",              False, None,  True),
    (DBInteger, "ncoeff",                False, None,  True),
    (DBString,  "coeff",                 False, None,  True))

_tab_resp_paz = DBTable("resp_paz",
    (DBString,  "name",                  True,  "",    False),
    (DBString,  "type",                  False, None,  True),
    (DBFloat,   "gain",                  False, None,  True),
    (DBFloat,   "norm_fac",              False, None,  True),
    (DBFloat,   "norm_freq",             False, None,  True),
    (DBInteger, "nzeros",                False, None,  True),
    (DBInteger, "npoles",                False, None,  True),
    (DBString,  "zeros",                 False, None,  True),
    (DBString,  "poles",                 False, None,  True))

_tab_datastream = DBTable("datastream",
    (DBString,  "datalogger",            True,  "",    False),
    (DBInteger, "sample_rate_num",       True,  0,     False),
    (DBInteger, "sample_rate_denom",     True,  0,     False),
    (DBString,  "analogue_filter_chain", False, None,  True),
    (DBString,  "digital_filter_chain",  False, None,  True))

_tab_datalogger = DBTable("datalogger",
    (DBString,  "name",                  True,  "",    False),
    (DBString,  "description",           False, None,  True),
    (DBString,  "categ",                 False, None,  True),
    (DBString,  "company",               False, None,  True),
    (DBFloat,   "primary_rate",          False, None,  True),
    (DBFloat,   "gain",                  False, None,  True),
    (DBFloat,   "max_clock_drift",       False, None,  True))

_tab_seismometer = DBTable("seismometer",
    (DBString,  "name",                  True,  "",    False),
    (DBString,  "description",           False, None,  True),
    (DBString,  "type",                  False, None,  True),
    (DBString,  "categ",                 False, None,  True),
    (DBString,  "company",               False, None,  True),
    (DBFloat,   "gain",                  False, None,  True),
    (DBFloat,   "norm_fac",              False, None,  True),
    (DBFloat,   "norm_freq",             False, None,  True),
    (DBInteger, "nzeros",                False, None,  True),
    (DBInteger, "npoles",                False, None,  True),
    (DBString,  "zeros",                 False, None,  True),
    (DBString,  "poles",                 False, None,  True))

def _getfld(f):
    def getfld(self):
        return self._rec.__getattr__(f)
        
    return getfld

def _setfld(f):
    def setfld(self, v):
        self._rec.__setattr__(f, v)
        
    return setfld

def _setfld_copy(f):
    def setfld(self, v):
        self._copy_record_if_needed()
        self._rec.__setattr__(f, v)

    return setfld

def _prop_rw(f, doc = None):
    return property(_getfld(f), _setfld(f), None, doc)

def _prop_rw_copy(f, doc = None):
    return property(_getfld(f), _setfld_copy(f), None, doc)

def _prop_rdonly(f, doc = None):
    return property(_getfld(f), None, None, doc)

def _Component(rec, n):
    class _Component(object):
        def __init__(self, rec):
            self._rec = rec

        code = _prop_rw('comp' + str(n) + '_code')
        azimuth = _prop_rw('comp' + str(n) + '_azimuth')
        dip = _prop_rw('comp' + str(n) + '_dip')

    return _Component(rec)

class _Stream(object):
    def __init__(self, rec):
        self._rec = rec
        self.component = [
            _Component(rec, 1),
            _Component(rec, 2),
            _Component(rec, 3) ]

    def _delete(self):
        self._rec.delete()

    code = _prop_rdonly('stream_code')
    loc_id = _prop_rw('loc_id')
    datalogger = _prop_rw('datalogger')
    datalogger_sn = _prop_rw('datalogger_sn')
    seismometer = _prop_rw('seismometer')
    seismometer_sn = _prop_rw('seismometer_sn')
    sample_rate_num = _prop_rw('sample_rate_num')
    sample_rate_denom = _prop_rw('sample_rate_denom')
    depth = _prop_rw('depth')
    format = _prop_rw('format')
    flags = _prop_rw('flags')
    
def _get_year(t):
    if t == None:
        return 0

    return t.year

def _get_day(t):
    if t == None:
        return 0

    return t.timetuple()[7]

class _TimePeriod(object):
    def __init__(self, rec, start_time, end_time):
        self._rec = rec
        self.__start_time = start_time
        self.__end_time = end_time
        self.stream = {}
    
    def __cmp__(self, other):
        if(self.__start_time < other.__start_time):
            return -1

        if(self.__start_time > other.__start_time):
            return 1

        return 0
    
    def _add_stat_pos(self, rec):
        self.stream[(rec.stream_code, rec.loc_id)] = _Stream(rec)

    def _delete(self):
        for s in self.stream.iteritems():
            s._delete()

        self.stream.clear()
    
    def insert_stream(self, stream_code, loc_id="", **other):
        s = self.stream.get((stream_code, loc_id))
        if s != None:
            raise DBError, "stream already exists"
        
        rec = _tab_stat_pos.insert_new(net_code = self._rec.net_code,
            stat_code = self._rec.stat_code,
            stream_code = stream_code,
            loc_id = loc_id,
            start_year = _get_year(self.__start_time),
            start_day = _get_day(self.__start_time),
            start_time = 0,
            end_year = _get_year(self.__end_time),
            end_day = _get_day(self.__end_time),
            end_time = 0,
            **other)

        s = _Stream(rec)
        self.stream[(stream_code, loc_id)] = s
        return s

    def remove_stream(self, stream_code, loc_id):
        s = self.stream[(stream_code, loc_id)]
        s._delete()
        del self.stream[(stream_code, loc_id)]

    def _copy_record_if_needed(self):
        if self._rec.start_year == 0:   # using defaults
            self._rec = _tab_stat_info.insert_new(net_code = self._rec.net_code,
                stat_code = self._rec.stat_code,
                start_year = _get_year(self.__start_time),
                start_day = _get_day(self.__start_time),
                start_time = 0,
                end_year = _get_year(self.__end_time),
                end_day = _get_day(self.__end_time),
                end_time = 0,
                latitude = self._rec.latitude,
                longitude = self._rec.longitude,
                elevation = self._rec.elevation,
                description = self._rec.description,
                place = self._rec.place,
                country = self._rec.country,
                server_url = self._rec.server_url)
    
    def __get_start(self):
        return self.__start_time
    
    def __get_end(self):
        return self.__end_time
    
    start = property(__get_start)
    end = property(__get_end)
    latitude = _prop_rw_copy('latitude')
    longitude = _prop_rw_copy('longitude')
    elevation = _prop_rw_copy('elevation')
    description = _prop_rw_copy('description')
    place = _prop_rw_copy('place')
    country = _prop_rw_copy('country')

class _Station(object):
    def __init__(self, rec):
        self._rec = rec
        self.time_period = []
        self.package = {}

    def __mktime(self, year, day, time):  # incomplete
        if year == None or year == 0:
            return None
        else:
            return date(*strptime(str(year)+"."+str(day), "%Y.%j")[:3])
    
    def __insert_time_period(self, start_time, end_time):
        # split overlapping time periods?
            
        for tp in self.time_period:
            if tp.start == start_time and tp.end == end_time:
                return tp

        stat_info_rec = _tab_stat_info.find(net_code = self._rec.net_code,
            stat_code = self._rec.stat_code, start_year = _get_year(start_time),
            start_day = _get_day(start_time))
    
        if stat_info_rec == None:
            stat_info_rec = self._rec
                    
        tp = _TimePeriod(stat_info_rec, start_time, end_time)
        self.time_period.append(tp)
        self.time_period.sort()
        return tp
    
    def _add_stat_pos(self, rec):
        start_time = self.__mktime(rec.start_year, rec.start_day, rec.start_time)
        end_time = self.__mktime(rec.end_year, rec.end_day, rec.end_time)

        tp = self.__insert_time_period(start_time, end_time)
        tp._add_stat_pos(rec)

    def _delete():
        for tp in self.time_period():
            tp._delete()

        self.time_period.clear()
    
    # returns an existing one if found
    def insert_time_period(self, start_time, end_time):
        return self.__insert_time_period(start_time, end_time)
        
    def remove_time_period(self, start_time):
        for tp in self.time_period:
            if tp.start == start_time:
                tp._delete()
                self.time_period.remove(tp)
    
    code = _prop_rw('stat_code')
    latitude = _prop_rw('latitude')
    longitude = _prop_rw('longitude')
    elevation = _prop_rw('elevation')
    description = _prop_rw('description')
    place = _prop_rw('place')
    country = _prop_rw('country')
    server_url = _prop_rw('server_url')

class _DummyRecord(object):
    def __getattr__(self, name):
        return None

    def __setattr__(self, name, value):
        raise DBError, "record does not exist"

class _Network(DBRecordWrapper):
    _table = _tab_network
    code = _prop_rw('net_code')
    def __init__(self, code):
        self._rec = _DummyRecord()
        self.__net_code = code
        self.station = {}

    def _add_stat_pos(self, rec):
        sta = self.station.get(rec.stat_code)
        if sta == None:
            # get default station description
            stat_info_rec = _tab_stat_info.find(net_code=rec.net_code,
                stat_code=rec.stat_code, start_year=0, start_day=0)

            if stat_info_rec == None:
                stat_info_rec = _tab_stat_info.insert_new(net_code=self.__net_code,
                    stat_code=rec.stat_code, start_year=0, start_day=0,
                    start_time=0, end_year=0, end_day=0, end_time=0)
                
            sta = _Station(stat_info_rec)
            self.station[rec.stat_code] = sta

        sta._add_stat_pos(rec)

    def _delete():
        for sta in self.station():
            sta._delete()

        self.station.clear()
    
    def insert_station(self, stat_code, **other):
        sta = self.station.get(stat_code)
        if sta != None:
            raise DBError, "station already exists"

        stat_info_rec = _tab_stat_info.insert_new(net_code=self.__net_code,
            stat_code=stat_code, start_year=0, start_day=0,
            start_time=0, end_year=0, end_day=0, end_time=0, **other)
                    
        sta = _Station(stat_info_rec)
        self.station[stat_code] = sta
        return sta

    def remove_station(self, stat_code):
        sta = self.station[stat_code]
        sta._delete()
        del self.station[stat_code]
        
def _ComponentCalib(rec, n):
    class _ComponentCalib(object):
        def __init__(self, rec):
            self._rec = rec

        gain = _prop_rw('comp' + str(n) + '_gain')

    return _ComponentCalib(rec)

class _Calibration(object):
    def __init__(self, rec):
        self._rec = rec
        self.component = [
            _ComponentCalib(rec, 1),
            _ComponentCalib(rec, 2),
            _ComponentCalib(rec, 3) ]

class _RespFIR(DBRecordWrapper):
    _table = _tab_resp_fir
    def __init__(self, rec):
        self._rec = rec

class _RespPAZ(DBRecordWrapper):
    _table = _tab_resp_paz
    def __init__(self, rec):
        self._rec = rec

class _Seismometer(DBRecordWrapper):
    _table = _tab_seismometer
    def __init__(self, rec):
        self._rec = rec
        self.calib = {}

    def _add_calibration(self, rec):
        self.calib[rec.sn] = _Calibration(rec)
        
class _Datalogger(DBRecordWrapper):
    _table = _tab_datalogger
    def __init__(self, rec):
        self._rec = rec
        self.stream = {}
        self.calib = {}

    def _add_stream(self, rec):
        self.stream[(rec.sample_rate_num, rec.sample_rate_denom)] = rec

    def _add_calibration(self, rec):
        self.calib[rec.sn] = _Calibration(rec)
        
class CFGROOT(object):
    def __init__(self, **connect_args):
        self.network = {}
        self.resp_fir = {}
        self.resp_paz = {}
        self.seismometer = {}
        self.datalogger = {}
        self._tables = [ _tab_network, _tab_stat_info, _tab_stat_pos,
            _tab_calibration, _tab_resp_fir, _tab_resp_paz, _tab_datastream,
            _tab_datalogger, _tab_seismometer ]

        connect_args.setdefault("db", "seiscomp")
        conn = MySQLdb.connect(**connect_args)
        self.__cur = conn.cursor()

    def load_stations(self, net_list = None, stat_list = None, current = False):
        w_list = []
        if net_list != None:
            w_net = " OR ".join([ "net_code='" + x.replace("'", r"\'") + "'" for x in net_list ])
            w_list.append(w_net)
        else:
            w_net = None

        if stat_list != None:
            w_list.append(" OR ".join([ "stat_code='" + x.replace("'", r"\'") + "'" for x in stat_list ]))

        if current:
            w_list.append("end_year=NULL")

        if len(w_list) != 0:
            w_stat = " AND ".join([ "(" + x + ")" for x in w_list ])
        else:
            w_stat = None

        _tab_network.read_from_db(self.__cur, w_net)
        for rec in _tab_network:
            net = self.network.get(rec.net_code)
            if net == None:
                net = _Network(rec.net_code)
                self.network[rec.net_code] = net

            net._rec = rec
        
        _tab_stat_info.read_from_db(self.__cur, w_stat)
        _tab_stat_pos.read_from_db(self.__cur, w_stat)
        for rec in _tab_stat_pos:
            net = self.network.get(rec.net_code)
            if net == None:
                net = _Network(rec.net_code)
                self.network[rec.net_code] = net

            net._add_stat_pos(rec)

    def load_instruments(self, all = True):
        w_seismo = None
        w_logger = None
        w_stream = None
        w_calib = None

        if not all:
            used_sensors = Set()
            used_loggers = Set()
            for net in self.network.itervalues():
                for sta in net.station.itervalues():
                    for tp in sta.time_period:
                        for strm in tp.stream.itervalues():
                            used_sensors.add(strm.seismometer.replace("'", r"\'"))
                            used_loggers.add(strm.datalogger.replace("'", r"\'"))

            if len(used_sensors) > 0:
                w_seismo = " OR ".join([ "name='" + x + "'" for x in used_sensors ])
            if len(used_loggers) > 0:
                w_logger = " OR ".join([ "name='" + x + "'" for x in used_loggers ])
                w_stream = " OR ".join([ "datalogger='" + x + "'" for x in used_loggers ])
            if len(used_sensors | used_loggers) < 0:
                w_calib = " OR ".join([ "device='" + x + "'" for x in used_sensors | used_loggers ])

        _tab_seismometer.read_from_db(self.__cur, w_seismo)
        for rec in _tab_seismometer:
            self.seismometer[rec.name] = _Seismometer(rec)

        _tab_datalogger.read_from_db(self.__cur, w_logger)
        for rec in _tab_datalogger:
            self.datalogger[rec.name] = _Datalogger(rec)

        _tab_calibration.read_from_db(self.__cur, w_calib)
        for rec in _tab_calibration:
            logger = self.datalogger.get(rec.device)
            if logger != None:
                logger._add_calibration(rec)

            seismo = self.seismometer.get(rec.device)
            if seismo != None:
                seismo._add_calibration(rec)
                
        _tab_datastream.read_from_db(self.__cur, w_stream)
        
        w_fir = None
        w_paz = None

        if not all:
            used_fir = Set()
            used_paz = Set()
        
            for rec in _tab_datastream:
                logger = self.datalogger.get(rec.datalogger)
                if logger != None:
                    logger._add_stream(rec)

                if len(rec.analogue_filter_chain) > 0:
                    for f in rec.analogue_filter_chain.split(", "):
                        f = f.strip()
                        if f[:4] == "PAZ_":
                            used_paz.add(f[4:].replace("'", r"\'"))

                if len(rec.digital_filter_chain) > 0:
                    for f in rec.digital_filter_chain.split(", "):
                        f = f.strip()
                        if f[:4] == "FIR_":
                            used_fir.add(f[4:].replace("'", r"\'"))
                        elif f[:4] == "PAZ_":
                            used_paz.add(f[4:].replace("'", r"\'"))

            if len(used_fir) > 0:
                w_fir = " OR ".join([ "name='" + x + "'" for x in used_fir ])

            if len(used_paz) > 0:    
                w_paz = " OR ".join([ "name='" + x + "'" for x in used_paz ])
        else:
            for rec in _tab_datastream:
                logger = self.datalogger.get(rec.datalogger)
                if logger != None:
                    logger._add_stream(rec)

        _tab_resp_fir.read_from_db(self.__cur, w_fir)
        for rec in _tab_resp_fir:
            self.resp_fir[rec.name] = _RespFIR(rec)

        _tab_resp_paz.read_from_db(self.__cur, w_paz)
        for rec in _tab_resp_paz:
            self.resp_paz[rec.name] = _RespPAZ(rec)

    def save(self):
        for t in self._tables:
            t.save_to_db(self.__cur)

    def insert_network(self, net_code, **other):
        net = self.network.get(net_code)
        if net != None:
            raise DBError, "network already exists"

        net = _Network(net_code)
        net._rec = _tab_network.insert_new(net_code=net_code, **other)
        self.network[net_code] = net
        return net

    def remove_network(self, net_code):
        sta = self.network[net_code]
        sta._delete()
        del self.network[net_code]
  
