#***************************************************************************** 
# dbcore.py
#
# MySQL/DBAPI interface for SeisComP
#
# (c) 2005 Andres Heinloo, GFZ Potsdam
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any later
# version. For more information, see http://www.gnu.org/
#*****************************************************************************

import MySQLdb
from sets import Set
from array import array

try:
    from decimal import Decimal   # Python 2.4
    _have_decimal = True
except ImportError:
    _have_decimal = False         # Will use float for decimal, hope it works

class DBError(Exception):
    pass

class DBField(object):
    def __init__(self, name, iskey, value, fromdb):
        self.name = name
        self.iskey = iskey
        self._value = value

    def set(self, value):
        self._value = value

    def get(self):
        return self._value

    def dbvalue(self):
        if self._value == None:
            return "NULL"

        return repr(self._value)

class _DBRecord(object):
    def __init__(self, key, fields, dirty):
        self.__dict__['_key'] = key
        self.__dict__['_dirty'] = dirty
        self.__dict__['_deleted'] = False
        self.__dict__['__fields'] = fields
        
    def __setattr__(self, name, value):
        try:
            f = self.__dict__['__fields'][name]
        except KeyError:
            raise DBError, "trying to set unknown field '" + name + "'"

        if f.iskey:
            raise DBError, "trying to set key field '" + name + "'"

        f.set(value)
        self.__dict__['_dirty'] = True

    def __getattr__(self, name):
        return self.__dict__['__fields'][name].get()

    def delete(self):
        self.__dict__['_deleted'] = True
    
    def dbkeyvalues(self):
        return [ f.name + "=" + f.dbvalue()
            for f in self.__dict__['__fields'].itervalues()
            if f.iskey ]
    
    def dbfields(self):
        return self.__dict__['__fields'].keys()

    def dbvalues(self):
        return [ f.dbvalue() for f in self.__dict__['__fields'].itervalues() ]

# field_spec: list of f, where
#     f[0] field_type   issubclass(f[0], DBField) == True
#     f[1] field_name
#     f[2] iskey
#     f[3] defaultvalue
#     f[4] null (default True)
#     f[5:] optional parameters (eg., precision)

class _DBRecordFactory(object):
    def __init__(self, field_specs):
        self.__field_specs = field_specs
        self.__fieldset = Set([ f[1] for f in field_specs ])
        self.__keyset = Set([ f[1] for f in field_specs if f[2] ])

    def key(self, fvmap):
        fieldset = Set(fvmap.keys())
        if fieldset != self.__keyset:
            raise DBError, "invalid fieldset: " + str(fieldset)

        return tuple([ fvmap[n] for n in self.__keyset ])

    def new(self, fvmap):
        fieldset = Set(fvmap.keys())
        if not self.__keyset <= fieldset <= self.__fieldset:
            raise DBError, "invalid fieldset: " + str(fieldset)

        key = tuple([ fvmap[n] for n in self.__keyset ])
        fields = dict([(f[1], f[0](f[1], f[2], fvmap.get(f[1], f[3]), False, *f[4:]))
            for f in self.__field_specs ])
        
        return _DBRecord(key, fields, True)
    
    def new_from_db(self, values):
        fvmap = dict(zip(self.__fieldset, values))
        key = tuple([ fvmap[n] for n in self.__keyset ])
        fields = dict([(f[1], f[0](f[1], f[2], fvmap[f[1]], True, *f[4:]))
            for f in self.__field_specs ])
        
        return _DBRecord(key, fields, False)
    
    def dbfields(self):
        return tuple(self.__fieldset)
        
class _DBRecordIterator(object):
    def __init__(self, records):
        self.__iter = records.itervalues()

    def __iter__(self):
        return self

    def next(self):
        r = self.__iter.next()
        while r._deleted:
            r = self.__iter.next()

        return r

class DBTable(object):
    def __init__(self, table, *field_specs):
        self.__table = table
        self.__fac = _DBRecordFactory(field_specs)
        self.__records = {}

    def __iter__(self):
        return _DBRecordIterator(self.__records)
    
    def read_from_db(self, cur, where = None):
        if where == None:
            where_clause = ""
        else:
            where_clause = " WHERE " + where

#       print "SELECT " + ",".join([self.__table + "." + f
#           for f in self.__fac.dbfields()]) + \
#           " FROM " + self.__table + where_clause

        cur.execute("SELECT " + ",".join([self.__table + "." + f
            for f in self.__fac.dbfields()]) +
            " FROM " + self.__table + where_clause)

        r = cur.fetchone()
        while r != None:
            dbrec = self.__fac.new_from_db(r)
            if self.__records.get(dbrec._key) == None:
                self.__records[dbrec._key] = dbrec

            r = cur.fetchone()
    
    def save_to_db(self, cur):
        for dbrec in self.__records.itervalues():
            if dbrec._deleted:
                print "DELETE FROM " + self.__table + \
                    " WHERE " + " AND ".join(dbrec.dbkeyvalues())
#               cur.execute("DELETE FROM " + self.__table +
#                   " WHERE " + " AND ".join(dbrec.dbkeyvalues()))
            elif dbrec._dirty:
                print "REPLACE INTO " + self.__table + \
                    " (" + ",".join([self.__table + "." + f \
                    for f in dbrec.dbfields()]) + ")" + \
                    " VALUES(" + ",".join(dbrec.dbvalues()) + ")"
#               cur.execute("REPLACE INTO " + self.__table +
#                   " (" + ",".join([self.__table + "." + f   
#                   for f in dbrec.dbfields()]) + ")" + 
#                   " VALUES(" + ",".join(dbrec.dbvalues()) + ")")

    def flush(self, cur):
        self.save_to_db(cur)
        self.__records = {}
    
    def dbfields(self):
        return self.__fac.dbfields()
    
    def find(self, **fields):
        return self.__records.get(self.__fac.key(fields))
    
    def insert_new(self, **fields):
        dbrec = self.__fac.new(fields)
        self.__records[dbrec._key] = dbrec;
        return dbrec

    def delete(self, dbrec):
        self.__records[dbrec._key].delete()

class DBInteger(DBField):
    def __init__(self, name, iskey, value, fromdb, null=True):
        self.__null = null
        if null and value == None:
            DBField.__init__(self, name, iskey, None, fromdb)
            return
        
        DBField.__init__(self, name, iskey, int(value), fromdb)

    def set(self, value):
        if self.__null and value == None:
            self._value = None

        self._value = int(value)

class DBFloat(DBField):
    def __init__(self, name, iskey, value, fromdb, null=True):
        self.__null = null
        if null and value == None:
            DBField.__init__(self, name, iskey, None, fromdb)
            return
        
        DBField.__init__(self, name, iskey, float(value), fromdb)

    def set(self, value):
        if self.__null and value == None:
            self._value = None

        self._value = float(value)

if _have_decimal:
    class DBDecimal(DBField):
        def __init__(self, name, iskey, value, fromdb, null=True):
            self.__null = null
            if null and value == None:
                DBField.__init__(self, name, iskey, None, fromdb)
                return
            
            DBField.__init__(self, name, iskey, Decimal(str(value)), fromdb)

        def set(self, value):
            if self.__null and value == None:
                self._value = None

            self._value = Decimal(str(value))

        def dbvalue(self):
            if self._value == None:
                return "NULL"

            return str(self._value)
else:
    DBDecimal = DBFloat

class DBString(DBField):
    def __init__(self, name, iskey, value, fromdb, null=True):
        self.__null = null
        if null and value == None:
            DBField.__init__(self, name, iskey, None, fromdb)
            return
        
        if(isinstance(value, array)):
            DBField.__init__(self, name, iskey, value.tostring(), fromdb)
            return
        
        DBField.__init__(self, name, iskey, str(value), fromdb)

    def set(self, value):
        if self.__null and value == None:
            self._value = None

        self._value = str(value)

class DBBoolean(DBField):
    def __init__(self, name, iskey, value, fromdb, null=True):
        self.__null = null
        if null and value == None:
            DBField.__init__(self, name, iskey, None, fromdb)
            return
        
        DBField.__init__(self, name, iskey, value == True, fromdb)

    def set(self, value):
        if self.__null and value == None:
            self._value = None

        self._value = (value == True)
    
    def dbvalue(self):
        if self._value == None:
            return "NULL"

        return str(int(self._value))

class DBList(DBField):
    def __init__(self, name, iskey, value, fromdb, null=True, sep=",", addspace=True):
        self.__null = null
        if null and value == None:
            DBField.__init__(self, name, iskey, None, fromdb)
            return

        if addspace:
            self.__sep = sep + " "
            if(fromdb):
                value = [ s.strip() for s in value.split(sep) ]
        else:
            self.__sep = sep
            if(fromdb):
                value = value.split(sep)
            
        DBField.__init__(self, name, iskey, list(value), fromdb)

    def set(self, value):
        if self.__null and value == None:
            self._value = None

        self._value = list(value)
    
    def dbvalue(self):
        if self._value == None:
            return "NULL"

        return repr(sep.join(self._value))

class DBAutoTable(DBTable):
    def __init__(self, table, cur):
        field_specs = []
        cur.execute("SHOW COLUMNS FROM " + table)
        r = cur.fetchone()
        while r != None:
            fs = []
            typ = r[1].split("(")[0]
            if typ == "char" or typ == "varchar" or typ == "blob" or \
                typ == "tinyblob" or typ == "mediumblob" or typ == "longblob":
                fs.append(DBString)
            elif typ == "int" or typ == "tinyint" or typ == "smallint":
                fs.append(DBInteger)
            elif typ == "float" or typ == "double":
                fs.append(DBFloat)
            elif typ == "decimal":
                fs.append(DBDecimal)
            else:
                continue     # unknown type

            fs.append(r[0])           # field name
            fs.append(r[3] == "PRI")  # key
            fs.append(r[4])           # default
            fs.append(r[2] == "YES")  # null

            field_specs.append(fs)
            r = cur.fetchone()

        DBTable.__init__(self, table, *field_specs)

# Here is a class that automatically adds to itself all attributes
# of a record. _table must be defined in class body and point to
# an instance of DBTable. _rec must be defined in __init__() and point
# to an instance of DBRecord that belongs to table.

def _getfld(f):
    def getfld(self):
        return self._rec.__getattr__(f)
    return getfld

def _setfld(f):
    def setfld(self, v):
        self._rec.__setattr__(f, v)
    return setfld

class DBRecordWrapperType(type):
    def __new__(cls, classname, bases, classdict):
        table = classdict.get('_table')
        if table != None:
            for f in table.dbfields():
                classdict[f] = property(_getfld(f), _setfld(f))

        return type.__new__(cls, classname, bases, classdict)

class DBRecordWrapper(object):
    __metaclass__ = DBRecordWrapperType

