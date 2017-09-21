import os, seis, seiscomp.mseed


class DataSource(object):
    pass

def _timespan(fname, bufsize=512):

    try:
        fsize = os.path.getsize(fname)
    except OSError:
        raise LookupError # XXX
    f     = file(fname)
    buf   = f.read(bufsize)
    tim1  = seiscomp.mseed.Record(buf).time
    f.seek(fsize-bufsize)
    buf   = f.read(bufsize)
    tim2  = seiscomp.mseed.Record(buf).endtime
    f.close()

#   return tim1.time1970, tim2.time1970
    return tim1, tim2


def _findtime(fname, tim, bufsize=512):
    # searches within an SDS file for the record that
    # contains the specified time
    # -> offset of that record

    tim1, tim2 = _timespan(fname, bufsize)
    if not tim1 <= tim <= tim2:
        return -1

    fsize = os.path.getsize(fname)
    f     = file(fname)
    i0 = 0
    i2 = int(fsize/bufsize)

    while i2-i0 > 1:
        i1 = i0 + (i2-i0)/2
        f.seek(i1*bufsize)
        buf = f.read(bufsize)

        if tim > seiscomp.mseed.Record(buf).time:
            i0 = i1
        else:
            i2 = i1

    f.close()

    return i1*bufsize


def scan(ms, dtmax=1.):

    if not isinstance(ms, list):
        ms = [ ms ]

    windows = []
    t1 = None
    for msfile in ms:
        for rec in seiscomp.mseed.Input(msfile):
            if t1 is None:
                t1, t2 = rec.begtime, rec.endtime
                continue

            dt = (rec.begtime-t2)*rec.fsamp
            if abs(dt)>dtmax:
                windows.append((t1, t2))
                t1 = rec.begtime

            t2 = rec.endtime

    windows.append((t1, t2))
    return windows


class SDS(DataSource):

    def __init__(self, root, bufsize=512):
        self.root    = root
        self.bufsize = bufsize
        self.dtmax   = 86400

    def __fname(self, tim, net, sta, cha, loc):
        # constructs an SDS file name. The file need not exist.
        postfix = tim.strftime("%Y.%j")
        nam = "%s/%d/%s/%s/%s.D/%s.%s.%s.%s.D.%s" % \
            (self.root, tim.yr, net, sta, cha, net, sta, loc, cha, postfix)
        return nam

    def __dname(self, year, net, sta, cha):
        # constructs an SDS directory name. The directory need not exist.
        nam = "%s/%d/%s/%s/%s.D" % (self.root, year, net, sta, cha)
        return nam 

    def __fname_list(self, tim, net="*", sta="*", cha="*", loc="*"):
        # constructs a list of *existing* SDS file names by globbing.
        from glob import glob
        pat = __fname(self, tim, net, sta, cha, loc)
        return glob(pat)

    def __dname_list(self, year, net="*", sta="*", cha="*"):
        # constructs a list of *existing* SDS directory names by globbing.
        from glob import glob
        pat = self.__dname(year, net, sta, cha)
        return glob(pat)


    def __streams(self, tim1, tim2, net="*", sta="*", cha="*", loc="*"):
        # constructs a list of all streams available between tim1 and tim2
        from glob import glob
        years = range(tim1.yr, tim2.yr+1)
        streams = []
        prefixes = []

        for yr in years:
            dirs = self.__dname_list(yr, net, sta, cha)
            for d in dirs:
                files = glob("%s/%s.%s.%s.%s.D.%d.*" % (d,net,sta,loc,cha,yr))
                for f in files:
                    prefix = f[:-9] # remove the .YYYY.DDD
                    if prefix in prefixes:
                        continue
                    prefixes.append(prefix)
                    # XXX to be optimized...
                    stream = prefix.split("/")[-1].split(".")
                    if not stream in streams:
                        streams.append(stream)
        return streams
                        

    def getwin(self, tim1, tim2, net, sta, cha, loc):

        assert tim1 <= tim2 and tim2-tim1 <= self.dtmax

        fname1 = self.__fname(tim1, net, sta, cha, loc)
        fname2 = self.__fname(tim2, net, sta, cha, loc)

        # print fname1,fname2

        try:
            # XXX TO BE FIXED XXX
            # Very crude check to see if both start and end time
            # fall within available time windows. If not, not data
            # is returned.
            offset1 = _findtime(fname1, tim1, self.bufsize)
            offset2 = _findtime(fname2, tim2, self.bufsize) + self.bufsize
        except LookupError: # XXX
            return None

        if fname1 == fname2:
            f = file(fname1)
            f.seek(offset1)
            data = f.read(offset2-offset1)
            f.close()
        else:
            f = file(fname1)
            f.seek(offset1)
            data1 = f.read(os.path.getsize(fname1) - offset1)
            f.close()

            f = file(fname2)
            data2 = f.read(offset2)
            f.close()
            data = data1 + data2

        return data

    def waveform_request(self, waveform_request_batch):
        pass

    def inventory(self, tim1, tim2, net="*", sta="*", cha="*", loc="*"):
        # derives inventory information (available time windows) from the SDS
        # file hierarchy itself (not from content files etc.)

        from glob import glob

        # first we need to determine which streams are available
        # for the given time span at all
        streams = self.__streams(tim1, tim2, net, sta, cha, loc)
        streams.sort()

        # finally for each stream the inventory is computed
        for stream in streams:
            net, sta, loc, cha, typ = stream

            files = []

            # loop over days from tim1 to tim2
            day1 = int(tim1.time1970/86400)
            day2 = int(tim2.time1970/86400)
            for day in xrange(day1, day2+1):
                t = seis.Time(time1970=day*86400)
                f = self.__fname(t, net, sta, cha, loc)
                if os.path.isfile(f):
                    files.append(f)

            windows = scan(files)
            for win in windows:
                t1, t2 = win
                print net, sta, loc, cha, typ, str(t1), str(t2)

        return

#       if tim1.yr == tim2.yr:
#           # simplest case
#           pass
#       else:
        if True:
            # lot of globbing required
            years = range(tim1.yr, tim2.yr+1)

            for yr in years:
                dirs = self.__dname_list(yr, net, sta, cha)
                for d in dirs:
                    files = glob("%s/%s.%s.%s.%s.D.%d.*" \
                                % (d,net,sta,loc,cha,yr))
                    for f in files:
                        print f

