#!/usr/bin/env python

import  os, sys, re, seis
from    seis.tools  import commented_file
from    sds         import SDS
from    fnmatch     import fnmatch
from    getopt      import getopt, GetoptError

usage_info = """
Usage: reqhandler [-d tmpdir] [-f in_fd:out_fd]
"""

def usage(exitcode=0):
    sys.stderr.write(usage_info)
    sys.exit(exitcode)

tmpdir = "/tmp"

class Router(object):

    def __init__(self, conf):
        self.__read_conf(conf)

    def __read_conf(self, conf):
        self.__route = []

        for line in commented_file(conf):
            tmp = line.split()
            pat, start, end, name, meth = tmp[:5]
            # XXX THIS IS A MESS. WILL BE CHANGED SOON
            if meth == "SDS":
                sds_root = tmp[5]
                source = SDS(sds_root)
            self.__route.append( (pat, start, end, name, source) )

    def route(self, line):

        for pat, start, end, name, source in self.__route:
            stream = "%s.%s.%s.%s" % (line.net, line.sta, line.loc, line.cha)
            if fnmatch(stream, pat):
                return (name, source)

        return None


class RequestHandler(object):

    def __init__(self, outfile=sys.stdout, logfile=sys.stderr):
        self.router  = Router("router.conf")
        self.outfile = outfile
        self.logfile = logfile

    def msg(self, txt):
        self.outfile.write(txt+"\n")
        self.outfile.flush()

    def feed(self, request):

        volumes = {}

        for lineno, line in enumerate(request.rq):
            # 'name' is the name of the router
            # 'source' is the corresponding data source that provides
            # a getwin() method.
            (name, source) = self.router.route(line)
            self.msg("STATUS LINE %i PROCESSING %s" % (lineno, name))
            self.msg("STATUS LINE %i MESSAGE some funny message" % lineno)

            if not name in volumes:
                volumes[name] = []
            volumes[name].append( (lineno, line, source) )

        for v in volumes:

            v_flen = 0
            v_fnam = "%s/%s.%s" % (tmpdir, request.id, v)
            v_file = file(v_fnam, "w")

            for lineno, line, source in volumes[v]:

                # additional steps to be implemented:
                #   * query inventory for time window
                #   * stop if whole time window unavailable
                #   * issue MESSAGE about partially available windows

                self.msg("STATUS LINE %i MESSAGE working" % lineno)

                # request the data from data source
                data = source.getwin(line.t1,  line.t2, \
                                     line.net, line.sta, line.cha, line.loc)
                if data:
                    n = len(data)
                    v_flen += n
                    self.msg("STATUS LINE %i SIZE %ld" % (lineno, n))
                    self.msg("STATUS LINE %i OK" % lineno)
                    v_file.write(data)
                else:
                    self.msg("STATUS LINE %i NODATA" % lineno)


            v_file.close()
            if v_flen == 0:
                self.msg("STATUS VOLUME %s NODATA" % v)
                continue

            self.msg("STATUS VOLUME %s SIZE %ld" % (v, v_flen))
            # XXX create SEED volume here
            # self.msg("STATUS VOLUME %s MESSAGE %ld bytes seed" % (v, seedlen))
            self.msg("STATUS VOLUME %s MESSAGE wrote %s" % (v, v_fnam))
            self.msg("STATUS VOLUME %s OK" % v)
        
        # Tell arclink that the request is finished. At this point,
        # arclink may shut down the request handler with TERM signal
        # or feed the next request.
        self.msg("END")


class RequestLine(object):

    # can probably be replace by a simple tuple

    def __init__(self, line):
        ll = line.split()
        nf = len(ll)
        if   nf == 6:
            t1, t2, self.net, self.sta, self.cha, self.loc = ll
            if self.loc == ".":
                self.loc = ""
        elif nf == 5:
            t1, t2, self.net, self.sta, self.cha           = ll
            self.loc = ""
        else:
            raise SyntaxError, line

        self.t1 = seis.Time(t1)
        self.t2 = seis.Time(t2)


def generate_id():
    return "GENERATED_ID"


class Request(object):
    pass


class InventoryRequest(Request):
    # yet to be implemented
    pass


class WaveformRequest(Request):

    regex = re.compile("[0-9,]+ +[0-9,]+ +[A-Z][A-Z] +[A-Z0-9]+ +[A-Z][A-Z][A-Z0-9] *[0-9]*")

    def __init__(self, id=None):
        self.id = id
        self.rq = []

    def add(self, line):

        if self.regex.match(line):
            self.rq.append( RequestLine(line) )
        else:
            raise SyntaxError, "'%s'" % line

    def read(self, infile, outfile=sys.stdout):

        begin_regex = re.compile("REQUEST +(WAVEFORM|INVENTORY) *(.*) *")

        # read input lines until a valid REQUEST line is found
        while True:
            line = infile.readline(80)
            if not line:
                raise EOFError

            line = line.strip()
            if not line: # ignore blank lines outside requests
                continue

            match = begin_regex.match(line)
            if not match:
#               raise SyntaxError, "'%s'" % line
                continue

            keyword, id = match.groups()
            if keyword != "WAVEFORM":
#               outfile.write("%s not available\n" % keyword)
                continue

            break # assume we succeeded

        if keyword == "WAVEFORM":
            # read rest of message until END
            while True:
                line = infile.readline(80)
                if not line:
                    raise EOFError
                line = line.strip()
                if not line:
                    raise SyntaxError, "missing END statement"
                if line.startswith("END"):
                    break

                self.add(line)

            # valid but empty BEGIN/END block
            if not self.rq:
                raise SyntaxError, "empty request"

        # assign a request id
        if not id:
            id = generate_id()
        self.id = id


def read_requests(infile, outfile):
    # simple iterator

    while True:
        req = WaveformRequest()
        try:
            req.read(infile)
            yield req
        except SyntaxError, e:
            outfile.write("SYNERR %s\n" % e)
            outfile.flush()
            continue
        except EOFError:
            return


fd_in, fd_out = 0, 1

try:
    opts, args = getopt(sys.argv[1:], "d:f:h")
except GetoptError:
    usage(exitcode=2)

for flag, arg in opts:
    if   flag == "-d":
        tmpdir = arg
    elif flag == "-f":
        try:
            fd_in, fd_out = map(int,arg.split(':'))
        except ValueError:
            usage(1)
    elif flag == "-h":
        usage(0)

infile  = os.fdopen(fd_in)
outfile = os.fdopen(fd_out, "w")

handler = RequestHandler(outfile=outfile)

for request in read_requests(infile, outfile):
    handler.feed(request)
