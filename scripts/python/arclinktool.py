#***************************************************************************** 
# arclinktool.py
#
# Simple ArcLink command-line client
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
from optparse import OptionParser
from seiscomp.alclient import *

VERSION = "0.1 (2004.245)"

def _err_print(s):
    print >>sys.stderr, s
    sys.stderr.flush()

arcl = Arclink(_err_print)

def submit(file):
    reqf = open(file)
    try:
        req_id = arcl.submit(reqf)
    finally:
        reqf.close()
    
    print "Request successfully submitted"
    print "Request ID:", req_id

def show_status(req_id):
    status = arcl.get_status(req_id)

    for req in status.request:
        if req.ready:
            req_status = "READY"
        else:
            req_status = "PROCESSING"

        print "Request ID: %s, Type: %s, Status: %s, Size: %d" % \
          (req.id, req.type, req_status, req.size)

        if req.user != "":
            print "User: %s, Institution: %s" % (req.user, req.institution)

        for vol in req.volume:
            print "Volume ID: %s, Status: %s, Size: %d\nInfo: %s" % \
              (vol.id, arclink_status_string(vol.status), vol.size, vol.message)

            for rqln in vol.line:
                print "    Request: %s" % (rqln.content,)
                print "    Status: %s, Size: %d, Info: %s" % \
                  (arclink_status_string(rqln.status), rqln.size, rqln.message)
 
def download(req_vol):
    rv = req_vol.split(".", 1)
    req_id = rv[0]
    if len(rv) == 1:
        vol_id = None
    else:
        vol_id = rv[1]

    outfd = open(outf, "w")
    try:
        arcl.download(outfd, req_id, vol_id)
    finally:
        outfd.close()

    print "Download successful"

def purge(req_id):
    arcl.purge(req_id)
    print "Product successfully deleted"

def process_options():
    parser = OptionParser(usage="usage: %prog -u user [-i institution] [-o file] {-r|-s|-d|-p} host:port",
      version="%prog v" + VERSION)

    parser.add_option("-u", "--user", type="string", dest="user",
      help="user's e-mail address")

    parser.add_option("-i", "--institution", type="string", dest="inst",
      help="user's institution")

    parser.add_option("-o", "--output-file", type="string", dest="outf",
      help="file where downloaded data is written")

    parser.add_option("-r", "--submit", type="string", dest="request_file",
      help="submit request")

    parser.add_option("-s", "--status", type="string", dest="status_id",
      help="check status")

    parser.add_option("-d", "--download", type="string", dest="download_id",
      help="download product")

    parser.add_option("-p", "--purge", type="string", dest="purge_id",
      help="delete product from the server")

    (options, args) = parser.parse_args()

    if len(args) != 1:
        parser.error("incorrect number of arguments")

    m = re.compile(r'([^:]+):([0-9]{1,5})').match(args[0])
    if m == None:
        parser.error("address not in form of host:port")

    (host, port) = m.groups()
    
    if options.user == None:
        parser.error("username required")
    
    action = None
    action_arg = None

    if options.request_file != None:
        action = submit
        action_arg = options.request_file

    if options.status_id != None:
        if action != None:
            parser.error("conflicting options");
            
        action = show_status
        action_arg = options.status_id

    if options.download_id != None:
        if action != None:
            parser.error("conflicting options");

        if options.outf == None:
            parser.error("output file required")
        
        action = download
        action_arg = options.download_id

    if options.purge_id != None:
        if action != None:
            parser.error("conflicting options");

        action = purge
        action_arg = options.purge_id

    if action == None:
        parser.error("one of -r/-s/-d/-p must be given")

    return (options.user, options.inst, options.outf, host, int(port),
      action, action_arg)

(user, inst, outf, host, port, action, action_arg) = process_options()

try:
    arcl.open_connection(host, port, user, inst)
    print "Connected to", arcl.software, "at", arcl.organization
    action(action_arg)
    sys.exit(0)

except ArclinkAuthFailed:
    print "Authentification failed"

except ArclinkCommandNotAccepted:
    print "Error:", arcl.get_errmsg()

except ArclinkError, e:
    print "Error:", str(e)

sys.exit(1)

