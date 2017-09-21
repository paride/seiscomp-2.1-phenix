# Default handlers, to be overridden by packages, eg.:
#
# def log_info(s):
#     print time.ctime() + " - trigger: " + s
#     sys.stdout.flush()
#
# seiscomp.logs.info = log_info

def info(s):
    print s

def notice(s):
    print s

def warning(s):
    print s

def error(s):
    print s

