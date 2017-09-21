import sys
import time
import termios
import tty
from datetime import datetime, timedelta
from seiscomp import iniparser, plugin
from optparse import OptionParser

VERSION     = "1.0 ()"
CONFIG_FILE = "/home/sysop/config/plugins.ini"
BUFSIZE     = 100

baud_rates = {  2400:  termios.B2400,
                4800:  termios.B4800,
                9600:  termios.B9600,
               19200: termios.B19200,
               38400: termios.B38400 }

class DataBuffer(object):
    def __init__(self, station_name, channel_name, sample_rate, signal_offset,
      time_offset, max_time_error):
        self.station_name = station_name
        self.channel_name = channel_name
        self.sample_rate = sample_rate
        self.signal_offset = signal_offset
        self.time_offset = timedelta(microseconds = time_offset)
        self.max_time_error = timedelta(microseconds = max_time_error)
        self.data_array = []
        self.nsamples = -1
        self.buf_start_time = None
        self.buf_end_time = None
        self.time_diff = timedelta()
    
    def put_sample(self, x):
        t = datetime.utcnow()
        self.data_array.append(x - self.signal_offset)
        
        if self.nsamples == -1:
            self.buf_start_time = t
            self.buf_end_time = t
            self.nsamples = 0
        elif self.nsamples == 0:
            self.buf_start_time = self.buf_end_time

        d = t - self.buf_end_time
        self.time_diff = (self.time_diff * self.nsamples + d) / (self.nsamples + 1)
        self.nsamples += 1
        self.buf_end_time = self.buf_start_time + \
          timedelta(microseconds = 1000000 * self.nsamples / self.sample_rate)
        
        if self.nsamples == BUFSIZE:
            self.flush()

    def flush(self):
        if self.nsamples == 0:
            return

        if abs(self.time_diff) > self.max_time_error:
            self.buf_start_time += self.time_diff
            self.buf_end_time += self.time_diff
            self.time_diff = timedelta()
        
        time_corr = (self.time_diff.days * 24 * 60 * 60 + \
          self.time_diff.seconds) * 1000000 + self.time_diff.microseconds

        plugin.send_raw(self.station_name, self.channel_name,
          self.buf_start_time - self.time_offset, time_corr, -1,
          self.data_array)
        
        self.data_array = []
        self.nsamples = 0
        self.time_diff = timedelta()

def log_print(s):
    print time.ctime() + " - " + plugin_name + ": " + s
    sys.stdout.flush()

def process_options():
    global plugin_name, port, baud, station, channel, sample_rate, \
      signal_offset, time_offset, max_time_error

    parser = OptionParser(usage="usage: %prog [options] plugin_name",
      version="%prog v" + VERSION)

    parser.add_option("-f", "--config-file", type="string", dest="config_file",
      default=CONFIG_FILE, help="alternative configuration file")

    (options, args) = parser.parse_args()

    if len(args) != 1:
        parser.error("incorrect number of arguments")

    plugin_name = args[0]

    log_print("reading configuration from " + options.config_file)
    config = iniparser.read_ini(options.config_file, log_print)

    as1_config = config.get(plugin_name)
    if as1_config == None:
        log_print("section '" + plugin_name + "' does not exist in file '" + \
          config_file + "'")
        sys.exit(1)

    port = as1_config.get("port")
    if port == None:
        log_print("serial port is not specified")
        sys.exit(1)

    baud_s = as1_config.get("speed")
    if baud_s == None:
        log_print("baud rate is not specified")
        sys.exit(1)
    
    try:
        baud_i = int(baud_s)
    except ValueError:
        log_print("invalid baud rate")
        sys.exit(1)
    
    baud = baud_rates.get(baud_i)
    if baud == None:
        log_print("invalid baud rate")
        sys.exit(1)

    station = as1_config.get("station")
    if station == None:
        log_print("station name is not specified")
        sys.exit(1)

    channel = as1_config.get("channel")
    if channel == None:
        log_print("channel name is not specified")
        sys.exit(1)
    
    sample_rate_s = as1_config.get("sample_rate")
    if sample_rate_s == None:
        log_print("sample rate is not specified")
        sys.exit(1)

    try:
        sample_rate = float(sample_rate_s)
    except ValueError:
        log_print("invalid sample rate")
        sys.exit(1)

    signal_offset_s = as1_config.get("signal_offset")
    if signal_offset_s == None:
        log_print("signal offset is not specified")
        sys.exit(1)

    try:
        signal_offset = int(signal_offset_s)
    except ValueError:
        log_print("invalid signal offset")
        sys.exit(1)

    time_offset_s = as1_config.get("time_offset")
    if time_offset_s == None:
        log_print("time offset is not specified")
        sys.exit(1)

    try:
        time_offset = int(time_offset_s)
    except ValueError:
        log_print("invalid time offset")
        sys.exit(1)

    max_time_error_s = as1_config.get("max_time_error")
    if max_time_error_s == None:
        log_print("maximum time error is not specified")
        sys.exit(1)

    try:
        max_time_error = int(max_time_error_s)
    except ValueError:
        log_print("invalid maximum time error")
        sys.exit(1)

process_options()

fd = open(port)

newattr = termios.tcgetattr(fd)

newattr[tty.ISPEED] = baud
newattr[tty.OSPEED] = baud

termios.tcsetattr(fd, termios.TCSANOW, newattr)
tty.setraw(fd)

buf = DataBuffer(station, channel, sample_rate, signal_offset,
  time_offset, max_time_error)

line = fd.readline(10)
while line != 0:
    try:
        buf.put_sample(int(line))
    except ValueError:
        log_print("corrupted data: " + line[:-1])

    line = fd.readline(10)
 
