[hrd24_#statid#]

* Settings for the Nanometrics HRD24 digitizer

* Station ID (network/station code is set in seedlink.ini)
station=#statid#

* Use the command 'serial_plugin -m' to find out which protocols are
* supported.
protocol=hrd24

* Port can be a serial port or a pipe. 
port=#comport#

* Baud rate is only meaningful for a serial port.
bps=#baudrate#

* Number of "bundles" in one packet
bundles=59

* Maximum number of consecutive zeros in datastream before data gap will be
* declared (-1 = disabled).
zero_sample_limit = -1

* Keyword 'channel' is used to map input channels to symbolic channel
* names. Channel names are arbitrary 1..10-letter identifiers which should
* match the input names of the stream processing scheme in streams.xml,
* which is referenced from seedlink.ini

channel Z source_id=0
channel N source_id=1
channel E source_id=2

