[guralp_#statid#]

* Settings for Guralp DM24 unit

* Station ID (network/station code is set in seedlink.ini)
station=#statid#

* Use the command 'serial_plugin -m' to find out which protocols are
* supported.
protocol=guralp

* Serial port name, such as /dev/ttyS0
port=#comport#

* Baud rate
bps=#baudrate#

* Least significant bit (relative to 32-bit samples), normally 8 for 24-bit
* samples. Setting lsb to less than 8 does not have any effect with Guralp
* DM24 module.
lsb=8

* The amount of microseconds to be added to the time reported by the
* digitizer.
time_offset=0

* Maximum number of consecutive zeros in datastream before data gap will be
* declared (-1 = disabled).
zero_sample_limit = -1

* Keyword 'channel' is used to map input channels to symbolic channel
* names. Use the 'gcfutil' program to find out the source IDs of your
* digitizer. Channel names are arbitrary 1..10-letter identifiers which
* should match the input names of the stream processing scheme in
* streams.xml, which is referenced from seedlink.ini

channel Z source_id=3704Z4
channel N source_id=3704N4
channel E source_id=3704E4

