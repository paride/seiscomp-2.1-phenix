[cserv]

* Settings for the Comserv plugin

* Override network code
* network=GE

* Attach to stations
* stations=KRIS,EDD

* If timing quality is not available, use this value as default
* (-1 = disabled)
* default_timing_quality = -1

* The set of stations made available via Seedlink further depends on
* Seedlink settings in the 'seedlink.ini' file. This option is just for
* reducing system load if only a few of the many Comserv stations are made
* available via Seedlink. Default is to attach to all available stations.

* Unpack Mini-SEED streams SH{Z,N,E} and send into the Stream Processor
* as channels Z,N,E. Note that this setting applies to all stations that
* commerv_plugin is attached to. Use multiple comserv_plugin instances
* (with different stations= setting) if you want to use different unpack
* settings with different stations.

* unpack SHZ target_id=Z
* unpack SHN target_id=N
* unpack SHE target_id=E

* Triggered datastreams (applies to all stations that commerv_plugin is
* attached to)

* command_fifo = /home/sysop/cfifo

* trigger SHZ buffer_length=60 pre_seconds=20 post_seconds=20
* trigger SHN buffer_length=60 pre_seconds=20 post_seconds=20
* trigger SHE buffer_length=60 pre_seconds=20 post_seconds=20

