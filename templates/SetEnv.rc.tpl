# should reflect your home directory where DRM is installed
DRM_HOME=#home_drm#/operator	
export DRM_HOME

# should reflect your home directory where SeisComP is installed
COMSERV_HOME=#home_sysop#
export COMSERV_HOME

# the location where binary files are
SEED_STUFF_BIN=#home_drm#/bin
export SEED_STUFF_BIN

# the location where the SEED header data base files are
SEED_STUFF_HOME=#home_sysop#/config
export SEED_STUFF_HOME

# your Comserv data location	 
DATA_DIR=#home_sysop#/data	
export DATA_DIR

# your DRM tmp data location	 
TMP_DIR=#home_drm#/tmp
export TMP_DIR

# mail address of sysop
SYS_MAIL_ADDR=#opmail#
export SYS_MAIL_ADDR

PATH=$SEED_STUFF_HOME:$PATH

