# should reflect your home directory where DRM is installed
setenv DRM_HOME 	#home_drm#/operator	


# should reflect your home directory where Comserv is installed
setenv COMSERV_HOME 	#home_sysop#	

# the location where binary files are
setenv SEED_STUFF_BIN 	#home_drm#/bin

# the location where the SEED header data base files are
setenv SEED_STUFF_HOME  #home_sysop#/config

# your Comserv data location	 
setenv DATA_DIR 	#home_sysop#/data	

# your DRM tmp data location	 
setenv TMP_DIR 		#home_drm#/tmp 

# mail address of sysop
setenv SYS_MAIL_ADDR    #opmail#

setenv PATH ${SEED_STUFF_HOME}:${PATH}

