*/3 * * * * #home_sysop#/bin/seiscomp_ctrl check > /dev/null 2>&1
55 23 * * * #home_sysop#/bin/backup_seqfiles > /dev/null 2>&1
#DLOG#10 3 * * * #home_sysop#/bin/check_disksize > /dev/null 2>&1
20 3 * * * #home_sysop#/bin/purge_datafiles > /dev/null 2>&1
#PLOT#30 3 * * * #home_sysop#/bin/purge_plotfiles > /dev/null 2>&1
#PLOT#*/10 * * * * #home_sysop#/bin/update_plotfiles > /dev/null 2>&1
#LINKS#10 5 * * * #home_sysop#/bin/update_datalinks > /dev/null 2>&1
#DRM#0 1 * * * #home_drm#/operator/drm_cleantmp > /dev/null 2>&1 
#AUTOBOOT#10,40 * * * * grfsoft/prog/autoboot.csh >>/data/log/autoboot.log 2>&1
#CDREC#39 2,22 * * * grfsoft/prog/process_dayfiles.csh yesterday #home_sysop#/grfsoft/prog/globaldefs.csh >>/data/log/cronlog.log 2>&1
#CDREC#01 04 * * * grfsoft/prog/make_sfdlist.csh > /dev/null 2>&1
