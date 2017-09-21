#!/bin/sh

echo ... Removing temporary directories \(older then 1 day\) ...
find    #home_drm#/tmp           \
        -depth             \
        -type d                 \
        -name 'tmp.*' \
        -user drm          \
        -mtime +1               \
        -exec /bin/rm -r {} \;
exit
