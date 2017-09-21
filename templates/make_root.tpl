#/bin/sh

SYSOP_HOME=#home_sysop#
DRMHOME=#home_drm#

PORTLIST=$(for p in `cat $SYSOP_HOME/key/key_* | egrep '(COM|TERM)PORT'`; do
   eval echo $p | cut -d "=" -f 2 | sed -e '/^ *none *$/d'
done)

echo "SeisComP serial port list:" $PORTLIST
echo ""

for PORT in $PORTLIST; do
   echo "set root permission for port $PORT ..."
   echo chmod 666 $PORT
   chmod 666 $PORT
   echo ""
done
echo "...port setting done"
echo ""

echo "set root permission for directories..."
echo chmod 755 $SYSOP_HOME
chmod 755 $SYSOP_HOME
echo chmod 755 $DRMHOME
chmod 755 $DRMHOME
echo chmod 777 /var/spool/uucp
chmod 777 /var/spool/uucp
echo "...done"
echo ""

echo "set root links..."

ln -f -s $SYSOP_HOME/config/stations.ini /etc/stations.ini
ln -f -s $SYSOP_HOME/config/network.ini /etc/network.ini
ln -f -s $SYSOP_HOME/bin/leapseconds /usr/local/lib/leapseconds
rm -f /nrt
ln -f -s $SYSOP_HOME/data /nrt
echo "...done"
echo ""

if cat $SYSOP_HOME/key/key_* | grep -q "PLUGIN='m24'"; then
   echo "make $SYSOP_HOME/bin/m24-plug suid root..."
   chown root $SYSOP_HOME/bin/m24-plug
   chmod u+s $SYSOP_HOME/bin/m24-plug
   echo "...done"
   echo ""
fi

