#!/usr/bin/perl

$sysop = "#home_sysop#";
$datadir = "$sysop/data";
$cfgdir = "$sysop/config";

@statlist = qx(for f in $cfgdir/rc_*; do echo \${f##*_}; done);

foreach $stat (@statlist)
{
   chomp $stat;
   $statdir = "$datadir/$stat";
   @chanlist = qx(cd $statdir; ls | grep -v station 2>&1);
   #print"$stat @chanlist\n";
   
   foreach $chanx (@chanlist)
   {
      chomp $chanx;
      $chandir = "$statdir/$chanx";
      $chanx =~ tr/./#/;
      $chan = (split("#",$chanx))[0];
      $max_size = &get_size ($statdir, $chan);
      #print"chan: $chan $chandir $max_size\n";
      $exit = 0;

      while ($exit == 0)
      {
         @list = qx(cd $chandir; ls -l 2>&1);

         $tot_size = 0;
         $no = -1;
         foreach $file (@list)
         {
            if(index($file,"total") < 0)
            {
               $no++;
               $size = (split(" ",$file))[4];
               @name[$no] = (split(" ",$file))[8];
               $tot_size = $tot_size + $size;
               #print "@name[$no] $size $tot_size\n";
            }
         }

         print "Total size for channel $chan: $tot_size; max size: $max_size\n";

         if ( $tot_size > $max_size)
         {
            print "Size too large for channel $chan - delete @name[0]\n";
            qx(cd $chandir; /bin/rm @name[0]);
         }else{
            $exit = 1;
         }
      }
   }
}

exit;

sub get_size
{
local $datadir = $_[0];
local $chan = $_[1];
local $defsize, $maxsize;

$defsize = qx(cat $datadir/station.ini | grep defdisksize 2>&1);
$defsize = (split("=",$defsize))[1];

$chanx = substr($chan,0,2);
$disksize = qx(cat $datadir/station.ini | grep disksize | grep $chanx | grep -v *disk 2>&1);
if($disksize ne "")
{
   $maxsize = (split(",",$disksize))[1];
   #print "disksize for channel $chanx $maxsize";
}else{
   $maxsize = $defsize;
   #print "defdisksize $chanx $maxsize";
}
$maxsize = $maxsize * 1000000;
#print"$chan $maxsize $size\n";

return ($maxsize)
}
