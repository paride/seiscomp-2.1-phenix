#!/usr/bin/perl

$db = "nrt";

if($#ARGV == -1)
{
   print"Usage: times.pl <mode> <stat> <net> <start time> <end time> <chan>\n
   mode: -c - normal chart, -cc - color chart, -l - file list\n";
   exit;
}

$prtype = @ARGV[0];
if($prtype eq "" || $prtype eq "-c") {$prtype = "chart";}
if($prtype eq "-cc") {$prtype = "color";}
if($prtype eq "-l") {$prtype = "list";}

$stat = @ARGV[1];
$net = @ARGV[2];
$chan = @ARGV[5];
if($chan eq "") {$chan = "BHZ";}

$start_time = @ARGV[3];
if($start_time eq "")
{
   $st_year = 1990;
   $st_doy = 1;
}else{
   $st_year = substr($start_time,0,2);
   if($st_year < 50)
   {
      $st_year = $st_year + 2000;
   }else{
      $st_year = $st_year + 1900;
   }
   $st_mon = substr($start_time,2,2);
   $st_day = substr($start_time,4,2);
   $st_doy = &get_doy($st_year, $st_mon, $st_day);
}
$end_time = @ARGV[4];
if($end_time eq "")
{
   $en_year = 2038;
   $en_doy = 1;
}else{
   $en_year = substr($end_time,0,2);
   if($en_year < 50)
   {
      $en_year = $en_year + 2000;
   }else{
      $en_year = $en_year + 1900;
   }
   $en_mon = substr($end_time,2,2);
   $en_day = substr($end_time,4,2);
   $en_doy = &get_doy($en_year, $en_mon, $en_day);
}
$fyear = 3000;

$all = 0;
$nodb = 1;
if($db eq "")
{
   $all = 1;
   $nodb = 3;
}
$nrt = "nrt";
$dcp = "dcp";
$archive = "archive";
if($net eq "") {$net = "GE";}

$ind = 0;
$nol = -1;

while ($ind < $nodb)
{
   $ind++;
   if($all == 1)
   {
      if($ind == 1) {$db = "$nrt";}
      if($ind == 2) {$db = "$dcp";}
      if($ind == 3) {$db = "$archive";}
      #print"All db mode, current db: $db\n";
   }

   if($db eq "$archive")
   {
      $data_dir = "/$archive";
   }else{
      $data_dir = "/$db";
   }

   $st = qx(cd $data_dir 2>&1);
   $ls = index($st,"No such");
   if(index($st,"No such") >= 0)
   {
      print"-----\n";
      print"Data directory $data_dir not available: try again later\n";
      print"-----\n";
      exit;
   }

   $filelist = "file_$db";
   ($ok, $nofil, @$filelist) = &check_times($db, $data_dir, $stat, $net, $chan, $st_year, $st_doy, $en_year, $en_doy );

   if($ok > 0)
   {
      if($first_year < $fyear) {$fyear = $first_year}
      if($last_year > $lyear) {$lyear = $last_year}
      #print"$first_year $last_year $fyear $lyear\n";
      foreach $elem (@$filelist)
      {
	 $lelem = "$elem\_$db";
	 if($lelem ne @list[$nol])
	 {
            $nol++;
	    @list[$nol] = $lelem;
	 }
      }
      #print"check_times: $ok $nofil $fyear $lyear @$filelist\n";
   }else{
      #print"check_times error: $ok\n";
   }
}

#print"$#list ; @list\n";
@list = sort (@list);
$nolist = $#list + 1;
if($prtype eq "list") {print"$nolist @list\n";}
if($prtype eq "chart" || $prtype eq "color") {&chart;}

exit;

sub chart
{

$no_years = $lyear - $fyear + 1;
$iy = 0;

while ($iy < $no_years)
{
   $cr_year = $fyear + $iy;
   $ydoy = 365;
   $yleap = int($cr_year) - int(int(int($cr_year) /4) *4);
   if ($yleap == 0) {$ydoy = 366};
   #print"$cr_year $ydoy\n";
   
   $iy++;
   if($chan eq "*")
   {
      print "\n                       Summary for station $stat in year $cr_year\n\n";
   }else{
      if(index($chan,"active") >=0)
      {
         $ll = index($chan,"active") - 1;
         $chan = substr($chan,0,$ll);
      }
      print "\n            Summary for station $stat and channel $chan in year $cr_year\n\n";
   }
   print "            0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3\n";
   print "            1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1\n";
   $nof = -1;

   $gmask = substr($cr_year,0,4);
   $add = 5;
   foreach $file (@list)
   {
      #print"$file $gmask\n";
      if(index($file,$gmask) >= 0)
      {
         $nof++;
         @file_year[$nof] = $file;
	 #print"$cr_year: $nof $file\n";
      }
   }
   
   $iof = -1;
   $cr_doy = 0;
   while ($iof < $nof)
   {
      
      $xdoy = -1;
      #$xdoy = 0;
      while ($xdoy < $ydoy)
      {
	 $xdoy++;
	 #print"split:$xdoy,$ydoy,$cr_year\n";
         ($xday, $xmon) = &split_doy($cr_year, $xdoy);
	 #print"doy: $xday $xmon $xdoy $cr_doy $iof $cr_type\n";
	 if($xday == 1)
	 {
	    if($xmon == 1) {print"\n   JAN     ";}
	    if($xmon == 2) {print"\n   FEB     ";}
	    if($xmon == 3) {print"\n   MAR     ";}
	    if($xmon == 4) {print"\n   APR     ";}
	    if($xmon == 5) {print"\n   MAY     ";}
	    if($xmon == 6) {print"\n   JUN     ";}
	    if($xmon == 7) {print"\n   JUL     ";}
	    if($xmon == 8) {print"\n   AUG     ";}
	    if($xmon == 9) {print"\n   SEP     ";}
	    if($xmon == 10) {print"\n   OCT     ";}
	    if($xmon == 11) {print"\n   NOV     ";}
	    if($xmon == 12) {print"\n   DEC     ";}
	 }
	 
	 if($xdoy < $cr_doy)
	 {
	    print" -";
	 }else{
	    if($iof >= 0 && $xdoy > 0)
	    {
	       if($prtype eq "chart")
	       {
	          if($cr_type eq "nrt") {print" N";}
	          if($cr_type eq "dcp") {print" D";}
	          if($cr_type eq "arc") {print" A";}
	          if($cr_type eq "arcdcp") {print" B";}
	          if($cr_type eq "arcnrt") {print" X";}
	          if($cr_type eq "dcpnrt") {print" Y";}
	          if($cr_type eq "arcdcpnrt") {print" Z";}
	       }else{
	          if($cr_type eq "nrt") {print"<font color=#FF0000> X</font>";}
	          if($cr_type eq "dcp") {print"<font color=#009900> X</font>";}
	          if($cr_type eq "arc") {print" X";}
	          if($cr_type eq "arcdcp") {print"<font color=#3366FF> X</font>";}
	          if($cr_type eq "arcnrt") {print"<font color=#3366FF> Y</font>";}
	          if($cr_type eq "dcpnrt") {print"<font color=#CC66CC> X</font>";}
	          if($cr_type eq "arcdcpnrt") {print"<font color=#3366FF> Z</font>";}
	       }
	    }
            $iof++;
	    if($iof <= $nof)
	    {
	       #print"$iof $file_year[$iof]\n";
               $cr_doy = substr(@file_year[$iof],$add,3);
	       $cr_type = substr(@file_year[$iof],$add+4,3);
               $cr_doy_n = substr(@file_year[$iof+1],$add,3);
	       $cr_type_n = substr(@file_year[$iof+1],$add+4,3);
               #print"loop: @file_year[$iof] $cr_doy $cr_type $cr_doy_n $cr_type_n $xdoy\n";
	       if($cr_doy == $cr_doy_n)
	       {
	          $cr_type = "$cr_type$cr_type_n";
		  #print"$cr_type\n";
		  $iof++;
                  $cr_doy_n = substr(@file_year[$iof+1],$add,3);
	          $cr_type_n = substr(@file_year[$iof+1],$add+4,3);
	          if($cr_doy == $cr_doy_n)
	          {
	             $cr_type = "$cr_type$cr_type_n";
		     #print"$cr_type\n";
		     $iof++;
 	          }
	       }
	    }else{
	       $cr_doy = 1000;
	    }
	 }
      }
   }
   print"\n\n";
}

return;
}

sub fyear {

local ($year = $_[0]);

if ($year > 1900) {return $year;}
if ($year > 70) {
        $year = $year + 1900;
        }
if ($year < 20) {
        $year = $year + 2000;
        }
return $year;
}

sub get_doy {

local (@Mon = (0,31,28,31,30,31,30,31,31,30,31,30,31));
local($year = $_[0]);
local($mon = $_[1]);
local($day = $_[2]);
local($doy = 0);

#print "get_doy: $year $mon $day\n";

$leap = int($year) - int(int(int($year) /4) *4);
if ($leap == 0) {$Mon[2] = 29};
#print"get_doy: $year $leap $Mon[2]\n";

$i = 0;
while ($i < $mon-1) {
        $i++;
        $doy = $doy + $Mon[$i];
        #print "$i $Mon[$i] $doy\n";
        }
        $doy = $doy + $day;
        #print "$doy\n";
return $doy;
}

sub split_doy {

local (@Mon = (0,31,28,31,30,31,30,31,31,30,31,30,31));
local ($year = $_[0]);
local ($idoy = $_[1]);
local ($mon = 0);
local ($day = 0);
local ($doy = 0);
local $i;

#print "split_doy: $year $idoy\n";

$leap = int($year) - int(int(int($year) /4) *4);
if ($leap == 0) {$Mon[2] = 29};
#print"get_doy: $year $leap $Mon[2]\n";

$i = 0;
while ($doy < $idoy) {
        $i++;
        $doy = $doy + $Mon[$i];
        #print "$i $Mon[$i] $doy\n";
        }
$mon = $i;
$day = $idoy - $doy + $Mon[$i];
#print "$day $mon\n";

return ($day, $mon);
}

sub get_abstim {

local($year = $_[0]);
local($doy = $_[1]);
local($hour = $_[2]);
local($min = $_[3]);
local($sec = $_[4]);
local($tsec = 0);
local($time = 0);

#print "get_abstim: $year $doy $hour $min $sec\n";

$tsec = $hour *3600 + $min *60 + $sec;
if($year < 1970 || $year > 2500 || $doy < 1 || $doy > 366 ||
        $tsec < 0 || $tsec > 86400) {

        print "Time error: $year $doy $tsec";

        }else {

        $time = ($year - 1970)*31536000+(($year-1969)/4)*86400;
        $time = $time+($doy-1)*86400+$tsec;
        }
return $time;
}

sub split_abstim
{
local ($time = $_[0]);
local $year;
local $doy;
local $hour;
local $min;
local $sec;
local $secyr;
local $secdy;

$year = int($time/31536000+1970);
$secyr = ($year-1970)*31536000+(($year-1969)/4)*86400;

while ($secyr > $time)
{
   $secyr = ($year-1970)*31536000+(($year-1969)/4)*86400;
   $year = $year - 1;
}
$doy = int(($time - $secyr)/86400 + 1);
$secdy = ($doy-1)*86400;
$hour = int(($time - $secyr - $secdy)/3600);
$min = int(($time - $secyr - $secdy - $hour*3600)/60);
$sec = int($time - $secyr - $secdy - $hour*3600 - $min*60);

#print"split_abstim: $year $doy $hour $min $sec\n";

return ($year, $doy, $hour, $min, $sec);
}

sub check_digit {

local $ls = $_[0];
local $string = $_[1];

if(index($string,".") >= 0) {$string = int($string);}
        while( length($string) < $ls) {
                $string = "0$string";
        }
        return($string);
}

sub check_times
#
#   -1 - unitree not available; 0 - file not found, 1 - file found;
#
{
local $db = $_[0];
local $data_dir = $_[1];
local $stat = $_[2];
local $net = $_[3];
local $chan = $_[4];
local $st_year = $_[5];
local $st_doy = $_[6];
local $en_year = $_[7];
local $en_doy = $_[8];
local $status = 0;
local $nofil = 0;
local @file, $type;

if(substr($chan,2,1) eq "?")
{
   $chanx = substr($chan,0,2);
   $chanx = "$chanx\Z";
}else{
   $chanx = $chan;
}
#print"chan $chanx\n";

if($db eq "$nrt")
{
   $type="II";
   #$st = qx(cat $config/station_$stat 2>&1);
   #if(index($st,"No such") >= 0)
   #{
   #return;
   #}
   #chomp $st;
   #if($st eq "")
   #{
   #   $type = "II";
   #}else{
   #   $type ="I";
   #}
}elsif($db eq "$dcp")
{
   $type = "I";
}else
{
   $type = "III";
}

#print"type: $db $stat $type\n";

if($type eq "I")
{
   ($status, $no_fil, @file) = check_times_1 ($data_dir, $stat, $chanx, $st_year, $st_doy, $en_year, $en_doy);
}elsif($type eq "II"){
   ($status, $no_fil, @file) = check_times_2 ($data_dir, $stat, $chanx, $st_year, $st_doy, $en_year, $en_doy);
}else{
   ($status, $no_fil, @file) = check_times_3 ($data_dir, $stat, $chanx, $st_year, $st_doy, $en_year, $en_doy);
}
#print("check_times: $status, $no_fil, @file, $first_year $last_year\n");

return ($status, $nofil, @file);
}

sub check_times_1
#
#   -1 - directory not available; 0 - file not found, 1 - file found;
#
{
local $dir = $_[0];
local $stat = $_[1];
local $chan = $_[2];
local $st_year = $_[3];
local $st_doy = $_[4];
local $en_year = $_[5];
local $en_doy = $_[6];
local $year, $mon, $day, $doy;
local $status = 0;
local $nofil = 0;
local $ndoy = 0;
local @list = " ";
local @file = " ";

$first_year = 3000;
$last_year = 0;

#print"dir: $dir $stat $chan\n";
@list = qx {cd $dir; ls $stat/*$chan 2>&1};
chomp @list;
#print"liste: @list\n";

if(index(@list[0],"No such") >= 0)
{
   $status = 0;
   $no_fil = 0;
   return ($status, $no_fil, @file);
}

if ($st_year < 1900) {$st_year = $st_year + 1900}
if ($st_year < 1950 ) {$st_year = $st_year + 100}
if ($en_year < 1900) {$en_year = $en_year + 1900}
if ($en_year < 1950 ) {$en_year = $en_year + 100}

$st_time = &get_abstim($st_year, $st_doy, 0, 0, 0);
$en_time = &get_abstim($en_year, $en_doy, 23, 59, 59);
#print"start-end: $st_time $en_time\n";

$lenst = length($stat);
$i = -1;
foreach $gfile (@list)
{
   @st = split("/",$gfile);
   $efile = @st[$#st];
   $year = substr($efile,index($efile,"$stat")+$lenst,2);
   if ($year < 1900) {$year = $year + 1900}
   if ($year < 1950 ) {$year = $year + 100}
   $mon = substr($efile,index($efile,"$stat")+$lenst+2,2);
   $day = substr($efile,index($efile,"$stat")+$lenst+4,2);
   $doy = &check_digit(3,&get_doy($year, $mon, $day));
   #print"$year $doy $mon $day\n";
   $ftime = &get_abstim($year, $doy, 0, 0, 0);
   if($ftime >= $st_time && $ftime < $en_time)
   {
      $nofil++;
      $status = 1;
      #@file[$nofil-1] = $gfile;
      @file[$nofil-1] = "$year/$doy";
      if($year < $first_year) {$first_year = $year;}
      if($year > $last_year) {$last_year = $year;}
   }
}

return ($status, $nofil, @file);
}

sub check_times_2
#
#   -1 - directory not available; 0 - file not found, 1 - file found;
#
{
local $dir = $_[0];
local $stat = $_[1];
local $chan = $_[2];
local $st_year = $_[3];
local $st_doy = $_[4];
local $en_year = $_[5];
local $en_doy = $_[6];
local $year, $mon, $day, $doy;
local $status = 0;
local $nofil = 0;
local $ndoy = 0;
local @list = " ";
local @file = " ";

#print"dir: $dir $stat $chan\n";
if(index($chan,"active") > 0)
{
   $act = 1;
   $chan = (split("/",$chan))[0];
   @list = qx {cd $dir; ls $stat/$chan.D/active 2>&1};
}else{
   $act = 0;
   @list = qx {cd $dir; ls $stat/$chan.D/* 2>&1};
}
chomp @list;
#print"liste: @list, act: $act\n";

if(index(@list[0],"No such") >= 0)
{
   $status = 0;
   $no_fil = 0;
   return ($status, $no_fil, @file);
}

if ($st_year < 1900) {$st_year = $st_year + 1900}
if ($st_year < 1950 ) {$st_year = $st_year + 100}
if ($en_year < 1900) {$en_year = $en_year + 1900}
if ($en_year < 1950 ) {$en_year = $en_year + 100}

$st_time = &get_abstim($st_year, $st_doy, 0, 0, 0);
$en_time = &get_abstim($en_year, $en_doy, 23, 59, 59);
#print"start-end: $st_time $en_time\n";

$lenst = length($stat);
$i = -1;
foreach $gfile (@list)
{
   if(index($gfile,"active") < 0)
   {
      @st = split("/",$gfile);
      $efile = @st[$#st];
      $ls = index($efile,"$chan.D");
      $year = substr($efile,index($efile,"$chan.D")+6,4);
      $doy = substr($efile,index($efile,"$chan.D")+11,3);
      #print"$efile $year $doy\n";

   }else{

      $st = qx(check_seed $data_dir/$gfile -B 512 -a 2 2>&1);
      #print "$st\n";
      if(index($st,"not found") > 0)
      {
         print"$st";
	 exit;
      }
      @liste = split(" ", $st);
      $i = -1;
      foreach $element (@liste)
      {
         $i++;
         if($element eq "to")
         {
            $tim_str = "@liste[$i-2] @liste[$i-1]";
         }
      }
      print"tim_str: $tim_str\n";
      $year = &check_digit(2,substr($tim_str,6,4));
      $mon = &check_digit(2,substr($tim_str,3,2));
      $day = &check_digit(2,substr($tim_str,0,2));
      $doy = &check_digit(3,&get_doy($year, $mon, $day));
      if($opt eq "-c")
      {
         $gfile = "$dir/$stat/$chan.D/$stat.GE.$chan.D.$year.$doy.0000";
      }
   }
   #print"$year $doy\n";
   $ftime = &get_abstim($year, $doy, 0, 0, 0);
   if($ftime >= $st_time && $ftime < $en_time)
   {
      $nofil++;
      $status = 1;
      #@file[$nofil-1] = $gfile;
      @file[$nofil-1] = "$year/$doy";
      if($nofil == 1) {$first_year = $year;}
      if($nofil == $#list+1) {$last_year = $year;}
   }
}

return ($status, $nofil, @file);
}

sub check_times_3
#
#   -1 - directory not available; 0 - file not found, 1 - file found;
#
{
local $dir = $_[0];
local $stat = $_[1];
local $chan = $_[2];
local $st_year = $_[3];
local $st_doy = $_[4];
local $en_year = $_[5];
local $en_doy = $_[6];
local $year, $mon, $day, $doy;
local $status = 0;
local $nofil = 0;
local $ndoy = 0;
local @list = " ";
local @file = " ";

$first_year = 3000;
$last_year = 0;

#print"dir: $dir $stat $chan\n";
@list = qx {cd $dir; ls */$net/$stat/* 2>&1};
chomp @list;
#print"liste: @list\n";

if(index(@list[0],"No such") >= 0)
{
   $status = 0;
   $no_fil = 0;
   return ($status, $no_fil, @file);
}

if ($st_year < 1900) {$st_year = $st_year + 1900}
if ($st_year < 1950 ) {$st_year = $st_year + 100}
if ($en_year < 1900) {$en_year = $en_year + 1900}
if ($en_year < 1950 ) {$en_year = $en_year + 100}

$st_time = &get_abstim($st_year, $st_doy, 0, 0, 0);
$en_time = &get_abstim($en_year, $en_doy, 23, 59, 59);
#print"start-end: $st_time $en_time\n";

$lenst = length($stat);
$i = -1;
foreach $gfile (@list)
{
   @st = split("/",$gfile);
   $efile = @st[$#st];
   $year = substr($efile,index($efile,"$stat")+$lenst+4,2);
   $doy = substr($efile,index($efile,"$stat")+$lenst+7,3);
   $year = $year + 1900;
   if ($year < 1950 ) {$year = $year + 100;}
   $ftime = &get_abstim($year, $doy, 0, 0, 0);
   #print"$efile $year $doy $ftime $st_time $en_time\n";
   if($ftime >= $st_time && $ftime < $en_time)
   {
      #print"$doy\n";
      $nofil++;
      $status = 1;
      #@file[$nofil-1] = $gfile;
      @file[$nofil-1] = "$year/$doy";
      if($year < $first_year) {$first_year = $year;}
      if($year > $last_year) {$last_year = $year;}
   }
}

return ($status, $nofil, @file);
}
