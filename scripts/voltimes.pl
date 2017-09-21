#!/usr/bin/perl

$bin = "$ENV{'SEED_STUFF_BIN'}";
if( ! -d "$bin" ) {
    printf STDERR "\nThe enviroment variable SEED_STUFF_BIN is not set or directory [%s] does not exist !\n",
    $bin;
    exit(1);
}
#$owner = "users";
$opt = $ARGV[0];
$data_dir = $ARGV[1];
$stat = $ARGV[2];
$chan_mask = $ARGV[3];
$type = $ARGV[4];
$start_time = $ARGV[5];
$end_time = $ARGV[6];
$net = $ARGV[7];
if($net eq "") {$net = "*";}

if($type eq "I")
{
   #print"type I\n";
   $status = qx(cd $data_dir/$stat; ls *$chan_mask);
   @liste = split(" ",$status);
   #print"@liste\n";

   if(index($status,$chan_mask) >= 0)
   {
      @channel[0] = $chan_mask;
      #print"single channel: @channel \n";
   }else{
      $i = -1;
      foreach $element (@liste)
      {
         $chan = substr($element,index($element,".")+1,3);
         $j = -1;
	 $found = 0;
	 while ($j < $i)
	 {
	    $j++;
	    if(@channel[$j] eq $chan)
	    {
	       $found = 1;
	    }
	 }
         if($found == 0)
         {
            $i++;
            @channel[$i] = $chan;
            #print"c: $i $element @channel[$i]\n";
         }
      }
   }

}elsif($type eq "II"){
   #print"type II $chan_mask\n";
   $act = 0;
   if(index($chan_mask,"active") >= 0)
   {
      @mask = split("/",$chan_mask);
      if($#mask > 0)
      {
         $chan_mask = @mask[0];
      }else{
         $chan_mask = "*";
      }
   $act = 1;
   #print"active channels only: $chan_mask \n";
   }

   if($act == 1)
   {
      $status = qx(cd $data_dir/$stat; ls $chan_mask*.D/active);
      #print"cd $data_dir/$stat; ls $chan_mask*.D/active\n";
      @liste = split(" ",$status);
   }else{
      $status = qx(cd $data_dir/$stat; ls $chan_mask*.D);
      #print"cd $data_dir/$stat; ls $chan_mask*.D\n";
      @liste1 = split(" ",$status);
      $i = -1;
      foreach $elem (@liste1)
      {
         if(index($elem,"active") < 0)
         {
            $i++;
            @liste[$i] = $elem;
         }
      }
   }
   #print"@liste\n";
   
   if(index($status,$chan_mask) >= 0)
   {
      @channel[0] = $chan_mask;
      if($act == 1) {@channel[0] = "@channel[0]/active"}
      #print"single channel: @channel \n";
   }else{
      $i = -1;
      foreach $element (@liste)
      {
         if(index($element,".D") > 0)
         {
            $ls = index($element,".D");
	    $chan_new = substr($element,$ls-3,3);
	    $new = 1;
	    foreach $elch (@channel)
	    {
	       if($elch eq $chan_new) {$new = 0;}
	    }
	    if($new == 1)
	    {
               $i++;
               @channel[$i] = $chan_new;
	       if($act == 1) {@channel[$i] = "@channel[$i]/active"}
               #print"$i $element @channel[$i]\n";
	    }
         }
      }
   }
}else{
   #print"type III\n";
   #$status = qx(cd $data_dir; ls */*/$stat);
   #@liste = split(" ",$status);
   #print"@liste\n";
   @channel = "*";
}


if($opt eq "-l")
{
   foreach $chan (@channel)
   {
      #print "$data_dir $stat $chan $start_time $end_time\n";
      &list;
   }
}elsif($opt eq "-c"){
   foreach $chan (@channel)
   {
      #print "$data_dir $stat $chan $start_time $end_time\n";
      &chart;
   }
}elsif($opt eq "-e"){
   foreach $chan (@channel)
   {
      #print "$data_dir $stat $chan $start_time $end_time\n";
      ($no, @liste) = &make_extr;
      $n = -1;
      while ($n < $no-1)
      {
         $n++;
         print "@liste[$n]\n";
      }
   }
}

exit;

sub chart
{
$st_year = substr($start_time,0,2);
$st_year = $st_year + 1900;
if($st_year < 1950) {$st_year = $st_year + 100;}
$st_mon = substr($start_time,2,2);
$st_day = substr($start_time,4,2);
$st_doy = &get_doy($st_year, $st_mon, $st_day);
$st_hour = substr($start_time,6,2);
$st_min = substr($start_time,8,2);
$st_sec = substr($start_time,10,2);

$en_year = substr($end_time,0,2);
$en_year = $en_year + 1900;
if($en_year < 1950) {$en_year = $en_year + 100;}
$en_mon = substr($end_time,2,2);
$en_day = substr($end_time,4,2);
$en_doy = &get_doy($en_year, $en_mon, $en_day);
$en_hour = substr($end_time,6,2);
$en_min = substr($end_time,8,2);
$en_sec = substr($end_time,10,2);

if($type eq "I")
{
   ($status, $no_fil, @file) = check_times_1 ($data_dir, $stat, $chan, $st_year, $st_doy, $en_year, $en_doy);
}elsif($type eq "II"){
   ($status, $no_fil, @file) = check_times_2 ($data_dir, $stat, $chan, $st_year, $st_doy, $en_year, $en_doy);
}else{
   ($status, $no_fil, @file) = check_times_3 ($data_dir, $stat, $chan, $st_year, $st_doy, $en_year, $en_doy);
}
#print("check_times: $status, $no_fil, @file, $first_year $last_year\n");

if($status < 1) {return;}

$no_years = $last_year - $first_year + 1;
$iy = 0;

while ($iy < $no_years)
{
   $cr_year = $first_year + $iy;
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
   if($type eq "I")
   {
      $sh_year = substr($cr_year,2,2);
      $gmask = "$stat$sh_year";
      $add = length($gmask);
   }elsif($type eq "II"){
      $gmask = ".$cr_year.";
      $add = 6;
   }else{
      $sh_year = substr($cr_year,2,2);
      $gmask = ".$sh_year.";
      $add = 4;
   }
   #print"gmask: $gmask $add\n";
   foreach $file (@file)
   {
      if(index($file,$gmask) > 0)
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
      while ($xdoy < $ydoy)
      {
	 $xdoy++;
         ($xday, $xmon) = &split_doy($cr_year, $xdoy);
	 #print"doy: $xday $xmon $xdoy $cr_doy $iof\n";
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
	    if($iof >= 0 && $xdoy > 0) {print" X";}
            $iof++;
	    if($iof <= $nof)
	    {
	       #print"$iof $file_year[$iof]\n";
	       if($type eq "I")
	       {
                  $cr_day = substr(@file_year[$iof],index(@file_year[$iof],$gmask)+$add+2,2);
                  $cr_mon = substr(@file_year[$iof],index(@file_year[$iof],$gmask)+$add,2);
                  $cr_doy = &get_doy($cr_year, $cr_mon, $cr_day);
	       }else{
                  $cr_doy = substr(@file_year[$iof],index(@file_year[$iof],$gmask)+$add,3);
                  ($cr_day, $cr_mon) = &split_doy($cr_year, $cr_doy);
	       }
               #print"@file_year[$iof] $cr_doy $cr_mon $cr_day $xdoy\n";
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

sub list
{
$st_year = substr($start_time,0,2);
$st_year = $st_year + 1900;
if($st_year < 1950) {$st_year = $st_year + 100;}
$st_mon = substr($start_time,2,2);
$st_day = substr($start_time,4,2);
$st_doy = &get_doy($st_year, $st_mon, $st_day);
$st_hour = substr($start_time,6,2);
$st_min = substr($start_time,8,2);
$st_sec = substr($start_time,10,2);

$en_year = substr($end_time,0,2);
$en_year = $en_year + 1900;
if($en_year < 1950) {$en_year = $en_year + 100;}
$en_mon = substr($end_time,2,2);
$en_day = substr($end_time,4,2);
$en_doy = &get_doy($en_year, $en_mon, $en_day);
$en_hour = substr($end_time,6,2);
$en_min = substr($end_time,8,2);
$en_sec = substr($end_time,10,2);

if($type eq "I")
{
   ($status, $no_fil, @file) = check_times_1 ($data_dir, $stat, $chan, $st_year, $st_doy, $en_year, $en_doy);
   $blk = 4096;
   $anf = 1;
}elsif($type eq "II"){
   ($status, $no_fil, @file) = check_times_2 ($data_dir, $stat, $chan, $st_year, $st_doy, $en_year, $en_doy);
   $blk = 512;
   $anf = 2;
}else{
   exit;
}
#print("check_times: $status, $no_fil, @file\n");

if($status < 1) {return;}

$no = -1;
foreach $file (@file)
 {
   $no++;
   $status = qx(ls -l $data_dir/$file);
   @liste = split(" ",$status);
   #print"ls: @liste\n";
   $rec = @liste[4]/$blk;
   #print"record @liste[4] $rec\n";
   
   #$i = -1;
   #foreach $element (@liste)
   #{
   #   $i++;
   #  if($element eq $owner)
   #   {
   #      $rec = @liste[$i+1]/$blk;
   #	 print"record @liste[$i+1] $rec\n";
   #   }
   #}
   
   $status = qx($bin/check_seed $data_dir/$file -B $blk -a $anf);
   #print "$status\n";
   @liste = split(" ", $status);
   $i = -1;
   foreach $element (@liste)
   {
      $i++;
      if($element eq "to")
      {
         @st[$no] = "@liste[$i-2] @liste[$i-1]";
     }
   }
   $status = qx($bin/check_seed $data_dir/$file -B $blk -a $rec);
   #print "$status\n";
   @liste = split(" ", $status);
   $i = -1;
   foreach $element (@liste)
   {
      $i++;
      if($element eq "to")
      {
         if(index($file,"active") >= 0)
	 {
            @en[$no] = "@liste[$i+1] @liste[$i+2]*";
	    if(index($chan,"active") >=0)
	    {
	       $ll = index($chan,"active") - 1;
	       $chan = substr($chan,0,$ll);
	    }
	 }else{
            @en[$no] = "@liste[$i+1] @liste[$i+2]";
	 }
	 #print"File $file: $stat   $chan  @st[$no]  -  @en[$no]\n";
      }
   }
 }

$i = -1;
$start = @st[0];
while ($i < $no)
 {
   $i++;
   #print"$i @st[$i+1] @en[$i]\n";
   $en_rnd = &rnd_tim(@en[$i]);
   if($i < $no) {$st_rnd = &rnd_tim(@st[$i+1]);}
   if(($st_rnd - $en_rnd) != 0.0 || $i == $no)
   {
      $end = @en[$i];
      print" $stat   $chan  $start  -  $end\n";
      $start = @st[$i+1];
   }
 }
return;
}

sub make_extr
{
local $no = 0;
local @liste = " ";

$st_year = substr($start_time,0,2);
$st_year = $st_year + 1900;
if($st_year < 1950) {$st_year = $st_year + 100;}
$st_mon = substr($start_time,2,2);
$st_day = substr($start_time,4,2);
$st_doy = &get_doy($st_year, $st_mon, $st_day);
$st_hour = substr($start_time,6,2);
$st_min = substr($start_time,8,2);
$st_sec = substr($start_time,10,2);

$en_year = substr($end_time,0,2);
$en_year = $en_year + 1900;
if($en_year < 1950) {$en_year = $en_year + 100;}
$en_mon = substr($end_time,2,2);
$en_day = substr($end_time,4,2);
$en_doy = &get_doy($en_year, $en_mon, $en_day);
$en_hour = substr($end_time,6,2);
$en_min = substr($end_time,8,2);
$en_sec = substr($end_time,10,2);

$start_day = substr($start_time,0,6);
$start_tim = substr($start_time,6,6);
$start = "$start_day\_$start_tim";
$end_day = substr($end_time,0,6);
$end_tim = substr($end_time,6,6);
$end = "$end_day\_$end_tim";

if($type eq "I")
{
   ($status, $no_fil, @file) = check_times_1 ($data_dir, $stat, $chan, $st_year, $st_doy, $en_year, $en_doy);
   $add = " ";
}else{
   ($status, $no_fil, @file) = check_times_2 ($data_dir, $stat, $chan, $st_year, $st_doy, $en_year, $en_doy);
   $add = "-d CS";
}
#print("check_times: $status, $no_fil, @file\n");

if($status < 1) {return;}

$no = -1;
foreach $file (@file)
 {
   $no++;
   @liste[$no] = "extr_file $data_dir/$file -b $start -e $end $add";
 }
$no = $no + 1;

return ($no, @liste);
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

#print"dir: $dir $stat $chan\n";
@list = qx {cd $dir; ls $stat/*$chan 2>&1};
chomp @list;
#print"liste in : @list\n";
@list = &sort_list(@list);
#print"liste out : @list\n";

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
   $doy = &get_doy($year, $mon, $day);
   #print"$year $doy $mon $day\n";
   $ftime = &get_abstim($year, $doy, 0, 0, 0);
   #print"check: $ftime $st_time $en_time\n";
   if($ftime >= $st_time && $ftime < $en_time)
   {
      $nofil++;
      $status = 1;
      @file[$nofil-1] = $gfile;
      if($nofil == 1) {$first_year = $year;}
      if($nofil == $#list+1) {$last_year = $year;}
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
   @list = qx {cd $dir; ls $stat/$chan.D/*$chan* 2>&1};
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
      $year = substr($efile,index($efile,"$chan.D")+6,4);
      $doy = substr($efile,index($efile,"$chan.D")+11,3);

   }else{

      $st = qx($bin/check_seed $data_dir/$gfile -B 512 -a 2 2>&1);
      #print "$st\n";
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
      #print"$tim_str\n";
      $year = &check_digit(2,substr($tim_str,6,4));
      $mon = &check_digit(2,substr($tim_str,3,2));
      $day = &check_digit(2,substr($tim_str,0,2));
      $doy = &get_doy($year, $mon, $day);
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
      @file[$nofil-1] = $gfile;
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

#print"dir: $dir $stat $chan\n";
@list = qx {cd $dir; ls */$net/$stat/*$stat* 2>&1};
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
   #$year = substr($efile,index($efile,"$stat")+$lenst+4,4);
   #$doy = substr($efile,index($efile,"$stat")+$lenst+9,3);
   $year = substr($efile,index($efile,"$stat")+$lenst+4,2);
   $doy = substr($efile,index($efile,"$stat")+$lenst+7,3);
   if($year > 50)
   {
      $year = 1900 + $year;
   }else{
      $year = 2000 + $year;
   }
   $doy = substr($efile,index($efile,"$stat")+$lenst+7,3);
   $ftime = &get_abstim($year, $doy, 0, 0, 0);
   if($ftime >= $st_time && $ftime < $en_time)
   {
      $nofil++;
      $status = 1;
      @file[$nofil-1] = $gfile;
      if($nofil == 1) {$first_year = $year;}
      if($nofil == $#list+1) {$last_year = $year;}
   }
}

return ($status, $nofil, @file);
}

sub check_digit {

local $ls = $_[0];
local $string = $_[1];

if(index($string,".") > 0) {$string = int($string);}

        while( length($string) < $ls) {
	        $string = "0$string";
        }
        return($string);
}

sub get_doy {

local @Mon = (0,31,28,31,30,31,30,31,31,30,31,30,31);
local $year = $_[0];
local $mon = $_[1];
local $day = $_[2];
local $doy;
local $i;

#print "get_doy: $year $mon $day\n";

$leap = int($year) - int(int(int($year) /4) *4);
if ($leap == 0) {$Mon[2] = 29};

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

sub rnd_tim {

local $tim_str = $_[0];
local $year;
local $mon;
local $day;
local $doy;
local $hour;
local $min;
local $sec;
local $tsec;

#print "$tim_str\n";
$year = &check_digit(2,substr($tim_str,6,4));
$mon = &check_digit(2,substr($tim_str,3,2));
$day = &check_digit(2,substr($tim_str,0,2));
$doy = &get_doy($year, $mon, $day);
$hour = &check_digit(2,substr($tim_str,11,2));
$min = &check_digit(2,substr($tim_str,14,2));
$sec = &check_digit(2,substr($tim_str,17,2));
$tsec = &check_digit(2,substr($tim_str,20,4));
$sec = $sec + $tsec*0.0001;
#print"$year $mon $day $doy $hour $min $sec $tsec\n";

$time = &get_abstim($year, $doy, $hour, $min, $sec);
#print"rndtime $time\n";

return $time;
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

	print "Time error: $year $doy $tsec\n";

	}else {

	$time = ($year - 1970)*31536000+(($year-1969)/4)*86400;
	$time = $time+($doy-1)*86400+$tsec;
	}
return $time;
}

sub sort_list {

local @list = @_;
local @list_sort;
local $status = 0;

$ls = index(@list[0],"\/$stat") + length($stat) + 1;
$i = -1;
foreach $elem (@list)
{
   $year = substr($elem,$ls,2);
   #print"$elem $year\n";
   if($year >= 70)
   {
      $i++;
      @list_sort[$i] = $elem;
      #print"i: $i @list_sort[$i]\n";
   }
}
foreach $elem (@list)
{
   $year = substr($elem,$ls,2);
   #print"$elem $year\n";
   if($year <= 38)
   {
      $i++;
      @list_sort[$i] = $elem;
      #print"i: $i @list_sort[$i]\n";
   }
}
#print"@list_sort\n";

return @list_sort;

}
