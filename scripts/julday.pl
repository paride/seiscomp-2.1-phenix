#!/usr/bin/perl

if (@ARGV[0] eq "-h" || @ARGV[0] eq "--help")
{
   print "Usage : julday [ mm dd [year]]\n";
   exit;
}

$date = qx(date);
@date = split(" ",$date);
$cyr = @date[5];

if($#ARGV == -1)
{
   $year = $cyr;
   $mon = @date[1];
   $day = &check_digit(2,@date[2]);
   $month = &num_month($mon);
   $month = &check_digit(2,$month);

}else{

   $month = @ARGV[0];
   $day = @ARGV[1];
   $year = @ARGV[2];
   if ($year == "") {$year = $cyr;}
}

$doy = &check_digit(3,&get_doy($year,$month,$day));
print"\n Calendar Date $month $day $year\n";
print" Julian Date $doy $year\n\n";

exit;

sub num_month {

local @Mon = ("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec");
local $mon = $_[0];
local $no = 0;
local $month;

while ($mon ne @Mon[$no]){$no++;}
$month = $no + 1;

return ($month);
}

sub get_doy {

local (@Mon = (0,31,28,31,30,31,30,31,31,30,31,30,31));
local($year = $_[0]);
local($mon = $_[1]);
local($day = $_[2]);
local($doy = 0);

$leap = int($year) - int(int(int($year) /4) *4);
if ($leap == 0) {$Mon[2] = 29};

$i = 0;
while ($i < $mon-1) {
        $i++;
        $doy = $doy + $Mon[$i];
        }
        $doy = $doy + $day;
return $doy;
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
