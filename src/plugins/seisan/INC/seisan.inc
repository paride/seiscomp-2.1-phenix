c
c    Include file for SEISAN system
c
c  19.06.98  lo     : included merge_wav, and when was map variables put in ???
c  sep 16, 98 by jh : -------- version 7.0 check --------------------------
c  nov 26  98 jh    : add variables n_wav_dirs,wav_dirs
c  sep 20           : add geo_depth1, geo_depth2
c  sep 23           : add herkij_distance
c  feb           lo : add keep_auto
c  feb 28  2000     : add copy_wav_dir
c  mar 10        jh : add text_print
c  april 11         : fix common boundaries 
c  oct 20           : merge_wav to 5 chars
c  oct 24           : add high_accuracy
c  july 1        jh : common block boundaries screwed up again !!!!
c  oct 11 2001   lo : add cursortype
c  april 29 2002 bjb: add hypo71_offset parameter
c  may 13 2002   lo : add confirmation_level
c  may 27 2002   lo : add reg_auto_process
c  apr 10 2003   lo : add myxfont
c
      integer n_wav_dirs         ! number of directories to search
c
      character*80 wav_dirs(100) ! waveform directories to search
      character*80 myxfont       ! name of x font
c
c   waveform directories
c
      integer n_wav_bases        ! number of waveform bases under WAV

      character*5 wav_base(100)  ! waveform base name
      character*80 full_path_wav_name  ! full path waveform file name
      character*5 merge_wav      ! code for waveform file when merging 
                                 ! from mulplt

      real map_lat,map_lon       ! lat and long for starting epimap
      character*10 map_file      ! name of epimap map file
      character*4 map_stations   ! print stations in epimap
      integer map_proj           ! number of projection
      real confirmation_level    ! 0 if skip confirmation

c  spectral
 
      real geo_depth1,geo_depth2 ! geometrical spreading depths
      real herkij_distance       ! herrnmann kijko geo spreading distance
c
      real keep_auto             ! keep auto phases when register is 1.
      character*5 copy_wav_dir   ! copy to this data base in mulplt 
      character*80 text_print    ! print command for a text file
      logical high_accuracy      ! if true, use high accuracy

c
c variables defined for working with continuos data
c

c-- maximum number of databases
      integer max_cont
      parameter(max_cont=50)
      integer max_cont_comp
      parameter(max_cont_comp=150)
      character*5 cont_base(max_cont)    ! name of cont databases
      character*5 cont_net_code          ! name of cont database for combined waveform file
      character*4 cont_comp(max_cont_comp)  ! component

      integer n_cont_base                ! number of ------
      integer n_cont_comp                ! number of components
      character*4 cont_operator          ! name of operator 
      real cont_before                   ! beginning of time interval for
      real cont_after                    ! end ----- reading waveform headers
      real cont_interval                 ! time interval to be plotted in sec 
      real cont_interval_org             ! original ------ 
      real cont_int_s                    ! small  ------
     
      integer cursortype                 ! choice of cursor

      real reg_autoprocess_flag          ! 0,1,2
      character*10 reg_autoprocess_name  ! name of process to run when register

c parameter to offset station longitudes for events close to
c the Greenwich meridien
      real hypo71_offset
c maximum solutions in focmec
      integer focmec_maxsol


C *********   KEEP PARAMETER TOGETHER WITH SAME BYTE LENGHT **************


C  TEXT
      common/seisanch/    
     &                   wav_dirs,
     &                   full_path_wav_name,
     &                   text_print,myxfont,
     &                   reg_autoprocess_name,
     &                   cont_operator,cont_comp

C  4 BYTES
      common/seisan4/    
     &                   cont_before,cont_after,
     &                   keep_auto,
     &                   cont_interval,cont_interval_org,
     &                   cont_int_s,
     &                   n_cont_base,n_wav_bases,n_wav_dirs,
     &                   n_cont_comp,
     &                   cursortype,confirmation_level,focmec_maxsol,
     &                   reg_autoprocess_flag
   
c  5 bytes

      common/seisan5/ merge_wav,copy_wav_dir,cont_base,cont_net_code,
     &                wav_base

c  4 bytes

      common/epimap4/ map_lat,map_lon,map_stations,map_proj

c  10 bytes

      common/epimap10/ map_file
c
c  4 bytes

      common/allspec/ geo_depth1,geo_depth2,herkij_distance 
c
c  1 byte

      common/seisan1/ high_accuracy

      common/hypo71/ hypo71_offset

