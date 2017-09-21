c
c  include for handling waveform files
c
c  updates
c
c  april 7 2000 jh.: add several
c  may  11 2000 lo : add wav_resp_file
c  feb  19 2001 lo : common block signal renamed to signalx, since signal 
c                    used in Linux system routines, Redhat 7.0
c  mar  21 2001 lo : add wav_out_stat, wav_out_comp
c  july 13      lo : add wav_resp_filename
c  oct 5        jh : add wav_out_cbyte,wav_out_abs_time,cwav block
c  nov 11          : add wav_sav...
c  may 7 2003   lo : add wav_interactive
c
 
      character*80	wav_error_message	   ! error message of operation     
      integer 		wav_nfiles                 ! number of waveform files
      character*80      wav_header_text(200)       ! info text in wav file
      character*80      wav_filename(200)          ! waveform file name
      character*10	wav_file_format(200)       ! format of wav file
      integer 		wav_year(max_trace)        ! year
      integer           wav_month(max_trace)       ! month
      integer           wav_day(max_trace)         ! day
      integer           wav_hour(max_trace)        ! hour
      integer           wav_min(max_trace)         ! minute
      real              wav_sec(max_trace)         ! second
      double precision  wav_abs_time(max_trace)    ! abs time since 1900
      character*5       wav_stat(max_trace)        ! station
      character*4       wav_comp(max_trace)        ! component
      integer           wav_nsamp(max_trace)       ! number of samples
      real              wav_rate(max_trace)        ! sample rate
      integer           wav_nchan                  ! total number of channels
      real              wav_duration(max_trace)    ! channel duration in secs
      integer           wav_chan_nr_file(max_trace)! channel number in file 
      integer           wav_file_nr_chan(max_trace)! file number for channel
      character*1       wav_cbyte(max_trace)       ! 2 or 4 byte integer
      real		wav_total_time 		   ! total time all channels
      integer           wav_first		   ! index of ealiest trace
      integer           wav_last                   ! --------- last ending --
      integer           wav_current_chan(3)        ! last channel read 1:z, 2:n 3:e
      real 		wav_delay(max_trace)       ! delay each ch. rel. main
      character*80	wav_time_error(max_trace)  ! timing error,char 1 E
                                                   ! rest not defined
c
      real 		signal1(max_sample)        ! all samples from z   trace
      real 		signal2(max_sample)        ! all samples from ns  trace
      real 		signal3(max_sample)        ! all samples from ew  trace
c
      real  		wav_y1(max_sample)         ! work arrays
      real		wav_y2(max_sample)
      real 		wav_y3(max_sample)
cfix   next should be equivalenced to something
c     real		wav_y3comp(max_sample/3,3)
      real		wav_y3comp(max_sample,3)  ! changed lo
c
c   for output selection
c
      integer		wav_out_nchan		   ! number of chan in subset
      real		wav_out_total_time         ! total time all out channels
      character*5       wav_out_stat(max_trace)    ! station
      character*4       wav_out_comp(max_trace)    ! component
      integer		wav_out_chan(max_trace)	   ! select channel numbers
      real		wav_out_start(max_trace)   ! start of trace rel. first
      real		wav_out_duration(max_trace)! duration of selection
      integer		wav_out_nsamp(max_trace)   ! number of samples out
      real		wav_out_rate(max_trace)    ! sample rate out
      integer		wav_out_year(max_trace)    ! year of start
      integer		wav_out_month(max_trace)   ! month of start
      integer		wav_out_day(max_trace)     ! day of start
      integer		wav_out_hour(max_trace)    ! hour of start
      integer		wav_out_min(max_trace)     ! min of start
      real 		wav_out_sec(max_trace)     ! sec of start
      double precision  wav_out_abs_time(max_trace)! abs time
      integer		wav_out_status(max_trace)  ! status of selection
      integer		wav_out_first_sample(max_trace) ! first sample nr sel.
      character*1       wav_out_cbyte(max_trace)   ! 2 or 4 bytes
      character*80      wav_out_header_text(200)   ! info text in wav file
      real              wav_out_delay(max_trace)   ! delay rel main header
      integer           wav_out_first              ! index of first trace
      integer           wav_out_last               ! index of last trace

      integer           wav_nsamp_req(max_trace)   ! number of samples in required time interval

c
c   for saving parameters
c
      integer           wav_sav_nchan              ! number of chan in subset
      real              wav_sav_total_time         ! total time all out channels
      character*5       wav_sav_stat(max_trace)    ! station
      character*4       wav_sav_comp(max_trace)    ! component
      integer           wav_sav_chan(max_trace)    ! select channel numbers
      real              wav_sav_start(max_trace)   ! start of trace rel. first
      real              wav_sav_duration(max_trace)! duration of selection
      integer           wav_sav_nsamp(max_trace)   ! number of samples out
      real              wav_sav_rate(max_trace)    ! sample rate out
      integer           wav_sav_year(max_trace)    ! year of start
      integer           wav_sav_month(max_trace)   ! month of start
      integer           wav_sav_day(max_trace)     ! day of start
      integer           wav_sav_hour(max_trace)    ! hour of start
      integer           wav_sav_min(max_trace)     ! min of start
      real              wav_sav_sec(max_trace)     ! sec of start
      double precision  wav_sav_abs_time(max_trace)! abs time
      integer           wav_sav_status(max_trace)  ! status of selection
      integer           wav_sav_first_sample(max_trace) ! first sample nr sel.
      character*1       wav_sav_cbyte(max_trace)   ! 2 or 4 bytes
      character*80      wav_sav_header_text(200)   ! info text in wav file
      real              wav_sav_delay(max_trace)   ! delay rel main header
      integer           wav_sav_first              ! index of first trace
      integer           wav_sav_last               ! index of last trace


c
c  response related
c
      character*5       wav_resp_stat              ! station code
      character*4       wav_resp_comp              ! component code
      character*80	wav_resp_status            ! char 1, seisan status 
      character*80      wav_resp_file              ! response file name
      character*80      wav_resp_filename          ! response file name
      character*80      wav_resp_type              ! GSE or SEISAN for type
      character*80      wav_resp_action            ! char 1, seisan action
      character*1040    wav_resp_seisan_chead      ! seisan channel head
      integer		wav_resp_year              ! year of response file
      integer 		wav_resp_month             ! month of response file
      integer		wav_resp_day               ! day of repsponse file
      
c
      character*1	wav_rot_comp(max_trace)    ! indicate if rotated,T or R
      real		wav_rot_delay(max_trace)   ! delay due to rotation
c
c  cwav block
c

      logical           cwav                       ! true if continuous data

      integer           max_ctrace                 ! max number of traces
      parameter         (max_ctrace=50)
      integer           max_cseg                   ! maximum number of segments for each trace
      parameter         (max_cseg=300)
      integer           n_cont_trace               ! number of cont traces

c
      character*80      cwav_filename(max_ctrace,max_cseg)    ! waveform file name
      character*10      cwav_file_format(max_ctrace,max_cseg) ! format of wav file
      double precision  cwav_abs_time(max_ctrace,max_cseg)    ! abs time since 1900
      integer           cwav_nsamp(max_ctrace,max_cseg)       ! number of samples
      integer           cwav_year(max_ctrace,max_cseg)        ! year
      integer           cwav_month(max_ctrace,max_cseg)       ! month
      integer           cwav_day(max_ctrace,max_cseg)         ! day
      integer           cwav_hour(max_ctrace,max_cseg)        ! hour
      integer           cwav_min(max_ctrace,max_cseg)         ! min
      real              cwav_sec(max_ctrace,max_cseg)         ! sec
      real              cwav_rate(max_ctrace,max_cseg)        ! sample rate
      real              cwav_duration(max_ctrace,max_cseg)    ! channel duration in secs
      integer           cwav_chan_nr_file(max_ctrace,max_cseg)! channel number in file 
      character*80      cwav_time_error(max_ctrace,max_cseg)  ! timing error
      character*5       cwav_stat(max_ctrace)                 ! station
      character*1       cwav_cbyte(max_ctrace)                ! number of bytes
      character*4       cwav_comp(max_ctrace)                 ! component
      integer           cwav_nseg(max_ctrace)                 ! number of segments
      character*14      cwav_start_time                       ! start time for data set yyyymmddhhmmss
      character*14      cwav_data_start_time                  ! start to find fist s-file
      character*14      cwav_end_time                         ! end time for data set, yyyymmddhhmmss
      double precision  cwav_abs_start_time                   ! abs time cwav_start_time
      double precision  cwav_abs_end_time                     ! abs time for cwav_end_time

      logical wav_interactive                                 ! interactive mode

      common /waveform2/ wav_nfiles,wav_filename,wav_file_format,
     *                  wav_year,wav_month,wav_day,wav_hour,wav_min,
     *                  wav_sec,wav_stat,wav_out_stat,wav_sav_stat,
     *                  wav_comp,wav_out_comp,wav_nsamp,wav_rate,
     *                  wav_nsamp_req,wav_sav_comp,
     *                  wav_file_nr_chan,wav_nchan,wav_duration,
     *                  wav_chan_nr_file,wav_cbyte,wav_first, 
     *                  wav_last,wav_total_time,wav_out_total_time,
     *                  wav_delay,wav_rot_delay,wav_sav_total_time,
     *                  wav_error_message,wav_header_text,
     *                  wav_current_chan,
     *                  wav_out_nchan,wav_out_chan,wav_out_start,
     *                  wav_out_duration,wav_out_year,wav_out_month,
     *                  wav_out_day,wav_out_hour,wav_out_min,
     *                  wav_out_sec,wav_out_status,
     *                  wav_out_first_sample,wav_out_nsamp,
     *                  wav_out_rate,wav_out_header_text,
     *                  wav_sav_nchan,wav_sav_chan,wav_sav_start,
     *                  wav_sav_duration,wav_sav_year,wav_sav_month,
     *                  wav_sav_day,wav_sav_hour,wav_sav_min,
     *                  wav_sav_sec,wav_sav_status,
     *                  wav_sav_first_sample,wav_sav_nsamp,
     *                  wav_sav_rate,wav_sav_header_text,
     *                  wav_y1,wav_y2,wav_y3,wav_y3comp,wav_time_error,
     *                  wav_resp_comp,wav_resp_file,wav_resp_filename,
     *                  wav_resp_status,wav_resp_action,wav_resp_type,
     *                  wav_resp_year,wav_resp_month,wav_resp_day,
     *                  wav_resp_seisan_chead,wav_out_first,
     *                  wav_sav_first,
     *                  wav_out_last,wav_out_delay,
     *                  wav_sav_last,wav_sav_delay,
     *                  wav_interactive
c
      common /waveform4/ wav_abs_time
      common /waveform5/ wav_resp_stat
      common /waveform8/ wav_out_abs_time, wav_sav_abs_time
c
      common /waveform1/ wav_rot_comp,wav_out_cbyte,wav_sav_cbyte
c
      common /signalx/signal1
      common /signal1/signal2,signal3
c
c  cwAV BLOCK

      common /cwav1/    cwav,cwav_cbyte
      common /cwav4/    cwav_abs_time,
     *                  cwav_rate,
     *                  cwav_duration,
     *                  n_cont_trace,
     *                  cwav_nseg,
     *                  cwav_chan_nr_file,cwav_nsamp,
     *                  cwav_time_error,
     *                  cwav_filename,
     *                  cwav_comp,
     *                  cwav_year,cwav_month,cwav_day,
     *                  cwav_hour,cwav_min,cwav_sec

      common /cwav5/    cwav_stat
      common /cwav8/    cwav_abs_start_time,cwav_abs_end_time
      common /cwav10/   cwav_file_format

      common /cwav14/   cwav_start_time,cwav_end_time,
     *                  cwav_data_start_time


