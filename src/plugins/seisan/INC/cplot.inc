c
c  include for plotting continuous data
c
c  updates
c

      integer max_cstat             ! max number of stations
      parameter (max_cstat=50)
      integer max_cnsamp             ! maximum number of samples per trace 
      parameter (max_cnsamp=300000)  ! changed from 402000

c
c header info
c
      integer    cwav_nsamp_nogap(max_ctrace,max_cseg) ! number of samples with no gap
      integer    cwav_start_samp_nr(max_ctrace,max_cseg) ! first sample # in seg of full
      integer    last_seg_nr(max_ctrace)               ! last segment to be read
      integer    last_wav_nsamp(max_ctrace)            ! last sample to be read
      integer    cwav_seg_start(max_ctrace)            ! number segment to start with if
		               			       ! data allready in commonblock
      integer    cwav_seg_end(max_ctrace)              ! number segment to end with if
						       ! data allready in commonblock
      logical    cwav_flag(max_ctrace)                 ! false if no data
      logical    new_memory(max_ctrace)                ! true if rereading memory
      logical    head_memory(max_ctrace,max_cseg)  ! true if waveform data and header info in memory 
							! false if wav data not in memory
      logical         wav_memory(max_ctrace,max_cseg)   ! true if waveform data in memory 
c
c waveform data
c
      real            cwav_signal(max_ctrace,max_cnsamp)      ! data array
      real            cwav_full_signal(max_ctrace,max_cnsamp) ! data array
c
c time data
c
      character*14      data_start_time(max_cstat)    ! start time of data
      character*14      data_end_time(max_cstat)      ! end time of data
      double precision  wave_start_org                ! start time before zooming 
      double precision  wave_end_org                  ! end time before zooming 
c
c canvas size
c
      integer cplot_xmax,cplot_ymax,cplot_xmax_org, cplot_ymax_org
      character*14 start_time,end_time
      double precision wave_start,wave_end   ! required time


      common /cont1/
     *                  last_seg_nr,last_wav_nsamp,
     *                  cwav_start_samp_nr,
     *                  cwav_seg_start,cwav_seg_end,
     *                  cwav_nsamp_nogap,
     *                  head_memory,wav_memory,new_memory,
     *                  cwav_flag

      common /cont2/    cwav_full_signal,cwav_signal

      common /cont3/    cplot_xmax,cplot_ymax,
     *			cplot_xmax_org,cplot_ymax_org

      common /time1/ 
     *                  wave_start,wave_end,
     *                  start_time,end_time

      common /time2/   data_start_time,data_end_time,
     *			wave_start_org,wave_end_org

