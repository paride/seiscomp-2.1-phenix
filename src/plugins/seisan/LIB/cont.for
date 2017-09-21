c*********************************************************************
c    
c subroutines to read continuous data from database
c
c  nov. 2000 - may 2001
c  Written by Lars Ottemoeller and Susanne Lund Jensen
c
c*********************************************************************

      subroutine continuous_init
      implicit none

      include 'continuous.inc'
      integer i

      n_cont_trace = 0
      do i=1,max_ctrace  !max_ctrace def in continuous.inc
        cwav_nseg(i)=0
        last_seg_nr(i)=0
        cwav_flag(i)=.false.
      enddo

      return
      end

c --------------------------------------------------
c                                                                               
c   Subroutine to read header information from databases and 
c      append to continuous common block 
c
c     subroutine read_cont_header(cbase,start_time,end_time)
      subroutine read_cont_header(cbase)
      implicit none                                                 
c    
c    input: cbase      - name of continuous data base
c           start_time - expanded start time  
c           end_time   - expanded end time  
c
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'mulplt.inc' 
       include 'libsei.inc'                ! Open file definitions
       include 'continuous.inc'
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C
      character*80  file_out       !complete wav name
      integer       nstat,nphase   !See routine: 'indata'
      character*1   type,exp       !See routine: 'indata
      integer       id             !id line number
      integer       n_wav_file     !number wavform files in s-file
      integer       read1          !read unit 1
      logical       flag,b_flag    !flag
c     character*14  start_time,end_time  !time interval chosen
      character*40  base_name      !data base name
      character*5   cbase          !data base name
      integer       from_eev !indicate to routine findevin that call is from eev
      character*80  evfile         !event file name       
c      integer       event_no       !event number         
      integer       status         !status of event search               
      integer       code           !returned code 
      integer       new_month      !new month indicator              
      integer       fstart         !see base               
      integer       k,i,j          !counter
      integer       ns(max_ctrace) !sample counter
      character*10  key            !key in findevin
      integer       ind            !index
 
c
c   get seisan and cplot defaults
c
      call get_seisan_def
      call get_cplot_def

      base_name=' '
      base_name(1:5)=cbase
      key= ' '
      
10    continue

c     do ind=1,4
c       write(*,*)'Data start time c',data_start_time(ind)
c       write(*,*)'Data end time   c',data_end_time(ind)
c     enddo

c
c find s-file name, go through database
c
      call findevin
     &   (base_name,start_time,end_time,key,from_eev,event_no,
     &   evfile,fstart,new_month,status)
      write(*,*)'sfile ', evfile
c
c return at end of time period
c
      if (status.eq.3) goto 20

c
c init wave structure
c
        call wav_init

c
c open and read s-file
c
       call sei open( old$,                   ! Open old file.
     &                  ' ',                  ! No prompt.
     &                  evfile,               ! This file.
     &                  read1,                ! On this unit.
     &                  b_flag,               ! Existance?.
     &                  code )                ! Condition (n/a).
       call indata(read1,nstat,nphase,nhead,nrecord,type,exp,data,id)
       call sei close(close$,read1,code)    ! Close (stop on error).
c
c get waveform headers
c
c    -- Find waveform file (wav_filename) in sfile (data)
          call auto_tr(data,nhead,n_wav_file,wav_filename) 

	  do k=1,n_wav_file !there can be more than one wavform file in a s-file 
	     call get_full_wav_name(wav_filename(k),file_out)
	     wav_filename(k)=file_out
             call read_wav_header(k)
c
c copy waveform header information to continuous common block of all traces
c
             do j=1,wav_nchan
c
c check which trace, new if not found
c
               flag=.false.
               do i=1,n_cont_trace
                 if (cwav_stat(i).eq.wav_stat(j).and.
     &               cwav_comp(i).eq.wav_comp(j)) then

                    flag=.true.
                    ind=i
                   
                 endif
               enddo
	       if(wav_comp(j)(3:3).ne.' ') then
		  wav_comp(j)(4:4)=wav_comp(j)(3:3)
		  wav_comp(j)(3:3)=' '
               endif
               if (.not.flag.and.n_cont_trace.lt.max_ctrace) then !max_ctrace def in continuous.inc
                 n_cont_trace=n_cont_trace+1
                 cwav_stat(i)=wav_stat(j)
                 cwav_comp(i)=wav_comp(j)
                 ind=n_cont_trace
                 write(*,*) n_cont_trace,'  ',cwav_stat(i),cwav_comp(i)
               endif
c
c check if number of maximum traces exceeded 
c
               if (n_cont_trace.ge.max_ctrace)  
     &            write(*,*)' number of maximum traces exceeded'
c
c check if memory full
c
	       new_memory(ind)=.false.  ! memory not full
               if (cwav_nseg(ind).ge.max_cseg)
     &          write(*,*)'*** ', wav_stat(j),wav_comp(j),
     &                ': Number of maximum waveform files exceeded ***'
               if (ns(ind)+wav_nsamp(j).ge.max_cnsamp)  
     &          write(*,*)'*** ', wav_stat(j),wav_comp(j),
     &                ': Number of maximum samples exceeded ***'
               ns(ind)=ns(ind)+wav_nsamp(j)
c
c store information in continuous block
c
               cwav_nseg(ind)=cwav_nseg(ind)+1
	       head_memory(ind,cwav_nseg(ind))=.false.
               cwav_filename(ind,cwav_nseg(ind))=wav_filename(k)
               cwav_file_format(ind,cwav_nseg(ind))=wav_file_format(j)
               cwav_abs_time(ind,cwav_nseg(ind))=wav_abs_time(j)
               cwav_nsamp(ind,cwav_nseg(ind))=wav_nsamp(j)
               cwav_rate(ind,cwav_nseg(ind))=wav_rate(j)
               cwav_duration(ind,cwav_nseg(ind))=wav_duration(j)
               cwav_chan_nr_file(ind,cwav_nseg(ind))=wav_chan_nr_file(j)
               cwav_time_error(ind,cwav_nseg(ind))=wav_time_error(j)
             enddo
c          write(*,*)'wave file: ', cwav_filename(ind,cwav_nseg(ind))
          enddo
c
c next event file
c
      key='next'
      goto 10

20    continue

      return
      end

c ---------------------------------------------------------------
c
c routine to extract data if end of time interval not in commomblock
c
      subroutine no_end_data

      implicit none
C
C    Seisan library inserts and routines...
C    ======================================
C
      include 'mulplt.inc'
      include 'libsei.inc'                ! Open file definitions
      include 'seisan.inc'
      include 'continuous.inc'
C
C    ============= end of list ==========
C
      integer base_cnt                     ! data base counter
      double precision s_t,e_t             ! help time 
      character*14 ss_t,ee_t               ! help time 
      integer year,month,day,doy,hour,min  ! date and time
      real sec                             ! --------
      logical flag                         ! flag
      character*5 bc                       ! help base
      integer id                           ! trace id
      integer i                            ! counter
c      double precision wave_start,wave_end ! required time 
c      character*14 start_time,end_time     ! extended time yyyymmddhhmmss
c      common /time_1/ start_time,end_time,wave_start,wave_end



c
c extended start and end time for next database serach
c starting at the end of the last file, which depends on 
c the database
c
      do base_cnt=1,n_cont_base               !loop over databases
         flag=.false.
	 bc='     '
	 bc(1:3)=cont_base(base_cnt)
	 do id=1, n_cont_trace
 	   if (.not.flag.and.(bc.eq.cwav_stat(id))) then !new station
             write(*,*)'cont_base and trace ',cont_base(base_cnt),id
c
c   start time	     
c
             s_t=cwav_abs_time(id,cwav_nseg(id)) +  
     &		  cwav_duration(id,cwav_nseg(id))
             call sectim(s_t,year,doy,month,day,hour,min,sec) 
             write(ss_t(1:14), '(i4,5(i2))') year,month,day,hour,min
             do i=1,14                ! fill up blancs with 0
               if(ss_t(i:i).eq.' ') ss_t(i:i)='0'
             enddo
             data_start_time(base_cnt)=ss_t !store time in commonblock
c            write(*,*)'Data start time ',data_start_time(base_cnt)
c
c   end time
c
             e_t=s_t+cont_before*60+cont_interval+cont_after*60
             call sectim(e_t,year,doy,month,day,hour,min,sec)    
             write(ee_t(1:14), '(i4,5(i2))') year,month,day,hour,min
             do i=1,14                ! fill up blancs with 0
               if(ee_t(i:i).eq.' ') ee_t(i:i)='0'
             enddo
             data_end_time(base_cnt)=ee_t !store time in commonblock
c            write(*,*)'Data end time   ',data_end_time(base_cnt)
 	     flag=.true.
           endif
         enddo
      enddo
c
c
c extended start and end time for next database serach
c starting at the previous end time 
c
c     do base_cnt=1,n_cont_base               !loop over databases
c        flag=.false.
c        bc='     '
c        bc(1:3)=cont_base(base_cnt)
c        write(*,*)'BBBB  bc ',bc
c        do id=1, n_cont_trace
c          write(*,*)'BBBB  cwav_stat(id)',cwav_stat(id)
c	   if (.not.flag.and.(bc.eq.cwav_stat(id))) then !new station
c	    flag=.true.
c           write(*,*)'cont_base and trace ',cont_base(base_cnt),id
c           endif
c        enddo
c     enddo
c
c store time in commonblock
c
c     do base_cnt=1,n_cont_base               !loop over databases
c       data_start_time(base_cnt)=start_time
c       write(*,*)'TTTTTTTTT  start  time',data_start_time(base_cnt)
c       read(start_time,'(i4,4(i2))') year,month,day,hour,min
c       sec=0.
c       call timsec(year,month,day,hour,min,sec,msec) ! starttime in sec
c       e_t=msec+cont_before*60+cont_interval+ ! end time
c    &		      cont_after*60
c       call sectim(e_t,year,doy,month,day,hour,min,sec)    
c       write(end_time(1:14), '(i4,5(i2))') year,month,day,hour,min
c       do i=1,14                ! fill up blancs with 0
c         if(end_time(i:i).eq.' ') end_time(i:i)='0'
c       enddo
c       data_end_time(base_cnt)=end_time
c       write(*,*)'TTTTTTTTT  end time   ',data_end_time(base_cnt)
c     enddo
c     do i=1,n_cont_trace
c       write(*,*)'abs time ', cwav_abs_time(i,cwav_nseg(i))
c     enddo
c
c
c
c drawing the data
c
      call read_plot_bases

      return
      end



c ---------------------------------------------------------------
c
c routine to determine if data is allready in commonblock else
c new waveform file is read
c
      subroutine wav_data_next

      implicit none
C
C    Seisan library inserts and routines...
C    ======================================
C
      include 'mulplt.inc'
      include 'libsei.inc'                ! Open file definitions
      include 'continuous.inc'
C
C    ============= end of list ==========
C
c      double precision wave_start,wave_end ! required time 
c      character*14 start_time,end_time     ! extended time yyyymmddhhmmss
      character*10 interval                ! filter interval
      integer id                           ! trace id
      integer j                            ! counter
      character*1 data_info(max_ctrace)    ! data information
      integer deci                         ! decimation factor
      logical fflag                        ! filter flag
      logical read_more_end_wav            ! flag: no end data
      logical read_more_end_head           ! flag: no end data
      logical read_no_more                 ! flag: data allready in memory
      logical seg_start_ok,wav_seg_end_ok  ! flag
      logical head_seg_end_ok              ! flag
      integer ierror                       ! error code, 0 is ok
c      common /time_1/ start_time,end_time,wave_start,wave_end

      wav_nchan=0
      do id=1,n_cont_trace
        cwav_seg_start(id)=0  
      enddo
      do id=1,n_cont_trace
        seg_start_ok=.false.         ! true if data in memory
        wav_seg_end_ok=.false.       ! true if data in memory
        head_seg_end_ok=.false.      ! true if data in memory

        do j=1,cwav_nseg(id)
c
c checking if start of data allready in database
c
          if(.not.seg_start_ok.and.
     & 	      cwav_abs_time(id,j).lt.wave_start.
     &	       and.wave_start.lt.cwav_abs_time(id,j)+
     &         cwav_duration(id,j)) then
c	    write(*,*)'====== Start data in memory ======'
            cwav_seg_start(id)=j  
	    seg_start_ok=.true.
          endif
c
c checking if end of data allready in waveform commonblock
c
          if(.not.wav_seg_end_ok.and.head_memory(id,j).and.   
     & 	      wave_end.lt.cwav_abs_time(id,j)+cwav_duration(id,j)) then
c	    write(*,*)'====== End data in waveform memory ======'
            cwav_seg_end(id)=j  
	    wav_seg_end_ok=.true.
          endif
c
c checking if end of data allready in header commonblock
c
          if(.not.head_seg_end_ok.and..not.head_memory(id,j).and.   
     & 	      wave_end.lt.cwav_abs_time(id,j)+cwav_duration(id,j)) then
 	    write(*,*)'=== End data in header memory ==='
            cwav_seg_end(id)=j  
	    head_seg_end_ok=.true.
          endif
        enddo
c
c data is in the waveform commonblock
c
        if(seg_start_ok.and.wav_seg_end_ok) then
	  write(*,*)'=== ',cwav_stat(id),cwav_comp(id),
     &		    ': Data allready in memory ==='
	  data_info(id)='0'
c
c end data is not in the waveform commonblock
c
	elseif(.not.wav_seg_end_ok.and.seg_start_ok) then
	  write(*,*)'=== ',cwav_stat(id),cwav_comp(id),
     &		    ': End data not in waveform memory ==='
	  data_info(id)='1'
c
c end data is not in the header commonblock
c
	elseif(.not.head_seg_end_ok.and.seg_start_ok) then
	  write(*,*)'=== ',cwav_stat(id),cwav_comp(id),
     &		    ': End data not in header memory ==='
	  data_info(id)='2'
c
c start data is not in the commonblock
c
	elseif(.not.seg_start_ok.and.wav_seg_end_ok) then
	  write(*,*)'=== ',cwav_stat(id),cwav_comp(id),
     &		    ': Start data not in memory ==='
	endif
      enddo
c
c  what to do next
c
      read_no_more=.false.
      read_more_end_wav=.false.
      read_more_end_head=.false.
      do id=1,n_cont_trace
        if (data_info(id).eq.'0') read_no_more=.true.       !data allready in memory
	if (data_info(id).eq.'1') read_more_end_wav=.true.  !end data not in wav memory
 	if (data_info(id).eq.'2') read_more_end_head=.true. !end data not in head memory
      enddo
c
c  Data in header memory and waveform memory
c
      if (.not.read_more_end_wav.and.read_no_more) then 
        do id=1,n_cont_trace
	  wav_nchan=wav_nchan+1
          call wav_read_req_signal(id,wave_start,wave_end,deci,
     &                               fflag,ierror)
        enddo
        call xclear                         ! clear display
        call cplot_multi_trace(interval)              ! plot trace
      endif	 
c
c  End data in header memory but not in waveform memory
c
      if(read_more_end_head) then
	 call read_plot_traces
      endif
c
c  End data in not header memory and not in waveform memory
c
      if(read_more_end_wav) then
	 call no_end_data
      endif

      return
      end


c ---------------------------------------------------------------
c
c routine to read signal from continuous database
c
      subroutine wav_read_signal(id,chan_id,deci,fflag,ierror) 
      implicit none
c
c  input:   - id            trace id
c           - chan_id       channel id
c           - deci          decimation factor
c           - fflag         filter flag
c           - ierror        error code, 0 is ok
C
C    Seisan library inserts and routines...
C    ======================================
C
      include 'mulplt.inc'
      include 'libsei.inc'                ! Open file definitions
      include 'continuous.inc'
C
C    ============= end of list ==========
C
      integer id,chan_id                  ! trace and channel id
c      double precision wave_start,wave_end ! required time 
c      character*14 start_time,end_time     ! extended time yyyymmddhhmmss
      integer deci                        ! decimation factor
      logical fflag                       ! filter flag
      integer ierror                      ! error code, 0 is ok
      integer seg_end                     ! segment number where max samples exceeded
      integer first_wav_segment           ! first segment where head_memory is true
      integer i,j,l,counter               ! counter
      logical new_seg                     ! flag for new segment 
      logical new_trace                   ! flag for new trace
      logical flag                        ! flag 
      real last_signal                    ! amplitude of last sample
      real diff                           ! time difference
c      common /time_1/ start_time,end_time,wave_start,wave_end
c
c check if memory is full
c
c finding first segment where head_memory is true i.e. waveform data in memory
c
      flag = .false.
      do j=1, cwav_nseg(id)
         if (head_memory(id,j).and..not.flag) then
	    first_wav_segment = j
	    flag = .true.
         endif
      enddo

      counter=0
      new_memory(id)=.false.
      do j=first_wav_segment, cwav_nseg(id)
	 if (.not.new_memory(id)) then
	   counter = cwav_nsamp(id,j) + counter
	   if (counter.ge.max_cnsamp) then
	       new_memory(id)=.true.
	       seg_end = j
           endif
         endif
      enddo
c
c memory is full
c
      if (new_memory(id)) then       
	 l=0
	 do j=1, cwav_nseg(id)
	    if (head_memory(id,j).and. 
     &		 l.lt.cwav_nseg(id)-last_seg_nr(id)) then
	       head_memory(id,j)=.false. !first waveform file no longer in memory
	       l=l+1
	    endif
	 enddo
      endif

      wav_nsamp(id)=last_wav_nsamp(id)
      new_trace=.true.

      ierror=-1
      do j=last_seg_nr(id)+1,cwav_nseg(id) 
        if (new_trace) then             !starting with a new trace
          ierror=0
          new_trace=.false.
        endif
 	if (last_wav_nsamp(id)+cwav_nsamp(id,j).gt.max_cnsamp) 
     &    wav_nsamp(id)=0
c
c extract samples for all files not in memory
c
        wav_filename(1)=cwav_filename(id,j)
        call wav_read_channel(cwav_chan_nr_file(id,j))!signal1 put in commomblock
c store the value of wav_nsamp that correspond to the first sample in segment
        new_seg=.true.
        if(new_seg) then
          cwav_start_samp_nr(wav_nchan,j)=wav_nsamp(wav_nchan)+1
        endif

        do i=1, cwav_nsamp(id,j)
c
c set flag, indicating that data found
c
          cwav_flag(wav_nchan)=.true.
          wav_nsamp(wav_nchan)=wav_nsamp(wav_nchan)+1
          cwav_full_signal(wav_nchan,wav_nsamp(wav_nchan))=
     &	      signal1(i) 
          if (wav_nsamp(wav_nchan).eq.1) then
            wav_abs_time(wav_nchan)=cwav_abs_time(id,j)+
     &            i/cwav_rate(id,j)
            wav_rate(wav_nchan)=cwav_rate(id,j)
          endif
        enddo 
	head_memory(id,j)=.true.
        last_wav_nsamp(wav_nchan)=wav_nsamp(wav_nchan)
      enddo
      last_seg_nr(id)=cwav_nseg(id)
cc
cc check if samples have to be filled in
cc
cc             if (new_seg.and.wav_nsamp(wav_nchan).ne.0.and.j.gt.1) then
cc               diff=cwav_abs_time(id,j)-
cc     &               cwav_abs_time(id,j-1)-cwav_duration(id,j-1)
cc               write(*,*) ' filling gap of sec ',diff
cc               write(*,*) ' with samples ',int(diff*cwav_rate(id,j))
cc               write(*,*) ' in wav file ',cwav_filename(id,j)
cc               write(*,*) '  ' 
cc               do l=1,int(diff*cwav_rate(id,j))
cc                 wav_nsamp(wav_nchan)=wav_nsamp(wav_nchan)+1
ccc                cwav_full_signal(id,i)=        !ny
cc                 cwav_full_signal(wav_nchan,wav_nsamp(wav_nchan))=
cc     &			 (last_signal+signal1(i))/2.
cc               enddo
cc             endif
cc             new_seg=.false.
cc             wav_nsamp(wav_nchan)=wav_nsamp(wav_nchan)+1
ccc            cwav_full_signal(id,i)=
cc             cwav_full_signal(wav_nchan,wav_nsamp(wav_nchan))=
cc     &		     signal1(i) 
cc             last_signal=signal1(i)
cc             if (wav_nsamp(wav_nchan).eq.1) then
cc               wav_abs_time(wav_nchan)=cwav_abs_time(id,j)+
cc     &              i/cwav_rate(id,j)
cc               wav_rate(wav_nchan)=cwav_rate(id,j)
cc             endif
cc         enddo 
cc       write(*,*)'wav_nsamp(',wav_nchan,') ',wav_nsamp(wav_nchan)
cc       write(*,*)'cwav_nseg(',id,') ',cwav_nseg(id)
cc      enddo
      return
      end

c ---------------------------------------------------------------
c
c routine to read signal from continuous database in required time interval
c
      subroutine wav_read_req_signal(id,start_msec,
     &                             end_msec,deci,fflag,ierror) 

      implicit none
c
c  input:   - id            trace id
c           - chan_id       channel id
c           - start_msec    required start time
c           - end_msec      required end time
c           - deci          decimation factor
c           - fflag         filter flag
c           - ierror        error code, 0 is ok
C
C    Seisan library inserts and routines...
C    ======================================
C
      include 'mulplt.inc'
      include 'libsei.inc'                ! Open file definitions
      include 'continuous.inc'
C
C    ============= end of list ==========
C
      integer id                          ! trace and channel id
      double precision start_msec,end_msec ! time in sec
c      double precision wave_start,wave_end ! required time 
c      character*14 start_time,end_time    ! extended time yyyymmddhhmmss
      double precision sample_time        ! time interval between samples
      integer deci                        ! decimation factor
      logical fflag                       ! filter flag
      logical flag                        ! help flag
      integer ierror                      ! error code, 0 is ok
      integer segments                    ! number of segments in waveform memory
      integer first_segment               ! first segment in waveform memory
      integer i,j,k                       ! counter
      integer year,month,day,doy,hour,min ! date and time
      real sec                            ! --------
c      common /time_1/ start_time,end_time,wave_start,wave_end


      wav_nsamp_req(id)=0
      ierror=0
c
c finding in what order to look through the waveform files 
c
      flag=.false.
      segments = 0
      do j=1, cwav_nseg(id)
	 if (head_memory(id,j)) segments = segments + 1
         if (head_memory(id,j).and..not.flag) then
	    first_segment = j
	    flag = .true.
         endif
      enddo

c
c extact samples in requested time interval
c
      do j=first_segment, first_segment + segments - 1 
        do k=cwav_start_samp_nr(id,j), 
     &	     cwav_start_samp_nr(id,j)+cwav_nsamp(id,j)-1
          sample_time=cwav_abs_time(id,j) +
     &  	      (k-cwav_start_samp_nr(id,j)) * 1/cwav_rate(id,j)
          if (sample_time.ge.start_msec.and.          !requested time interval
     &        sample_time.le.end_msec) then

c
c set flag, indicating that data found 
c
	    cwav_flag(id)=.true.
c
c counting the number of samples in requested time interval
c
            wav_nsamp_req(id) = wav_nsamp_req(id) + 1
	    i= k
c
c copy signal
c
            cwav_signal(id,wav_nsamp_req(id))=
     &          cwav_full_signal(id,k)
          endif
        enddo	
      enddo	
c
c set header information for the data to be plotted
c
        wav_stat(wav_nchan)=cwav_stat(id)
        wav_comp(wav_nchan)=cwav_comp(id)
        wav_cbyte(wav_nchan)='4'
        wav_duration(wav_nchan)=wav_nsamp_req(wav_nchan)/
     &		     wav_rate(wav_nchan)
        call sectim(start_msec,year,doy,month,day,hour,min,
     &          sec)
        wav_year(wav_nchan)=year
        wav_month(wav_nchan)=month
        wav_day(wav_nchan)=day
        wav_hour(wav_nchan)=hour
        wav_min(wav_nchan)=min
        wav_sec(wav_nchan)=sec

      return
      end

c ---------------------------------------------------------------

      subroutine cplot_multi_trace(interval)

      implicit none
C
C    Seisan library inserts and routines...
C    ======================================
C
      include 'mulplt.inc'
      include 'continuous.inc'
      include 'libsei.inc'
      include 'seisan.inc'
C
C    ============= end of list ==========
C
      double precision minium  ! time data starts in memory
      character*12 time,s_time ! time data starts in memory
      integer year,month,day,doy,hour,min  ! date and time
      real sec                             ! --------
      integer first_wav_segment ! first segment where head_memory is true
      logical flag             ! flag
      integer ix,iy,iiy        ! screen position 
      integer pix,piy          ! previous position       
      integer color_type       ! window color
      character*1 cha_old      ! character returned by mouse of keyboard
      integer id               ! trace id
      integer n                ! skip n samples when plotting
      integer i,j,k            ! counter
      integer m                ! number of traces to be plotted 
      real dc                  ! dc
      real max_amp             ! max amplitude
c      character*14 start_time,end_time       ! extended time  yyyymmddhhmmss
c      double precision wave_start,wave_end   ! required time
      logical clearflag        ! flag for clearing the screen
      character*10 interval    ! filter interval
      real fflow,ffhigh        ! filter band pass f
      real cof(8)              ! filter coefficients
      real gain                ! filter gain
c      common /time_1/ start_time,end_time,wave_start,wave_end
c
c  default values
c
       cha_old=' '
c
c  x-window size
c
       color_type=0
c
c  copy signal
c
       clearflag=.false.
       m=0    
       do id=1,wav_nchan
        do k=1, n_cont_comp
         if (cwav_flag(id).and.wav_comp(id).eq.cont_comp(k)) then
	   m=m+1
           do i=1,wav_nsamp_req(id)
             signal1(i)=cwav_signal(id,i)
           enddo
c
c  remove dc
c
           call remove_dc(signal1,wav_nsamp_req(id),
     &		    dc,wav_nsamp_req(id))
c
c filter 
c
          if (interval(1:1).eq.' ') then
	    if (.not.clearflag) then
	       call xclear                    ! clear display   
	       clearflag=.true.
	    endif
	    if (interval(2:2).ne.'n') then
               read(interval,'(1x,f4.1,1x,f4.1)') fflow,ffhigh
               call bndpas(fflow,ffhigh,1000.0/wav_rate(id),cof,gain)
               call filter(signal1,wav_nsamp_req(id),cof,gain,1)   ! 4 pole one way
               call xtext('Filter:',7,cplot_xmax-100,cplot_ymax-15)
               call xtext(interval,10,cplot_xmax-100,cplot_ymax-30)
               call xtext('----------',10,cplot_xmax-100,cplot_ymax-40)
            else
               call xtext('         ',9,cplot_xmax-100,cplot_ymax-15)
c              call xtext('no filter',9,10,cplot_ymax-15)
            endif
          endif
c
c  find max amplitude 
c
           max_amp=0.
           do i=1,wav_nsamp_req(id)
             if (abs(signal1(i)).gt.max_amp) max_amp=abs(signal1(i))
           enddo

           do j=1,cwav_nseg(id)
             wav_rate(j)=cwav_rate(id,j)
           enddo
c
c   write the seismogram 
c
c           iiy=int(-float(m)/float(wav_nchan)*600.)+
c          iiy=int(-float(id)/float(wav_nchan)*600.)+
c     &         int(1./float(wav_nchan)*600.)+800 !initial y-axis position
c
c changed lo 25-04-2001
c

           iiy=(cplot_ymax-25)-
     *            int(float(id)/float(wav_nchan)*(cplot_ymax-100)-
     *            1/float(wav_nchan)*(cplot_ymax-100)/2.)
           call xtext  !writing base and comp name(length of text,x pos, y pos) 
     & (wav_stat(id)// ' '// wav_comp(id),10,10,iiy)
           call xmoveto(100,iiy)
           pix=100
           piy=iiy
c
c  resampling only plot every n sample
c
           call resample(id,n)   ! changed resample, 25-04-2001
c           call resample(id,iiy,n)
c
c  points along the x-axis
c
           do i=1,wav_nsamp_req(id),n
c             ix=int(float(i)/float(wav_nsamp_req(id))*xsize)+100
c             iy=int(signal1(i)/max_amp*
c     &           1./float(wav_nchan)*280.)+iiy
c
c changed lo 25-04-2001
c
             ix=int(float(i)/float(wav_nsamp_req(id))*
     *             (cplot_xmax-130))+100

             write(666,*) i,ix

             iy=int(signal1(i)/(1.25*max_amp)*(cplot_ymax-100)/
     *         float(wav_nchan))+iiy
c
c  no points with the same x value are plotte
c
               call xlineto(pix,piy,ix,iy)
             pix=ix
             piy=iy
           enddo
         endif
	enddo
       enddo
c
c finding first segment where head_memory is true i.e. waveform data in memory
c
      minium = 9999999999.9999
      do id=1,wav_nchan
         flag = .false.
	 do j=1, cwav_nseg(id)
	    if (head_memory(id,j).and..not.flag) then
	       first_wav_segment = j
	       flag = .true.
	       if (minium.ge.cwav_abs_time(id,j)) minium = cwav_abs_time(id,j)
	    endif
	 enddo
      enddo
c
c  typing the start time
c
      call sectim(minium,year,doy,month,day,hour,min,sec)
      write(time(1:5), '(i2,a,i2,)') hour,':',min
      do i=1,5               ! fill up blancs with 0
         if(time(i:i).eq.' ') s_time(i:i)='0'
      enddo
      call xtext('Max back to',11,10,cplot_ymax-15)
      call xtext(time,5,13,cplot_ymax-30)
      call xtext('----------',10,10,cplot_ymax-40)
c  
c  draw the x-axis
c
       call x_axis(cplot_xmax-130,45)

       return
       end 


c ----------------------------------------------------------------

      subroutine x_axis(xsize,y_position)
c
c  draw the x-axis and writes the date, start and end time
c
      implicit none
C
C    Seisan library inserts and routines...
C    ======================================
C
      include 'mulplt.inc'
      include 'continuous.inc'
      include 'seisan.inc'
C
C    ============= end of list ==========
C

      integer i                              ! counter
      integer axis                           ! position of axis
      integer alpha,beta                     ! help variable
      integer xsize                          ! length of the x-axis in pixels
      integer y_position                     ! last position in the y direction
      character*12 s_time                    ! required start time  
      character*10 dato                      ! required dato   
      integer year,month,day,doy,hour,min    ! date and time
      real sec                               ! --------
      real start_sec                         ! seconds in start time
      real end_sec                           ! seconds in end time
      integer start_min_I                    ! minutes in start time
      character*5 start_min                  ! minutes in start time
c      character*14 start_time,end_time       ! extended time  yyyymmddhhmmss
c      double precision wave_start,wave_end   ! required time
c      common /time_1/ start_time,end_time,wave_start,wave_end

       if (n_cont_comp.eq.0) then
         write(*,*)'ERROR ' 
         write(*,*)' components (CONT_COMP) must be given in SEISAN.DEF'
	 stop
       endif
c  
c  draw the x-axis
c
c       if (n_cont_comp*n_cont_base.eq.1) y_position=750
c       axis=int(y_position-(800-y_position)/(n_cont_comp*n_cont_base))
       axis= y_position
       call xlineto(100,axis,
     &		    xsize+100,axis)
       call xlineto(100,axis,100,axis-20)
       call xlineto(xsize+100,axis,xsize+100,axis-20)
c
c  typing the start time 
c
       call sectim(wave_start,year,doy,month,day,hour,min,sec)   ! start time
       write(s_time(1:9), '(i2,a,i2,a,F3.0)') hour,':',min,':',sec
       do i=1,9               ! fill up blancs with 0
         if(s_time(i:i).eq.' ') s_time(i:i)='0'
       enddo
       s_time(9:9)=' '
       call xtext(s_time,9,73,axis-38)
       start_sec = sec
       write(start_min(1:2),'(i2)') min
c      write(*,*)'min',start_min
       start_min_I = min

c
c  typing the date
c
       write(dato(1:10),'(i4,a,i2,a,i2)') year,'/',month,'/',day !date
       do i=1,10                ! fill up blancs with 0
         if(dato(i:i).eq.' ') dato(i:i)='0'
       enddo
       call xtext(dato,10,450,axis-40)
c
c  typing the end time
c
       call sectim(wave_end,year,doy,month,day,hour,min,sec)   ! start time
       write(s_time(1:9), '(i2,a,i2,a,F3.0)') hour,':',min,':',sec
       do i=1,9                ! fill up blancs with 0
         if(s_time(i:i).eq.' ') s_time(i:i)='0'
       enddo
       s_time(9:9)=' '
       call xtext(s_time,9,xsize+74,axis-38)
       end_sec = sec
c
c making a tick line and number every minute
c
       alpha=cont_interval/60
       if (int((real(cont_interval/60)-int(cont_interval/60))*60).ne.0.
     &	   and.start_sec.gt.end_sec) 
     &	   alpha = alpha + 1
       do i=0,alpha-1 
 	 beta = int((60-start_sec)*xsize/cont_interval)+
     &      int(60*i*xsize/cont_interval) + 100
         call xlineto (beta, axis, beta, axis-10)
         start_min_I = start_min_I + 1
	 if (start_min_I.eq.60) start_min_I = 0
         write(start_min(1:2),'(i2)') start_min_I
         call xtext(start_min,2,beta-8,axis-25)
       enddo

       return
       end

c ----------------------------------------------------------------

      subroutine resample(id,n)  
c
c  resampling only plot every n sample
c
      implicit none
C
C    Seisan library inserts and routines...
C    ======================================
C
      include 'mulplt.inc'
      include 'continuous.inc'
C
C    ============= end of list ==========
C
      integer id               ! trace id
      integer iiy              ! screen position 
      integer n                ! skip n samples when plotting
      integer j                ! counter
      real rti                 ! required time interval 
      real wav_rateH           ! sample rate helper
c      character*14 start_time,end_time       ! extended time  yyyymmddhhmmss
c      double precision wave_start,wave_end   ! required time
c      common /time_1/ start_time,end_time,wave_start,wave_end

      n=1

      n=int((wave_end-wave_start)*cwav_rate(id,1)/(cplot_xmax*3.))

c      do j=1,cwav_nseg(id)
c        wav_rateH=cwav_rate(id,j)
c      enddo
c      rti=wave_end-wave_start
c      if(wav_rateH.ge.100.and.rti.ge.180) then
c         n=50
c         call xtext('50',2,905,iiy)
c	 call xtext('plotting every 50 sample',24,200,700)
c      else 
c         n=1
c         call xtext('1',1,905,iiy)
c	 call xtext('plotting every sample',21,200,700)
c      endif 
      write(*,*)'Plotting every ',n,' sample of ',   
     &	   cwav_stat(id),cwav_comp(id)

      return
      end

c ----------------------------------------------------------------

      subroutine write_cont_wavfile  
c
c  makes an output waveform file
c
      implicit none
C
C    Seisan library inserts and routines...
C    ======================================
C
      include 'mulplt.inc'
      include 'continuous.inc'
      include 'libsei.inc'                   ! Open file definitions
      include 'seisan.inc'
C
C    ============= end of list ==========
C
      character*5 net_code                   ! network code
      character*80 outfile                   ! file name
c      character*1040 chead                   ! trace header
      character*5 base,def_base              ! data base name
      integer i,j,id                         ! help variable
      integer temp(max_trace)                ! temporary number of samples


c     call xtext('Answer questions in the terminal window',39,
c    &		 100,cplot_ymax-15)
c
c finding the database, if nothing given in SEISAN.DEF or CPLOT.FOR then 
c  the default base is used (COM/.SEISAN)
c
      i= ichar(cont_net_code)
      call getenv('DEF_BASE',def_base)
      net_code = ' '
      if (i.ne.0) then
         net_code = cont_net_code
      else
	 net_code = DEF_BASE
      endif
c
c temporary storing wav_nsamp since they are overwritten below 
c    and have to be remembered
c
      do id=1, n_cont_trace
	temp(id)=wav_nsamp(id)
	wav_nsamp(id)=wav_nsamp_req(id) 
      enddo
c
c set waveform header
c
      call wav_sheads(1,net_code,outfile,mainhead,chead)
      open(66,file=outfile,status='unknown',form='unformatted')
c
c write main header
c
      do i=1,12
        write(66) mainhead(i)
      enddo
c
c write traces
c
      do i=1,wav_nchan
        if (cwav_flag(i)) then
c
c make trace header, and write it out
c
          call wav_sheads(i,net_code,outfile,mainhead,chead)
          write(66) chead
c
c write out data
c
          write(66) (int(cwav_signal(i,j)),j=1,wav_nsamp(i))
        endif
      enddo
      close(66)
c
c writing the values of wav_nsamp back to memory
c
      do i=1, n_cont_trace
	wav_nsamp(i)=temp(i)
      enddo
c
c making the s-file
c
      call write_cont_sfile(outfile,net_code)

      end

c ----------------------------------------------------------------

      subroutine write_cont_sfile(wavfile,net_code)  
c
c  makes an output s-file
c
      implicit none
C
C    Seisan library inserts and routines...
C    ======================================
C
      include 'mulplt.inc'
      include 'continuous.inc'
      include 'libsei.inc'                ! Open file definitions
      include 'seisan.inc'
C
C    ============= end of list ==========
C
      character*80 wavfile                ! waveform file name
c      character*80 sfile                  ! sfile name
      character*80 tmp_file               ! temporary sfile name
      integer nf                          ! length of sfile name
      character*5 net_code                ! network code
      logical b_old                       ! logical for exixting file or not
      logical b_eof                       ! logical 
      integer code                        ! returned code
      integer write01                     ! write unit number 1-5
      character*80 idline                 ! id line             
      character*12 p_time                 ! processing time
      character*14 proc_time              ! processing time
      character*1 type,type_in            ! event type
      character*5 base                    ! data base name
      character*4 operator,operator_in    ! operator name
      integer i                           ! counter
      integer year,month,day,hour,min,isec ! date and time
      real sec                            ! --------

c
c   set event type, local is default
c
 100  continue
      write(*,*)' ' 
      write(6,*)' Event type for all events: Local:    L (default)'
      write(6,*)'                            Regional: R'
      write(6,*)'                            Distant:  D'
      write(6,*)'                            Quit:     Q'
      read(5,'(a)') type_in
      if(type_in.eq.' ') then
         type='L'
      else
         type=type_in
      endif

      if(type.eq.'q'.or.type.eq.'Q') then        ! quiting making a file
         write(*,*)' Continue in the cplot window '
         return
      endif

      if(type.eq.'l') type='L'
      if(type.eq.'r') type='R'
      if(type.eq.'d') type='D'
      if(type.ne.'L'.and.type.ne.'R'.and.type.ne.'D') then
          write(6,*)' Wrong type, enter again'
 	  goto 100
      endif
      write(6,*)' event type is: ', type
c
c get data base name
c
      base = net_code
      if(base(1:1).ne.' ') then
	do i=2, 5
	   if (base(i:i).eq.' ') base(i:i) = '_'
        enddo
      endif
      write(6,*)' data base is: ', base
c
c getting the operator, if nothing is written then the operator 
c  given in SEISAN.DEF is used
c
      i= ichar(cont_operator)
 60   continue
      if (i.eq.0) then
         write(*,*) ' Operator, max 4 chars '
      else
         write(*,*) ' Operator, max 4 chars (default is '
     &		      ,cont_operator,')'
      endif
      read(5,'(a)') operator_in
      if(operator_in.eq.'    '.and.i.eq.0) then
         write(6,*)' You must give operator id'
         goto 60
      elseif(operator_in.eq.'    '.and.i.ne.0) then
         operator = cont_operator
      elseif(operator_in.ne.'    ') then
	 operator = operator_in
      endif
      write(6,*)' operator is: ', operator
c
c   make output sfile name
c
      year  = wav_year(wav_nchan)
      month = wav_month(wav_nchan)
      day   = wav_day(wav_nchan)
      hour  = wav_hour(wav_nchan)
      min   = wav_min(wav_nchan)
      sec   = wav_sec(wav_nchan)
      isec  = sec
      call sfilname
     &   (year,month,day,hour,min,isec,base,type,sfile,nf)
      write(*,*) ' '
      write(*,*)' waveform file: '
      write(*,*) wavfile
      write(*,*) ' '
      write(6,*)' sfile: '
      write(6,*) sfile

c   write header line in file
c
      data(1)=' '
      write(data(1),'(1x,i4,1x,2i2,1x,2i2,1x,f4.1,1x,a1)')
     &   year,month,day,hour,min,sec,type
      data(2)(1:1)=' '
      data(1)(80:80)='1'
c
c   next line is id line
c
      idline(1:40)= ' ACTION:                   OP:     STATU'
      idline(41:80)='S:               ID:                   I'
      write(idline(61:75),'(i4,5I2)')
     &   year,month,day,hour,min,isec
      do i = 61, 74
	if (idline(i:i).eq.' ') idline(i:i)='0'
      enddo
      call systime(p_time,proc_time)
      write(idline(31:34),'(A)') operator
      write(idline(13:26),'(A)') proc_time
      write(idline(9:11),'(A)')'ARG'
      data(2) = idline
c
c   line with waveform file name
c
      do i = 1, 80
	data(3)(i:i) = ' '
      enddo
      i = index(wavfile,' ') - 1
      if (i.lt.1) then
	data(3)(2:13) = wavfile(1:12)
      else
	data(3)(2:i+1) = wavfile(1:i)
      endif
      data(3)(80:80) = '6'
c
c   help line
c
      data(4)(1:57) =
     &  ' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO '
      data(4)(58:80)='AIN AR TRES W  DIS CAZ7'
c
c   blank last line
c
      do i = 1, 80
	data(5)(i:i) = ' ' 
      enddo
c
c   open and write s-file
c
      tmp_file = sfile(1:80)
      call sei open(unknown$+warn$,       ! Open an unknown status file.
     &                      ' ',          ! Prompt file name (n/a).
     &                      tmp_file,     ! File name
     &                      write01,      ! Write unit #1
     &                      b_old,        ! Already exists? (n/a).
     &                      code)         ! Returned condition
      if (code .ne. e_ok$) then
        write(6,*)' Data base sub directory not made'
        stop
      else 
        write(write01,'(a80)',iostat=code) (data(i),i=1,5)
        call sei code(stop$,code,write01,b_eof)
        call sei close (close$,write01,code)
      endif

      write(*,*) ' '
      write(*,*)' Continue in the cplot window '
      return
      end

c ----------------------------------------------------------------

