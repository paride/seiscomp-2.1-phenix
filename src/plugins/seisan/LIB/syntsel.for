      subroutine syntsel(data,ndata,t0,window,t0synt,dtt,nsynt,codsyn)
c
c     updates jan 25
c
c     jan 95   jh    *********version 5.0 ******************************
c     feb 95         new findchan
c     april 28 , 95  fix rotaion bug with angle
c     may  8     95  new seisinc common block
c     oct 95 by jh    : new seisinc, use delays by default
c     april 99 by jh  : --------------   version 7.0 check ----------------
c                       stats to 5 char, year 2000
c     august 99       : wrong year in synthetic data
c                       call get_seisan_def
c     may 30          : wrong year write out of synt data for 2000
c     feb 19, 2001 lo : changed common block signal
c
c
c   Puts together synthetic data from WKBJ  and real data
c   from corresponding Seisnor file to one Seisnor file. One real
c   trace is followed by one synthetic.
c   The time window starts at the seism origin + t0
c 
c
c   input:  data      the hyp.out file
c           ndata:    number of records in hyp.out
c           t0    :   start time of the plot in seconds from seism origin time
c           window:   time window to plot(secs)
c           t0synt:   start time of the synthetics at each station
c                                 in seconds from seism origin time 
c           dtt   :   sampling interval of the synthetics (secs)
c           nsynt:    number of samples in a synthetic signal
c           codsyn:   'SW '(for WKBJ), 
c                           'SH '(for Herrmann) or 'SB '(for Bouchon)
c
c
       implicit none
       include 'seidim.inc'                  ! dimensions
       include 'libsei.inc'                  ! file open etc
c-- single trace file header
      character*1040	chead,cheadsave
c-- main header
      character*80      mhead(max_trace),mmhead(max_trace)
c-- data array for s-file
      character*80 data(*)
c-- trace file name
      character*80 trace_file,tt,file
c-- network code
      character*5 net_code		
c-- dummy
      character*80 dummy		
c-- station code
      character*5  stat(max_trace)		
c-- code for labelling the synthetic components
      character*3 codsyn
c-- file units
      integer wav_unit,unit_out
c-- code for component orientation
      logical radial
c-- component
      character*4  comp(max_trace)	,compd(max_trace)
c-- amplitudes of the transfer fonctions at 1Hz
      real ampln,ample,dampl
c-- station and component
      character*9 statcomp(max_trace)
c-- start time of plot and synthetic relative to seism origin time
      real t0,t0synt(max_trace),dtt
c-- start time to plot relative to first sample
      real tstart(max_trace)
c--  time window selected
      real window
c-- ---------
      integer n,ns
c-- read function
      real sei real num
c-- header parameters
      real strike,dip,rake,depth
      integer istrike,idip,irake
c-- path to seismo
      character*60 top_directory		
c-- number of channels in w-file
      integer nchan
c-- .true. if waform file exists
      logical exist			
c-- number of samples in one channel
      integer nsamp
c-- 1: test output, 0: no output
      integer check			
c-- origin date
      integer oyear,omonth,oday,odoy		
c-- origin time
      integer ohour,omin		
c-- origin time
      real    osec			
c-- date of first sample
      integer wyear,wmonth,wday		
c-- time of ------------
      integer whour,wmin		
c-- --------------------
      real    wsec
c-- date of first sample for synthetics
      integer syear,smonth,sday,sdoy		
c-- time of ------------
      integer shour,smin		
c-- --------------------
      real    ssec
c-- abs times
      double precision wtime,otime,stime
c-- sample rate
      real    rate			
c-- error variables
      integer error,ierr		
c-- channel numbers to select
      integer chan(max_trace)			
c-- interval each channel
      real cinter(max_trace)
c-- delay of first sample selected relative to origin time
      real delay(max_trace)
c-- addtional channel delay
      real delay_chan(max_trace)
c-- text string
      character*80 text
c-- number of signals in synthetic traces
      integer nsynt
c-- sample rate synthetic data
      real syntrate
c-- station number corresponding to a given channel
      integer jstat(max_trace)
c-- p-residual
      real p_residual
c-- azimuth for rotating the horizontal seismograms
      real backazi,caz,saz
c-- help variables
      real c_delay
      integer	i,j,l,n1,n2,k,ichan,kchan,ilec,ndata,ncbid,code
c-- seismic signal
      integer*2 isignal2(max_sample*2)
      integer b_flag                         ! used with file opening
      real signal(max_sample)			
      real signaln(max_sample),signale(max_sample)
      integer isignal(max_sample)
      real baz(max_trace)
      character*1 rot_comp(max_trace)  ! T or R
      real        del_rot       ! delay rotated trace
      logical rotate
c
      equivalence(signal,isignal)
c
c      common /signal/signal,rotate,baz,rot_comp,del_rot
      common /signalx/signal
      error=0
      check=0
c
c   get seisan defaults
c
      call get_seisan_def
c
c   read seism origin time
c
      read(data(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)')
     *oyear,omonth,oday,ohour,omin,osec
c
c   get seism origin time in secs
c
      call timsec(oyear,omonth,oday,ohour,omin,osec,otime)
c
c   calculate plot origin time in secs
c
      otime = otime + t0
      call sectim(otime,oyear,odoy,omonth,oday,ohour,omin,osec)
c
c   read if Radial-Transverse or North-East components
c
      do i=1,ndata
         if(data(i)(2:15).eq.'SYNT: COMPON-:') then
            radial=.false.
            if  (data(i)(20:25).eq.'RADIAL') radial=.true.
         endif
      enddo
c
c   find number of channels to display, if component is blank, use all,else
c   only use component selected. At least the type of instrument must be
c   specified, e.g. S
c
      nchan=0
      do i=1,ndata
         if(data(i)(80:80).eq.'3'.and.data(i)(2:15).eq.
     *      'SYNT: STATION:'.and.data(i)(22:23).ne.'  ')
     *   then
            do k=1,nchan
              if(statcomp(k).eq.data(i)(17:25)) goto 80
            enddo
c
c   channel(s) was not counted before
c
            nchan=nchan+1
            statcomp(nchan)=data(i)(17:25)
            if(statcomp(nchan)(9:9).eq.' ') then
               statcomp(nchan)(9:9)='Z'
               statcomp(nchan+1)=statcomp(nchan)
               statcomp(nchan+1)(9:9)='N'
               if (radial) statcomp(nchan+1)(9:9)='R'
               statcomp(nchan+2)=statcomp(nchan)
               statcomp(nchan+2)(9:9)='E'
               if (radial) statcomp(nchan+2)(9:9)='T'
               nchan=nchan+2
            endif
 80         continue
         endif
      enddo
      write(6,*)'Number of channels to display', nchan
c
c  make codes for real and synthetic channels which will be every second
c
      k=1
      j=0
      do i=1,nchan*2,2
         stat(i)=statcomp(k)(1:5)
         stat(i+1)=stat(i)
         comp(i)=statcomp(k)(6:9)
         compd(i)=comp(i)
         if (compd(i)(4:4).eq.'R')  compd(i)(4:4)='N'
         if (compd(i)(4:4).eq.'T')  compd(i)(4:4)='E'
         comp(i+1)(1:3)=codsyn
	     comp(i+1)(4:4)=statcomp(k)(9:9)
         if (i.eq.1.or.stat(i).ne.stat(i-2))  j=j+1
         jstat(i+1)=j
         k=k+1
      enddo
c
c   get trace data file name from s-file
c
c     rewind 15
c     call auto_filename(trace_file,15,dummy)
      rewind 1
      call auto_filename(trace_file,1 ,dummy)
      exist=.false.
      if(trace_file(1:3).eq.'EOF') then
         write(6,*) 'No waveform file name available for event'
         exist=.false.
c
c   look for waveform file first in own direcrtory, then in WAV
c
      else
        i=index(trace_file,' ')-1
        write(6,'(1x,a,a)') ' File to look for: ',trace_file(1:i)
c       chr_f_form$ = 'unformatted'
        chr_f_access$ = 'direct'            ! Type of file.
        f_recl$=2048                        ! Record length

        call sei get file( check$+warn$,    ! Find and open without messages.
c       call sei get file( open$,    ! Find and open without messages.
     &                       0,         ! On file unit.
c    &                       wav_unit,         ! On file unit.
     &                   code,             ! Condition (n/a).
     &                   'WAV',            ! Alternative directory to search.
     &                   trace_file   )    ! For this file.
c     chr_f_form$ = 'unformatted'                 ! Type of file.
      do i=1,80
         if(ichar(trace_file(i:i)).eq.0) trace_file(i:i)=' '
      enddo
c     write(6,*)' Open', trace_file

       call sei open( old$+warn$,                        ! Open old file.
     &               ' ',                         ! No prompt.
     &               trace_file,                       ! This file.
     &               wav_unit,                   ! On this unit.
     &               b_flag,                      ! Exists?.
     &               code )                       ! Returned condition.


        if(wav_unit.gt.0) exist=.true.    ! set flag if file ok

      endif
c
c---------------------------------------------------------
c   enter loop for channels, first read header only,
c---------------------------------------------------------
c
      do 1000 ichan=1,2*nchan,2
        if (ichan.gt.1.and.stat(ichan).eq.stat(ichan-2))   then 
           delay_chan(ichan)= delay_chan(ichan-2)
           delay_chan(ichan+1)= delay_chan(ichan-1)
        else
c
c   find first station to be modelled, read P time residual
c
        do i=2,ndata
           if(data(i)(2:6).eq.stat(ichan).
     &                   and.data(i)(11:11).eq.'P') then
             read(data(i)(64:68),'(f5.1)') p_residual
             goto 300
           endif
        enddo
 350    write(6,*)'No P-reading '
        p_residual=0.0
 300    continue
c
c   read how to delay the data trace to make sure synthetic and real
c   signal are aligned.
c
        delay_chan(ichan)=0.0
c       if (chan(ichan).ne.0)   then
          write(6,'(a,a,a,f7.3)')
     *    ' At station ',stat(ichan),', the P-residual is :',p_residual
          write(6,'(a)')
     *  ' By how much do you want to delay the data trace ?'
          write(6,'(a$)') 
     &   ' Return for P-residual delay, which align P-phases '
           read(5,'(a)') text
          c_delay=sei real num(text, code)
c         write(6,*) c_delay
          if(text(1:4).eq.'    ') then
              delay_chan(ichan)=-p_residual
          else
              delay_chan(ichan)=c_delay
          endif
c          if(text(1:4).ne.'    ')   read(text,*) delay_chan(ichan)
c       endif
c
c   do not delay the synthetic traces
c
        delay_chan(ichan+1)=0.0
c
      endif
c
c   find channel number corresponding to station and
c   component, also get header time
c
         if (exist) then
           call seisinc
     *     (wav_unit,0,ncbid,0,mmhead,chead,0.0,0.0)
c          rewind wav_unit 
           call findchan
     *     (mmhead,stat(ichan),compd(ichan),chan(ichan),wyear,wmonth,
     *     wday,whour,wmin,wsec,ierr)
           if(ierr.eq.1) then
              error=error+1
              write(6,'(1x,a5,a4,1x,a)')
     *        stat(ichan),compd(ichan),' Not found in trace file'
           endif
         endif
         if(.not.exist.or.ierr.eq.1) then
           write(6,*)' Cannot find waveform file',ierr
           chan(ichan)=0
	   cinter(ichan)=0.0
	   tstart(ichan)=0.0
         else
           if(check.eq.1) write(6,*)'chan',chan(ichan)
c          rewind wav_unit 
           call seisinc
     *     (wav_unit,chan(ichan),ncbid,2,mmhead,chead,0.0,0.0)
c
c   get sample rate and number of samples
c
           read(chead(37:43),'(f7.2)')rate
           read(chead(45:50),'(i6)') nsamp
c
c  get abs trace file start time
c
           call timsec(wyear,wmonth,wday,whour,wmin,wsec,wtime)
           if(check.eq.1) write(6,*) wyear,wmonth
c
c   calculate start time for window  (origin time) relative to first
c   sample in trace data and check if window length should be reduced
c
           tstart(ichan)=otime-wtime -delay_chan(ichan)
           delay(ichan)=0.0
           cinter(ichan)=amin1(window,(nsamp-1)/rate-tstart(ichan))
           if(tstart(ichan).lt.0.0) then
              cinter(ichan)=amin1(window+tstart(ichan),(nsamp-1)/rate)
              delay(ichan)=abs(tstart(ichan))
              tstart(ichan)=0.0
           endif
         endif
c
c   put in data for synthetic channels
c
         tstart(ichan+1)=t0-t0synt(jstat(ichan+1)) -delay_chan(ichan+1)
         delay(ichan+1)=0.0
         cinter(ichan+1)=amin1(window,dtt*(nsynt-1)-tstart(ichan+1))
         if(tstart(ichan+1).lt.0.0) then
            cinter(ichan+1)=amin1(window+tstart(ichan+1),dtt*(nsynt-1))
            delay(ichan+1)=abs(tstart(ichan+1))
            tstart(ichan+1)=0.0
         endif
c
 1000 continue
c
c   make main header and file name
c
      net_code='     '
      call mfhead(oyear,omonth,oday,ohour,omin,osec,window,nchan*2,
     *           net_code,stat,comp,delay,cinter,file,mhead)
c
c   get vital info for header
c
      do i=1,ndata
         if(data(i)(2:15).eq.'SYNT: ST-D-RK:') 
     &         read(data(i),'(15x,3f10.2)') strike, dip, rake
         if(data(i)(2:15).eq.'SYNT: DEPTH--:') 
     &         read(data(i),'(15x,f10.2)') depth
      enddo
      istrike=strike+0.5
      idip=dip+0.5
      irake=rake+0.5
c 
      write(mhead(1)(2:30),271) depth,istrike,idip,irake,nsynt
 271  format('H',f5.1,1x,'SDR',i4,i3,i4,1x,'N',i4)

      chr_f_form$ = 'unformatted'


      if (codsyn.eq.'SB ')  
     *    call sei open( unknown$,                ! Open file.
     &                   ' ',                     ! No prompt.
     &                   'bousei.out',            ! On this file.
     &                   unit_out,                ! On this unit.
     &                   b_flag,                  ! File exists?
     &                   code )                   ! Returned condition.

      if (codsyn.eq.'SH ')  
     *    call sei open( unknown$,                ! Open file.
     &                   ' ',                     ! No prompt.
     &                   'hersei.out',            ! On this file.
     &                   unit_out,                ! On this unit.
     &                   b_flag,                  ! File exists?
     &                   code )                   ! Returned condition.
      if (codsyn.eq.'SW ')  
     *    call sei open( unknown$,                ! Open file.
     &                   ' ',                     ! No prompt.
     &                   'wkbjsei.out',           ! On this file.
     &                   unit_out,                ! On this unit.
     &                   b_flag,                  ! File exists?
     &                   code )                   ! Returned condition.
c
c   read channels and write out, first header
c
c
      write(6,'(1x,a)') mhead(1)(1:70)
      do i=1,12
         write(unit_out)mhead(i)
      enddo
c
c
c   write remaining main header lines if more than 30
c
         if(nchan.gt.30) then
            k=(nchan-31)/3+1
            do i=13,k+12
               write(unit_out) mhead(i)
            enddo
         endif

c
c---------------------------------
      do 2000 ichan=1,2*nchan,2
c---------------------------------
c
      kchan=ichan
      ilec=0
c
c      if transverse, skip the reading
c      if radial, read the EAST component after having read the NORTH
c
      if (comp(ichan)(4:4).eq.'T')  goto 61
  60  continue
      if (comp(ichan)(4:4).eq.'R'.and.ilec.eq.1) kchan=ichan+2
c
c   rewind output file from main program
c
      rewind 20
c
c   read one trace
c
      if(check.eq.1) write(6,*)'call seisin'
c
c   if the channel is there, then read it, else generate some data
c
      if(chan(kchan).ne.0) then
c        rewind wav_unit 
         call seisinc(wav_unit,chan(kchan),ncbid,2,mmhead,chead,0.0,0.0)
c
c   get sample rate and number of samples
c
         read(chead(37:43),'(f7.2)')rate
         read(chead(45:50),'(i6)') nsamp
         if(check.eq.1) write(6,*)'rate,nsamp',rate,nsamp
c
c   select window
c
         n1=tstart(kchan)*rate+1
         n2=(tstart(kchan)+cinter(kchan))*rate+1
         j=0
         do i=n1,n2
           j=j+1
           signal(j)=signal(i)
         enddo
         n=j
      else
         do i=1,1040
           chead(i:i)=' '
         enddo
         chead(1:5)=stat(kchan)
         chead(6:9)=comp(kchan)
         signal(1)=0
         n=1
         rate=50.0
      endif
c
      if (.not.radial.or.compd(kchan)(4:4).eq.'Z')  then
        cheadsave=chead
        goto 70
      endif
c
c
      if (radial.and.compd(kchan)(4:4).eq.'N')   then
        do i=1,n
          signaln(i)=signal(i)
        enddo
        cheadsave=chead
        ilec=1
        goto 60
      endif
      if (radial.and.compd(kchan)(4:4).eq.'E')   then
c           verify that the two components have compatible parameters
        if(chead(1:5).ne.cheadsave(1:5).or.
     *      chead(10:77).ne.cheadsave(10:77))    then
          write (*,'(a)') 'incompatible headers for horizontal compo:'
          write (*,'(a)') cheadsave(1:77)
          write (*,'(a)') chead(1:77)
          stop
        endif
c           verify that the transfer fonctions are similar at 5%
          read (cheadsave(241:249),'(g8.3)')  ampln
          read (chead(241:249),'(g8.3)')  ample
              write (*,'(a,a,2g8.3)')
     *        'transfer functions at ',stat(ichan),ampln,ample
          if(ampln.eq.0..and.ample.eq.0.)   goto 65
          dampl=2.*(ampln-ample)/(ampln+ample)
          if (dampl.gt.0.05) write (*,'(a,a,2g8.3)')
     * 'transfer functions quite different at ',stat(ichan),ampln,ample
 65       continue
        do i=1,n
          signale(i)=signal(i)
        enddo
      endif
c   put to zero the part of the header concerning instrument
      do i=81,1040
        cheadsave(i:i)=' '
      enddo
c
c    reading the backazimuth
c
      do i=1,ndata
        if(stat(ichan).eq.data(i)(17:21).and.
     *      data(i)(47:55).eq.' BAZIMUT:'.and.data(i)(80:80).eq.'3')
     *      read(data(i)(56:65),'(f10.1)') backazi
      enddo
      caz = -cos(backazi*3.14159/180.)
      saz = -sin(backazi*3.14159/180.)
c
c       making the radial component (away from the source)
c
      do i=1,n
        signal(i) = caz*signaln(i) + saz*signale(i)
      enddo
      goto 62
 61   continue
c
c       making the transversal component
c
      do i=1,n
        signal(i) = -saz*signaln(i) + caz*signale(i)
      enddo
 62   continue
c
c  modify channel header for new start time, which should be origin time, same
c  as mainhead, or, if a delay, unchanged in time relative to original
c  main header.
c  write the data elements in cheadsave, because chead is used to write
c  the elements related to the synthetics, and we have to write
c  alternatively data R, synthetics R, data E and synthetics E
c  (therefore some mix-up occurs if one uses chead in all cases)
c
 70   continue
      if(delay(ichan).eq.0.0) then
        cheadsave(10:35)=mhead(1)(34:59)
      else
        cheadsave(10:35)=mmhead(1)(34:59)
      endif
      write(cheadsave(45:50),'(i6)') n
      write(cheadsave(37:43),'(f7.2)') rate
      cheadsave(6:9)=comp(ichan)
c   write down the header for the data
      write(unit_out) cheadsave
      write(6,'(1x,a)') cheadsave(1:70)
c
c   convert to integers
c
      if(cheadsave(77:77).eq.'4') then
         do i=1,n
           isignal(i)=signal(i)
         enddo
         write(unit_out) (isignal(i),i=1,n)
      else
         do i=1,n
            isignal2(i)=signal(i)
         enddo
         write(unit_out) (isignal2(i),i=1,n)
      endif
c
c   find synthetics in output file, only check that component is ok,
c   assume that stations are in correct order

c
 700  continue
      read(20,'(a)',end=701) text
        if(text(1:5).eq.stat(ichan).and.
     *     text(9:9).eq.comp(ichan+1)(4:4)) then
           read(20,*) (signal(i),i=1,nsynt)
        else
           goto 700
        endif
      goto 702
  701 continue
      write(6,'(1x,a5,a4,a)') stat(ichan),comp(ichan+1),
     *              ' Not found in synthetic file, will zero'
      do i=1,nsynt
        signal(i)=0
      enddo
  702 continue
c
c   write synthetic channel
c
      chead(6:9)=comp(ichan+1)
      syntrate=1./dtt
      write(chead(37:43),'(f7.2)') syntrate
      do i=81,1040
        chead(i:i)=' '
      enddo
c
c   indicate 4 byte integers
c
      chead(77:77)='4'
c
c  calculate header time
c
      stime=otime+delay(ichan+1)
      call sectim(stime,syear,sdoy,smonth,sday,shour,smin,ssec)
      write(chead(10:35),'(i3,1x,i3,1x,4(i2,1x),f6.3)')
     *               syear-1900,sdoy,smonth,sday,shour,smin,ssec
c
c   shift synthetic signal according to delay
c
      n1=tstart(ichan+1)*syntrate+1
      n2=(tstart(ichan+1)+cinter(ichan+1))*syntrate+1
      j=0
      do i=n1,n2
        j=j+1
        isignal(j)=signal(i)
      enddo
      ns=j
      write(chead(45:50),'(i6)') ns
c
c  write header and data
c
      write(unit_out) chead
      write(6,'(1x,a)') chead(1:78)
      write(unit_out)(isignal(i),i=1,ns)
      if(check.eq.1) write(6,*)
     *' rate,tstart,ns,nsynt',syntrate,tstart(ichan+1),ns,nsynt
	  if(check.eq.1) write(6,*)(signal(i),i=1,10)
 2000 continue
c
c   close files
c
      if(exist)call sei close( close$, wav_unit, code ) ! Close (Default stop on error).
c     call sei close( close$, unit_out, code ) ! Close (Default stop on error).

      return
      end
c
c----------------------------------------------------------------------------
