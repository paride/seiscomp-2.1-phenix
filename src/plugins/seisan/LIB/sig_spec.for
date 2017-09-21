c   Signal and spectral processing routines. This colllection of routines were
c   put together january, 2001 by gneralizing earlier routines belonging to
c   mulplt, spec and qlg


c     spec_select: select a time window of digitial data acording to given
c                  phase etc
c
c     smooth     : smoothing of a signal, usully a spectrum
c
c     remove_resp: correct a time signal for response
c
c     prepare    : prepare signal for spectral analysis, taper
c
c     spectrum   : make the complex spectrum
c
c     precoh1    : predicted coherence method for 3-comp analysis
c
c     cross      : corss correlaiton
c
c     rotate_comp: component rotation
c
c     spec_values: get corrected spectral levels at given frequencies
c
c     check_clipped: check if signal is clipped
c
c
c
c  updates:
c
c 01.02.2001 lo : change in spec_value, dont modify input signal
c 11.03.2001 lo : add ipow to the arguments of spev_values
c 22.06.2001 lo : free dimension of buff in smooth
c
c-----------------------------------------------------------------------------

      subroutine spec_select(data,nhead,nrecord,rotate,
     *           station,component,
     *           sel_crit,start,tstart,t0,nsamp,error,
     *           err_text)           
c
c                                                                               
c                                                                               
c   selects data for spectral analysis. Given a station and compoenent,
c   the chennel is selected from the waveform file and the time of the first
c   sample to use is calculated from start criterias and the header
c   time in the waveform file
c   the routine assumes that the general waveform reading routines are used
c
c   input:  data     : nordic
c           nhead    : number of headers
c           nrecord  : number of records
c           rotate   : rotate or not
c           station  : station to use
c           component: Component to use
c           nhead    : number of headers in Noridc file
c           nrecord  : number of records in --------
c           sel_crit: 1: ptime, 2: stime, 3: S from P time, 4: abs start
c           start:    start time from origin time in units of                   
c                     p or s-travel times, if sel_crit is 4, in secs                     
c
c   output:                            
c           tstart:   time of first sample in window relative to start          
c                     of waveform data file data trace       
c           t0:       time of origin relative to first sample in wav. file
c                     data trace
c           nsamp:    number of points in selected trace                                 
c           rate:     samples pr sec                                            
c           error:    number of errors found, should be zero                    
c           err_text  text of error
c                                                                               
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      character*80 data(1)
      integer nhead,nrecord
      logical rotate
      character*80 err_text
c-- trace file name                                
      character*80 trace_file
      integer nfile
      integer sel_crit
      real baz(max_trace)  ! back azimuth angles each trace
c-- station code                                  
      character*5  station,sta
      integer plotoption
c-- component                                           
      character*4  component			
c-- phase                                               
      character*1  phasex		
c-- see above                             
      real start,tstart    
c-- start of window relative origin time                                            
      real win_start
c-- number of channels in w-file          
      integer nchan                     
c-- number of samples in one channel      
      integer nsamp                     
c-- 1: test output, 0: no output                            
      integer check			
c-- origin date                                  
      integer oyear,omonth,oday		
c-- origin time                                         
      integer ohour,omin		
c-- origin time                                              
      real    osec			
c-- origin time relative time of first sample
      real t0
c-- phase time                                               
      integer phahour,phamin		
c-- -----                                 
      real    phasec                      
c-- abs times                            
      double precision phatime,wtime,otime	
c-- error variables                                     
      integer error		
c-- channel number to select                                 
      integer chan		
c-- directory separater char
      character*1 dchar	
c-- help variables                                
      integer	i,k,seiclen
      error=0                                                                   
      check=0
      call dir_char(dchar)                                                                   
c                                                                               
c   read origin time                                           
c                                                                               
      read(data(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)')                               
     *oyear,omonth,oday,ohour,omin,osec                                         
c                                                                               
c   get absolute origin time in secs                                            
c                                                                               
      call timsec(oyear,omonth,oday,ohour,omin,osec,otime)                      
c                                                                               
c   find station and  p-phase if so required                                                
c                                              
      if(sel_crit.eq.1.or.sel_crit.eq.3) then                                 
         do i=nhead+1,nrecord
            read(data(i),'(1x,a5,4x,a1)') sta,phasex                                                              
            if(sta.eq.station.and.phasex.eq.'P') go to  6                           
         enddo           
      endif                                                     
c                                                                               
c   find station and s-phase if so required                                                
c                                              
      if(sel_crit.eq.2) then                                 
         do i=nhead+1,nrecord
            read(data(i),'(1x,a5,4x,a1)') sta,phasex                                                              
            if(sta.eq.station.and.phasex.eq.'S') go to  6                           
         enddo                                                                
      endif
c                                                                               
c   find only station if abs time is used                                      
c                                              
      if(sel_crit.eq.4) then                                 
         do i=nhead+1,nrecord
            read(data(i),'(1x,a5,4x,a1)') sta,phasex                           
            if(sta.eq.station) go to  6                           
         enddo
c
c   no station was found, can still continue if station in waveform file
c
           goto 7                                                                
      endif
c
c  if here, data not found, write error message
c
      if(sel_crit.eq.1.or.sel_crit.eq.3)
     *write(err_text,200) station,data(1)(1:20)
 200  format(' P for station ',a5,' not found for event ',a20)                  
      if(sel_crit.eq.2) write(err_text,210) station,data(1)(1:20)                                      
 210  format(' S for station ',a5,' not found for event ',a20)                  
      if(sel_crit.eq.4) write(err_text,220) station,data(1)(1:20)                                      
 220  format(' Station ',a5,' not found for event ',a20)                  
      error=error+1                                                             
      return                                                                    
c                                                                               
c   station or (station and phase) found, now read it                                      
c                                                                               
 6    continue                                                                  
      read(data(i),'(1x,a5,4x,a1,7x,2i2,f6.1)')                                
     *sta,phasex,phahour,phamin,phasec
c
 7    continue
c
c   calculate time from origin to start of data window
c
      if(sel_crit.eq.4) then              ! abs time not related to any phase
         win_start=start
      endif
c
      if(sel_crit.eq.1.or.sel_crit.eq.2.or.sel_crit.eq.3) then
c                                                                               
c   calculate abs phase time                                                         
c                                                                               
         call timsec(oyear,omonth,oday,phahour,phamin,phasec,phatime)
c                                                                               
c   calculate start from origin                                         
c      
         phatime=phatime-otime
         if(sel_crit.eq.3) then           ! calculate s time from p time                                                                         
            phatime=phatime*1.78                                
         endif
c
c   phatime is now start time of window from origin time, now multiply
c   by factor to give number of start times or just move it a bit
c
         win_start=phatime*start            
      endif
c                                                                               
c   get trace data file names                                                  
c               
      call auto_tr(data,nhead,nfile,wav_filename)
c
c   check if ifles exist and where
c
       k=0
       do i=1,nfile
          call  get_full_wav_name(wav_filename(i),trace_file)
          if(trace_file.eq.' ') then
              write(6,'(1x,a,a)') ' No such file: ',
     *        wav_filename(i)(1:seiclen(wav_filename(i)))
         else
            k=k+1
            wav_filename(k)=trace_file
         endif
       enddo
       nfile=k
c     write(6,*) nfile
c
c   check if any name given
c
      if(nfile.eq.0) then
         write(err_text,'(a,a)')
     * ' No waveform file given for event', data(1)(1:20)
         error=error+1
         return
      endif
c                                                                               
c   initialize waveform file info
c
      call wav_init

c          
c   read all header information from all files
c          
       wav_nfiles=k
       do i=1,wav_nfiles
          call read_wav_header(i)
       enddo
       write(6,'(a,a)')' Read headers from files:'
       do i=1,wav_nfiles
          write(6,'(1x,a)') wav_filename(i)(1:80)
       enddo

c                                                                               
c   find channel number corresponding to station and                            
c   component, also get header time                                
c            
      call wav_find_chan(station,component,chan)
         if(chan.eq.0) then
         error=error+1                                                          
         write(err_text,'(a,a,a)')
     *   ' Station and component not found in trace file  ',
     *   station,component
         return
      endif                                                                     
c
c   if rotate, find back azimuths, read and rotate
c   else just read channel
c
      wav_out_start(1)=wav_delay(chan)   ! normally no delay
      if(rotate) then
          call get_baz(wav_nchan,wav_stat,data,nhead,nrecord,baz)
          call wav_read_2channel(chan)  ! read 2 channels
          nsamp=wav_out_duration(1)*wav_rate(chan)+1
          if(wav_comp(chan)(4:4).eq.'N') wav_rot_comp(chan)='R'
          if(wav_comp(chan)(4:4).eq.'E') wav_rot_comp(chan)='T'
          call rotate_comp(nsamp,
     *    wav_out_first_sample(wav_current_chan(2)),
     *    wav_out_first_sample(wav_current_chan(3)),
     *    wav_rot_comp(chan),
     *    baz(chan),signal1,signal2,signal3)  ! rotated signal now in signal1
      else
          call wav_read_channel(chan)
          nsamp=wav_nsamp(chan)
      endif
c

c                                                                               
c  get abs trace file start time                                                
c                                    
       wtime=wav_abs_time(chan)+wav_out_start(1)-wav_delay(chan)
c                                                                               
c   calculate start time for window relative to first                      
c   sample in trace data                                                        
c                                                                               
      tstart=win_start-(wtime-otime)                                           
c                                                                               
c   extract origin time t0 relative to time of first sample                     
c                                                                               
      t0=otime-wtime                          
c
c   if abs time, correct
c
      if(sel_crit.eq.4) then
         tstart=start
         t0=0.0
      endif
c                                                                               
c   close trace data file                                                       
c                                                                               
c                                                                               
      return                                                                    
      end                                                                       
                                                                                
                                                                                
                                                                                
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
C                                                                               
      SUBROUTINE SMOOTH(BUFF,JJ,ISMO)                                           
C                                                                               
C     BUFF  = BUFFER TO BE SMOOTHED --  DIMENSION : BUFF(JJ)                    
C     ISMO  = NUMBER OF PASSES                                                  
C                                                                               
c      DIMENSION BUFF(1)                                                         
      DIMENSION BUFF(*)                                                         
C                                                                               
      IF(ISMO.LE.0)     RETURN                                                  
C                                                                               
      DO 50  IS=1,ISMO                                                          
C                                                                               
      J1=JJ-1                                                                   
      A=.54*BUFF(1)+.46*BUFF(2)                                                 
      B=.54*BUFF(JJ)+.46*BUFF(J1)                                               
      SJ=BUFF(1)                                                                
      SK=BUFF(2)                                                                
C                                                                               
      DO 10  J=2,J1                                                             
      SI=SJ                                                                     
      SJ=SK                                                                     
      SK=BUFF(J+1)                                                              
   10 BUFF(J)=.54*SJ+.23*(SI+SK)                                                
C                                                                               
      BUFF(1)=A                                                                 
      BUFF(JJ)=B                                                                
C                                                                               
   50 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       



      subroutine remove_resp(y,y_com,nsamp,rate,disp_vel,
     *f_low,f_high,pole_low,pole_high)
c                                                                               
c     Routine to remove instrument response from a trace. The response
c     informaiton is suppose to already have been read in and available
c     in the waveform common block, has to be checked outside this routine
c
c     The trace is restored to nm, nm/s, or nm/s*s
c
c     input:  y           : data vector
c             nsamp       : number of samples in data vector
c             rate        : sample rate
c             disp_vel    : displacement ,velocity or acceelration out (1-3)
c             f_low,_high _ filter limits
c             pole_low,pole_high: poles of filters
c     output  Y           : corrected signal
c             y_com       : complex equivalent, only there to use external
c                           memory definition
c                                                                               
c     Written by C. Lindholm Jan. -90, substantially changed by jh                                           
c
c-- common block         
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'

      real y(*)
c-- percentage of tapering (fixed)                       
      real taper/10./				
c-- length of input vector                                 
      integer nsamp				
c-- length after padding                                    
      integer ipow				
c-- vector padded with npad zeros                           
      integer npad				
c-- Fixed to 1 for system rem.                        
      integer rem_resp/1/			                       
c-- counters,etc                                               
      integer i,j
      real f_low,f_high            ! added filter to use
      integer pole_low,pole_high   ! -------------------
      real rate                    ! sample rate
      integer disp_vel             ! displacemnt, velocity or acceleration
      complex y_com(*)
      double precision ttime

      ttime=0.
c                                                                               
c------- prepare data.     Pad with zeros and taper 10%.                        
c                                                                               
      call prepare(nsamp,y,taper,ipow,npad,y_com)
c
c-------- Calculate the spectrum, and remove the system response
c         q, qzero, kappa and travel time set to zero 
c
      call spectrum(y_com,ipow,disp_vel,rem_resp,rate,
     +     f_low,f_high,pole_low,pole_high,0.0,0.0,
     *     0.0,ttime)
c                                                                               
c-------Do inverse FFT -------                                                  
c                                                                               
      j = ipow                                                                  
      do i = 1,20                                                               
        j = j/2                                                                 
          if (j .eq. 1)then                                                     
            j = i                                                               
            go to 222                                                           
          endif                                                                 
      enddo                                                                     
222   continue                                                                  
                                                                                
      call fft(j,1.,y_com)                                                        
c                                                                               
c------Normalize                     
c                                                                               
      do i=1,nsamp                                                               
        y(i) = (1./real(ipow)) * real(y_com(i))                                   
      enddo                                      
      return
      end                                                                       
                                                                                
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc           
      subroutine prepare(nsamp,y,taper,ipow,npad,y_com)                        
c                                                                               
c     Subroutine to pad data with zeros up to a number that is power of 2,      
c     and to taper the data.                                                    
c     Returned is the complex vector with the data prepared for FFT             
c                                                                               
c     Written by C. Lindholm Jan -90
c     jan 2001, jh: Put all input output in call
c                                                                               
c     input:   nsamp        Samples in data vector
c              y            Data vector
c     output:  npad         Number of zeros added                               
c              ipow         Length of new data vector
c              y_com        Complex data vector
c
      implicit none
c-- percentage of tapering                                    
      real taper				
      real pi/3.1415972/
      real y(*)
      complex y_com(*)
c-- dummy variable                                             
      real arg					
c-- number of samples to taper                              
      integer ntap				
      integer nsamp                                                             
c-- max = 2**20                                             
      integer ipow				
      integer npad                                                              
c-- counters                                               
      integer i				
c-- used in dc-removal                                    
      integer ndc
      real dc				
                                                                                
c                                                                               
c---------- Remove dc ------                                                    
c                                                                               
      ndc = 0                                                                   
      call remove_dc(y,ndc,dc,nsamp)                                         
c                                                                               
c-------Taper if wanted ------                                                  
c                                                                               
      if (taper .gt. 0.0) then                                                  
        ntap = (nsamp/2) * (taper/100)                                          
        arg = pi / 2 / ntap                                                     
        do i = 1,ntap                                                           
         y(i) = y(i) * sin((i-1) * arg)                                   
         y(nsamp-i+1) = y(nsamp-i+1) * sin((i-1) * arg)                   
        enddo                                                                   
      endif                                                                     
c                                                                               
c-------Now pad with zeros -------                                              
c                                                                               
      ipow = 1                                                                  
      do i = 1,20				                                                           
          ipow = ipow*2                                                            
       if(ipow .ge. nsamp) go to 10                                             
      enddo                                                                     
10    continue                                                                  
                                                                                
      npad = ipow - nsamp                                                       
c                                                                               
c-------fill complex vector                                                     
c                                                                               
      do i = 1,ipow                                                             
        if(i .le. nsamp)then                                                    
          y_com(i) = cmplx(y(i),0.0)                                           
        else                                                                    
          y_com(i) = cmplx(0.0,0.0)                                               
          y(i) = 0.0                                                         
        endif                                                                   
      enddo                                                                     
                                                                                
      return                                                                    
      end                                                                       
                                                                                


      subroutine spectrum(y_com,ipow,disp_vel,rem_resp,srate,
     +     f_low,f_high,pole_low,pole_high,q0,qalpha,
     *     kappa,travel_time)
c
c     Routine to do the FFT and to remove instrument response
c     from the data. 
c
c     Written by C. Lindholm Jan. -90
c
c     updates:
c     oct 19, 90 by j.h. : fix all kinds of spectra
c     nov 1   90    j.h. : add q correction
c     jul 93        jh   : remove assumption that response cureves for
c                          accellerometers is g, now assume m
c     jan 2001      jh   : all data through window, no reference to mulplt.inc
c
c     input:      y_com       Complex vector with data in real part
c                 ipow        Length of complex vector
c                 disp_vel    response: 1 = displacement
c                                       2 = velocity
c                                       3 = acceleration
c                 rem_resp    Switch to remove or not remove response.
c                                       1 = remove response
c                 srate       Sampling rate
c                 f_low,f_high,pole_low,pole_high: frequecy and poles for filter
c                 q,qalpha  : Q
c                 travel_time: travel time needed for q-correction, to start
c                             window
c                 kappa     : near surface term
c        
c     
c     output:     y_com
c                 ipow
c
      implicit none
      complex y_com(*)                          ! data vector
      real f_low,f_high                         ! filter band
      integer pole_low,pole_high                ! filter poles
      real q0,qalpha                            ! q
      integer disp_vel                          ! kind of spectrum
c      real travel_time   ! before 10 May, 2001
      double precision travel_time              ! changed lo, 10 may 2001
      real kappa                                ! kappa
      complex hs                                ! help variable
      complex respons				! complex respons
      real freq					! frequency corresp. to spectrum
      real srate
      real delta				! 1. / srate
      real xx					! dummy variabel
      real pi/3.1415972/
      integer ipow
      integer iresp				! power factor for response type
      integer rem_resp
      integer i,inv,j				! counter
      integer spstart,spstop			! bounds for spectr. counter

      delta = 1. / srate

c
c   it is assumed that all response curves are in displacement, also
c   for accelerometers
c
         iresp=disp_vel-1               

c
c------- Do FFT -------
c
      j = ipow
      do i = 1,20
        j = j/2
          if (j .eq. 1)then
            j = i
            go to 222
          endif
      enddo
222   continue
      call fft(j,-1.,y_com)
c
c--------Remove response------
c
      spstart = 2
      spstop = ((ipow/2) + 1)

      if(rem_resp .eq. 1) then
        xx = ipow*delta
        do i = spstart,spstop
         freq = (i-1)/xx
c
c-------Butterworth filter ---------------------
c
         if((f_low.gt.0.0.and.pole_low. gt.0).or.(
     *      f_high.gt.0.0.and.pole_high.gt.0)) then
               if(f_low.gt.0.0.and.pole_low.gt.0) then
                  call bworth(freq,f_low,-pole_low,hs) 
                  y_com(i) = y_com(i) * hs
               endif
               if(f_high.gt.0.and.pole_high.gt.0) then
                   call bworth(freq,f_high,pole_high,hs) 
                   y_com(i) = y_com(i) * hs
               endif
               inv = ipow - i + 2
                y_com(inv) = conjg (y_com(i))
         endif
c
c--------Q--------------------------------------
c
         if(q0.gt.0) then
           y_com(i)=y_com(i)*exp(pi*freq*travel_time/(q0*freq**qalpha))
         endif
c
c   correct for kappa
c
          if(kappa.gt.0) then
             y_com(i)=y_com(i)*exp(pi*freq*kappa)
          endif

c
c-------- Calculate the response, it is assumed that the response parameters
c         for the current channel has already been read in and available
c         in resonse common block
c
           call calc_resp(freq,respons)
c
c-------Now correct spectrum -----------
c
           hs=(0,1)*(2*pi*freq)
           y_com(i) = (y_com(i) / respons ) *hs**iresp
           inv = ipow - i + 2
           y_com(inv) = conjg (y_com(i))
        enddo				! end of loop
      endif				! end of response removed

      return
      end




      SUBROUTINE PRECOH1(Z,MPTS,IPTS,LPTS,PVEL,COHER,
     *AZIM,VELO,RMSAMP)
      REAL Z(MPTS,3)
      real dc(3)
C
C  S/R TO DO 3-COMPONENT AZIMUTH AND VELOCITY ESTIMATION FOR A P-PHASE
C  FOR A FIXED USER SPECIFIED TIME WINDOW.
C  METHOD: PREDICTED COHERENCE (R. ROBERTS, UPPSALA)
c 
c  jan 94 add remove dc
C
C  INPUT:
C     Z(MPTS,3) - DATA MATRIX
C                 Z(*,1) - VERTICAL CHANNEL (POSITIVE DIRECTION UP)
C                 Z(*,2) - NORTH CHANNEL
C                 Z(*,3) - EAST CHANNEL
C     MPTS      - USED FOR DIMENSIONING OF DATA MATRIX
C     IPTS      - FIRST SAMPLE IN TIME INTERVAL
C     LPTS      - LAST SAMPLE IN TIME INTERVAL
C     PVEL      - LOCAL P-WAVE VELOCITY AT SITE
C
C  OUTPUT:
C     COHER     - PREDICTED COHERENCE, SHOULD BE POSITIVE AND
C                 LARGER THAN ABOUT 0.1 FOR A P-PHASE
C     AZIM      - P-PHASE AZIMUTH (TOWARDS EVENT) IN DEGREES
C     VELO      - P-WAVE APPARENT VELOCITY IN KM/SEC
C     RMSAMP    - ROOT MEAN SQUARE AMPLITUDE
C
C  NOTE: IF COHER IS LESS THAN ABOUT 0.1 AZIMUTH AND VELOCITY ESTIMATE SHOULD
C        NOT BE USED   
C
      DATA RADDEG/57.2958/
      DATA PSRAT/1.73/   
c
c   remove dc
c
      do i=1,3
        dc(i)=0.0
      enddo
      do i=ipts,lpts
         do k=1,3
            dc(k)=dc(k)+z(i,k)
         enddo
      enddo
      do i=1,3
        dc(i)=dc(i)/(lpts-ipts+1)
        do k=ipts,lpts
          z(k,i)=z(k,i)-dc(i)
        enddo
      enddo
C
C  MODEL 8 - P WAVES ONLY
C  CALCULATE THE AUTO AND CROSS CORRELATIONS
C
      LENWIN=LPTS-IPTS+1
      CALL CROSS(Z(IPTS,2),Z(IPTS,2),LENWIN,XX)
      CALL CROSS(Z(IPTS,2),Z(IPTS,3),LENWIN,XY)
      CALL CROSS(Z(IPTS,2),Z(IPTS,1),LENWIN,XZ)
      CALL CROSS(Z(IPTS,3),Z(IPTS,3),LENWIN,YY)
      CALL CROSS(Z(IPTS,3),Z(IPTS,1),LENWIN,YZ)
      CALL CROSS(Z(IPTS,1),Z(IPTS,1),LENWIN,ZZ)
      YX=XY
      ZX=XZ
      ZY=YZ
      POWER=(XX+YY+ZZ)/LENWIN
      RMSAMP = SQRT(POWER)
C
C  CALCULATE AZIMUTH AND VERTICAL/RADIAL AMPLITUDE RATIO
C
      AZI=ATAN2(-YZ,-XZ)
      AZIM=AZI*RADDEG
      IF(AZIM.LT.0.) AZIM = AZIM + 360.
      ZOVERR=ZZ/SQRT(XZ*XZ+YZ*YZ)
      A=-ZOVERR*COS(AZI)
      B=-ZOVERR*SIN(AZI)
c     A=ZOVERR*COS(AZI)
c     B=ZOVERR*SIN(AZI)
c     write(6,*)'a,b,',a,b
C
C  CALCULATE PREDICTED COHERENCE
C
      ERR=0.
c     write(6,*) ipts,lpts
      DO 50 II=IPTS,LPTS
        CC=Z(II,1)-A*Z(II,2)-B*Z(II,3)
        ERR=ERR+CC*CC
 50   CONTINUE
      COHER=1.-ERR/ZZ
C
C  SIMPLE BIASED VELOCITY ESTIMATION (TO OBTAIN AN UNBIASED ESTIMATE
C  ONE NEED TO KNOW THE NOISE AMPLITUDE)
C
      SVEL=PVEL/PSRAT
      AI=ATAN(1./ZOVERR)
      VELO=SVEL/SIN(AI/2.)
C
      RETURN
      END


      SUBROUTINE CROSS(X,Y,L,A)
C-----------------------------------------------------------------------
C CALCULATES UNNORMALISED CROSS CORRELATION BETWEEN THE TWO
C REAL TIME SERIES X AND Y OF LENGTH L.
C THE RESULT IS RETURNED IN A.
C-----------------------------------------------------------------------
      DIMENSION X(L),Y(L)
      A=0.
      DO 1 I=1,L
        A=A+X(I)*Y(I)
 1    CONTINUE
      RETURN
      END


      subroutine rotate_comp(nsamp,start1,start2,
     *component,backazi,y1,y2,y3)
c
c   rotates the signal in y2(ns) and y3(ew), the result is put
c   variable y1. The output component must be specifed as T or R. Rotation
c   is done on nsamp samples starting from the first sample first1 and first2
c   in input arrays and starting with first sample in output array.
c   backazi is the backazimuth

      implicit none
      real y1(*),y2(*),y3(*)
      real backazi,caz,saz
      integer i,start1,start2,nsamp
      character*1 component
c
c   rotation parameters
c
      caz = -cos(backazi*3.14159/180.)
      saz = -sin(backazi*3.14159/180.)

      if(component.eq.'R') then
         do i=1,nsamp
            y1(i) = caz*y2(i+start1-1) + saz*y3(i+start2-1)  ! radial component
         enddo
      endif
c
      if(component.eq.'T') then
         do i=1,nsamp
           y1(i) = -saz*y2(i+start1-1) + caz*y3(i+start2-1)  ! transverse comp.
         enddo
      endif
c
      return
      end



      subroutine spec_value(current,sig,nsamp,nsmooth,f_low,
     &  f_high,disp_vel,farray,nfreq,write1,level,y_com,ipow)

c
c subroutine to take out spectral level at given frequencies
c lo 2000
c
c genralized by jh jan 2001
c 
c input:  current - current channel ID
c         sig     - input time signal
c         nsamp   - number of samples
c         f_low,f_high - frequency limits for spectrum
c         farray  - frequency value array
c         nfreq   - 
c         write1  - output unit, 0 if none
c
c output: level   - spectral amplitude levels, averaged around frequencies
c                   given in farray
c         y_com   - complex spectrum
c         ipow    - spectral values in y_com between 2 and (ipow/2+1)
c         
c changes
c
      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
      include 'waveform.inc'
 
      integer i,j,b,c         ! counters
      integer current         ! current trace number
      integer nsamp           ! number of samples in signal
      real sig(*)             ! the signal, time domain
      real y(max_sample)      ! the signal
      complex y_com(*)        ! complex spectrum
      real f_low,f_high       ! low and high limit for spactrum
      integer disp_vel
      integer nfreq           ! number of frequencies in farray
      integer n(max_sample)   ! counters
      real farray(*)          ! array with selected frequencies
      real level(*)           ! spectral level atfrequencies given in farray
      integer write1          ! output unit, 0 for no output

      integer npad            ! vector padded with npad zeros
      integer ipow            ! length after padding
      real ff                 ! frequency
      real taper/5./          ! percentage of tapering (fixed)
      integer rem_resp        ! 1 if remove
      integer nsmooth         ! number of times to smooth
      real fdelta             ! width for averaging of spectra around 
                              ! selected frequencies
      real window             ! time length of window
      real level_next_f(max_sample)  ! level at next f outside desired f window
      double precision ttime

      i=current
c      nsmooth=5
cjh      disp_vel=1
      rem_resp=1
      fdelta=0.12

      do j=1,nsamp ! lo keep original signal
        y(j)=sig(j)
      enddo

      window = nsamp / wav_rate(i)

c
c------- prepare data.     Pad with zeros and taper 10%.
c
      call prepare(nsamp,y,taper,ipow,npad,y_com)

c
c remove response and get spectrum, no q or kappa correction,
c no filters
c
      ttime=0.
      call spectrum(y_com,ipow,disp_vel,rem_resp,wav_rate(i),
     +          0.,0.,0,0,0.,0.,0.,ttime)

c
c   calculate real spectrum, limit to frequencies ffmin and ffmax
c
      c = 0
      do b = 2,((ipow/2) + 1)
       ff = (b-1)*wav_rate(i)/ipow
       if(ff .ge.f_low.and. ff .le. f_high) then
         c = c + 1
         y(c) = ((1/wav_rate(i))**2)*(y_com(b)*conjg(y_com(b)))
         y(c) = sqrt(y(c))         
       endif
      enddo
c
c   smooth
c
c      call smooth(y,c,nsmooth)

c
c now extract values from spectrum
c
      do j=1,nfreq
        level(j)=0.
        level_next_f(j)=-1.
        n(j)=0
      enddo

      c = 0
      do b = 2,((ipow/2) + 1)
       ff = (b-1)*wav_rate(i)/ipow
       if(ff .ge.f_low.and. ff .le. f_high)then
         c = c + 1
         if (write1.ne.0) write(write1,'(f10.3,1x,f15.7)') ff,y(c)
         do j=1,nfreq
           if (ff.ge.farray(j)-fdelta*ff.and.
     &       ff.le.farray(j)+fdelta*ff) then
c           if (ff.ge.farray(j)-sqrt(ff*fdelta*wav_rate(i)/ipow).and.
c     &       ff.le.farray(j)+sqrt(ff*fdelta*wav_rate(i)/ipow)) then
                level(j)=level(j)+y(c)
                n(j) = n(j) + 1
           elseif (ff.gt.farray(j)+fdelta*ff.and.level_next_f(j).eq.-1.) 
     &     then
             level_next_f(j) = y(c)
           endif
         enddo
       endif
      enddo
      if (write1.ne.0) write(write1,'(a1)') '>'

c
c average levels around center frequency
c
      do j=1,nfreq
        if (n(j).gt.0) then
          level(j)=level(j)/float(n(j))
c
c take level at next frequency
c
        else
          level(j)=level_next_f(j)
        endif
      enddo
  
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine check_clipped(x,nsamp,clipped)
c
c routine to check if signal is clipped, which is if the max amplitude is 
c reapeated more than 25 times
c
c input:  x       - time series
c         nsamp   - number of samples in x
c output: clipped - true if signal is clipped
c

      implicit none
      real x(*)                         ! data
      integer nsamp,i,max_cnt           ! number of samples, counters
      real max                          ! max value
      logical clipped                   ! flag to show that signal is clipped

      max=0.
      max_cnt=0.
      clipped=.false.
      do i=1,nsamp
        if (abs(x(i)).gt.max) then
          max=abs(x(i)) 
        endif
      enddo
      do i=1,nsamp
        if (abs(x(i)).gt.max*.99) then
          max_cnt=max_cnt+1
        endif
      enddo
c
c if the max amplitude appears several times, the signal is considered
c to be clipped
c

c
c changed to 10, 13 March 2001, lo
c
      if (max_cnt.ge.100) then
        clipped=.true.
      endif

      return
      end
