c   updates
c
c   jun 6 by jh : add mb filter
c   jul 25      : do not calculate moment etc if no distance
c   dec 28      : ******** version 5.0 ********************* (no change)
c                 put in new xy_plot routine
c   fef 95      : -------------------------- with axis text
c   mar 21      : bug in get_3_chan
CJAB(BGS)Apr95  : Give prompt text the prompt text colour in remove_resp, so 
CJAB(BGS)Apr95.   that it always shows.
c   apr 21 95 jh : ratation in get_3_chan
c   april 28  jh : az bug
c   jun 7        : only ask once for disp vel in multitrace mode
c   nov 17       : ffmin and ffmax as parameters from MULPLT.DEF
c   dec 95       : plot response
c   jan 23   96  : noise spectrum
c   jan 31       : spectral fitting
c   feb 14       : enter moment interactively
c   mar 20       : message if response from header
c   jul 25       : fix picking error if 3 comp
c   aug 2        : do not calculate hbypocentral distance, now in convert
c                  use seiget for spectral modeling
c   sep 24       : power spectrum
c   dec 96       : remove all reference to depth in calc moment
c   feb 97   97  : ms response
c   feb 19       : new message in resp and spec
c   feb 20       : remove disp_vel from call and spectrum input, put in
c                  mulplt.inc
c   sep 22       : limit wa response to 20 hz
c   oct          : spectral output
c   oct 29       : check if no zero lenght spectrum 
c   nov 30       : do not ask question if waveform out
c   jan 23 98    : do not save chead since now done in sys_resp
c   feb 17       : small changes to selection
c   feb 18       : fix spectral bug with p spectra, auto selection of velocity
c-----------------------------------------------------------------------------
c   sep 98 jh    : ----------- year 2000 check -----------------------------
c                  year 2000, 5 character station code
c   nov 4  jh    : linux logig
c   nov 5        : change c-routines having '_'
c   nov 26 jh    : change use of kappa, also kappa in spectrum
c   dec 18       : bug turning off permanenly response removel if one trace
c                  did not have a response file
c   march 17, 99 : bug in 3 comp
c
c   mar 24 bmt   : clear input in oneline
c   may 27 jh    : hardwire  petersen noise curves
c   may 2        : fix noise spectrum level                
c   jul 14       : put correct petersen curves
c   aug 23       : do not correct for q and kappa when making noise spectra
c   sep 19       : add routien spec_dist, change geometrical spreading calcula
c   sep 23       : add geo_distance
c   sep 27       : put kappa correction back in, where did it go ????
c   nov 9 99 jh  : do not calculate moment if not displacment spec
c                  clean up display a bit
c   nov 26       : add date to response plot, shift a bit dec 6
c   dec 08 lo    : in get_3_chan read more then 30 stations from header
c   mar 30 00 jh : fix text for plotting response file
c   oct 26       : frequency limits in spectral plotting is determined by
c                  filter used, overdides defualts
c   dec  14      : scale power noise spectra after peterson curves
c   dec  17      : choice of lin x-axis in plot, chose fmin, fmax, t*
c   jan 2001     : remove mulplt.inc from prepare, spectrum, rem_resp
c                  remove prpare, remove_resp, pectrum precoh1,cross
c                  and rotate_comp form this file
c   feb 2001 bgs : bgs added choise of axis types and what else ?
c                  t* disabled again
c   mar 28     lo: seesm that lars corrected the power spectrum
c   may 27     jh: make log-log the defualt spectrum again
c   june  10   jh: some correction to above
c   june 22    lo: set dimension of nnspec in amp_spec to 3
c   jul 10     jh: add output of modeled spectrum in spectral_fit.out
c   sep 10       : add selection of type of spectrum
c   sep 21       : more spectrum selection fixing
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc             
      subroutine amp_spec                                                 
c                                                                               
c     routine to make a real spectrum                
c     The tapering of  the data is fixed to 10%                                 
c                                                                               
c     First written by C. Lindholm Jan. -90  Jh june 93                                         
c
      implicit none                                                                               
c                                                                                
c-- common block     
c                                
      include 'mulplt.inc'			
      include 'libsei.inc'
      include 'seiplot.inc'
      include 'seisan.inc'
c
c-- percentage of tapering (fixed)                       
      real taper/10./				
c-- length of input vector                                 
      integer nsamp				
c-- length after padding                                    
      integer ipow				
c-- vector padded with npad zeros                           
      integer npad				
c-- indicator for response removal or not (1 or 0).                        
      integer rem_resp			
      real ff ! frequency
c-- number of points in spectrum
      integer nspec
      real f_low,f_high
      integer pole_low,pole_high
c-- if several data sets, number of points in each
c      integer nnspec(2)
      integer nnspec(3)   ! lo, 22 June 2001
      integer xy_cursor             ! for call to xy_plot
      real xlength                  ! x-axis length for spectrum
      real save_q0,save_kappa       ! temporary storage
c-- counters and help variables                                               
      integer i,j
      real aa,bb,corr,rms           ! output from least squares
      real tstar                    ! tstar atteneuation, same as kk
      character*80 text, text1
      character*30 xtext,ytext      ! axis text
      character*1 ch                ! retuern from spectral type select
      character*1 ch1               ! return from axes type select (bjb 2001/02/22)
      real xx,yy                    ! help variables
      real amp_average              ! average log amplitude spectra
      real norm_factor              ! normalization factor for source fit
      real qq,qa,kk,f0              ! source test parameters:Q0, qalpha,diminu
                                    ! -ation parameter, cornor frequency
      integer code                  ! return status code
c--returns from spectral plot
      character*1 c_spec(20)
      character*80 message(10)      ! text to screen
      character*80 blank(10)        ! blank text strings
      real average_moment           ! moment read from data
      real ssdrop                   ! sress drop calculated in spectral fitting
      real sei real num             ! function
      real x0                       ! left corner of spectrum
c-- origin time
      integer oyear,omonth,oday,ohour,omin
      real osec
      double precision otime
      real cx(20),cy(20)
      integer nc,k
      real geo_factor     ! geometrical spreading factor
      real x_work(max_sample)
      logical power    ! if true, make power spectrum
      logical linx     ! if true, linear x-axis in spec
c-- axis type variable for spectra
c-- 1=Log-Linear
c-- 2=Log-Log
c-- 3=Linear-Linear
      integer iaxes_type
      character*80 answer
      logical sun,pc,linux
      real low_f(22),high_f(14),low_l(22),high_l(14) ! petersen noise
      equivalence(x_work,com)
c
c   petersen noise curves, log f and db
c
      data high_f/   1.000000,    6.575773E-01,    4.948500E-01,
     *           9.691001E-02,   -5.797836E-01,   -6.627579E-01,
     *           -7.993407E-01,  -8.976271E-01,   -1.187521,
     *           -1.301030,       -2.549984,       -4.000000,
     *            -4.477121, -5.0/
      data high_l/ -91.5,-97.4,-110.5,-120.0,-97.9,-96.5,-101.0,
     *             -113.5,-120.0,-138.5,-126.0,-80.1,-65.0,-48.5/ 

      data low_f/    1.000000,    7.695511E-01,    3.979400E-01,
     *             9.691001E-02, -9.342168E-02,   -3.802112E-01,
     *            -6.334685E-01,  -6.989700E-01,  -7.781512E-01,
     *             -1.000000,       -1.079181,       -1.193125,
     *              -1.340444, -1.50, -1.65, -1.89, -2.0,
     *              -2.19,-2.51,-2.78,-4.0,-5.0/
      data low_l/   -168.0,-166.7,-166.7,-169.2,-163.7,-148.6,
     *              -141.1,-141.1,-149.0,-163.8,-166.2,-162.1,
     *              -177.5,-185.0,-187.5,-187.5,-185.0,-184.9,
     *              -187.5,-184.4,-151.9,-103.1/ 
c
      do i=1,10
        blank(i)=' '
      enddo
      linx=.false.      ! normally log x axis
      call computer_type(sun,pc,linux)
c
c   check if enough points
c
      if((last-first).lt.20) then
         text=' Window too short for transformation, nothing done'
         call xmessage(text,1,80,10.0,60.0)
         return
      endif
                 
      average_moment=0.0
c-- indicator for type of spectrum fixed to 1 for displacement                 
      disp_vel=1			

      iaxes_type=2  ! defualt log-log axis
c
c   display general parameters
c
 400  continue
c
c   find if p or s spectrum
c
      if(spec_phase.eq.'P') spec_velocity=pvelocity
      if(spec_phase.eq.'S'.or.spec_phase.eq.'N') spec_velocity=svelocity
      if(spec_phase.eq.'N') spec_phase='?'  ! unknown spectrum
c
      message(1)='General parameters, '//spec_phase//'-spectrum'
      write(message(2),'(a,f5.1,a,f5.2,a,i5)')  'Vel ',spec_velocity,
     *' Dens',density, ' Dist',int(sdistance)
      write(message(3),'(a,i5,a,f5.2,a,f5.3)')
     *'Q0  ',int(q0),' Qalp',
     *   qalpha,' k   ',kappa
      call xmessage(message,3,30,620.0,100.0)
    
      
c
c   first check if response is to be removed and if available
c
      rem_resp=0
      message(1)='Displacement:    D'
      message(2)='Velocity:        V'
      message(3)='Acceleration:    A'
      message(4)='Raw spectrum:    R'
      message(5)='Change velocity: C'
      message(6)='Change moment:   M'
      message(7)='Noise Pow. spec: N'
      message(8)='Lin axis:        L'
      message(9)='New spec f-lim.. F'
      message(10)='Change spectrum  S'
      call xmessage(message,10,40,620.0,180.0)
c
c   get answer
c
      call xscursr(i,xx,yy)
      ch=char(i)
      if(ch.eq.'c'.or.ch.eq.'C') then
         call xmessage(blank,10,40,620.0,180.0)
	   text = '                    '
         call oneline('New velocity',12,text,20,620.0,300.0)
         read(text,'(f8.2)') spec_velocity
         if(sun.or.linux) call xscursr(i,xx,yy)  ! on sun buffer not cleared, why ?
         goto 400   ! start again
      endif

      if(ch.eq.'f'.or.ch.eq.'F') then
         call xmessage(blank,10,40,620.0,180.0)
	   text = '                    '
         call oneline('Spec fmin, fmax',15,text,20,620.0,300.0)
         call sei get values(2,text,code)
         ffmin=array$(1)
         ffmax=array$(2)
         if(sun.or.linux) call xscursr(i,xx,yy)  ! on sun buffer not cleared, why ?
         goto 400   ! start again
      endif

c commented following option for linear axes bjb 22/02/2001
c      if(ch.eq.'l'.or.ch.eq.'L') then
c          linx=.true.   ! linar x-axis
c         call xmessage(blank,9,40,620.0,180.0)
c           text = '                    '
c         call oneline('New velocity',12,text,20,620.0,350.0)
c         read(text,'(f8.2)') spec_velocity
c         if(sun.or.linux) call xscursr(i,xx,yy)  ! on sun buffer not cleared, why ?
c         goto 400   ! start again
c      endif

       if(ch.eq.'l') then
c
c Ask for axes type log-log or log-linear or linear-linear (bjb 01/03/2000)
c
           message(1)='Log(x)-Linear(y)    1'
           message(2)='Log(x)-Log(y)       2'
           message(3)='Linear(x)-Linear(y) 3'
           message(4)='Linear(x)-Log(y)    4'
     
           call xmessage(message,4,30,800.0,275.0)
c
c   get answer
c
           call xscursr(i,xx,yy)
           ch1=char(i)
           call xmessage(blank,3,30,800.0,275.0)

           iaxes_type=1
           if(ch1.eq.'1') iaxes_type=1 
           if(ch1.eq.'2') iaxes_type=2
           if(ch1.eq.'3') iaxes_type=3
           if(ch1.eq.'4') iaxes_type=4
           goto 400
      endif

      if(ch.eq.'m'.or.ch.eq.'M') then
         call xmessage(blank,10,40,620.0,180.0)
	   text = '                    '
         call oneline('New moment',12,text,20,620.0,300.0)
         read(text,'(f8.2)') average_moment
         goto 400   ! start again
      endif
c
      if(ch.eq.'s'.or.ch.eq.'S') then
 7654    continue
         call xmessage(blank,10,40,620.0,180.0)
	   text = '                    '
         call oneline('Enter p or s for type',21,text,5,620.0,300.0)
         spec_phase=text(1:1)
         if(spec_phase.eq.'p') spec_phase='P'
         if(spec_phase.eq.'s') spec_phase='S'
         if(spec_phase.ne.'P'.and.spec_phase.ne.'S') goto 7654
         goto 400   ! start again
      endif
c
c   final clear before going on
c
      call xmessage(blank,10,40,620.0,180.0)
 
         
      disp_vel=0
      power=.false.
      if(ch.eq.'A'.or.ch.eq.'a') disp_vel=3
      if(ch.eq.'V'.or.ch.eq.'v') disp_vel=2
      if(ch.eq.'D'.or.ch.eq.'d') disp_vel=1
      if(ch.eq.'D'.or.ch.eq.'V'.or.ch.eq.'A') power=.true.
      if(ch.eq.'n'.or.ch.eq.'N') then
        power=.true.
        disp_vel=3
        ch='N'
      endif
      if(disp_vel.gt.0) rem_resp=1
      if(rem_resp.eq.1) then
         wav_resp_file = ' '
         call read_resp
         if(wav_resp_status(1:1).eq.'9') then
            write(text(1:20),'(a)')'No response info ***'
            call tchars(text,20,620.0,225.0)
            rem_resp=0
         endif
         if(wav_resp_status.eq.'8') then
            write(text(1:33),'(a)')'Response from waweform header ***'
            call tchars(text,33,620.0,320.0)
         endif
      endif
c
c   check if velocity given for displacment spectrum
c
      if(disp_vel.eq.1.and.spec_phase.eq.'?') then
 9765        continue
             text='Spectrum not defined, enter p or s'
             call oneline(text,35,answer,2,500.0,500.0)
             spec_phase=answer(1:1)
             if(spec_phase.eq.'p') spec_phase='P'
             if(spec_phase.eq.'s') spec_phase='S'
             if(spec_phase.ne.'S'.and.spec_phase.ne.'P') goto 9765
      endif
c                                                                               
c--------put part of the signal into local datavector, can be noise or signal
c
 1    continue                  ! back here if a noise spectrum made
c                                                                               
      if(noise_spectrum.eq.2) then   ! use start of signal for noise spectrum
         last=last-first
         first=1
         do i = 1,last                                                         
            y(i)=signal1(i)                                                              
         enddo
      else                      ! use selected time window
         j =  0                                                                    
         do i = first,last                                                         
            j = j + 1                                                               
            y(j) = y(i)                                                            
         enddo                                                                     
      endif
                                                                                
      nsamp = last-first+1                                                                 
c                                                                               
c------- prepare data.     Pad with zeros and taper 10%.                        
c                                                                               
      call prepare(nsamp,y,taper,ipow,npad,com)                                
c
c   check for max number of points
c
      if(ipow.ge.max_sample/2) then
           call clear_to_alpha
           write(6,*)' Too many points in spectrum'
           write(6,*)' Max number is ', max_sample/2
           stop
      endif                                

c
c   calculate travel time for q correction, first get origin time
c   but only if running from data base (opmode = 0)
c
      if(opmode.eq.0) read(data(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)')                                     
     *oyear,omonth,oday,ohour,omin,osec                                         
c                                                                               
c   get absolute origin time in secs                                            
c                                                                               
      if(opmode.eq.0) 
     *call timsec(oyear,omonth,oday,ohour,omin,osec,otime)                      
c
c   calculate travel time. ffsec is waveform file abs start time from
c   routine convert in mulplt, stime is time from start of wave form file
c   to first sample in spectral window
c
      if(opmode.eq.0) travel_time=ffsec-otime+stime
c
c   put in some defaults if opmode not 0
c
      if(opmode.ne.0) then
         travel_time=1.0
      endif

c                                                                               
c-------- Calculate the spectrum, and remove the system response
c         if desired. If noise sepctrum, do not use q and kappa                
c              
      if(ch.eq.'N') then
         save_q0=q0
         save_kappa=kappa
         kappa=0
         q0=0          ! do not use q and kappa
         spec_phase='N'
      endif
c
c   rewrite message since it might have changed
c
      message(1)='General parameters, '//spec_phase//'-spectrum'
      write(message(2),'(a,f5.1,a,f5.2,a,i5)')  'Vel ',spec_velocity,
     *' Dens',density, ' Dist',int(sdistance)
      write(message(3),'(a,i5,a,f5.2,a,f5.3)')
     *'Q0  ',int(q0),' Qalp',
     *   qalpha,' k   ',kappa
      call xmessage(message,3,30,620.0,100.0)
      pole_low=0
      pole_high=0
      if(filt.gt.0) then
         f_low=flow(filt)
         f_high=fhigh(filt)
         pole_low=8
         pole_high=8
      endif
      call spectrum(com,ipow,disp_vel,rem_resp,rate,
     +     f_low,f_high,pole_low,pole_high,q0,qalpha,
     *     kappa,travel_time)

c
      if(ch.eq.'N') then          ! put values back in
         q0=save_q0                            
         kappa=save_kappa
         iaxes_type=2             ! only allow log-log
      endif

c
c   write out spectrum if not a noise spectrum and flag set
c
      if(noise_spectrum.lt.2.and.abs(spec_out).eq.1) then
          open(15,file='com_spec.out',status='unknown')
          write(15,*) ipow,rate,swindow
          write(15,'(4E15.8)')(com(i),i=1,ipow/2+1)
          close(15)
          spec_out=-1   ! indicate this was first spectrum
      endif
c
c   calculate real spectrum, limit to frequencies ffmin and ffmax
c
      j = 0
 
      do i = 2,((ipow/2) + 1)
       ff = (i-1)*rate/ipow
       if((filt.ne.0.and.ff .ge. flow(filt) .and. ff .le. fhigh(filt)).
     * or.(filt.eq.0.and.ff .ge. ffmin      .and. ff .le. ffmax)) then
         j = j + 1
         if(power) then
c            y(j) = ((com(i)*conjg(com(i)))/nsamp)    ! stationary signal
c changed March 28, 2001, lo, psd as defined in the numerical recip.
c
            y(j) =2*com(i)*conjg(com(i))/(rate*nsamp)    ! stationary signal
         else
            y(j) = ((1/rate)**2)*(com(i)*conjg(com(i)))    ! transient signal
            y(j) = sqrt(y(j))              ! j.h. change
         endif

c         y(j) = log10( y(j) )                        ! take the logarithm
c
c Set the axes type (bjb 22/02/2001) 
c
         if(iaxes_type.eq.1) then
           x_work(j) = log10(ff)
           ytext='    Amplitude                '
           x0=-110.0                  ! indicate antilog on x-axis notation
         endif
         if(iaxes_type.eq.2) then
           y(j) = log10( y(j) )       ! take the logarithm
           x_work(j) = log10(ff)      
           ytext='Log amplitude                '
           x0=-110.0                  ! indicate antilog on x-axis notation
         endif
         if(iaxes_type.eq.3) then
           x_work(j) = ff
           ytext='    Amplitude                '
           x0=110.0                   
         endif
         if(iaxes_type.eq.4) then
           y(j) = log10( y(j) )       ! take the logarithm
           x_work(j) = ff             
           ytext='Log amplitude                '
           x0=110.0                   
         endif

         if(ch.eq.'N') y(j)=(y(j)-18.0)*10.0  ! db (m/s**2)**2

c         if(linx) then
c            x_work(j) = (ff)
c         else
c            x_work(j) = log10(ff)
c         endif

       endif
      enddo
c
c   text for kind of spectrum
c
      if(disp_vel.eq.0) write(text,'(a)')'Uncorrected  '
      if(disp_vel.eq.3) write(text,'(a)')'Acceleration '
      if(disp_vel.eq.2) write(text,'(a)')'Velocity     '
      if(disp_vel.eq.1) write(text,'(a)')'Displacement '
      if(power.and.ch.ne.'N') text=text(1:12)//', power'
      if(ch.eq.'N') text=text(1:12)//', power, db(m/s**2)**2/Hz'

c
c  possibly clculate slope and t*
c
      if(linx.and.noise_spectrum.lt.2) then
         call lsqlin(j,x_work,y,aa,bb,corr,rms)
         tstar=bb*0.879    ! 0.879 = 1/(pi * log10(e))
         message(1)(1:12)='Lin fit: t*='
         write(message(1)(13:20),'(f8.3)') tstar
         message(1)(21:22)=' c '
         write(message(1)(23:30),'(f8.3)') corr
         call xmessage(message,1,30,620.0,20.0)
      endif
c
c   possibly write out amplitude spectrum
c

      if(noise_spectrum.lt.2.and.abs(spec_out).eq.1) then
          open(15,file='amp_spec.out',status='unknown')
          write(15,'(a)') text
          if(linx) then
             do i=1,j
               write(15,*) x_work(i),y(i)
             enddo
          else
             do i=1,j
               write(15,*) 10.0**x_work(i),y(i)
             enddo
          endif
          spec_out=-1           ! indicate a spectrum written out
          close(15)
      endif
c
c   check that filter limits for spectral output give at least 3 points
c
      if(j.lt.3) then
         call clear_to_alpha
         write(6,*)' Too few points in spectrum or limits wrong'
         write(6,*)' see mulplt.def'
         stop
      endif
c
c   find some kind of average to use with spectral fitting when
c   the signal spectrum is in y
c
      if(noise_spectrum.lt.2) then
c        amp_average=(y(j/2) +y(j/2-5)+y(j/2+10))/3.0
          do i=3,j/4
             amp_average=amp_average+y(i)
          enddo
          amp_average=amp_average/(j/4-3+1)
      endif
      nspec=j
c
c   clear top part of screen and display new choises
c
      call xset_color(color_back)
      call fillbox(0.0,690.0,1024.0,780.0)
      
c
c   plot spectrum
c
      xtext='          Frequency (Hz)     '
c commented following line (bjb 22/02/2001)
c      ytext='Log amplitude                '
      nnspec(1)=nspec
      if(linx) then
         x0=110.0
      else
c commented following line (bjb 22/02/2001)
c         x0=-110.0      ! indicate antilog on x-axis notation
      endif
      xlength=500.0    ! length of x-axis
      if(noise_spectrum.eq.0.or.noise_spectrum.eq.2) then
         xy_cursor=4   ! stop with cursor, signal or noise after sign spectrum
         if(noise_spectrum.eq.2) xlength=0.0  ! same scale as signal spectrum
      else
         xy_cursor=0   ! do not call up cursor
      endif
c
c   display new choises
c
      message(1)=
     *'Valid input is now:  Mouse click to select 3 points in spectrum'
	  message(2)=
     *'                     R: Replot     F: Foreward or next trace   ' 
      message(3)=
     *'                     Q: Quit       S: Make spectral modeling   '
      call xmessage(message,3,64,10.0,700.0)

c
c   add noise limits if power noise spectrum, only log-log
c
      if(ch.eq.'N') then
        k=0
        do i=1,22
          ff = 10.0**low_f(i)
          if(ff .ge. ffmin .and. ff .le. ffmax) then
             j=j+1
             k=k+1
                x_work(j)=low_f(i)
                y(j)=low_l(i)
           endif
        enddo
        nnspec(2)=k
        k=0
        do i=1,14
          ff = 10.0**high_f(i)
          if(ff .ge. ffmin .and. ff .le. ffmax) then
             j=j+1
             k=k+1
               x_work(j)=high_f(i)
               y(j)=high_l(i)
          endif
        enddo
        nnspec(3)=k
        xy_cursor=4   ! stop after plot
       endif

       k=1
       if(ch.eq.'N') k=3

      call xy_plot
     *(k,nnspec,x_work,y,text,xtext,ytext,xlength,270.0,
     *x0,70.0,2,1,30.0
     *,xy_cursor,c_spec,nc,cx,cy)

c
c   if noise spectrum (not power noise), go back to the beginning, 
c   but first reset
c   noise spectrum indicator
c
      if(noise_spectrum.eq.1.and.ch.ne.'N') then
         noise_spectrum=2     ! indicate next spectrum to be a noise spectrum
         goto 1
      else
         noise_spectrum=0     ! reset
      endif
      if(c_spec(nc).eq.'f'.or.c_spec(nc).eq.'F') choice='FINI'
      if(c_spec(nc).eq.'r'.or.c_spec(nc).eq.'R') choice='REPL'
      if(c_spec(nc).eq.'q'.or.c_spec(nc).eq.'Q') choice='QUIT'
      if(c_spec(nc).eq.'s'.or.c_spec(nc).eq.'S') choice='FIT '
c
c   calculate and write spectral values if at least 3 points sampled,
c   last point is exit, use 3 values before that.
c   
c
      omega0=0.0
      cornerf=0.0
      if(nc.gt.3) then
            if(ch.eq.'D'.or.ch.eq.'d') then   ! only calculate parameters for
            omega0=(cy(nc-3)+cy(nc-2))/2.0    ! displacement spectrum
            cornerf=10.0**cx(nc-2)
            if((cx(nc-2)-cx(nc-1)).ne.0.0)
     *      sslope=-(cy(nc-2)-cy(nc-1))/(cx(nc-2)-cx(nc-1))
            moment=0.0
            sdrop=0.0
            call calcmoment
c
c  output on screen
c
            message(1)= 'Amplitude spectral parameters'
            write(message(2),
     *      '(a,f6.2,a,f6.1,a,f5.1)') 'Mo  ',moment,
     *      ' ST ',sdrop,' OM  ',omega0
            write(message(3),'(a,f5.1,a,f5.1,a,f5.1)')
     *      'f0   ',cornerf,' R   ',radius,' MW  ',mw
            message(4)= ' '
            message(5)='Push any key to continue, R to replot '
            call xmessage(message,5,40,620.0,275.0)
         else
            moment=0.0
            sdrop=0.0
            message(1)='No spectral parameters calculated'
            message(2)='Not a displacement spectrum'
            message(3)='Push any key to continue, R to replot '
            call xmessage(message,3,40,620.0,275.0)
         endif
c
c   if choice is forward, stop so display remain
c
         call xscursr(i,xx,yy)			
         if(char(i).eq.'r'.or.char(i).eq.'R') choice='REPL'
         if(char(i).eq.'s'.or.char(i).eq.'S') choice='FIT '
      endif
c
c------------------------------------------------
c   calculate spectral fitting parameters
c------------------------------------------------
c
      if(choice.eq.'FIT ') then      
c
c   get moment if available and if not already entered manually
c
         if(average_moment.eq.0.0) then
            do i=2,nhead
              if(data(i)(2:13).eq.'SPEC AVERAGE') then
                 read(data(i)(18:22),'(f5.2)',err=3846) average_moment
                 goto 3847
              endif
 3846         continue
              average_moment=0.0    ! is log moment
            enddo
 3847       continue              ! moment found
         endif
c
c   if no distance, set to 1.0
c
         if(sdistance.eq.0.0) sdistance=1.0
 7464    continue
c
c   read parameters
c

         write(text1,'(a3,f5.1,a17)')'f0=',f0,' enter new/return'
	   text = '                    '
         call oneline(text1,25,text,8,630.0,250.0)
         if(text(1:4).ne.'    ') then
            call sei get values (1,text,code)
            f0=array$(1)
         endif
         write(text1,'(a3,f5.3,a17)')'k =',kk,' enter new/return'
	   text = '                    '
         call oneline(text1,25,text,8,630.0,250.0)
         if(text(1:4).ne.'    ') then
            call sei get values (1,text,code)
            kk=array$(1)
         endif
         write(text1,'(a3,i5,a17)')'Q0=',int(qq),' enter new/return'
         text = '                    '
	   call oneline(text1,25,text,8,630.0,250.0)
         if(text(1:4).ne.'    ') then
            call sei get values (1,text,code)
            qq=array$(1)
         endif
         write(text1,'(a3,f5.3,a17)')'qa=',qa,' enter new/return'
         text = '                    '
	   call oneline(text1,25,text,10,630.0,250.0)
         if(text(1:4).ne.'    ') then
            call sei get values (1,text,code)
            qa=array$(1)
         endif



c
c   check q
c
         if(qq.eq.0.0) qq=99999.0   ! a large q is like no q
c
c  calculate f0 or stress drop dependent on what was entered
c
c
c   if cornor frequency not used, enter stress drop
c
         if(f0.eq.0.0) then
	      text = '                    '
            call oneline('Stress drop',11,text,20,630.0,250.0)
            ssdrop=sei real num(text,i)  ! i is error code
c
c   calculate f0 from stress drop
c
            if(average_moment.gt.0.0.and.svelocity.gt.0.0) then 
              f0=ssdrop*1e14*svelocity**3/(8.5*10**average_moment)
              f0=f0**0.333333
            else
              f0=1.0
            endif
         else
c
c  calculate stress drop from f0
c
            if(average_moment.gt.0.0.and.svelocity.gt.0.0) then
              ssdrop=(1e-14)*8.5*10**average_moment*f0**3/svelocity**3
            else
              ssdrop=-1.0
            endif
         endif
c
c   calculate theroretical spectrum
c
         j=0
         do i = 2,((ipow/2) + 1)
           ff = (i-1)*rate/ipow
           if((filt.ne.0.and.ff .ge. flow(filt) .
     *     and. ff .le. fhigh(filt)).
     *     or.(filt.eq.0.and.ff .ge. ffmin
     *     .and. ff .le. ffmax)) then
              j = j + 1
              y(j) = ((2*3.14*ff)**(disp_vel-1)/(1+ff*ff/(f0*f0)))*  ! source
     *               exp(-3.14*sdistance*ff/(spec_velocity*qq*(ff**qa))) ! Q
     *               *exp(-3.14*kk*ff)      ! surface correction
              y(j) = log10( y(j) )      ! take the logarithm
c              if(linx) then
c                x_work(j)=ff
c              else
                x_work(j) = log10(ff)     ! this after use of com, equivalent
c              endif
            endif
         enddo
c
c   normalize values, use middle frequency value for normalizing, 
c   first calculate constant due to remaining factors
c
         norm_factor=average_moment     ! moment, here it is logaritmic
         norm_factor=norm_factor+log10(1.2)   ! rad. pat, free surface
         norm_factor=norm_factor+log10(1.0/   ! the rest, 1000 to convert metr.
     *               (4*3.14*(density*1000)*((1000*spec_velocity)**3)))
c
c   geometrical spreading
c
         call spec_dist
     *   (spec_phase,edistance,edepth,geo_factor)
         norm_factor=norm_factor-log10(1000.0/geo_factor)
         geo_distance=1.0/geo_factor
c        if(sdistance.lt.100.0) then           ! distance correction
c           norm_factor=norm_factor-log10(sdistance*1000.0)
c        else
c           norm_factor=norm_factor-log10(1000.0*sqrt(100.0*sdistance))
c        endif

         norm_factor=norm_factor+9             ! convert to nanometers
c
c   add factor
c
         do i=1,nspec
            y(i)=y(i)+norm_factor
         enddo
c
c   factor to normalize plot
c
          norm_factor=0.0
          do i=3,j/4
             norm_factor=norm_factor+y(i)
          enddo
          norm_factor=norm_factor/(j/4-3+1)
         norm_factor=amp_average - norm_factor

c        norm_factor=amp_average - 
c    *   (y(nspec/2)+y(nspec/2-5)+y(nspec/2+10))/3.0
c
c   output
c
c
         message(1)= 'Spectral fitting parameters'
         write(message(2),'(a,f6.2)') 'Obs - calc level', norm_factor
         write(message(3),'(a,f6.2,a,i5)')      'Moment ',
     *   average_moment, ' Geo_dist ',int(geo_distance)
         write(message(4),'(a,f9.2,a,f5.1)') 
     *   'Stress drop', ssdrop,' f0 ',f0
         write(message(5),'(a,f5.3,a,i5,a,f5.2)')
     *   'k   ',kk,' q   ',int(qq),' qa ',qa 
         call xmessage(message,5,30,620.0,275.0)
         do i=1,nspec
           y(i)=y(i)+norm_factor
         enddo
         xlength=0.0     ! use same scale as before
c
c   check type of axis
c

        do j=1,nspec
           if(iaxes_type.eq.3.or.iaxes_type.eq.4)
     *     then
             x_work(j)=10.0**x_work(j)
           endif
           if(iaxes_type.eq.3.or.iaxes_type.eq.1) then
              y(j)=10.0**y(j)
           endif
        enddo
c
c   put in a file
c
        open(15,file='spectral_fit.out',status='unknown')
        write(6,*) 'sepctral fit f, amp'
        do i=1,nspec
          write(15,*) x_work(i),y(i)
        enddo
        close(15)


         call xy_plot
     *   (1,nnspec,x_work,y,text,xtext,ytext,xlength,270.0,
     *   x0,70.0,2,1,30.0
     *   ,xy_cursor,c_spec,nc,cx,cy)
         if(c_spec(nc).eq.'f'.or.c_spec(nc).eq.'F') choice='FINI'
         if(c_spec(nc).eq.'r'.or.c_spec(nc).eq.'R') choice='REPL'
         if(c_spec(nc).eq.'q'.or.c_spec(nc).eq.'Q') choice='QUIT'
         if(c_spec(nc).eq.'s'.or.c_spec(nc).eq.'S') choice='FIT '
c
c   try again
c
         if(choice.eq.'FIT ') goto 7464
 7465    continue
      endif

      return                                                                    
      end                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc            
      subroutine calcmoment
c
c     Subroutine to calculate log10(seismic moment)
c     and stressdrop.
c     Author: C. Lindholm, July, 1993
c
c     Returned parameters:
c       log10(seismic moment)
c       stress drop
c
c       output is newton-meter for moment and km for radius
c       input is nanometer-sec for spectral level and else in 
c       km/sec, g/cm*cm and km

      implicit none
      include 'mulplt.inc'      
      include 'seisan.inc'
      real pi
      real geo_factor  ! geometrical spreading factor
      real surface,radpat  !  surface effect and radiation pattern effect 
      real factor
c
      pi=3.14159265
      surface=2.0
      radpat=0.6
      radius=0.35*svelocity/cornerf
c
      if(sdistance.eq.0) then
         moment=0.0
         sdrop=0.0
         mw=0.0
         return
      endif
c
c   geometrical spreading
c
      call spec_dist
     *(spec_phase,edistance,edepth,geo_factor)
      geo_distance=1.0/geo_factor
      factor= 4*pi*(density*1000.0)*((spec_velocity*1000.0)**3)
c     moment = factor*(sdistance*1000.0)*  ! old geo spreading
      moment = factor*(1000.0/geo_factor)*
     *(10.0**omega0)/(radpat*surface*1.0e9)
      sdrop = (0.44*moment)/(1.0e14*radius**3)
      moment = log10(moment)
      if(moment.gt.0.0) mw=moment*0.667-6.06

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine get_3_chan
c                
c  does 3 component analysis, azimut calculation and component
c  rotation
c                                                               
      include 'mulplt.inc'
      include 'seiplot.inc'                                                             
c                                                                               
c-- No. of channels in file                     
      integer nchan		
c-- number of samples
      integer nsamp
c-- counters,etc                                         
      integer i,j,k,nsamp1			
c-- general text string
      character*80 text
c-- Station and component info    
      character*5 station(max_trace),stat_c
      character*4 comp(max_trace),comp_c
c-- time of main header and channel header
      integer myear,mmonth,mday,mhour,mmin,cyear,
     *cmonth,cday,chour,cmin,doy
      real msec,csec
c-- index for channels this station
      integer chan_z,chan_e, chan_n
c-- 3 component parmeters, coherence and rms
       real COHER,RMSAMP
c-- azimuth for rotating the horizontal seismograms
      real backazi,caz,saz
c-- channel delay
      real delay
      double precision mtime,ctime
c-- stat time and window relative main header
      real start,window
      integer first1,last1   ! local counters
c-- save xscale
      real x_old_scale
c-- 1: error, 0: no error                                  
      integer error
      integer code    ! error code
c-- save rotate flag
      logical save_rotate
      integer first_org			
      integer hl,c_chan
      integer current_chan_3com  ! channel number on entry of routine
c
      backazi=10000    ! always ask for backazimuth, this parameter gives
c
c   save current channel from which call was made
c
      current_chan_3com=wav_current_chan(1)
c
c   get and check rotation paramters
c
      if(rotate) then
        if(backazi.gt.999) then    ! no azimuth given, enter manually
            text = '                    '
            call oneline(' Give back-azimuth ',19,text,10,500.0,35.0)
            backazi=sei real num(text,code)
        endif
      endif
c
c   save old xscale
c
      x_old_scale=xscale
      error=0
      j = 1                                                                     
c
c   read 3 channels
c
      current_chan_3com=wav_current_chan(1)    ! save active channel
      first_org=first
      rate=wav_rate(wav_current_chan(1))
c
      call wav_read_3channel(current_chan_3com)
c
c   check if both horizontal channels there
c                  
      if(wav_current_chan(2).eq.0.or.wav_current_chan(3).eq.0) then
         error=1                                                               
         write(text,'(a)')' Not both horizontal channels, replot'
         call xchars(text,36,100.0,350.0)
         goto 500
      endif
c
c   calculate start time for all 3 channels, check if data is there
c
cfix
      wav_out_nchan=3
      do i= 1,3
        k=wav_current_chan(i)
        wav_out_chan(i)=k  ! index of 3 channels
        wav_out_start(k)=wav_delay(current_chan_3com)+(first-1)/rate !same start
        wav_out_duration(k)=(last-first+1)/rate
      enddo
c
      call wav_get_interval          ! find where and if data available
c
c   write out if a problem
c
      if(wav_out_status(wav_current_chan(1)).ne.4.or.wav_out_status
     *(wav_current_chan(2)).ne.4.or.wav_out_status
     *(wav_current_chan(3)).ne.4) then
         error=1                         
         write(text,'(a)')' Data not available same interval, replot'
         call xchars(text,36,100.0,350.0)
         goto 500
      endif
c
c   select, filter, remove response, save in work array
c
      nsamp=wav_out_duration(wav_current_chan(1))*rate ! assume same for3 chan.
      do i=1,3
         numb_samp=wav_nsamp(wav_current_chan(i))
         call trans_filt(i)
         first1=wav_out_first_sample(wav_current_chan(i))
         last1=first1+nsamp-1   ! cfix
c
c   put window in beginning of array
c		 
         do k=first1,last1
            if(i.eq.1)
     *      wav_y1(k-first1+1)=wav_y1(k)
            if(i.eq.2)
     *      wav_y2(k-first1+1)=wav_y2(k)
            if(i.eq.3)
     *      wav_y3(k-first1+1)=wav_y3(k)
         enddo
      enddo
      numb_samp=nsamp
c                                                                               
c   calculate start second for lower plot, will be the same for all 3 channels 
c 
cfix                                                                           
         fsec=wav_sec(wav_current_chan(1))+(first-1)/rate                      
         fmin=wav_min(wav_current_chan(1))+(first-1)/(rate*60.0)
                       
c                                                                               
c  set plotting parameters etc
c
      ypos=270                                                                  
      height=90        
      pframe=3     ! plot only top and sides
      paxis=0      ! no axis tics
      paxisnumb=0  ! no axis numbers                                           
      pmax=0       ! plot no max below trace
      ppick=0      ! no picking mode
      page_time=time2-time1                                                     
c                                                                               
c    plot z channel                                                         
c                 
         first=1
         last=nsamp
         do i=1,nsamp
            y(i)=wav_y1(i)
         enddo                                                              
         call plotw	                                                        
         i=max+0.5                                                        
         write(text,'(i9)') i                                             
         call xchars(text,9,900.0,ypos+height-22)	
         text(1:1)='Z'
         call xchars(text,1,20.0,ypos+height-22)	
         if(rotate) then
            i=backazi+0.5
            write(text,'(a,i4)')'baz=',i
            call xchars(text,8,50.0,ypos+height-22)
         endif
c
c   plot picks
c
         page_start=0.0
         k=wav_current_chan(1)
c
c   pics are referenced in time to channel where zoom was made,
c   therefore use trace start time from current_chan_3com
c
         wav_rot_comp(k)=' '
c
c   save picks from original channel displayed on top
c
         call convert(wav_abs_time(current_chan_3com),
     *   wav_stat(current_chan_3com),wav_comp(current_chan_3com),1)
c
c   load picks from z-channel
c
         call convert(wav_abs_time(current_chan_3com),
     *   wav_stat(k),wav_comp(k),-1)
         first=first_org   ! original zoom start

         call tecpic       ! plot picks

         first=1           ! now data put into array starting with zoom start
         
c
c  set rotation parameters
c
      caz = -cos(backazi*3.14159/180.)
      saz = -sin(backazi*3.14159/180.)
c
c  plot nw channel
c
      pframe=2    ! plot side of frame only
      ypos=180
c
      do i=1,nsamp
        if(rotate) then
           y(i) = caz*wav_y2(i) + saz*wav_y3(i)  ! make radial component
        else
           y(i)=wav_y2(i)
        endif
      enddo
c
c   load picks from n-channel
c
         call convert(wav_abs_time(current_chan_3com),
     *   wav_stat(wav_current_chan(2)),
     *   wav_comp(wav_current_chan(2)),-1)
         first=first_org   ! original zoom start
         call tecpic
         first=1
c                                                                               
c   plot                                                              
c                                                                               
      call plotw	                                                        
      i=max+0.5                                                        
      write(text,'(i9)') i                                             
      call xchars(text,9,900.0,ypos+height-22)	
      text(1:1)='N'
      if(rotate) text(1:1)='R'
      call xchars(text,1,20.0,ypos+height-22)	
c
c  plot ew channel, transverse channel if rotate
c
      ypos=90                                                                  
      paxis=1      ! plot axis again
      paxisnumb=1                                    
c
c   put signal in plotting array
c
      do i=1,nsamp
        if(rotate) then
           y(i) = -saz*wav_y2(i) + caz*wav_y3(i)  ! transvers component
        else
           y(i)=wav_y3(i)
        endif
      enddo
c                                                                               
c   plot ew                                         
c                                                                               
c
c   load picks from ew-channel
c
      call convert(wav_abs_time(current_chan_3com),
     *wav_stat(wav_current_chan(3)),
     *wav_comp(wav_current_chan(3)),-1)
      first=first_org   ! original zoom start
      call tecpic
      first=1
      call plotw	                
      i=max+0.5                                                        
      write(text,'(i9)') i                                             
      call xchars(text,9,900.0,ypos+height-22)	
      text(1:1)='E'
      if(rotate) text(1:1)='T'
      call xchars(text,1,20.0,ypos+height-22)	
c
c   do 3 component analysis
c
      do i=1,nsamp
         wav_y3comp(i,1)=wav_y1(i)
         wav_y3comp(i,2)=wav_y2(i)
         wav_y3comp(i,3)=wav_y3(i)
      enddo
      call PRECOH1(wav_y3comp,max_sample,1,nsamp,three_comp_velocity,
     *COHER,AZIM,VELI,RMSAMP)
      i=azim+0.5
      write(text,333) i,veli,coher
 333  format('Az ',i4,1x,'Vel ',f7.1,1x,'Co ',f6.1)
      call xchars(text,31,1.0,3.0)
 500  continue

c
c   load in original channel, disable rotate
c
      rotate=.false.
      call wav_read_channel(current_chan_3com)
c
c   put in original picks
c
         call convert(wav_abs_time(current_chan_3com),
     *   wav_stat(current_chan_3com),wav_comp(current_chan_3com),0)

      first=1
      numb_samp=wav_nsamp(wav_current_chan(1))
c
c   rest plot parameters
c
      pframe=1                                                                  
      paxis=1                                                                   
      paxisnumb=1                                                               
      phelp=1                                                                   
      pmax=1                                                                    
      ppick=1                                                                   
      ptitle=1                                                                  
      filt=0
      ypos=450.0
      height=200.0
      xscale=x_old_scale
c     write(39,*) 'first,last',first,last
c     write(39,*)'p st,last_ix',page_start,last_ix
c     write(39,*)'pictim',(pictim(i),i=1,5)
c     write(39,*)'xscale,rate,xpos',xscale,rate,xpos
c
      return                                                                    
      end                                                                       




      subroutine plot_resp                                                 
c                                                                               
c     routine to plot response function                
c                                                                                
c-- common block     
c                                
      include 'mulplt.inc'	
      include 'seiplot.inc'		
c
c-- indicator for response removal or not (1 or 0).                        
      integer rem_resp			
      real ff ! frequency
c-- number of points in spectrum
      integer nspec
c-- if several data sets, number of points in each
      integer nnspec(2)
c-- counters and help variables                                               
      integer i,j				
c-- response value
      complex respons
      character*80 text
      character*80 message(10)
      character*30 xtext,ytext      ! axis text
c--returns from spectral plot
      character*1 c_spec(20)
      real x0                       ! left corner of spectrum
      real cx(20),cy(20)
      real ix,iy                    ! mouse position
      integer nc
      real x_work(max_sample)
c-- counter bounds for spectrum                    
      equivalence(x_work,com)                                                     
      
c
c   first check if response is available
c
      rem_resp=0
         wav_resp_file = ' '
         call read_resp
         if(wav_resp_status.eq.'9') then
            write(text(1:20),'(a)')'No response info ***'
            call tchars(text,20,600.0,225.0)
            write(text(1:24),'(a)')'Push any key to continue'
            call tchars(text,23,600.0,190.0)
c
c-- call up cursor                               
c
            call xscursr(i,ix,iy)			
            choice='REPL'
            return
         endif
         if(wav_resp_status(1:1).eq.'8') then
            write(text(1:33),'(a)')'Response from waweform header ***'
            call tchars(text,33,600.0,320.0)
         endif
c
c  the amplitude response
c
      j=0
      do i = -260,190,1    ! log frequencies
         j=j+1
         ff=10**(i/100.0)
         call calc_resp(ff,respons)
         y(j)=log10(cabs(respons))
         x_work(j) = log10(ff)          
 1243    continue
      enddo
      nspec=j
c
c   clear top part of screen and display new choises
c
      call xset_color(color_back)
      call fillbox(0.0,700.0,1024.0,780.0)
      message(1)='Valid input is now:  R: Replot          '
      message(2)='                     Q: Quit            '
      message(3)='                     F: Foreward or next'
      call xmessage(message,3,40,10.0,700.0)
      
c
c   plot spectrum
c
      xtext='      Log frequency (Hz)     '
      ytext='Log amplitude                '
      write(text,'(a)')'Amplitude Response, count/nm '
      text(30:80)=' '
      if(wav_resp_status.eq.' ') then
         write(text(34:42),'(i4,1x,2i2)') wav_resp_year,wav_resp_month,
     *                                 wav_resp_day
      endif
      if(wav_resp_status.eq.'8') then
         text(34:41)='W header'
      endif
      text(44:44)=wav_resp_type(1:1)
      nnspec(1)=nspec
      x0=100.0                
      call xy_plot
     *(1,nnspec,x_work,y,text,xtext,ytext,380.0,270.0,
     *x0,70.0,2,1,30.0
     *,0,c_spec,nc,cx,cy)
c
c  the phase response
c
      j=0
      do i = -260,190,1    ! log frequencies
         j=j+1
         ff=10**(i/100.0)
         call calc_resp(ff,respons)
         y(j)=180.0*atan2(imag(respons),real(respons))/3.141592654
         x_work(j) = log10(ff)          
      enddo
      nspec=j
c
c   unwrap phases
c
c      call unwrap_phase(y,nspec,y)
c
c   plot spectrum
c
      ytext='Phase                '
      write(text,'(a)')'Phase response '
      nnspec(1)=nspec
      x0=620.0                
      call xy_plot
     *(1,nnspec,x_work,y,text,xtext,ytext,380.0,270.0,
     *x0,70.0,2,1,30.0
     *,1,c_spec,nc,cx,cy)
      if(c_spec(nc).eq.'f'.or.c_spec(nc).eq.'F') choice='FINI'
      if(c_spec(nc).eq.'r'.or.c_spec(nc).eq.'R') choice='REPL'
      if(c_spec(nc).eq.'q'.or.c_spec(nc).eq.'Q') choice='QUIT'
      return
      end                                                                       

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
