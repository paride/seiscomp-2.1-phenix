c$DEBUG 
c
c   system response subroutines
c
c   changes:
c   dec 95 by jh : poles and zeros
c                  unwrap routine
c   mar 20, 96   : change error position from 1024 to 160, allow the use of
c                  internal file header use
c   may 30, 96   : fix bug , do not use header 160 to check, use 161..
c   aug 96  96   : changed phase response , seem that there was
c                        an error, term with 2 * i * damp changed sign
c   dec 96       : no filenr.lis needed to get list of cal files
c   feb 97 lo    : added subroutine inter_p from resp.for
c   mar 21 jh    : more precision in poles and zero routine
c   jan 23 98 jh : in read_resp, save first 80 chars, done in main prog. before
c   feb 15       : improve interp
c-----------------------------------------------------------------------------
c   oct 98  jh   : ------------ version 7.0 --------------------------------
c                  year 2000, longer station names, new resp file names
c   nov4         : remove computer specifics
c   feb 4 99 mv  : gencon=0 was set at a wrong place when using tabulated values
c   mar 1 99 jh  : fix forced header read
c   nov 30       : save date of respons file in comments field in read_resp
c   mar 23, 00 lo: mistake in resp_f fixed, was correct before aug 96
c   mar 26 00 lo : bug fixes in reading PAZ
c   mar 29 00 lo : if only filters use resp_f, not interpolation
c   mar 29 00 jh : implement read of gse files
c   april        : new wav structure
c   sep 27 00 lo : new routine find_resp_file
c   nov  1 00 lo : change in reading gse cal2 line, was gse2.1 before 
c   nov  3 00 lo : another change in the GSE resp reading
c   dec 12 00 lo : major changes in read_gse_resp
c   jan 29 00 jh : new flag to force table read in seisan format
c   feb 16    jh : fix forced read of seisan heders or optional read of
c                  seisan header if no file in cal
c   jun 08 01 lo : set wav_resp_filename in read_resp
c   jul 13 07 lo : changes in gse resp reading routine
c
c
c
c
      subroutine get_seisan_resp(header)
c
c   read seisan response values from header and return them in common 
c   block response
c
      implicit none
      include 'seidim.inc'
      logical gse_resp          ! true if gse format
      logical sei_resp          ! true if seisan format
      integer nfa               ! number of frequencies and amplitude
      logical ftab              ! if true, force tabulated values
c
C   NUMBER OF FILTERS,  POLES,AND FREQUENCIES
      INTEGER NFILT,POLE(10)
      real xpole(10)
      REAL FFILT(10)
      real pp(max_resp_value*2),pz(max_resp_value*2) ! PAZ, real and im.
      complex pol(max_resp_value),zero(max_resp_value)  ! complex PAZ
      integer npol,nzero        ! number of poles and zeros
      real norm                 ! normalization constant for poles and zeros
C   AMPLIFIER GAIN (DB), SEISMOMETER GENERATOR CONSTANT (LOADED V/M/S)
      REAL GAIN,GENCON
C   SEISMOMETER OR ACCELEROMETER DAMPING RATIO AND SEISMOMETER PERIOD
      REAL DAMPIN,PERIOD
C   RECORDING MEDIA GAIN (COUNTS/V OR M/VOLT)
      REAL REGAIN
c   sensor type 2: seismometer, 3: accelerometer
      integer sentyp
      real g1hz  ! gain at 1 hz
c   individual response values
      real freq_val(max_resp_value),gain_val(max_resp_value),
     *phas_val(max_resp_value)
C   SEISAN HEADER
      CHARACTER*1040 HEADER
      character*80 chead(13)
      INTEGER I,J,I1,I2,I3,I4,I5,I6,J1,J2,k,line
c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  THIS COMMON BLOCK ALSO IN pr_resp and presp
c  should have been in an include file
c***********************************************************************
c
      common /response/nfilt,pole,ffilt,gain,gencon,dampin,period,ftab,
     *                 regain,g1hz,freq_val,gain_val,phas_val,sentyp,
     *                 npol,nzero,pol,zero,norm,gse_resp,sei_resp,nfa
      equivalence (xpole,pole)
      period=0.
      dampin=0.
      gencon=0.
c
c   if gse response, return since values read in directly
c
      if(gse_resp) return
c
c   first check if seisan poles and zeros, read if so
c
      npol=0
      nzero=0
      if(header(78:78).eq.'P') then       
         do i=1,13
           chead(i)=header((i-1)*80+1:i*80)
         enddo
         read(header(161:182),'(1x,2i5,g11.4)') npol,nzero,norm
         k=23
         line=3
         do i=1,npol*2
            read(chead(line)(k:k+10),'(g11.4)') pp(i)
            k=k+11
            if(k.eq.78) then
              k=1
              line=line+1
            endif
         enddo
c         do i=1,nzero ! before march 2000
         do i=1,nzero*2
            read(chead(line)(k:k+10),'(g11.4)') pz(i)
            k=k+11
            if(k.eq.78) then
              k=1
              line=line+1
            endif
         enddo
c
c   convert to complex
c
         k=0
         do i=1,npol*2,2
            k=k+1
c           pol(i)=cmplx(pp(i),pp(i+npol)) ! before March 2000
           pol(k)=cmplx(pp(i),pp(i+1))
c           write(*,*) pol(i)
         enddo
         k=0
         do i=1,nzero*2,2
           k=k+1
c          zero(i)=cmplx(pz(i),pz(i+nzero)) ! before March 2000
           zero(k)=cmplx(pz(i),pz(i+1))
c           write(*,*) zero(i)
         enddo
         return
       endif
c
c    get sensor type
c
      sentyp=2                           ! initially assume velocity transducer
      if(header(6:6).eq.'A') sentyp=3    ! A must be given for accelerometer 
c
c   read response parameters from header
c  
c      read(header(161:240), '(10g8.3)',err=999) 
      read(header(161:240), '(10g8.3)') 
     &    period, dampin, gencon,gain, 
     &    regain, g1hz, ffilt(1), xpole(1), ffilt(2), xpole(2)
      read(header(241:320), '(10g8.3)') (ffilt(i), xpole(i),i=3,7)
c
c   check if interpolation
c
      ftab=.false.
      if(header(78:78).eq.'T') then
         ftab=.true.                ! force table calculation 
         nfa=30                     ! always 30 in seisan format
      endif
c
c   poles are integers
c
      do i=1,7
        pole(i)=xpole(i)
      enddo
c
c   find how many filters
c
      nfilt=0
      do i=1,7
        if(ffilt(i).ne.0.0.and.pole(i).ne.0) nfilt=nfilt+1
      enddo
c
c   read individual response values from header
c
      do i=1,3                                                                  
         j1=(i-1)*10+1                                                          
         j2=j1+9                                                                
         i1=320+(i-1)*240+1                                                     
         i2=i1+79                                                               
         i3=400+(i-1)*240+1                                                     
         i4=i3+79                                                               
         i5=480+(i-1)*240+1                                                     
         i6=i5+79
         
         read(header(i1:i2),'(10g8.3)') (freq_val(j),j=j1,j2)                       
         read(header(i3:i4),'(10g8.3)') (gain_val(j),j=j1,j2)                       
         read(header(i5:i6),'(10g8.3)') (phas_val(j),j=j1,j2)                       
      enddo                              
999   continue
c
      return
      end                                       

      subroutine calc_resp(f,respons)                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                               
c  Subroutine to calculate the response. Response is        
c  given in counts/Nanometer.  If no parameters are given, response
c  is calculated by interpolating the table values. This is decided by looking
c  at the gencon value, which must then be zero.
c  Response values are on common block response
c                                                                               
c  Input:                                                                       
c    f      - Frequency at which respons is calculated                          
c           - Response info in common block
c                                                                               
c  Output:                                                                      
c    respons - Complex response of system                                       
c                                                                               
c                                                                               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      include 'seidim.inc'                                                                                
c--frequency                                                       
      real f			
c--response                                        
      complex respons
      logical gse_resp            ! true if gse response
      logical sei_resp            ! true if seisan format
      integer nfa                 ! number of frequencies and amplitude
      logical ftab              ! if true, force tabulated values

c		
C   NUMBER OF FILTERS,  POLES,AND FREQUENCIES
      INTEGER NFILT,POLE(10)
      REAL FFILT(10)
C   AMPLIFIER GAIN (DB), SEISMOMETER GENERATOR CONSTANT (LOADED V/M/S)
      REAL GAIN,GENCON
C   SEISMOMETER OR ACCELEROMETER DAMPING RATIO AND SEISMOMETER PERIOD
      REAL DAMPIN,PERIOD
C   RECORDING MEDIA GAIN (COUNTS/V OR M/VOLT)
      REAL REGAIN
      real g1hz   ! gain at 1 hz
c   individul response values
      real freq_val(max_resp_value),gain_val(max_resp_value),
     *phas_val(max_resp_value)
      integer sentyp  ! sensor type
      complex pol(max_resp_value),zero(max_resp_value)  ! complex PAZ
      integer npol,nzero        ! number of poles and zeros
      real norm                 ! normalization constant for poles and zeros
c
      common /response/nfilt,pole,ffilt,gain,gencon,dampin,period,ftab,
     *                 regain,g1hz,freq_val,gain_val,phas_val,sentyp,
     *                 npol,nzero,pol,zero,norm,gse_resp,sei_resp,nfa
c
c   could be poles and zeros
c
        if(npol.ne.0.or.nzero.ne.0) then
           call pazresp (f, norm, nzero, zero,
     $     npol, pol,  respons)
           goto 500
        endif
c                                                                               
c  Decide response calculation or interpolation    
c                                                                               
         if(.not.ftab) then  ! jh jan 2001
c       if(gencon.ne.0.0.or.nfilt.ne.0)  ! before jan 2001  
          call resp_f(f,respons)                                         
        else                                                                    
          call interp(f,respons)                                         
        endif 
c                 
 500  continue                 
c
c   convert to nanometers
c
      respons=respons/1.0e9
c                                
      return                                                                    
      end                                                                       
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine resp_f(f,respon)
c
C
C   CALCULATES DISPLACEMENT RESPONSE OF A SEISMIC SYSTEM INCLUDING
C   SENSORS, AMPLIFIER, FILTERS ETC.
c
c   Input is frequency f, and output
c   is response respon. Response info is in common block response
C
C   ALL UNITS ARE IN METERS AND SECONDS EXCEPT ACCELERATION
C   WHICH IS SUPPOSED TO BE IN G.
C
      IMPLICIT NONE
      include 'seidim.inc'
C
C   VARIABLES
C
      logical gse_resp            ! true if gse response
      logical sei_resp            ! true if seisan response
      integer nfa                 ! number of frequencies and amplitude
      logical ftab              ! if true, force tabulated values

C   COMPLEX RESPONSE OF SYSTEM FOR NF VALUES
      COMPLEX RESPON,xx
C   FREQUENCY FOR CALCULATIONS
      REAL F
C   NUMBER OF FILTERS,  POLES,AND FREQUENCIES
      INTEGER NFILT,POLE(10)
      REAL FFILT(10)
C   AMPLIFIER GAIN (DB), SEISMOMETER GENERATOR CONSTANT (LOADED V/M/S)
      REAL GAIN,GENCON
C   SEISMOMETER OR ACCELEROMETER DAMPING RATIO AND SEISMOMETER PERIOD
      REAL DAMPIN,PERIOD
C   RECORDING MEDIA GAIN (COUNTS/V OR M/VOLT)
      REAL REGAIN
C   FLAG FOR TYPE OF SENSOR 1: NONE 2: SEISMOMETER 3: ACCELEROMETER
      INTEGER SENTYP
C   RESPONSE CURVE CORRECTION EXPONENT
      INTEGER RESCOR
C   CONSTANT
      REAL PI
      real g1hz   ! gain at 1 hz
c   individul response values
      real freq_val(max_resp_value),gain_val(max_resp_value),
     *phas_val(max_resp_value)
C   HELP VARIABLES
      COMPLEX CX
      REAL X
      INTEGER J
      complex pol(max_resp_value),zero(max_resp_value)  ! complex PAZ
      integer npol,nzero        ! number of poles and zeros
      real norm                 ! normalization constant for poles and zeros
      common /response/nfilt,pole,ffilt,gain,gencon,dampin,period,ftab,
     *                 regain,g1hz,freq_val,gain_val,phas_val,sentyp,
     *                 npol,nzero,pol,zero,norm,gse_resp,sei_resp,nfa
      DATA PI/3.141592654/
C
C   CALCULATE EXPONENT FOR SENSOR TYPE 
C        
      IF(SENTYP.EQ.2) RESCOR=0
      IF(SENTYP.EQ.3) RESCOR=-2
C
C   NOW READY FOR CALCULATIONS, FIRST CALCULATE CONSTANT FACTORS  
C
      RESPON=1.0
      IF (GENCON.eq.0.) GOTO 15 ! if only filters
C
C  AMP GAIN AND RECORDING MEDIA GAIN
C
      RESPON=RESPON*10**(GAIN/20.0)*REGAIN
C
C  SEISMOMETER IF ANY
C
      IF(SENTYP.EQ.2) THEN
         RESPON=RESPON*PI*(0.0,2.0)*GENCON*(-1.0)*PERIOD**2
      ENDIF
C
C  ACCELEROMETER IF ANY, CONVERT FROM G TO M/SS
C
      IF(SENTYP.EQ.3) RESPON=RESPON*GENCON/9.81
C
C   SEISMOMETER
C
         IF(SENTYP.EQ.2) THEN
            X=PERIOD*F
            RESPON=RESPON*F**3/(1.+(0.,2.)*DAMPIN*X-X*X)   ! before aug 96 and after 03-00
c            RESPON=RESPON*F**3/(1.-(0.,2.)*DAMPIN*X-X*X)  ! after aug 96, was wrong
         ENDIF
c
c   type of sensor and response type
c
          xx=(0.0,1.0)*2*pi*f
          respon=RESPON/(xx**rescor)
C
C   FILTERS
C
 15      CONTINUE

         IF(NFILT.GT.0) THEN
            DO 20, J=1,NFILT
               CALL BWORTH(F,FFILT(J),POLE(J),CX)
               RESPON=RESPON*CX
 20         CONTINUE
         ENDIF
c   
         return
         end
c---------------------------------------------------------------------------    
                                                                                
      subroutine interp(f,respons)                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                               
c   interpolation of response values               
c                                                                               
c      input   :    f       : frequency                                         
c                                                                               
c      output  :    response: interpolated response value at frequency f        
c                                                       
c                                                                               
c    J. Havskov, Jun 22, 1988                                                   
c    Modified R. Hansen                                                         
c                                                                               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      include 'seidim.inc'
      logical gse_resp            ! true if gse response
      logical sei_resp            ! true if seisan response
      integer nfa                 ! number of frequencies and amplitude
      logical ftab              ! if true, force tabulated values
C   NUMBER OF FILTERS,  POLES,AND FREQUENCIES
      INTEGER NFILT,POLE(10)
      REAL FFILT(10)
C   AMPLIFIER GAIN (DB), SEISMOMETER GENERATOR CONSTANT (LOADED V/M/S)
      REAL GAIN,GENCON
C   SEISMOMETER OR ACCELEROMETER DAMPING RATIO AND SEISMOMETER PERIOD
      REAL DAMPIN,PERIOD
C   RECORDING MEDIA GAIN (COUNTS/V OR M/VOLT)
      REAL REGAIN
c   sensor type 2: seismometer, 3: accelerometer
      integer sentyp
      real g1hz   ! gain at 1 hz
c   individul response values
      real freq_val(max_resp_value),gain_val(max_resp_value),
     *phas_val(max_resp_value)
      INTEGER J
      complex pol(max_resp_value),zero(max_resp_value)  ! complex PAZ
      integer npol,nzero        ! number of poles and zeros
      real norm                 ! normalization constant for poles and zeros
c
      common /response/nfilt,pole,ffilt,gain,gencon,dampin,period,ftab,
     *                 regain,g1hz,freq_val,gain_val,phas_val,sentyp,
     *                 npol,nzero,pol,zero,norm,gse_resp,sei_resp,nfa
c-- frequency                                                      
      real		f		
      complex		respons 	
c-- gain and phase at f                                                  
      real		gainf,phasf		
c-- phase at f                              
c                                                                               
c   do linear interpolation
c
c   if desired frequency lower than lowest, use the lowest                                                     
c                                                         
      if(f.le.freq_val(1)) then
         gainf=gain_val(1)
         phasf=phas_val(1)
         goto 2
      endif
c                      
c-- max nfa values                                             
      do j=1,nfa-1				
         if(f.ge.freq_val(j).and.f.le.freq_val(j+1)) go to 1 ! interpolate                           
c
         if(freq_val(j+1).eq.0.0.or.j.eq.nfa-1) then ! not high enough value, use highest
            gainf=gain_val(j)
            phasf=phas_val(j)
            goto 2
         endif	
      enddo                                                                    
                                                                               
 1    continue                                                                 
      gainf=gain_val(j)+(f-freq_val(j))*(gain_val(j+1)-gain_val(j))/                           
     *(freq_val(j+1)-freq_val(j))                                                      
      phasf=phas_val(j)+(f-freq_val(j))*(phas_val(j+1)-phas_val(j))/                           
     *(freq_val(j+1)-freq_val(j))                                                      
                                                                               
c      if(freq_val(j+1).eq.0.0.or.j.eq.nfa-1) 
c     *write(6,*)' response suspicious'
 
 2    continue
c                                                                               
c   calculate complex response value                                            
c                                                                               
      respons=g1hz*gainf*cmplx(cos(phasf/57.3),sin(phasf/57.3))                        
      return                                                                    
      end                                                                       
                                                                                

c--------------------------------------------------------------------------     
                                                                                
                                                                                
      subroutine read_resp                                              
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                               
c  Subroutine to read response header from CAL directory                     
c                                                                               
c  Input: Is taken from waveform common block
c                    
c     sta  -  Station name (a5) (e.g. KTK1 )                                     
c     comp -  Station component (a4) (e.g. SZ L)                                
c     date -  Date: year,month,,day,hour,min                    
c                                                                               
c  Output:                                                                      
c     response in response common block                 
c                                                                               
c  Programmed by R. A. Hansen   June 21, 1988                                   
c  Changes:                                                                     
c  3-1-90 by j.h.   : small cleanup and comments,                               
c                     return if no file found, not chrash                       
c  dec 7 91         : return 9 in header(1040) if no file
c  sep 11 92        : add a close 88 after read filenr.lis
c  nov 4            : clear header(1040:1040) for flag
c  dec 93           : pc adoption
c  may 95           : rotated components
c  dec 95           : poles and zeros
c  dec 96           : no filenr.lis to get cal files
c  oct 98           : year 2000 and much more
c                                                                               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      include 'seidim.inc'           
      include 'waveform.inc'
C
C    SEISAN library inclusions...
c    ----------------------------
C
      include 'libsei.inc'                 ! Library definitions & data defns.
      integer  code                        ! Condition.
      logical  b_flag                      ! Flag!!
      logical ftab              ! if true, force tabulated values
    
      double precision time_cal,time_event ! time of cal file and event file
                                                               
c--file name we are looking for                       
      character*10   file			
c-- calibration files
      character*29 cal_files(max_cal_files$)
      character*80 cal_file
c--file name to open                              
      character*80   filename			
      character*80   local_cal  ! alternative directory for cal files
c-- top dir                                   
      character*60   top_directory		
      character*1040 header                                                     
      integer year                  
      integer ical  ! cal file number found                                            
c--do loop indices                                         
      integer i,j,k				
c--character counter                                          
      integer nt				
c-- number of calibration files in directory
      integer ncal_files
      integer imon,idy,ihr,imin
c--number of files with sta-comp                          
      integer seiclen
      character*1 dchar
      integer read1       ! file unit

c
c   for common block
c
      logical gse_resp          ! true if gse format
      logical sei_resp          ! true if seisan format
      integer nfa               ! number of frequencies and amplitude
C   NUMBER OF FILTERS,  POLES,AND FREQUENCIES
      INTEGER NFILT,POLE(10)
      REAL FFILT(10)
      real pp(max_resp_value*2),pz(max_resp_value*2) ! PAZ, real and im.
      complex pol(max_resp_value),zero(max_resp_value)  ! complex PAZ
      integer npol,nzero        ! number of poles and zeros
      real norm                 ! normalization constant for poles and zeros
C   AMPLIFIER GAIN (DB), SEISMOMETER GENERATOR CONSTANT (LOADED V/M/S)
      REAL GAIN,GENCON
C   SEISMOMETER OR ACCELEROMETER DAMPING RATIO AND SEISMOMETER PERIOD
      REAL DAMPIN,PERIOD
C   RECORDING MEDIA GAIN (COUNTS/V OR M/VOLT)
      REAL REGAIN
c   sensor type 2: seismometer, 3: accelerometer
      integer sentyp
      real g1hz  ! gain at 1 hz
c   individual response values
      real freq_val(max_resp_value),gain_val(max_resp_value),
     *phas_val(max_resp_value)
      common /response/nfilt,pole,ffilt,gain,gencon,dampin,period,ftab,
     *                 regain,g1hz,freq_val,gain_val,phas_val,sentyp,
     *                 npol,nzero,pol,zero,norm,gse_resp,sei_resp,nfa

      call dir_char(dchar)
c
c clear flag incating if a response and from where
c
      wav_resp_status=' '
c     wav_resp_action=' ' !! jh feb , why cleared ??
      wav_resp_type=' '
      wav_resp_filename=' '
c     wav_resp_seisan_chead=' '  ! jh feb 2001 , should not be cleared  
      sei_resp=.true.
      gse_resp=.false.      ! default seisan response file

c
c  if wav_resp_file set the skip 
c
      if (wav_resp_file.ne.' ') then
        cal_file = wav_resp_file
        ical=1
        cal_files(ical)=cal_file
        goto 250
      endif

c
c   check if response values MUST be read from SEISAN header
c   then read it and return, in future could also be used for GSE
c
      if(wav_resp_action(1:1).eq.'F') then
         wav_resp_status(1:1)='8' ! indicate header response
         wav_resp_type='SEISAN'
c
c   read response values to common block
c
         call get_seisan_resp(wav_resp_seisan_chead)
C
         return
      endif
c
      file(1:5)=wav_stat(wav_current_chan(1))
      file(6:9)=wav_comp(wav_current_chan(1))
      time_event=wav_abs_time(wav_current_chan(1))
c
c  rotated components are assumed to have same response
c  as horizontal components
c
         if(file(9:9).eq.'R') file(9:9)='N'
         if(file(9:9).eq.'T') file(9:9)='E'
         nt=9                                                                      
         do i=1,nt                                                             
           if(file(i:i).eq.' '.or.file(i:i).eq.char(0)) file(i:i)='_'
         enddo                                                                 
c                                                                               
c  get list of cal files in CAL directory, local directory is first searched,
c  then the main cal directory and finally subdirectories of the cal directory
c                                                                               

c
c-----------------------------------------------------------
c  case of a local CAL directory, variable local_cal is set
c-----------------------------------------------------------
c
      call get_env_cal(local_cal)
      if(local_cal(1:3).ne.'   ') then
         k=seiclen(local_cal)
         filename=local_cal(1:k)//file(1:9)
c
c   read file names
c
         call respfil(filename,k+9,cal_files,
     *   max_cal_files$,ncal_files)
c
c   check time
c
         if(ncal_files.gt.0) then            ! now check date
           ical=0
           do i=1,ncal_files
              read(cal_files(i)(11:25),
     *        '(i4,1x,i2,1x,i2,1x,2i2)') year,imon,idy,ihr,imin
c
c calculate abs time of cal file
c 
              call timsec(year,imon,idy,ihr,imin,0.0,time_cal)                    
c                                                                               
c  search for response file with nearest date                                   
c                                                                               
              if (time_cal.le.time_event) then   ! found file
                 ical=i
              endif
           enddo
           if(ical.ne.0) then
              cal_file=filename(1:k)//cal_files(ical)(1:29)
              goto 250  ! jump out of search loop to read cal file  
           endif                                               
         endif                                                                     
      endif
c
c------------------------------
c   case of CAL main directory
c------------------------------
      call topdir(top_directory)                                                
      k=index(top_directory,' ')-1
                                              
c
c   make directory name and start of file name for that station and component
c
      filename=
     *top_directory(1:k)//dchar//'CAL'//dchar//file(1:9)
c
c   read files
c
      call respfil(filename,k+14,cal_files,
     *max_cal_files$,ncal_files)
c
c   check time
c
      if(ncal_files.gt.0) then
        ical=0
        do i=1,ncal_files
           read(cal_files(i)(11:25),
     *     '(i4,1x,i2,1x,i2,1x,2i2)') year,imon,idy,ihr,imin
c
c calculate abs time of cal file
c 
           call timsec(year,imon,idy,ihr,imin,0.0,time_cal)                    
c                                                                               
c  search for response file with nearest date                                   
c                                                                               
           if (time_cal.le.time_event) then  ! found one
              ical=i
           endif                                               
         enddo
         if(ical.ne.0) then
            cal_file=filename(1:k+5)//cal_files(ical)(1:29)
            goto 250  ! jump out of search loop to read cal file  
         endif                                               
      endif                                                                     
c
c  if no files or no files at correct time, check if in a subdirectory to CAL
c
c----------------------------------
c   case of CAL  subdirectory
c----------------------------------
c
      filename=
     *top_directory(1:k)//dchar//
     *'CAL'//dchar//file(1:5)//dchar//file(1:9)
c
c   read files
c
      call respfil(filename,k+20,cal_files,
     *max_cal_files$,ncal_files)
      if(ncal_files.eq.0) goto 77             ! no calibration file available
      
      if(ncal_files.gt.0) then
        ical=0
        do i=1,ncal_files
           read(cal_files(i)(11:25),
     *     '(i4,1x,i2,1x,i2,1x,2i2)') year,imon,idy,ihr,imin
c
c calculate abs time of cal file
c 
           call timsec(year,imon,idy,ihr,imin,0.0,time_cal)                    
c                                                                               
c  search for response file with nearest date                                   
c                                                                               
           if (time_cal.le.time_event) then     ! found one
             ical=i
           endif                                               
        enddo
        if(ical.ne.0) then
           cal_file=filename(1:k+11)//cal_files(ical)(1:29)
           goto 250  ! jump out of search loop to read cal file  
        endif                                               
c
c   if here, no file was found
c
        goto 77
      endif                                                                     
c
c---------------------------------------
c   cal file found, now read it
c---------------------------------------
c
 250  continue
c
c  save time of cal file
c
      wav_resp_stat=cal_files(ical)(1:5)
      wav_resp_comp=cal_files(ical)(6:9)
c
c store name of file in common block
c
      wav_resp_filename=cal_files(ical)

      read(cal_files(ical)(11:14),'(i4)') wav_resp_year
      read(cal_files(ical)(16:17),'(i2)') wav_resp_month
      read(cal_files(ical)(19:20),'(i2)') wav_resp_day

c                                                                               
c                                                                               
c   open response file and read                                                 
c                                                                               

            call sei open( old$,               ! Open file (stop on error).
     &                     ' ',                ! No prompt.
     &                     cal_file,           ! This filename.
     &                     read1,              ! On unit.
     &                     b_flag,             ! Exists!!
     &                     code )              !
            if( code .ne. e_ok$ ) goto 77      ! Does not exist.


c
c   read response file, first determine type
c
      if(cal_files(ical)(27:29).eq.'GSE') then
          gse_resp=.true.
          sei_resp=.false.
      endif   
c
c
c   seisan response file
c
      if(sei_resp) then
         wav_resp_type='SEISAN'
         do i=1,13                                                              
           j=(i-1)*80 + 1                                                      
           k=j+79                                                               
           read(read1,'(a80)') header(j:k)
         enddo
         wav_resp_seisan_chead=header ! to be used if response put into s. head.
      endif
c
c   read gse response file
c
      if(gse_resp) then
         wav_resp_type='GSE'
         call read_gse_resp(read1)
      endif                                                                     
      call sei close( close$, read1, code )           ! Close cal file
c
c   read response values to common block
c
      if (sei_resp) then
        call get_seisan_resp(header)
      endif
c
      goto 78
 77   continue
      wav_resp_status(1:1)='9'  ! indicate no response file in cal
c
c   check if response values in header in which case only seisan
c   response is available
c
      if(wav_resp_seisan_chead(161:208).ne.' '.and.
     &   seiclen(wav_resp_seisan_chead(161:208)).ne.0) then
         gse_resp=.false.
         sei_resp=.true.
         wav_resp_type='SEISAN'
         wav_resp_status(1:1)='8'    ! indicate seisan header response available
c
c   read response values to common block
c
         call get_seisan_resp(wav_resp_seisan_chead)
      endif
 78   continue
c
      return                                                                    
      end                                                                       
c------------------------------------------------------------------------       
                                                                                
                                                                                
      subroutine bworth(f,fo,np,hs)                                             
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc   
ccccc                                                                           
c    bworth cal the response of a np pole butterworth filter up to              
c    as many poles as the arrays s and t are dimensioned                        
c                                                                               
c    f-frequency(hz)                                                            
c    fo-the corner frequency of the filter                                      
c    np-the number of poles, negative for high pass                             
c    hs-complex response of the filter                                          
c                                                                               
c    the formula used -- h(s)=1/(s-s1)(s-s2)...(s-sk)                           
c                        i*pi*(1/2+((2*k-1)/(2*np)))                            
c    where         sk=exp                                                       
c                                   k=1,2, ... np                               
c                  s = i(f/fo)                                                  
c                                                                               
c    ref theory and application of digital signal processing                    
c    rabiner and gold page 227 prentice-hall 1975                               
c                                                                               
c    Adapted to Vax/VMS by R. A. Hansen                                         
c                                                                               
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc   
ccccc                                                                           
c                                                                               
      complex s(20),t(20),as,bk,hs                                              
      hs=cmplx(1.0,0.0)                                                         
      n=iabs(np)                                                                
      if(np .eq. 0) go to 6                                                     
      if(f .eq. 0.0 .and. np .lt. 0) hs=cmplx(0.0,0.0)                          
      if(f .eq. 0.0 .and. np .lt. 0) go to 6                                    
      do 1 k=1,n                                                                
      an=float(k)                                                               
      ak=3.141592654*(0.5+(((2.*an)-1.)/(2.*float(n))))                         
      bk=cmplx(0.0,ak)                                                          
 1    s(k)=cexp(bk)                                                             
      ss=f/fo                                                                   
      as=cmplx(0.0,ss)                                                          
      if(np.lt.0) as=1./as                                                      
      t(1)=as-s(1)                                                              
      if(n.eq.1) go to 5                                                        
      do 2 i=2,n                                                                
 2    t(i)=(as-s(i))*t(i-1)                                                     
 5    continue                                                                  
      hs=1./t(n)                                                                
 6    return                                                                    
      end                                                                       
                                                                                

      subroutine pazresp (freq, pazconstant, numzeros, zeros,
     $     numpoles, poles,  clxrsp)
      implicit none
      complex zeros(*), poles(*), clxrsp
      real     freq, pazconstant
      integer    numzeros, numpoles


c.======================================================================
c.    Purpose                                                           
c     Compute_response_from_poles_and_zeros                       resp<<
c.----------------------------------------------------------------------
c.    Keywords                                                          
c.----------------------------------------------------------------------
c.    Package                                                           
c.    Visible                                                           
c.    Standard_fortran_77                                               
c.    Use_only                                                          
c.----------------------------------------------------------------------
c.    Input                                                             
c     
c..   freq        - Frequency (Hz)
c..   pazconstant - Constant in paz representation
c..   numzeros    - Number of zeros
c..   zeros       - Complex zeros
c..   numpoles    - Number of poles
c..   poles       - Complex poles
c
c.    Output
c
c..   clxrsp     - Complex amplitude in counts/m
c     
c.----------------------------------------------------------------------
c.    Programmer    Tormod Kvaerna                                     
c.    Creation_date 080395
c.    Made_at  
c     NORSAR
c     Granaveien 33
c     N-2007 Kjeller
c     
c.    Modification
c.    dec 17, 95 : simplification, use single precision
c.    Correction                                                        
c.======================================================================

c----
c     Internal declarations
c----
      complex*16      s, tzero, tpole
      real            pi, omega
      integer         i

      pi   = 3.141592654d0
c----
c     Poles and zeros response
c----
      omega = 2.0*pi*freq
      s     = cmplx(0.0, omega)
      tzero = (1.0,0.0)
      tpole = (1.0,0.0)
      do 1000 i = 1, numzeros
         tzero = tzero*(s-zeros(i))
 1000 continue
      do 2000 i = 1, numpoles
         tpole = tpole*(s-poles(i))
 2000 continue

      clxrsp    = cmplx(pazconstant,0.0)*tzero/tpole

      return

      end
      subroutine unwrap_phase (wrapped, nphase, unwrapped)

      real        wrapped(*), unwrapped(*)
      integer       nphase

c.======================================================================
c.    Purpose                                                           
c     Convert_wrapped_phase_to_unwrapped                          resp<<
c.----------------------------------------------------------------------
c.    Keywords                                                          
c.----------------------------------------------------------------------
c.    Package                                                           
c.    Visible                                                           
c.    Standard_fortran_77                                               
c.    Use_only                                                          
c.----------------------------------------------------------------------
c.    Input                                                             
c     
c..   wrapped    - Array of wrapped phases (degrees)
c..   nphase     - Number of phase elements
c
c.    Output
c
c..   unwrapped  - Array of unwrapped phases(degrees)
c     
c.----------------------------------------------------------------------
c.    Programmer    Tormod Kvaerna                                     
c.    Creation_date 080395
c.    Made_at  
c     NORSAR
c     Granaveien 33
c     N-2007 Kjeller
c     
c.    Modification
c.                
c.    Correction                                                        
c.======================================================================

c----
c     Internal declarations
c----
      integer       i
      real        wrapadd, diff1, diff2

      wrapadd = 0.0
      unwrapped(1) = wrapped(1)
      do 1000 i = 2, nphase
         diff1 = wrapped(i) - wrapped(i-1)
         diff2 = wrapped(i) - wrapped(i-1) - 360.0
         if (abs(diff2).lt.abs(diff1)) then
            wrapadd = wrapadd + 360.0
         endif
         diff2 = wrapped(i) - wrapped(i-1) + 360.0
         if (abs(diff2).lt.abs(diff1)) then
            wrapadd = wrapadd - 360.0
         endif
         unwrapped(i) = wrapped(i) - wrapadd
         
 1000 continue

      return

      end



      subroutine inter_p(nresp,normtab,
     *freq_val,gain_val,phas_val,f,respons)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   interpolation of response values
c
c      nresp   : number of values in
c      normtab : value to multiply with
c      freq_val,gain_val,phas_val : frequency, amplitude and phase in

c      f       : frequency for desired output
c      response: interpolated response value at frequency f
c
c
c    J. Havskov, feb 97
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none
      real normtab   ! multiplying factor
c   individul response values
      real freq_val(*),gain_val(*),phas_val(*)
      INTEGER J
c-- frequency
      real              f
      integer nresp
      complex           respons
c-- gain and phase at f
      real              gainf,phasf
c           
c   do linear interpolation, first find which 2 frequencies to use
c           
      do j=1,nresp-1
         if(freq_val(j+1).eq.0.0) go to 1       ! no more response values
         if(f.ge.freq_val(j).and.f.le.freq_val(j+1)) go to 1
      enddo 

 1    continue
      gainf=gain_val(j)+(f-freq_val(j))*(gain_val(j+1)-gain_val(j))/
     *(freq_val(j+1)-freq_val(j))
      phasf=phas_val(j)+(f-freq_val(j))*(phas_val(j+1)-phas_val(j))/
     *(freq_val(j+1)-freq_val(j))

c      if(freq_val(j+1).eq.0.0.or.j.eq.nresp-1)
c     *write(6,*)' response suspicious'

c           
c   calculate complex response value
c

      respons=normtab*gainf*cmplx(cos(phasf/57.3),sin(phasf/57.3))
      return
      end


      subroutine read_gse_resp(read1)
c
c   read response values from GSE file and put in common block,
c   for GSE1/2 and FAP/PAZ
c
      implicit none
      include 'seidim.inc'
      logical gse_resp          ! true if gse format
      logical sei_resp          ! true if seisan format
      integer nfa               ! number of frequencies and amplitude
      logical ftab              ! if true, force tabulated values
      integer read1             ! file unit

c
C   NUMBER OF FILTERS,  POLES,AND FREQUENCIES
      INTEGER NFILT,POLE(10)
      REAL FFILT(10)
      complex pol(max_resp_value),zero(max_resp_value)  ! complex PAZ
      integer npol,nzero        ! number of poles and zeros
      real norm                 ! normalization constant for poles and zeros
C   AMPLIFIER GAIN (DB), SEISMOMETER GENERATOR CONSTANT (LOADED V/M/S)
      REAL GAIN,GENCON
C   SEISMOMETER OR ACCELEROMETER DAMPING RATIO AND SEISMOMETER PERIOD
      REAL DAMPIN,PERIOD
C   RECORDING MEDIA GAIN (COUNTS/V OR M/VOLT)
      REAL REGAIN
c   sensor type 2: seismometer, 3: accelerometer
      integer sentyp
      real g1hz  ! gain at 1 hz
c   individual response values
      real freq_val(max_resp_value),gain_val(max_resp_value),
     *phas_val(max_resp_value)
      INTEGER x,c
c gse line 
      character*90 gseline 
      character*2 g1comp                         ! component GSE1
      double precision scale                     ! sensitivity build up from 
                                                 ! components
      double precision sfactor                   ! component sens.
      double precision system_sensitivity        ! overall system sens. nm/count
      integer gsever                             ! gse1 or gse2
      real re,im                                 ! real and imaginary part
      integer iphase                             ! integer phase
      real calper                                ! calibration period
      integer pol_cnt,zero_cnt                   ! counters
      real f                                     ! frequency array
      complex respons

      common /response/nfilt,pole,ffilt,gain,gencon,dampin,period,ftab,
     *                 regain,g1hz,freq_val,gain_val,phas_val,sentyp,
     *                 npol,nzero,pol,zero,norm,gse_resp,sei_resp,nfa

      scale=1.
      pol_cnt=0
      zero_cnt=0

100   continue
      read(read1,'(a)',end=9999) gseline

c 
c gse1 response to be added
c
      if (gseline(1:4).eq.'CAL1') then
         write(*,*) ' gse1 response not supported '
         return
      endif

c
c CAL2 line, GSE2.0
c
      if (gseline(1:4).eq.'CAL2') then
c
c system sensitivity is given in nm/count
c
         read(gseline(28:37),'(e10.2)') system_sensitivity
         read(gseline(39:45),'(f7.3)') calper
         goto 100
      endif


c response in GSE1
c      if (gsever.eq.1) then
c         if (g1format.eq.'PAZ') then
c read poles
c           read(gseline(1:8),'(i8)') npol  ! get number of poles
c           do x=1,npol
c             read(read1,'(a)') gseline
c             read(gseline(1:8),'(g8.3)') re 
c             read(gseline(9:16),'(g8.3)') im
c             pol(x)=cmplx(re,im)
c           enddo
c
c           read(read1,'(a)',end=9999) gseline
c read zeros 
c           read(gseline(1:8),'(i8)') nzero  ! get number of poles
c           do x=1,nzero
c             read(read1,'(a)') gseline
c             read(gseline(1:8),'(g8.3)') re
c             read(gseline(9:16),'(g8.3)') im
c             zero(x)=cmplx(re,im)
c           enddo 
c
c read normalization constant 
c           read(read1,'(a)',end=9999) gseline
c           read(gseline(1:16),'(g16.3)')  g1calib
c           norm=1E9/g1calib
c
c         elseif (g1format.eq.'FAP') then 
c
c           gencon=0.0
c read FAP  
c           read(read1,'(a)',end=9999) gseline
c           read(gseline(1:8),'(i8)') nfa  ! get number of fap 
c           do x=1,nfa
c             read(read1,'(a)') gseline
c             read(gseline,'(e10.5,1x,e10.5,1x,e10.4)')
c     &         freq_val(x),gain_val(x),phas_val(x)
c             g1hz=1E9
c           enddo
c         endif

c
c read GSE2 response
c

c
c PAZ2
c
      if (gseline(1:4).eq.'PAZ2') then
        read(gseline(11:25),'(e15.8)') sfactor
        scale=scale*sfactor
 
c           norm = scalib / sfactor    ! CAL2 already contains scaling for all 
c                                      ! stages
        read(gseline(41:43),'(i3)') npol
        read(gseline(45:47),'(i3)') nzero

        do c=1,npol     ! read the poles
           read(read1,'(a)',end=9999) gseline
           read(gseline(2:16),'(g15.8)') re 
           read(gseline(18:32),'(g15.8)') im
           pol_cnt=pol_cnt+1
           pol(pol_cnt)=cmplx(re,im)
        enddo
 
        do c=1,nzero    ! read the zeros
           read(read1,'(a)',end=9999) gseline
           read(gseline(2:16),'(g15.8)') re 
           read(gseline(18:32),'(g15.8)') im
           zero_cnt=zero_cnt+1
           zero(zero_cnt)=cmplx(re,im)
        enddo
     
        goto 100
      endif
c
c DIG2
c
      if (gseline(1:4).eq.'DIG2') then
        read(gseline(9:23),'(e15.8)') sfactor
        scale=scale*sfactor
        goto 100
      endif
       
c
c FAP2
c
      if (gseline(1:4).eq.'FAP2') then
        read(gseline(25:27),'(i3)') nfa
        gencon=0.0
        do c=1,nfa
          read(read1,'(a)',end=9999) gseline
          read(gseline(2:11),'(f10.5)') freq_val(c)
          read(gseline(13:27),'(e15.8)') gain_val(c)
          read(gseline(29:32),'(i4)') iphase
          phas_val(c)=iphase
        enddo
        goto 100
      endif 
      
9999  continue
c
c set npol and nzero
c
      npol=pol_cnt
      nzero=zero_cnt
 
c
c check sensitivity and set seisan normalization constant (c/m)
c
      norm = scale * 1.E9     ! convert c/nm -> c/m

c
c compute system sensitivity at calperiod
c
      f=1/calper
      call calc_resp(f,respons)
      g1hz = 1.E9*cabs(respons)  ! in c/m

      if (1./cabs(respons).ge.1.05*system_sensitivity.or.
     &    1./cabs(respons).le.0.95*system_sensitivity) 
     &      write(*,*) ' total system sensitivity not equal ' //
     &      'sum of all components, should be ',1./cabs(respons)
 
c
c check if paz2 section normalized
c
      if (cabs(respons).le.1.025.and.cabs(respons).ge.0.9975) then
        write(*,*) ' PAZ section normalized to 1 at calperiod ' //
     &    '-> using sensitivity from CAL2 line '
        norm=norm/system_sensitivity   ! units already converted
      endif
        
c
c if FAP
c
c      g1hz = 1.E9/system_sensitivity

      return
      end


      subroutine read_resp_head(header)
c
c add response to header
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'

      integer year,month,day,hour,min   ! date and time
      real sec
      character*1040 header                  ! header

      wav_resp_seisan_chead=' '         ! there should be nothing since new
      wav_stat(1)=header(1:5)
      wav_comp(1)=header(6:9)
      wav_current_chan(1)=1
      read(header(10:35),'(i3,5x,4(i2,1x),f6.3)',err=99) 
     *year,month,day,
     *hour,min,sec
      year=year+1900
      goto 199
 99   continue
      write(6,*)' Something wrong with channel header generated'
      stop
 199  continue
      call timsec(year,month,day,hour,min,sec,wav_abs_time(1))
      wav_resp_file=' '
      call read_resp
      header(78:1040)=wav_resp_seisan_chead(78:1040)

      if (wav_resp_type.eq.'GSE') then
        write(6,*)
     *       ' Response is in GSE format -------'
      elseif(wav_resp_status(1:1).eq.'9') then
        write(6,*)
     *       ' No response file for this channel --------'
      endif

      return
      end



      subroutine find_resp_file(stat,comp,time_event,cal_file)
c
c routine searches for path and filename to CAL file
c
      implicit none

      include 'seidim.inc'
      character*1 dchar
      double precision time_event,time_cal
      character*10   file
      character*5 stat
      character*4 comp
      integer i,k
      character*80   local_cal,filename,cal_file
      integer ncal_files
      character*29 cal_files(max_cal_files$)
      integer seiclen
      integer year,imon,idy,ihr,imin,ical
      character*60   top_directory

      call dir_char(dchar)
      cal_file = ' '

c
c set station and component names in filenam
c
      file(1:5)=stat
      file(6:9)=comp
      do i=1,9 
        if(file(i:i).eq.' '.or.file(i:i).eq.char(0)) file(i:i)='_'
      enddo

      write(*,*) ' resp: ',file(1:9)


c
c-----------------------------------------------------------
c  case of a local CAL directory, variable local_cal is set
c-----------------------------------------------------------
c
      call get_env_cal(local_cal)
      if(local_cal(1:3).ne.'   ') then
         k=seiclen(local_cal)
         filename=local_cal(1:k)//file(1:9)
c
c   read file names
c
         call respfil(filename,k+9,cal_files,
     *   max_cal_files$,ncal_files)
c
c   check time
c
         if(ncal_files.gt.0) then            ! now check date
           ical=0
           do i=1,ncal_files
              read(cal_files(i)(11:25),
     *        '(i4,1x,i2,1x,i2,1x,2i2)') year,imon,idy,ihr,imin
c
c calculate abs time of cal file
c
              call timsec(year,imon,idy,ihr,imin,0.0,time_cal)

c
c  search for response file with nearest date
c
              if (time_cal.le.time_event) then   ! found file
                 ical=i
              endif
           enddo
           if(ical.ne.0) then
              cal_file=filename(1:k)//cal_files(ical)(1:29)
              return
           endif
         endif

      endif

c
c------------------------------
c   case of CAL main directory
c------------------------------
      call topdir(top_directory)
      k=index(top_directory,' ')-1

c
c   make directory name and start of file name for that station and component
c
      filename=
     *top_directory(1:k)//dchar//'CAL'//dchar//file(1:9)
c
c   read files
c
      call respfil(filename,k+14,cal_files,
     *max_cal_files$,ncal_files)
c
c   check time
c
      if(ncal_files.gt.0) then
        ical=0
        do i=1,ncal_files
           read(cal_files(i)(11:25),
     *     '(i4,1x,i2,1x,i2,1x,2i2)') year,imon,idy,ihr,imin
c
c calculate abs time of cal file
c
           call timsec(year,imon,idy,ihr,imin,0.0,time_cal)
c
c  search for response file with nearest date
c
           if (time_cal.le.time_event) then  ! found one
              ical=i
           endif
         enddo
         if(ical.ne.0) then
            cal_file=filename(1:k+5)//cal_files(ical)(1:29)
            return
         endif
      endif

c
c----------------------------------
c   case of CAL  subdirectory
c----------------------------------
c
      filename=
     *top_directory(1:k)//dchar//
     *'CAL'//dchar//file(1:5)//dchar//file(1:9)
c
c   read files
c
      call respfil(filename,k+20,cal_files,
     *max_cal_files$,ncal_files)
      if(ncal_files.eq.0) return              ! no calibration file available

      if(ncal_files.gt.0) then
        ical=0
        do i=1,ncal_files
           read(cal_files(i)(11:25),
     *     '(i4,1x,i2,1x,i2,1x,2i2)') year,imon,idy,ihr,imin
c
c calculate abs time of cal file
c
           call timsec(year,imon,idy,ihr,imin,0.0,time_cal)
c
c  search for response file with nearest date
c
           if (time_cal.le.time_event) then     ! found one
             ical=i
           endif
        enddo
        if(ical.ne.0) then
           cal_file=filename(1:k+11)//cal_files(ical)(1:29)
           return
        endif
      endif


      return
      end


