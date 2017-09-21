c
c   subroutines for parameter input - output
c
c   all parameters are going through common block in rea.inc
c
c
c   rea_event_in                : read one whole event
c
c   rea_event_out               : write one whole event
c
c   rea_hyp1_in                 : read  type 1 line
c
c   rea_hyph_in                 : read high accuracy hypocenter line
c
c   rea_hype_in                 : read error line
c
c   rea_hyp1_out                : write  type 1 line
c
c   rea_hyph_out                : write high accuracy hypocenter line
c
c   rea_hype_out                : write error line
c
c   rea_hypm_out                : write extra magnitudes
c
c   rea_hyp_clear               : clear hypocenter parameters
c
c   rea_phase_out               : write phase parameters to phase line
c
c   rea_phase_in                : read phase parameters from phase line
c
c   rea_phase_clear             : clear phase parameters
c
c   add_phase_to_sfile          : add phase to data array
c
c   rea_spec_out                : write spectral parameters to spec lines
c
c   rea_av_spec_out             : write average spectral parameter to text line
c
c   rea_spec_in                 : read  spectral parameters to memory
c
c   rea_av_spec_out             : read average spectral parameters
c
c   add_spec_to_sfile           : add spectral lines to data array
c
c   get_par                     : read general parameter def file, set defs
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   General info:
c
c   The phase name DELETE is reserved. Giving a phase the name DELETE, will
c                         remove it at the next write out
c
c
c------------------------------------------------------------------------------
c changes:
c   01 May 2001 lo : change in rea_av_spec_in, AVARAGA -> AVERAGE
c   26 may 2001 jh : add phase DELETE which delete phase on write out
c                    add arrays for all magnitudes,agencies and types
c   06 May 2001 lo : added reading of auto phase to rea_phase_in

      subroutine rea_event_in(unit,all,data,code)
c
c   Read all data from one event in data array and puts it into common block
c   data that canot go into variables in common block are stored in rea_data
c
c     input :   unit:  File unit to read from, if 0, read directly
c                      from data array, MAKE SURE TO PASS NRECORD AND
C                      NHEAD TO COMMON BLOCK BEFORE CALLL
c               all:   If true, read whole event, if false only headers
c               data:  S-file data array
c
c     output:   code:  0: Ok, 1: End of file, 2: Read error (not implemented)
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      integer unit
      logical all
      integer code
      integer nh,ne                   ! count high accurarcy and errror lines
      integer nphase                  ! as counted by indata
      integer nstat                   ! as counted by indata
      character*80 data(1000)
      character*1 type,exp            ! for indata
      integer i,k

      ne=0
      nh=0
      code=0
      rea_nphase=0
c
c  check if reading from a file
c
      if(unit.gt.0) then
         call indata
     *   (unit,nstat,nphase,rea_nhead,rea_nrecord,
     *   type,exp,data,k)
         if(rea_nrecord.eq.0) then
             code=1
             return
         endif
      endif
c
c  now read all data, first headers, the main hypocenter (index 1) also
c  contains error info, high accuracy and extra magnitudes 
c
      rea_nphase=0      ! count number of phase lines
      rea_nhyp=0        ! count number of hypocenters
      rea_nwav=0        ! --------------- waveform files
      rea_ncomment=0    ! --------------- comment lines
      rea_nmacro=0      ! --------------- macroseismic data
      rea_nfault=0      ! --------------- fault plane solutions
      rea_nspec=0       ! --------------- spectral solutions
      rea_nmag=0        ! --------------- magnitudes
c
      i=1
      dowhile(i.le.rea_nhead)
         if(data(i)(80:80).eq.'1'.or.i.eq.1) then  ! allow first line without 1
             rea_nhyp=rea_nhyp+1
             k=rea_nhyp
             call rea_hyp_clear(k)
             call rea_hyp1_in(data(i),k)
             goto 10
         endif
         if(data(i)(80:80).eq.'H') then
            call rea_hyph_in(data(i),1)     ! put high precision info in 1
            nh=nh+1
            goto 10
         endif
         if(data(i)(80:80).eq.'E') then
            call rea_hype_in(data(i),1)        ! put error info in 1
            ne=ne+1
            goto 10
         endif
c
c   check for additional magnitudes, are in type 1 line with same first
c   23 chars and same hypocenter agency as the prime solution
c
         if(data(i)(1:23).eq.data(1)(1:23).and.data(i)(46:48).eq.
     *      data(1)(46:48).and.data(i)(80:80).eq.'1'.and.i.gt.1) then
            call rea_hyp_clear(100)
            call rea_hyp1_in(data(i),100)  ! temporarely use index 100
            do k=1,3                       ! put into last 3 positions
c               hyp_mag(i+3,1)=hyp_mag(100,i)  ! changed lo, may 2001
               hyp_mag(i+3,1)=hyp_mag(6,i)
c               hyp_mag_type(i+3,1)=hyp_mag_type(100,i)
               hyp_mag_type(i+3,1)=hyp_mag_type(6,i)
c               hyp_mag_agency(i+3,1)=hyp_mag_type(100,1)
               hyp_mag_agency(i+3,1)=hyp_mag_agency(6,1)
            enddo
            call rea_hyp_clear(100)            ! clear again
            goto 10
         endif
         
c
c   spectral average line
c
         if(data(i)(80:80).eq.'3'.and.data(i)(2:13).eq.
     *   'SPEC AVERAGE') then        ! changed from AVARAGE lo, May 2001
            call rea_av_spec_in(data(i))
            goto 10
         endif
c
c   spectral station lines
c
         if(data(i)(80:80).eq.'3'.and.data(i)(2:5).eq.'SPEC'.and.
     *      data(i)(7:13).ne.'AVERAGE') then
            if(data(i+1)(80:80).eq.'3'.
     *      and.data(i+1)(2:5).eq.'SPEC') then
               rea_nphase=rea_nphase+1
               k=rea_nphase
               call rea_spec_in(k,data(i),data(i+1))
               rea_nspec=rea_nspec+1
               i=i+1                      ! there are 2 lines
               goto 10
            endif
         endif
c
c   id line
c
         if(data(i)(80:80).eq.'I') then
            rea_id_line=data(i)
            goto 10
          endif
c
c   fault plane solutions
c
         if(data(i)(80:80).eq.'F') then
            rea_nfault=rea_nfault+1
            rea_fault(rea_nfault)=data(i)
            goto 10
         endif
c
c    waveform file names
c
         if(data(i)(80:80).eq.'6') then
             rea_nwav=rea_nwav+1
             rea_wav(rea_nwav)=data(i)
             goto 10
         endif
c
c    macroseismic info
c
         if(data(i)(80:80).eq.'2') then
             rea_nmacro=rea_nmacro+1
             rea_macro(rea_nmacro)=data(i)
             goto 10
         endif

c
c    comment lines
c
         if(data(i)(80:80).eq.'3') then
            rea_ncomment=rea_ncomment+1
            rea_comment(rea_ncomment)=data(i)
         endif


 10      continue
         i=i+1
c
      enddo
c
c   check that there was not too many high accuracy lines or error lines
c
      if(ne.gt.1.or.nh.gt.1) then
          write(6,*) ' Too many error or high accuracy lines'
          write(6,'(a)') data(i)(1:79)
          stop
      endif
c
c   count total number of magnitudes and put into one array
c
      rea_nmag=0
      do i=1,rea_nhyp
        do k=1,6
           if(hyp_mag(k,i).gt.-20.0) then
              rea_nmag=rea_nmag+1
              hyp_mag_all(rea_nmag)=hyp_mag(k,i)
              hyp_mag_type_all(rea_nmag)=hyp_mag_type(k,i)
              hyp_mag_agency_all(rea_nmag)=hyp_mag_agency(k,i)
           endif
        enddo
      enddo
         

c
c   read phases if specified
c
      if(all) then
         do i=rea_nhead+1,rea_nrecord
            if(data(i).ne.' ') then
               rea_nphase=rea_nphase+1
               call rea_phase_clear(rea_nphase)          ! clear variables
               call rea_phase_in(rea_nphase,data(i))     ! read
            else
               if(i.ne.rea_nrecord) then                 ! last can be blank
                  write(6,*)' Blank line in phase data'
                  write(6,'(a)')data(1)(1:79)
                  stop
               endif
            endif
c
c   check if last record is blank as it should be, if not, add one
c
            if(data(rea_nrecord).ne.' ') then
               rea_nrecord=rea_nrecord+1
               data(rea_nrecord)=' '
            endif
         enddo
      endif
c
c
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea_event_out(unit,all,data,code)
c
c   Write all data to data array and optinally to file also. Variables
c   all come from common block.
c
c
c     input :   unit:  File unit to write from, if 0, only write
c                      to data array
c               all:   If true, read whole event, if false only headers
c               
c
c     output:   data:  S-file data array
c               code:  0: Ok, 1: Write error (not implemented)
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      integer unit,code
      logical all
      character*80 data(1000)
      integer i,k

c
c  now write all data, first main header, the main hypocenter (index 1) also
c  contains error info, high accuracy and extra magnitudes 
c
      rea_nrecord=0
c
c   there must be at least one hypo line
c
      if(rea_nhyp.le.0) then
           write(6,*)' Ny hypocenter lines for output'
           stop
      endif
      do i=1,rea_nhyp
         rea_nrecord=rea_nrecord+1
         call rea_hyp1_out(data(rea_nrecord),i)
c
c   for first prime hypocenter, there could be additional info
c
         if(i.eq.1) then
c
c   there could be an addiitonal high accuracy line
c
             if(hyp_high_accuracy(i)) then
                 rea_nrecord=rea_nrecord+1
                 call rea_hyph_out(data(rea_nrecord),i)
             endif
c
c   there could be an error line
c
             if(hyp_error(i))  then
                 rea_nrecord=rea_nrecord+1
                 call rea_hype_out(data(rea_nrecord),i)
             endif
c
c   check for additonal magnitudes, are in type 1 line with same first
c   23 chars and same hypocenter agency as the prime solution
c
             if(hyp_mag(4,1).gt.-100.0.or.hyp_mag(5,1).gt.-100.0.or.
     *       hyp_mag(6,1).gt.-100.0) then
                rea_nrecord=rea_nrecord+1
                call rea_hypm_out(data(rea_nrecord),i)
             endif
          endif
      enddo
c
c   spectral average line
c
      if(rea_av_moment.gt.0.0.or.rea_av_omega0.gt.0.0) then
         rea_nrecord=rea_nrecord+1
         call rea_av_spec_out(data(rea_nrecord))
      endif
      
c
c   spectral lines  
c
      do i=1,rea_nphase
         if(rea_phase(i)(1:4).eq.'SPEC') then
             rea_nrecord=rea_nrecord+1
             call rea_spec_out(i,data(rea_nrecord),data(rea_nrecord+1))
             rea_nrecord=rea_nrecord+1       ! there are 2 spectral lines
          endif
      enddo
c
c   fault plane solutions
c
      if(rea_nfault.gt.0) then
          do i=1,rea_nfault
             rea_nrecord=rea_nrecord+1
             data(rea_nrecord)=rea_fault(i)
          enddo
      endif
c
c    waveform file names
c
      if(rea_nwav.gt.0) then
          do i=1,rea_nwav
             rea_nrecord=rea_nrecord+1
             data(rea_nrecord)=rea_wav(i)
          enddo
      endif

c
c    macroseismic info
c
      if(rea_nmacro.gt.0) then
          do i=1,rea_nmacro
             rea_nrecord=rea_nrecord+1
             data(rea_nrecord)=rea_macro(i)
          enddo
      endif
c
c    comment lines
c
      if(rea_ncomment.gt.0) then
          do i=1,rea_ncomment
             rea_nrecord=rea_nrecord+1
             data(rea_nrecord)=rea_comment(i)
          enddo
      endif
c
c   id line
c
      if(rea_id_line.ne.' ') then
         rea_nrecord=rea_nrecord+1
         data(rea_nrecord)=rea_id_line
      endif
c
c    phase explanation line
c
      rea_nrecord=rea_nrecord+1
      data(rea_nrecord)=
     *' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO '//
     *'AIN AR TRES W  DIS CAZ7'
c     *'SNR AR TRES W  DIS CAZ7'
c
c   number of header lines
c
       rea_nhead=rea_nrecord
c
c   write phases if specified, spectral related phases not written
c
      if(all) then
         do i=1,rea_nphase
            if(rea_phase(i)(1:4).ne.'SPEC'.and.rea_phase(i)(1:6).ne.
     *         'DELETE') then
               rea_nrecord=rea_nrecord+1
               call rea_phase_out(i,data(rea_nrecord))     ! write
            endif
         enddo
      endif
c
c   make last record blank
c
      rea_nrecord=rea_nrecord+1
      data(rea_nrecord)=' '
c
c   write out if flag set
c
       if(unit.gt.0) then
           write(unit,'(a)',err=100) (data(i),i=1,rea_nrecord)
           goto 101
 100       continue
           write(6,*)' Error writing S-file data array, data'
           stop
 101       continue
      endif
c
      return
      end

                  
      subroutine rea_hyp1_in(text,ihyp)
c
c   read all hypocenter info from type 1 line in text into
c   common block hypocenter array number ihyp
c   some checking is done
c
c   note: agencies are 5 chars of which only 3 is used
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      character*80   text
      character*20   err_text       ! error text
      integer        ihyp,seiclen
c
      if (seiclen(text(2:79)).le.0) goto 60
      if(text(80:80).ne.' '.and.text(80:80).ne.'1') then
         write(6,*) ' Not a type 1 line'
         stop
      endif
c      write(3,*)' in read1',ihyp,text(1:40)
c
c   read times
c
      err_text='hypocenter time'
      if(text(2:5).ne.' ')   read(text(2:5),   '(i4)',  err=50)
     *hyp_year(ihyp)
      if(text(7:8).ne.' ' )  read(text(7:8),   '(i2)',  err=50)
     *hyp_month(ihyp)
      if(text(9:10).ne.' ' ) read(text(9:10),  '(i2)',  err=50)
     *hyp_day(ihyp)
      if(text(12:13).ne.' ') read(text(12:13), '(i2)',  err=50)
     *hyp_hour(ihyp)
      if(text(14:15).ne.' ') read(text(14:15), '(i2)',  err=50)
     *hyp_min(ihyp)
      if(text(17:20).ne.' ') read(text(17:20), '(f4.1)',err=50)
     *hyp_sec(ihyp)
c      write(3,*)ihyp,hyp_year(ihyp)

c
c   flags, check a bit
c
      hyp_model(ihyp)=text(21:21)
      hyp_dist_id(ihyp)=text(22:22)
      if(hyp_dist_id(ihyp).ne.'L'.and.hyp_dist_id(ihyp).ne.'R'.and.
     *hyp_dist_id(ihyp).ne.'D') then
         err_text='distance id'
         goto 50
      endif
      hyp_type(ihyp)=text(23:23)
      hyp_fix_org(ihyp)=text(11:11)
      if(hyp_fix_org(ihyp).ne.' '.and.hyp_fix_org(ihyp).ne.'F') then
         err_text='origin time flag'
         goto 50
      endif
c
c  hypocenter
c
      err_text='hypocenter'
      if(text(24:30).ne.' ') read(text(24:30),
     *'(f7.3)',err=50) hyp_lat(ihyp)
      if(text(31:38).ne.' ') read(text(31:38),
     *'(f8.3)',err=50) hyp_lon(ihyp)
      if(text(39:43).ne.' ') read(text(39:43),
     *'(f5.1)',err=50) hyp_depth(ihyp)
c
c   hypocenter flags
c
      hyp_depth_flag(ihyp)=text(44:44)
      hyp_epi_flag(ihyp)=text(45:45)
      if( hyp_depth_flag(ihyp).ne.' '.and.hyp_depth_flag(ihyp).ne.'F'.
     *and.hyp_depth_flag(ihyp).ne.'S'.and.hyp_epi_flag(ihyp)  .ne.' '.
     *and.  hyp_epi_flag(ihyp).ne.'F'.and.hyp_epi_flag(ihyp).  ne.'*')
     *then
          err_text='hypocenter flags'
          goto 50
      endif
c
c   agency
c
      hyp_agency(ihyp)(1:3)=text(46:48)
c
c   number of stations
c
      err_text='number of stations'
      if(text(49:51).ne.' ')
     *read(text(49:51),'(i3)',err=50) hyp_nstat(ihyp)
c
c   rms
c
      err_text='rms'
      if(text(52:55).ne.' ')
     *read(text(52:55),'(f4.1)',err=50) hyp_rms(ihyp)
c
c   magnitudes
c
      err_text='magnitudes'
      if(text(56:63).ne.' ')
     *read(text(56:63),'(f4.1,a1,a3)',err=50)
     *hyp_mag(1,ihyp),hyp_mag_type(1,ihyp),hyp_mag_agency(1,ihyp)(1:3)

      if(text(64:71).ne.' ')
     *read(text(64:71),'(f4.1,a1,a3)',err=50)
     *hyp_mag(2,ihyp),hyp_mag_type(2,ihyp),hyp_mag_agency(2,ihyp)(1:3)

      if(text(72:79).ne.' ')
     *read(text(72:79),'(f4.1,a1,a3)',err=50)
     *hyp_mag(3,ihyp),hyp_mag_type(3,ihyp),hyp_mag_agency(3,ihyp)(1:3)

c
c   finish reading without errors
c
      goto 60
c
c   error
c

 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,' reading hyp line'
      write(6,'(a)') text
      stop

 60   continue
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea_hyph_in(text,ihyp)
c
c   read all hypocenter info from type H line (high accuracy hypoenter
c   line) in text into common block hypocenter array number ihyp
c
c   some checking is done
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      character*80   text
      character*20   err_text       ! error text
      integer        ihyp
c
      if(text(80:80).ne.'H') then
         write(6,*)' Not a high accuracy hypocenter line'
         stop
      endif
c
c   read times
c
      hyp_high_accuracy(ihyp)=.true.
      err_text='hypocenter time'
      if(text(2:5).ne.' ')   read(text(2:5),   '(i4)',  err=50)
     *hyp_year(ihyp)
      if(text(7:8).ne.' ' )  read(text(7:8),   '(i2)',  err=50)
     *hyp_month(ihyp)
      if(text(9:10).ne.' ' ) read(text(9:10),  '(i2)',  err=50)
     *hyp_day(ihyp)
      if(text(12:13).ne.' ') read(text(12:13), '(i2)',  err=50)
     *hyp_hour(ihyp)
      if(text(14:15).ne.' ') read(text(14:15), '(i2)',  err=50)
     *hyp_min(ihyp)
      if(text(17:23).ne.' ') read(text(17:23), '(f6.3)',err=50)
     *hyp_sec(ihyp)

c
c  hypocenter
c
      err_text='hypocenter'
      if(text(24:32).ne.' ') read(text(24:32),
     *'(f9.5)',err=50) hyp_lat(ihyp)
      if(text(34:43).ne.' ') read(text(34:43),
     *'(f10.5)',err=50) hyp_lon(ihyp)
      if(text(45:52).ne.' ') read(text(45:52),
     *'(f8.3)',err=50) hyp_depth(ihyp)
c
c   rms
c
      err_text='rms'
      if(text(54:59).ne.' ')
     *read(text(54:59),'(f6.3)',err=50) hyp_rms(ihyp)
c
c   finish reading without errors
c
      goto 60
c
c   error
c

 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,
     *' reading hyp high acc. line'
      write(6,'(a)') text
      stop

 60   continue
      return
      end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea_hype_in(text,ihyp)
c
c   read all error hypocenter info from type E line (error info
c   line) in text into common block hypocenter array number ihyp
c
c   some checking is done
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      character*80   text
      character*20   err_text       ! error text
      integer        ihyp
c
      if(text(80:80).ne.'E') then
         write(6,*)' Not a hypocenter error line'
         stop
      endif
c
      hyp_error(ihyp)=.true.
c
c   gap
c
      err_text='gap'
      if(text(6:8).ne.' ') then
          read(text(6:8),'(f3.0)',err=50) hyp_gap(ihyp)
      endif
c
c   read origin time error
c
      err_text='origin time error'
      if(text(15:20).ne.' ') read(text(15:20),'(f6.2)',err=50)
     *hyp_sec_err(ihyp)
c
c  hypocenter errors
c
      err_text='hypocenter error'
      if(text(25:30).ne.' ') read(text(25:33),
     *'(f6.1)',err=50) hyp_lat_err(ihyp)
      if(text(33:38).ne.' ') read(text(33:38),
     *'(f6.1)',err=50) hyp_lon_err(ihyp)
      if(text(39:43).ne.' ') read(text(39:43),
     *'(f5.1)',err=50) hyp_depth_err(ihyp)
c
c   covariance terms
c
      err_text='hypocent covariance'
      if(text(44:55).ne.' ')
     *read(text(44:55),'(e12.4)',err=50) hyp_cov(1,ihyp)
      if(text(56:67).ne.' ')
     *read(text(56:67),'(e12.4)',err=50) hyp_cov(2,ihyp)
      if(text(68:79).ne.' ')
     *read(text(68:79),'(e12.4)',err=50) hyp_cov(3,ihyp)

c
c   finish reading without errors
c
      goto 60
c
c   error
c

 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,
     *' reading hyp error line'
      write(6,'(a)') text
      stop

 60   continue
      return
      end



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea_hyp1_out(text,ihyp)
c
c   write all hypocenter info to type 1 line text from
c   common block array number ihyp
c
c   note: agencies are 5 chars of which only 3 is used
c
c   some checking is done
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      character*80   text
      character*20   err_text       ! error text
      integer        ihyp
c
c   clear
c
      text=' '
      text(80:80)='1'
c
c   write times
c
      err_text='hypocenter time'
      if(hyp_year(ihyp). gt.0  ) write(text(2:5),   '(i4)',  err=50)
     *hyp_year(ihyp)
      if(hyp_month(ihyp).gt.0  ) write(text(7:8),   '(i2)',  err=50)
     *hyp_month(ihyp)
      if(hyp_day(ihyp).  gt.0  ) write(text(9:10),  '(i2)',  err=50)
     *hyp_day(ihyp)
      if(hyp_hour(ihyp). ge.0  ) write(text(12:13), '(i2)',  err=50)
     *hyp_hour(ihyp)
      if(hyp_min(ihyp).  ge.0  ) write(text(14:15), '(i2)',  err=50)
     *hyp_min(ihyp)
      if(hyp_sec(ihyp).  ge.0.0) write(text(17:20), '(f4.1)',err=50)
     *hyp_sec(ihyp)
c
c   flags, check a bit
c
      text(21:21) = hyp_model(ihyp)
      text(22:22)=hyp_dist_id(ihyp)
      if(hyp_dist_id(ihyp).ne.'L'.and.hyp_dist_id(ihyp).ne.'R'.and.
     *hyp_dist_id(ihyp).ne.'D') then
         err_text='distance id'
         goto 50
      endif
      text(23:23)=hyp_type(ihyp)
      text(11:11)=hyp_fix_org(ihyp)
      if(hyp_fix_org(ihyp).ne.' '.and.hyp_fix_org(ihyp).ne.'F') then
         err_text='origin time flag'
         goto 50
      endif
c
c  hypocenter
c
      err_text='hypocenter'
      if(hyp_lat(ihyp).ne.-999.0) write(text(24:30),
     *'(f7.3)',err=50) hyp_lat(ihyp)
      if(hyp_lon(ihyp).ne.-999.0) write(text(31:38),
     *'(f8.3)',err=50) hyp_lon(ihyp)
      if(hyp_depth(ihyp).ge.0) then
         if(hyp_depth(ihyp).lt.10.0) then
            write(text(39:43),'(f5.2)',err=50) hyp_depth(ihyp)
         else
            write(text(39:43),'(f5.1)',err=50) hyp_depth(ihyp)
         endif
      endif
c
c   hypocenter flags
c
      text(44:44)=hyp_depth_flag(ihyp)
      text(45:45)=hyp_epi_flag(ihyp)
      if( hyp_depth_flag(ihyp).ne.' '.and.hyp_depth_flag(ihyp).ne.'F'.
     *and.hyp_depth_flag(ihyp).ne.'S'.and.hyp_epi_flag(ihyp)  .ne.' '.
     *and.  hyp_epi_flag(ihyp).ne.'F'.and.hyp_epi_flag(ihyp).  ne.'*')
     *then
          err_text='hypocenter flags'
          goto 50
      endif
c
c   agency
c
      text(46:48)=hyp_agency(ihyp)(1:3)
c
c   number of stations, if more than 1000, reduce to 999
c
      err_text='number of stations'
      if(hyp_nstat(ihyp).gt.1000) hyp_nstat(ihyp)=999
      if(hyp_nstat(ihyp).gt.0)
     *write(text(49:51),'(i3)',err=50) hyp_nstat(ihyp)
c
c   rms
c
      if(hyp_rms(ihyp).ge.0.0) then
         err_text='rms'
         if(hyp_rms(ihyp).ge.100.0) hyp_rms(ihyp)=99.0
         if(hyp_rms(ihyp).lt.1.0) then
             write(text(53:55),'(f3.2)',err=50) hyp_rms(ihyp)
         else

             write(text(52:55),'(f4.1)',err=50) hyp_rms(ihyp)
         endif
       endif
c
c   magnitudes
c
      err_text='magnitudes'
      if(hyp_mag(1,ihyp).gt.-100.0)
     *write(text(56:63),'(f4.1,a1,a3)',err=50)
     *hyp_mag(1,ihyp),hyp_mag_type(1,ihyp),hyp_mag_agency(1,ihyp)(1:3)

      if(hyp_mag(2,ihyp).gt.-100.0)
     *write(text(64:71),'(f4.1,a1,a3)',err=50)
     *hyp_mag(2,ihyp),hyp_mag_type(2,ihyp),hyp_mag_agency(2,ihyp)(1:3)

      if(hyp_mag(3,ihyp).gt.-100.0)
     *write(text(72:79),'(f4.1,a1,a3)',err=50)
     *hyp_mag(3,ihyp),hyp_mag_type(3,ihyp),hyp_mag_agency(3,ihyp)(1:3)


c
c   finish writing without errors
c
      goto 60
c
c   error
c

 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,' writing hyp line'
      write(6,'(a)') text
      stop

 60   continue
      return
      end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea_hypm_out(text,ihyp)
c
c   write  extra magnitude info (mag 3 to 6) type 1 to line text from
c   common block array number ihyp. The first 23 chars and the agency
c   is the same as for the first hypocenter line for this hypocenter
c   with index ihyp
c
c   note: agencies are 5 chars of which only 3 is used
c
c   some checking is done
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      character*80   text
      character*20   err_text       ! error text
      integer        ihyp
c
c   clear
c
      text=' '
      text(80:80)='1'
c
c   write times
c
      err_text='hypocenter time'
      if(hyp_year(ihyp). gt.0  ) write(text(2:5),   '(i4)',  err=50)
     *hyp_year(ihyp)
      if(hyp_month(ihyp).gt.0  ) write(text(7:8),   '(i2)',  err=50)
     *hyp_month(ihyp)
      if(hyp_day(ihyp).  gt.0  ) write(text(9:10),  '(i2)',  err=50)
     *hyp_day(ihyp)
      if(hyp_hour(ihyp). ge.0  ) write(text(12:13), '(i2)',  err=50)
     *hyp_hour(ihyp)
      if(hyp_min(ihyp).  ge.0  ) write(text(14:15), '(i2)',  err=50)
     *hyp_min(ihyp)
      if(hyp_sec(ihyp).  ge.0.0) write(text(17:20), '(f4.1)',err=50)
     *hyp_sec(ihyp)
c
c   flags, check a bit
c
      text(21:21) = hyp_model(ihyp)
      text(22:22)=hyp_dist_id(ihyp)
      if(hyp_dist_id(ihyp).ne.'L'.and.hyp_dist_id(ihyp).ne.'R'.and.
     *hyp_dist_id(ihyp).ne.'D') then
         err_text='distance id'
         goto 50
      endif
      text(23:23)=hyp_type(ihyp)
      text(11:11)=hyp_fix_org(ihyp)
      if(hyp_fix_org(ihyp).ne.' '.and.hyp_fix_org(ihyp).ne.'F') then
         err_text='origin time flag'
         goto 50
      endif
c
c   agency
c
      text(46:48)=hyp_agency(ihyp)(1:3)
c
c   magnitudes
c
      err_text='magnitudes'
      if(hyp_mag(4,ihyp).gt.-100.0)
     *write(text(56:63),'(f4.1,a1,a3)',err=50)
     *hyp_mag(1,ihyp),hyp_mag_type(1,ihyp),hyp_mag_agency(1,ihyp)(1:3)

      if(hyp_mag(5,ihyp).gt.-100.0)
     *write(text(64:71),'(f4.1,a1,a3)',err=50)
     *hyp_mag(2,ihyp),hyp_mag_type(2,ihyp),hyp_mag_agency(2,ihyp)(1:3)

      if(hyp_mag(6,ihyp).gt.-100.0)
     *write(text(72:79),'(f4.1,a1,a3)',err=50)
     *hyp_mag(3,ihyp),hyp_mag_type(3,ihyp),hyp_mag_agency(3,ihyp)(1:3)
c
c   finish writing without errors
c
      goto 60
c
c   error
c

 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,' writing hyp line'
      write(6,'(a)') text
      stop

 60   continue
      return
      end






cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea_hyph_out(text,ihyp)
c
c   write all hypocenter info to type H line text from
c   common block array number ihyp
c
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      character*80   text
      character*20   err_text       ! error text
      integer        ihyp
c
c   clear
c
      text=' '
      text(80:80)='H'
c
c   write times
c
      err_text='hypocenter time'
      if(hyp_year(ihyp). gt.0  ) write(text(2:5),   '(i4)',  err=50)
     *hyp_year(ihyp)
      if(hyp_month(ihyp).gt.0  ) write(text(7:8),   '(i2)',  err=50)
     *hyp_month(ihyp)
      if(hyp_day(ihyp).  gt.0  ) write(text(9:10),  '(i2)',  err=50)
     *hyp_day(ihyp)
      if(hyp_hour(ihyp). ge.0  ) write(text(12:13), '(i2)',  err=50)
     *hyp_hour(ihyp)
      if(hyp_min(ihyp).  ge.0  ) write(text(14:15), '(i2)',  err=50)
     *hyp_min(ihyp)
      if(hyp_sec(ihyp).  ge.0.0) write(text(17:22), '(f6.3)',err=50)
     *hyp_sec(ihyp)
c
c  hypocenter
c
      err_text='hypocenter'
      if(hyp_lat(ihyp).ne.-999.0) write(text(24:32),
     *'(f9.5)',err=50) hyp_lat(ihyp)
      if(hyp_lon(ihyp).ne.-999.0) write(text(34:43),
     *'(f10.5)',err=50) hyp_lon(ihyp)
      if(hyp_depth(ihyp).ge.0) 
     *write(text(45:52),'(f8.3)',err=50) hyp_depth(ihyp)
c
c   rms
c
      if(hyp_rms(ihyp).ge.0.0) then
         err_text='rms'
         if(hyp_rms(ihyp).gt.100.0) hyp_rms(ihyp)=99.0
         write(text(54:59),'(f6.3)',err=50) hyp_rms(ihyp)
       endif
c
c   finish writing without errors
c
      goto 60
c
c   error
c

 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,
     *' writing hyp high accuracy line'
      write(6,'(a)') text
      stop

 60   continue
      return
      end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea_hype_out(text,ihyp)
c
c   read all error hypocenter info from type E line (error info
c   line) in text into common block hypocenter array number ihyp
c
c   some checking is done
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      character*80   text
      character*20   err_text       ! error text
      integer        ihyp
c

      text=' '
      text(80:80)='E'

c
c   gap
c
      err_text='gap'
      if(hyp_gap(ihyp).ge.0.0) then
          text(2:5)='GAP='
          write(text(6:8),'(i3)',err=50) int(hyp_gap(ihyp)+0.5)
      endif
c
c   write origin time error
c
      err_text='origin time error'
      if(hyp_sec_err(ihyp).ge.0.0) then
         if(hyp_sec_err(ihyp).gt.1000.0) hyp_sec_err(ihyp)=999.0
         if(hyp_sec_err(ihyp).gt.1.0) 
     *   write(text(15:20),'(f6.2)',err=50) hyp_sec_err(ihyp)
         if(hyp_sec_err(ihyp).le.1.0) 
     *   write(text(15:20),'(f6.3)',err=50) hyp_sec_err(ihyp)
       endif
c
c  hypocenter errors
c
      err_text='hypocenter error'
      if(hyp_lat_err(ihyp).ge.0.0) then
         if(hyp_lat_err(ihyp).gt.1000.0) hyp_lat_err(ihyp)=999.0
         if(hyp_lat_err(ihyp).le.1.0) write(text(25:33),
     *   '(f6.3)',err=50) hyp_lat_err(ihyp)
         if(hyp_lat_err(ihyp).gt.1.0) write(text(25:33),
     *   '(f6.1)',err=50) hyp_lat_err(ihyp)
      endif

      if(hyp_lon_err(ihyp).ge.0.0) then
         if(hyp_lon_err(ihyp).gt.1000.0) hyp_lon_err(ihyp)=999.0
         if(hyp_lon_err(ihyp).le.1.0) write(text(33:38),
     *   '(f6.3)',err=50) hyp_lon_err(ihyp)
         if(hyp_lat_err(ihyp).gt.1.0) write(text(33:38),
     *   '(f6.1)',err=50) hyp_lon_err(ihyp)
      endif

      if(hyp_depth_err(ihyp).ge.0.0) then
         if(hyp_depth_err(ihyp).gt.1000.0) hyp_depth_err(ihyp)=999.0
         if(hyp_depth_err(ihyp).le.1.0) write(text(39:43),
     *   '(f5.3)',err=50) hyp_depth_err(ihyp)
         if(hyp_depth_err(ihyp).gt.1.0) write(text(39:43),
     *   '(f5.1)',err=50) hyp_depth_err(ihyp)
      endif
 
c
c   covariance terms
c
      err_text='hypocent covariance'
      if(hyp_cov(1,ihyp).ge.-1.0e9)
     *write(text(44:55),'(e12.4)',err=50) hyp_cov(1,ihyp)
      if(hyp_cov(2,ihyp).ge.-1.0e9)
     *write(text(56:67),'(e12.4)',err=50) hyp_cov(2,ihyp)
      if(hyp_cov(3,ihyp).ge.-1.0e9)
     *write(text(68:79),'(e12.4)',err=50) hyp_cov(3,ihyp)

c
c   finish reading without errors
c
      goto 60
c
c   error
c

 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,
     *' writing hyp error line'
      write(6,'(a)') text
      stop

 60   continue
      return
      end




cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine rea_hyp_clear(ihyp)
c
c   initialize hyp1 line parameters for one entry at index ihyp,
c   character items are set to blanks and numbers to -999 excepth
c   covariace element which is set to -9.9e10
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      integer ihyp,i

      hyp_year(ihyp)=-999              ! hypocenter year
      hyp_month(ihyp)=-999
      hyp_day(ihyp)=-999
      hyp_hour(ihyp)=-999
      hyp_min(ihyp)=-999
      hyp_sec(ihyp)=-999.0
      hyp_model(ihyp)=' '             ! model indicator
      hyp_dist_id(ihyp)=' '           ! distance indicator
      hyp_type(ihyp)=' '              ! event type like E
      hyp_fix_org(ihyp)=' '           ! fix origin time flag
      hyp_lat(ihyp)=-999.0            ! latitude
      hyp_lon(ihyp)=-999.0            ! longitude
      hyp_depth(ihyp)=-999.0          ! depth
      hyp_depth_flag(ihyp)=' '        ! depth flag
      hyp_epi_flag(ihyp)=' '          ! epicenter flag
      hyp_agency(ihyp)=' '            ! hypocenter agency, use 3 only
      hyp_nstat(ihyp)=-999            ! number of station
      hyp_rms(ihyp)=-999.0            ! rms of hypocenter solution
      do i=1,6
         hyp_mag(i,ihyp)=-999.0       ! magnitudes
         hyp_mag_type(i,ihyp)=' '     ! magnitude types
         hyp_mag_agency(i,ihyp)=' '   ! magnitude agencies
      enddo
      hyp_high_accuracy(ihyp)=.false. ! high accurcy flag
      hyp_error(ihyp)=.false.         ! error flag
      do i=1,3
         hyp_cov(i,ihyp)= -9.9e10     ! covarieance elements
      enddo
      hyp_gap(ihyp)=-999.0            ! gap
      hyp_sec_err(ihyp)=-999.0        ! origin time error
      hyp_lat_err(ihyp)=-999.0        ! hypocenter errors
      hyp_lon_err(ihyp)=-999.0
      hyp_depth_err(ihyp)=-999.0
      hyp_auto(ihyp)=' '              ! not an automatic parameter

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      subroutine rea_hyp_inp(data,nhead)
c
c   read all hypocenter info from data array nhead header lines into
c   common block hypocenter arrays
c
c      implicit none
c      include 'seidim.inc'
c      include 'rea.inc'
c      return
c     end



      subroutine rea_phase_out(iphas,text)
c
c   write one phase line from rea common, the phase number is iphas
c   and it is written to text, some checking of the values is
c   done, if errors the program stops
c
c   the time to write out is taken from the hr, min, sec. if not
c   available (still have initial values) use abs time. Be aware that
c   if a phase time is just after midnight, oriign time before midninght,
c   the hour must be 24 or more. So the abs time migh give an undesirded
c   effect
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      include 'seisan.inc'
      character*80 text
      integer iphas,i

      text=' '
c
c   if phase is a spectrum or deleted, do not write out
c
      if(rea_phase(iphas)(1:4).eq.'SPEC'   .or.
     *   rea_phase(iphas)(1:6).eq.'DELETE') return

c
c   check which time to use
c
      if((rea_hour(iphas).eq.-999.0.or.rea_min(iphas).eq.-999.0).
     * and.rea_abs_time(iphas).gt.0.0)
     * call sectim(rea_abs_time(iphas),rea_year(iphas),i,
     * rea_month(iphas),rea_day(iphas),rea_hour(iphas),
     * rea_min(iphas),rea_sec(iphas))
c
c   station
c
      text(2:6)=rea_stat(iphas)
c
c   component, if short notion is not there, try to make it
c

      if(rea_co(iphas).eq.' ') then
         call component(rea_comp(iphas),rea_co(iphas))
      endif
      text(7:8)=rea_co(iphas)
c
c   onset
c
      text(10:10)=rea_onset(iphas)


c
c   phase name can be long in which case no input polarity can be given
c   and weight is put in column 9
c
      if(rea_phase(iphas)(5:8).ne.' ') then
         text(11:18)=rea_phase(iphas)
         text(9:9)=rea_weight_in(iphas)
      else
         text(11:14)=rea_phase(iphas)(1:4)
         text(15:15)=rea_weight_in(iphas)
         text(17:17)=rea_polarity(iphas)
      endif
c
c   automatic process
c
      if(rea_auto(iphas).ne.' ') text(16:16)='A'
c
c   hour
c
      if(rea_hour(iphas).ge.0) then
          write(text(19:20),'(i2)',err=5) rea_hour(iphas)
          goto 6
 5        continue
             write(6,*)' Error writing time, hr:',rea_hour(iphas)
             write(6,'(a)') text
             stop
 6        continue
       endif
c
c   min
c
      if(rea_min(iphas).ge.0) then
          write(text(21:22),'(i2)',err=7) rea_min(iphas)
          goto 8
 7        continue
             write(6,*)' Error writing time, min:',rea_min(iphas)
             write(6,'(a)') text
             stop
 8        continue
       endif
       
c
c  seconds, can be high accuracy
c
      if(rea_sec(iphas).ge.0.0) then
         if(high_accuracy) then
            write(text(23:28),'(f6.3)',err=10) rea_sec(iphas)
         else
            write(text(24:28),'(f5.2)',err=10) rea_sec(iphas)
         endif
         goto 11
 10      continue
            write(6,*)' Error writing time, sec:',rea_sec(iphas)
            write(6,'(a)') text
         stop
 11      continue
      endif
c
c   coda
c
      if(rea_coda(iphas).gt.0.0) then
         write(text(30:33),'(i4)',err=15) int(rea_coda(iphas))
         goto 16
 15      continue
           write(6,*)' Error writing coda:', rea_coda(iphas)
           write(6,'(a)') text
           stop
 16      continue
      endif
c
c  amplitude, amplitude must be formated to a range of values
c

      if(rea_amp(iphas).gt.0.0) then
c
c   check size of amplitude                                                 
c        
         if(rea_amp(iphas).ne.0..and.rea_amp(iphas).lt.100000.)                   
     +   write(text(34:40),'(f7.1)')
     +   rea_amp(iphas)
         if(rea_amp(iphas).ne.0..and.rea_amp(iphas).ge.1.0e5
     +      .and.rea_amp(iphas).lt.1.0e7)then
            rea_amp(iphas)=rea_amp(iphas)/1.0e4                 
            write(text(34:40),'(f5.1,a)')
     +      rea_amp(iphas),'e4'
         endif
         if(rea_amp(iphas).ne.0..and.rea_amp(iphas).ge.1.0e7
     +      .and.rea_amp(iphas).lt.1.0e9)then
            rea_amp(iphas)=rea_amp(iphas)/1.0e6                 
            write(text(34:40),'(f5.1,a)')
     +      rea_amp(iphas),'e6'
         endif
         if(rea_amp(iphas).ne.0..and.rea_amp(iphas).ge.1.0e9
     +      .and.rea_amp(iphas).lt.1.0e11)then
            rea_amp(iphas)=rea_amp(iphas)/1.0e8                 
            write(text(34:40),'(f5.1,a)')
     +      rea_amp(iphas),'e8'
         endif
         if(rea_amp(iphas).ge.1.0e11) then
             write(6,*)' Amplitude larger than 100 meters, unrealistic'
             stop
         endif
      endif
c
c   period
c
      if(rea_per(iphas).gt.0.0) then
         if(rea_per(iphas).lt.10.0) then
             write(text(42:45),'(f4.2)',err=20) rea_per(iphas)
         else
             write(text(42:45),'(f4.1)',err=20) rea_per(iphas)
         endif
         goto 21
 20      continue
            write(6,*)' Error writing period:', rea_per(iphas)
            write(6,'(a)') text
            stop
 21      continue
      endif
c
c   back azimuth
c
      if(rea_baz_obs(iphas).ne.-999.0) then
         write(text(47:51),'(f5.1)',err=25) rea_baz_obs(iphas)
         goto 26
 25      write(6,*)' Error writing observed back azimuth:',
     *   rea_baz_obs(iphas)
         write(6,'(a)') text
         stop
 26   continue
      endif
c
c  apparent velocity
c
      if(rea_vel(iphas).ne.-999.0) then
         write(text(53:56),'(f4.1)',err=30) rea_vel(iphas)
         goto 31
 30      write(6,*)' Error writing apparent velocity:',
     *   rea_vel(iphas)
         write(6,'(a)') text
         stop
 31   continue
      endif
c
c  signal to noise ratio, reduce to 999 if larger than 1000
c
      if(rea_sn(iphas).ge.0.0) then
         if(rea_sn(iphas).gt.1000.0) rea_sn(iphas)=999.0 ! max 1000
         if(rea_sn(iphas).lt.100.0) then
             write(text(57:60),'(f4.1)',err=35) rea_sn(iphas)
         else
             write(text(57:60),'(f4.0)',err=35) rea_sn(iphas)
         endif
         goto 36
 35      write(6,*)' Error writing signal to noise ratio:',
     *   rea_sn(iphas)
         write(6,'(a)') text
         stop
 36   continue
      endif
c
c  azimuth residual, if negative convert to positive if smaller than -99
c
      if(rea_baz_res(iphas).ne.-999.0) then
         if(rea_baz_res(iphas).lt.-99.0)
     *   rea_baz_res(iphas)=rea_baz_res(iphas)+360.0
         if(rea_baz_res(iphas).gt.0.0) then
            write(text(61:63),'(i3)',err=40) int(rea_baz_res(iphas)+0.5)
         else
            write(text(61:63),'(i3)',err=40) int(rea_baz_res(iphas)-0.5)
         endif
         goto 41
 40      write(6,*)' Error writing back azimuth residual:',
     *   rea_baz_res(iphas)
         write(6,'(a)') text
         stop
 41   continue
      endif
c
c   travel time residual, values larger than 100 are truncated to 99.0
c
      if(rea_res(iphas).ne.-999.0) then
         if(rea_res(iphas).gt.100.0)  rea_res(iphas)=99.0
         if(rea_res(iphas).lt.-100.0) rea_res(iphas)=-99.0
         if(abs(rea_res(iphas)).lt.1.0)
     *      write(text(64:68),'(f5.3)',err=45) rea_res(iphas)
         if(abs(rea_res(iphas)).lt.10.0.and.abs(rea_res(iphas)).ge.1.0)
     *      write(text(64:68),'(f5.2)',err=45) rea_res(iphas)
         if(abs(rea_res(iphas)).lt.100.0.and.abs(rea_res(iphas)).
     *      ge.10.0)
     *      write(text(64:68),'(f5.1)',err=45) rea_res(iphas)
         goto 46
 45         write(6,*)' Error writing travel time residual:',
     *      rea_res(iphas)
            write(6,'(a)') text
            stop
 46      continue
      endif
      
c
c   output weight
c
      if(rea_weight_out(iphas).ne.' ') then
         read(rea_weight_out(iphas),'(i2)',err=50) i
         text(69:70)=rea_weight_out(iphas)
         goto 51
 50      continue
            write(6,'(a2)')' Error with output weight:',
     *         rea_weight_out(iphas)
               write(6,'(a)') text
               stop
 51         continue
      endif
c
c  azimuth at source, if negative convert to positive
c
      if(rea_az(iphas).ne.-999.0) then
         if(rea_az(iphas).lt.0.0)
     *   rea_az(iphas)=rea_az(iphas)+360.0
         write(text(77:79),'(i3)',err=55) int(rea_az(iphas)+0.5)
         goto 56
 55         write(6,*)' Error writing azimuth:',
     *      rea_az(iphas)
            write(6,'(a)') text
            stop
 56      continue
      endif
c
c   epicentral distance
c
      if(rea_dist(iphas).ge.0.0) then
         if(rea_dist(iphas).lt.10.0)
     *      write(text(71:75),'(f5.3)',err=60) rea_dist(iphas)
         if(rea_dist(iphas).lt.100.0.and.rea_dist(iphas).ge.10.0)
     *      write(text(71:75),'(f5.2)',err=60) rea_dist(iphas)
         if(rea_dist(iphas).lt.1000.0.and.rea_dist(iphas).
     *      ge.100.0)
     *      write(text(71:75),'(f5.1)',err=60) rea_dist(iphas)
         if(rea_dist(iphas).ge.1000.0)
     *      write(text(71:75),'(i5)',err=60) int(rea_dist(iphas))
         goto 61
 60         write(6,*)' Error writing epicentral distance',
     *      rea_dist(iphas)
            write(6,'(a)') text
            stop
 61      continue
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine rea_phase_in(iphas,text)
c
c   reads one phase line from text line text,
c   the phase number to put info in is iphas,
c   data are read to rea-common block variables,
c   if errors the program stops
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      character*80 text
      integer iphas,i
c
      if(text(80:80).ne.' ') then
          write(6,*) ' Not a phase line'
          write(6,*) text
c          stop    ! commented out, lo
      endif
c
c   clear variables with phase parameters
c
      call rea_phase_clear(iphas)
c
c   station
c
      rea_stat(iphas)=text(2:6)
c
c   component, if short notion is not there, try to make it
c
      rea_co(iphas)=text(7:8)
c
c   onset
c
      rea_onset(iphas)=text(10:10)
c
c   phase name can be long in which case no input polarity can be given
c   and weight is in column 9
c
      if(text(9:9).ne.' '.or.(text(14:14).ne.' '.and.
     * text(15:18).ne.' '.and.text(17:17).ne.'C'.and.text(17:17).
     * ne.'D'.and.text(15:15).ne.' '.and.text(15:15).ne.'0'.and.
     * text(15:15).ne.'1'.and.text(15:15).ne.'2'.and.text(15:15).
     * ne.'3'.and.text(15:15).ne.'4'.and.text(15:15).ne.'9')) then
         rea_phase(iphas)=text(11:18)
         rea_weight_in(iphas)=text(9:9)
      else
         rea_phase(iphas)(1:4)=text(11:14)
         rea_weight_in(iphas)=text(15:15)
         rea_polarity(iphas)=text(17:17)
      endif
c
c auto phase
c
      if(text(16:16).eq.'A') rea_auto(iphas)='auto'

c
c   hour
c
      if(text(19:20).ne.' ') then
          read(text(19:20),'(i2)',err=5) rea_hour(iphas)
          goto 6
 5        continue
             write(6,*)' Error reading hour'
             write(6,'(a)') text
             stop
 6        continue
       endif
c
c   min
c
      if(text(21:22).ne.' ') then
          read(text(21:22),'(i2)',err=7) rea_min(iphas)
          goto 8
 7        continue
             write(6,*)' Error reading min:'
             write(6,'(a)') text
             stop
 8        continue
       endif
       
c
c  seconds
c
      if(text(23:28).ne.' ') then
         read(text(23:28),'(f6.3)',err=10) rea_sec(iphas)
         goto 11
 10      continue
            write(6,*)' Error reading secc:'
            write(6,'(a)') text
         stop
 11      continue
      endif
c
c   coda
c
      if(text(30:33).ne.' ') then
         read(text(30:33),'(f4.0)',err=15) rea_coda(iphas)
         goto 16
 15      continue
           write(6,*)' Error reading coda:'
           write(6,'(a)') text
           stop
 16      continue
      endif
c
c  amplitude
c

      if(text(34:40).ne.' ') then
          read(text(34:40),'(g7.1)',err=17) rea_amp(iphas)
          goto 18
 17       continue
             write(6,*)' Error reading amplitude'
             write(6,'(a)') text
 18       continue
      endif
c
c   period
c
      if(text(42:45).ne.' ') then
         read(text(42:45),'(f4.2)',err=20) rea_per(iphas)
         goto 21
 20      continue
            write(6,*)' Error reading period'
            write(6,'(a)') text
            stop
 21      continue
      endif
c
c   back azimuth
c
      if(text(47:51).ne.' ') then
         read(text(47:51),'(f5.1)',err=25) rea_baz_obs(iphas)
         goto 26
 25      write(6,*)' Error reading observed back azimuth:'
         write(6,'(a)') text
         stop
 26   continue
      endif
c
c  apparent velocity
c
      if(text(53:56).ne.' ') then
         read(text(53:56),'(f4.1)',err=30) rea_vel(iphas)
         goto 31
 30      write(6,*)' Error reading apparent velocity:'
         write(6,'(a)') text
         stop
 31   continue
      endif
c
c  signal to noise ratio
c
      if(text(57:60).ne.' ') then
         read(text(57:60),'(f4.1)',err=35) rea_sn(iphas)
         goto 36
 35         write(6,*)' Error reading signal to noise ratio:'
            write(6,'(a)') text
            stop
 36      continue
      endif
c
c  azimuth residual
c
      if(text(61:63).ne.' ') then
         read(text(61:63),'(f3.0)',err=40) rea_baz_res(iphas)
         goto 41
 40         write(6,*)' Error readin back azimuth residual:'
            write(6,'(a)') text
            stop
 41      continue
      endif
c
c   travel time residual
c
      if(text(64:69).ne.' ') then
         read(text(64:68),'(f5.3)',err=45) rea_res(iphas)
         goto 46
 45         write(6,*)' Error reading travel time residual:'
            write(6,'(a)') text
            stop
 46      continue
      endif
      

c
c   output weight
c
      read(text(69:70),'(i2)',err=50) i
      rea_weight_out(iphas)=text(69:70)
      goto 51
 50   continue
         write(6,*)' Error reading output weight:'
         write(6,'(a)') text
         stop
 51   continue
c
c  azimuth at source
c
      if(text(77:79).ne.' ') then
         read(text(77:79),'(f3.0)',err=55) rea_az(iphas)
         goto 56
 55         write(6,*)' Error reading azimuth:'
            write(6,'(a)') text
            stop
 56      continue
      endif
c
c   epicentral distance
c
      if(text(71:75).ne.' ') then
         read(text(71:75),'(f5.0)',err=60) rea_dist(iphas)
         goto 61
 60         write(6,*)' Error reading epicentral distance'
            write(6,'(a)') text
            stop
 61      continue
      endif

      return
      end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine rea_phase_clear(iphas)
c
c   initialize phase line parameters for one entry at index iphas,
c   character items are set to blanks and numbers to -999
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      integer iphas
c
      rea_stat(iphas)=' '        ! station codes
      rea_comp(iphas)=' '        ! componenets
      rea_co(iphas)= ' '         ! 2 letter componenets
      rea_phase(iphas)= ' '      ! phase name
      rea_onset(iphas)=  ' '     ! onset I or E or blank
      rea_weight_in(iphas)= ' '  ! input weight
      rea_weight_out(iphas)=' '  ! weight out
      rea_polarity(iphas)=' '    ! polarity, D or C
      rea_year(iphas)=-999
      rea_month(iphas)=-999
      rea_day(iphas)=-999
      rea_hour(iphas)= -999
      rea_min(iphas)= -999
      rea_sec(iphas)= -999
      rea_abs_time(iphas)=-999   ! abs phase time
      rea_coda(iphas)= -999      ! coda length in s
      rea_amp(iphas)=  -999      ! amplitude in nm
      rea_per(iphas)=  -999      ! period of amplitude
      rea_baz_obs(iphas)= -999   ! observed back azimuth
      rea_baz_cal(iphas)= -999   ! calculated back azimuth
      rea_vel(iphas)= -999       ! observed apparent velocity
      rea_sn(iphas)=  -999       ! observed signal to noise ratio
      rea_baz_res(iphas)= -999   ! back azimuth residual
      rea_res(iphas)=-999        ! travel time residual
      rea_dist(iphas)= -999      ! epicentral distance
      rea_az(iphas)=   -999      ! azimuth
c

      rea_moment(iphas)=-999     ! log moment, Nm
      rea_sdrop(iphas)=-999      ! stress drop, bar
      rea_omega0(iphas)=-999     ! log spectral flat level, ns
      rea_cornerf(iphas)=-999    ! corner f
      rea_radius(iphas)=-999     ! source radius
      rea_swin(iphas)=-999       ! window lenght used
      rea_geo_dist(iphas)=-999   ! geo distance, km
      rea_vs(iphas)=-999         ! s-velocity at source, km/s
      rea_vp(iphas)=-999         ! p-velocity at source, km/s
      rea_q0(iphas)=-999         ! q0
      rea_qalpha(iphas)=-999     ! q alpha
      rea_kappa(iphas)=-999      ! kappa
      rea_density(iphas)=-999    ! density g/cm**3
      rea_slope(iphas)=-999      ! - measured slope of spectrum
      rea_mc(iphas)=-999         ! coda
      rea_ml(iphas)=-999         ! local
      rea_mb(iphas)=-999         ! mb
      rea_ms(iphas)=-999         ! ms
      rea_mw(iphas)=-999         ! mw
      rea_auto(iphas)=' '        ! not an automatic parameter
      

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea_spec_in(iphas,text1,text2)
c
c   read spectral info to rea common, the phase number is iphas
c   and it is read from text1 and 2, some checking of the values is
c   done, if errors the program stops
c
c
c   there are two spectral phases defined: SPECP: P-spectrum
c                                          SPECS: S-spectrum
c
c   the time read in is put in normal phase variables
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      character*80 text1,text2
      integer iphas,i
      real x                  ! help variable
      character*20 err_text   ! error text

c
c   clear variables
c
      call rea_phase_clear(iphas)

c
c   check if text strings are spectral strings and belong together
c
      if(text1(2:5).ne.'SPEC'.or.text2(2:5).ne.'SPEC'.or.
     *   text1(80:80).ne.'3'.or.text2(80:80).ne.'3'.or.
     *   text1(7:14).ne.text2(7:14)) then
         write(6,*)
     *   ' Two spec lines of wrong type or do not belong together'
         write(6,'(a)') text1
         write(6,'(a)') text2
c         stop
       endif
        
c
c   station
c
      rea_stat(iphas)(1:4)=text1(7:10)  ! not room for 5. letter
c
c   component
c
      rea_comp(iphas)=text1(11:14)

c
c   moment
c

      err_text='moment'
      if(text1(16:17).ne.'MO') goto 50
      if(text1(18:22).ne.' ') 
     *read(text1(18:22),'(f5.1)',err=50) rea_moment(iphas)
      
c
c   stress drop
c
      err_text='stress drop'
      if(text1(24:25).ne.'ST') goto 50
      if(text1(26:30).ne.' ') 
     *read(text1(26:30),'(f5.1)',err=50) rea_sdrop(iphas)
c
c   omega0
c
      err_text='omega0'
      if(text1(32:33).ne.'OM') goto 50
      if(text1(34:38).ne.' ')
     *read(text1(34:38),'(f5.1)',err=50) rea_omega0(iphas)
c
c   corner frequency
c
      err_text='corner frequency'
      if(text1(40:41).ne.'f0') goto 50
      if(text1(42:46).ne.' ') 
     *read(text1(42:46),'(f5.2)',err=50) rea_cornerf(iphas)
c
c   radius
c
      err_text='source radius'
      if(text1(48:48).ne.'R') goto 50
      if(text1(50:54).ne.' ') 
     *read(text1(50:54),'(f5.1)',err=50) rea_radius(iphas)

c
c   specteral slope, abs value
c
      err_text='spectral slope'
      if(text1(56:57).ne.'AL') goto 50
      if(text1(58:62).ne.' ') then 
         read(text1(58:62),'(f5.2)',err=50) rea_slope(iphas)
         rea_slope(iphas)=-rea_slope(iphas)
      endif
c
c   window length
c
      err_text='spectral window'
      if(text1(64:65).ne.'WI') goto 50
      if(text1(66:70).ne.' ') 
     *read(text1(66:70),'(f5.2)',err=50) rea_swin(iphas)
c
c   moment magnitude
c
      err_text='moment magnitude'
      if(text1(72:73).ne.'MW') goto 50
      if(text1(74:78).ne.' ')
     *read(text1(74:78),'(f5.1)',err=50) rea_mw(iphas)
c
c-----------------------------
c   second spec line
c-----------------------------
c

c
c   start of window
c
      err_text='window start time'
      if(text2(16:16).ne.'T') goto 51
      if(text2(17:22).ne.' ') then
         read(text2(17:22),'(3i2)',err=51) rea_hour(iphas),
     *    rea_min(iphas),i
          rea_sec(iphas)=i
      endif
c
c   kappa
c
      err_text='kappa'
      if(text2(24:25).ne.'K ') goto 51
      if(text2(26:30).ne.' ')
     *read(text2(26:30),'(f5.3)',err=51) rea_kappa(iphas)
c
c   geodistance
c
      err_text='geo distance'
      if(text2(32:33).ne.'GD') goto 51
      if(text2(34:38).ne.' ') 
     *read(text2(34:38),'(f5.1)',err=51) rea_geo_dist(iphas)
c
c   velocity for spectrum and spectral phases
c
      err_text='source velocity'
      if(text2(40:40).ne.'V ') goto 51
      rea_phase(iphas)='SPEC '
      rea_phase(iphas)(5:5)=text2(41:41)   ! type of spectrum
      if(text2(42:46).ne.' ') then
         read(text2(42:46),'(f5.2)',err=51) x
c
c   assign velocity to right velocity
c
         if(rea_phase(iphas)(5:5).eq.'S') rea_vs(iphas)=x  
         if(rea_phase(iphas)(5:5).eq.'P') rea_vp(iphas)=x
      endif
c
c   density
c
      err_text='density'
      if(text2(48:49).ne.'DE') goto 51
      if(text2(50:54).ne.' ') 
     *read(text2(50:54),'(f5.2)',err=51) rea_density(iphas)
c
c    q0
c
      err_text='q0'
      if(text2(56:57).ne.'Q0') goto 51
      if(text2(58:62).ne.' ') 
     *read(text2(58:62),'(f5.1)',err=51) rea_q0(iphas)
c
c   q-alpha
c
      err_text='q-alpha'
      if(text2(64:65).ne.'QA') goto 51
      if(text2(66:70).ne.' ') 
     *read(text2(66:70),'(f5.2)',err=51) rea_qalpha(iphas)
c
c   s-velocity
c
      err_text='S-velocty'
      if(text2(72:73).ne.'VS') goto 51
      if(text2(74:78).ne.' ') 
     *read(text2(74:78),'(f5.2)',err=51) rea_vs(iphas)
c
      goto 52


 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,' reading spec line'
      write(6,'(a)') text1
c      stop
 51   continue
      write(6,'(a,a,a)') 'Error with ',err_text,' reading spec line'
      write(6,'(a)') text2
c      stop

 52   continue
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea_av_spec_in(text)
c
c   read spectral average info to rea common, the phase number is iphas
c   and it is read from text1,  some checking of the values is
c   done, if errors the program stops
c
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      character*80 text
      character*20 err_text   ! error text


c
c  check if correct string
c
      if(text(2:13).ne.'SPEC AVERAGE') then
          write(6,*)' Not a spectral average input string'
          write(6,'(a)') text
          stop
      endif
c
c   clear variables
c
      rea_av_moment=-999.0
      rea_av_sdrop=-999.0
      rea_av_omega0=-999.0
      rea_av_cornerf=-999.0
      rea_av_radius=-999.0
      rea_av_slope=-999.0
      rea_av_swin=-999.0
      rea_av_mw=-999.0
c
c   moment
c

      err_text='moment'
      if(text(16:17).ne.'MO') goto 50
      if(text(18:22).ne.' ') 
     *read(text(18:22),'(f5.1)',err=50) rea_av_moment
      
c
c   stress drop
c
      err_text='stress drop'
      if(text(24:25).ne.'ST') goto 50
      if(text(26:30).ne.' ') 
     *read(text(26:30),'(f5.1)',err=50) rea_av_sdrop
c
c   omega0
c
      err_text='omega0'
      if(text(32:33).ne.'OM') goto 50
      if(text(34:38).ne.' ')
     *read(text(34:38),'(f5.1)',err=50) rea_av_omega0
c
c   corner frequency
c
      err_text='corner frequency'
      if(text(40:41).ne.'f0') goto 50
      if(text(42:46).ne.' ') 
     *read(text(42:46),'(f5.2)',err=50) rea_av_cornerf
c
c   radius
c
      err_text='source radius'
      if(text(48:48).ne.'R') goto 50
      if(text(50:54).ne.' ') 
     *read(text(50:54),'(f5.1)',err=50) rea_av_radius

c
c   specteral slope, abs value
c
      err_text='spectral slope'
      if(text(56:57).ne.'AL') goto 50
      if(text(58:62).ne.' ') then 
         read(text(58:62),'(f5.2)',err=50) rea_av_slope
         rea_av_slope=-rea_av_slope
      endif
c
c   window length
c
      err_text='spectral window'
      if(text(64:65).ne.'WI') goto 50
      if(text(66:70).ne.' ') 
     *read(text(66:70),'(f5.2)',err=50) rea_av_swin
c
c   moment magnitude
c
      err_text='moment magnitude'
      if(text(72:73).ne.'MW') goto 50
      if(text(74:78).ne.' ') 
     *read(text(74:78),'(f5.1)',err=50) rea_av_mw

      goto 51

 50   continue
      write(6,'(a,a,a)')
     *'Error with ',err_text,' reading average spec line'
      write(6,'(a)') text
      stop

 51   continue
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea_spec_out(iphas,text1,text2)
c
c   write spectral info from rea common, the phase number is iphas
c   and it is written to text, some checking of the values is
c   done, if errors the program stops
c
c
c   there are two spectral phases defined: SPECP: P-spectrum
c                                          SPECS: S-spectrum

c   the time to write out is taken from the hr, min, sec. if not
c   available (still have initial values) use abs time
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      character*80 text1,text2
      integer iphas,i
      real x                  ! help variable
      character*20 err_text   ! error text

      text1=' '
      text2=' '

c
c   check which time to use
c
      if((rea_hour(iphas).eq.-999.0.or.rea_min(iphas).eq.-999.0).
     * and.rea_abs_time(iphas).gt.0.0)
     * call sectim(rea_abs_time(iphas),rea_year(iphas),i,
     * rea_month(iphas),rea_day(iphas),rea_hour(iphas),
     * rea_min(iphas),rea_sec(iphas))
c
c   fixed values
c
      text1(2:5)='SPEC'
      text1(80:80)='3'
c
c   check if an auto process
c
      if(rea_auto(iphas).ne.' ') text1(6:6)='A'
c
c   station
c
      text1(7:10)=rea_stat(iphas)(1:4)  ! not room for 5. letter
c
c   component
c
      text1(11:14)=rea_comp(iphas)
c
c   same content
c
      text2=text1
c
c   check for overflow
c
      if(rea_sdrop(iphas).gt.999.0)  rea_sdrop(iphas)=999.0
      if(rea_cornerf(iphas).gt.99.0) rea_cornerf(iphas)=99.0
      if(rea_omega0(iphas).gt.999.0) rea_omega0(iphas)=999.0
      if(abs(rea_slope(iphas)).gt.999.0)   rea_slope(iphas)=999.0
      if(rea_geo_dist(iphas).gt.99999.) rea_geo_dist(iphas)=99999.0
      if(rea_radius(iphas).gt.999.0) rea_radius(iphas)=999.0
      if(rea_swin(iphas).gt.999.0) rea_swin(iphas)=999.0

c
c   moment
c
      text1(16:17)='MO'
      if(rea_moment(iphas).gt.0.0) then
         err_text='moment'
         write(text1(18:22),'(f5.1)',err=50) rea_moment(iphas)
      endif
c
c   stress drop
c
      text1(24:25)='ST'
      if(rea_sdrop(iphas).gt.0.0) then
         err_text='stress drop'
         write(text1(26:30),'(f5.1)',err=50) rea_sdrop(iphas)
      endif
c
c   omega0
c
      text1(32:33)='OM'
      if(rea_omega0(iphas).gt.0.0) then
         err_text='omega0'
         write(text1(34:38),'(f5.1)',err=50) rea_omega0(iphas)
      endif
c
c   corner frequency
c
      text1(40:41)='f0'
      if(rea_cornerf(iphas).gt.0.0) then
         err_text='corner frequency'
         write(text1(42:46),'(f5.2)',err=50) rea_cornerf(iphas)
      endif
c
c   radius
c
      text1(48:48)='R'
      if(rea_radius(iphas).gt.0.0) then
         err_text='source radius'
         if(rea_radius(iphas).lt.1.0)
     *   write(text1(50:54),'(f5.3)',err=50) rea_radius(iphas)
         if(rea_radius(iphas).lt.10.0.and.rea_radius(iphas).ge.1.0)
     *   write(text1(50:54),'(f5.2)',err=50) rea_radius(iphas)
         if(rea_radius(iphas).ge.10.0)
     *   write(text1(50:54),'(f5.1)',err=50) rea_radius(iphas)
      endif

c
c   specteral slope, abs value
c
      text1(56:57)='AL'
      if(rea_slope(iphas).gt.-20.0) then
         err_text='spectral slope'
         write(text1(58:62),'(f5.2)',err=50) -rea_slope(iphas)
      endif
c
c   window length
c
      text1(64:65)='WI'
      if(rea_swin(iphas).gt.0.0) then
         err_text='spectral window'
         if(rea_swin(iphas).lt.10.0)
     *   write(text1(66:70),'(f5.2)',err=50) rea_swin(iphas)
         if(rea_swin(iphas).ge.10.0)
     *   write(text1(66:70),'(f5.1)',err=50) rea_swin(iphas)
      endif
c
c   moment magnitude
c
      text1(72:73)='MW'
      if(rea_mw(iphas).gt.-10.0) then
         err_text='moment magnitude'
         write(text1(74:78),'(f5.1)',err=50) rea_mw(iphas)
      endif
c
c-----------------------------
c   second spec line
c-----------------------------
c

c
c   start of window
c
      text2(16:16)='T'
      if(rea_hour(iphas).gt.-1.and.rea_min(iphas).
     *gt.-1.and.rea_sec(iphas).gt.-1.0) then
         err_text='window start time'
         write(text2(17:22),'(3i2)',err=51) rea_hour(iphas),
     *   rea_min(iphas),int(rea_sec(iphas)+0.5)
      endif
c
c   kappa
c
      text2(24:25)='K '
      if(rea_kappa(iphas).gt.0.0) then
         err_text='kappa'
         write(text2(26:30),'(f5.3)',err=51) rea_kappa(iphas)
      endif
c
c   geodistance
c
      text2(32:33)='GD'
      if(rea_geo_dist(iphas).gt.0.0) then
         err_text='geo distance'
         if(rea_geo_dist(iphas).lt.10.0)
     *   write(text2(34:38),'(f5.3)',err=51) rea_geo_dist(iphas)
         if(rea_geo_dist(iphas).ge.10.0.and.rea_geo_dist(iphas).
     *   lt.100.0)
     *    write(text2(34:38),'(f5.2)',err=51) rea_geo_dist(iphas)
         if(rea_geo_dist(iphas).ge.100.0)
     *   write(text2(34:38),'(f5.1)',err=51) rea_geo_dist(iphas)
      endif
c
c   velocity for spectrum
c
      text2(40:41)='V '
      text2(41:41)=rea_phase(iphas)(5:5)  ! indicate if p or s-spectrum
      x=0.0
      if(rea_phase(iphas)(5:5).eq.'S') x=rea_vs(iphas)  ! find which velocity if any
      if(rea_phase(iphas)(5:5).eq.'P') x=rea_vp(iphas)
      if(x.gt.0.0) then
         err_text='source velocity'
         write(text2(42:46),'(f5.2)',err=51) x
      endif
c
c   density
c
      text2(48:49)='DE'
      if(rea_density(iphas).gt.0.0) then
         err_text='density'
         write(text2(50:54),'(f5.2)',err=51) rea_density(iphas)
      endif
c
c    q0
c
      text2(56:57)='Q0'
      if(rea_q0(iphas).gt.0.0) then
         if(rea_q0(iphas).gt.1000.0) rea_q0(iphas)=999.0
         err_text='q0'
         write(text2(58:62),'(f5.1)',err=51) rea_q0(iphas)
      endif
c
c   q-alpha
c
      text2(64:65)='QA'
      if(rea_qalpha(iphas).ge.0.0) then
         err_text='q-alpha'
         write(text2(66:70),'(f5.2)',err=51) rea_qalpha(iphas)
      endif
c
c   s-velocity
c
      text2(72:73)='VS'
      if(rea_vs(iphas).gt.0.0) then
         err_text='S-velocty'
         write(text2(74:78),'(f5.2)',err=51) rea_vs(iphas)
      endif
c
      goto 52


 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,' writing spec line'
      write(6,'(a)') text1
      stop
 51   continue
      write(6,'(a,a,a)') 'Error with ',err_text,' writing spec line'
      write(6,'(a)') text2
      stop

 52   continue
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea_av_spec_out(text)
c
c   write average spectral info from rea common to text
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      character*80 text
      character*20 err_text   ! error text

      text=' '
c
c   fixed values
c
      text(2:13)='SPEC AVERAGE'
      text(80:80)='3'
c
c   check for overflow
c
      if(rea_av_sdrop.gt.999.0)  rea_av_sdrop=999.0
      if(rea_av_cornerf.gt.99.0) rea_av_cornerf=99.0
      if(rea_av_omega0.gt.999.0) rea_av_omega0=999.0
      if(abs(rea_av_slope).gt.999.0)   rea_av_slope=999.0
      if(rea_av_radius.gt.999.0) rea_av_radius=999.0
      if(rea_av_swin.gt.999.0) rea_av_swin=999.0

c
c   moment
c
      text(16:17)='MO'
      if(rea_av_moment.gt.0.0) then
         err_text='moment'
         write(text(18:22),'(f5.1)',err=50) rea_av_moment
      endif
c
c   stress drop
c
      text(24:25)='ST'
      if(rea_av_sdrop.gt.0.0) then
         err_text='stress drop'
         write(text(26:30),'(f5.1)',err=50) rea_av_sdrop
      endif
c
c   omega0
c
      text(32:33)='OM'
      if(rea_av_omega0.gt.0.0) then
         err_text='omega0'
         write(text(34:38),'(f5.1)',err=50) rea_av_omega0
      endif
c
c   corner frequency
c
      text(40:41)='f0'
      if(rea_av_cornerf.gt.0.0) then
         err_text='corner frequency'
         write(text(42:46),'(f5.2)',err=50) rea_av_cornerf
      endif
c
c   radius
c
      text(48:48)='R'
      if(rea_av_radius.gt.0.0) then
         err_text='source radius'
         if(rea_av_radius.lt.1.0)
     *   write(text(50:54),'(f5.3)',err=50) rea_av_radius
         if(rea_av_radius.lt.10.0.and.rea_av_radius.ge.1.0)
     *   write(text(50:54),'(f5.2)',err=50) rea_av_radius
         if(rea_av_radius.ge.10.0)
     *   write(text(50:54),'(f5.1)',err=50) rea_av_radius
      endif

c
c   specteral slope, abs value
c
      text(56:57)='AL'
      if(rea_av_slope.gt.-20.0) then
         err_text='spectral slope'
         write(text(58:62),'(f5.2)',err=50) -rea_av_slope
      endif
c
c   window length
c
      text(64:65)='WI'
      if(rea_av_swin.gt.0.0) then
         err_text='spectral window'
         if(rea_av_swin.lt.10.0)
     *   write(text(66:70),'(f5.2)',err=50) rea_av_swin
         if(rea_av_swin.ge.10.0)
     *   write(text(66:70),'(f5.1)',err=50) rea_av_swin
      endif
c
c   moment magnitude
c
      text(72:73)='MW'
      if(rea_av_mw.gt.-10.0) then
         err_text='moment magnitude'
         write(text(74:78),'(f5.1)',err=50) rea_av_mw
      endif

      goto 51

 50   continue
      write(6,'(a,a,a)')
     *'Error with ',err_text,' writing avarage spec line'
      write(6,'(a)') text
      stop

 51   continue
      return
      end

c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
      subroutine get_par(def_file)
c
c read definition file and store variables in common clock
c
      implicit none
      include 'libsei.inc' 
      include 'seidim.inc'
      include 'rea.inc'

      character*80 def_file
      integer in                    ! file unit
      logical exist                 ! true if def file exists
      integer code                  ! return code from opening def file
      integer i                     ! counter
      character*80 line             ! text line
      real var                      ! value of variables

c
c check for file, can be in local dir or DAT
c

      call sei get file( open$+ignore$,   ! Open waveform file.
     &                   in,              ! On unit.
     &                   code,            ! Returned condition.
     &                   'DAT',           ! Alternative search directory.
     &                   def_file )       ! For this filename.

      if(code.ne.e_ok$) then
        write(*,'(a,a)') ' Definition file does not exist:',
     *                    def_file
        stop   
      endif
c
c init values
c
      par_q0=0.0
      par_qalpha=0.0
      par_density=3.0
      par_kappa=0.0
      par_vp=6.0
      par_vs=3.5
c
c  p
c
      par_pick_p=0.0
      par_mb_amp=0.0
      par_mb_flow=1.0
      par_mb_flow_pole=4
      par_mb_fhigh=5.0
      par_mb_fhigh_pole=4
      par_pspec=0.0
      par_pspec_sn=4.0
      par_p_q0=0.0
      par_p_qalpha=0.0
      par_p_kappa=0.0
      par_p_flow=0.01
      par_p_flow_pole=8
      par_p_fhigh=50.0
      par_p_fhigh_pole=8

c
c  s
c
      par_pick_s=0.0
      par_ml_amp=0.0
      par_ml_flow=1.0
      par_ml_flow_pole=4
      par_ml_fhigh=5.0
      par_ml_fhigh_pole=4
      par_sspec=0.0
      par_sspec_sn=4.0
      par_s_q0=0.0
      par_s_qalpha=0.0
      par_s_kappa=0.0
      par_s_flow=0.01
      par_s_flow_pole=8
      par_s_fhigh=50.0
      par_s_fhigh_pole=8


      i=0

 1000 continue

c
c read text line from file, check for code and set variables
c
      read(in,'(a)',end=4000,err=3000) line
c
c  q0 and qalpha
c
      if (line(1:11).eq.'SPECTRAL Q0') then 
         read(line(41:50),'(f10.1)',err=3000) par_q0
      elseif(line(1:15).eq.'SPECTRAL QALPHA') then
         read(line(41:50),'(f10.1)',err=3000) par_qalpha
      elseif (line(1:16).eq.'SPECTRAL DENSITY') then
        read(line(41:50),'(f10.1)',err=3000) par_density
      elseif (line(1:14).eq.'SPECTRAL KAPPA') then
        read(line(41:50),'(f10.1)',err=3000) par_kappa
      elseif (line(1:19).eq.'SPECTRAL P-VELOCITY') then
        read(line(41:50),'(f10.1)',err=3000) par_vp
      elseif (line(1:19).eq.'SPECTRAL S-VELOCITY') then
        read(line(41:50),'(f10.1)',err=3000) par_vs
c
c   special P
c
      elseif (line(1:12).eq.'PICK P-PHASE') then
        read(line(41:50),'(f10.1)',err=3000) par_pick_p
      elseif (line(1:11).eq.'PICK AMP-MB') then
        read(line(41:50),'(f10.1)',err=3000) par_mb_amp
      elseif (line(1:20).eq.'MB LOW CUT AND POLES') then
        read(line(41:50),'(2f10.1)',err=3000) par_mb_flow,
     *  par_mb_flow_pole
      elseif (line(1:21).eq.'MB HIGH CUT AND POLES') then
        read(line(41:50),'(2f10.1)',err=3000) par_mb_fhigh,
     *  par_mb_fhigh_pole
      elseif (line(1:10).eq.'MB MIN S/N') then
        read(line(41:50),'(f10.1)',err=3000) par_mb_amp_sn
      elseif (line(1:10).eq.'P-SPECTRUM') then
        read(line(41:50),'(f10.1)',err=3000) par_pspec
      elseif (line(1:18).eq.'P-SPECTRUM MIN S/N') then
        read(line(41:50),'(f10.1)',err=3000) par_pspec_sn
      elseif (line(1:13).eq.'P-SPECTRUM Q0') then
        read(line(41:50),'(f10.1)',err=3000) par_p_q0
      elseif (line(1:17).eq.'P-SPECTRUM QALPHA') then
        read(line(41:50),'(f10.1)',err=3000) par_p_qalpha
      elseif (line(1:16).eq.'P_SPECTURM KAPPA') then
        read(line(41:50),'(f10.1)',err=3000) par_p_kappa
      elseif (line(1:28).eq.'P-SPECTRUM LOW CUT AND POLES') then
        read(line(41:50),'(2f10.1)',err=3000) par_p_flow,
     *  par_p_flow_pole
      elseif (line(1:29).eq.'P-SPECTRUM HIGH CUT AND POLES') then
        read(line(41:50),'(2f10.1)',err=3000) par_p_fhigh,
     *par_p_fhigh_pole
c
c  S
c

      elseif (line(1:12).eq.'PICK S-PHASE') then
        read(line(41:50),'(f10.1)',err=3000) par_pick_s
      elseif (line(1:11).eq.'PICK AMP-ML') then
        read(line(41:50),'(f10.1)',err=3000) par_ml_amp
      elseif (line(1:20).eq.'ML LOW CUT AND POLES') then
        read(line(41:50),'(2f10.1)',err=3000) par_ml_flow,
     *  par_ml_flow_pole
      elseif (line(1:21).eq.'ML HIGH CUT AND POLES') then
        read(line(41:50),'(2f10.1)',err=3000) par_ml_fhigh,
     *  par_ml_fhigh_pole
      elseif (line(1:10).eq.'ML MIN S/N') then
        read(line(41:50),'(f10.1)',err=3000) par_ml_amp_sn
      elseif (line(1:10).eq.'S-SPECTRUM') then
        read(line(41:50),'(f10.1)',err=3000) par_sspec
      elseif (line(1:18).eq.'S-SPECTRUM MIN S/N') then
        read(line(41:50),'(f10.1)',err=3000) par_sspec_sn
      elseif (line(1:13).eq.'S-SPECTRUM Q0') then
        read(line(41:50),'(f10.1)',err=3000) par_s_q0
      elseif (line(1:17).eq.'S-SPECTRUM QALPHA') then
        read(line(41:50),'(f10.1)',err=3000) par_s_qalpha
      elseif (line(1:16).eq.'S_SPECTURM KAPPA') then
        read(line(41:50),'(f10.1)',err=3000) par_s_kappa
      elseif (line(1:28).eq.'S-SPECTRUM LOW CUT AND POLES') then
        read(line(41:50),'(2f10.1)',err=3000) par_s_flow,
     *  par_s_flow_pole
      elseif (line(1:29).eq.'S-SPECTRUM HIGH CUT AND POLES') then
        read(line(41:50),'(2f10.1)',err=3000) par_s_fhigh,
     *par_s_fhigh_pole

c
c   stations to use
c
      elseif (line(1:7).eq.'STATION') then
         i=i+1
cTATION MOL   S  Z    3.0   20.0   50.0   10.0    5.0  5.0 10.0

         par_stat(i)=' '
         par_comp(i)=' '
         par_sta(i)=0.0
         par_lta(i)=0.0
         par_ratio(i)=0.0
         par_mincoda(i)=0.0
         par_dtrle(i)=0.0
         par_fill(i)=0.0
         par_filh(i)=0.0

         read(line(9:63),'(a5,1x,a4,5(1x,f6.1),2(1x,f4.1))',
     &   err=3000)
     &   par_stat(i),par_comp(i),par_sta(i),par_lta(i),
     &   par_ratio(i),par_mincoda(i),par_dtrle(i),par_fill(i),
     &   par_filh(i)

      endif
     
      goto 1000

 3000 continue
      write(6,'(a,a)') ' Error in def file: ',def_file
      write(6,'(a)') line
      stop

4000  continue

      par_nstat=i

      write(6,'(a,a,2x,i3)') ' Number of stations in ',
     *def_file(1:20),par_nstat
      call sei close( close$, in, code )

      return
      end 

      subroutine add_phase_to_sfile(text,data,nhead,nrecord,overwrite)
c
c  and one new line to phase list in s-file, line added. If the same
c  component and phase exists and associated info (column 2:18),
c  overwrite if flag set
c
c  jh march 2001
c

c      input:   text: new line
c               data: data
c               nhead: number of headers
c               nrecord: number of records
c               overwrite: if true, overwrite
c
c      output:  data,nrecord updated

      implicit none
      character*80 data(*),text
      integer nhead,nrecord
      logical overwrite
      integer i
c
c   overwrite
c
      if(overwrite) then
         do i=nhead+1,nrecord
             if(text(2:18).eq.data(i)(2:18)) then
                data(i)=text
                goto 999
             endif
         enddo
      endif
c
c  add at end since no overwrite or overwrite flag not set
c
      if(data(nrecord).eq.' ') then   ! last line blank as it should be
         data(nrecord)=text
         data(nrecord+1)=' '
         nrecord=nrecord+1
      else
         data(nrecord+1)=text
         data(nrecord+2)=' '          ! last lin enot blank, add one extra line
         nrecord=nrecord+2
      endif
c
 999  continue
      return
      end

      subroutine add_spec_to_sfile
     *(text1,text2,data,nhead,nrecord,overwrite)
c
c  add 2 new lines to spec list in s-file. If the same
c  component and phase exists and spectrum is the same type (P or S)
c  overwrite if flag owerwrite set
c
c  jh march 2001
c

c      input:   text1,tet2: new 2 lines
c               data: data
c               nhead: number of headers
c               nrecord: number of records
c               overwrite: if true, overwrite
c
c      output:  data,nrecord,nhead updated

      implicit none
      character*80 data(*),text1,text2
      integer nhead,nrecord
      logical overwrite
      integer i,n,k
c
c   overwrite
c
      if(overwrite) then
         n=0             ! count number of spec lines replaced
         do i=2,nhead
             if(data(i)(2:5).eq.'SPEC'.and.data(i)(80:80).eq.'3'.and.
     *          text1(2:17).eq.data(i)(2:17)) then  ! check if first type
                n=n+1
                data(i)=text1
             endif
             if(data(i)(2:5).eq.'SPEC'.and.data(i)(80:80).eq.'3'.and.
     *          text2(2:16).eq.data(i)(2:16).and.text2(41:41).eq.   ! S or P
     *          data(i)(41:41)) then
                data(i)=text2
                n=n+1
             endif
         enddo
c
c   check for a missing line
c
         if(n.eq.1) then
            write(6,*)' One spectral line missing in input file'
            write(6,'(a)') text1(1:79)
            stop
         endif
c
c   check for too many lines
c
          if(n.gt.2) then
             write(6,*)' Too many spectral line in input file',
     *       ' for same channel'
             write(6,'(a)') text1(1:79)
             stop
          endif
c
c   if both replaced, return
c   
          if(n.eq.2) goto 999
      endif
c
c  add since no overwrite or overwrite flag not set
c
c
c   find where to add
c
      n=0
      k=0
      do i=2,nhead
         if(data(i)(2:5).eq.'SPEC') n=i   ! find last previous spectral line
         if(data(i)(80:80).eq.'E') k=i    ! find error line
      enddo
      n=n+1
      k=k+1
c
c   if no spectal lines, add after error line if there, if no error line
c   start in number 2
c
      if(n.eq.1) n=k
      if(k.eq.1) n=2
c
c    shift down data to make room for spectral parameters
c
      do i=nrecord,n,-1
         data(i+2)=data(i)
      enddo
c
c   put in new spectral data
c
      data(n)=text1
      data(n+1)=text2
      nhead=nhead+2
      nrecord=nrecord+2
c
 999  continue
      return
      end







      subroutine moment_f0(nfreq,amp,freq,level,f0,rms)
c
c  fit the brune model to an observed spectrum by grid search
c
c  jh feb 2000
c
c      input:  nfreq:          number of spectral values
c              amp             amplitudes (linear)
c              freq:           corresponding frequencies, start with
c                              lowest frequencies
c              f0:             starting corner frequency
c
c      output: level           omega zero level or flat level
c              f0:             corner frequency
c              rms:            rms error
c

      
      implicit none
      integer nfreq
      real amp(*),freq(*),level,f0,rms
      real ftest(54)         ! test f0
      integer ntest          ! number of test frequencies
      integer fmin           ! frequecy number for best fit
      real rmsmin            ! minimum rms
      real x,y               ! help variable
      real brune             ! brune function
      integer i,j,k,l        ! counters

      data  ftest/0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09,
     *             0.1,  0.2,  0.3,  0.4,  0.5,  0.6,  0.7,  0.8,  0.9,
     *        1.0, 1.1,  1.2,  1.3,  1.4,  1.5,  1.6,  1.7,  1.8,  1.9,
     *                   2.0,  3.0,  4.0,  5.0,  6.0,  7.0,  8.0,  9.0,
     *        10.0,     12.0,       14.0,       16.0,      18.0,
     *        20.0,     22.0,       24.0,       26.0,      28.0,
     *        30.0,                       35.0,
     *        40.0,                       45.0,
     *        50.0,                             60.0, 70.0, 80.0/ 


      ntest=54
c
c-------------------------------------------------------------------
c   run routine a few times in a loop to improve esitimate
c
       do l=1,3
c
c
c   first get a first estimate of level using data up to f0/2
c
       i=1
       level=0.0
       do while(freq(i).lt.f0/2.0.and.i.lt.nfreq-1)
          level=level+amp(i)
          i=i+1
       enddo
       i=i-1
       if(i.gt.0) then
          level=level/i
       else
           level=1.0                                  
       endif
       level=level/0.92       ! correct for level going down  a bit , fix !!

c
c  enter loop of trials
c
       rmsmin=10.0e30
       do k=1,100
       y=level*(50.0+k)/100.0
       do i=1,ntest      ! loop over trial f0
          rms=0.0
          do j=1,nfreq   ! loop over observations
c             x=alog10(amp(j))-alog10(y*brune(freq(j),ftest(i)))
             x=(amp(j))-y*brune(freq(j),ftest(i))
             rms=rms+x*x
          enddo
          if(rms.lt.rmsmin) then
             rmsmin=rms
             fmin=i
          endif
c          write(1,*) ftest(i),rms
       enddo
       enddo
       rms=rmsmin/nfreq
       f0=ftest(fmin)
c
c  end of iterative loop
c
       enddo         ! enddo of l

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c
c  recalculate level using all frequencies
c
       x=0.0
       do i=1,nfreq
           x=x+amp(i)/brune(freq(i),f0)
       enddo
       x=x/nfreq
       level=x

       return
       end

      real function brune(f,f0)
c
c   calculates the level of the Brune displacement spectrum at 
c   at frequency f . The spectrum has corner frequency f0. Spectral
c   level is 1.0

      implicit none
      real f,f0

      brune=1.0/(1.0+(f/f0)**2)
      return
      end
