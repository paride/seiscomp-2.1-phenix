

      subroutine write_seisan_to_sacbin(ichan,keep_path,
     *          wav_out,eqla,eqlo,eqel,nerr)
c 
c routine to write sac file for trace of data read by  wav_read_channel
c data and header are known from the wav common block, the SAC libraries
c are required
c
c input:  ichan - channel id from Seisan WAV structure
c         keep_path - true if path remains in output file, otherwise false
c         wav_out - true if data in wav_out structure
c         eqla,eqlo,eqel - event lat, lon and elevation
c         nerr - error code given by SAC, 0 if ok
c
c Lars Ottemoeller, 11 May 2000
c
c changes:
c
c  nov 13, 2001 jh : wrong argument to set_def_chan, check for null chars 
cv                   in stat code
c  dec 18  2002 jh : station and component check not implemented in 
c                    read_sacasc_to_seisan
c  may 8 2003 lo   : check for sacaux in reading routine
c

      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      integer seiclen         ! function to get length of string
      integer jday            ! julian day
      integer ichan           ! trace index
      logical keep_path       ! true if complete path in output file
      logical wav_out         ! true if wav_out
      integer i,j             ! counters
      real stla,stlo,stel     ! station location
      character*80 outfile    ! output file name
      character*80 infile     ! input file name
      real cmpaz,cmpinc       ! channel orientation
      real eqla,eqlo,eqel     ! event location
      integer nerr            ! sac header error code
      real data_x(1)          ! x axis data, not used if data evenly spaced
      logical set_comp        ! true if cmpaz and cmpinc set
      integer ind             ! index
      character*1 dchar       ! directory delim.
      real beg                ! begin time
      parameter (beg=0.)
      integer year,month,day,hour,min
      real sec
      character*5 stat
      character*4 comp
      real rate
      integer nsamp
      real duration
      character*80 deffile
      character*5 net_code              ! network code
      character*29 mainhead_text

      data_x(1) = 1

      call dir_char(dchar)
c
c   get def file for station codes, give file name
c
      deffile='sacsei.def'
c      no_net = .FALSE.
      net_code=' '

      call read_def_chan(deffile,mainhead_text,net_code)

c
c read variables
c
      if (wav_out) then
        year=wav_out_year(ichan)
        month=wav_out_month(ichan)
        day=wav_out_day(ichan)
        hour=wav_out_hour(ichan)
        min=wav_out_min(ichan)
        sec=wav_out_sec(ichan)
        stat=wav_stat(wav_out_chan(ichan))
        comp=wav_comp(wav_out_chan(ichan))
        infile = wav_filename(wav_file_nr_chan
     *                       (wav_out_chan(ichan)))
        nsamp=wav_out_nsamp(ichan)
        rate=wav_out_rate(ichan)
        duration=wav_out_duration(ichan)
      else
        year=wav_year(ichan)
        month=wav_month(ichan)
        day=wav_day(ichan)
        hour=wav_hour(ichan)
        min=wav_min(ichan)
        sec=wav_sec(ichan)
        stat=wav_stat(ichan)
        comp=wav_comp(ichan)
        infile = wav_filename(wav_file_nr_chan(ichan))
        nsamp=wav_nsamp(ichan)
        rate=wav_rate(ichan)
        duration=wav_duration(ichan)
      endif
    
c
c get julian day
c
      call date_doy(jday,day,month,year)

c
c convert comp and station name
c
      call set_def_chan(1,stat,comp) 

c
c get station location
c
      call stat_loc(stat,'0',stla,stlo,stel)

c
c make sac file name
c
         outfile = ' '

         outfile= infile(1:seiclen(infile)) 
         if (.not.keep_path) then
           ind=-1
           do i=seiclen(outfile),1,-1
             if (outfile(i:i).eq.dchar.and.ind.eq.-1) then
               ind=i+1
             endif
           enddo
           if (ind.ne.-1) then
             outfile = outfile(ind:seiclen(outfile))
           endif
         endif
         outfile = outfile(1:seiclen(outfile)) // '_'
     &         // stat(1:5) // '_' //
     &           comp(1:4)  // '_SAC'

         j= seiclen(outfile)
         do i=1,j
           if (outfile(i:i).eq.' ') outfile(i:i)='_'
         enddo
         write(6,*) ' SAC output filename: ',
     *       outfile(1:seiclen(outfile))

c
c write sac header
c
        call newhdr
c number of points
        call setnhv('npts',nsamp,nerr)
c beginning and ending value of data
        call setfhv('b',beg,nerr)
        call setfhv('e',duration,nerr)
c type of file
        call setihv('iftype','itime',nerr)
c evenly spaced
        call setlhv('leven',.true.,nerr)
c sample rate
        call setfhv('delta',1/rate,nerr)
c date and time
        call setnhv('nzyear',year,nerr)
        call setnhv('nzjday',jday,nerr)
        call setnhv('nzhour',hour,nerr)
        call setnhv('nzmin',min,nerr)
        call setnhv('nzsec',int(sec),nerr)
        call setnhv('nzmsec',int(1000.*(sec-
     *        int(sec))),nerr)
c station coord
        call setfhv('stla',stla,nerr)
        call setfhv('stlo',stlo,nerr)
        call setfhv('stel',stel,nerr)
c station and comp name
        call setkhv('kstnm',stat,nerr)
        call setkhv('kcmpnm',comp,nerr)
c hyp
        if (eqla.ne.0.or.eqlo.ne.0) then
          call setfhv('evla',eqla,nerr)
          call setfhv('evlo',eqlo,nerr)
          call setfhv('evel',eqel,nerr)
        endif
 
        set_comp = .false.

c az and ic of components
        if (comp(4:4).eq.'Z'.or.comp(3:3).
     *            eq.'Z') then
          cmpaz=0.
          cmpinc=0. 
          set_comp=.true.
        elseif (comp(4:4).eq.'N'.or.comp(3:3).
     *            eq.'N') then
          cmpaz=0.
          cmpinc=90.
          set_comp=.true.
        elseif (comp(4:4).eq.'E'.or.comp(3:3).
     *            eq.'E') then
          cmpaz=90.
          cmpinc=90.
          set_comp=.true.
        endif
        if (set_comp) then
          call setfhv('cmpaz',cmpaz,nerr)
          call setfhv('cmpinc',cmpinc,nerr)
        endif

c
c write sac data
c
        call wsac0(outfile,data_x,signal1,nerr)
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine read_sacbin_to_seisan(infile,ichan,nerr)
c
c   read binary sac
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      integer seiclen
      integer ichan               ! channel id
      character*80 infile         ! input file name
      integer nerr                ! sac error code
      real del                    ! delta 
      real max                    ! max number of samples from rsac
      real beg,en                 ! beginning and end of trace
      integer jday                ! julian day
      integer isec,msec           ! sec and millisec
      double precision dpsec      ! absolute time
      integer i                   ! counter
      integer nlen
      character*80 deffile
      character*29 mainhead_text
      character*5 net_code      
      character*80 sacaux

c
c check if SACAUX defined
c
      call get_env_sacaux(sacaux)
      if (seiclen(sacaux).le.0) then
        return
      endif

c
c   get def file for station codes, give file name
c
      deffile='sacsei.def'
      net_code=' '

      call read_def_chan(deffile,mainhead_text,net_code)

c
c set defaults      
c
      wav_cbyte(ichan)='4'

      beg=0.
      max=max_sample
      call rsac1(infile,signal1,nlen,beg,del,max,nerr)
      if(nerr.ne.0) then
        nerr=-1
        return
      endif
 
c
c get header values
c

c number of points
        call getnhv('npts',wav_nsamp(ichan),nerr)
c begining and ending value of data
        call getfhv('b',beg,nerr)
          if (beg.lt.0.0001) beg = 0.
        call getfhv('e',en,nerr)
c sample rate
        call getfhv('delta',del,nerr)
        wav_rate(ichan) = 1/del
c station and comp name
        call getkhv('kstnm',wav_stat(ichan),nerr)
c
c check for null
c
        do i=1,5
           if(ichar(wav_stat(ichan)(i:i)).eq.0) wav_stat(ichan)(i:i)=' '
        enddo
c
        call getkhv('kcmpnm',wav_comp(ichan),nerr)
        do i=seiclen(wav_comp(ichan))+1,4
           wav_comp(ichan)(i:i) = ' '
        enddo
        call set_def_chan(ichan,wav_stat(ichan),wav_comp(ichan))
c date and time
        call getnhv('nzyear',wav_year(ichan),nerr)
          if (wav_year(ichan).lt.100) wav_year(ichan)=
     *        wav_year(ichan)+1900
        call getnhv('nzjday',jday,nerr)
        call dte(jday,wav_day(ichan),wav_month(ichan),wav_year(ichan))
        call getnhv('nzhour',wav_hour(ichan),nerr)
        call getnhv('nzmin',wav_min(ichan),nerr)
        call getnhv('nzsec',isec,nerr)
        call getnhv('nzmsec',msec,nerr)
        wav_sec(ichan)=isec+msec/1000.

c
c add beg to absolute time
c
        call timsec(wav_year(ichan),wav_month(ichan),wav_day(ichan),
     *      wav_hour(ichan),wav_min(ichan),wav_sec(ichan),dpsec)
        dpsec=dpsec+beg
        call sectim(dpsec,wav_year(ichan),jday,wav_month(ichan),
     *    wav_day(ichan),wav_hour(ichan),wav_min(ichan),wav_sec(ichan))

        wav_abs_time(ichan)=dpsec

c
c duration
c
        wav_duration(ichan) = wav_nsamp(ichan) / wav_rate(ichan)

      return
      end




      subroutine read_sacasc_to_seisan(infile,ichan,nerr)

      implicit none
      include 'libsei.inc' 
      external sei open,                  ! Open file routine.
     &         sei close                  ! Close file routine
      integer  read1,                     ! Input unit 1
     &         code                       ! Local condition
      logical  bflag                      ! Flag end of file
      include 'seidim.inc'
      include 'waveform.inc'
      integer seiclen
      integer ichan               ! channel id
      character*80 infile         ! input file name
      character*80 text           ! input text line
      integer nerr                ! sac error code
      real del                    ! delta
      real max                    ! max number of samples from rsac
      real beg,en                 ! beginning and end of trace
      integer jday                ! julian day
      integer isec,msec           ! sec and millisec
      double precision dpsec      ! absolute time
      integer i,n,tt,stoploop     ! counter
      real rate                   ! sample rate
      character*80 deffile
      character*29 mainhead_text
      character*5 net_code


      n=0
      nerr=0
c
c   get def file for station codes, give file name
c
      deffile='sacsei.def'
      net_code=' '



c
c  open ascii file and read variables
c
        call sei open( old$+warn$,             ! Open file (stop on error).
     &                     ' ',                ! No prompt.
     &                     infile,             ! This filename.
     &                     read1,              ! On unit.
     &                     bflag,              ! Exists!!
     &                     code )     
   
        if (.not.bflag) then
          nerr=-1 
          return
        endif

c
c read from asc header
c
c
c line : 1
       read(read1,'(a)',err=998) text

       call SEI GET VALUES( 5, text, CODE )
       if (code.ne.0) goto 998

       rate=ARRAY$(1)
       wav_rate(ichan)=1/rate

c-- read lines
c skip until line 14
       do i=1,13
          read(read1,*,err=998)
       enddo

c-- read start time
c line 15
       read(read1,'(a)',err=998) text

       call SEI GET VALUES( 5, text, CODE )
       if (code.ne.0) goto 998

        wav_year(ichan)=ARRAY$(1)
        jday=ARRAY$(2)
        call dte(jday,wav_day(ichan),wav_month(ichan),wav_year(ichan))
        wav_hour(ichan)=ARRAY$(3)
        wav_min(ichan)=ARRAY$(4)
        isec=ARRAY$(5)

c line 16
        read(read1,'(a)') text
        call SEI GET VALUES( 5, text, CODE )
        msec=ARRAY$(1)
        wav_sec(ichan)=isec+msec/1000.

c-- number of samples
        wav_nsamp(ichan)=ARRAY$(5)

c-- skip until 22
        do i=1,6
          read(read1,*)
        enddo

c-- read station
       read(read1,'(a)') text
       read(text(1:5),'(a5)') wav_stat(ichan)
c
c check for null
c
        do i=1,5
           if(ichar(wav_stat(ichan)(i:i)).eq.0) wav_stat(ichan)(i:i)=' '
        enddo
   

c-- skip lines until 28
       do i=1,5
          read(read1,*)
       enddo

c-- read channel
c line 29
       read(read1,'(a)') text
       read(text(17:20),'(a4)') wav_comp(ichan)
c
c   fix station and component names
c
        call set_def_chan(ichan,wav_stat(ichan),wav_comp(ichan))

       read(read1,'(a)') text
c
c read waveform data
c
       stoploop=int(wav_nsamp(ichan)/5)+1
       do i=1,stoploop
          read(read1,'(a)',end=999) text

          do tt=1,80

            if (text(tt:tt).eq.'.') then
              if (text(tt+1:tt+7).eq.'0000000') then
                text(tt+1:tt+7)='       '
              elseif (text(tt+7:tt+7).eq.'0') then
                text(tt+7:tt+7)=' '
              endif
             endif
          enddo

c227545.0000000-381483.0000000-492502.0000000-646966.0000000-711058.0000000
c827626.0000000-1120953.0000000-1584441.0000000-1508225.0000000-880364.0000000
          if (n.LE.wav_nsamp(ichan)) THEN
             call SEI GET VALUES( 5, text, CODE )
             do tt=1,5
               n=n+1
               if (n.LE.wav_nsamp(ichan)) THEN
                 signal1(n)=ARRAY$(tt)
               endif
             enddo
          ELSE
            GOTO 999
          ENDIF

       enddo

      goto 999

  998 continue
      nerr=-1

  999 CONTINUE
      call sei close( close$, read1, code )
      if (nerr.ne.-1) then
c
c duration
c
        wav_duration(ichan) = wav_nsamp(ichan) / wav_rate(ichan)
        wav_chan_nr_file(ichan)=1
        wav_cbyte(ichan)='4'
      endif

      return
      end



