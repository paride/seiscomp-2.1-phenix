
c
c changes:
c
c sep 26 00 lo : use channel definition file to set components
c oct 23 00 lo : bug fix in write gse out
c feb 24 03 lo : add interactive flag to header_gse_to_seisan
c


      subroutine write_seisan_to_gse(write1,k,wav_out,
     *     format_out)

c
c Input:
c
c   write1    :   output unit
c   k         :   channel index
c   wav_out   :   true if wav_out* variables set
c   format_out:   gse output format
c
c
c write out gse data from Seisan wave block
c
c Lars Ottemoeller, 12 May 2000
c
c
c changes
c

      implicit none
      include 'codeco_common.f'
      include 'seidim.inc'
      include 'waveform.inc'
      integer seiclen

      integer write1           ! write unit
      integer k                ! channel index
      logical wav_out          ! true if wav_out
      integer l                ! counter
      integer year,month,day,hour,min
      real sec
      integer nsamp
      real rate
      character*5 stat
      character*4 comp
      character*80 format_out
c
c following variables for GSE output
c
      character*1 cbuf(c_bufsize)        ! gse ascii data
      integer*4 iy(c_sigsize)            ! gse integer data
      integer*4 ichecksum                ! gse checksum
      character*29 mainhead_text
      character*80 text
      character*5 net_code


c
c   get def file for station codes, give file name
c
      text='gsesei.def'
      net_code=' '
      hdr_coosys='            '

      call read_def_chan(text,mainhead_text,net_code)

      if (wav_out) then
         year=wav_out_year(k)
         month=wav_out_month(k)
         day=wav_out_day(k)
         hour=wav_out_hour(k)
         min=wav_out_min(k)
         sec=wav_out_sec(k)
         nsamp=wav_out_nsamp(k)
         rate=wav_out_rate(k)
         stat=wav_stat(k)
         comp=wav_comp(k)
      else
         year=wav_year(k)
         month=wav_month(k)
         day=wav_day(k)
         hour=wav_hour(k)
         min=wav_min(k)
         sec=wav_sec(k)
         nsamp=wav_nsamp(k)
         rate=wav_rate(k)
         stat=wav_stat(k)
         comp=wav_comp(k)
      endif
      do l=1,nsamp
         iy(l)=int(signal1(l))
      enddo
c
c write to GSE header
c

      hdr_year=year
      hdr_month=month
      hdr_day=day
      hdr_hour=hour
      hdr_min=min
      hdr_sec=int(sec)
      hdr_msec=int((sec-int(sec))*1000)
c
c station location
c
      hdr_station = ' '
      hdr_station = stat(1:5)
      call stat_loc(stat,' ',
     *    hdr_stalat,hdr_stalon,hdr_staelev)
      hdr_chan = ' '

      call set_def_chan(1,stat(1:5),comp)
      hdr_chan(1:3)=comp(1:3)
      
      hdr_stadescr= ' '
      hdr_nsamp=nsamp
      hdr_smprate=rate
      hdr_ifmtnum=0
      hdr_calunit=0
      hdr_calfac=1.
      hdr_calper=1.
      hdr_instr=' '
      hdr_network='         '
      hdr_stadepth=0.
      if (hdr_chan(3:3).eq.'Z'.or.hdr_chan(3:3).eq.'z') then
        hdr_hang=-1.
        hdr_vang=0.
      elseif (hdr_chan(3:3).eq.'N'.or.hdr_chan(3:3).eq.'n') then
        hdr_hang=0.
        hdr_vang=90.
      elseif (hdr_chan(3:3).eq.'E'.or.hdr_chan(3:3).eq.'e') then
        hdr_hang=90.
        hdr_vang=90.
      endif
c
c select GSE format, default is CM6
c
      if (format_out(4:4).eq.' '.or.
     * format_out(4:6).eq.'CM6') then
        hdr_ofmtname='CM6'
        hdr_ofmtnum=2
        hdr_odiff=2
      elseif (format_out(4:6).eq.'INT') then
        hdr_ofmtname='INT'
        hdr_ofmtnum=2
        hdr_odiff=0
      endif
      call gseout(write1,cbuf,iy,ichecksum)

      return
      end



      subroutine header_gse_to_seisan(ifile,ierr)
c
c read header from gse file and write to wav structure
c
c Lars Ottemoeller, 12 May 2000
c
      implicit none
      include 'codeco_common.f'
      include 'seidim.inc'
      include 'waveform.inc'
      integer k,i              ! channel id total and file, 
      integer ifile            ! wave file number
      character*1 cbuf(c_bufsize)        ! gse ascii data
      integer*4 iy(c_sigsize)            ! gse integer data
      integer*4 ichecksum                ! gse checksum
      integer*4 ierr                     ! gse error
      character*1 ch
      integer seiclen

      i=0
      k=wav_nchan     ! set total channel counter
      open(95,file=wav_filename(ifile),status='unknown')
      open(94,file='gsetemp.out',status='unknown')
 200  continue    ! read until end of file

c
c try to read first channel
c
      call gsein( 95, 94, cbuf, iy, ichecksum, ierr )
      if (ierr.ne.0) goto 250

      k = k + 1
      i = i + 1

      wav_year(k)=hdr_year
c use aux code as option, lo Nov 2001
      if (hdr_stadescr.ne.' '.and.seiclen(ch).lt.1) then
        if (wav_interactive) then
      write(*,*) ' aux station code not empty, use as station code? '
          read(5,'(a1)') ch
          call casefold(ch)
        else 
          ch='N'
        endif
      endif
      if (ch.eq.'Y') then
        wav_stat(k)=hdr_stadescr(1:4)
      else
        wav_stat(k)=hdr_station(1:5)
      endif
c      wav_stat(k)=hdr_station(1:5)
      wav_comp(k)(1:2)=hdr_chan(1:2)
      wav_comp(k)(3:3)=' '
      wav_comp(k)(4:4)=hdr_chan(3:3)
      wav_time_error(k)=' '
      wav_month(k)=hdr_month
      wav_day(k)=hdr_day
      wav_hour(k)=hdr_hour
      wav_min(k)=hdr_min
      wav_sec(k)=float(hdr_sec)+float(hdr_msec)/1000.
      wav_file_nr_chan(k)=ifile
      wav_chan_nr_file(k)=i
      call timsec(wav_year(k),wav_month(k),wav_day(k),
     *       wav_hour(k),wav_min(k),wav_sec(k),wav_abs_time(k))
      wav_nsamp(k)=hdr_nsamp
      wav_rate(k)=hdr_smprate
      wav_cbyte(k)='4'
      wav_duration(k)=(wav_nsamp(k)-1)/wav_rate(k)
      goto 200
 250  continue
      wav_nchan=k    ! save number of channels

      close(95)
      close(94)

      return
      end


