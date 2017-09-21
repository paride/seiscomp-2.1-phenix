C   computer dependent subroutines including dummy routines to be
c  able to use same programs on linux, pc and sun
c
c    ***** this file is for sun and linux******
c
c
c  Updates:
c
c  jul 20 94 by jh    : add get_display
c  nov 24       jh    : add clear_to_text
cJAB(BGS)Jan95        : add get_arch to find SEISAN operating system environment
cJAB(BGS)Jul95        : add dummy PC_TO_TEXT which on PC clears to alpha.
cJAB(BGS)Jul95        : add random number generator 
c  jun 96       jh    : add editor name
c  feb 97       lo    : add cputimer
c  feb 24 97    jh    : add tektronics dummy routines
c  oct 6              : add put_env_string and get_env_string
c  sep 98       jh    : -----------   version 7.0 check -----------------
c                       data base to 5 chars
c  sep 98       linux :  removed ieee_flag, drand  
c  nov 4 98 by jh     : linux logic removed from this file whcih was
c                       renamed comp_unix
c  mar 23, 99  lo     : in get_env_base put _ if blank
c  mar 30 99 bmt      : remove pc_to_text
c  sep 16, 99  lo     : send_plot, use sleep
c  jan 18 2000 bmt    : add put_env_seistop     
c  dec 14 2000 lo     : change in uncompress routine
c  feb 21 2001 lo     : changed arguments to subroutines to variable length
c
      subroutine topdir(top_directory)
c
c  path-name for main directory of seisan seismic analysis system
      character*(*)      top_directory
c   change following line if needed, now only on vax, sometimes on pc:
c
c   get top dir from enviromental variable, usually defined in ../COM/.SEISAN
c
      call getenv('SEISAN_TOP',top_directory)
      return
      end
c
      subroutine get_editor(eev_edit)
c
c   get editor name for eev
c
      character*(*) eev_edit
      integer sei clen
c
c  get env variable
c
      call getenv('SEISAN_EDITOR',eev_edit)
c
c   set to default if not set
c
      if(sei clen(eev_edit).eq.0) eev_edit='vi'
      return
      end

c
c get env variable SEISAN_EXTENSION
c
      subroutine get_env_seisan_extension(text)
      implicit none
      character text*(*)
      call getenv('SEISAN_EXTENSION',text)
      return
      end

      subroutine get_env_sacaux(text)
      implicit none
      character text*(*)
      call getenv('SACAUX',text)
      return
      end
c
c get operator id
c
      subroutine get_env_op(text)
      implicit none
      character*(*) text
      call getenv('SEISAN_OPERATOR',text)
      return
      end

c
c    get event name if any
c
      subroutine get_env_event(event)
      implicit none
      character*(*) event
      call getenv('TRANSFER_EVENT',event)
      return
      end
c
c   put event in memory
c
      subroutine put_env_event(event)
      implicit none
      character*(*) event
      character*100 text
      write(text,'(a,a)')'TRANSFER_EVENT=',event
      call putenvsun(text)   !c call
      return
      end
c
c   put base name in memory
c
      subroutine put_env_base(base)
      implicit none
      character*(*) base
      character*100 text
      write(text,'(a,a)')'TRANSFER_BASE=',base
      call putenvsun(text)   !c call
      return
      end

      subroutine put_env_seistop(path)
      implicit none
      character*(*) path
      character*100 text
      write(text,'(a,a)')'SEISAN_TOP=',path
      call putenvsun(text)   !c call
      return
      end

      subroutine put_env_op(op)
      implicit none
      character*(*) op
      character*100 text
      write(text,'(a,a)')'SEISAN_OPERATOR=',op 
      call putenvsun(text)   !c call
      return
      end


c
c    get base  name if any
c
      subroutine get_env_base(base)
      implicit none
      character*(*) base 
      call getenv('TRANSFER_BASE',base)
      return
      end
c
c    get alternative cal directory if there 
c
      subroutine get_env_cal(local_cal)
      implicit none
      character*(*) local_cal
      local_cal=' '
      call getenv('LOCAL_CAL',local_cal)
      return
      end


c
c
      subroutine get_def_base(def_base)
c
c   get enviromental variable def_base 
c
      character*(*) def_base 
      integer i
      call getenv('DEF_BASE',def_base)
      if (def_base(1:1).ne.' ') then
        do i=2,5
          if (def_base(i:i).eq.' ') def_base(i:i) = '_'
        enddo
      endif
      if(def_base.eq.'   '.or.ichar(def_base).eq.0) def_base='AGA__'
      return
      end
c
c
c
c    get a general string from any program
c
      subroutine get_env_string(text)
      implicit none
      character*(*) text
      text=' '
      call getenv('ANY_STRING',text)
      return
      end
c
c
c   put a general string  to enviromental variable
c
      subroutine put_env_string(text)
      implicit none
      character*(*) text
      character*100 text1
c      integer seiclen
      text1=' '
      write(text1,'(a,a)')'ANY_STRING=',text
      call putenvsun(text1)   !c call
      return
      end



      subroutine get_agency(agency)
c
c   get agency 
c
      character*(*) agency 
      call getenv('AGENCY',agency)
      return
      end
c
c   get Environment Architecture...
c
      integer function get_arch( chr_arch )
      character    chr_arch *(20)          ! Operating system
      call getenv('SEISARCH',chr_arch)
      chr_arch = chr_arch(:index(chr_arch,' ')-1) //
     &           char(0)                   ! Add a null.
      get_arch = 0                         ! Return a success.
      return
      end
c
c
c  send plot to laser
c
      subroutine send_plot(t,ilength)
      character*(*) t
      character*240 text
      integer ilength
      text=t
      call systemc('lpr '//text(1:ilength),ilength+4)
c     call systemc('lp -c '//text(1:ilength),ilength+6)
c     call system('lp -c '//text(1:ilength))
c
c due to problem on Solaris, wait for 5 secs after each plot
c
      call sleep(5)
      return
      end

      subroutine get_display(display_type)
      implicit none
      integer display_type
      character*80 text
c
c   get display type
c
      call getenv('DISPLAY',text)
      display_type=1                        ! x-window
      if(text(1:2).eq.'  ')display_type=0   ! tektronics 
      return
      end

c
      subroutine dir_char(dchar)
c   directory separator character
      implicit none
      character*1 dchar
      dchar='/'
      return
      end
c
c   input of arguments using sun calls
c
      subroutine get_arguments(nars,argument)
      implicit none
      integer nars,i,iargc
c      character*80 argument(*)
      character*(*) argument(*)
c
c   get number of arguments
c
      nars=iargc()
c
c   get actual arguments
c
      if(nars.gt.0) then
         do i=1, nars
           call getarg(i,argument(i))
         enddo
      endif
c
      return
      end
c
c  clear underflow on sun to gewt rid of message at end of execution
c
      subroutine clear_underflow
c      j=ieee_flags('clear','exception','all')
c     write(6,*)'clear'
      return
      end
c      
      real function sei rand( function )
CSTART**************************************************************************
C                                                                              *
C   Supplier          : BGS/GSRG Applications Programming Unit                 *
C   System            : SEISAN                                                 *
C   Procedure Name    : SEI RAND                                               *
C   Purpose           : To set up a random sequence (seed 1) or return a       *
C                       a random number depending on function                  *
C   Arguments  -input : function (I) function to perform. Values are:          *       
C                                    OPEN$ - seed the generator                *
C                                    READ$ - read a random value               *
C   Note              : For use on SUN                                         *
C                       Function value returns random number (-1.0 -> +1.0)    *
C   Author            : J. A. Bolton                                           *
C   Date              : 4 July 1995                                            *
C   Version           : V01                                                    *
C                                                                              *
CEND****************************************************************************
c
      external     drand                   ! Random # generator (0.0->1.0)
     &            ,sei code                ! Error handler.
      real*8       drand                   ! & function.
c
c    System definitions...
c    =====================
c
      include 'libsei.inc'                 ! Library definitions.               
c
c    Arguments...
c    ============
c
      integer      function                ! Function toperform.
c
c    Local variable...
c    =================
c
      real*8       value                   ! & value.
      logical      b_flag                  ! Dummy operations flag.
c
c    Initialise...
c    =============
c
      if( function .eq. open$ ) then       ! Set up.
c      value = drand(1)                     ! Start the randomiser.
      value = 0.0d0                        ! Re-set returned value.
c            
c    Read a random number...
c    =======================
c
      else if( function .eq. read$ ) then  ! Get a random number.
      value = -1.0d0 + 2.0d0 ! *drand(0)      ! & value.
c
c    Invalid function...
c    ===================
c
      else                                ! Invalid.
      call sei code( stop$, e_init$, 0, b_flag ) ! Abort, bad initialisation.
      end if                              !
c
c     Return to Caller...
c     ===================
c
9999  sei rand = value                    ! Install the number.
      return
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   dummies
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c

      subroutine gettim(i1,i2,i3,i4,i5)
c dummy from pc
      implicit none
      integer*2 i1,i2,i3,i4,i5
      return
      end
c
      subroutine getdat(i1,i2,i3)
c   dummy from pc
      integer*2 i1,i2,i3
      return
      end
c
c
      subroutine clear_to_text()
      return
      end
c
      subroutine set_color_pc(color)
      return
      end
c
      subroutine sscursr(key,ix,iy)
      integer key,ix,iy
      return
      end
c
      subroutine set_back_pc(color)
      integer color
      return
      end
c
      subroutine dirf_pc(dir,file,n)
      character*(*) dir
      character*(*) file(*)
      integer n
      return
      end
c

      subroutine movabs(ix,iy)
      integer ix,iy
      return
      end
c
      subroutine drwabs(ix,iy)
      integer ix,iy
      return
      end
c
      subroutine initt(baud)
      integer baud
      return
      end
c
      subroutine finitt(k,j)
      integer k,j
      return
      end
c
      subroutine erase()
      return
      end
c
      subroutine tsend()
      return
      end
c
      subroutine anstr(char,i)
      integer i
      character*1 char(*)
      return
      end
c
      subroutine scursr(ich,ix,iy)
      integer ich,ix,iy
      return
      end
c
      subroutine ancho(i)
      integer i
      return
      end

      subroutine anstrpc(it,k,ix,iy)
      integer it
      character*80 k
      integer ix,iy
      return
      end

      subroutine get_env_psscale(xscale,yscale)
c
c   get scaling parameters 
c
      implicit none
c      character*80 text
      character*5 scale 
      real xscale,yscale
      integer sei clen
c
c  get SEISAN_PSSCALE_X variable
c
      call getenv('SEISAN_PSSCALE_X',scale)
c
c   set to default if not set
c
      if (sei clen(scale).eq.0) then
         xscale=0.55
      else
        read(scale,'(f5.2)') xscale
      endif

c
c  get SEISAN_PSSCALE_Y variable
c
      call getenv('SEISAN_PSSCALE_Y',scale)
c
c   set to default if not set
c
      if (sei clen(scale).eq.0) then
        yscale=1.0
      else
        read(scale,'(f5.2)') yscale
      endif

      return
      end


      subroutine uncompress_file(file)
c
c check if file is compressed and uncompress
c

      implicit none

      character*(*) file      ! file name
      character*220 text     ! system call
      character*60 top_dir   ! Seisan top directory
      character*1 dchar      ! Directory deliminator
      character*20 compression_format
      integer seiclen
      integer i,j            ! counters

      call topdir(top_dir)
      call dir_char(dchar)

      compression_format = ' '

c
c check which compression format used  
c
      if (file(seiclen(file)-1:seiclen(file)).eq.'gz') then
        compression_format = 'gzip'
      elseif (file(seiclen(file)-2:seiclen(file)).eq.'zip') then
        compression_format = 'zip'
      elseif (file(seiclen(file)-1:seiclen(file)).eq.'.Z') then
        compression_format = 'compress'
      elseif (file(seiclen(file)-2:seiclen(file)).eq.'bz2') then
        compression_format = 'bzip2'
      endif

      if (compression_format.eq.' ') then
        return
      endif

c
c copy file to TMP directory
c
      text = ' '
      text = 'cp -f ' // file(1:seiclen(file)) // ' ' //
     *         top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar

      write(*,*) text(1:seiclen(text))
      call systemc(text,seiclen(text))

c
c set j to last '/'
c
      j=0
      do i=seiclen(file),1,-1
        if (file(i:i).eq.'/'.and.j.eq.0) j=i
      enddo
      j=j+1

c 
c uncompress if gzip
c   
      text = ' '
      if (compression_format.eq.'gzip') then
        text = 'gunzip -f ' // top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file))
        write(*,*) text(1:seiclen(text))
        call systemc(text,seiclen(text))
        file = top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file)-3)

c 
c uncompress if zip 
c   
      elseif (compression_format.eq.'zip') then
        text = 'unzip ' // top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file))
        write(*,*) text(1:seiclen(text))
        call systemc(text,seiclen(text))
        file = top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file)-4)
c 
c uncompress if compress
c   
      elseif (compression_format.eq.'compress') then
        text = 'uncompress ' // top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file))
        write(*,*) text(1:seiclen(text))
        call systemc(text,seiclen(text))
        file = top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file)-2)
c 
c uncompress if bzip2
c   
      elseif (compression_format.eq.'bzip2') then
        text = 'bzip2 -d ' // top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file))
        write(*,*) text(1:seiclen(text))
        call systemc(text,seiclen(text))
        file = top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file)-4)

      endif

      return
      end


