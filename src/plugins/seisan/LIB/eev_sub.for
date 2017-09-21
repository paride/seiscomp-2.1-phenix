c
c    eev subroutines moved to LIB oct 1 99
c
c    changes
c
c  oct 01 99 by jh: Check for id before checking for evfile name
c  oct 4          : replace read_s_file and write_s_file by 
c                           read_s_file_e and write_s_file_e to avoid
c                   conflict with nearly same files in mulplt
c  nov 3          : check for null chars after edit
c  nov 28         : stop if wrong time interval, check better Eyyymm option
c                   fix map option for PC
c  nov 30         : new options comment and U
c  jan 11 00      : variable index_file gets changed when calling mulplt for
c                   unknown reason, to patch it up, its value was saved in
c                   old_index_file before call to mulplt and restored
c                   after the call
c  feb 16 00      : remove screen output for hyp when used with focmec etc
c                   make commands pw,ph,pb for plotting synthetics
c  feb 23 00      : use parameter if to delete auto phases
c  mar 10 00      : add command print
c  may 16 00      : check if id and file name ok for several operations like
c                   R,REG and C
c  may 30 00  jh  : do not delete SEISNET request lines if reg
c  aug 18 00 jh   : add grid search, wadati
c  oct 19         : fix so if a bad seisan.mes is present, eev will not crash
c  feb 2001       : BGS changes, hypo71
c  feb 23     jh  : add local data base for copy function
c  mar 2          : accept distnace in f-format
c  feb 1, 02  jh  : do not delete s-file with a request line 
c  april 11   lot : changed seisnet request check
c  april 11   lot : register in any database, copy wave files into waveform database setup in SEISAN.DEF
c  may 7 02   lot : keep seisnet request, if event registerd into another
c                   database
c  may 27 02  lot : added selection for volcanic sub classes
c  jun 04 02  lot : add register of events from local database
c  aug 23 02  je  : added quarrycheck option
c  oct 17 02  lo  : fixed problem with event type when register
c  nov 25 02  lo  : add id if not exist when registering
c  feb 02 03  jh  : fix bug in invrad.out
c  may 14 03  jh  : explosion input
c  jul 21 03  jh  : mak esure invrad can use Pg and Sg
c  
      subroutine eev_action(base_name,start_time,end_time,keys,
     *command,event_no,evfile,fstart,index_file,
     *from_eev,from_mulplt,operator)
c
c   routine for the action of eev
c
      implicit none                                                 
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Open file definitions
       include 'seidim.inc'                ! dimensions
       include 'seisan.inc'
       include 'waveform.inc'
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C

c-- top directory name
      character*60 top_directory
c-- time interval chosen          
      CHARACTER*14      START_TIME,END_TIME     
c-- data base name                
      CHARACTER*40      BASE_NAME               
      character*80  wave_file   ! waveform file name
c-- input fron transfer file      
c      character*40      base_or_time            
c-- one line                                            
      character*80	text
      character*160     pitsa_text,sac_text 
      character*200 txt			
c-- answer
      character*1 choise
      character*80 eev_edit   ! editor name
      integer sei clen            ! function to get string length
      integer from_eev     ! indicate to routine findevin that call is from eev
c-- unix command
      character*80 command
c-- event file name               
      character*80      evfile,oldfile          
c-- waveform file name
      character*80      wavefile,wavefile1,directory
      character*2       new_type   ! new type of event
      character*1       old_type   ! old type of event
c-- old and new----               
      character*80      newfile   !,master 
c-- ev. selection key             
      character*10      keys                    
c-- file name printed out         
      character*80      out_name                
c-- base name to move file to                   
      character*5       new_base
c-- answer                        
      character*1       answer                  
c-- event id line number
      integer id
c-- operator code
      character*(*) operator
      character*5 reg_base         ! database to register in
      character*4 cwavyear
      character*2 cwavmon
      character*80 from_mulplt     ! command from mulplt
      character*1    mulplt_def    ! if O, all default for mulplt plotting

c---time of making update
      character*12 p_time
      character*14 proc_time
c-- event number                  
      integer           event_no,oldno,current_event_number,old_event_no
c-- new month indicator           
      integer           new_month               
c-- status of event search        
      integer           status                  
c-- see base                      
      integer           fstart,old_fstart
c-- event arrays                  
      character*80      data1(max_data),data2(max_data)   
c-- no of stations for event                        
      integer		nstat1,nstat2		
c-- no of phases for one event
      integer nphase1
c-- no of records for event                  
      integer     	nrecord1,nrecord2	
c-- no of headers for event                         
      integer		nhead1,nhead2		
c-- event ids--------------               
      character*1	exp1,exp2,evid1,evid2	
      integer show_menu            ! indicator if menu shown in mulplt
      real dist,azim,amp           ! distance and azimuth and amplitude		
      real strike,dip,rake
      real hdepth                  ! hypocenter depth
      real amp_error               ! error in amplitudes for mom tensor
      real depth(50),vp(50)        ! model

      real minlat,maxlat,minlon,maxlon,dlat,dlon   ! for gridsearch
c
c-- number of errors in s-file
      integer nerr
c-- counter                                                    
      integer		i,k,ilen,l
      logical       loc_append  ! true if appended events only to be located
      logical b_flag,exist  ! for input output
c--- logicals for fault plane solution 
      logical oldfault,newfault
c-- dirctory separation character
      character*1 dchar
c-- indicator of computer used
          logical sun,pc,linux
c logical for end of file
       logical          b_eof
c logical for existing file or not
       logical          b_old
c returned code
       integer          code
c read unit #1 .. #6
       integer          read01, read02, read03 
c write unit #1 .. #5
       integer          write01,  write03, write04
c file name
       character*80     chr_file
c indicator of the use of an index file
       logical index_file, old_index_file
       real sec          ! seconds
       integer isec      ! seconds
       character*3 month(12)
       real range        ! help variable
       integer year,mon,day,hour,min
c length of the command line to call vi editor, LAA 09.97
       integer leng
c variables for epimap
       real map_lat_c,map_lon_c
c number of wave files for conversion to pitsa
       integer npitsa,nsac
c list of wave files
       character*80 file_list(99)
c command line to run gmtmap
       character*160 gmtmap
c line to run user command
       character*160 usercom
c volcanic sub class
       character*6 sub_class
c flag
       logical request_flag
c SEISAN extension
       character*10 extension
       common/eev/data1,nstat1,nphase1,nrecord1,nhead1,id,exp1,evid1
       data month /'Jan','Feb','Mar','Apr','May',
     * 'Jun','Jul','Aug','Sep','Oct','Nov','Dec'/ 

      call topdir(top_directory)
      call dir_char(dchar)
      call computer_type(sun,pc,linux)
      call get_seisan_def
      call get_env_seisan_extension(extension)



c
c   jump data base positioning
c
      goto 15
c
c   back here for data base action before return
c
 10   continue
        
C                                                                               
C  data base action, find next event                                                   
C                                                                               
c
      call findevin
     *(base_name,start_time,end_time,keys,from_eev,event_no, 
     *evfile,fstart,new_month,status)
      call put_env_event(evfile)   ! put event file name in memory
c
c  check if previous command was from mulplt, if so, reset and return to
c  mulplt
c
      if(from_mulplt(1:4).ne.'    ') then
         from_mulplt(1:10)='P         '
      endif

c
c   if a jump to another month is made, reset jump indicator
c
      if(from_eev.eq.0) from_eev=1
c
c                                                                               
C   CHECK IF END OF TIME INTERVAL OR OTHER ERRORS                               
C                                                                               
      IF(STATUS.GT.0) THEN                                                      
         if(status.eq.1) write(6,*)' ***** event not found'                     
         if(status.eq.5) write(6,*)' ***** wrong time interval'                 
c
c   if an empty index file, stop
c
         if(event_no.eq.0.and.status.eq.3) stop
         if(status.eq.9.or.status.eq.5) stop   ! something wrong
c
cccc the next section is probably not active anymore since variable
c    from_eev prevent status of being 2 or 3
cccc
c
c   if here, assume end of month, status=3 start again
c
c-- begin with first event        
         keys(1:10)='#1        '
         goto 10                                
      ENDIF  
      return                                                                   
c
c-- continue here at entry to subroutine                      
c
15    continue
c
c---------------------------------------------------------------------
c   check here if eev should go over several months indicated by eyymm
c   e.g. e9202
c---------------------------------------------------------------------
c
      if((keys(1:1).eq.'e'.or.keys(1:1).eq.'E').and.
     *   keys(2:3).ne.'  ') then
         read(keys(2:7),'(i6)',err=7733) i   ! just check if a valid number
c         goto 7734
c 7733    continue
c         keys(1:4)='SAME'
c         goto 10
c 7734    continue
         end_time(1:6)=keys(2:7)
         if (end_time(5:6).eq.'  '.or.end_time(3:4).eq.'  '.or.
     *       end_time(2:2).eq.' ') then
           write(*,'(a)') ' TRY Eyyyymm ********************'
           keys(1:4)='SAME'
           goto 10
         endif
         keys=' '
         from_eev=2
         goto 10
      endif

 7733 continue          
c
c-----------------------------------------------------------------------
c  check if eev jump to another month and base, j command
c-----------------------------------------------------------------------
c
      if((keys(1:1).eq.'j'.or.keys(1:1).eq.'J').and.
     *   keys(2:5).ne.'     ') then
         read(keys(2:7),'(i6)',err=8733) i   ! just check if a valid number
         goto 8734
 8733    continue
         keys(1:4)='SAME'
         goto 10
 8734    continue
         start_time=keys(2:7)
         end_time=' '
         from_eev=0
         if(keys(9:10).ne.' ') then
           base_name(1:2)=keys(9:10) ! new base
           base_name(3:3)=command(1:1)
         endif
         keys=' '
         goto 10
      endif
         
                                                                                
c                                                                               
c   check for lower and upper case                                              
c                                                                               
      if(keys(1:7).eq.'autosig') keys(1:7)='AUTOSIG'
      if(keys(1:4).eq.'iasp') keys(1:4)='IASP'
      if(keys(1:8).eq.'herrmann') keys(1:8)='HERRMANN'
      if(keys(1:6).eq.'hersei') keys(1:6)='HERSEI'
      if(keys(1:5).eq.'bouch') keys(1:5)='BOUCH'
      if(keys(1:6).eq.'bousei') keys(1:6)='BOUSEI'
      if(keys(1:6).eq.'rmsdep') keys(1:6)='RMSDEP'
      if(keys(1:4).eq.'wkbj') keys(1:4)='WKBJ'
      if(keys(1:3).eq.'wad') keys(1:3)='WAD'
      if(keys(1:3).eq.'put')  keys(1:3)='PUT'
      if(keys(1:3).eq.'reg')  keys(1:3)='REG'
      if(keys(1:3).eq.'sac') keys(1:3)='SAC'
      if(keys(1:5).eq.'pitsa') keys(1:5)='PITSA'
      if(keys(1:3).eq.'map') keys(1:3)='MAP'
      if(keys(1:4).eq.'grid') keys(1:4)='GRID'
      if(keys(1:6).eq.'gmtmap') keys(1:6)='GMTMAP'
      if(keys(1:2).eq.'ss') keys(1:2)='SS'
      if(keys(1:2).eq.'s ') keys(1:2)='S '                                      
      if(keys(1:1).eq.'s') keys(1:1)='S'                                      
      if(keys(1:1).eq.'t') keys(1:1)='T'                                        
      if(keys(2:2).eq.'t') keys(2:2)='T'                                        
      if(keys(1:2).eq.'e ') keys(1:1)='E '                                        
      if(keys(1:3).eq.'dup') keys(1:3)='DUP'
      if(keys(1:1).eq.'d') keys(1:1)='D'                                        
      if(keys(1:2).eq.'p ') keys(1:2)='P '                                    
      if(keys(1:2).eq.'po') keys(1:2)='PO'
      if(keys(1:2).eq.'pw') keys(1:2)='PW'
      if(keys(1:2).eq.'ph') keys(1:2)='PH'
      if(keys(1:2).eq.'pb') keys(1:2)='PB'
      if(keys(1:1).eq.'a') keys(1:1)='A'                                        
      if(keys(1:1).eq.'l') keys(1:1)='L'                                        
      if(keys(1:6).eq.'hypo71') keys(1:6)='HYPO71'                             
      if(keys(1:1).eq.'h') keys(1:1)='H'                                        
      if(keys(1:6).eq.'invrad') keys(1:6)='INVRAD'                             
      if(keys(1:7).eq.'comment') keys(1:7)='COMMENT'
      if(keys(1:3).eq.'com') keys(1:3)='COM'
      if(keys(1:1).eq.'c') keys(1:1)='C'                                        
      if(keys(1:1).eq.'m') keys(1:1)='M'                                        
      if(keys(1:1).eq.'w') keys(1:1)='W'
      if(keys(1:1).eq.'r') keys(1:1)='R'                                        
      if(keys(1:1).eq.'z') keys(1:1)='Z'
      if(keys(1:2).eq.'fc') keys(1:2)='FC'
      if(keys(1:1).eq.'f') keys(1:1)='F'
      if(keys(1:1).eq.'o') keys(1:1)='O'
      if(keys(1:4).eq.'Synt') keys(1:4)='SYNT'
      if(keys(1:4).eq.'synt') keys(1:4)='SYNT'
      if(keys(1:5).eq.'print') keys(1:5)='PRINT'
      if(keys(1:3).eq.'Mac') keys(1:3)='MAC'
      if(keys(1:4).eq.'pmac') keys(1:4)='PMAC'
      if(keys(1:3).eq.'new') keys(1:3)='NEW'
      if(keys(1:8).eq.'inputepi') keys(1:8)='INPUTEPI' 
      if(keys(1:8).eq.'inputone') keys(1:8)='INPUTONE' 
      if(keys(1:6).eq.'update')    keys(1:6)='UPDATE'
      if(keys(1:7).eq.'usercom') keys(1:7)='USERCOM'
      if(keys(1:1).eq.'u') keys(1:1)='U'
      if(keys(1:6).eq.'quarry') keys(1:6)='QUARRY'  !JE Aug2002 Quarrycheck
      if(keys(1:3).eq.'exp') keys(1:3)='EXP'
c
c----------------------------------------------------------
c   help
c----------------------------------------------------------
c
      if(keys(1:1).eq.'?') then
         i=index(top_directory,' ')-1

         chr_file = top_directory(1:i)//'/DAT/EEV.HLP'
             call sei open(old$+warn$,            ! Open a existing file.
     &                     ' ',                   ! Prompt file name (n/a).
     &                     chr_file,              ! File name
     &                     read01,                ! Read unit #1
     &                     b_old,                 ! Already exists? (n/a).
     &                     code)                  ! Returned condition.
         if(code.ne.e_ok$) then
            write(6,*)' Help file missing'
            keys(1:4)='SAME'
            goto 10
         endif
         i=1
         do k=1,1000
            i=i+1
            if(i.eq.20)  then
               write(6,*)' Return for next page'
               read(5,'(a)') i
               i=1
            endif
            read(read01,'(a)',iostat=code) text
            call sei code(fort$,code,read01,b_eof)
            if (b_eof) go to 1001
            write(6,'(a)') text
         enddo
 1001    continue
         call sei close (close$,read01,code)
         keys(1:4)='SAME'
         goto 10
      endif
c
c----------------------------
c MULTILETTER COMMANDS
c----------------------------
c
c-----------------------------------------------------------------------
c put in epicenter and origin time
c
      if(keys(1:8).eq.'INPUTEPI') then
         chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
         if (code .ne. e_ok$) go to 1329
         goto 1330                                                         
 1329    continue                                                              
         write(6,*) 'No such event'                                            
         keys(1:4)='SAME'                                                       
        goto 10                                                                 
c                                                                               
 1330   continue                                                                
        call indata
     *  (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)      
c
c   update line
c
         write(6,'(a)') ' Input data for header line'
         write(6,'(a,a)') ' Type below some or all data below fields,',
     *   ' field starts with - or upper case chr'
         write(6,'(a,a,a)') ' Original information remain ',
     *              'unless new is put in,',' text is deleted with _'
         write(6,*)' Original data is shown below text line'

 
         write(6,'(a,a)') 
     *              ' YEAR MODA HRMI SECC LE-Latitu-Longitu-DeptFF',
     *              'AGA-NS-RMS MAGLXXX MAGLXXX MAGlXXX'
         write(6,'(a)') data1(1)(1:79)
         if(pc)read(5,'(a)')text(2:79)
         if(sun.or.linux)read(5,'(a)')text(1:79)
         text(1:1)=' '
         text(80:80)='1'
c
c   only non blank fields are put in
c
         do i=1,80
          if(text(i:i).ne.' ') data1(1)(i:i)=text(i:i)
          if(data1(1)(i:i).eq.'_') data1(1)(i:i)=' '
         enddo
c-- write in file                            
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
        do i=1,nrecord1
          write(write01,'(a)',iostat=code) data1(i)
          call sei code(fort$,code,write01,b_eof)
        enddo
        call sei close (close$,write01,code)
        keys(1:4)='SAME'
        goto 10
      endif
c
c----------------------------------------------------------------------
c  print S-file 
c---------------------------------------------------------------------
c
      if(keys(1:5).eq.'PRINT') then
        if(text_print.eq.' ') then
           write(6,*) 'Printer command not defined in SEISAN.DEF'
           keys(1:4)='SAME'
           goto 10
        endif
        call systemc(text_print(1:seiclen(text_print))//' '//
     *  evfile(1:fstart+20),seiclen(text_print)+fstart+21)
         write(6,*)
     *   ' S-file being printed'
        keys(1:4)='SAME'
        goto 10
      endif

c
c----------------------------------------------------------------------
c   input comments
c---------------------------------------------------------------------
c
      if(keys(1:3).eq.'COM') then
         write(6,*)
     *   ' Input comments, line by line, finish by blank line'
         i=1
 6342    continue
         read(5,'(a)') data2(i)            ! read comments
         if(data2(i).eq.' ') then
           i=i-1
           if(i.gt.0) then
              call read_s_file_e(evfile)
              do k=nrecord1, 2, -1          ! shift records to make room
                 data1(k+i)=data1(k)
              enddo
              do k=1,i
                 data1(k+1)(2:79)=data2(k)(1:78)
                 data1(k+1)(80:80)='3'
              enddo
              nrecord1=nrecord1+i
              nhead1=nhead1+1
              call write_s_file_e(evfile)
           endif
         else
           i=i+1
           goto 6342
         endif
         keys(1:4)='SAME'
         goto 10
      endif
c
c----------------------------------------------------------------------
c   input explosion data 
c---------------------------------------------------------------------
c
      if(keys(1:3).eq.'EXP') then
         write(6,'(a,a)')
     *   ' Inputting data for a known explosion,',
     *   ' event will be classified as an explosion'
c
c   read s-file
c
         call read_s_file_e(evfile) 
c
c   shift records 3 lines to make room
c
         do k=nrecord1, 2, -1          
            data1(k+3)=data1(k)
         enddo
c
c   classify main event entry as explosion
c
         data1(1)(23:23)='E'
c
c   enter agency, to be put in in 1 and second explosion line
c
         write(6,'(a,a)')
     *   ' Enter agency for explosion, 3 characters,',
     *   ' return for none and OTH will be used'
         read(5,'(a)') text
         data1(2)=' '                             ! first exp line
         data1(2)(23:23)='E'
         data1(2)(22:22)=data1(1)(22:22)          ! same type as in first line
         data1(2)(80:80)='1'
         data1(2)(41:43)='0.0'
         if(text(1:3).eq.' ') text(1:3)='OTH'     ! unknown agency
         data1(2)(46:48)=text(1:3)
c
c   enter info, could be ok from header line so start with that
c
         data1(2)(1:20)=data1(1)(1:20)
         write(6,'(a)') ' Input info for explosion line'
         write(6,'(a,a)') ' Type below some or all data below fields,',
     *   ' field starts with - or upper case chr'
         write(6,'(a,a,a)') ' Original information remain ',
     *              'unless new is put in,',' text is deleted with _'
         write(6,'(a)')' Original data is shown below text line'
         write(6,'(a,a)') 
     *              ' YEAR MODA HRMI SECC LE-Latitu-Longitu-Dept ',
     *              ' AGA'

         
         write(6,'(a)') data1(2)(1:48)
         if(pc)read(5,'(a)')text(2:79)
         if(sun.or.linux)read(5,'(a)')text(1:79)
c
c   only non blank fields are put in
c

         do i=1,48
             if(text(i:i).ne.' ') data1(2)(i:i)=text(i:i)
             if(text(i:i).eq.'_') data1(2)(i:i)=' '
         enddo
                        




c         write(6,*)
c    *   ' Enter latitude and longitude, return for none'
c         read(5,'(a)') text
c
c         if(text.ne.' ') then
c            call sei get values( 2, text, code )   ! Extract 2 values.        
c            code = e_ok$                           ! re-store.
c            dlat = array$(1)                       ! Retrieve.
c            dlon = array$(2)                       ! Ditto.
c            write(data1(2)(24:38),'(f7.3,f8.3)')   
c         endif
c
         data1(3)=data1(2)
         data1(3)(78:80)='E13'               ! special explosion line
c
c   last explosion line
c
         data1(4)=' '
         data1(4)(78:80)='EC3'
         data1(4)(2:11)='CHARGE(T):'
         write(6,*)' Enter charge in ton, return for unknown'
         read(5,'(a)') text
c
c   get charge
c
         if(text.ne.' ') then                 
            call sei get values( 1, text, code )   ! Extract 2 values.        
            code = e_ok$                           ! re-store.
            dlat = array$(1)                       ! Retrieve.
            write(data1(4)(12:20),'(f9.3)') dlat
         endif
c
c  comments
c
         write(6,*)' Enter comments, max 56 characters'
         read(5,'(a)') text
         data1(4)(22:77)=text(1:56)  
c
c   write event out again
c
         nrecord1=nrecord1+3
         nhead1=nhead1+3
         call write_s_file_e(evfile)
c
         keys(1:4)='SAME'
         goto 10
      endif

c
c------------------------------------------------------------------------
c Input a whole header type line
c------------------------------------------------------------------------
c
      if(keys(1:8).eq.'INPUTONE') then
         chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
         if (code .ne. e_ok$) go to 1529
         goto 1530                                                         
 1529    continue                                                              
         write(6,*) 'No such event'                                            
         keys(1:4)='SAME'                                                       
         goto 10                                                                
c
 1530   continue                                                                
         write(6,'(a)') ' Input type 1 line'
         write(6,'(a,a)') 
     *' Type below some or all info below fields,',
     *   ' field starts with - or capital letter'
         write(6,'(a,a)') 
     *' YEAR MODA HRMI SECC LE-Latitu-Longitu-DeptFF',
     *              'AGA-NS-RMS MAGLXXX MAGLXXX MAGlXXX'
         if(pc)read(5,'(a)')text(2:79)
         if(sun.or.linux)read(5,'(a)')text(1:79)
c        read(5,'(a)')text(2:79)
         text(1:1)=' '
         text(80:80)='1'
c                                                                               
        call indata
     *  (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)      
c-- write in file                            
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
          write(write01,'(a)',iostat=code) data1(1)
          call sei code(fort$,code,write01,b_eof)
          write(write01,'(a)',iostat=code) text
          call sei code(fort$,code,write01,b_eof)
        do i=2,nrecord1
          write(write01,'(a)',iostat=code) data1(i)
          call sei code(fort$,code,write01,b_eof)
        enddo
        call sei close (close$,write01,code)
        keys(1:4)='SAME'
        goto 10
      endif
c

c
c------------------------------------------------------------------------
c iaspei phase calculation
c------------------------------------------------------------------------
      if(keys(1:4).eq.'IASP') then
         call systemc("iasp",4)
         keys(1:4)='SAME'
         goto 10
      endif
c
c------------------------------------------------------------------------
c wadati diagram
c------------------------------------------------------------------------
c
      if(keys(1:3).eq.'WAD') then
         call systemc('wad_plot '//evfile(1:fstart+20),9+20+fstart)
         keys(1:4)='SAME'
         goto 10
      endif

c------------------------------------------------------------------------
c isoseismal calculation and effect input
c------------------------------------------------------------------------
      if(keys(1:4).eq.'PMAC') then
        if(.not.pc) then
          write(6,*)' Program only works on PC'
        else
          text='promac ' // evfile(1:fstart+20)
          call systemc(text,fstart+27)
        endif
        keys(1:4)='SAME'
        goto 10
      endif


c
c-----------------------------------------------------------------------
c  find next new event
c-----------------------------------------------------------------------
c
      if(keys(1:2).eq.'SS') then
         current_event_number=event_no       ! save so search only done once
         keys(1:4)='NEXT'
 1010    continue
         call findevin
     *   (base_name,start_time,end_time,keys,from_eev,event_no,
     *   evfile,fstart,new_month,status)
c
c   check if process has read whole month or base
c
         if(event_no.eq.current_event_number) then
            write(6,*)' No new event found *********************'
            keys(1:4)='SAME'
            goto 10
         endif
c  
         call read_s_file_e(evfile) 
c
c   check if new event
c

         if((data1(id)(9:11).eq.'NEW'.or.data1(id)(9:11).eq.'SPL'
     *       .or.data1(id)(9:11).eq.'HYP'.or.
     *       data1(id)(9:11).eq.
     *       'ARG').and.
     *       data1(1)(46:48).ne.'NAO') then
            write(6,*)' New event ------------------------------'
            keys(1:4)='SAME'
            goto 10
         else
            keys(1:4)='NEXT'
            goto 1010
         endif
      endif
            
c
c------------------------------------------------------------------------
c register or put event
c------------------------------------------------------------------------
c

      if(keys(1:3).eq.'PUT'.or.keys(1:3).eq.'REG') then
c
c   read file
c
         call read_s_file_e(evfile)
c
c   check that file not already has been registered
c
         if(id.gt.0) then
            if(data1(id)(9:11).eq.'REG'.or.data1(id)(9:11).eq.'REE'.
     *      or.data1(id)(9:10).eq.'UP' .or.data1(id)(9:11).eq.'REE'.
     *      or.data1(id)(9:11).eq.'UPD') 
     *      then
c added confirmation level, lot 25-7-2002
              if (confirmation_level.eq.1.) then
                write(6,*)' Event already registered, continue(y/n)'
                read(5,'(a)') answer
                if(answer.ne.'y'.and.answer.ne.'Y') then
                  keys(1:4)='SAME'
                  goto  10
                endif
              endif
            endif
         else
c
c add id line
c
            write(6,*)' No ID line, fixing it !!!'
            do k=nhead1+1,nrecord1,-1
              data1(k)=data1(k-1)
            enddo
            data1(nhead1+1)=data1(nhead1)
            nrecord1=nrecord1+1
            nhead1=nhead1+1
            id=nhead1-1
            read(evfile(seiclen(evfile)-18:seiclen(evfile)),
     &         '(i2,1x,i2,i2,1x,i2,3x,i4,i2)')
     &         day,hour,min,isec,year,mon
            data1(id)=
     &    ' ACTION:REE 02-11-22 00:01 OP:aut  STATUS:'//
     &    '               ID:20021122050017     I'
            call systime(p_time,proc_time)
            WRITE(data1(id)(13:26),'(A)')PROC_TIME
            write(data1(id)(61:74),'(i4.4,5i2.2)')
     &        year,mon,day,hour,min,isec

            write(*,'(a)') ' new ID line : '//data1(id)
c            write(6,*)' No ID line, fix it !!!'
         endif
c
c   check if id and file name ok
c
         if(id.ne.0) then 
            call check_id(evfile,evid1,data1(id),nerr)
            if(nerr.ne.0) then
               write(6,*)
     *         ' Filename and ID different or event type (L,R,D)'
               write(6,*)' not the same as in file name, fix it !!!'
               write(6,*)
               write(6,'(a,a,a,a)') ' ID and type ', data1(id)(61:74),
     *         ' ',evid1
               l=seiclen(evfile)
               write(6,'(a,a)')   ' S-file name ',evfile(l-18:l)
               write(6,*)' Return to continue'
               read(5,'(a)')answer
               keys(1:4)='SAME'
               goto 10
            endif
         endif

         write(6,*)' You are now about to register the current ',
     *   'event '
c in the data base.'
         write(6,*)
     *   ' File will be cleaned up and waveform files copied to WAV'
c only ask if confirmation wanted, 17-04-2002
         if (confirmation_level.eq.1.) then
           write(6,*)' Sure you want to register, (y/n) ?'
           read(5,'(a)') answer
           if(answer.ne.'Y'.and.answer.ne.'y') then
              keys(1:4)='SAME'
              goto 10
           endif
         endif

         write(6,*)                                                             
 1724    continue                                                               
c
c set database for registration
c
         if (seiclen(reg_base).eq.0) then
           write(6,*) ' Give 2-5 letter data base, ' //
     *       ' return for same base'
           read(5,'(a)') reg_base
           do k=1,5
             if (reg_base(k:k).eq.' ') reg_base(k:k)='_'
           enddo
         endif

         write(6,*)
     *   ' Change event type to L,R or D'
         write(6,*)
     *   ' A second character can be added for event ID (e.g. E)'
         write(6,*)' Return for no change ?'
         read(5,'(a2)') new_type                                                
         if(new_type(1:1).eq.'l') new_type(1:1)='L'                             
         if(new_type(1:1).eq.'r') new_type(1:1)='R'                             
         if(new_type(1:1).eq.'d') new_type(1:1)='D'                             
         if(new_type(2:2).eq.'e') new_type(2:2)='E'
         if(new_type(2:2).eq.'p') new_type(2:2)='P'
         if(new_type(2:2).eq.'v') new_type(2:2)='V'
         if(new_type(1:1).ne.'L'.and.new_type(1:1).ne.                         
     *   'R'.and.new_type(1:1).ne.'D'.and.new_type(1:1).ne.' ') then        
            write(6,*)' Wrong type ******'                                      
            goto 1724                                                  
         endif
         old_type=data1(1)(22:23)
         if (seiclen(new_type).le.0) new_type=old_type
c                                                                               
c   check if type is different, if so reset
c                                                  
         if(data1(1)(22:23).eq.new_type) then 
            write(6,*)' New type same as old, nothing changed'                  
         else
            if(new_type(1:1).ne.' ') data1(1)(22:23)=new_type ! put in new type
         endif

c
c if volcanic event, select subclass
c
         if (new_type(1:2).eq.'LV') then
           call select_volcano_subclass(sub_class)
c
c add line
c          
           data1(nhead1+1)=data1(nhead1)
           data1(nhead1)=' VOLC MAIN                              '
     &                //'                                       3'
           write(data1(nhead1)(12:17),'(a6)') sub_class
           nhead1=nhead1+1
         endif
c
c set database for registration
c
         if (seiclen(reg_base).eq.0) then
           write(6,*) ' Give 2-5 letter data base, ' //
     *       ' return for same base'
           read(5,'(a)') reg_base
           do k=1,5
             if (reg_base(k:k).eq.' ') reg_base(k:k)='_'
           enddo
         endif

         if((new_type(1:1).ne.' '.and.old_type.ne.
     *   new_type(1:1)).or.reg_base(1:1).ne.'_') then                                                                  
c
c   make new file name
c                                            

c
c set event file name, newfile
c


            if (seiclen(evfile).gt.19) then   ! lot 04-06-2002
c event not in local database
              newfile=evfile                   
              if (reg_base(1:1).ne.'_') then
                k=seiclen(newfile)-32
                newfile(k:k+4)=reg_base(1:5)
              endif
              newfile(fstart+10:fstart+10)=new_type(1:1) 
            else 
c event in local database
              newfile=top_directory(1:seiclen(top_directory))
     &                //dchar//'REA'//dchar//reg_base(1:5)//dchar//
     &                evfile(14:17)//dchar//evfile(18:19)//
     &                dchar//evfile(1:seiclen(evfile))
              fstart=seiclen(newfile) - 18
            endif
c
c   check if file can be opened or if it already is there
c
 7331       continue
            call sei open(unknown$+warn$,          ! Open file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      newfile,               ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
             if (code .ne. e_ok$) then                ! check 
                write(6,'(1x,a,a)')' Cannot open file: ',newfile
                keys='SAME'
                goto 10
             endif
c
c   check for duplicate event in base
c
             if(b_old) then                               ! check if file there
 7333           continue	  
                write(6,*)
                write(6,'(a)')
     *          ' File already exists in base, options are:'
                write(6,'(a)')
     *          ' Do not change type:                        Return'
                write(6,'(a)')
     *          ' Overwrite existing event:                       O'
                if(id.ne.0) write(6,'(a)')
     *          ' Create a new event in base, different ID:       N'
                read(5,'(a)') choise
                if(choise.eq.' ') then
                   write(6,*)' File type will not be changed'
                   keys(1:4)='SAME'
                   goto 10
                endif
                if((choise.eq.'n'.or.choise.eq.'N').and.id.ne.0) then
                   call inc_id(data1(id),newfile,fstart+18)
                   call sei close (close$,write01,code) ! close, a new will be tested
                   goto 7331                            ! test again
                endif
                if(choise.eq.'o'.or.choise.eq.'O') then
                   write(6,*)' Overwriting old event'
                   goto 8155   ! go to write out
                endif
                goto 7333                                ! no valid choise
             endif
c
c    write out
c
 8155    continue             ! from just above
               write(write01,'(a80)',iostat=code)
     *         (data1(i),i=1,nrecord1)
               call sei code(fort$,code,write01,b_eof)
               call sei close (close$,write01,code)
c
c check for request
c
              request_flag=.false.
              do i=2,nrecord1
                if (data1(i)(2:4).eq.'NET'.and.
     *            data1(i)(76:78).eq.'REQ') then
                  request_flag=.true.
                endif
              enddo

              write(6,'(a,a)')' New file       ', newfile(1:fstart+20)
              if (reg_base(1:1).ne.'_'.and.      ! lot 07-05-2002
     &            request_flag) then
c
c keep old file with Seisnet request lines only
c
                chr_file = evfile(1:fstart+20)
                call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
                do i=1,nrecord1
                  if ((data1(i)(2:4).eq.'NET'.and.
     &                 data1(i)(76:78).eq.'REQ').or.
     &                 data1(i)(80:80).eq.'1'.or.
     &                 data1(i)(80:80).eq.'I'.or.
     &                 data1(i)(80:80).eq.'7') then
                    if (data1(i)(80:80).eq.'1')then
                       data1(i)(22:22)=chr_file
     &       (seiclen(chr_file)-8:seiclen(chr_file)-8)
                    endif
                  write(write01,'(a80)') data1(i)

                  endif
                enddo
                write(6,'(a,a)')' Keeping file:  ', evfile(1:fstart+20)                 
                call sei close (close$,write01,code)

              else
                                                                                
c                                                                               
c   delete old event if no Seisnet request lines
c
                chr_file = evfile(1:fstart+20)
                call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
                call sei close (delete$,write01,code)
                write(6,'(a,a)')' Deleted file:  ', evfile(1:fstart+20)                 
              endif
c                                                                               
c   update event list                                                           
c                                                                               
              keys(1:3)='REN'                                                        
              CALL findevin
     *        (base_name,start_time,end_time,keys,from_eev,event_no,
     *        evfile,fstart,new_month,status)                                       
  
              evfile=newfile   ! new file name
c
         endif
c
c  copy waveform files to wav
c
         do k=2,nhead1
            if(data1(k)(80:80).eq.'6') then
                wave_file=' '
                txt=' '
                read(data1(k)(2:79),'(a)') wave_file(1:78)
             
c
c get waveform header info
c
                call wav_init
                call get_full_wav_name(wave_file,wav_filename(1))
              if(wav_filename(1).eq.' ') then
                  write(*,*) ' File not found: '//
     &              wav_filename(1)(1:seiclen(wav_filename(1)))

              else
                call read_wav_header(1)
                write(cwavyear,'(i4)') wav_year(1)
                write(cwavmon,'(i2)') wav_month(1)
                do i=1,2
                  if (cwavmon(i:i).eq.' ') cwavmon(i:i)='0' 
                enddo
  
                if( pc ) then
                  txt  = 'copy X' 

c                txt  = 'copy ' // wave_file(:seiclen(wave_file)) 
c     &                 // ' '
c     &                 // top_directory(:seiclen(top_directory)) 
c     &                 // dchar//'WAV'//dchar                    
c     &                 // wave_file(:seiclen(wave_file))
c
                else if( sun.or.linux ) then
                  txt  = 'cp   X' 
                else
                  chr_err_msg$ = 
     &'**** ERROR: could not determine the computer type'
                  call sei code( stop$, e_init$, 0, b_flag ) ! Halt program
                end if                                     !

                if(copy_wav_dir.eq.' ') then     ! allow to copy to database, same as in mulplt, lot 11-4-2002

                  txt = txt(1:seiclen(txt)-1) // 
     &                wav_filename(1)(:seiclen(wav_filename(1))) //
     &                ' '                                    //
     &                top_directory(:seiclen(top_directory)) //
     &                dchar//'WAV'//dchar                    //
     &                wave_file(:seiclen(wave_file))
                else

                  txt = txt(1:seiclen(txt)-1) //
     &                wav_filename(1)(:seiclen(wav_filename(1))) //
     &                ' '                                    //
     &                top_directory(:seiclen(top_directory)) //
     &                dchar//'WAV'//dchar                    //
     &                copy_wav_dir//dchar//cwavyear(1:4)//dchar //
     &                cwavmon(1:2)//dchar //
     &                wave_file(:seiclen(wave_file))
                endif


                write(6,'(1x,a)') txt                      ! show file name
c
c  check if file already there
c
                call sei open( check$,              ! Check file exists.
     &                    ' ',                 ! No prompt.
     &                    txt(7+seiclen(wav_filename(1)):seiclen(txt)),
     &                    0,                   ! Unit (n/a).
     &                    exist,               ! File exists?.
     &                    code )               ! Condition (n/a).
                if(exist) then
                   write(6,*)' File already transferred to WAV *****'
                endif
c
c now copy if not already there
c
                if(.not.exist) then
                   call systemc( txt,
     &                         seiclen(txt) )
c
c  check that file got there
c
                   call sei open( check$,        ! Check file exists.
     &                    ' ',                 ! No prompt.
     &                    txt(7+seiclen(wav_filename(1)):seiclen(txt)),
     &                    0,                   ! Unit (n/a).
     &                    exist,               ! File exists?.
     &                    code )               ! Condition (n/a).
                   if(exist) then
                      write(6,*)' File transferred to WAV **********'
                   else
                      write(6,*)' Failure to transfer to WAV ****'
                      write(6,*)' Return to continue'
                      read(5,'(a4)') i
                   endif
                   write(6,*)
                endif                          
              endif
            endif
         enddo 
c
c   check if current operator id given
c
         call get_operator(operator,linux)
c 4723       continue
c            if(operator.eq.'    ') then
c               write(6,*) 'Give operator code, max 4 characters'
c               read(5,'(a)') operator
c               goto 4723
c            endif
c
c   get system time
c
            call systime(p_time,proc_time)
c
c   update id line
c
            if(id.gt.0) then
               WRITE(DATA1(ID)(31:34),'(A)')OPERATOR
               WRITE(data1(id)(13:26),'(A)')PROC_TIME
               WRITE(data1(id)(9:11),'(A)')'REE'
            else
               write(6,*)' No ID line !!!!!!!'
            endif
c
c   clean up junk lines from seisnet, automatic picks and old header lines
c
            k=2
            data2(1)=data1(1)
            if(new_type(1:1).ne.' ') data2(1)(22:23)=new_type ! put in new type, lot 17-10-2002
            data2(2)=data1(id)
            if (data2(1)(45:45).eq.'*') data2(1)(45:45)=' '
            do i=2,nrecord1

              if( (data1(i)(2:4).eq.'NET'.and.data1(i)(80:80).eq.'3'.
     *             and.data1(i)(76:78).ne.'REQ') 
     *        .or. data1(i)(80:80).eq.'I'.or. 
     *            (data1(i)(80:80).eq.'1'.and.data1(i)(46:48).ne.'PDE')
     *        .or.(data1(i)(80:80).eq.' '.and.data1(i)(16:16).eq.'A'
     *               .and.keep_auto.eq.0.)
     *        .or.(data1(i)(2:6).eq.'SPECA')
     *        .or.(data1(i)(80:80).eq.'3'.and.data1(i)(2:7).
     *         eq.'ACTION'))
     *        then
                 continue    ! skip line
              else
                 k=k+1
                 data2(k)=data1(i)
                 if(data1(i)(76:78).eq.'REQ') then
                    write(6,*) ' This event contains a SEISNET',
     *              ' request line, is NOT deleted'
                 endif
              endif
            enddo
            data2(k+1)=' '
            nrecord1=k+1
            do i=1,nrecord1
              data1(i)=data2(i)
            enddo
            call write_s_file_e(evfile)

c
c   optinally start a process
c
            if(reg_autoprocess_flag.eq.2) then
               write(6,'(a,a)') ' Run process(y/return=n)? '
     *            ,reg_autoprocess_name
               call flush (6)
               read(5,'(a1)') answer
               if(answer.eq.'y'.or.answer.eq.'Y') then
                   call systemc(reg_autoprocess_name,10)
               endif
            elseif (reg_autoprocess_flag.eq.1) then
               write(*,*) ' running '//reg_autoprocess_name
               call systemc(reg_autoprocess_name,10)
            endif

         keys(1:4)='SAME'
         goto 10
      endif



c
c------------------------------------------------------------------------
c rmsdep calculation
c------------------------------------------------------------------------
      if(keys(1:6).eq.'RMSDEP') then
         call systemc("rmsdep",6)
         keys(1:4)='SAME'
         goto 10
      endif

c----------------------------------------------------------------
c
c  quarry check command  (JE Aug2002)
c-----------------------------------------------------------------
c
      if(keys(1:6).eq.'QUARRY'.and.extension.eq.'BGS') then
         write(6,*) 'Checking '//evfile//' against quarry criteria...'
         call quarrycheck(evfile)
         goto 10
      endif


c
c------------------------------------------------------------------------
c epimap old
c------------------------------------------------------------------------
 
       if(keys(1:3).eq.'MAP') then
 
         call put_env_event(evfile)
c
c check for location in S-file first
c
         call sei open(old$+warn$,                  ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       evfile,                ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition
         if (code .ne. e_ok$) go to 4724

         call indata
     *      (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)
            rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)

         call sei close (close$,write01,code)

         read(data1(1)(24:30),'(f7.3)') map_lat_c
         read(data1(1)(31:38),'(f8.3)') map_lon_c

         if (map_lat_c.ne.0.0.and.map_lon_c.ne.0.0) then

               call sei open(unknown$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       'hyp.out',             ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition

           do k=1,nrecord1
             write(write01,'(a80)') data1(k) 
           enddo

           call sei close (close$,write01,code)
           goto 4726

         else
           write(*,*) ' sorry but you need to have a location !'
           goto 4724
         endif


c
c try to locate and use that location, if no location given in S-file
c
c
c         call systemc('hyp',3)
c               chr_file = 'hyp.out'
c               call sei open(old$+warn$,            ! Open a existing file.
c     &                       ' ',                   ! Prompt file name (n/a).
c     &                       chr_file,              ! File name
c     &                       write01,               ! Write unit
c     &                       b_old,                 ! Already exists? (n/a).
c     &                       code)                  ! Returned condition.
c            if (code .ne. e_ok$) go to 4724
c            goto 4725
c 
c            write(6,*) 'No such event'
c 
 4724       continue
            keys(1:4)='SAME'
 
            goto 10
 

c 4725       continue
c 
c            call indata
c     *      (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)
c            rewind (write01,iostat=code)
c            call sei code(fort$,code,write01,b_eof)
c 
c            call sei close (close$,write01,code)
c 
c 
c            read(data1(1)(24:30),'(f7.3)') map_lat_c
c            read(data1(1)(31:38),'(f8.3)') map_lon_c
 4726       continue
 
c            if (map_lat_c.eq.0.0.and.map_lon_c.eq.0.0) then
c              write(*,*) 'no location !!!'
c              goto 10
c            endif

c
c enter map_lat and map_lon if one of them is 0.
c
            if (map_lat.eq.0.or.map_lon.eq.0) then
              write(*,*) ' distance of map border from '
     *           // 'event location in degrees (e.g. 10.) '
              write(*,*) '     latitude: '
              read(*,'(f5.1)') map_lat
              write(*,*) '     longitude: '
              read(*,'(f5.1)') map_lon
            endif
 
            write(*,*) 'starting epimap ... '
            chr_file = 'epimap.inp'
 
            call sei open(unknown$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
 
 
            write(write01,'(i2)') map_proj
            write(write01,'(f6.1,1x,f6.1)') map_lat_c-map_lat,
     &             map_lat_c+ map_lat
            write(write01,'(f7.1,1x,f7.1)') map_lon_c-map_lon,
     &             map_lon_c+map_lon
            write(write01,*)
            write(write01,'(a3)') '1 1'
            write(write01,'(a3)') '1 1'
            write(write01,*)
            txt  = top_directory(:seiclen(top_directory))
     &           // dchar // "DAT" // dchar //
     &           map_file(:seiclen(map_file)) // ".MAP"
            write(write01,'(a80)') txt
            write(write01,'(a31)') '$$$$$$end of contour list$$$$$$'
            write(write01,*)
            write(write01,*)
            write(write01,*)
            write(write01,'(a1)') map_stations
            write(write01,'(a7)') 'hyp.out'
            write(write01,'(a38)') '$$$$$$end of epicentre file list$$$$$
     *$'
            write(write01,*)
            write(*,*) 'writing epimap.inp'
            call sei close (close$,write01,code)
 
            call systemc("epimap epimap.inp",17)
 
 
           goto 10
 
         endif


c
c------------------------------------------------------------------------
c gmtmap
c------------------------------------------------------------------------

       if(keys(1:6).eq.'GMTMAP') then

         if (pc) then
           keys(1:4)='SAME'
           goto 10
         endif

c
c check for location in S-file first
c
         call sei open(old$+warn$,                  ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       evfile,                ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition
         if (code .ne. e_ok$) then
           keys(1:4)='SAME'
           goto 10
         endif
            

         call indata
     *      (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)

         call sei close (close$,write01,code)

         read(data1(1)(24:30),'(f7.3)') map_lat_c
         read(data1(1)(31:38),'(f8.3)') map_lon_c

         if (map_lat_c.eq.0.and.map_lon_c.eq.0) then
           write(*,*) ' sorry but you need to have a location !'
           keys(1:4)='SAME'
           goto 10
         endif

c
c enter map_lat and map_lon if one of them is 0.
c
         if (map_lat.eq.0.or.map_lon.eq.0) then
           write(*,*) ' distance of map border from '
     *           // 'event location in degrees (e.g. 10.) '
           write(*,*) '     latitude: '
           read(*,'(f5.1)') map_lat
           write(*,*) '     longitude: '
           read(*,'(f5.1)') map_lon
         endif

         write(*,*) 'starting gmtmap ... '

         write(gmtmap,'(3a,f5.1,1x,f5.1,a,f5.1,1x,f5.1)') 
c     &      'gmtmap.exp -seisan nordic ',   ! changed nov 2000, lo
     &      'gmtmap.exp -seisan_file ',
     &      evfile(1:seiclen(evfile)),
     &      ' -res f -lat ',map_lat_c-map_lat,map_lat_c+map_lat,
     &      ' -lon ',map_lon_c-map_lon,map_lon_c+map_lon
         write(*,*) gmtmap(1:seiclen(gmtmap))

        do i=20,seiclen(gmtmap)
          if (gmtmap(i:i).eq.' '.and.gmtmap(i+1:i+1).eq.' ') then
            gmtmap(i:seiclen(gmtmap)-1)=gmtmap(i+1:seiclen(gmtmap))
          endif
        enddo

        write(*,*) gmtmap

c            write(write01,'(f6.1,1x,f6.1)') map_lat_c-map_lat,
c     &             map_lat_c+ map_lat
c            write(write01,'(f7.1,1x,f7.1)') map_lon_c-map_lon,
c     &             map_lon_c+map_lon

            call systemc(gmtmap,seiclen(gmtmap))
            call systemc("gs gmt.ps",9)

           goto 10

         endif

c
c------------------------------------------------------------------------
c grid search
c------------------------------------------------------------------------
 
       if(keys(1:4).eq.'GRID') then
         call put_env_event(evfile)
         call systemc('hyp -gridsearch',15)
c
c   check if plotting the grid
c
         write(6,*)' Plot rms grid (y=default/n)'
         read(5,'(a)') answer
         if(answer.eq.'n') then
            keys(1:4)='SAME'
            goto 10
         endif

               chr_file = 'gridsearch.out'
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
            if (code .ne. e_ok$) go to 4924
            goto 4925
 
            write(6,*) 'No gridsearch.out file, hyp failure'
 
 4924       continue
            keys(1:4)='SAME'
            goto 10
 

 4925       continue

            read(write01,'(a)') i
            read(write01,'(40x,2f10.3)') minlat,maxlat
            read(write01,'(40x,2f10.3)') minlon,maxlon
            call sei close (close$,write01,code)
c
c   get grid search solution for header
c
            call read_s_file_e('hyp.out')
 
            write(*,*) 'starting epimap ... '
            chr_file = 'epimap.inp'
 
            call sei open(unknown$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
 
 
            write(write01,'(i2)') map_proj
            write(write01,'(f6.1,1x,f6.1)') minlat,maxlat
            write(write01,'(f7.1,1x,f7.1)') minlon,maxlon
            write(write01,*)
            range=maxlat-minlat
            if(range.ge.100.0)                    dlat=20.0
            if(range.lt.100.0.and.range.ge. 50.0) dlat=10.0
            if(range.lt.50.0. and.range.ge. 10.0) dlat= 5.0
            if(range.lt.10.0. and.range.ge.  5.0) dlat= 1.0
            if(range.lt.5.0.  and.range.ge.  1.0) dlat= 0.5
            if(range.lt.1.0.  and.range.ge.  0.5) dlat= 0.1
            if(range.lt.0.5)                      dlat=0.05
            range=maxlon-minlon
            if(range.ge.200.0)                    dlon=40.0
            if(range.lt.200.0.and.range.ge.100.0) dlon=20.0
            if(range.lt.100.0.and.range.ge. 50.0) dlon=10.0
            if(range.lt.50.0. and.range.ge. 10.0) dlon= 5.0
            if(range.lt.10.0. and.range.ge.  5.0) dlon= 1.0
            if(range.lt.5.0.  and.range.ge.  1.0) dlon= 0.5
            if(range.lt.1.0.  and.range.ge.  0.5) dlon= 0.1
            if(range.lt.0.5)                      dlon=0.05
            write(write01,'(2f7.2)') dlat,dlon
            write(write01,'(2f7.2)') dlat,dlon
            write(write01,*)
            txt  = top_directory(:seiclen(top_directory))
     &           // dchar // "DAT" // dchar //
     &           map_file(:seiclen(map_file)) // ".MAP"
            write(write01,'(a80)') txt
            write(write01,'(a31)') '$$$$$$end of contour list$$$$$$'
            write(write01,'(a)') data1(1)(2:79)
            write(write01,*)
            write(write01,'(a)') 'gridsearch.out'
            write(write01,*)
            write(write01,'(a7)') 'hyp.out'
            write(write01,'(a38)') '$$$$$$end of epicentre file list$$$$$
     *$'
            write(write01,*)
            write(*,*) 'writing epimap.inp'
            call sei close (close$,write01,code)
 
            call systemc("epimap epimap.inp",17)
 
 
           goto 10
 
         endif



c
c------------------------------------------------------------------------
c userdefined command
c------------------------------------------------------------------------
c
      if (keys(1:7).eq.'USERCOM') then
         usercom = ' '
         usercom = 'usercom -sfile ' // evfile(1:seiclen(evfile))
         write(*,*) ' ',usercom(1:seiclen(usercom))
         call systemc(usercom,seiclen(usercom))
         keys(1:4)='SAME'
         goto 10
      endif
        
         

c
c------------------------------------------------------------------------
c macroseismic input
c------------------------------------------------------------------------
c
      if(keys(1:3).eq.'MAC') then
         call systemc("macroin",7)
         keys(1:4)='SAME'
         goto 10
      endif
c
c-----------------------------------------------------------------------        
c  update event                                                          
c-----------------------------------------------------------------------        
c
      if(keys(1:6).eq.'UPDATE') then                                           
c
c   read and check if event should be updated
c        
         call read_s_file_e(evfile)
         if(data1(1)(45:45).eq.'*') then
            write(6,*)' Event marked for no location'
            keys(1:4)='SAME'
            goto  10
         endif
c
         call systemc('hyp',3)
c
c   copy file back to data base
c
         write(6,*)' You are now about to overwite the current ',
     *   'event in the data base.'
         write(6,*)' with the solution just shown'
         write(6,*) ' The catalog is not updated !!!!!'
         write(6,*)' Sure you want to update, (y/n) ?'
         read(5,'(a)') answer
         if(answer.eq.'Y'.or.answer.eq.'y') then
            if(sun.or.linux)call systemc('cp hyp.out '//evfile,91) 
            if(pc) call systemc('copy hyp.out '//evfile,91)
c             must compile with debug on  to do this ???????
c             whether using copy or rewrite, must be fixed
c             if(pc) then ! above copy chrashes on pc, so do below
c
               chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
            if (code .ne. e_ok$) go to 1929
            goto 1930                                                         
 1929       continue                                                              
            write(6,*) 'No such event'                                            
            keys(1:4)='SAME'                                                       
            goto 10                                                                
c
 1930       continue                                                                
            call indata
     *      (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)
            rewind (write01,iostat=code)
            call sei code(fort$,code,write01,b_eof)
c
c   check if current operator id given
c
         call get_operator(operator,linux)
c 4623       continue
c            if(operator.eq.'    ') then
c               write(6,*) 'Give operator code, max 4 characters'
c               read(5,'(a)') operator
c               goto 4623
c            endif

c
c   get system time
c
            call systime(p_time,proc_time)
c
c   update id line
c
            WRITE(DATA1(ID)(31:34),'(A)')OPERATOR
            WRITE(data1(id)(13:26),'(A)')PROC_TIME
            WRITE(data1(id)(9:11),'(A)')'UP '

            do i=1,nrecord1
               write(write01,'(a)',iostat=code) data1(i)
               call sei code(fort$,code,write01,b_eof)
            enddo
            call sei close (close$,write01,code)
         endif
         keys(1:4)='SAME'
         goto 10
      endif                                                                     
c

c
c-----------------------------------------------------------------------        
c  moment tensor with invrad                                                   
c-----------------------------------------------------------------------        
c
      if(keys(1:6).eq.'INVRAD') then                                                

               chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
            if (code .ne. e_ok$) go to 3929
            goto 3930                                                         
 3929       continue                                                              
            write(6,*) 'No such event'                                            
            keys(1:4)='SAME'                                                       
            goto 10                                                                
c
 3930       continue                                                                
            call indata
     *      (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)
            call sei close (close$,write01,code)
c
c   find relevant data
c
            read(data1(1)(39:43),'(f5.1)') hdepth
            k=0
            do i=nhead1+1,nrecord1
              if((data1(i)(17:17).eq.'D'.or.data1(i)(17:17).eq.'C').
     *        and.(data1(i)(11:12).eq.'PG'.or.data1(i)(11:12).eq.'SG'.
     *        or.data1(i)(11:12).eq.'Pg'.or.data1(i)(11:12).eq.'Sg').
     *        and.data1(i)(34:40).ne.'       '.and.
     *        data1(i)(71:75).ne.'     ') then
                k=k+1
                read(data1(i)(71:75),'(f5.0)') dist 
c jh 0203                dist=l
                read(data1(i)(77:79),'(i3)') l
                azim=l
                read(data1(i)(34:40),'(g7.1)') amp
                if(data1(i)(17:17).eq.'D') amp=-amp
                amp=amp/1000.0      ! convert to microns
                write(data2(k),'(1x,a4,1x,a1,1x,a1,f8.2,f11.2,5x,g9.3)') 
     *          data1(i)(2:5),data1(i)(8:8),data1(i)(11:11),dist,azim,
     *          amp
                if(data1(i)(11:11).eq.'S'.and.(data1(i)(8:8).eq.'Z'.or.
     *          data1(i)(8:8).eq.'R')) data2(k)(10:10)='V'    ! assume SV
                if(data1(i)(11:11).eq.'S'.and.data1(i)(8:8).eq.'T')
     *          data2(k)(10:10)='H'    ! assume SH
              endif
            enddo
            if(k.gt.4) then
               write(6,*)k,' data for inversion'
               chr_file = 'invrad.inp'
               call sei open(unknown$+warn$,            ! Open file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
               write(write01,*) ' Data for moment inversion'
               amp_error=0.1
               write(write01,'(i5,f5.1,f5.3)') k,hdepth,amp_error
               do i=1,k
                  write(write01,'(a80)') data2(i)
               enddo
               call sei close (close$,write01,code)
            else
               write(6,*)k,' data for inversion, not enough'
               keys(1:4)='SAME'
               goto 10
            endif
c
c   make model file
c
c
c  first look for station file in current directory, then in DAT
c
c
c  make file name for both file in current and dat directory
c  find if alternative model is to be used
c
         text=' '
         text(1:12)='STATION0.HYP'        ! Default.
         if(data1(1)(21:21).ne.' ') text(8:8)=data1(1)(21:21) ! specific model
 
          call sei get file( open$,           ! Get file.
     &                      read03,           ! Opened on this unit.
     &                      code,             ! Returned condition.
     &                      'DAT',            ! Alternative directory to search.
     &                      text )            ! For this station file.
 
c   read down to model, past 2 blank lines
c        
        k=0   ! counting blank lines
 3330   continue
        read(read03,'(a)',iostat=code) text
        call sei code(fort$,code,read03,b_eof)
            if(text(1:3).eq.'   ') k=k+1
            if(k.eq.2) goto 3340
            goto 3330
 3340    continue
c
c  read model
c
        k=0
 3350   continue
        read(read03,'(a)',iostat=code) text
        call sei code(fort$,code,read03,b_eof)
            if(text(1:3).eq.'   ') goto 3360
            k=k+1
            read(text,'(2f8.2)') vp(k),depth(k)
            goto 3350
 3360    continue
c
c   close file
c
         call sei close (close$,read03,code)
c
c   generate and write model
c
               call sei open(unknown$+warn$,            ! Open file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       'invrad.mod',          ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
c
c   add infinite layer
c
         depth(k+1)=700.0
         do i=1,k
            write(write01,'(2f8.2)') depth(i),vp(i)
            write(write01,'(2f8.2)') depth(i+1),vp(i)
         enddo  
         call sei close (close$,write01,code)
c
c   run inversion program
c
         call systemc('invrad',6)

c
c  update data base with fault plane solution, open file with solution
c
         call sei open(old$+warn$,                 ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      'invrad.out',          ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
c
         if( code .eq. e_ok$ ) then        ! Updating database.
           write(*,*)
     &     '.... updating database with  fault plane solution'
c
         else                              ! Can't open file.
            code = e_ok$                      ! Re-set condition.
            keys(1:4)='SAME'                  ! Back to loop.
            goto 10                           !
         end if                            !
c
c   read fault plane solution
c
 1234    continue
         read(write01,'(a80)') text
         if(text(1:6).eq.'STRIKE') then
             read(text(8:14) ,'(f7.1)') strike
             read(text(31:36),'(f6.1)') dip
             read(text(53:59),'(f7.1)') rake
             newfault=.true.
             goto 1235
         endif
         goto 1234
 1235    continue
c
         call sei close (close$,write01,code)
c
c   write out in data base if there was a solution
c
         text=' '
         if(newfault) then
            write(text,'(3f10.1)') strike,dip,rake
            text(79:80)=' F'
            text(71:78)='INVRAD  '
            chr_file = evfile(1:fstart+20)
            call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
            call indata(write01,nstat1,nphase1,nhead1,
     *      nrecord1,evid1,exp1,data1,id)        
            oldfault=.false.
            do i=2,nhead1
              if(data1(i)(79:80).eq.' F') then
                 data1(i)=text                     ! overwrite old solution
                 oldfault=.true.
               endif
            enddo
c
            rewind (write01,iostat=code)
            call sei code(fort$,code,write01,b_eof)
c
            if(oldfault) then
               write(write01,'(a80)',iostat=code) 
     *              (data1(i),i=1,nrecord1)
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 2539
c
            else
               write(write01,'(a80)',iostat=code) data1(1)
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 2539
c
               write(write01,'(a80)',iostat=code) text
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 2539
c
               write(write01,'(a80)',iostat=code) 
     *              (data1(i),i=2,nrecord1)
               call sei code(fort$,code,write01,b_eof)
            endif
c
c    Errors...
c
2539        if( code .ne. e_ok$ ) then
            chr_err_msg$ =                      ! Write error message.
     &'**** WARN: no update with fault plane solution ****'
            call sei code(fort$+warn$,code,write01,b_eof)
            end if
c
c    Close down file & back to menu...
c
            call sei close (close$,write01,code)
         endif
         keys(1:4)='SAME'
         goto 10
      endif                                                                     
c



c
c-----------------------------------------------------------------------        
c  Dublicate existing event                                                    
c-----------------------------------------------------------------------        
c
      if(keys(1:3).eq.'DUP') then
c
c   check if index file
c
      do i=1,40
        if(base_name(i:i).eq.'.') then
           write(6,*)' Cannot duplicate when using index file'
           keys(1:4)='SAME'
           goto 10
        endif
      enddo

c
c   copy existing event with a new id
c
c   open exising file
c
               chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
            if (code .ne. e_ok$) go to 2929
            goto 2930                                                         
 2929       continue                                                           
            write(6,*) 'No such event'                                        
            keys(1:4)='SAME'                                                   
            goto 10              
c
c   read event
c
 2930       continue                                                                
            call indata
     *      (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)
            call sei code(fort$,code,write01,b_eof)
            call sei close (close$,write01,code)
c
c   check if id and file name ok
c
         if(id.ne.0) then
            call check_id(evfile,evid1,data1(id),nerr)
            if(nerr.ne.0) then
               write(6,*)
     *         ' Filename and ID different or event type (L,R,D)'
               write(6,*)' not the same as in file name, fix it !!!'
               write(6,*)
               write(6,'(a,a,a,a)') ' ID and type ', data1(id)(61:74),
     *         ' ',evid1
               l=seiclen(evfile)
               write(6,'(a,a)')   ' S-file name ',evfile(l-18:l)
               write(6,*)' Return to continue'
               read(5,'(a)')answer
               keys(1:4)='SAME'
               goto 10
            endif
         endif

c
c   check if current operator id given
c
 4793       continue
            if(operator.eq.'    ') then
               write(6,*) 'Give operator code, max 4 characters'
               read(5,'(a)') operator
               goto 4793
            endif
c
c   get system time
c
            call systime(p_time,proc_time)
c
c   make new id and file name
c
           newfile=evfile                                                    
c           newfile(fstart-10:fstart-8)=new_base                              
c
c   make new id by adding one second
c
                call inc_id(data1(id),newfile,fstart+18)
c
c  get here to test if duplicate file already is there
c
 7531      continue
           chr_file = newfile(1:fstart+20)
              call sei open(unknown$+warn$,       ! Open a unknown status file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
           if (code .ne. e_ok$) go to 9154
           goto 9155                                                          
 9154      continue                                                          
           write(6,*)' Something wrong with data base'                                    
           keys(1:4)='SAME'                                                  
           goto 10                                                           
c
c   check for duplicate event in base
c
 9155     continue                                                          
          if(b_old) then                               ! check if file there
 7533        continue	  
             write(6,*)
             write(6,'(a)')
     *       ' File already exists in base, options are       :'
             write(6,'(a)')
     *       ' Do not make new file:                     Return'
             write(6,'(a)')
     *       ' Overwrite existing event:                       O'
             if(id.ne.0) write(6,'(a)')
     *       ' Create a new event,         different ID:       N'
             read(5,'(a)') choise
             if(choise.eq.' ') then
                write(6,*)' No new file made'
                keys(1:4)='SAME'
                goto 10
             endif
             if((choise.eq.'n'.or.choise.eq.'N').and.id.ne.0) then
                call inc_id(data1(id),newfile,fstart+18)
                call sei close (close$,write01,code) ! close, a new will be tested
                goto 7531                            ! test again
             endif
             if(choise.eq.'o'.or.choise.eq.'O') then
                write(6,*)' Overwriting old event'
                goto 7255   ! go to write out
             endif
             goto 7533                                ! no valid choise
          endif
c
c    write out
c
 7255     continue
c
c   update id line
c
           WRITE(DATA1(ID)(31:34),'(A)')OPERATOR
           WRITE(data1(id)(13:26),'(A)')PROC_TIME
           WRITE(data1(id)(9:11),'(A)')'DUP'
c
c  write
c
           write(write01,'(a80)',iostat=code) 
     *          (data1(i),i=1,nrecord1)
           call sei code(fort$,code,write01,b_eof)
           call sei close (close$,write01,code)
           write(6,'(a,1x,a)')' New event file: ',newfile(1:fstart+20)
c                                                                               
c   update event list                                                           
c                                                                               
           keys(1:3)='REN'                                                        
           CALL findevin
     *     (base_name,start_time,end_time,keys,from_eev,event_no,                          
     *     evfile,fstart,new_month,status)                                       
           keys(1:4)='SAME'                                                  
           goto 10                                                           
        endif                                                                   

c
c-----------------------------------------------------------------------        
c  NEW event                                                          
c-----------------------------------------------------------------------        
c
      if(keys(1:3).eq.'NEW') then                                                
c
c   check if index file
c
      do i=1,40
        if(base_name(i:i).eq.'.') then
           write(6,*)' Cannot make new event when using index file'
           keys(1:4)='SAME'
           goto 10
        endif
      enddo

c
c
c   check if current operator id given
c
 8723       continue
            if(operator.eq.'    ') then
               write(6,*) 'Give operator code, max 4 characters'
               read(5,'(a)') operator
               goto 8723
            endif
c
c   get system time
c
            call systime(p_time,proc_time)
c
c   get data for new event
c
         write(6,'(a,a)') ' Header line for new event, only info up ',
     *   'and including L is needed'
         write(6,'(a,a)')' Same base is assumed ***********'
 7473    continue            ! enter here if wrong data entry'
         write(6,'(a,a)') 
     *' Type below some or all info below fields,',
     *   ' field starts with - or capital letter'
         write(6,'(1x,a,a,a)') 
     *'CCYY MO',
     *'DA HRMI SECC LE-Latitu-Longitu-DeptFF',
     *              'AGA-NS-RMS MAGLXXX MAGLXXX MAGlXXX'
         if(pc)read(5,'(a)')text(2:79)
         if(sun.or.linux)read(5,'(a)')text(1:79)
         text(1:1)=' '
         text(80:80)='1'
c
         data1(1)=text
c
         read(text,'(1x,i4,1x,2i2,1x,2i2,1x,f4.1)',err=7474) 
     *   year,mon,day,hour,min,sec
c
c   error check
c
         goto 7475
 7474    continue
         write(6,*)' Something wrong with data entry, try again'
         goto 7473
 7475    continue
         if(text(22:22).ne.'L'.and.text(22:22).ne.'R'.and.text(22:22).
     *   ne.'D') then
            write(6,*)' Wrong event type (L)'
            goto 7473
         endif
         if(day.gt.31.or.hour.gt.24.or.min.gt.60.or.sec.gt.60.or
     *   .day.lt.1.or.hour.lt.0.or.min.lt.0.or.sec.lt.0) then
            write(6,*) ' Wrong day or time'
            goto 7473
         endif
c
c  make ID line
c
         isec=sec
         data1(2)=' '
         data1(2)(1:40)= ' ACTION:                   OP:     STATU'
         data1(2)(41:80)='S:               ID:                   I'
         WRITE(data1(2)(61:75),'(i4,6I2)')
     *   YEAR,MON,DAY,HOUR,MIN,ISEC
         DO I=61,74
            IF(data1(2)(I:I).EQ.' ') data1(2)(I:I)='0'
         ENDDO
         WRITE(data1(2)(31:34),'(A)')OPERATOR
         WRITE(data1(2)(13:26),'(A)')PROC_TIME
         WRITE(data1(2)(9:11),'(A)')'NEW' 
         id=2
c
c   help line
c
         data1(3)( 1:40)=' STAT SP IPHASW D HRMM SECON CODA AMPLIT'
c         data1(3)(41:80)=' PERI AZIMU VELO SNR AR TRES W  DIS CAZ7'
         data1(3)(41:80)=' PERI AZIMU VELO AIN AR TRES W  DIS CAZ7'
         data1(4)=' '
         nrecord1=4

c
c   make new file name, assume same data base, year and month
c
           call sfilname
     *     (year,mon,day,hour,min,sec,base_name(1:5),
     *     data1(1)(22:22),newfile,i)

c
c  get here to test if duplicate file already is there
c
 8531      continue
           chr_file = newfile(1:fstart+20)
              call sei open(unknown$+warn$,       ! Open a unknown status file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
           if (code .ne. e_ok$) go to 8154
           goto 8455                                                          
 8154      continue                                                          
           write(6,*)' Something wrong with data base'                                    
           keys(1:4)='SAME'                                                  
           goto 10                                                           
c
c   check for duplicate event in base
c
 8455     continue                                                          
          if(b_old) then                               ! check if file there
 8533        continue	  
             write(6,*)
             write(6,'(a,a,a)')
     *       ' File already exists in base ',new_base,' options are:'
             write(6,'(a)')
     *       ' Do not make new file:                     Return'
             write(6,'(a)')
     *       ' Overwrite existing event:                       O'
             if(id.ne.0) write(6,'(a)')
     *       ' Create a new event,         different ID:       N'
             read(5,'(a)') choise
             if(choise.eq.' ') then
                write(6,*)' No new file made'
                keys(1:4)='SAME'
                goto 10
             endif
             if((choise.eq.'n'.or.choise.eq.'N').and.id.ne.0) then
                call inc_id(data1(id),newfile,fstart+18)
                call sei close (close$,write01,code) ! close, a new will be tested
                goto 8531                            ! test again
             endif
             if(choise.eq.'o'.or.choise.eq.'O') then
                write(6,*)' Overwriting old event'
                goto 8255   ! go to write out
             endif
             goto 8533                                ! no valid choise
          endif
c
c    write out
c
 8255     continue
c
c   update id line
c
           WRITE(DATA1(ID)(31:34),'(A)')OPERATOR
           WRITE(data1(id)(13:26),'(A)')PROC_TIME
           WRITE(data1(id)(9:11),'(A)')'NEW'
c
c  write
c
           write(write01,'(a80)',iostat=code) 
     *          (data1(i),i=1,nrecord1)
           call sei code(fort$,code,write01,b_eof)
           call sei close (close$,write01,code)
           write(6,'(a,1x,a)')' New event file: ',newfile(1:fstart+20)
c                                                                               
c   update event list                                                           
c                                                                               
           keys(1:3)='REN'                                                        
           CALL findevin
     *     (base_name,start_time,end_time,keys,from_eev,event_no,                          
     *     evfile,fstart,new_month,status)                                       
           keys(1:4)='SAME'                                                  
           goto 10                                                           
        endif                                                                   
c
c------------------------------------------------------------------------
c modelling, for most commands, a relocation must be done first
c------------------------------------------------------------------------
c
      if(keys(1:8).eq.'HERRMANN'.or.keys(1:5).eq.'BOUCH'.or.
     *keys(1:6).eq.'BOUSEI'.or.keys(1:6).eq.'HERSEI'.or.
     *keys(1:4).eq.'WKBJ') then

c
c   first locate event, do not write out
c
         call hyp_loc(answer)
         if(answer.eq.'N') then
            keys(1:4)='SAME'
            goto 10
         endif

         if(keys(1:6).eq.'HERSEI') call systemc('hersei',6)
         if(keys(1:5).eq.'BOUCH') call systemc('bouch',5)
         if(keys(1:6).eq.'BOUSEI') call systemc('bousei',6)
         if(keys(1:8).eq.'HERRMANN') call systemc('herrman',7)
         if(keys(1:4).eq.'WKBJ') call systemc('wkbj',4)
c
c   delete event name in memory  so it is possible to use plot or other
c   external programs using event nam ein memory 
c
         call put_env_event('      ')
         keys(1:4)='SAME'
         goto 10
      endif
c
c----------------------------------------------------------------
c
c  operating system command
c-----------------------------------------------------------------
c
      if(keys(1:1).eq.'O') then
c
c   delete event name in memory so it is possible to use plot or other
c   external programs 
c
         call put_env_event('      ')
         text(1:9)=keys(2:10)
         text(10:80)=command(1:70)
         write(6,'(1x,a)') text
         call systemc(text,79)
         keys(1:4)='SAME'
         goto 10
      endif
c----------------------------------------------------------------         
c  moving single events out or between data bases                               
c---------------------------------------------------------------        
      if(keys(1:1).eq.'C') then                                                 
        write(6,*)                                                              
        write(6,*)                                                              
     *' Copy event: Other data base, give 1-5 letter name'                        
        write(6,*)                                                              
     *'             Local data base, type ,,             '                        
        write(6,*)                                                              
     *'             Working directory in file eev.out: return'                  
        read(5,'(a5)') new_base
        if(new_base(1:1).ne.' ') then     ! if blank, use eev.out
           do i=2,5
             if(new_base(i:i).eq.' ') new_base(i:i)='_'
           enddo                        
        endif                         
         chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
        if (code .ne. e_ok$) go to 129              
        goto 130                                                                
 129    continue                                                                
        write(6,*) 'No such event'                                              
        keys(1:4)='SAME'                                                        
        goto 10                                                                 
c                                                                               
 130    continue                                                                
        call indata
     *  (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)      
        call sei close (close$,write01,code)

c
c   check if id and file name ok
c
         if(id.ne.0) then
            call check_id(evfile,evid1,data1(id),nerr)
            if(nerr.ne.0) then
               write(6,*)
     *         ' Filename and ID different or event type (L,R,D)'
               write(6,*)' not the same as in file name, fix it !!!'
               write(6,*)
               write(6,'(a,a,a,a)') ' ID and type ', data1(id)(61:74),
     *         ' ',evid1
               l=seiclen(evfile)
               write(6,'(a,a)')   ' S-file name ',evfile(l-18:l)
               write(6,*)' Return to continue'
               read(5,'(a)')answer
               keys(1:4)='SAME'
               goto 10
            endif
         endif


c-- write in file                            
        if(new_base.eq.'     ') then 		
           chr_f_access$ = 'append'
              call sei open(unknown$,              ! Open a unknown status file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      'eev.out',             ! File name
     &                      write03,               ! Write unit #3
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
         write(write03,'(a80)',iostat=code) (data1(i),i=1,nrecord1)
         call sei code(fort$,code,write03,b_eof)
         call sei close (close$,write03,code)
c
c   open file with file names, index file
c
              chr_f_access$ = 'append'
              call sei open(unknown$,              ! Open a unknown status file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      'indexeev.out',        ! File name
     &                      write04,               ! Write unit #4
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
         write(write04,'(7x,a70)',iostat=code) evfile 
         call sei code(fort$,code,write04,b_eof)
         call sei close (close$,write04,code)
c
           keys(1:4)='SAME'                                                     
           goto 10                                                              
        else            
           if(base_name(1:2).eq.',,') then
              write(6,*)
     *   ' Not possible to copy to a data base from a local data base'
              keys(1:4)='SAME'
              goto 10
           endif                                                        
c                                                                               
c  copy event to other data base                   
c
           newfile=evfile                                                    
           newfile(fstart-14:fstart-10)=new_base                              
c
c  copy to local base
c
           if(new_base(1:2).eq.',,') then
              newfile=' '
              newfile=evfile(fstart:fstart+18)
           endif
c
c  get here to test
c
 1531      continue
           chr_file = newfile(1:fstart+20)
              call sei open(unknown$+warn$,       ! Open a unknown status file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
           if (code .ne. e_ok$) go to 154
           goto 155                                                          
 154       continue                                                          
           write(6,*)' No such data base'                                    
           keys(1:4)='SAME'                                                  
           goto 10                                                           
c
c   check for duplicate event in base
c
 155      continue                                                          
          if(b_old) then                               ! check if file there
 1533        continue	  
             write(6,*)
             write(6,'(a,a,a)')
     *       ' File already exists in base ',new_base,' options are:'
             write(6,'(a)')
     *       ' Do not copy type:                          Return'
             write(6,'(a)')
     *       ' Overwrite existing event:                       O'
             if(id.ne.0) write(6,'(a)')
     *       ' Create a new event in base, different ID:       N'
             read(5,'(a)') choise
             if(choise.eq.' ') then
                write(6,*)' File type will not be changed'
                keys(1:4)='SAME'
                goto 10
             endif
             if((choise.eq.'n'.or.choise.eq.'N').and.id.ne.0) then
                if(new_base(1:2).eq.',,') then
                   call inc_id(data1(id),newfile,19)
                else
                   call inc_id(data1(id),newfile,fstart+18)
                endif
                call sei close (close$,write01,code) ! close, a new will be tested
                goto 1531                            ! test again
             endif
             if(choise.eq.'o'.or.choise.eq.'O') then
                write(6,*)' Overwriting old event'
                goto 2255   ! go to write out
             endif
             goto 1533                                ! no valid choise
          endif
c
c    write out
c
 2255     continue
           write(write01,'(a80)',iostat=code) 
     *          (data1(i),i=1,nrecord1)
           call sei code(fort$,code,write01,b_eof)
           call sei close (close$,write01,code)
           write(6,'(a)')' File copied: ',newfile(1:fstart+20)
           keys(1:4)='SAME'                                                  
           goto 10                                                           
        endif                                                                   
      endif                                                                     
c
c----------------------------------------------------------
c  fault plane solution
c----------------------------------------------------------
c
      if(keys(1:2).eq.'F ') then
c
c   first locate event, do not write out
c
         call hyp_loc(answer)
         if(answer.eq.'N') then
            keys(1:4)='SAME'
            goto 10
         endif
c
c  start focmec program
c
         call systemc('focmec',6)
         write(*,*)'================================'
c
c  update data base with fault plane solution
c
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      'focmec.inp',          ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
c
            if( code .eq. e_ok$ ) then        ! Updating database.
            write(*,*)
     &'.... updating database with any saved fault plane solution'
c
            else                              ! Can't open file.
            code = e_ok$                      ! Re-set condition.
            keys(1:4)='SAME'                  ! Back to loop.
            goto 10                           !
            end if                            !
c
         call indata
     *   (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)        
         if(data1(1)(72:72).eq.'.') then
             write(6,*)' No focmec save, no solution made'
             call sei close (close$,write01,code)
             keys(1:4)='SAME'
             goto 10
         endif
         newfault=.false.
         do i=2,nhead1
           if(data1(i)(79:80).eq.' F') then
              text=data1(i)
              newfault=.true.
           endif
         enddo
         call sei close (close$,write01,code)
         if(newfault) then
            chr_file = evfile(1:fstart+20)
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
            call indata(write01,nstat1,nphase1,nhead1,
     *      nrecord1,evid1,exp1,data1,id)        
            oldfault=.false.
            do i=2,nhead1
              if(data1(i)(79:80).eq.' F') then
                 data1(i)=text
                 oldfault=.true.
                 goto 3745          ! only first valid solution updated
               endif
            enddo
 3745       continue
c
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
c
            if(oldfault) then
               write(write01,'(a80)',iostat=code) 
     *              (data1(i),i=1,nrecord1)
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 2534
c
            else
               write(write01,'(a80)',iostat=code) data1(1)
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 2534
c
               write(write01,'(a80)',iostat=code) text
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 2534
c
               write(write01,'(a80)',iostat=code) 
     *              (data1(i),i=2,nrecord1)
               call sei code(fort$,code,write01,b_eof)
            endif
c
c    Errors...
c
2534        if( code .ne. e_ok$ ) then
            chr_err_msg$ =                      ! Write error message.
     &'**** WARN: no update with fault plane solution ****'
            call sei code(fort$+warn$,code,write01,b_eof)
            end if
c
c    Close down file & back to menu...
c
         call sei close (close$,write01,code)
         endif
c
      keys(1:4)='SAME'
      goto 10
      endif
c
c----------------------------------------------------------
c  composite fault plane solution
c----------------------------------------------------------
c
      if(keys(1:2).eq.'FC') then
c
c   first locate event
c
         call systemc('hyp',3)
c
c   save focmec input data
c
         call composite_foc
         keys(1:4)='SAME'
         goto 10
      endif
c
c------------------------------------------------------------------------       
c   change event type                                                           
c------------------------------------------------------------------------       
c
      if(keys(1:1).eq.'R') then                               
         if(index_file) then
            write(6,*)
     *    ' Event type cannot be changed using an index file'
            keys(1:4)='SAME'
            goto 10
          endif
         write(6,*)                                                             
 724     continue                                                               
         write(6,*)' Change event type to L,R or D ?'                           
         read(5,'(a1)') new_type                                                
         if(new_type.eq.'l') new_type='L'                                       
         if(new_type.eq.'r') new_type='R'                                       
         if(new_type.eq.'d') new_type='D'                                       
         if(new_type.ne.'L'.and.new_type.ne.                                    
     *   'R'.and.new_type.ne.'D') then                                          
            write(6,*)' Wrong type ******'                                      
            goto 724                                                            
         endif                                                                  
c
c   open and read file
c
            chr_file = evfile(1:fstart+20)
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      read01,                ! Read unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
            if (code .ne. e_ok$) go to 1129
         goto 1130                                                              
 1129    continue                                                               
         write(6,*) 'No such event'                                             
         keys(1:4)='SAME'                                                       
         goto 10                                                                
c                                                                               
c   read file, change type and rewrite                                          
c                                                                               
 1130    continue                                                               
         call indata
     *   (read01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)
         call sei close (close$,read01,code)
c
c   check if id and file name ok
c
         if(id.ne.0) then
            call check_id(evfile,evid1,data1(id),nerr)
            if(nerr.ne.0) then
               write(6,*)
     *         ' Filename and ID different or event type (L,R,D)'
               write(6,*)' not the same as in file name, fix it !!!'
               write(6,*)
               write(6,'(a,a,a,a)') ' ID and type ', data1(id)(61:74),
     *         ' ',evid1
               l=seiclen(evfile)
               write(6,'(a,a)')   ' S-file name ',evfile(l-18:l)
               write(6,*)' Return to continue'
               read(5,'(a)')answer
               keys(1:4)='SAME'
               goto 10
            endif
         endif

c                                                                               
c   check if type is different                                                  
c                                                                               
         if(data1(1)(22:22).eq.new_type) then                                   
            write(6,*)' New type same as old, nothing changed'                  
            keys(1:4)='SAME'                                                    
            goto 10                                                             
         endif                      
c
c   make new file name
c                                            

         newfile=evfile                   
         newfile(fstart+10:fstart+10)=new_type                                  

         if (newfile(fstart+10:fstart+10).eq.' ') 
     &          newfile(fstart+10:fstart+10)=data1(1)(22:22)
c
c   check if file can be opened or if it already is there
c
 1331    continue
           call sei open(unknown$+warn$,           ! Open file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      newfile,               ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
          if (code .ne. e_ok$) then               ! check 
             write(6,'(1x,a,a)')' Cannot open file: ',newfile
             keys='SAME'
             goto 10
          endif
c
c   check for id line
c
c           if(id.eq.0) then
c              write(6,*)' No id line, cannot check for duplication'
c     *                  ' of file id'
c              goto 1155
c          endif
c
c   check for duplicate event in base
c
          if(b_old) then                               ! check if file there
 1333        continue	  
             write(6,*)
             write(6,'(a)')
     *       ' File already exists in base, options are:'
             write(6,'(a)')
     *       ' Do not change type:                        Return'
             write(6,'(a)')
     *       ' Overwrite existing event:                       O'
             if(id.ne.0) write(6,'(a)')
     *       ' Create a new event in base, different ID:       N'
             read(5,'(a)') choise
             if(choise.eq.' ') then
                write(6,*)' File type will not be changed'
                keys(1:4)='SAME'
                goto 10
             endif
             if((choise.eq.'n'.or.choise.eq.'N').and.id.ne.0) then
                call inc_id(data1(id),newfile,fstart+18)
                call sei close (close$,write01,code) ! close, a new will be tested
                goto 1331                            ! test again
             endif
             if(choise.eq.'o'.or.choise.eq.'O') then
                write(6,*)' Overwriting old event'
                goto 2155   ! go to write out
             endif
             goto 1333                                ! no valid choise
          endif
c
c    write out
c
 2155    continue             ! from just above
               data1(1)(22:22)=new_type
               write(write01,'(a80)',iostat=code)
     *              (data1(i),i=1,nrecord1)
               call sei code(fort$,code,write01,b_eof)
               call sei close (close$,write01,code)
                                                                                
c                                                                               
c   delete old event                                                            
c                                                                               
              chr_file = evfile(1:fstart+20)
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
               call sei close (delete$,write01,code)
          write(6,'(a,a)')' New file       ', newfile(1:fstart+20)
          write(6,'(a,a)')' Deleted file:  ', evfile(1:fstart+20)                 
c                                                                               
c   update event list                                                           
c                                                                               
         keys(1:3)='REN'                                                        
         CALL findevin
     *   (base_name,start_time,end_time,keys,from_eev,event_no,                          
     *   newfile,fstart,new_month,status)                                       
c
          keys(1:4)='SAME'                                                      
          goto 10                                                               
      endif                                                                     
c------------------------------------------------------------------------       
c  editor                                                                       
c-----------------------------------------------------------------------        
      if(keys(1:2).eq.'E ') then                                                 
c
c   get editor type
c
         call get_editor(eev_edit)
c        write(6,*) eev_edit
         text = eev_edit(1:sei clen(eev_edit)) // ' ' //
     *     evfile(1:fstart+20)     ! otherwise crash on Solaris lo
 358     continue
         leng = sei clen(text) ! added by LAA, 09.97
         call systemc(text,leng)                             
c
c   read file again to check errors
c
              chr_file = evfile(1:fstart+20)
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
         call indata(write01,nstat1,nphase1,nhead1,
     *   nrecord1,evid1,exp1,data1,id)
         call sei close (close$,write01,code)
c
c   check for null chars
c
         do k=1,nrecord1-1
            do i=1,80
               if(data1(k)(i:i).eq.char(0)) then
                   write(6,*) 'NULL char in line ',k,' row ',i
               endif
            enddo
         enddo
c
c   check if any id
c
         if(id.eq.0) then
            write(6,*)' No valid ID line, check!!'
            write(6,*)' Return to continue, i to ignore'
            read(5,'(a)')answer 
            if(answer(1:1).eq.'i'.or.answer(1:1).eq.'I') goto 3761
            goto 358
         endif
 3761    continue
c
c   check if id and filename are ok but only if id line is there
c
         if(id.eq.0) goto 3758
         call check_id(evfile,evid1,data1(id),nerr)
         if(nerr.ne.0) then
            write(6,*)
     *      ' Filename and ID different or event type (L,R,D)'
            write(6,*)' not the same as in file name, fix it !!!'
            write(6,*)
            write(6,'(a,a,a,a)') ' ID and type ', data1(id)(61:74),
     *      ' ',evid1
            l=seiclen(evfile)
            write(6,'(a,a)')   ' S-file name ',evfile(l-18:l) 
            write(6,*)' Return to continue, i to ignore'
            read(5,'(a)')answer
            if(answer(1:1).eq.'i'.or.answer(1:1).eq.'I') goto 3758
            goto 358
         endif
 3758    continue
c
c   check all fields in s-file
c
c   if output destination eq file (1 or 2), unit number must be changed
         call check_s(data1,                     ! file
     &                nrecord1,                  ! number of records
     &                evfile,                    ! file name
     &                nerr,                      ! number of errors
     &                0,                         ! output destination
     &                0)                         ! unit number
         if(nerr.gt.0) then
            write(6,*)' '
            write(6,*)'  Error in file, return to continue, i to ignore'
            read(5,'(a)')answer
            if(answer(1:1).eq.'i'.or.answer(1:1).eq.'I') goto 3759
            goto 358
         end if
 3759    continue
         keys(1:4)='SAME'
         goto 10
      endif                                                                     


c
c---------------------------------------------------------
c generate input values for synthetic modelling
c---------------------------------------------------------
c
c
      if(keys(1:4).eq.'SYNT') then
c
c   first locate event, do not write out
c
         call hyp_loc(answer)
         if(answer.eq.'N') then
            keys(1:4)='SAME'
            goto 10
         endif
c
              chr_file = evfile(1:fstart+20)
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
         call indata(write01,nstat1,nphase1,nhead1,
     *   nrecord1,evid1,exp1,data1,id)      
c
c   delete old references to synthetics
c
         do i=1,max_data
            do k=1,80
               data2(i)(k:k)=' '
            enddo
         enddo
         k=0
         do i=1, nrecord1
            if(data1(i)(2:6).ne.'SYNT:') then
               k=k+1
c
c  remove the station markers for synthetics
c
               if(data1(i)(1:1).eq.'s') data1(i)(1:1)=' '
               data2(k)=data1(i)
            endif
         enddo
c
c   write and read again to reinitialize counters nhead1 and nrecord1
c
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
               write(write01,'(a)',iostat=code) (data2(i),i=1,k)
               call sei code(fort$,code,write01,b_eof)
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
         call indata(write01,nstat1,nphase1,nhead1,
     *   nrecord1,evid1,exp1,data1,id)      

c
c   generate input file for synthetic programs
c
         call makesyn
c
c   put in new syntetic data from synt.inp
c
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      'synt.inp',            ! File name
     &                      read03,                ! Read unit 
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
         write(write01,'(a)',iostat=code) (data1(i),i=1,nhead1-1)
         call sei code(fort$,code,write01,b_eof)
 373     continue
         read(read03,'(a)',iostat=code) text
         call sei code(fort$,code,read03,b_eof)
         if (b_eof) go to 374
         write(write01,'(a)',iostat=code) text
         call sei code(fort$,code,write01,b_eof)
         goto 373
 374     continue
         write(write01,'(a)',iostat=code) 
     *        (data1(i),i=nhead1,nrecord1)
         call sei code(fort$,code,write01,b_eof)
         call sei close (close$,read03,code)
         call sei close (close$,write01,code)
         keys(1:4)='SAME'
         goto 10
      endif
c
c------------------------------------------------------------------------
c   find event by time association, can be a default 180 sec set in findevin
c   or a number input below
c------------------------------------------------------------------------
      if(keys(1:1).eq.'S'.and.(keys(2:2).eq.'0'.or.keys(2:2).eq.'1'.
     *  or.keys(2:2).eq.'3'.or.keys(2:2).eq.'4'.or.keys(2:2).eq.'5'.
     *  or.keys(2:2).eq.'6'.or.keys(2:2).eq.'7'.or.keys(2:2).eq.'8'.
     *  or.keys(2:2).eq.'9'.or.keys(2:2).eq.' '.or.keys(2:2).eq.'2'
     *  )) then
c
c   search for event
c
         CALL findevin
     *   (base_name,start_time,end_time,keys,from_eev,event_no,
     *   evfile,fstart,new_month,status)
         out_name=evfile(fstart:60)

c
c   if event found, display this and previous event
c
         if(status.eq.0) then
           open(1,file=evfile,status='old')
              read(1,'(a)') text
           close(1)
            write(6,*)
c
c   write nicely
c
      read(text(17:20),'(f4.1)') sec
      isec=sec
      read(text(7:8),'(i2)') i
c
c   check for magnitudes, if first not there, see if others
c
      if(text(57:64).eq.'        ') then
         if(text(65:71).ne.'       ') then
            text(57:64)=text(65:71)
         elseif(text(73:79).ne.'       ') then
            text(57:64)=text(73:79)
         endif
      endif
      write(6,289) event_no,
     *text(9:10),             ! day
     *month(i),               ! month
     *text(2:5),              ! year
     *text(12:13),            ! hour
     *text(14:15),            ! min
     *isec,                   ! sec
     *text(22:23),            ! indicators
     *text(24:30),text(31:38),text(39:45), ! hypocenter
     *text(57:64),                         ! first magnitude
     *text(49:51)                          ! number of stations
 289  format('#',i5,1x,a2,1x,a3,1x,a4,1x,a2,':',a2,1x,i2,2x,
     *a2,
     *a7,a8,a7,6x,a8,1x,a3,' Associat')   
c
c   back up one event
c
            keys(1:4)='BACK'

            CALL findevin
     *      (base_name,start_time,end_time,keys,from_eev,event_no,
     *      evfile,fstart,new_month,status)
            out_name=evfile(fstart:60)
c
c   go and ask what to do
c
             return
          endif
          keys(1:4)='SAME'
          goto 10
      endif

c
c-----------------------------------------------------------------------        
c  find existance of a waveform file                                                          
c-----------------------------------------------------------------------        
c
      if(keys(1:1).eq.'W'.and.keys(2:2).eq.' ') then                                                
c
c   read s-file
c
              chr_file = evfile(1:fstart+20)
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
         call indata(write01,nstat1,nphase1,nhead1,
     *   nrecord1,evid1,exp1,data1,id)
         call sei close (close$,write01,code)
c
c   search for wav file and check for existance
c
         k=0
         do i=2,nhead1
           if(data1(i)(80:80).eq.'6'.and.data1(i)(2:2).ne.' ') then
              k=k+1
              wave_file=' '
              wave_file(1:78)=data1(i)(2:79)
              call  get_full_wav_name(wave_file,text)
              if(text.eq.' ') then
                 write(6,'(a,a)') 
     *           ' File does not exist: ',wave_file(:seiclen(wave_file))
              else
                 write(6,'(a,a)') 
     *           ' Full path name    :  ', text(:seiclen(text))
              endif
           endif
         enddo
         if(k.eq.0) 
     *   write(6,'(a)')' No waveform file names for this event'
         keys(1:4)='SAME'
         goto 10
      endif                                                                     

c
c
c-----------------------------------------------------------------------        
c  hypocenter program                                                          
c-----------------------------------------------------------------------        
      if(keys(1:2).eq.'L ') then                                                
         call systemc('hyp',3)
c         text=data1(1)
c         text(2:5)='New '
c         write(6,'(a)') text
         keys(1:4)='SAME'
         goto 10
      endif                                                                     
c
c
c-----------------------------------------------------------------------        
c  hypo71 program
c-----------------------------------------------------------------------        
      if(keys(1:6).eq.'HYPO71') then   
         if (sun.or.linux) then
           call systemc('hypo71',6)
         elseif (pc) then
           call systemc('hypo71.exe',10)
         endif
         if (pc) then
            call systemc('more < hypo71.brief',19)
         else
            call systemc('more hypo71.brief',17)
         endif
c         text=data1(1)
c         text(2:5)='New '
c         write(6,'(a)') text
         keys(1:4)='SAME'
         goto 10
      endif                                                                     
c
c-----------------------------------------------------------------------        
c  hypo inverse program                                                         
c-----------------------------------------------------------------------        
      if(keys(1:2).eq.'H ') then                                                
         write(text,'(a,a)') 'norhin ',evfile(1:fstart+20)
         write(6,'(a)') text(1:fstart+20+7)
         call systemc(text,fstart+20+7)       ! make hypinv phase input file
         call systemc('makehin',7)            ! make hypinv parameter files
         call systemc('hypinv',6)             ! locate
         if( sun.or.linux ) then                        ! JAB(BGS)Mar95.
         call systemc('more print.out',14)    !
         else if( pc ) then                    ! JAB(BGS)Mar95.
         call systemc('more < print.out', 16) ! JAB(BGS)Mar95..results.
         end if                                !
         keys(1:4)='SAME'
         goto 10
      endif                                                                     
c
c-----------------------------------------------------------------------
c  autopick
c-----------------------------------------------------------------------
c
      if(keys(1:2).eq.'ZN') then               !JAB(BGS)Aug95.
         call systemc('neuropic',8)           !JAB(BGS)Aug95.
         keys(1:4)='SAME'                      !JAB(BGS)Aug95.
         goto 10                               !JAB(BGS)Aug95.
      else if(keys(1:2).eq.'ZT') then          !JAB(BGS)Aug95.
         call systemc('neurotrn',8)           !JAB(BGS)Aug95.
         keys(1:4)='SAME'                      !JAB(BGS)Aug95.
         goto 10                               !JAB(BGS)Aug95.
      else if(keys(1:1).eq.'Z') then           !JAB(BGS)Aug95.
         call systemc('autopic',7)
c        call systemc('dbx autopic',11)
         keys(1:4)='SAME'
         goto 10
      endif

c
c--------------------------------------------------------------------------
c autosig
c--------------------------------------------------------------------------
c
      if (keys(1:7).eq.'AUTOSIG') then

         call put_env_event(evfile)
         call sei open(old$+warn$,                  ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       evfile,                ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition
         if (code .ne. e_ok$) go to 4724

         call indata
     *      (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)

         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)

         call sei close (close$,write01,code)
c
c run autosig for all wave files
c
         do i=1,nhead1

           if (data1(i)(80:80).eq.'6') then
             text = data1(i)(2:79)
             text = 'autosig ' // text(1:seiclen(text))
             write(*,'(a)') ' starting autosig'
             write(*,'(1x,a)') text
             call put_message('autosig',text)
             call systemc('autosig',7)

           endif
         enddo
         keys(1:4)='SAME'
         goto 10
      endif


c
c--------------------------------------------------------------------------
c  plot traces with mulplt 
c--------------------------------------------------------------------------
c

      if(keys(1:1).eq.'P'.and.
     *(keys(2:2).eq.' '.or.keys(2:2).eq.'O'.or.keys(2:2).eq.'o'.or.
     * keys(2:2).eq.'W'.or.keys(2:2).eq.'H'.or.keys(2:2).eq.'B')) then

c
c check if any waveform files
c
c
c   read s-file
c
           chr_file = evfile(1:fstart+20)
           call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
           call indata(write01,nstat1,nphase1,nhead1,
     *             nrecord1,evid1,exp1,data1,id)
           call sei close (close$,write01,code)

           k=0
           do i=2,nhead1
             if(data1(i)(80:80).eq.'6'.and.data1(i)(2:2).ne.' ') then
                wave_file=' '
                wave_file(1:78)=data1(i)(2:79)
                call get_full_wav_name(wave_file,text)
                if(text.ne.' ') then
                  k=k+1
                endif
             endif
           enddo
           if(keys(2:2).ne.'O'.and.keys(2:2).ne.'o') then
             if(k.eq.0) then
               write(6,'(a)')' No waveform file names for this event'
               from_mulplt='NEXT       0'
               return
             endif
           endif

         if(keys(2:2).eq.'O'.or.keys(2:2).eq.'o') then
           mulplt_def='O'! save def value
         endif
         if(mulplt_def.ne.'O') mulplt_def=' '
         oldfile=evfile
         old_event_no = event_no
         old_fstart = fstart
         old_index_file=index_file
         text=' ' 
         text(1:1)=mulplt_def   ! see if all options
         write(text(3:8),'(i6)') event_no
         write(text(12:12),'(i1)') show_menu ! signal to mulplt if menu shown
         text(16:20)=base_name(1:5)  ! used in cont plot
         text(21:34)=start_time       ! used in cont. plot
c
c   name of synthetics if plotted
c
         if(keys(2:2).eq.'W') text(40:50)='wkbjsei.out'
         if(keys(2:2).eq.'B') text(40:49)='bousei.out'
         if(keys(2:2).eq.'H') text(40:49)='hersei.out'
c        write(6,*) 'message ',text,' ',seiclen(text)
c
c   put message to mulplt about options
c
         call put_env_string(text)
         text=' '
         call systemc('mulplt',6)
c
c  get message back from mulplt if any command to pass to eev
c
         call get_seisan_message(from_mulplt)
c         write(6,*) 'from mulplt: ', from_mulplt
c
            evfile=oldfile          ! where did it get deleted ?
            event_no = old_event_no
            fstart   = old_fstart
            index_file=old_index_file ! where did it get changed ??

         if(from_mulplt(1:4).ne.'    ') then ! a command to eev
             read(from_mulplt(12:12),'(i1)',err=4757) show_menu
             goto 4758
 4757        continue               ! error, should not happen
               mulplt_def=' '
               keys(1:4)='SAME'
               from_mulplt=' '
               goto 10
 4758        continue

c            evfile=oldfile          ! where did it get deleted ?
c            event_no = old_event_no
c            fstart   = old_fstart
c            write(*,*) evfile
            return                  ! if command, go to command in main
         else
            mulplt_def=' '
            keys(1:4)='SAME'
            goto 10
         endif
      endif

c
c--------------------------------------------------------------------------
c  plot in routine mode from data base 
c--------------------------------------------------------------------------
c
cx     if(keys(1:2).eq.'PP') then
c
c   send message to mulplt about current data base and event number
c
cx         write(text,'(i5,a40,2a14)') event_no,base_name,
cx     *   start_time,end_time
cx         text(70:70)=keys(3:3)   ! see if all options
cx         do i=1,80
cx           if(ichar(text(i:i)).eq.0) text(i:i)=' ' ! remove null chars
cx         enddo
cx         call put_env_string(text)
cx         text=' '
cx         call systemc('mulplt',6)
cx         keys(1:4)='SAME'
cx         goto 10
cx      endif

c
c--------------------------------------------------------------------------
c  SAC   
c--------------------------------------------------------------------------
c
      if(keys(1:3).eq.'SAC') then
         if(pc) then
            write(6,*)' SAC not available on PC'
            keys(1:4)='SAME'
            goto 10
         endif

         sac_text = 'extract -sfile ' //
     *      evfile(1:seiclen(evfile)) //
     *      ' -format SAC ' 
  
         call systemc(sac_text,seiclen(sac_text))
       
c
c start sac
c
         call systemc('sac',3)
         keys(1:4)='SAME'
         goto 10
      endif





c
c--------------------------------------------------------------------------
c  PITSA
c--------------------------------------------------------------------------
c
      if(keys(1:5).eq.'pitsa' .or. keys(1:5).eq.'PITSA') then
         if(pc) then
            write(6,*)' Pitsa not available on PC'
            keys(1:4)='SAME'
            goto 10
         endif

         pitsa_text = 'extract -sfile ' //
     *      evfile(1:seiclen(evfile)) //
     *      ' -format GSE -wav_out_file pitsa.gse' 

         write(6,*) ' GSE input file: pitsa.gse '

         call systemc(pitsa_text,seiclen(pitsa_text))
  
c
c start pitsa
c
         call systemc('pitsa',5)
         keys(1:4)='SAME'
         goto 10
      endif

c
c-------------------------------------------------------------------------      
c   delete event and save the deleted event in base DELET                      
c------------------------------------------------------------------------       
                                                                                
      if(keys(1:3).eq.'d  '.or.keys(1:3).eq.'D  ') then                         
c only ask if confirmation wanted lot, 17-04-2002
         if(from_mulplt(1:4).ne.'    ') then
           if (confirmation_level.eq.1.) then
             write(6,*)
     *       ' Sure you want to delete S-file ?'          
             read(5,'(a)') answer            
           else
             answer='y'
           endif
         else
           write(6,*)' Sure you want to delete event (y/n) ?'                 
           read(5,'(a)') answer            
         endif
         if(answer.eq.'y'.or.answer.eq.'Y') then
 3749    continue                              ! get to here from append                                
c                                                                               
c  copy event to DELET base before deleting if not in DELET already                   
c
c
c   open file to delete
c
           chr_file = evfile(1:fstart+20)
           call sei open(old$+warn$,              ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      read01,                ! Read unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
          if (code .ne. e_ok$) go to 2001
c
c   if in DELET data base, event to be deleted without copy !!!!!
c   if local data base, --------------------------------------
c          if(evfile(fstart-10:fstart-8).eq.'DEL'.or.
          if(base_name.eq.'DELET'.or.
     *    base_name(1:2).eq.',,') then
            write(6,*)' File deleted without backup copy'
            goto 335
          endif
c
c  read file
c
          call indata
     *    (read01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)      
c
c   only alow delete if no SEISNET request line
c
          do i=2,nrecord1
          if(data1(i)(2:4).eq.'NET'.and.data1(i)(80:80).eq.'3'.
     *          and.data1(i)(66:72).eq.'REQUEST') then
                write(6,*) 
     *          'Event not deleted since it contains a request'
                keys='SAME      '                                             
                goto 10    
             endif
          enddo                                                    
      
           newfile=evfile                                                    
           newfile(fstart-14:fstart-10)='DELET'                              
           chr_file = newfile(1:fstart+20)
 331       continue                                ! get here to test again
c
c   check if file can be opened in DEL and if it already is there
c
           call sei open(unknown$+warn$,           ! Open file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
           if (code .ne. e_ok$) go to 1154         ! check if DELET base there
c
c   check for id line
c
           if(id.eq.0) then
              write(6,*)' No id line, cannot check for duplication'
              write(6,*)' Event will be saved in DELET base with same',
     *       ' name'
              goto 1155
          endif
c
c   check for duplicate event in DELET base
c
          if(b_old) then                               ! check if file there
 333         continue	  
             write(6,*)
             write(6,'(a)')
     *       ' File already exists DELET base, options are:'
             write(6,'(a)')
     *       ' Do not save deleted event in DELET base:        Return'
             write(6,'(a)')
     *       ' Overwrite saved event in DELET base:            O'
             write(6,'(a)')
     *       ' Create a new event in DELET base, different ID: N'
             read(5,'(a)') choise
             if(choise.eq.' ') then
                write(6,*)' File will not be saved in DELET data base'
                goto 335  ! go to delete only
             endif
             if(choise.eq.'n'.or.choise.eq.'N') then
                call inc_id(data1(id),chr_file,fstart+18)
                call sei close (close$,write01,code) ! close, a new will be tested
                goto 331                             ! test again
             endif
             if(choise.eq.'o'.or.choise.eq.'O') then
                write(6,*)' Overwriting old saved event in DELET base'
                goto 1155   ! go to copy and delete
             endif
             goto 333                                ! no valid choise
          endif
c
c   error output
c
          goto 1155                                                          
 1154     continue                                                          
          write(6,*)' No such data base'                                    
          keys(1:4)='SAME'                                                  
          goto 10                                                           
c
c   copy and delete
c
 1155     continue
c
c   if here, file is read and written out in DEL data base
c                                                          
c
c  write file in DELET, header line first
c
           write(write01,'(a80)',iostat=code) 
     *     data1(1)
c
c   write who deleted event and when
c
c
c   check if current operator id given
c
 5723       continue
            if(operator.eq.'    ') then
               write(6,*) 'Give operator code, max 4 characters'
               read(5,'(a)') operator
               goto 5723
            endif
c
c   get system time
c
            call systime(p_time,proc_time)

           text=' '
           text(1:29)=' File deleted from data base '
           text(30:34)=base_name(1:5)
           text(36:37)='by'
           text(39:42)=operator
           text(44:45)='at'
           text(47:60)=proc_time
           text(80:80)='3'
           
           write(write01,'(a80)',iostat=code) 
     *     text 
           
           write(write01,'(a80)',iostat=code) 
     *     (data1(i),i=2,nrecord1)
           call sei code(fort$,code,write01,b_eof)
           call sei close (close$,write01,code)
           write(6,'(a,a)')
     *    ' Backup copy saved as: ', chr_file(1:fstart+20)                          
c
c  close and delete
c
 335        continue                                ! to here if delete
            call sei close (delete$,read01,code)
            write(6,'(a,a)')
     *      ' Deleted file         ', evfile(1:fstart+20)                              
            goto 2002                                                           
 2001       continue                                                            
            write(6,*)' No such file: ',evfile                                  
            keys='SAME'                                                         
            goto 10                                                             
 2002       continue
                                                            
c                                                                               
c   update list of files                                                       
c                                                                               
            keys(1:3)='DEL'                                                     
            CALL findevin
     *      (base_name,start_time,end_time,keys,from_eev,event_no,             
     *      evfile,fstart,new_month,status)                                     
            keys='SAME      '                                                   
c
c   if delete initiated from append, go to same event as appended to
c
            if(oldno.ne.0) then
               keys(1:1)='#'
               write(keys(2:5),'(i4)') oldno
               oldno=0
            endif
            goto 10                                                             
         endif                                                                  
         keys='SAME      '                                                      
         goto 10                                                                
      endif                                                                     
c
c----------------------------------------------------------------
c   update list of files   
c-----------------------------------------------------------------
c
      if(keys(1:1).eq.'U') then
            keys(1:3)='REN'
            CALL findevin
     *      (base_name,start_time,end_time,keys,from_eev,event_no,  
     *      evfile,fstart,new_month,status)
            keys='SAME      '
            goto 10
      endif

c                                                                               
c-------------------------------------------------------------------------                                                                               
c   another event                                                               
c-------------------------------------------------------------------------
c      
      if(keys(1:1).eq.' '.or.keys(1:1).eq.'#'.or.
     *   keys(1:1).eq.'d'.or.keys(1:1).eq.'D'.or.
     *   keys(1:1).eq.'1'.or.keys(1:1).eq.'2'.or.keys(1:1).eq.'3'.or.
     *   keys(1:1).eq.'4'.or.keys(1:1).eq.'5'.or.keys(1:1).eq.'6'.or.
     *   keys(1:1).eq.'7'.or.keys(1:1).eq.'8'.or.keys(1:1).eq.'9'.or.
     *   keys(1:1).eq.'0'.or.keys(1:4).eq.'NEXT'.or.
     *   keys(1:4).eq.'next'.or.keys(1:4).eq.'SAME'.or.
     *   keys(1:4).eq.'same') go to 10
c                                                                               
c   back space one event                                                        
c                                                                               
      if(keys(1:1).eq.'B'.or.keys(1:1).eq.'b') then                             
         keys(1:4)='BACK'                                                       
         goto 10                                                                
      endif                                                                     
c
c----------------------------------------------------------------
c   type event                                                                  
c----------------------------------------------------------------          
c
                                                                                
      if(keys(1:1).eq.'t'.or.keys(1:1).eq.'T') then                             
	     i=1
         write(6,*)
         write(6,'(1x,a,a)') 'File name: ',evfile                                               
c        write(6,*)
         chr_file = evfile(1:fstart+20)
         call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      read01,                ! Read unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
              if (code .ne. e_ok$) go to 2023
 6       continue                                                               
         read(read01,'(a)',iostat=code) text
         call sei code(fort$,code,read01,b_eof)
         if (b_eof) go to 7
         write(6,'(1x,a79)') text(2:80)                                        
         i=i+1
         if(i.gt.20) then
            write(6,*)' Return to continue, q to return to EEV'
            read(5,'(a)') text
            if(text(1:1).eq.'q'.or.text(1:1).eq.'Q') goto 7
            i=1
         endif
         if(keys(2:2).eq.'T') goto 7   ! only print header line 
         goto 6                                                                 
 7       continue                                                               
         call sei close (close$,read01,code)
         goto 2024                                                              
 2023    continue                                                               
c        write(6,*)' No such file: ',evfile                                     
 2024    continue             
         write(6,*)                                                  
         keys='SAME      '                                                      
         goto 10                                                                
      endif                                                              
c       
c------------------------------------------------------------------------
c   append an event, also in case of temporary append for location
c-------------------------------------------------------------------------
c
      loc_append=.false.            ! default is not to locate aappended events
      if(keys(1:1).eq.'L'.and.keys(2:2).ne.' ') loc_append=.true.
      if((keys(1:1).eq.'A'.or.loc_append).and.
     &   keys(1:2).ne.'AU') then
c
c   check  if a number was given
c
         if(keys(2:2).eq.' ') then
            write(6,*)' You must give a number to append from'
            keys(1:4)='SAME'
            goto 10
         endif
         oldno=event_no
         oldfile=evfile
         chr_file = evfile(1:fstart+20)
         call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      read01,                ! Read unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
         call indata
     *   (read01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)
         keys(1:1)='#'
         call sei close (close$,read01,code)
c
c   use next event if so specified without a number
c
         if(keys(1:2).eq.'#L'.or.keys(1:2).eq.'#A') keys(1:5)='NEXT '
         if(keys(1:2).eq.'#l'.or.keys(1:2).eq.'#a') keys(1:5)='NEXT '
c
c   find event to be appended
c
         CALL findevin
     *   (base_name,start_time,end_time,keys,from_eev,event_no,
     *   evfile,fstart,new_month,status)
c
c   check that you do not append to same event
c
         if(event_no.eq.oldno) then
            write(6,*)' Cannot append to same event'
            keys(1:4)='SAME'
            oldno=0
            goto 10
         endif
c
         if(status.eq.9) then
            write(6,*)' File to append not there'
            keys(1:4)='SAME'
            oldno=0
            goto 10
         endif
c
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      evfile,                ! File name
     &                      read01,                ! Read unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
          call indata
     *(read01,nstat2,nphase1,nhead2,nrecord2,evid2,exp2,data2,id)
          call sei close (close$,read01,code)
c
c   append events and write out, in temporary file if for location
c
      call merge_f(data1,data2,nhead1,nhead2,nrecord1,nrecord2)
      if(loc_append) then
        oldfile='append.out'
        evfile='append.out'
        keys(1:10)='L         '
      endif
              call sei open(unknown$,             ! Open an unknown status file.
     &                      ' ',                  ! Prompt file name (n/a).
     &                      oldfile,              ! File name
     &                      write01,              ! Write unit #1
     &                      b_old,                ! Already exists? (n/a).
     &                      code)                 ! Returned condition
         write(write01,'(a)',iostat=code)
     *        (data1(i),i=1,nrecord1)
         call sei code(fort$,code,write01,b_eof)
         call sei close (close$,write01,code)
c
c   locate if temporary append
c
      if(loc_append) then
         call put_env_event(evfile)
         write(6,422) event_no,oldno
 422     format(' Event # ',i4,
     *   ' temporarely appended to event # ',i4)
         call systemc('hyp',3)
         keys(1:4)='SAME'
         oldno=0
         goto 10
      endif
c
      write(6,222) event_no,oldno
 222  format(' Event # ',i4,' appended to event # ',i4,
     *   '   Appended event still present')
      write(6,*)' Do you want to delete appended event(y/n=return)'
      read(5,'(a)') choise
      if(choise.eq.'y'.or.choise.eq.'Y') goto 3749   ! goto delete
c
c   position at old event
c
      keys(1:1)='#'
      write(keys(2:5),'(i4)') oldno
      oldno=0
      goto 10
      endif

      write(6,*)' WRONG CHOICE, TRY AGAIN ********************'                 
c      go to 14                                                                  
      return
                                                                                
c                                                                               
c   stop                                                                        
c                                                                               
      return
      end                                                                       
                                                                                

c
c
c

c
c   Subroutines for synthetic modelling
c
c   latest update:
c
c  apr 93 by ra:  add herrmann parameters to maksyn
c  aug 94 by jh:  add wkbj
c
      subroutine makesyn
c
c   made a synthetic input modeling file from station.hyp and hyp.out
c
c
      implicit none
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Open file definitions
       include 'seidim.inc'                ! dimentions
C
       external sei open,                  ! Open file routine.
     &          sei get file,              ! Find & open file in ?directory?.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
       integer  code                       ! Returned contition.
C
C    ============= end of list ==========
C
c--- name of station-model file in current directory
      character*60 cur_file
c--- the hyp file
      character*80 data(max_data)
c--- nymber fo lines in hyp file
      integer ndata
c--- model indicator
      character*1 model
c--- model parameters
      real vp(30),vs(30),qp(30),qs(30),dens(30),depth(30),thicknes,vpvs
c--- fault plane solution
      real strike,dip,rake
c--- focal depth
      real fdepth
c--- a flag
      logical flag
c--- a control
c      logical check
c--- one line of text
      character*80 text
c--- stations to model
      character*5 stat(32)
c--- distance and different parameters
      integer ipar
      real distance
      character*1 moho(50)   ! indicates moho if 'N'
c--- counters and pointers and help variablws
      integer i,il,l,iblank,nstat,k,istat
c-- flag for changing parameters due to changing depth
      logical change_depth
      real total_time,start_time,window_time
      character*1 dchar    ! directory separation character
c-- coordinates of epicenter and stations
      real elat,elon,slat,slon
c-- distance azimuth etc
      real delta, azim,bazim,height
c logical for end of file
       logical          b_eof
c logical for existing file or not
       logical          b_old
c read unit #1 .. #6
       integer          read03, read06
c write unit #1 .. #5
       integer          write05
c
c   set computer type
c
      call dir_char(dchar)
c
c   open and read hyp.out file
c
      ndata=1
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      'hyp.out',             ! File name
     &                      read03,                ! Read unit #3
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
 10   continue
        read(read03,'(a)',iostat=code) data(ndata)
        call sei code(fort$,code,read03,b_eof)
        if (b_eof) go to 20
        ndata=ndata+1
        goto 10
  20  continue
      ndata=ndata-1
      call sei close (close$,read03,code)
c
c   open model output file
c
      call sei open(unknown$,              ! Open a unknown status file.
     &              ' ',                   ! Prompt file name (n/a).
     &              'synt.inp',            ! File name
     &              write05,               ! Write unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.
c
c   generate crustal model, first look in hyp.out, if no model there
c   use model from stationx.hyp, x can be any character
c
      flag=.false.
      do i=1,ndata
         if(data(i)(80:80).eq.'3'.and.data(i)(2:15).eq.'SYNT: MODEL--:') 
     *   then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
             flag=.true.
         endif
      enddo
c
c  if no model in hyp.out, read from station file, first look for
c  file in current directory, then in DAT
c
      if(.not.flag) then
c
c  make file name for both file in current and dat directory
c  find if alternative model is to be used
c
         cur_file(1:12)='STATION0.HYP'        ! Default.
         read(data(1),'(20x,a1)') model       ! Alternative.
         if(model.ne.' ') cur_file(8:8)=model ! Install it.
 
          call sei get file( open$,            ! Get file.
     &                      read06,           ! Opened on this unit.
     &                      code,             ! Returned condition.
     &                      'DAT',            ! Alternative directory to search.
     &                      cur_file )        ! For this station file.
 
c   read down to model, past 2 blank lines
c        
         iblank=0
 30      continue
        read(read06,'(a)',iostat=code) text
        call sei code(fort$,code,read06,b_eof)
            if(text(1:3).eq.'   ') iblank=iblank+1
            if(iblank.eq.2) goto 40
            goto 30
 40      continue
c
c  read model
c
         il=0
 50      continue
        read(read06,'(a)',iostat=code) text
        call sei code(fort$,code,read06,b_eof)
            if(text(1:3).eq.'   ') goto 60
            il=il+1
            read(text,'(2f8.2,5x,a1,3x,4f10.2)') vp(il),depth(il),
     *      moho(il),vs(il),dens(il),qp(il),qs(il)
            goto 50
 60      continue
c
c   give a thickness to last layer
c
         depth(il+1)=50.0+depth(il)
c
c   read vp/vs ratio in case it is needed
c
        read(read06,'(16x,f4.2)',iostat=code) vpvs
        call sei code(fort$,code,read06,b_eof)
c
c   close file
c
      call sei close (close$,read06,code)
c
c   generate model
c
         write(write05,200,iostat=code)
         call sei code(fort$,code,write05,b_eof)
 200   format(1x,'SYNT: MODEL--:     THICK        VP        VS',
     * '      DENS        QP        QS    3')
         do i=1,il
            if(vs(i).eq.0.0) vs(i)=vp(i)/vpvs
            if(dens(i).eq.0.0) dens(i)=2.4+i*0.2
            thicknes=depth(i+1)-depth(i)
         write(write05,201,iostat=code) thicknes,vp(i),vs(i),
     *      dens(i),qp(i),qs(i),moho(i)
         call sei code(fort$,code,write05,b_eof)
 201        format(' SYNT: MODEL--:',6f10.3,1x,a1,2x,'3')
         enddo
      endif
c
c  get fault plane solution, first look if one specified for synthetic
c  modelling, else use Nordic format type F line or else set one arbitrarily
c
      flag=.false.
      do i=1,ndata
         if(data(i)(80:80).eq.'3'.and.data(i)(2:15).eq.'SYNT: ST-D-RK:') 
     *   then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
            flag=.true.
         endif
      enddo
c
c   look for F-line, only the first is used
c
      if(.not.flag) then
         do i=1,ndata
            if(data(i)(80:80).eq.'F') then
               read(data(i),'(3f10.2)') strike,dip,rake
               goto 70
            endif
         enddo
c
c   there were no values given, make some
c
         strike=0.0
         dip=45.0
         rake=90.0
 70      continue
         write(write05,203,iostat=code) strike,dip,rake
         call sei code(fort$,code,write05,b_eof)
 203     format(' SYNT: ST-D-RK:',3f10.1,34x,'3')
      endif
c
c   get focal depth
c
      change_depth=.false.
      flag=.false.
      do i=1,ndata
         if(data(i)(80:80).eq.'3'.and.data(i)(2:15).eq.'SYNT: DEPTH--:') 
     *   then
c
c   check if update due to changing depth
c
         if(data(i)(35:35).eq.'F') then
            change_depth=.true.
            read(data(1)(39:43),'(f5.0)') fdepth
            write(data(i)(16:25),'(f10.1)') fdepth
         endif
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
            flag=.true.
         endif
      enddo
c
c   use header line depth if not given above
c
      if(.not.flag) then
         read(data(1)(39:43),'(f5.0)') fdepth
         write(write05,204,iostat=code) fdepth
         call sei code(fort$,code,write05,b_eof)
 204     format(' SYNT: DEPTH--:',f10.1,54x,'3')
      endif
c
c   number of points to model
c
         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: NPOINTS:') then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               goto 1152
            endif
         enddo
         ipar=256
         write(6,'(a,i5)')
     *   ' Number of points to model not given, will use:',ipar
         write(write05,1208,iostat=code) ipar
         call sei code(fort$,code,write05,b_eof)
 1208    format(' SYNT: NPOINTS:',i10,54x,'3')
 1152    continue
c
c   time window
c
         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: TIMES--:') then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               read(data(i)(16:25),'(15x,3(10x,f10.1))') 
     *         total_time,start_time,
     *         window_time
c
c   make sure not zero
c
               if(total_time.eq.0.0) total_time=60.0      
               if(start_time.eq.0.0) start_time=0.0      
               if(window_time.eq.0.0) window_time=60.0      
               goto 1154
            endif
         enddo
c
c   no time was given, set to 60 secs
c
         total_time=60.0
         start_time=0.0
         window_time=60.0
         write(6,'(a,f6.1)')
     *   ' No time windows given, will use:',total_time
         write(write05,1212,iostat=code) 
     *         total_time,start_time,window_time
         call sei code(fort$,code,write05,b_eof)
 1212    format(' SYNT: TIMES--:','     TOTAL',f10.3
     *   ,'   INITIAL',f10.3,'  SY-TRACE',f10.3,4x,'3')
 1154    continue
c
c   damping, bouchon
c
c         do i=1,ndata
c            if(data(i)(2:15).eq.'SYNT: DAMPING:') then
c         write(write05,'(a)',iostat=code) data(i)
c         call sei code(fort$,code,write05,b_eof)
c               goto 1155
c            endif
c         enddo
c
c   use duration as damping
c
c         write(write05,1214,iostat=code) total_time
c         call sei code(fort$,code,write05,b_eof)
c 1214    format(' SYNT: DAMPING:',f10.1,54x,'3')
c 1155    continue

c
c    bouchon parameters xl, m and eps
c
         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: BOUPAR-:') then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               goto 1156
            endif
         enddo
c
c   use defaults for BOUPAR
c
         write(write05,1216,iostat=code) 
         call sei code(fort$,code,write05,b_eof)
 1216    format(' SYNT: BOUPAR-:',
     *   '     300.0       600     0.001',34x,'3')
 1156    continue
c
c    herrmann parameters xleng and xfac
c
c         do i=1,ndata
c            if(data(i)(2:15).eq.'SYNT: HERPAR-:') then
c         write(write05,'(a)',iostat=code) data(i)
c         call sei code(fort$,code,write05,b_eof)
c               check = .true.
c            endif
c         enddo
c
c   use defaults for BOUPAR
c
c         if (.not. check) then
c         write(write05,1217,iostat=code) 
c         call sei code(fort$,code,write05,b_eof)
c 1217             format(' SYNT: HERPAR-:',
c     *   '    1000.0         4     0.001',34x,'3')
c         endif
c
c   liquid layer or not
c
c         do i=1,ndata
c            if(data(i)(2:15).eq.'SYNT: LIQUID-:') then
c         write(write05,'(a)',iostat=code) data(i)
c         call sei code(fort$,code,write05,b_eof)
c               goto 1157
c            endif
c         enddo
c
c   use no liquid layer as default
c
c         write(write05,1218,iostat=code) 
c         call sei code(fort$,code,write05,b_eof)
c 1218    format(' SYNT: LIQUID-:',
c     *   '         f',54x,'3')
c 1157    continue
c
c   wkbj parameters
c
c   phases
c
         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: PHASES-:') then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               goto 1256    ! do no change
            endif
         enddo
c
         write(write05,'(a,a,14x,a)',iostat=code) 
     *   ' SYNT: PHASES-:        Pg        Sg       PmP       SmS',
     *   '       SmP','3' 
         call sei code(fort$,code,write05,b_eof)
 1256    continue
c
c  sampling rate and source duration
c
         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: DT-Tsou:') then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               goto 1257    ! do not change
            endif
         enddo

         write(write05,'(a,44x,a)',iostat=code)
     *   ' SYNT: DT-Tsou:     0.050      .100','3'
         call sei code(fort$,code,write05,b_eof)
 1257    continue
c
c   reduction velocity
c
         do i=1,ndata
            if(data(i)(2:15).eq.' SYNT: REDVELO:') then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               goto 1258    ! do not change
            endif
         enddo

         write(write05,'(a,54x,a)',iostat=code) 
     *          ' SYNT: REDVELO:    8.0000','3'
         call sei code(fort$,code,write05,b_eof)
 1258    continue
c
c  component 
c
         do i=1,ndata
            if(data(i)(2:15).eq.' SYNT: COMPON-:') then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               goto 1259    ! do not change
            endif
         enddo

         write(write05,'(a,54x,a)',iostat=code) 
     *          ' SYNT: COMPON-:    RADIAL','3'
         call sei code(fort$,code,write05,b_eof)
 1259    continue
c
c  free surface 
c
         do i=1,ndata
            if(data(i)(2:15).eq.' SYNT: STAT-AT:') then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               goto 1260    ! do not change
            endif
         enddo

         write(write05,'(a,47x,a)',iostat=code) 
     *          ' SYNT: STAT-AT:  no             ','3'
         call sei code(fort$,code,write05,b_eof)
 1260    continue

c
c   find number of stations and stations to model
c
      nstat=0
      do i=1,ndata
         if((data(i)(80:80).eq.'3'.and.data(i)(2:15).eq.
     *        'SYNT: STATION:').or.data(i)(1:1).eq.'s') 
     *   then
            do k=1,nstat
              if(stat(k).eq.data(i)(17:21)) goto 80
              if(stat(k).eq.data(i)(2:6)) goto 80
            enddo
c
c   station was not counted before
c
            nstat=nstat+1
            if(data(i)(1:1).eq.'s') then
               stat(nstat)=data(i)(2:6)
            else
               stat(nstat)=data(i)(17:21)
            endif 
 80         continue
         endif
      enddo
      write(6,*)' Number of stations to model', nstat
         write(write05,205,iostat=code) nstat
         call sei code(fort$,code,write05,b_eof)
 205  format(' SYNT: NSTAT--:',i10,54('-'),'3')
c
c----------------------------------------------------------------------------
c   enter loop over stations
c----------------------------------------------------------------------------
c
      do istat=1,nstat
         write(write05,99,iostat=code) 
         call sei code(fort$,code,write05,b_eof)
 99      format(' SYNT: NEW STAT:',63('-'),'3')
c
c   find distance for stations, either given by SYNT, or by hyp
c
         do i=1,ndata
            if(stat(istat)(1:5).eq.data(i)(17:21).and.
     *      data(i)(28:35).eq.'DISTANC:'.and.data(i)(80:80).eq.'3') 
     *      then
c
c   check for an update due to changing depth
c
               if(change_depth) then
                  do l=1,ndata
                     if(stat(istat)(1:5).eq.data(l)(2:6)) then
                        read(data(l)(71:75),'(f5.0)') distance
                        write(data(i)(36:45),'(f10.1)') distance
                        goto 163
                     endif
                  enddo
                endif
 163         continue
             write(write05,'(a)',iostat=code) data(i)
             call sei code(fort$,code,write05,b_eof)
             goto 150
            endif
         enddo
         do i=1,ndata
            if(stat(istat)(1:5).eq.data(i)(2:6)) then
               read(data(i)(71:75),'(f5.0)') distance
         write(write05,206,iostat=code) 
     *         stat(istat),distance
         call sei code(fort$,code,write05,b_eof)
 206           format
     *         (' SYNT: STATION: ',a5,'S  Z'
     *         ,'  DISTANC:',f10.1,34x,'3')
               goto 150
            endif
         enddo
         write(6,'(a,a)')' No distance for station',stat(istat)
 150     continue
c  
c   get azimuth
c
         do i=1,ndata
            if(stat(istat)(1:5).eq.data(i)(17:21).and.
     *      data(i)(28:35).eq.'AZIMUTH:'.and.data(i)(80:80).eq.'3') 
     *      then
c
c   check for an update due to changing depth
c
               if(change_depth) then
                  do l=1,ndata
                     if(stat(istat)(1:5).eq.data(l)(2:6)) then
                        read(data(l)(77:79),'(i3)') ipar
c
c   calculate backaz
c
                        read(data(1)(24:38),'(f7.3,f8.3)') elat,elon  ! epicente
                        call stat_loc(stat(istat),data(1)(21:21),
     *                  slat,slon,height)    ! station lat and lon
                        if(slat.eq.0.0.and.slon.eq.0.0.and.
     *                  height.eq.0.0) then
                           write(6,'(a,a,a)')' Station ',stat(istat),
     *                     ' not in station file'
                           write(6,*)
     *                     ' Will use azimut+180 for back azimuth'
                           bazim=ipar+180
                           if(bazim.gt.360) bazim=bazim-360.0
                           else
                           call azibazi(elat,elon,slat,slon,
     *                     delta,azim,bazim) ! calculate baz
                        endif
                        write(data(i)(36:45),'(f10.2)') float(ipar)
                        write(data(i)(56:65),'(f10.2)') bazim
                        goto 164
                     endif
                  enddo
                endif
 164     continue

         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               goto 151
            endif
         enddo
         do i=1,ndata
            if(stat(istat)(1:5).eq.data(i)(2:6)) then
               read(data(i)(77:79),'(i3)') ipar
c
c   calculate backaz
c
         read(data(1)(24:38),'(f7.3,f8.3)') elat,elon  ! epicenter lat and lon
         call stat_loc(stat(istat),data(1)(21:21),slat,slon,height)    ! station lat and lon
c         call stat_loc(stat(istat),slat,slon,height)    ! station lat and lon
         if(slat.eq.0.0.and.slon.eq.0.0.and.height.eq.0.0) then
            write(6,'(a,a,a)')' Station ',stat(istat),
     *      ' not in station file'
            write(6,*)' Will use azimut+180 for back azimuth'
            bazim=ipar+180
            if(bazim.gt.360) bazim=bazim-360.0
         else
            call azibazi(elat,elon,slat,slon,delta,azim,bazim) ! calculate baz
         endif
         write(write05,207,iostat=code) stat(istat),float(ipar),
     *   bazim 
         call sei code(fort$,code,write05,b_eof)
 207           format
     *         (' SYNT: STATION: ',a5,4x,'  AZIMUTH:',
     *          f10.1,' BAZIMUTH:',f10.1,14x,'3')
               goto 151
            endif
         enddo
         write(6,'(a,a)')' No azimuth for station',stat(istat)
 151     continue



      enddo
c     call sei close (close$,read03,code)
      call sei close (close$,write05,code)
      return
      end

      subroutine composite_foc
c
c  save data for a composite fault plane solution in one file
c  focmec.inp
C
c
      implicit none
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! File operations.
       include 'seidim.inc'                ! dimentions
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C
c--data array for one event
      CHARACTER*80 DATA(max_data)		
C  NUMBER OF DIFFERENT STATIONS IN DATA
      INTEGER NSTAT			
C  NUMBER OF HEADERS AND RECORDS IN DATA
      INTEGER NHEAD,NRECORD		
C  ANGLE OF INCIDENCE
      REAL ANGINC			
      integer iang
C  NUMBER OF DIFFERENT PHASE FOR EVENT
      INTEGER NPHASE			
      INTEGER I,id			
c---event type etc
      character*1 type,exp
c-- one line in print file
      character*80 oneline         	
c logical for end of file
       logical          b_eof
c logical for existing file or not
       logical          b_old
c returned code
       integer          code
c read unit #1 .. #6
       integer          read04, read05
c write unit #1 .. #5
       integer          write04
c
c   open print file
c
             call sei open(old$+warn$,            ! Open a existing file.
     &                     ' ',                   ! Prompt file name (n/a).
     &                     'print.out',           ! File name
     &                     read05,                ! Read unit #5
     &                     b_old,                 ! Already exists? (n/a).
     &                     code)                  ! Returned condition.
c
c   open and read hyp.out file
c
             call sei open(old$+warn$,            ! Open a existing file.
     &                     ' ',                   ! Prompt file name (n/a).
     &                     'hyp.out',             ! File name
     &                     read04,                ! Read unit #4
     &                     b_old,                 ! Already exists? (n/a).
     &                     code)                  ! Returned condition.
      call INDATA(read04,nstat,nphase,NHEAD,NRECORD,TYPE,EXP,DATA,id)
      call sei close (close$,read04,code)
c
c  read forward to station lines in print file
c
 22   continue
            read(read05,'(a80)',iostat=code) oneline
            call sei code(fort$,code,read05,b_eof)
            if (b_eof) go to 99
      if(oneline(2:4).eq.'stn') go to 3
      go to 22     
c
c  station line found
c
 3    continue
c
c  updating starts at first phase line
c
      i=nhead			
 4    continue
            read(read05,'(a80)',iostat=code) oneline
            call sei code(fort$,code,read05,b_eof)
c
c   check for end of phases
c	  
      if(oneline(1:13).eq.'          ') go to 99 
c	  
c   check if a station line
c
         if(oneline(1:4).ne.'    ') then  
c
c   check if normal phase line or az line
c
            if(oneline(26:27).ne.'AZ') then
               i=i+1
               READ(oneline,101) anginc
101            FORMAT(17x,f6.1)              ! was 18x, feb 97
               data(i)(58:60)='   '	
               iang=anginc+0.5
               WRITE(DATA(I)(58:60),'(I3)')iang
            endif 
         endif
c
c   back for next phase
c
      goto 4
c
c
c   end of file
c
 99   continue
      call sei close (close$,read05,code)
c
c   open and write out focmec file
c
           chr_f_access$ = 'append'
              call sei open(old$+ignore$,      ! Open a existing file.
     &                      ' ',               ! Prompt file name (n/a).
     &                      'focmec.inp',      ! File name
     &                      write04,           ! Write unit #4
     &                      b_old,             ! Already exists? (n/a).
     &                      code)              ! Returned condition.
              if (code .ne. e_ok$) go to 200

      write(6,*)' You are appending to an existing focmec.inp file'
      goto 210
 200  continue  
c
c  must be a new file, open as new and write whole event
c  only write out P-phases
c
              call sei open(new$+warn$,        ! Open a new file.
     &                      ' ',               ! Prompt file name (n/a).
     &                      'focmec.inp',      ! File name
     &                      write04,           ! Write unit #4
     &                      b_old,             ! Already exists? (n/a).
     &                      code)              ! Returned condition.
         write(write04,'(a80)',iostat=code) 
     *        (data(i),i=1,nhead)
         do i=nhead+1,nrecord-1
            if(data(i)(11:11).eq.'p'.or.data(i)(11:11).eq.'P')
     *      write(write04,'(a80)',iostat=code) 
     *      data(i)
         enddo
         call sei code(fort$,code,write04,b_eof)
         call sei close (close$,write04,code)
      return                                   ! return added  feb 97
c
c   already some data, only write phase lines
c
 210  continue
         do i=nhead+1,nrecord-1
            if(data(i)(11:11).eq.'p'.or.data(i)(11:11).eq.'P')
     *      write(write04,'(a80)',iostat=code) 
     *      data(i)
         enddo
         call sei code(fort$,code,write04,b_eof)
         call sei close (close$,write04,code)
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine check_s_file(s_file,data)
c
c   checks the s-file for errors, if err=0, no errors
c   file name is s_file, and data is in data, only there to not redefine
c   so space is saved
c
c   variables as in eev
c
      implicit none
      include 'libsei.inc'
      integer unit,code,nstat,nphase,nhead,nrecord,id,err
      character*80 data(*)
      character*80 s_file
      logical b_old
      character*1 evid,exp

              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      s_file  ,              ! File name
     &                      unit   ,               ! Write unit 
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition

         call indata(unit,nstat,nphase,nhead,
     *   nrecord,evid,exp,data,id)
         call sei close (close$,unit,code)
c
c   check all fields in s-file
c
c   if output destination eq file (1 or 2), unit number must be changed
         call check_s(data,                      ! file
     &                nrecord,                   ! number of records
     &                s_file,                    ! file name
     &                err,                       ! number of errors
     &                0,                         ! output destination
     &                0)                         ! unit number
c
      return
      end

      subroutine read_s_file_e(sfile)

c
c   open and read s-file, close
c
c   see main program for variables
c
      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
      character*80 sfile
      integer read_unit
      logical b_flag
      integer code,nstat,nphase,id,nrecord,nhead
      character*80 data(max_data)
      character*1 exp,type
      common /eev/ data,nstat,nphase,nrecord,nhead,id,exp,type
      
              call sei open( old$,                 ! Open old file.
     &                  ' ',                  ! No prompt.
     &                  sfile,                ! This file.
     &                  read_unit,            ! On this unit.
     &                  b_flag,               ! Existance?.
     &                  code )                ! Condition (n/a).
             call indata
     *       (read_unit,nstat,nphase,nhead,nrecord,type,exp,data,id)
             call sei close(close$,read_unit,code)    ! Close (stop on error).
       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine write_s_file_e(sfile)

c
c   open and write s-file, close 
c
c   see main program for variables
c
      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
      character*80 sfile
      integer write_unit
      logical b_flag
      integer code,i,nrecord,nhead,nphase,nstat,id
      character*80 data(max_data)
      character*1 exp,type
      common /eev/ data,nstat,nphase,nrecord,nhead,id,exp,type
      
              call sei open( old$,                 ! Open old file.
     &                  ' ',                  ! No prompt.
     &                  sfile,                ! This file.
     &                  write_unit,           ! On this unit.
     &                  b_flag,               ! Existance?.
     &                  code )                ! Condition (n/a).
              write(write_unit,'(a80)')(data(i),i=1,nrecord)
             call sei close(close$,write_unit,code)    ! Close (stop on error).
       return
       end

       subroutine id_status(unit,ids)
c
c   find id line in file opened at unit unit and return status in ids
c
       implicit none
       integer unit,i
       character*3 ids
       character*80 text
c
       ids='   '
       do i=1,10000
         read(unit,'(a)',end=1) text
         if(text(80:80).eq.'I') then
            ids=text(9:11)
            goto 1
         endif
       enddo
c
 1     continue
       return
       end

       subroutine hyp_loc(status)
c
c   sennds message to hyp for no write out on screen. locates event
c   and ask for a return to continue
c   if status is N, operation might be stopped
c
      implicit none
      character*200 text
      character*1 status
c
c   set text for no write out for hyp for some operations
c
      text='hyp non interactive'
c
      write(6,*)
      write(6,*)' **** now locating with hyp as a preparation ***'
      write(6,*)

      call put_seisan_message(text)
      call systemc('hyp',3)            ! locate
c
      write(6,*)' If location not ok, result might be unpredictable'
      write(6,*)' Return to continue (y=return/N)'
      read(5,'(a1)') status
      if(status.eq.'n') status='N'
      return
      end

      subroutine get_operator(operator,linux)
      implicit none
      character*(*) operator
      logical linux
    
      if (linux) call get_env_op(operator)
1     continue
      if(operator.eq.'    ') then
         write(6,*) 'Give operator code, max 4 characters'
         read(5,'(a)') operator
         goto 1
      endif
      if (linux) call put_env_op(operator)

      return
      end


      subroutine write_idline(year,month,day,hr,min,isec,
     &   operator,regcode,idline)
c
c create id line
c
      implicit none
      integer YEAR,MONTH,DAY,HR,MIN,ISEC,i
      character*80 idline
      character*12 p_time
      character*14 proc_time
      character*3 regcode
      character*2 operator

      idline(1:40)= ' ACTION:                   OP:     STATU'
      idline(41:80)='S:               ID:                   I'
      WRITE(IDLINE(61:75),'(i4,5I2)')
     *YEAR,MONTH,DAY,HR,MIN,ISEC
      DO I=61,74
         IF(IDLINE(I:I).EQ.' ') IDLINE(I:I)='0'
      ENDDO
      call systime(p_time,proc_time)
      WRITE(IDLINE(31:34),'(A)')OPERATOR
      WRITE(IDLINE(13:26),'(A)')PROC_TIME
      WRITE(IDLINE(9:11),'(A)')REGCODE

      return
      end
