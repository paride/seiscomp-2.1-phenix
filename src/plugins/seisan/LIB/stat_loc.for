      subroutine stat_loc(station,file_ind,slat,slon,elev)                            
c                                                                               
c     Routine to return coordinates of a given station in                       
c     degrees (and decimaldegrees) and elevation                                
c                                                                               
c    written by C. Lindholm May 1990                                            
c
c    updates:
c    mar 23,  92 by j.h. : unix adoption
c    Aug. 20 -93 c.l. topdirectory til charachter*60
c    Jan 95      j.h : ********** version 5.0 *******************
c    Feb 2           : bug
c    sep 16 98 jh    : ----------- version 7.0 check ------------
c    oct               changed for 5 char station
c    feb  2 99 lo    : bug fixed
c    feb 19 99 lo    : elev changed to f4.0
c    oct  16   jh    : posibility of reading coordinates with one more digit
      implicit none 
      include 'libsei.inc'
c-- latitude and longitude (output)                        
      real slat,slon			
c-- elevation (output)                                         
      real elev				
c-- station code (input)                              
      character*5 station 		
c-- station file indicator x in STATIONx.HYP
      character*1 file_ind
c-- string with data read from stationfile             
      character*80 string		
c-- station file name
      character*80 stat_file
c-- internal var.                       
      integer lat,lon
      real dlat,dlon
c-- unit and error code
      integer unit,code
c-- Direction indicators                               
      character*1 ns,ew				
                                     
      slat=0.
      slon=0.
      elev=0.
      stat_file(1:12)='STATION0.HYP'
      if(file_ind.ne.' ') stat_file(8:8)=file_ind ! check if alternative file
c
c  Search  file in current, then in DAT 
c
           call sei get file( open$+ignore$,   ! Check for  file.
     &                        unit,            ! Unit (n/a).
     &                        code,            ! Returned condition.
     &                        'DAT',           ! Alternative search directory.
     &                   stat_file )           ! For this filename.

                                                                                
c                                                                               
c---- read until station is found---                                            
c                                                                               
1     read(unit,'(a)',end=999)string
      if(station(5:5).eq.' ') then  ! 4 char station                                              
         if(string(3:6) .ne. station(1:4)) go to 1
      else                          ! 5 char station
         if(string(2:6) .ne. station(1:5)) go to 1
      endif                                              
c
c-------- station found ---
c
      read(string,10) lat,dlat,ns,lon,dlon,ew,elev                    
10    format(6x,i2,f5.3,a1,i3,f5.3,a1,f4.0)      
      slat = lat + dlat/60.  
      if(ns .eq. 'S') slat = -slat                                              
      slon = lon + dlon/60.                     
      if(ew .eq. 'W') slon = -slon                                              
      goto 20                                                                                
999   continue
      slat = 0.0
      slon = 0.0
      elev = 0.0
 20   continue                                                                
      call sei close(close$,unit,code)
      return
      end                                                                       
