
      subroutine merge_f(data1,data2,nhead1,nhead2,nrecord1,nrecord2)
c	Subroutine to merge two S files. All headers are kept.
c
c	Written by C. Lindholm, Aug. -90
c
c
c       Updates:
c              Nov. 25 ; C.L.   ; Change from next line to this line indicator
c              nov 25    j.h.   : put a 1 in header2
c              jul 8 93  jh     : version 3.0 *****************************
c              aug 98    jh     : --------verison 7.0 check ----------------
c                                 include seidim.inc for data array
c              may 2000         : check for different days
c
c	Input:
c		data1 : Vector of character strings containing the first s file
c		data2 : Vector of character strings containing the last s file
c               nhead1: Number of headers in first file
c               nhead2: Number of headers in second file
c               nrecord1: Number of records in first file
c               nrecord2: Number of records in second file
c	Output:
c		data1 : Vector of character strings containing the output file
c               nhead1: Number of headers in output file
c               nrecord1: Number of records in output file
c
c
      implicit none
      include 'seidim.inc'

      character*80 data1(*),data2(*)
c-- number of headers
      integer nhead1,nhead2
c-- number of records
      integer nrecord1,nrecord2
c-- time of two headers
      integer year1,month1,day1,hour1,min1
      real sec1
      integer year2,month2,day2,hour2,min2
      real sec2
      double precision time1,time2
      real timedif
      logical add_day         ! true if a day has to be added
c-- Array used in this routine only
      character*80 local(max_data)
c-- Local variables
      integer nrecord,nhead
c-- Explanatory header
      character*80 explanatory
c-- Counters
      integer i,k
c
c------ Initialize -------
c
      k = 0
      explanatory = ' '
      add_day=.false.
c
c   change id line of 2. file to a comment line
c
      do i=1,nhead2
         if(data2(i)(80:80).eq.'I') data2(i)(80:80)='3'
      enddo
c
c   make sure second header has a 1 if not originally there
c
      data2(1)(80:80)='1'	  
c
c   check if files are from same day
c
      read(data1(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)') 
     *year1,month1,day1,hour1,min1,sec1
      read(data2(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)') 
     *year2,month2,day2,hour1,min1,sec1
      call timsec(year1,month1,day1,hour1,min1,sec1,time1)
      call timsec(year2,month2,day2,hour2,min2,sec2,time2)
      timedif=time2-time1
c
c   first check if files more 24 hours apart
c
      if(abs(timedif).ge.24*3600.0) then
          write(6,*)
     *    ' You cannot merge 2 S-files more than 24 hours apart'
          stop
      endif
c
c   if files not on same day, it must start with earliest file
c
      if(day1.ne.day2) then
         if(time1.gt.time2) then
            write(6,*)' S-files on different days, you must',
     *      ' start with earliest file'
            stop
         else 
            add_day=.true.
         endif
      endif
c
c----- first transfer data1 into local variables -----
c

      nrecord = nrecord1
      nhead = nhead1

      do 10 i=1,nrecord
         local(i) = data1(i)
10    continue
c
c------ then transfer the headers  --------
c
      do 20 i = 1,nhead
         k = k + 1
         data1(k) = local(i)
         if(local(i)(80:80) .eq. '7') then
            explanatory = local(i)
            k = k - 1
            go to 100
         endif
20    continue
100   continue

      do 30 i = 1,nhead2
         k = k + 1
         data1(k) = data2(i)
         if(data2(i)(80:80) .eq. '7') then
            explanatory = data2(i)
            k = k - 1
            go to 200
         endif
30    continue
200   continue
      if(explanatory .ne. ' ') then
         k = k + 1
         data1(k) = explanatory
      endif

      nhead1 = k
c
c------ now transfer phase readings
c
      do 40 i = nhead+1,nrecord-1
         k = k + 1
         data1(k) = local(i)
40    continue
      do 50 i = nhead2+1,nrecord2
         k = k + 1
c
c   check if a day change in case 24 hr might have to be added
c
         if(add_day.and.data2(i).ne.' ') then
            read(data2(i)(19:20),'(i2)') hour2
            if(hour2.le.24) then   ! hour could be from next day already
               hour2=hour2+24
            endif
            write(data2(i)(19:20),'(i2)') hour2
         endif   
         data1(k) = data2(i)
50    continue

      nrecord1 = k

      return
      end

