      subroutine auto_tr(data,nhead,nfile,filename)
c
c   routine to get trace file names for nordic file
c
c   updates
c
c   feb 99 by jh :---------------  verison 7.0 check ------------------
c                 no changes
c
c   input:
c   data: nordic S-file
c   nhead: number of headers in file
c   output:
c   nfile: number of waveform files
c   filename: waveform files
c
      implicit none
      character*80 data(*)
ccc      character*80 filename(*)
      character*(*) filename(*)
      integer nhead,nfile
c
c-- help variables
c
      integer i
      nfile=0
      do i=2,nhead
         if(data(i)(80:80).eq.'6') then
            nfile=nfile+1
            filename(nfile)(1:78)=data(i)(2:79)
            filename(nfile)(79:80)=' '
          endif
      enddo
      return
      end
