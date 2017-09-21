c  computer dependent subroutines including dummy routines to be
c  able to use same programs on pc and sun
c
c    ***** this file is for linux ******
c
c
c  Updates:
c
c--------------------------------------------------------------------
c  nov5 by jh       version 7.0 check
c

      subroutine computer_type(sun,pc,linux)
c   which computer
      implicit none
      logical pc,sun,linux
      pc=.false.
      sun=.false.
      linux=.true.
      return
      end

      integer function JIAND(i1,i2)
      implicit none
      integer i1,i2
      jiand=iand(i1,i2)
      return
      end 

      subroutine iasp91_filename(name)
c set name of iasp91 files
      character*(*) name
      name='IASP91_linux'
      return
      end
