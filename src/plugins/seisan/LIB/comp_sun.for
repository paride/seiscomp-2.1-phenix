c   computer dependent subroutines including dummy routines to be
c  able to use same programs on pc, linux  and sun
c-----------------------------------------------------------------
c  5 11 98    version 7.0 check
c----------------------------------------------------------------
c
c    ***** this file is for sun ******
c
c
c  Updates:
c

      subroutine computer_type(sun,pc,linux)
c   which computer
      implicit none
      logical pc,sun,linux
      pc=.false.
      sun=.true.
      linux=.false.
      return
      end

      subroutine iasp91_filename(name)
c set name of iasp91 files
      character*(*) name
      name='IASP91_sun'
      return
      end

