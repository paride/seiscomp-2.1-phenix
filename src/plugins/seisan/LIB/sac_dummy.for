
c
c SAC dummy routines
c
      subroutine rsac1(a,b,c,d,e,f,nerr)
      character*1 a
      real b,d,e,f
      integer c,nerr
      nerr=-1
      return
      end

      subroutine wsac0(a,b,c,nerr)
      character*1 a
      real b,c
      integer nerr
      nerr=-1
      write(*,*) ' this program only works if you compile with '
     *    // 'the SAC libraries '
      return
      end

      subroutine getnhv(a,b,nerr)
      character*1 a
      integer b,nerr
      nerr=-1
      return
      end

      subroutine getfhv(a,b,nerr)
      character*1 a
      real b
      integer nerr
      nerr=-1
      return
      end

      subroutine getkhv(a,b,nerr)
      character*1 a,b
      integer nerr
      nerr=-1
      return
      end

      subroutine setfhv(a,b,nerr)
      character*1 a
      real b
      integer nerr
      nerr=-1
      return
      end

      subroutine setihv(a,b,nerr)
      character*1 a,b
      integer nerr
      nerr=-1
      return
      end

      subroutine setkhv(a,b,nerr)
      character*1 a,b
      integer nerr
      nerr=-1
      return
      end

      subroutine setlhv(a,b,nerr)
      character*1 a
      logical b
      integer nerr
      nerr=-1
      return
      end

      subroutine setnhv(a,b,nerr)
      character*1 a
      integer b,nerr
      nerr=-1
      return
      end

      subroutine newhdr
      return
      end


