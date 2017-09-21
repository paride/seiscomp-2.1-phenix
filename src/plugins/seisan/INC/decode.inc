c    for mini seed
c
      integer stdin, stdout
      integer blksize,steim1,steim2
      parameter (stdin=5, stdout=6)
      parameter (blksize = 4096)
      real iblk
      parameter (iblk = blksize / 4)
      integer lastvalue
      parameter (lastvalue = 1)
      integer initial
      parameter (initial = 2 )
      integer ifileopen
      parameter (ifileopen = 3)
      integer icurrlevel
      parameter (icurrlevel = 4)
      integer iprevlevel
      parameter (iprevlevel = 5)
      integer iseedname
      parameter (iseedname = 6)
      integer maxseedchannels
      parameter (maxseedchannels = 100)
      parameter (steim1 = 10)
      parameter (steim2 = 11)
      integer iexpansion
      parameter (iexpansion = 1)
      integer isamples
      parameter (isamples = 2)
      integer imissing
      parameter (imissing = 4)
      integer inaccurate
      parameter (inaccurate = 8)
      integer igap
      parameter (igap = 16)
      integer ibeginevent
      parameter (ibeginevent = 32)
