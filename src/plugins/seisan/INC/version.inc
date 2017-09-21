
      character*10 version_text
      character*20 version_date,out_version_date
      logical version_new

      parameter(version_text='SEISAN 8.0')
      parameter(version_date='September 4, 2003')
c
c   version_new : true  : always take date from include file
c                 false : take date from program
c
      parameter(version_new=.true.)

      common /verblock/out_version_date

