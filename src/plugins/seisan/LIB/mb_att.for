      subroutine mb_att(depth,dist,qout)
c
c   interpolates in the mb attenuation curve, depth is hypocentral
c   depth in km and dist is epicentral distance in degrees, qout is the 
c   corresponfing attenation
c   jh, june 94
c
      implicit none
      real dist,depth,qout
c
c   On the SUN, there are a maximum number of continuation cards
c   (about 17 ????), thus its is impossible to set data "x" without
c   compilation errors...divide up into xx1, xx2...& equivalence..
c   Note that mod(1111,96) has a remainder...pad out the last array
c   with zeroes in the data statement...
c
      real x(1111)         ! help variable
      real xx1(96), xx2(96), xx3(96), xx4(96), xx5(96),
     &     xx6(96), xx7(96), xx8(96), xx9(96), xxa(96),
     &     xxb(96), xxc(96)
      equivalence (x(1),   xx1),
     &            (x(97),  xx2),
     &            (x(193), xx3),
     &            (x(289), xx4),
     &            (x(385), xx5),
     &            (x(481), xx6),
     &            (x(577), xx7),
     &            (x(673), xx8),
     &            (x(769), xx9),
     &            (x(865), xxa),
     &            (x(961), xxb),
     &            (x(1057),xxc) 
c
      real q(101,11)       ! the attenuation values
      real h(11)           ! depths for attenuation values
      integer idp          ! depth index
      integer idist        ! distance index
      real    qlow,qhigh   ! interpolation values
      integer i
      equivalence(x,q)
c
      data h/0.0,15.0,40.0,100.0,200.0,300.0,400.0,500.0,600.0,
     *700.0,800.0/
      data xx1/
     *  .301, 1.191, 2.371, 2.501, 2.851, 3.061, 3.201, 3.321,
     * 3.401, 3.451, 3.491, 3.531, 3.551, 3.561, 3.561, 3.551,
     * 3.511, 3.401, 3.281, 3.091, 3.071, 3.101, 3.151, 3.241,
     * 3.341, 3.451, 3.551, 3.651, 3.721, 3.741, 3.721, 3.681,
     * 3.661, 3.661, 3.651, 3.641, 3.641, 3.641, 3.631, 3.631,
     * 3.621, 3.621, 3.621, 3.631, 3.631, 3.641, 3.641, 3.651,
     * 3.661, 3.661, 3.671, 3.671, 3.681, 3.691, 3.691, 3.701,
     * 3.701, 3.711, 3.721, 3.721, 3.731, 3.741, 3.741, 3.751,
     * 3.751, 3.761, 3.761, 3.771, 3.781, 3.781, 3.791, 3.801,
     * 3.801, 3.811, 3.811, 3.821, 3.831, 3.831, 3.841, 3.841,
     * 3.851, 3.861, 3.871, 3.881, 3.891, 3.811, 3.941, 3.961,
     * 3.981, 4.021, 4.061, 4.101, 4.151, 4.211, 4.281, 4.361/
      data xx2/
     * 4.441, 4.521, 4.601, 4.681, 4.761,-1.519, 3.221, 2.321,
     * 2.531, 2.831, 3.031, 3.161, 3.271, 3.341, 3.391, 3.431,
     * 3.461, 3.481, 3.491, 3.491, 3.471, 3.411, 3.301, 3.151,
     * 3.011, 3.011, 3.051, 3.121, 3.191, 3.291, 3.391, 3.491,
     * 3.571, 3.631, 3.651, 3.631, 3.611, 3.591, 3.581, 3.571,
     * 3.561, 3.551, 3.551, 3.541, 3.541, 3.541, 3.541, 3.541,
     * 3.541, 3.541, 3.551, 3.551, 3.561, 3.561, 3.571, 3.581,
     * 3.581, 3.591, 3.601, 3.601, 3.611, 3.621, 3.621, 3.631,
     * 3.641, 3.641, 3.651, 3.651, 3.661, 3.661, 3.671, 3.681,
     * 3.681, 3.691, 3.691, 3.701, 3.701, 3.711, 3.721, 3.721,
     * 3.731, 3.731, 3.741, 3.741, 3.751, 3.751, 3.761, 3.761,
     * 3.771, 3.791, 3.811, 3.831, 3.851, 3.881, 3.921, 3.961/
      data xx3/
     * 4.001, 4.051, 4.111, 4.191, 4.271, 4.351, 4.431, 4.511,
     * 4.591, 4.671,-1.139, 2.461, 2.591, 2.831, 3.021, 3.171,
     * 3.291, 3.381, 3.431, 3.471, 3.491, 3.501, 3.511, 3.511,
     * 3.511, 3.471, 3.381, 3.241, 3.051, 2.931, 2.921, 2.951,
     * 3.011, 3.091, 3.171, 3.271, 3.361, 3.431, 3.481, 3.491,
     * 3.501, 3.501, 3.491, 3.481, 3.461, 3.451, 3.441, 3.431,
     * 3.421, 3.421, 3.411, 3.411, 3.411, 3.411, 3.411, 3.411,
     * 3.421, 3.421, 3.431, 3.441, 3.441, 3.451, 3.461, 3.461,
     * 3.471, 3.481, 3.481, 3.491, 3.491, 3.501, 3.501, 3.511,
     * 3.511, 3.521, 3.521, 3.531, 3.541, 3.541, 3.551, 3.551,
     * 3.561, 3.561, 3.571, 3.571, 3.581, 3.581, 3.591, 3.591,
     * 3.601, 3.601, 3.611, 3.611, 3.621, 3.631, 3.641, 3.661/
      data xx4/
     * 3.681, 3.701, 3.741, 3.781, 3.821, 3.861, 3.911, 3.971,
     * 4.051, 4.131, 4.211, 4.291, 4.371, 4.451, 4.531, -.499,
     * -.009,  .691, 1.241, 1.661, 1.981, 2.251, 2.481, 2.671,
     * 2.821, 2.931, 3.021, 3.091, 3.141, 3.151, 3.091, 2.981,
     * 2.841, 2.761, 2.751, 2.781, 2.841, 2.911, 2.991, 3.081,
     * 3.181, 3.271, 3.341, 3.371, 3.381, 3.381, 3.371, 3.361,
     * 3.351, 3.341, 3.331, 3.321, 3.311, 3.301, 3.301, 3.291,
     * 3.291, 3.291, 3.291, 3.291, 3.301, 3.301, 3.311, 3.321,
     * 3.321, 3.331, 3.341, 3.341, 3.351, 3.361, 3.361, 3.371,
     * 3.381, 3.381, 3.391, 3.401, 3.401, 3.411, 3.421, 3.421,
     * 3.431, 3.441, 3.441, 3.451, 3.461, 3.461, 3.461, 3.471,
     * 3.471, 3.481, 3.481, 3.491, 3.491, 3.501, 3.501, 3.501/
      data xx5/
     * 3.501, 3.511, 3.531, 3.551, 3.571, 3.601, 3.621, 3.641,
     * 3.681, 3.721, 3.761, 3.811, 3.871, 3.951, 4.031, 4.111,
     * 4.191, 4.271, 4.351, 4.431, -.059,  .111,  .471,  .851,
     * 1.191, 1.491, 1.761, 1.991, 2.181, 2.331, 2.441, 2.511,
     * 2.551, 2.561, 2.531, 2.481, 2.451, 2.461, 2.491, 2.541,
     * 2.601, 2.681, 2.761, 2.861, 2.961, 3.041, 3.101, 3.131,
     * 3.141, 3.151, 3.151, 3.141, 3.131, 3.121, 3.111, 3.101,
     * 3.101, 3.091, 3.091, 3.091, 3.081, 3.081, 3.091, 3.091,
     * 3.101, 3.111, 3.111, 3.121, 3.131, 3.131, 3.141, 3.151,
     * 3.161, 3.161, 3.171, 3.181, 3.181, 3.191, 3.201, 3.211,
     * 3.211, 3.221, 3.231, 3.231, 3.241, 3.251, 3.251, 3.261,
     * 3.271, 3.281, 3.281, 3.291, 3.301, 3.301, 3.311, 3.311/
      data xx6/
     * 3.321, 3.321, 3.321, 3.331, 3.331, 3.331, 3.341, 3.351,
     * 3.361, 3.381, 3.401, 3.441, 3.481, 3.521, 3.561, 3.611,
     * 3.671, 3.741, 3.821, 3.901, 3.981, 4.061, 4.141, 4.221,
     * 4.301,  .201,  .291,  .491,  .741,  .991, 1.231, 1.441,
     * 1.621, 1.771, 1.891, 1.991, 2.061, 2.091, 2.121, 2.161,
     * 2.201, 2.251, 2.311, 2.371, 2.441, 2.521, 2.601, 2.691,
     * 2.781, 2.861, 2.921, 2.961, 2.981, 2.991, 2.991, 2.981,
     * 2.971, 2.961, 2.961, 2.951, 2.941, 2.941, 2.941, 2.931,
     * 2.931, 2.931, 2.941, 2.941, 2.951, 2.951, 2.961, 2.971,
     * 2.971, 2.981, 2.991, 3.001, 3.001, 3.011, 3.021, 3.031,
     * 3.031, 3.041, 3.051, 3.061, 3.061, 3.071, 3.081, 3.091,
     * 3.101, 3.101, 3.111, 3.121, 3.131, 3.141, 3.141, 3.151/
      data xx7/
     * 3.161, 3.161, 3.171, 3.181, 3.181, 3.191, 3.191, 3.201,
     * 3.201, 3.201, 3.211, 3.221, 3.241, 3.261, 3.291, 3.311,
     * 3.331, 3.371, 3.411, 3.451, 3.501, 3.561, 3.641, 3.721,
     * 3.801, 3.881, 3.961, 4.041, 4.121, 4.201,  .381,  .421,
     *  .551,  .711,  .901, 1.081, 1.251, 1.401, 1.531, 1.651,
     * 1.751, 1.841, 1.921, 1.981, 2.041, 2.101, 2.171, 2.251,
     * 2.321, 2.401, 2.481, 2.571, 2.651, 2.731, 2.801, 2.841,
     * 2.861, 2.871, 2.861, 2.841, 2.831, 2.821, 2.811, 2.811,
     * 2.811, 2.811, 2.811, 2.811, 2.811, 2.811, 2.821, 2.821,
     * 2.831, 2.841, 2.841, 2.851, 2.861, 2.871, 2.871, 2.881,
     * 2.891, 2.901, 2.911, 2.921, 2.931, 2.941, 2.941, 2.951,
     * 2.961, 2.971, 2.981, 2.981, 2.991, 3.001, 3.011, 3.021/
      data xx8/
     * 3.031, 3.041, 3.041, 3.051, 3.061, 3.071, 3.071, 3.081,
     * 3.091, 3.091, 3.101, 3.101, 3.111, 3.111, 3.111, 3.131,
     * 3.151, 3.171, 3.191, 3.211, 3.231, 3.251, 3.291, 3.331,
     * 3.381, 3.431, 3.501, 3.581, 3.661, 3.741, 3.821, 3.901,
     * 3.981, 4.061, 4.141,  .521,  .551,  .631,  .751,  .881,
     * 1.031, 1.171, 1.301, 1.421, 1.541, 1.641, 1.731, 1.821,
     * 1.901, 1.981, 2.061, 2.141, 2.221, 2.301, 2.371, 2.451,
     * 2.521, 2.581, 2.631, 2.671, 2.691, 2.691, 2.681, 2.681,
     * 2.681, 2.671, 2.671, 2.671, 2.671, 2.671, 2.671, 2.671,
     * 2.681, 2.681, 2.691, 2.701, 2.701, 2.711, 2.721, 2.731,
     * 2.731, 2.741, 2.751, 2.761, 2.771, 2.781, 2.791, 2.801,
     * 2.811, 2.821, 2.831, 2.841, 2.841, 2.851, 2.861, 2.871/
      data xx9/
     * 2.881, 2.891, 2.901, 2.911, 2.921, 2.921, 2.931, 2.941,
     * 2.951, 2.961, 2.971, 2.981, 2.981, 2.991, 3.001, 3.001,
     * 3.001, 3.011, 3.011, 3.021, 3.031, 3.041, 3.061, 3.081,
     * 3.101, 3.131, 3.171, 3.211, 3.251, 3.301, 3.361, 3.431,
     * 3.511, 3.591, 3.671, 3.751, 3.831, 3.911, 3.991, 4.071,
     *  .641,  .661,  .721,  .801,  .911, 1.031, 1.141, 1.251,
     * 1.361, 1.471, 1.571, 1.671, 1.771, 1.861, 1.941, 2.021,
     * 2.101, 2.171, 2.241, 2.311, 2.371, 2.431, 2.471, 2.491,
     * 2.501, 2.501, 2.491, 2.491, 2.481, 2.481, 2.481, 2.491,
     * 2.501, 2.511, 2.521, 2.531, 2.541, 2.541, 2.551, 2.561,
     * 2.571, 2.581, 2.591, 2.601, 2.611, 2.621, 2.631, 2.641,
     * 2.651, 2.661, 2.671, 2.681, 2.691, 2.701, 2.711, 2.721/
      data xxa/
     * 2.731, 2.741, 2.751, 2.761, 2.771, 2.781, 2.791, 2.801,
     * 2.811, 2.821, 2.831, 2.841, 2.851, 2.861, 2.861, 2.871,
     * 2.881, 2.891, 2.891, 2.901, 2.911, 2.911, 2.921, 2.921,
     * 2.931, 2.941, 2.961, 2.981, 3.001, 3.031, 3.061, 3.101,
     * 3.141, 3.181, 3.241, 3.301, 3.381, 3.461, 3.541, 3.621,
     * 3.701, 3.781, 3.861, 3.941, 4.021,  .741,  .761,  .801,
     *  .871,  .951, 1.051, 1.141, 1.241, 1.341, 1.441, 1.531,
     * 1.621, 1.711, 1.791, 1.871, 1.941, 2.011, 2.081, 2.141,
     * 2.181, 2.211, 2.231, 2.241, 2.241, 2.251, 2.261, 2.271,
     * 2.281, 2.291, 2.301, 2.311, 2.331, 2.341, 2.351, 2.361,
     * 2.371, 2.391, 2.401, 2.411, 2.421, 2.431, 2.451, 2.461,
     * 2.471, 2.481, 2.491, 2.511, 2.521, 2.531, 2.541, 2.561/
      data xxb/
     * 2.571, 2.581, 2.591, 2.611, 2.621, 2.631, 2.641, 2.651,
     * 2.671, 2.681, 2.691, 2.701, 2.711, 2.721, 2.731, 2.741,
     * 2.751, 2.761, 2.771, 2.781, 2.791, 2.791, 2.801, 2.801,
     * 2.811, 2.821, 2.821, 2.821, 2.831, 2.841, 2.861, 2.881,
     * 2.901, 2.931, 2.961, 2.991, 3.031, 3.071, 3.121, 3.181,
     * 3.251, 3.331, 3.411, 3.491, 3.571, 3.651, 3.731, 3.811,
     * 3.891, 3.971,  .821,  .831,  .861,  .921,  .981, 1.061,
     * 1.141, 1.221, 1.301, 1.391, 1.471, 1.551, 1.621, 1.691,
     * 1.751, 1.801, 1.851, 1.901, 1.941, 1.981, 2.011, 2.041,
     * 2.061, 2.081, 2.101, 2.121, 2.141, 2.161, 2.181, 2.201,
     * 2.221, 2.241, 2.261, 2.271, 2.291, 2.301, 2.321, 2.341,
     * 2.351, 2.371, 2.381, 2.391, 2.411, 2.421, 2.431, 2.451/
      data xxc/
     * 2.461, 2.471, 2.481, 2.491, 2.511, 2.521, 2.531, 2.541,
     * 2.551, 2.561, 2.571, 2.591, 2.601, 2.611, 2.621, 2.631,
     * 2.641, 2.651, 2.661, 2.671, 2.681, 2.691, 2.701, 2.711,
     * 2.721, 2.731, 2.741, 2.751, 2.761, 2.771, 2.771, 2.781,
     * 2.781, 2.791, 2.801, 2.821, 2.851, 2.881, 2.911, 2.951,
     * 2.991, 3.031, 3.081, 3.141, 3.211, 3.271, 3.331, 3.411,
     * 3.491, 3.571, 3.651, 3.731, 3.811, 3.891, 3.971, 41*0.0/
c
c   find lower bound depth curve
c
      do i=1,11
        if(depth.lt.h(i)) then
           idp=i-1
           goto 1
        endif
      enddo
 1    continue
c
c   interpolate on lower bound depth curve, if distance more than
c   100 deg, use value at 100 deg
c
      if(dist.ge.100.0) then
         qlow=q(101,idp)
         qhigh=q(101,idp+1)
         goto 2                 ! go to depth interpolation
      endif
      idist=dist+1              ! first value is for distance 0 degrees
      qlow=q(idist,idp) + (q(idist+1,idp)-q(idist,idp))*(dist-idist+1)
      qhigh=q(idist,idp+1) + 
     *       (q(idist+1,idp+1)-q(idist,idp+1))*(dist-idist+1)
c
c  now do depth interpolation
c
 2    continue
      qout=qlow + (qhigh-qlow)/(h(idp+1)-h(idp))*(depth-h(idp))
      return          
      end
