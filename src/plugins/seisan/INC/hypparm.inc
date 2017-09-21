c
c  include file for hypocenter as of version 7.0
c
c  changes:
c  october 98   station to 5 letters
c  november 99 jh : add nl, vs,v,d,parm,pos,rmssum to common block
c  jan 30         : add multi_model
c  aug 19 2000 jh : add one_rms  

      integer nstats,narriv,nlayer,niasp,nl
      parameter (nstats=5000,narriv=2000,nlayer=150,niasp=150)
      logical multi_model   ! true if searching through multiple models
      real one_rms          ! rms of current event
      real dtwt(narriv),x0(narriv),y0(narriv),test(200),
     &aninc(narriv),dl(narriv),xh0(3),tpc(narriv),
     &vs(nlayer),xh1(3),v(nlayer),d(nlayer),dlt(narriv),
     &std(3),std1(3),az0(narriv),baz0(narriv)
      real dt(narriv),dtw1(narriv),dtwt1(narriv),xh2(3),
     &tpc1(narriv),aninc1(narriv),xhd(3),rmss(10),resl(3,3),
     &xh(3),dt1(narriv),appar_veloc(narriv)
      real tt(niasp),dtdl(niasp),dtdh(niasp),dddp(niasp),xhh0(3)
      real dt2(narriv),parm(2*nlayer),di(narriv),di1(narriv)
      real ss11(3),ss01(3),scorr(narriv,9)
      integer iselv(narriv),ip(narriv),ips(narriv),isort(narriv)
      integer imap(narriv)
      double precision tp(narriv),tph,tph0
      character*1 fixor,distind
      character*5 st(narriv)
      character*8 phcd(niasp),trphs(narriv),phase(narriv)
      character*80 data(narriv),dataprev
      real g,dth,xm,var,xhh,alpha,ax,aa,ss1,eig,eig1,a
      real u,vel,alpha1,g1,ve,sgn,e,ve1,th,ss,xs,dx(3)
      integer nq,icflag,imflag
      real pos,rmssum
      common/comm1/th(nlayer),xs(3),
     &g(narriv,3),dth(3),xm(3),var(3,3),nq(3),xhh(3),
     &alpha(3),ax(3), ss(3),ss1(3),eig(3),eig1(3),a(16),
     &u(narriv,3),ve1(16),alpha1(3),g1(narriv,3),ve(16),
     &sgn(3),icflag,imflag,e(3),    !icflag, imflag added 6/98, BRL
     *nl,v,vs,d,parm,pos,rmssum,multi_model,one_rms
