c******************************************************************************
c   
c    used on pc, sun and vax, see commented out lines
c
c    latest update:
c
c    oct 17 91 by jh : increase field for ml to f4.1 in routine updatml
c    jan 7  92       : add routine read_stat_mod
c    jun 21 92    jh,cl: bug in ml calc. alog instead of alog10
c                        new global location changes. 
c    jul 30          : bug in hypout
c    oct 9,12  92    : update_ml also calculates mc magnitude, posible
c                      to calculate if no location if distance is there.
c                      calculate ms
c    oct 13          : small changes, updateml is now called update_mag
c    oct 26          : check for missing stations in print.out
c    nov 22          : use hypocentral distance for ml in update_mag
c    mar 93          : test(8) neg, use log coda squared in mc
c    jul 19    93    : iasp to capital letters
c    nov             : new bl program
c    nov 17    93    : magnitudes bug
c    april 5   94    : deleted hypout, added dlt and isort to hypocent window
c    jun 2           : no ml if no dist
c    oct 14          : limit mb to distances of 11000km
CJAB(BGS)Jan95       : Install file & error handling.
c    jun 2 95 jh     : only calculate Ml if balnk, S or L-phase
c     feb 21 96        : magnitudes on a second line
c    mar 6 96        : change condition for calculaiton of mb, mc and ml
c    mar 11          : add  dist type to second mag line
c    mar 14          : small change to clear_old
c    mar 21          : bug in mb
c    apr 16          : ---------
c    sep 19, 96      : bug average corner frequency, more data on 2 mag line
c    mar 4 97        : increase amplitude files for amp out from 8 to 12
c    mar 18          : in connection with above, move coda: right
c    jan 30    98    : magnitude residuals
c    feb 18          : spectral p velocity 
c    mar 1           : set svelocity to 3.5 if not defined in older data
c    sep  18   98 jh : ---------- seisan version 7.0 check --------------
c                      5 character stations
c
c     oct 28 98 bmt : linux changed, save and *
c    jun 10 99 jh    : add AMP as a valid phase for calculating ml and mb
c    jul 26          : remove routine mb_att since a single file already
c    jul 29          : add use_eev to routine read_stat_model
c    sep 24    lo    : put in edepth and spec_phase to upd_spec
c              jh    : fix new spectral distance
c    sep 27          : replace travel time with kappa in spectral output
c    feb 17 2000 jh  : suppres output if output false
c    oct 24          : clear out old H lines in clear routine
c    oct 26          : bug with spectral average and update: if station
c                      with phase line missing, following spectra not used
c    nov  5          : small correction to write out, only write number
c                      of spectra if more than 0
c    mar 5, 2001 jh  : distance as real
c    may 19          : more accuracy in sdrop and radius
c    may 27          : do not use spectra not P or S in average
c    may 31          : Recalculted spectra not written out !!!!!
c    apr 19 2002 bmt : change maxline=1000
c    apr 13 2003 jh  : implement AMPL, AMPb, AMPS
c    may 7  2003 lo  : ISC amp codes changed to AML, AMb and AMS
c    apr 08 2003 lot : add call to iasp91_filename to get comp de
c
C#################################################################



      SUBROUTINE UPDATE_MAG(DATA,NHEAD,NRECORD,AGENCY,TEST,OUTPUT)
C
C   ROUTINE CALCULATES AVERAGE  MAGNITUDES FOR EVENT WRITTEN
C   IN STANDARD FORMAT IN TEXT ARRAY DATA. MAGNITUDES  ARE
c   WRITTEN IN HEADER
C
C     INPUT:  DATA:    EVENT IN STANDARD FORMAT IN TEXT ARRAY
C             NHEAD:   NUMBER OF HEADER RECORDS IN DATA
C             NRECORD: TOTAL NUMBER OF RECORDS IN DATA         
C             AGENCY:  TYPE AND AGENCY E.G. BER
C             TEST   : RESET TEST FROM HYPOCENT
c             OUTPUT : if true, write to screen
C
C     OUTPUT: DATA:    EVENT IN STANDARD FORMAT IN TEXT ARRAY
C                      WITH AVERAGE LOCAL MAGNITUDE IN HEADER
C
C     CHANGES
C     JAN    91    RAN CHANGED TO PC 
c     aug 7  91    jh  : bugs
c     OCT 10 91 BY J.H.: include local  magnitude using the following
c                      ml = A*log(amp) + B*log(dist) + C * dist + D
c                      where amp is max ground motion o-p in nm, 
C                      dist is hypocentral distance in km 
c     oct 18           : add constant d
c     october 92       : mc and ms calculation
c     nov 22           : use hypocentral distance for ml
c     nov 17 93        : do not overwrite 3. magnitude
c     apr 94           : new hypocent
c     apr 94           : use reset test for magnitudes
c     jun 94           : add mb
c     jul 94           : distance bug
c     nov 95           : error when writing out ms magntude
c     feb 21 96        : magnitudes on a second line
c     jan 28 98    jh  : use hypocentral distance also for coda
c     jan 30           : magnitude residuals
c     apr 11 03    jh  : implement amplitude phases AMPL, AMPB and AMPS
c
      implicit none
      save
c

      include 'hypparm.inc'
      integer nhead,nrecord
c BRL 6/98: changed a,b,c,d to am,bm,cm,dm
      real am,bm,cm,dm
      logical output
      character*3 agency
      character*80 text
c-- amplitude,period, distance, hypocentral distance and coda
      real amp,period,dist,hdist,coda
c-- depth
      real depth
c--number of stations with different magnitudes 
      integer nml,nmc,nms,nmb
c-- help varibles
      integer nrec,icoda,nline,maxline
      real xmag,cmag,smag,bmag,q,cmag1
      logical mag_write          ! true if a posiiton found for writing mag.
C--data
c      CHARACTER*80 DATA(*)
c--single local magnitude
      REAL MAGNITUDE
c--reset test variables
c      real test(*)
      character*5 station(nstats)   ! station list
      character*5 stat              ! one station
      real mag_cor(5,nstats)        ! magnitude residuals,1:C, 2:L, 3:B, 4:S 5:W
      integer kstat                 ! station number currently used
c--counters etc
      integer i,k
      common/hyp/station,mag_cor,nline,maxline
c
      nmc=0
      nms=0
      nmb=0
      nms=0.0
      cmag=0.0
      smag=0.0
      bmag=0.0
      NML=0
      XMAG=0.0
      nline=0
c      maxline=20
      maxline=1000
c
c  get ml parameters
c
      am=test(75)
      bm=test(76)
      cm=test(77)
      dm=test(78)
c
c  read depth
c
      read(data(1)(39:43),'(f5.0)') depth
c
      DO 1 NREC=NHEAD+1,NRECORD-1
          READ(DATA(NREC),'(1x,a5,23X,i4,g7.1,1X,F4.1,25X,f5.0)'
     *    ,err=1234)
     *    stat,icoda,AMP,PERIOD,DIST
c
c   if amp or coda is not zero but distance zero, try to find distance at
c   other line with same station if no location flag set but
c   magntudes calculated, test 106
c
          if((amp.gt.0.0.or.icoda.gt.0).and.dist.eq.0.0.and.
     *    test(106).gt.0.0) then
             do i=nhead+1,nrecord-1
                if(stat.eq.data(i)(2:6).and.data(i)(71:75).ne.' ') 
     *          then
                   read(data(i)(71:75),'(f5.0)') dist
                   goto 1235
                endif
             enddo
          endif
          goto 1235
 1234     continue
          write(6,*)' Something wrong with input record:'
          write(6,'(a)') data(nrec)
          amp=0.0
 1235     continue 
c
c   find station number
c
          do i=1,nstats
            if(station(i).eq.stat) then
               kstat=i
               goto 1245
            endif
          enddo
 1245     continue
c         write(6,*)kstat,mag_cor(1,kstat)
c
c
c   use hypocentral distance
c
          hdist=sqrt(dist*dist+depth*depth)
c
c   must be larger than 1 km
c
          if(hdist.le.1.0) hdist=1.0     ! added april 2003 by jh
c
          coda=icoda
c
c   check if coda and distance available, use log coda squared if
c   coefficient is negative, if too far, do not calculate
c
          if(dist.gt.0.and.icoda.gt.0.and.dist.le.test(57)) then
             if(test(8).gt.0.0) then
              cmag1=test(7)+test(8)*alog10(coda)+hdist*test(9)
             else
              cmag1=test(7)-test(8)*alog10(coda)*alog10(coda)
     *        +hdist*test(9)
             endif
             if(mag_cor(1,kstat).ne.99.0) then
                cmag=cmag+cmag1+mag_cor(1,kstat)
                nmc=nmc+1
             endif
c
c   write distance, coda and mag
c
             if(nline.ge.0.and.output) then
                if(mag_cor(1,kstat).ne.99.0) then
                  if(mag_cor(1,kstat).eq.0.0) then
                     write
     *               (6,'(1x,a7,2x,a,2x,f8.1,3x,a,1x,f8.1,3x,a,f5.1)')
     *               data(nrec)(2:8),' hdist:',hdist,
     *               'coda:    ',coda,'mc = ',cmag1
                  else
                     write     ! write correction
     *               (6,'(1x,a7,2x,a,2x,f8.1,3x,a,1x,
     *               f8.1,3x,a,f5.1,a,f5.2)')
     *               data(nrec)(2:8),' hdist:',hdist,
     *               'coda:    ',coda,'mc = ',cmag1,' + ',
     *               mag_cor(1,kstat)
                  endif
                else
                  write(6,'(1x,a7,a,a)')data(nrec)(2:8),
     *            ' Coda available,',
     *    '       magnitude not used (weighted out)' 
                endif
                nline=nline+1
             endif
             if(nline.gt.maxline) then
               nline=0
               write(6,'('' Return to continue, q to end listing '',$)')
               read(5,'(a)') text
               if(text(1:1).eq.'q'.or.text(1:1).eq.'Q') nline=-1
             endif
          endif
c
c   check if amplitude is available, assume local magnitude if
c   phase is blank, AMPL, L or S, AMP, period less than 5 secs 
c      and distance less than test(57)
c
c
          IF(AMP.GT.0.0.and.period.lt.5.0.and.
     *       dist.gt.0.and.dist.lt.test(57).and.
     *           (data(nrec)(11:11).eq.' '
     *           .or.data(nrec)(11:11).eq.'L'
     *           .or.data(nrec)(11:12).eq.'S '
     *           .or.data(nrec)(11:12).eq.'Sg'
     *           .or.data(nrec)(11:12).eq.'SG'
     *           .or.data(nrec)(11:14).eq.'AMPL'
     *           .or.data(nrec)(11:13).eq.'AML'
     *           .or.data(nrec)(11:14).eq.'AMP ')
    
     *        ) then
             magnitude=am*alog10(amp)+bm*alog10(hdist)+cm*hdist+dm
             if(mag_cor(2,kstat).ne.99.0) then
                XMAG=XMAG+MAGNITUDE + mag_cor(2,kstat)
                nml=nml+1
             endif
            
c
c   write distance, amplitude and magnitude
c
             if(nline.ge.0.and.output) then
                if(mag_cor(2,kstat).ne.99.0) then
                   if(mag_cor(2,kstat).eq.0.0) then
                      write
     *                (6,'(1x,a7,2x,a,2x,f8.1,3x,a,2x,f12.1,3x,a,f5.1)')
     *                data(nrec)(2:8),' hdist:',hdist,
     *                'amp:',amp,'ml = ',magnitude
                   else        ! magnitude correction
                      write
     *                (6,'(1x,a7,2x,a,2x,f8.1,3x,a,2x,f12.1,
     *                3x,a,f5.1,a,f5.2)')
     *                data(nrec)(2:8),' hdist:',hdist,
     *                'amp:',amp,'ml = ',magnitude,' + ',
     *                mag_cor(2,kstat)
                   endif
                else
                  write(6,'(1x,a7,a,a)') data(nrec)(2:8),
     *            ' Amp for ml available, magnitude not used',
     *            ' (weighted out)' 
                endif
                nline=nline+1
             endif
            if(nline.gt.maxline) then
               nline=0
               write(6,'('' Return to continue, q to end listing '',$)')
               read(5,'(a)') text
               if(text(1:1).eq.'q'.or.text(1:1).eq.'Q') nline=-1
            endif
          ENDIF
c
c   check if surface wave magnitude, assume period gt 10 secs, 
c   any amplitude
c
          if(amp.gt.0.0.and.period.gt.10.0.and.dist.gt.0.0.
     *       and.(data(nrec)(11:11).eq.' '.or.
     *            data(nrec)(11:14).eq.'AMPS'.or.
     *            data(nrec)(11:13).eq.'AMS'.or.
     *            data(nrec)(11:14).eq.'AMP ')
     *       ) then
                magnitude=alog10(amp/(1000.0*period))+
     *          1.66*alog10(dist/111.2)+3.3
                if(mag_cor(4,kstat).ne.99.0) then
                   smag=smag+magnitude+mag_cor(4,kstat)
                   nms=nms+1
                endif
c
c   write distance, amplitude and magnitude 
c
             if(nline.ge.0.and.output) then
                if(mag_cor(4,kstat).ne.99.0) then
                   if(mag_cor(4,kstat).eq.0.0) then
                      write
     *                (6,'(1x,a7,2x,a,2x,f8.1,3x,a,2x,f12.1,3x,a,f5.1)')
     *                data(nrec)(2:8),'  dist:',dist,
     *                'amp:',amp,'ms = ',magnitude
                   else
                      write
     *                (6,'(1x,a7,2x,a,2x,f8.1,3x,a,2x,f12.1,
     *                3x,a,f5.1,a,f5.2)')
     *                data(nrec)(2:8),'  dist:',dist,
     *                'amp:',amp,'ms = ',magnitude,' + ',
     *                mag_cor(4,kstat)
                   endif
                else
                  write(6,'(1x,a7,a,a)')data(nrec)(2:8),
     *            ' Amp for ms available, magnitude not used',
     *            ' (weighted out)' 
                endif
                nline=nline+1
             endif
            if(nline.gt.maxline) then
               nline=0
               write(6,'('' Return to continue, q to end listing '',$)')
               read(5,'(a)') text
               if(text(1:1).eq.'q'.or.text(1:1).eq.'Q') nline=-1
            endif
          endif
c
c   mb only up to 100 deg
c   check if mb, only if period lt 5 secs and greater than 0.2 and
c   1: if phase is P
c   2: if phase is blank and and distance gt test(57),
c
          IF(AMP.GT.0.0.and.period.lt.5.0
     *    .and.period.gt.0.2.and.dist.gt.0
     *    .and.
     *          ((data(nrec)(11:11).eq.'P'.or.
     *            data(nrec)(11:14).eq.'AMPB'.or.
     *            data(nrec)(11:13).eq.'AMB'.or.
     *            data(nrec)(11:14).eq.'AMPb'.or.
     *            data(nrec)(11:13).eq.'AMb'.or.
     *            data(nrec)(11:14).eq.'AMP ')
     *           .or.
     *           (data(nrec)(11:11).eq.' '
     *           .and.dist.gt.test(57)))
     *    ) then
               if(dist.gt.11000) then
                 write(6,'(1x,a,1x,a)')
     *           data(nrec)(2:8), ' Station too far for mb'
               else
c
c   get attenuation term
c           
                 call mb_att(depth,dist/111.2,q)
                 magnitude=alog10(amp/period)+q
                 if(mag_cor(3,kstat).ne.99.0) then
                    bmag=bmag+magnitude+mag_cor(3,kstat)
                    nmb=nmb+1
                 endif
c
c   write distance and depth
c
                 if(nline.ge.0.and.output) then
                    if(mag_cor(3,kstat).ne.99.0) then
                       if(mag_cor(3,kstat).eq.0.0) then
                          write(6,'(1x,a7,2x,a,2x,
     *                    f8.1,3x,a,2x,f12.1,3x,a,f5.1)')
     *                    data(nrec)(2:8),'  dist:',dist,
     *                    'amp:',amp,'mb = ',magnitude
                       else
                          write(6,'(1x,a7,2x,a,2x,
     *                    f8.1,3x,a,2x,f12.1,3x,a,f5.1,a,f5.2)')
     *                    data(nrec)(2:8),'  dist:',dist,
     *                    'amp:',amp,'mb = ',magnitude,' + ',
     *                    mag_cor(3,kstat)
                       endif
                    else
                       write(6,'(1x,a7,a,a)')data(nrec)(2:8),
     *                 ' Amp for mb available, magnitude not used',
     *                 ' (weighted out)' 
                    endif           
                    nline=nline+1
                 endif
                 if(nline.gt.maxline) then
                   nline=0
                   write
     *             (6,'('' Return to continue, q to end listing '',$)')
                   read(5,'(a)') text
                   if(text(1:1).eq.'q'.or.text(1:1).eq.'Q') nline=-1
                 endif
             endif
          endif
 1     CONTINUE
c
c   write mc in first mag position if mag available
c
       if(nmc.ne.0) then
          cmag=cmag/nmc
          if(cmag.lt.-10.0.or.cmag.gt.10.0) cmag=-9.9
          write(data(1)(56:63),'(f4.1,a1,a3)')cmag,'C',agency
       endif
c
c   check if Ml magnitude is available, if  mag  in first pos. write
c   in second postion, else first.
c 
       IF(NML.NE.0) THEN
          XMAG=XMAG/NML
          if(xmag.lt.-10.0.or.xmag.gt.10.0) xmag=-9.9
          if(data(1)(60:60).ne.' ') then
             write(data(1)(64:71),202) xmag,agency
          else
             write(data(1)(56:63),202) xmag,agency
 202         format(f4.1,'L',a3)
          endif
       endif
c
c   check if ms is available, write in first available position
c
       if(nms.gt.0) then
          smag=smag/nms
          if(smag.lt.-10.0.or.smag.gt.10.0) smag=-9.9
          if(data(1)(60:60).eq.' ')then 
             write(data(1)(56:63),203) smag,agency
          elseif(data(1)(68:68).eq.' ')then 
             write(data(1)(64:71),203) smag,agency
          elseif(data(1)(76:76).eq.' ')then 
             write(data(1)(72:79),203) smag,agency
          else   ! all positions used on first line
c
c   put in a new line with magnitudes
c
                do k=nrecord,2,-1      ! move records one down 
                   data(k+1)=data(k)
                enddo
                data(2)=' '            ! new line with more magnitudes
                data(2)(46:48)=data(1)(46:48)    ! use same agency
                data(2)(1:23)=data(1)(1:23)      ! use same  start
                write(data(2)(56:63),203) smag,agency ! put in magnitude
                data(2)(80:80)='1'
                nhead=nhead+1
                nrecord=nrecord+1
 203         format(f4.1,'S',a3)
          endif
       endif
c
c   check if mb is available, write in first available position
c
       if(nmb.gt.0) then
          bmag=bmag/nmb
          if(bmag.lt.-10.0.or.bmag.gt.10.0) bmag=-9.9
          if(data(1)(60:60).eq.' ')then 
             write(data(1)(56:63),205) bmag,agency
          elseif(data(1)(68:68).eq.' ')then 
             write(data(1)(64:71),205) bmag,agency
          elseif(data(1)(76:76).eq.' ')then 
             write(data(1)(72:79),205) bmag,agency
 205         format(f4.1,'B',a3)
          else
             do i=2,nhead             ! check if other headers can be used
                if(data(i)(80:80).eq.'1'. ! must have same loc. agency
     *          and.data(1)(46:48).eq.data(i)(46:48)) then
                   write(data(i)(64:71),205) bmag,agency  ! must be 2. position
                   mag_write=.true.
                endif
             enddo
c
c   put in a new line with magnitudes
c
             if(.not. mag_write) then
                do k=nrecord,2,-1      ! move records one down 
                   data(k+1)=data(k)
                enddo
                data(2)=' '            ! new line with more magnitudes
                data(2)(46:48)=data(1)(46:48)    ! use same agency
                data(2)(22:22)=data(1)(22:22)    ! use same type
                data(2)(1:10)=data(1)(1:10)      ! use same y, m, d
                data(2)(80:80)='1'
                write(data(2)(56:63),205) bmag,agency ! put in magnitude 
                nhead=nhead+1
                nrecord=nrecord+1
             endif
          endif
       endif
       RETURN
       END
c---------------------------------------------------------------------------

      subroutine read_stat_mod
     &(agency,model_code,model,minstat,minphase,
     &modnam,loc_indicator,iustat,iuphs,iulst,iusum,iutab,isort,
     &test,dlt,yter,maxline,use_eev)
c
c   read model and stations etc, assumed from unit 1, the file name
c   is STATIONX.HYP where X can be any character given by model. If
c   model is blank, the standard model 0 is assumed. The routine will
c   first look for the station model in the curent directory, then
c   in the DAT directory. If the model file is not found, modlel=@ is
c   returned
c
c   iustat: 0: no station list, iulst: unit for print.out file
c
c   j havskov jan 92
c 
c   jul 92 by jh : add new par line for event type
c   jul 93       : iasp to capital letters
c   april 94 brl : added I/O numbers - was using #'s different from main program!!
c   apr 94 jh    : do not read addtional parameters except agency
CJAB(BGS)Jan95   : Install file & error handling...
CJAB(BGS)Jan95   : & remove VAX..
c
       implicit none
       save
      include 'libsei.inc'                 ! Library definitons.
      include 'hypparm.inc'                 !added 6/98 BRL
      external sei get file,               ! Find file & open handler.
     &         sei close,                  ! & closure.
     &         sei clen,                   ! String length.
     &         sei code                    ! Error condition handler.
      integer  sei clen                    ! & function.
      integer  code                        ! Condition.
      logical  b_flag                      ! A problem flagged?.
      logical use_eev                      ! true if call from eev
c
c      character*80 data(*)
      character*3 agency                                           
      character*20 model_code
      character*1 yter
      integer minstat,minphase,init,maxline
      character*1 model
c--- location indicator
      character*1 loc_indicator(3)
c--- name of station-model file in current directory
      character*80 cur_file
c--- name of stationmodel in DAT directory
      character*80 modnam
c--- event #
      integer eventno      
c--- isort &  test added
c      integer isort(*)                        
      logical locate
c--  covarrience matrix and origin time error
      real covar(3,3),oterr
c--- logical I/O
      integer iustat,iuphs,iulst,iusum,iutab
      integer idummy,nhead,nrecord
      
c      real test(200),dlt(narriv)
  
      model_code='                    ' ! currently do no tuse
c
c  Open the stations file...
c  =========================
c
      cur_file = 'STATION0.HYP'                ! Set up filename.
      if(model.ne.' ') cur_file(8:8)=model     ! & adjust
      call sei get file( open$+ignore$,        ! Find file & open it.
     &                   iustat,               ! On unit.
     &                   code,                 ! Returned condition.
     &                   'DAT',                ! Alternative directory to search
     &                   cur_file )            ! For stations file.
c
         if( code .ne. e_ok$ ) then         ! Does not exist.
         chr_err_msg$ = cur_file(:seiclen(cur_file)) //
     &                  ' does not exist'
         call sei code( stop$,              ! Halt the program with user-.
     &                  e_misf$,            ! Message; dummy use of code.
     &                  0,                  ! Unit (n/a).
     &                  b_flag )            ! Flag (n/a).
         end if                             !
c
c   check for iaspei files
c   ----------------------
c

      call iasp91_filename(modnam)
      modnam = modnam(:index(modnam,' ')-1) // '.TBL'  ! File name
      call sei get file( check$,               ! Find file.
     &                   0,                    ! On unit (n/a).
     &                   code,                 ! Returned condition.
     &                   'DAT',                ! Alternative directory to search
     &                   modnam )              ! For iaspei file.
c      modnam = 'IASP91.TBL'                    ! File name.
c
c    File exists somewhere...
c
         if( code .eq. e_ok$ ) then               ! Exists in pathname.
         modnam = modnam(:index(modnam,'.TBL')-1) ! Extract root.
c
c    File nowhere to be found...
c
         else                               ! Does not exist.
         chr_err_msg$ = '**** WARN: '     //
     &           modnam(:seiclen(modnam)) //
     &                  ' does not exist'
         write(*,*)
         call sei code( warn$,              ! Warn the user it doesn't exist.
     &                  e_misf$,            ! Message; dummy use of code.
     &                  0,                  ! Unit (n/a).
     &                  b_flag )            ! Flag (n/a).
         write(*,*)
         modnam= ' '
         end if                             !
C
C   READ STATION LIST AND MODEL
C
      call hypocent(1,iustat,iuphs,iulst,iusum,iutab,init,'N',
     &data,modnam,eventno,dlt,isort,test,yter,maxline,idummy,
     &locate,covar,oterr,nhead,nrecord,use_eev)
c
c   read agency 
c
      read(iustat,'(a3)',iostat=code) agency  ! Get agency.
      call sei code(fort$,code,iustat,b_flag) ! Process outcome.
      call sei close(close$,iustat,code)      ! Close the file (stop on error).
c
c   get minstat etc
c
      minstat=test(79)
      minphase=test(80)
      loc_indicator(1)='L'
      loc_indicator(2)='R'
      loc_indicator(3)='D'
      if(test(81).ne.1.0) loc_indicator(1)=' '
      if(test(82).ne.1.0) loc_indicator(2)=' '
      if(test(83).ne.1.0) loc_indicator(3)=' '
c
c  make sure minphase has been set to at least 3
c changed to 2 6/98 BRL
      if(minphase.lt.2) minphase=2
      return
c
c error
c
 3    continue
      model='@'   
      return
      end  


c
c------------------------------------------------------------------
c
      subroutine clear_old(data,nhead,nrecord,agency)
c
c   clear old secondary magnitude lines, old error lines
c   and old id lines
c
      implicit none
      save
      integer nrecord,nhead
      character*80 data(*)
      integer i,j,k
      character*3 agency
c
c   delete secondary header lines with magnitude, only delete line
c   if location agency is the same as default agency
c
      k=1
      if(nhead.gt.1) then
         do i=2,nhead             
             if(.not.(data(i)(80:80).eq.'1'. 
     *       and.agency.eq.data(i)(46:48)).and.
     *       data(i)(80:80).ne.'E'.and.
     *       data(i)(79:80).ne.'83'.and.
     *       data(i)(79:80).ne.'93'.and.
     *       data(i)(79:80).ne.'A3'.and.
     *       data(i)(80:80).ne.'H'
     *       .and.(.not.
     *       (data(i)(80:80).eq.'3'.and.data(i)(2:7).eq.'ACTION'))
     *       .and.(.not.
     *       (data(i)(80:80).eq.'3'.and.data(i)(2:13)
     *       .eq.'SPEC AVERAGE'))) then
                k=k+1
                if(k.ne.i) data(k)=data(i)
             endif
         enddo
         j=k          ! save  new number of headers
         if(k.lt.nhead) then           ! some header lines removed, shift rest
             do i=nhead+1,nrecord
                k=k+1
                data(k)=data(i)
             enddo
             nhead=j
             do i=k,nrecord    ! blank double lines
                data(i)=' '
             enddo
             nrecord=k
         endif
      endif
c
c   
      return
      end

c--------------------------------------------------------------------
      subroutine update_spec(data,nrecord,nhead,agency,output)
c
c   recalcualte spectral values so if distance has been changed,
c   the values are updated. Then calculate average values and write out 
c   in nordic data array data and return new number of headers and records.
c   the main header is updated with average mw if there is room and agency 
c   is written in the usual way.
c
      implicit none
      save
      include 'hypparm.inc'
c
c the following 2 otherwise defined in mulplt.inc
c
      real edepth        ! depth
      character*1 spec_phase ! nearest phase to spectrum
      character*3 agency     ! magnitude agency
      character*5 station
      character*4 compon ! component
      integer nrecord,nhead  ! number of record and headers
      real depth             ! hypocentral depth
c--- spectral values, see mulplt.inc
      real mw,moment,sdrop,sslope,svelocity,
     *     radius,swindow,omega0,cornerf,spec_velocity,density,q0,qalpha
      integer secc,travel_time,hour,min
      real geo_distance
      real kappa       ! near term attenuation
      real edistance,xdist   ! epicentral distance
      real avmw,avmoment,avsdrop,avsslope,avradius,avswindow,avomega0
      real avcornerf
      logical mag_write     ! true if room for magnitude on an existing line
      logical output        ! true if screen output
      integer nline         ! line count for magnitude output
      integer maxline       ! maximum magnitude lines out
c
      integer nspec,nspec1   ! number of spectra
      integer nmw            ! numbe rof mw, can be less than nspec if w. out
      real pi
      real surface,radpat  !  surface effect and radiation pattern effect 
      real factor
      character*80 text

      integer i,k
      character*5 stat(nstats)      ! station list
      real mag_cor(5,nstats)        ! magnitude residuals
      integer kstat                 ! station number currently used
      common/hyp/stat,mag_cor,nline,maxline
c
      pi=3.14159265
c     output=.true.
      surface=2.0
      radpat=0.6
c
c   read depth
c
      read(data(1)(39:43),'(f5.1)') edepth
c
      nspec=0
      nspec1=0
      avmw=0.0
      avmoment=0.0
      avsdrop=0.0
      avsslope=0.0
      avradius=0.0
      avswindow=0.0
      avcornerf=0.0
c
c   look for spectral values
c
      k=1
 10   continue
        k=k+1
        if(k.gt.nhead) goto 20   ! calculate average values when all h. read
        if(data(k)(80:80).eq.'3'.and.data(k)(2:5).eq.'SPEC') then
           if(data(k)(7:14).eq.data(k+1)(7:14).and.data(k)(16:30).
     *     ne.data(k+1)(16:30)) then      ! check if pair of lines ok
           nspec1=nspec1+1
c
c   find current distance from station
c
           do i=nhead+1,nrecord-1
              if(data(k)(7:10).eq.data(i)(2:5)) then
                 if(data(i)(71:75).eq.'     ') goto 2   ! no distance
                 read(data(i)(71:75),'(f5.0)') edistance
                 goto 1
              endif
 2            continue
           enddo
c
c   if here, no distance found, jump to next spectral values
c
           k=k+1
           goto 10
 1         continue
c
c  read old spectral values
c
           read(data(k),300)
     *     station(1:4),compon,moment,sdrop,omega0,cornerf,
     *     radius,sslope,swindow,mw
300        format(6x,a4,a4,3x,f5.1,3x,f5.1,3x,f5.1,
     *     3x,f5.2,2x,f6.2,3x,f5.2,3x,f5.1,3x,f5.1)
           station(5:5)=' '   ! only possible to use 4 char station in spec
           read(data(k+1),301)
     *     station(1:4),compon,hour,min,secc,kappa,xdist,
     *     spec_velocity,density,q0,qalpha,svelocity
           spec_phase=data(k+1)(41:41)    ! type of spectrum
           if(data(k+1)(24:25).eq.'TR') kappa=0.0    ! from old seisan
301        format(6x,a4,a4,2x,3i2,3x,f5.3,3x,
     *     f5.0,3x,f5.2,
     *     3x,f5.2,3x,f5.1,3x,f5.2,3x,f5.2)
           station(5:5)=' '
c
c   since svelocity and spec_velocity was introduced feb 98, some older data
c   will not have svelocity defined, so it must be done here. this will also 
c   fix the problem of earlier using p-velocity for radius when making
c   a p-wave spectrum
c
           if(svelocity.eq.0.0) svelocity=3.5    
c
c   find station number
c
          do i=1,nstats
            if(stat(i).eq.station) then
               kstat=i
               goto 1245
            endif
          enddo
 1245     continue
c
c   recalculate
c
c
c   calculate geo_distance
c
           call  spec_dist
     *     (spec_phase,edistance,edepth,factor)

c
           geo_distance=1.0/factor
           factor= 4*pi*(density*1000.0)*
     *          ((spec_velocity*1000.0)**3)
           moment = factor*(geo_distance*1000.0)*
     *     (10.0**omega0)/(radpat*surface*1.0e9)
           radius=0.35*svelocity/cornerf
           sdrop = (0.44*moment)/(1.0e14*radius**3)
           moment = alog10(moment)
           if(moment.gt.0.0)  then
              if(mag_cor(5,kstat).ne.99.0) then
                 mw=moment*0.667-6.06
              else
                 if(output)
     *           write(6,'(1x,a,a)') station, ' mw weighted out'
              endif
           endif
c
c   write geo_distance, amplitude and magnitude
c
             if(nline.ge.0.and.output) then
                if(mag_cor(5,kstat).ne.99.0) then
                   if(mag_cor(5,kstat).eq.0.0) then
                      write
     *                (6,'(1x,a4,1x,2a1,2x,a,2x,f8.1,
     *                3x,a,2x,f12.1,3x,a,f5.1)')
     *                data(k)(7:10),data(k)(11:11),
     *                data(k)(14:14),' gdist:',geo_distance,
     *                'mom:',moment,'mw = ',mw
                   else        ! magnitude correction
                      write
     *                (6,'(1x,a7,2x,a,2x,f8.1,3x,a,2x,f12.1,
     *                3x,a,f5.1,a,f5.2)')
     *                data(k)(7:13),' gdist:',geo_distance,
     *                'mom:',moment,'mw = ',mw,' + ',
     *                mag_cor(5,kstat)
                      mw=mw+mag_cor(5,kstat)
                   endif
                else
                  if(output)write(6,'(1x,a7,a,a)') data(k)(5:13),
     *            ' Mom for mw available, magnitude not used',
     *            ' (weighted out)' 
                endif
                nline=nline+1
             endif
            if(nline.gt.maxline.and.output) then
               nline=0
               write(6,'('' Return to continue, q to end listing '',$)')
               read(5,'(a)') text
               if(text(1:1).eq.'q'.or.text(1:1).eq.'Q') nline=-1
            endif
c
c   check for overflow
c
           if(moment.gt.999.0) moment=999.9
           if(sdrop.gt.999.0) sdrop=999.9
           if(geo_distance.gt.99999) geo_distance=99999
c
c   rewrite values
c
c jh 5-2001, strange seems recalculated spec not output
c          if(output) write(data(k),400)
           write(data(k),400)
     *     station(1:4),compon,moment,sdrop,omega0,cornerf,
     *     radius,sslope,swindow,mw
400        format(' SPEC ',a4,a4,' MO',f5.1,' ST',f5.1,' OM',f5.1,
     *     ' f0',f5.2,' R',f6.2,' AL',f5.2,' WI',f5.1,' MW',f5.1)
           if(sdrop.lt.1.0)
     *     write(data(k)(26:30),'(f5.3)') sdrop
           if(radius.lt.10.0)
     *     write(data(k)(49:54),'(f6.4)') radius
           data(k)(80:80)='3'
           write(data(k+1),401)
     *     station(1:4),compon,hour,min,secc,
     *     kappa,int(geo_distance),
     *     spec_velocity,density,q0,qalpha,svelocity
401        format(' SPEC ',a4,a4,' T',3i2,' K ',f5.3,' GD',
     *     I5,' V ',f5.2,
     *     ' DE',f5.2,' Q0',f5.1,' QA',f5.2,' VS',f5.2)
           data(k+1)(80:80)='3'
           data(k+1)(41:41)=spec_phase
           if(geo_distance.lt.100.0) write(data(k+1)(34:38),'(f5.1)') 
     *     geo_distance
c
c   sum for average, do not use magnitude weighted out value
c   do not use unidentified spectra, must be p or s (new may 2001, jh)
c
           if(spec_phase.eq.'P'.or.spec_phase.eq.'S') then
              avmoment=avmoment+moment
              avsdrop=avsdrop+sdrop
              avomega0=avomega0+omega0
              avcornerf=avcornerf+cornerf
              avradius=avradius+radius
              avsslope=avsslope+sslope
              avswindow=avswindow+swindow
              if(mag_cor(5,kstat).ne.99.0) then
                avmw=avmw+mw
                nmw=nmw+1
              else
                write(6,'(4a)')' Moment magnitude not used in average ',
     *                    'since weighted out ',station(1:4),compon
              endif
              nspec=nspec+1
           else
              write(6,'(3a)')
     *        ' Unidentified spectrum, not used in average ',
     *                   station(1:4),compon
           endif
        endif
c
        k=k+1         ! do not use next spectral line 
        endif
      goto 10           ! get next spectral line
c
c   enter here when all headers checked
c
 20   continue
c
c   return if no spectral values
c
      if(nspec.gt.0) write(6,'(a,2i6)')
     *' Number of spectra available and number used in average',
     *nspec1,nspec
      if(nspec.eq.0) return
c
c   move all lines down one to make room for new average spectral line
c   if not there from before
c
      k=0
      do i=2,nhead
        if(data(i)(1:13).eq.' SPEC AVERAGE') k=i
      enddo
      if(k.eq.0) then
         do i=nrecord,2,-1
            data(i+1)=data(i)
         enddo
         k=2
c
         nrecord=nrecord+1
         nhead=nhead+1
      endif
c
c   calculate average values
c
      moment=avmoment/nspec
      sdrop=avsdrop/nspec
      omega0=avomega0/nspec
      cornerf=avcornerf/nspec
      radius=avradius/nspec
      sslope=avsslope/nspec
      swindow=avswindow/nspec
      if(nmw.gt.0) then
        mw=avmw/nspec
      else
        mw=9.9         ! indicate no mw
      endif
c
c   check for overflow
c
      if(moment.gt.999.0) moment=999.9
      if(sdrop.gt.999.0) sdrop=999.9
      if(omega0.gt.999.0) omega0=999.9
      if(abs(sslope).gt.999.0) sslope=999.9
      if(swindow.gt.999.9) swindow=999.9
      if(radius.gt.999.9) radius=999.99
      if(mw.gt.9.9.or.mw.lt.-9) mw=9.9
c
c   write values
c
      write(data(k),500)
     *moment,sdrop,omega0,cornerf,
     *radius,sslope,swindow,mw
      if(sdrop.lt.1.0)
     *write(data(k)(26:30),'(f5.3)') sdrop
      if(radius.lt.10.0)
     *write(data(k)(49:54),'(f6.4)') radius

500   format(' SPEC ','AVERAGE ',' MO',f5.1,' ST',f5.1,' OM',f5.1,
     *' f0',f5.2,' R',f6.2,' AL',f5.2,' WI',f5.1,' MW',f5.1)
      data(k)(80:80)='3'
      if(mw.eq.9.9) data(k)(75:78)=' '   ! no mw
c
c   update header, if a mw,, find which header and where there is a free spot
c
      if(mw.ne.9.9) then
          if(data(1)(60:60).eq.' ')then 
             write(data(1)(56:63),205) mw,agency
          elseif(data(1)(68:68).eq.' ')then 
             write(data(1)(64:71),205) mw,agency
          elseif(data(1)(76:76).eq.' ')then 
             write(data(1)(72:79),205) mw,agency
 205         format(f4.1,'W',a3)
          else
             mag_write=.false.
             do i=2,nhead             ! check if other headers can be used
                if(data(i)(80:80).eq.'1'. ! must have same loc. agency
     *          and.data(1)(46:48).eq.data(i)(46:48)) then
                  mag_write=.true.
                  if(data(i)(60:60).eq.' ')then 
                     write(data(i)(56:63),205) mw,agency
                  elseif(data(i)(68:68).eq.' ')then 
                     write(data(i)(64:71),205) mw,agency
                  elseif(data(i)(76:76).eq.' ')then 
                     write(data(i)(72:79),205) mw,agency
                  endif
                endif
             enddo
c
c   put in a new line with magnitudes
c
             if(.not. mag_write) then
                do k=nrecord,2,-1      ! move records one down 
                   data(k+1)=data(k)
                enddo
                data(2)=' '            ! new line with more magnitudes
                data(2)(46:48)=data(1)(46:48)    ! use same agency
                data(2)(1:23)=data(1)(1:23)      ! use same start 
                data(2)(80:80)='1'
                write(data(2)(56:63),205) mw,agency ! put in magnitude
                nhead=nhead+1
                nrecord=nrecord+1
             endif
          endif
      endif
c
      return
      end
