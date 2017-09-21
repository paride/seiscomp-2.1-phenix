c
c    include file and common block for parameter data for one event
c
c    updates
c
c  may 26 2001 jh : add hyp_mag_all,hyp_mag_agency_all,hyp_type_all
c

c
c  general parameters
c
      integer      rea_nstat                  ! number of stations
      integer      rea_nphase                 ! number of phases lines
      integer      rea_nhead                  ! number of header lines
      integer      rea_nrecord                ! number of records
      integer      rea_nspec                  ! number of spectra 
      integer      rea_nhyp                   ! number of hypocenters
      integer      rea_nmag                   ! number of magnitudes
      integer      rea_nmacro                 ! number macroseismic lines
      integer      rea_nwav                   ! number of waveform files
      integer      rea_ncomment               ! number of comment lines
      integer      rea_nfault                 ! number of fault plane solutions
      character*80 rea_macro(100)             ! macroseismic solutions
      character*80 rea_wav(100)               ! waveform file names lines
      character*80 rea_fault(100)             ! fault plane solutions lines
      character*80 rea_comment(100)           ! comment lines
      character*80 rea_id_line                ! event id line
c
c   hypocentral parameters, index 1 is the prime solution in first
c   line
c
      integer     hyp_year(100)              ! hypocenter year
      integer     hyp_month(100)
      integer     hyp_day(100)
      integer     hyp_hour(100)
      integer     hyp_min(100)
      real        hyp_sec(100)
      character*1 hyp_model(100)             ! model indicator
      character*1 hyp_dist_id(100)           ! distance indicator
      character*1 hyp_type(100)              ! event type like E
      character*1 hyp_fix_org(100)           ! fix origin time flag
      real        hyp_lat(100)               ! latitude
      real        hyp_lon(100)               ! longitude
      real        hyp_depth(100)             ! depth
      character*1 hyp_depth_flag(100)        ! depth flag
      character*1 hyp_epi_flag(100)          ! epicenter flag
      character*5 hyp_agency(100)            ! hypocenter agency, use 3 only
      integer     hyp_nstat(100)             ! number of station
      real        hyp_rms(100)               ! rms of hypocenter solution
      real        hyp_mag(6,100)             ! magnitudes
      real        hyp_mag_all(200)           ! all magnitudes, no hyp. assco.
      character*1 hyp_mag_type(6,100)        ! magnitude types
      character*1 hyp_mag_type_all(200)      ! all -----------
      character*5 hyp_mag_agency(6,100)      ! magnitude agencies
      character*5 hyp_mag_agency_all(200)    ! all
      logical     hyp_high_accuracy(100)     ! high accurcy flag
      logical     hyp_error(100)             ! true if hypocenter has an error
      character*20 hyp_auto(100)             ! name of auto process for parameter
c
c   hypocenter errors
c
      real        hyp_gap(100)               ! gap, degrees
      real        hyp_sec_err(100)           ! oriign time error (sec)
      real        hyp_lat_err(100)           ! latitude error (km)
      real        hyp_lon_err(100)           ! longitude error (km)
      real        hyp_depth_err(100)         ! depth error (km)
      real        hyp_cov(3,100)             ! covariance, xy,xz,yz (kmXkm)
c
c  phase line parameters and associated parameters, all start with rea
c  and there can be nphase different parameters of each
c
      character*5 rea_stat(max_data/2)       ! station codes
      character*4 rea_comp(max_data/2)       ! componenets
      character*2 rea_co(max_data/2)         ! 2 letter componenets
      character*8 rea_phase(max_data/2)      ! phase name
      character*1 rea_onset(max_data/2)      ! onset I or E or blank
      character*1 rea_weight_in(max_data/2)  ! input weight
      character*2 rea_weight_out(max_data/2) ! weight out
      character*1 rea_polarity(max_data/2)   ! polarity, D or C
      integer     rea_year(max_data/2)
      integer     rea_month(max_data/2)
      integer     rea_day(max_data/2)
      integer     rea_hour(max_data/2)
      integer     rea_min(max_data/2)
      real        rea_sec(max_data/2)
      real*8      rea_abs_time(max_data/2)   ! abs time of phase time
      real        rea_coda(max_data/2)       ! coda length in s
      real        rea_amp(max_data/2)        ! amplitude in nm
      real        rea_per(max_data/2)        ! period of amplitude
      real        rea_baz_obs(max_data/2)    ! observed back azimuth
      real        rea_baz_cal(max_data/2)    ! calculated back azimuth
      real        rea_vel(max_data/2)        ! observed apparent velocity
      real        rea_sn(max_data/2)         ! observed signal to noise ratio
      real        rea_baz_res(max_data/2)    ! back azimuth residual
      real        rea_res(max_data/2)        ! travel time residual
      real        rea_dist(max_data/2)       ! epicentral distance
      real        rea_az(max_data/2)         ! azimuth
c
c   spectral parameters, time is give in parameter above, spectra
c   have phase name SPECP or SPECS and are thus considered phases
c   although not written as such in s-file. the avrge spectral values is
c   not considered a phase
c
      real        rea_moment(max_data/2)     ! log moment, Nm
      real        rea_sdrop(max_data/2)      ! stress drop, bar
      real        rea_omega0(max_data/2)     ! log spectral flat level, ns
      real        rea_cornerf(max_data/2)    ! corner f
      real        rea_radius(max_data/2)     ! source radius
      real        rea_swin(max_data/2)       ! window lenght used
      real        rea_vs(max_data/2)         ! s-velocity at source, km/s
      real        rea_vp(max_data/2)         ! p-velocity at source, km/s
      real        rea_q0(max_data/2)         ! q0
      real        rea_qalpha(max_data/2)     ! q alpha
      real        rea_kappa(max_data/2)      ! kappa
      real        rea_density(max_data/2)    ! density g/cm**3
      real        rea_slope(max_data/2)      ! - measured slope of spectrum
      real        rea_geo_dist(max_data/2)   ! geo distance
c
c   same as above, the but averages, only one value
c
      real        rea_av_moment              ! log moment, Nm
      real        rea_av_sdrop               ! stress drop, bar
      real        rea_av_omega0              ! log spectral flat level, ns
      real        rea_av_cornerf             ! corner f
      real        rea_av_radius              ! source radius
      real        rea_av_swin                ! window lenght used
      real        rea_av_mw                  ! moment mag
      real        rea_av_slope               ! slope
c
      character*20 rea_auto(max_data/2)      ! name of auto process making par

c   magnitudes
c
      real        rea_mc(max_data/2)         ! coda
      real        rea_ml(max_data/2)         ! local
      real        rea_mb(max_data/2)         ! mb
      real        rea_ms(max_data/2)         ! ms
      real        rea_mw(max_data/2)         ! mw

c
c   fixed parameters for spectral analysis and others parameters
c
      integer     par_nstat      ! number of stations with paremeters
      real        par_vs         ! s-velocity at source, km/s
      real        par_vp         ! p-velocity at source, km/s
      real        par_q0         ! q0, general for p and s if not given below
      real        par_qalpha     ! q alpha ----------------------------------
      real        par_kappa      ! kappa   ----------------------------------
      real        par_density    ! density at source g/cm**3
c
      real        par_eventype   ! determine event type (L or D) (0 or 1)
      real        par_coda       ! determine coda length if a P-phase (0 or 1)
      real        par_flow       ! filt low for spec, general
      real        par_flow_pole  ! number of poles 
      real        par_fhigh      ! filt high-----------------------------
      real        par_fhigh_pole ! number of poles ----------------------
c
c   p-phase processing
c
      real        par_pick_p       ! pick p-phases (0: no, 1 yes)
      real        par_mb_amp       ! determine mb amp if a P-phase (0 or 1)
      real        par_mb_flow      ! low filter for mb-amp
      real        par_mb_flow_pole ! number of poles for  mb low filter
      real        par_mb_fhigh     ! high filter for mb
      real        par_mb_fhigh_pole! number of poles for mb high filter
      real        par_mb_amp_sn    ! minimum s/n for mb amp
      real        par_pspec        ! do p-spectrum if P-phase (0 or 1)
      real        par_pspec_sn     ! minimum s/n for p-spec
      real        par_p_q0         ! Q0 for P, overrides par_q0 if set
      real        par_p_qalpha     ! qalpha -------------par_qalpha ---
      real        par_p_kappa      ! kappa --------------par_kappa----
      real        par_p_flow       ! filt low for spec., overrides par_flow
      real        par_p_flow_pole  ! number of poles ----------------------
      real        par_p_fhigh      ! filt high-----------------------------
      real        par_p_fhigh_pole ! number of poles ----------------------
c
c   s-phase processing
c
      real        par_pick_s       ! pick s or not (0,1)
      real        par_ml_amp       ! determine ml amp if a P-phase, or if
                                   ! origin time and P-phase (0 or 1)
      real        par_ml_flow      ! low filter for ml-amp
      real        par_ml_flow_pole ! number of poles for  ml low filter
      real        par_ml_fhigh     ! high filter for ml
      real        par_ml_fhigh_pole! number of poles for ml high filter
      real        par_ml_amp_sn    ! minimum s/n for ml amp
      real        par_sspec        ! do s-spectrum if S-phase, or P-phase
                                   ! and origin time (0 or 1)
      real        par_sspec_sn     ! minimum s/n for S-spec
      real        par_s_q0         ! Q0 for S, overrides par_q0 if set
      real        par_s_qalpha     ! qalpha -------------par_qalpha ---
      real        par_s_kappa      ! kappa --------------par_kappa----
      real        par_s_flow       ! filt low for spec., overrides par_flow
      real        par_s_flow_pole  ! number of poles ----------------------
      real        par_s_fhigh      ! filt high-----------------------------
      real        par_s_fhigh_pole ! number of poles ----------------------
c
c   station dependent parameters
c
      character*5 par_stat(100)    ! station to select
      character*4 par_comp(100)    ! component toselect
      real        par_sta(100)     ! short term average
      real        par_lta(100)     ! long term average
      real        par_ratio(100)   ! trigger ratio
      real        par_mincoda(100) ! minimum coda to select trace
      real        par_dtrle(100)   ! 
      real        par_fill(100)    ! filter low
      real        par_filh(100)    ! filter high
c
c
c 
      common /hyp1/hyp_model,hyp_dist_id,hyp_type,hyp_fix_org,
     *             hyp_depth_flag,hyp_epi_flag,hyp_mag_type,
     *             hyp_high_accuracy,hyp_error,hyp_mag_type_all
      common /hyp4/hyp_year,hyp_month,hyp_day,hyp_hour,
     *             hyp_min,hyp_sec,hyp_lat,hyp_lon,hyp_depth,
     *             hyp_nstat,hyp_rms,hyp_mag,hyp_mag_all,
     *             hyp_gap,hyp_sec_err,hyp_lat_err,hyp_lon_err,
     *             hyp_depth_err,hyp_cov,hyp_auto
      common /hyp5/hyp_agency,hyp_mag_agency,hyp_mag_agency_all
c
c 
      common /rea1/rea_weight_in,rea_onset,rea_polarity
      common /rea2/rea_co,rea_weight_out
      common /rea4/rea_comp,rea_hour,rea_min,rea_sec,rea_coda,
     *             rea_amp,rea_per,rea_baz_obs,rea_baz_cal,
     *             rea_vel,rea_sn,rea_baz_res,rea_dist,rea_az,
     *             rea_nstat,rea_nphase,rea_res,rea_year,rea_month,
     *             rea_day,rea_moment,rea_nmag,
     *             rea_sdrop,rea_omega0,rea_cornerf,rea_radius,
     *             rea_swin,rea_vs,rea_vp,
     *             rea_q0,rea_qalpha,rea_kappa,rea_density,rea_slope,
     *             rea_mc,rea_ml,rea_mb,rea_ms,rea_mw,rea_geo_dist,
     *             rea_nhead,rea_nrecord,rea_nspec,rea_nhyp,
     *             rea_id_line,rea_nmacro,rea_nwav,rea_nfault,
     *             rea_ncomment,
     *             rea_macro,rea_wav,rea_fault,rea_comment,
     *             rea_av_moment,rea_av_sdrop,rea_av_omega0,
     *             rea_av_cornerf,rea_av_radius,rea_av_swin,
     *             rea_av_mw,rea_av_slope,rea_auto 

c
c
      common /rea5/rea_stat
      common /rea8/rea_phase,rea_abs_time
      common /par4/par_nstat,par_vp,par_vs,par_q0,par_qalpha,par_kappa,
     *             par_density,par_comp,par_sta,par_lta,par_ratio,
     *             par_mincoda,par_dtrle,par_fill,par_filh,
     *             par_pick_p,par_mb_amp,par_mb_flow,par_mb_flow_pole,
     *             par_mb_fhigh,par_mb_fhigh_pole,par_mb_amp_sn,
     *             par_pspec,par_pspec_sn,par_p_q0,par_p_qalpha,
     *             par_p_kappa,par_p_flow,par_p_flow_pole,par_p_fhigh,
     *             par_p_fhigh_pole,
     *             par_ml_amp,par_ml_flow,par_ml_flow_pole,par_ml_fhigh,
     *             par_ml_fhigh_pole,par_ml_amp_sn,par_sspec,
     *             par_sspec_sn,par_s_q0,par_s_qalpha,par_s_kappa,
     *             par_s_flow,par_s_flow_pole,par_s_fhigh,
     *             par_s_fhigh_pole,par_pick_s,
     *             par_eventype,par_coda,par_flow,par_flow_pole,
     *             par_fhigh,par_fhigh_pole
     
      
      common /par5/par_stat

