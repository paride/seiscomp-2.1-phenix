c
c   definitions and common blocks for seisan general plotting routines
c
CJAB(BGS)Jan95   : Parametrize PostScript translations and scales
CJAB(BGS)Jan95   : Increase the # objects.
c   last updata. Dec 94
c   dec 23 pc sun
c   nov 6, 96 by jh   : add color_syn
c
c--------------------------------------------------------------------
c
      REAL       SCREEN_SIZEX$,                 ! Screen width (Tektronix).
     &           SCREEN_SIZEY$                  ! & height.
      PARAMETER (SCREEN_SIZEX$ = 1023.0)        ! & values.
      PARAMETER (SCREEN_SIZEY$ = 780.0)         !
C
      REAL       PLOT_TRANSX$,                  ! Translation in x.
     &           PLOT_TRANSY$,                  ! & in y.
     &           PLOT_SCALEX$,                  ! & scaling in x.
     &           PLOT_SCALEY$                   ! & in y.
      PARAMETER (PLOT_TRANSX$ = 15.0)           ! & values.
      PARAMETER (PLOT_TRANSY$ = 13.0)           !
c taken out: lo
c      PARAMETER (PLOT_SCALEX$ = 0.55)           !
c      PARAMETER (PLOT_SCALEY$ = 1.0)            !
C
C    To control spacing allocation when using non-proportional
C    fonts (eg. Times-Roman)                   
C
      LOGICAL    B_PROP_FONT$,                  ! Proportinal font used?
     &           B_PROP_PSFONT$,                ! And for PostScript?.
     &           B_FORCE_SPACE$                 ! Force a double spacing?.
      PARAMETER (B_PROP_FONT$   = .TRUE.)       ! Not using eg. times-roman.
      PARAMETER (B_PROP_PSFONT$ = .FALSE.)      ! Using eg. times roman.
      COMMON /FORCE_SPACE$/ B_FORCE_SPACE$      ! & saved.
c 
c--------------------------------------------------------------------  
      integer plotoption             !  0: only tek screen plot                                         
c                                       1: screen + hc file                                             
c                                       2: only hc file                                                 
c
      integer plotunit               ! file unit for hard copy file
c
      integer hctype                 ! 0: tektronics file                                              
c                                      1: postscript

      integer display_type           ! 0: tektronics
c
      integer disp_open              ! 0: not open, i: open
c
      integer wsize                  ! display size in picsels, x only
      integer from_input_box         ! 1. call originates from 
c                                         input_box routine, 0: it does not
      integer xcolour_n$             ! # colours
      integer white$,blue$,yellow$,  ! colours.
     &        red$,green$,black$     !
      parameter (blue$      = 1)     ! & colour values.
      parameter (green$     = 2)     !
      parameter (red$       = 3)     !
      parameter (yellow$    = 4)     !
      parameter (white$     = 5)     !
      parameter (black$     = 6)     !.
      parameter (xcolour_n$ = 6)     ! & # colours installed.
c
      integer xwhite,xblue,xyellow,xred,xgreen,xblack   ! colors

c
c  colors
c
       integer color_screen          ! 0: no colors, 1: colors
       integer color_hard_copy       ! 0: --------------------
       integer color_current         ! current color
       integer color_back            ! background color
       integer color_trace           ! seismic traces, map contours
       integer color_pic             ! phase picking
       integer color_syn             ! synthetic phases 
       integer color_zoom            ! zoom lines in mulplt
       integer color_def             ! default color
       integer color_frame           ! frames lik emap frames
       integer color_title           ! titles
       integer color_spec            ! spectras
       integer color_axis_not        ! axis notations
       integer color_epi             ! epicenters
       integer color_station         ! seismic stations
       integer color_map_contour     ! epimap contours
       integer color_map_grid        ! Lat-long/(x,y) grid
       integer color_label_grid      ! Grid labels for map
       integer color_symbol_key      ! Diagram key.
       integer color_prompt          ! Prompt text.
       integer color_section         ! section outline in epimap
       integer color_bval_np         ! bvalue, number of events
       integer color_bval_ac         ! ------, accumulated ---
       integer color_bval_line       ! ------, lsq line
       integer color_box             ! background color in dialog box
       integer color_box_letter      ! letters in dialog box
       integer color_foc_dilat       ! focmec dilatation
       integer color_foc_comp        ! focmec compression
       integer color_foc_p           ! focmec P-axis
       integer color_foc_t           ! focmec T-axis
       integer color_foc_plane       ! focmec fault planes

      common /tekout/plotoption,plotunit,hctype,display_type,
     *       disp_open,xwhite,xblue,xyellow,xred,xgreen,
     *       xblack,wsize,color_back,color_current,
     *       color_box,color_box_letter,color_zoom,
     *       color_screen,color_hard_copy,from_input_box
c
c   colors of objects
c
       common /mul_color/color_trace,color_pic,color_def,color_frame,
     * color_title,color_spec,color_axis_not,color_epi,color_station,
     * color_map_contour,color_section,color_bval_np,color_bval_ac,
     * color_bval_line,color_foc_dilat,color_foc_comp,color_foc_p,
     * color_foc_t,color_foc_plane,color_syn,
     * color_map_grid,color_symbol_key,color_prompt,color_label_grid

c
c  just to avoid warning
c
       common /junk_seiplot/PLOT_SCALEX$,PLOT_SCALEY$


