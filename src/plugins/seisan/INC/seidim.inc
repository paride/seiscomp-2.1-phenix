cJAB(BGS)Apr25 : waveform file header sizes...
c    Include file to set array dimensions for SEISAN system
c    jan 23, 95
c    dec 96        : new parameter for number of calibration files
c    nov 99        : remove bgs stuff
c    mar 29  00 jh : add dimensions for poles and zeros
c    may 12        : increased max_dirf to 2000
c    nov 7      lo : add cont parameters
c    mar 2   91 jh : max_dirf to 5000
c    mar 5   01 lo : max_trace to 500
c    oct 10     jh : max smaple to 600000
c    dec 6         : remove max_sample_dem
c    jun 23  03 jh : max_trace to 1000    
c
c-- max array dimenrsion for waveform file, one trace, must be a number which 
c   can be divided by 2 and 3
       integer max_sample 
       parameter(max_sample=600000)
c
c below commeneted out nov 3 99 jh
c-- Size of main header and trace headers in waveform file...
c      integer mainhead_c$ 
c    &        ,mainhead_x$                ! # mainheader cards.
c    &        ,tracehead_c$
c    &        ,trace_x$                   ! Maximum channels in wavefile.
c    &        ,stat_c$                    ! Station text length.
c    &        ,comp_c$                    ! Component text length.
c      parameter (mainhead_c$  = 80)
c      parameter (mainhead_x$  = 12)
c      parameter (tracehead_c$ = 1040)
c      parameter (trace_x$     = 32)
c      parameter (stat_c$      = 4)
c      parameter (comp_c$      = 4)
c
c-- max number of phases for one station or to be defined generally
       integer max_phase
       parameter(max_phase=120)
c-- max number of lines in a nordic file
       integer max_data
       parameter(max_data=4000)
c-- maximum number of trace headers = maximum number of channels = 
c   max number of wav files
       integer max_trace
       parameter(max_trace=1000)     
c-- maximum number of epicenters
       integer max_epi
       parameter(max_epi=90000)
c-- maximum number points in a polygon...
       integer max_polyn$
       parameter (max_polyn$=100)
c-- maximum number of events for eev or in one index file
       integer max_index$
       parameter (max_index$=9999)
c-- maximum number of files for dirf, must be less than 1000
       integer max_dirf
       parameter (max_dirf=5000)
c-- maximum number of calibration files
       integer max_cal_files$
       parameter (max_cal_files$=1500)
c-- maximum number of poles and zeros, and frequency amplitude values
       integer max_resp_value
       parameter (max_resp_value=2000)
