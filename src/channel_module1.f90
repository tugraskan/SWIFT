      module channel_module1
    
      implicit none
      
      type channel_data1
        character(len=16) :: name
        integer :: order
        real :: chw = 0.        !m          |channel width
        real :: chd = 0.        !m          |channel depth
        real :: chs = 0.        !m/m        |channel slope
        real :: chl = 0.        !km         |channel length
        real :: chn = 0.        !           |channel Manning's n
        real :: chk = 0.        !mm/h       |channel bottom conductivity
        real :: cherod = 0.     !           |channel erodibility
        real :: cov = 0.        !0-4500     |channel tree cohesion factor
        real :: sinu            !none       |sinuousity - ratio of channel length and straight line length
        real :: chseq = 0.      !m/m        |equilibrium channel slope
        real :: d50 = 0.        !mm         |channel median sediment size
        real :: ch_clay = 0.    !%          |clay percent of bank and bed
        real :: carbon = 0.     !%          |cabon percent of bank and bed
        real :: ch_bd = 0.      !t/m3       |dry bulk density
        real :: chss = 0.       !           |channel side slope
        real :: d50_sub = 20.   !mm         |d50 of the sub bottom material
        real :: fps = 0.000001  !m/m        |flood plain slope
        real :: fpn = 0.1       !           |flood plain Manning's n
        real :: n_conc = 0.     !mg/kg      |nitrogen concentration in channel bank
        real :: p_conc = 0.     !mg/kg      |phosphorus concentration in channel bank
        real :: p_bio = 0.      !frac       |fraction of p in bank that is bioavailable
      end type channel_data1
      
      type channel_sediment_data
        character(len=16) :: name
        real :: q1_qm = 3.          !ratio      |ratio of 1% flow to mean flow
        real :: rcurv_exp = 1.2     !           |exponent coefficient for sediment rating curve adjustment for overbank concentration
        real :: fp_inun_days = 2.   !days       |days of flood plain inundation per year
        real :: fp_flo_fr = 0.02    !frac       |fraction of annual flow that goes overbank
        real :: wd_yr = 0.3         !frac       |widths per year of channel widening
        real :: ch_fp_w_rto = 3.    !ratio      |channel/flood plain width ratio
        real :: ch_nsol_part = 0.01 !frac       |instream nitrogen soluble to particulate transformation coefficient
        real :: ch_psol_part = 0.01 !frac       |instream phosphorus soluble to particulate transformation coefficient
        real :: n_dep_fr = 0.50     !frac       |fraction of nitrogen deposition during sediment deposition 
        real :: p_dep_fr = 0.50     !frac       |fraction of nitrogen deposition during sediment deposition 
      end type channel_sediment_data
      
      type channel1
        character(len=25) :: name = ""
        integer :: obj_no
        real :: area_ha
        real :: km
        integer :: dr_db
        type (channel_data1) :: dat
        type (channel_sediment_data) :: sed
      end type channel1
      type (channel1), dimension(:), allocatable :: cha1
 
      type calibration_by_stream_order
        integer :: num_cha = 0      !       |number of channels in the stream orders
        real :: sum_wyr = 0.        !       |sum of widths per year 
        real :: sum_fpdep = 0.      !mm     |flood plain deposition
        real :: wyr_mean = 0.       !       |mean widths per year
        real :: fpdep_mean = 0.     !mm     |mean flood plain deposition
        real :: fparea = 0.         !mm     |mean flood plain area ***tu        
      end type calibration_by_stream_order
      type (calibration_by_stream_order) :: cal_bso !***tu
      
      type calibration_gage_data
        character(len=25) :: name = ""
        integer :: cha_num = 0      !       |channel number at gage site
        real :: sum_wyr = 0.        !       |sum of widths per year 
        real :: sed_t = 0.          !tons   |average annual sediment wash load at the gage
      end type calibration_gage_data
      type (calibration_gage_data) :: cal_out 
      
      type calibration_output
        type (calibration_by_stream_order), dimension(:), allocatable :: so1_3
        type (calibration_by_stream_order), dimension(:), allocatable :: so4_6
        type (calibration_by_stream_order), dimension(:), allocatable :: so7_9s
        real :: up_cha_rto = 0.     !       |ratio of upland sediment delivered to channel erosion ratio 
        real :: res_trap = 0.       !       |mean reservoir trap efficiency
        type (calibration_gage_data), dimension(:), allocatable :: gage
      end type calibration_output
      
 
      type sd_ch_output
        real :: flo_in = 0.             !(m^3/s)       |average daily inflow rate during time step
        real :: aqu_in = 0.             !(m^3/s)       |aveerage daily aquifer inflow rate during timestep
        real :: flo = 0.                !(m^3/s)       |average daily outflow rate during timestep
        real :: peakr = 0.              !(m^3/s)       |average peak runoff rate during timestep
        real :: washld_in = 0.          !(tons)        |wash load (suspended) in
        real :: bedld_in = 0.           !(tons)        |bed load in
        real :: washld_out = 0.         !(tons)        |wash load (suspended) out
        real :: bedld_out = 0.          !(tons)        |bed load out
        real :: dep = 0.                !(tons)        |deposition in channel and flood plain
        real :: deg_btm = 0.            !(tons)        |erosion of channel bottom 
        real :: deg_bank = 0.           !(tons)        |erosion of channel bank
        real :: hc_sed = 0.             !(tons)        |erosion from gully head cut
        real :: width = 0.              !m             |channel bank full top width at end of time step
        real :: depth = 0.              !m             |channel bank full depth at end of time step
        real :: slope = 0.              !m/m           |channel slope
        real :: deg_btm_m = 0.          !(m)           !downcutting of channel bottom
        real :: deg_bank_m = 0.         !(m)           |widening of channel banks
        real :: hc_m = 0.               !(m)           |headcut retreat
        real :: flo_in_mm = 0.          !(mm)          |inflow rate total sum for each time step
        real :: aqu_in_mm = 0.          !(mm)          |aquifer inflow rate total sum for each time step
        real :: flo_mm = 0.             !(mm)          |outflow rate total sum for each time step
        real :: sed_stor = 0.           !(tons)        |sed storage at end of timestep 
      end type sd_ch_output
      
      type (sd_ch_output), dimension(:), allocatable, save :: chsd_d
      type (sd_ch_output), dimension(:), allocatable, save :: chsd_m
      type (sd_ch_output), dimension(:), allocatable, save :: chsd_y
      type (sd_ch_output), dimension(:), allocatable, save :: chsd_a
      type (sd_ch_output), dimension(:), allocatable, save :: schsd_d
      type (sd_ch_output), dimension(:), allocatable, save :: schsd_m
      type (sd_ch_output), dimension(:), allocatable, save :: schsd_y
      type (sd_ch_output), dimension(:), allocatable, save :: schsd_a
      type (sd_ch_output) :: bchsd_d
      type (sd_ch_output) :: bchsd_m
      type (sd_ch_output) :: bchsd_y
      type (sd_ch_output) :: bchsd_a
      type (sd_ch_output) :: chsdz
            
      type cha_header      
          character(len=16) :: flo        =  " flow           "         ! (m^3)
          character(len=16) :: sed        =  " sediment       "         ! (tons)
          character(len=16) :: orgn       =  " organic_n      "         ! (kg)          
          character(len=16) :: orgp       =  " organic_p      "         ! (kg)         
          character(len=16) :: no3        =  " nitrate_no3    "         ! (kg)
          character(len=16) :: minp       =  " soluble_minp   "         ! (kg)          
          character(len=16) :: nh3        =  " bedload        "         ! (tons)
          character(len=16) :: no2        =  " null           "         ! (kg)
      end type cha_header
      type (cha_header) :: hdr_cha
      
     type cha_header_units        
          character(len=16) :: flo        =  " m^3            "         ! (m^3)
          character(len=16) :: sed        =  " tons           "         ! (tons)
          character(len=16) :: orgn       =  " kg             "         ! (kg)
          character(len=16) :: orgp       =  " kg             "         ! (kg)
          character(len=16) :: no3        =  " kg             "         ! (kg)       
          character(len=16) :: minp       =  " kg             "         ! (kg)
          character(len=16) :: nh3        =  " tons           "         ! (tons)    
          character(len=16) :: no2        =  " null           "         ! (kg)
      end type cha_header_units
      type (cha_header_units) :: hdr_cha_unt
      
      type cha_bnk_header
        character (len=12) :: cha         =   "      cha   "        
        character (len=25) :: name        =   "name                     "                                             
        character (len=12) :: area        =   "       area"
        character (len=15) :: width       =   "         width"
		character (len=15) :: length      =   "        length"
		character (len=15) :: bulk_den    =   "      bulk_den"
		character (len=15) :: b_exp       =   "         b_exp"
		character (len=15) :: clay        =   "          clay"
		character (len=15) :: sinous      =   "        sinous"
		character (len=15) :: vel_tot     =   "       vel_tot"
		character (len=15) :: vel_cr      =   "        vel_cr"
        character (len=15) :: cohesion    =   "      cohesion"
		character (len=15) :: edays       =   "         edays"
		character (len=15) :: rad_curv    =   "      rad_curv"
		character (len=15) :: arc_len     =   "       arc_len"
		character (len=15) :: ebank       =   "         ebank"
		character (len=15) :: ebank1      =   "         ebank"
		character (len=15) :: w_yr        =   "          w/yr"
		!character (len=15) :: washin      =   "        washin"
		!character (len=15) :: bedin       =   "         bedin"
        !character (len=15) :: washout     =   "       washout"
        !character (len=15) :: bedout      =   "        bedout"
      end type cha_bnk_header    
      type (cha_bnk_header) :: hdr_cha_bnk 
      
      type cha_bnk_unt_header          
        character (len=12) :: cha         =   "       --   "      
        character (len=25) :: name        =   " --                      "       
        character (len=12) :: area        =   "        ha  "
        character (len=15) :: width       =   "           m  "
		character (len=15) :: length      =   "          km  "
		character (len=15) :: bulk_den    =   "        t/m3  "
		character (len=15) :: b_exp       =   "          --  "
		character (len=15) :: clay        =   "           %  "
		character (len=15) :: sinous      =   "          --  "
		character (len=15) :: vel_tot     =   "         m/s  "
		character (len=15) :: vel_cr      =   "         m/s  "
        character (len=15) :: cohesion    =   "          Pa  "
		character (len=15) :: edays       =   "        days  "
		character (len=15) :: rad_curv    =   "           m  "
		character (len=15) :: arc_len     =   "           m  "
		character (len=15) :: ebank       =   "           t  "
		character (len=15) :: ebank1      =   "           m  "
		character (len=15) :: w_yr        =   "          --  "
		!character (len=15) :: washin      =   "           t  "
		!character (len=15) :: bedin       =   "           t  "
        !character (len=15) :: washout     =   "           t  "
        !character (len=15) :: bedout      =   "           t  "
      end type cha_bnk_unt_header    
      type (cha_bnk_unt_header) :: hdr_cha_bnk_unt 
      
      type cha_bed_header
        character (len=12) :: cha         =   "      cha   "        
        character (len=25) :: name        =   "name                     "                                             
        character (len=12) :: area        =   "       area"
        character (len=15) :: width       =   "         width"
        character (len=15) :: depth       =   "         depth"
		character (len=15) :: length      =   "        length"
		character (len=15) :: d50         =   "           d50"
		character (len=15) :: q           =   "          q"
		character (len=15) :: str_pow     =   "       str_pow"
		character (len=15) :: strpow_cr   =   "     strpow_cr"
        character (len=15) :: sedcap_ppm  =   "   sed_cap_ppm"
        character (len=15) :: sedcap_t    =   "     sed_cap_t"
		character (len=15) :: vel_tot     =   "       vel_tot"
		character (len=15) :: vel_cr      =   "        vel_cr"
		character (len=15) :: ebed_t      =   "       e_bed_t"
        character (len=15) :: ebed_m      =   "       e_bed_m"
        character (len=15) :: d_yr        =   "          d_yr"
		!character (len=15) :: washin      =   "        washin"
		!character (len=15) :: bedin       =   "         bedin"
        !character (len=15) :: washout     =   "       washout"
        !character (len=15) :: bedout      =   "        bedout"
      end type cha_bed_header    
      type (cha_bed_header) :: hdr_cha_bed 
      
      type cha_bed_unt_header         
        character (len=12) :: cha         =   "       --   "      
        character (len=25) :: name        =   " --                      "                                      
        character (len=12) :: area        =   "        ha  "
        character (len=15) :: width       =   "           m   "
        character (len=15) :: depth       =   "           m   "
		character (len=15) :: length      =   "          km   "       
		character (len=15) :: d50         =   "           m   "
		character (len=15) :: q           =   "        m3/s   "
		character (len=15) :: str_pow     =   "        W/m2   "
		character (len=15) :: strpow_cr   =   "        W/m2   "
        character (len=15) :: sedcap_ppm  =   "         ppm   "
        character (len=15) :: sedcap_t    =   "           t   "
		character (len=15) :: vel_tot     =   "         m/s   "
		character (len=15) :: vel_cr      =   "         m/s   "
		character (len=15) :: ebed_t      =   "           t   "
        character (len=15) :: ebed_m      =   "           m   "
        character (len=15) :: d_yr        =   "          --   "
		!character (len=15) :: washin      =   "           t  "
		!character (len=15) :: bedin       =   "           t  "
        !character (len=15) :: washout     =   "           t  "
        !character (len=15) :: bedout      =   "           t  "
      end type cha_bed_unt_header    
      type (cha_bed_unt_header) :: hdr_cha_bed_unt
      
      type cha_fp_header
        character (len=12) :: cha         =   "      cha   "        
        character (len=25) :: name        =   "name                     "                                             
        character (len=12) :: area        =   "       area"
        character (len=15) :: q1_qm       =   "         q1/qm"
        character (len=15) :: rcur_exp    =   "      rcur_dep"
		character (len=15) :: fp_inun_d   =   "     fp_inun_d"
		character (len=15) :: fp_flo_fr   =   "     fp_flo_fr"
		character (len=15) :: fp_sed_con  =   "    fp_sed_con"
		character (len=15) :: trap_eff    =   "      trap_eff"
		character (len=15) :: depo_fr     =   "       depo_fr"
        character (len=15) :: fp_width    =   "      fp_width"
        character (len=15) :: sed_in      =   "        sed_in"
		character (len=15) :: fp_sed_t    =   "      fp_sed_t"
		character (len=15) :: fp_sed_m    =   "      fp_sed_m"
      end type cha_fp_header    
      type (cha_fp_header) :: hdr_cha_fp 
      
      type cha_fp_header_unt         
        character (len=12) :: cha         =   "       --   "      
        character (len=25) :: name        =   " --                      "                                    
        character (len=12) :: area        =   "        ha  "      
        character (len=15) :: q1_qm       =   "         --   "
        character (len=15) :: rcur_exp    =   "         --   "
		character (len=15) :: fp_inun_d   =   "       days   "
		character (len=15) :: fp_flo_fr   =   "         --   "
		character (len=15) :: fp_sed_con  =   "        ppm   "
		character (len=15) :: trap_eff    =   "         --   "
		character (len=15) :: depo_fr     =   "         --   "
        character (len=15) :: fp_width    =   "         km   "
        character (len=15) :: sed_in      =   "          t   "
		character (len=15) :: fp_sed_t    =   "          t   "
		character (len=15) :: fp_sed_m    =   "         mm   "
      end type cha_fp_header_unt    
      type (cha_fp_header_unt) :: hdr_cha_fp_unt
     
      interface operator (+)
        module procedure chsd_add
      end interface
      
      interface operator (/)
        module procedure chsd_div
      end interface
              
      interface operator (//)
        module procedure chsd_ave
      end interface
        
      interface operator (*)
        module procedure chsd_mult
      end interface 

      contains
!! routines for swatdeg_hru module

      function chsd_add(cho1,cho2) result (cho3)
      type (sd_ch_output),  intent (in) :: cho1
      type (sd_ch_output),  intent (in) :: cho2
      type (sd_ch_output) :: cho3
       cho3%flo_in = cho1%flo_in + cho2%flo_in
       cho3%aqu_in = cho1%aqu_in + cho2%aqu_in
       cho3%flo = cho1%flo + cho2%flo
       cho3%peakr = cho1%peakr + cho2%peakr
       cho3%washld_in = cho1%washld_in + cho2%washld_in
       cho3%bedld_in = cho1%bedld_in + cho2%bedld_in
       cho3%washld_out = cho1%washld_out + cho2%washld_out
       cho3%bedld_out = cho1%bedld_out + cho2%bedld_out
       cho3%dep = cho1%dep + cho2%dep
       cho3%deg_btm = cho1%deg_btm + cho2%deg_btm
       cho3%deg_bank = cho1%deg_bank + cho2%deg_bank
       cho3%hc_sed = cho1%hc_sed + cho2%hc_sed
       cho3%width = cho1%width + cho2%width
       cho3%depth = cho1%depth + cho2%depth
       cho3%slope = cho1%slope + cho2%slope
       cho3%deg_btm_m = cho1%deg_btm_m + cho2%deg_btm_m
       cho3%deg_bank_m = cho1%deg_bank_m + cho2%deg_bank_m
       cho3%hc_m = cho1%hc_m + cho2%hc_m
       cho3%flo_in_mm = cho1%flo_in_mm + cho2%flo_in_mm
       cho3%aqu_in_mm = cho1%aqu_in_mm + cho2%aqu_in_mm
       cho3%flo_mm = cho1%flo_mm + cho2%flo_mm
       cho3%sed_stor = cho1%sed_stor + cho2%sed_stor
      end function
      
      function chsd_div (ch1,const) result (ch2)
        type (sd_ch_output), intent (in) :: ch1
        real, intent (in) :: const
        type (sd_ch_output) :: ch2
        ch2%flo_in = ch1%flo_in
        ch2%aqu_in = ch1%aqu_in
        ch2%flo = ch1%flo
        ch2%peakr = ch1%peakr
        ch2%washld_in = ch1%washld_in / const
        ch2%washld_out = ch1%washld_out / const
        ch2%bedld_in = ch1%bedld_in / const
        ch2%bedld_out = ch1%bedld_out / const
        ch2%dep = ch1%dep / const
        ch2%deg_btm = ch1%deg_btm / const
        ch2%deg_bank = ch1%deg_bank / const
        ch2%hc_sed = ch1%hc_sed / const
        ch2%width = ch1%width
        ch2%depth = ch1%depth
        ch2%slope = ch1%slope
        ch2%deg_btm_m = ch1%deg_btm_m
        ch2%deg_bank_m = ch1%deg_bank_m
        ch2%hc_m = ch1%hc_m
        ch2%flo_in_mm = ch1%flo_in_mm / const
        ch2%aqu_in_mm = ch1%aqu_in_mm / const
        ch2%flo_mm = ch1%flo_mm / const
        ch2%sed_stor = ch1%sed_stor / const
      end function chsd_div
            
      function chsd_ave (ch1,const) result (ch2)
        type (sd_ch_output), intent (in) :: ch1
        real, intent (in) :: const
        type (sd_ch_output) :: ch2
        ch2%flo_in = ch1%flo_in / const
        ch2%aqu_in = ch1%aqu_in / const
        ch2%flo = ch1%flo / const
        ch2%peakr = ch1%peakr / const
        ch2%washld_in = ch1%washld_in
        ch2%washld_out = ch1%washld_out
        ch2%bedld_in = ch1%bedld_in
        ch2%bedld_out = ch1%bedld_out
        ch2%dep = ch1%dep
        ch2%deg_btm = ch1%deg_btm
        ch2%deg_bank = ch1%deg_bank
        ch2%hc_sed = ch1%hc_sed
        ch2%width = ch1%width / const
        ch2%depth = ch1%depth / const
        ch2%slope = ch1%slope / const
        ch2%deg_btm_m = ch1%deg_btm_m / const
        ch2%deg_bank_m = ch1%deg_bank_m / const
        ch2%hc_m = ch1%hc_m / const
        ch2%flo_in_mm = ch1%flo_in_mm
        ch2%aqu_in_mm = ch1%aqu_in_mm
        ch2%flo_mm = ch1%flo_mm
        ch2%sed_stor = ch1%sed_stor
      end function chsd_ave
      
      function chsd_mult (const, chn1) result (chn2)
        type (sd_ch_output), intent (in) :: chn1
        real, intent (in) :: const
        type (sd_ch_output) :: chn2
        chn2%flo_in = const * chn1%flo_in
        chn2%aqu_in = const * chn1%aqu_in
        chn2%flo = const * chn1%flo
        chn2%peakr = const * chn1%peakr
        chn2%washld_in = const * chn1%washld_in
        chn2%washld_out = const * chn1%washld_out
        chn2%bedld_in = const * chn1%bedld_in
        chn2%bedld_out = const * chn1%bedld_out
        chn2%dep = const * chn1%dep
        chn2%deg_btm = const * chn1%deg_btm
        chn2%deg_bank = const * chn1%deg_bank
        chn2%hc_sed = const * chn1%hc_sed 
        chn2%width = const * chn1%width
        chn2%depth = const * chn1%depth
        chn2%slope = const * chn1%slope
        chn2%deg_btm_m = const * chn1%deg_btm_m
        chn2%deg_bank_m = const * chn1%deg_bank_m
        chn2%hc_m = const * chn1%hc_m
        chn2%flo_in_mm = const * chn1%flo_in_mm
        chn2%aqu_in_mm = const * chn1%aqu_in_mm
        chn2%flo_mm = const * chn1%flo_mm
        chn2%sed_stor = const * chn1%sed_stor
      end function chsd_mult
    
      end module channel_module1