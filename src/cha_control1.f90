      subroutine cha_control1 (iob, num_cha)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

      use channel_module1
      use hydrograph_module !, only : ob, cha2
      
      implicit none

      integer,  intent (in) :: iob, num_cha
      integer :: icha                   !none       |channel number
      integer :: icha_db                !none       |channel data pointer
      integer :: idr                    !none       |pointer to instream delivery ratio
      real :: trap_eff                  !frac       |sediment flood plain trap efficiency
      real :: depo_fr                   !frac       |fraction of sediment entering the flood plain that is deposited
      real :: fp_sed_conc               !frac       |annual sediment in flood plain flow
      real :: ch_flo                    !frac       |annual flow in channel flow
      real :: rad_curv                  !           |radius of curvature
      real :: bed_te                    !           |shear on channel bed
      real :: bank_te                   !           |shear on channel bank
      real :: t_crit                    !           |radius of curvature factor 
      real :: rc_fac                    !           |shear on channel bed
      real :: bank_kd                   !           |bank erodibility
      real :: cov_fac                   !           |cover factor
      real :: bank_ero                  !           |bank erosion
      real :: bed_ero                   !           |bed erosion
      real :: e_days                    !days       !days for calculating bank erosion
      real :: arc_len                   !m          |arc length
      real :: ebank_t                   !t          |bank erosion in tons
      real :: ebank_m                   !m          |bank erosion in meters
      real :: d50_m                     !m          |median bed particle size
      real :: str_pow                   !           |stream power
      real :: str_pow_cr                !           |critical stream power to initiate transport
      real :: sedcap_ppm                !ppm        |sediment transport capacity in ppm
      real :: sedcap_t                  !t          |sediment transport capacity in tons
      real :: sedin_t                   !t          |sediment entering reach
      real :: ebtm_t                    !t          |bed erosion in tons
      real :: ebtm_m                    !m          |bed erosion in meters
      real :: dep                       !t          |sediment deposition in channel
      real :: qcms                      !m3/s       |flow rate at bankfull
      real :: hyd_rad                   !m          |hydraulic radius
      real :: xarea                     !m2         |cross sectional area
      real :: b_exp                     !           |exponent on v/vcr
      real :: vel                       !m/s        |flow velocity
      real :: vel_bend                  !m/s        |flow velocity in meander bend
      real :: vel_tot                   !m/s        |mean flow velocity
      real :: vel_crit                  !m/s        |critical flow velocity to initiate bank movement
      real :: vel_crit_bd               !m/s        |critical flow velocity to initiate bed movement
      real :: ttime                     !hr         |travel time through the reach - currently at bankfull
      real :: cohesion                  !??         |soil cohesion
      real :: veg                       !           |cover factor
      real :: fpdepo_mm                 !mm         |flood plain sediment deposition
      real :: fpwidth_km                !mm         |flood plain width
      real :: w_yr                      !           |widths of bank erosion per year
      real :: d_yr                      !           |depths of downcutting per year
      real :: n_mann                    !           |mannings n adjusted for slope for small channels
      real :: d_part                    !mm         |suspended particle size
      real :: vel_fall                  !m/h        |fall velocity
      real :: dep_fall                  !m          |depth of particle fall
      real :: del_rto                   !frac       |delivery of suspended sediment through reach
      real :: q_sed_ppm                 !ppm        |bed transport - ppm
      real :: q_sed                     !t/day      |sediment bed erosion per unit width
      real :: conc_chng                 !frac       |change in concentration of instream nutrient transformations
      real :: avg_wyr                   !mm???      |average bank width per year for basin
      
      300   format((I4,4x1A,1F16.4),*(8x,1a16,8G16.4,2x))
      
      icha = ob(iob)%num
      icha_db = ob(iob)%props
      cha1(icha)%area_ha = ob(iob)%area_ha
      cha1(icha)%name = ob(iob)%name
      
      !! calculate flood plain deposition and losses
      !! calculate sediment concentration at bankfull (Q1) using sediment rating curve exp
      fp_sed_conc = (cha1(icha)%sed%q1_qm) ** cha1(icha)%sed%rcurv_exp
      !! fraction overbank sediment deposited - based on fall velocity and days inundated
      trap_eff = 1.2 * exp(-0.424 * cha1(icha)%sed%fp_inun_days)
      trap_eff = 0.24 * log(cha1(icha)%sed%fp_inun_days) + 0.1
      !! calculate deposition and outflow
      depo_fr = cha1(icha)%sed%fp_flo_fr * fp_sed_conc * trap_eff
      depo_fr = amax1(0., depo_fr)
      depo_fr = amin1(1., depo_fr)
      cha2(icha)%fp_dep = depo_fr * ob(iob)%hin
      !! don't deposit all the organics with the sediment
      cha2(icha)%fp_dep%orgn = cha1(icha)%sed%n_dep_fr * cha2(icha)%fp_dep%orgn
      cha2(icha)%fp_dep%sedp = cha1(icha)%sed%p_dep_fr * cha2(icha)%fp_dep%sedp
      cha2(icha)%fp_dep%nh3 = 0.
      
      !! compute flow rate at bankfull - assume rectangular
      xarea = cha1(icha)%dat%chw * cha1(icha)%dat%chd
      hyd_rad = xarea / (cha1(icha)%dat%chw + 2. * cha1(icha)%dat%chd)
      !! adjust n for small order streams < 10 km2
      !if (cha1(icha)%area_ha < 1000.) then
      !  n_mann = 1.85 * cha1(icha)%dat%chs ** 0.79
      !else
        n_mann = cha1(icha)%dat%chn
      !end if
        
      !***tu check and assign value if %dat%chs is 0   
      if (cha1(icha)%dat%chs < .00001) then 
          cha1(icha)%dat%chs = .00001
      end if  
        
      !vel = hyd_rad ** .6666 * Sqrt(cha1(icha)%dat%chs) / (n_mann + 0.0001)
      ob(iob)%hin%flo = Max (0., ob(iob)%hin%flo)
      vel = cha1(icha)%sed%q1_qm * ob(iob)%hin%flo / xarea / 86400.
      qcms = xarea * vel
      !! try qcms = q1 --> qcms = q1/qm * qm
      !qcms = cha1(icha)%sed%q1_qm * ob(iob)%hin%flo / 365.
      
      !***tu cha1(icha)%dat%ch_clay over 65 giving floating point error quick fix
      if (cha1(icha)%dat%ch_clay > 65) then
          cha1(icha)%dat%ch_clay = 65
      end if
      
      !! calculate bank erosion
      !b_exp = 5.53 / (1. + exp(-29.11 + 1.8 * cha1(icha)%dat%ch_clay)) ** 0.0248
      b_exp = 12.3 / sqrt (cha1(icha)%dat%ch_clay + 1.)
      b_exp = min (3.5, b_exp)
      !vel = 1.37 * (cha1(icha)%dat%chs ** 0.31) * (12. * cha1(icha)%dat%chw) ** 0.32
      
      cha1(icha)%dat%sinu = Max (1.0, cha1(icha)%dat%sinu)
      
      rad_curv = (12. * cha1(icha)%dat%chw * cha1(icha)%dat%sinu ** 1.5) /        &
                                       (13. * (cha1(icha)%dat%sinu - 0.999) ** 0.5)
      !vel_bend = vel * (1.74 - 0.52 * log10 (rad_curv / cha1(icha)%dat%chw))
      !vel_bend = vel * (1.66 - 0.42 * log10 (rad_curv / cha1(icha)%dat%chw))
      vel_bend = vel * (1. / rad_curv + 1.)
      vel_tot = 0.33 * vel_bend + 0.66 * vel
      vel_tot = vel_bend
      cohesion = (-87.1 + (42.82 * cha1(icha)%dat%ch_clay) - (0.267 * cha1(icha)%dat%ch_clay ** 2.) &
                                     + (0.029 * cha1(icha)%dat%ch_clay ** 3.)) * 2.
      cohesion = (15. / (15.3 - 0.438 * cha1(icha)%dat%ch_clay + 0.0044 * cha1(icha)%dat%ch_clay ** 2.)) * 1000.
      cohesion = amax1 (1000., cohesion)    !min 1000 for sandy soils
      veg = exp (-5. * cha1(icha)%dat%chd) * cha1(icha)%dat%cov                              !function of cover factor
      
      !***tu check and assign value if %dat%ch_bd is <1 prevents floating error
      if (cha1(icha)%dat%ch_bd < 1.) then 
          cha1(icha)%dat%ch_bd = 1.
      end if
      
      vel_crit = log10 (8.8 * cha1(icha)%dat%chd / 0.004) * (0.0004 * ((cha1(icha)%dat%ch_bd *     &
                        1000. - 1000.) * 9.81 * 0.004 + 0.021 * cohesion + veg)) ** 0.5
      e_days = 3.65     !assume 1% of time (q1 flows)
      if (vel_tot > vel_crit) then
        ebank_m = 0.0024 * e_days * (vel_tot / vel_crit) ** b_exp
      else
        ebank_m = 0.
      end if
          
      !! calc mass of sediment eroded -> t = bankcut (m) * depth (m) * lengthcut (m) * bd (t/m3)
      !! arc length = 0.33 * meander wavelength * sinuosity  -> protected length 
      arc_len = 0.33 *  (12. * cha1(icha)%dat%chw) * cha1(icha)%dat%sinu
      ebank_t = ebank_m * cha1(icha)%dat%chd * arc_len * cha1(icha)%dat%ch_bd
      !! assume 20% is sand and the remainder is washload - need to make a function of d50 or input sand in bank
      cha2(icha)%bank_ero%sed = 0.8 * ebank_t
      cha2(icha)%bank_ero%nh3 = 0.2 * ebank_t       !using nh3 for bedload
       !! calculate bank erosion associated nutrients - ppm
      cha2(icha)%bank_ero%orgn = cha2(icha)%bank_ero%sed * cha1(icha)%dat%n_conc / 1000.
      cha2(icha)%bank_ero%sedp = (1. - cha1(icha)%dat%p_bio) * cha2(icha)%bank_ero%sed * cha1(icha)%dat%p_conc / 1000.
      cha2(icha)%bank_ero%no3 = 0.
      cha2(icha)%bank_ero%solp = cha1(icha)%dat%p_bio * cha2(icha)%bank_ero%sed * cha1(icha)%dat%p_conc / 1000.
      cha2(icha)%bank_ero%no2 = 0.
      
      !! assume sand in bank is deposited and rest is washload
      
      !!calculate bed erosion
      d50_m = cha1(icha)%dat%d50 / 1000.
      str_pow = 9810. * qcms * cha1(icha)%dat%chs   !/ cha1(icha)%dat%chw !stream power
      str_pow_cr = 100. * (16.2 * d50_m) ** 1.5     !.1*1000. (2.65-1_*9.81*d50)**1.5
      if (str_pow > str_pow_cr) then
      !! Lammers and Bledsoe - ppm
        q_sed_ppm = 0.0214 * (str_pow - str_pow_cr) ** 1.5 * (d50_m ** -1) * (qcms ** -0.833)
      else
        q_sed_ppm = 0.0
      end if
      ebtm_t = 0.0027 * qcms * q_sed_ppm * cha1(icha)%dat%chw
      !! check if > incoming sediment
      if (ebtm_t * e_days > ob(iob)%hin%sed + ob(iob)%hin%nh3) then
        cha1(icha)%dat%d50_sub = 20.
        vel_crit_bd = 0.293 * (cha1(icha)%dat%d50_sub) ** 0.5
        if (vel_tot > vel_crit_bd) then
          !! downcutting of bottom sub surface (below deposited sand)
          e_days = 1.825    !assume Q0.5 does downcutting
          ebtm_m = 0.0024 * e_days * (vel_tot / vel_crit_bd) ** 1.5
        else
          ebtm_m = 0.
        end if
      else
        ebtm_m = 0.
      end if
      !! t = m * (t/m3 * m * km * 1000 m/km)
      ebtm_t = ebtm_m * (cha1(icha)%dat%ch_bd * cha1(icha)%dat%chw * cha1(icha)%dat%chl * 1000.)
      
      !! calculate channel deposition based on fall velocity - SWRRB book
      !! assume particle size = 0.03 mm -- median silt size
      d_part = 0.002
      vel_fall = 411. * d_part ** 2     ! m/h
      ttime = cha1(icha)%dat%chl / (3.6 * (vel + 0.01))      !km / m/s * 1000m/km * h/3600s
      dep_fall = vel_fall * ttime
      !! assume bankfull flow depth
      if (dep_fall < cha1(icha)%dat%chd) then
        del_rto = 1. - .5 * dep_fall / cha1(icha)%dat%chd
      else
        del_rto = .5 * cha1(icha)%dat%chd / dep_fall
      end if
      cha2(icha)%bed_dep%sed = (1. - del_rto) * ob(iob)%hin%sed
      cha2(icha)%bed_dep%orgn = cha1(icha)%sed%n_dep_fr * (1. - del_rto) * ob(iob)%hin%orgn
      cha2(icha)%bed_dep%sedp = cha1(icha)%sed%p_dep_fr * (1. - del_rto) * ob(iob)%hin%sedp

      !! calculate in channel nutrient losses -  input delivery ratio
      ch_flo = 1. - cha1(icha)%sed%fp_flo_fr
      cha2(icha)%ch_rout = cha_dr(icha)%instr_dr ** (ch_flo * ob(iob)%hin)
      
      !! calculate in channel nutrient transformations
      ttime = cha1(icha)%dat%chl / (3.6 * (vel + 0.01))      !hr = km/(3.6*m/s)
      ttime = Min (24., ttime)
      conc_chng = 1. - exp(-cha1(icha)%sed%ch_nsol_part * ttime)
      cha2(icha)%trans%no3 = -conc_chng * ob(iob)%hin%no3
      !cha2(icha)%trans%no3 = Max (cha2(icha)%trans%no3, ob(iob)%hin%orgn)
      cha2(icha)%trans%orgn = -cha2(icha)%trans%no3
      conc_chng = 1. - exp(-cha1(icha)%sed%ch_psol_part * ttime)
      cha2(icha)%trans%solp = -conc_chng * ob(iob)%hin%solp
      !cha2(icha)%trans%solp = Max (cha2(icha)%trans%solp, cha2(icha)%trans%sedp)
      cha2(icha)%trans%sedp = -cha2(icha)%trans%solp
      
      !! calculate bed erosion associated nutrients - ppm
      cha2(icha)%bed_ero%orgn = cha2(icha)%bed_ero%sed * cha1(icha)%dat%n_conc
      cha2(icha)%bed_ero%sedp = (1. - cha1(icha)%dat%p_bio) * cha2(icha)%bed_ero%sed * cha1(icha)%dat%p_conc
      cha2(icha)%bed_ero%no3 = 0.
      cha2(icha)%bed_ero%solp = cha1(icha)%dat%p_bio * cha2(icha)%bed_ero%sed * cha1(icha)%dat%p_conc
      cha2(icha)%bed_ero%nh3 = ebtm_t      !using nh3 for bedload
      cha2(icha)%bed_ero%no2 = 0.

      !! sediment leaving channel
      ob(iob)%hd(1) = ob(iob)%hin + cha2(icha)%bank_ero + cha2(icha)%bed_ero - cha2(icha)%fp_dep + cha2(icha)%trans

      write (1979, 300) icha, cha1(icha)%name, cha1(icha)%area_ha, &
          ' INFLOW     ', ob(iob)%hin,          &
          ' OUTFLOW    ', ob(iob)%hd(1),        &
          ' BANK       ', cha2(icha)%bank_ero,  &
          ' BED        ', cha2(icha)%bed_ero,   &
          ' BED DEP    ', cha2(icha)%bed_dep,   &   
          ' FP DEP     ', cha2(icha)%fp_dep

      !! bank parameter output
      w_yr = ebank_m / cha1(icha)%dat%chw
      write (1988, *) icha, cha1(icha)%name, cha1(icha)%area_ha, cha1(icha)%dat%chw, cha1(icha)%dat%chl,    &
          cha1(icha)%dat%ch_bd, b_exp, cha1(icha)%dat%ch_clay, cha1(icha)%dat%sinu, vel_tot, vel_crit,      &
          cohesion, e_days, rad_curv, arc_len, ebank_t, ebank_m, w_yr !, ob(iob)%hin%sed, ob(iob)%hin%nh3,    &
          !ob(iob)%hd(1)%sed, ob(iob)%hd(1)%nh3
      
      !! bed parameter output
      d_yr = ebtm_m / cha1(icha)%dat%chd
      write (1985, *) icha, cha1(icha)%name, cha1(icha)%area_ha, cha1(icha)%dat%chw, cha1(icha)%dat%chd,    &
          cha1(icha)%dat%chl, cha1(icha)%dat%d50_sub, qcms, str_pow, str_pow_cr, sedcap_ppm, sedcap_t,      &
          vel_tot, vel_crit_bd, ebtm_t, ebtm_m, d_yr !, ob(iob)%hin%sed, ob(iob)%hin%nh3, ob(iob)%hd(1)%sed,  &
          !ob(iob)%hd(1)%nh3
      
      !! flood plain parameter output
      fpwidth_km = 0.000012 * (100. * cha1(icha)%area_ha) ** 0.982 !Beighley and Gummadi, ESPL
      !! mm = t / (km*km*t/m3) * km/1000.m * km/1000.m * 1000.mm/m = t/(km*km*t/m3) / 1000.
      fpdepo_mm = cha2(icha)%fp_dep%sed / (1000. * fpwidth_km *  cha1(icha)%dat%chl * cha1(icha)%dat%ch_bd)
      write (1986, *) icha, cha1(icha)%name, cha1(icha)%area_ha, cha1(icha)%sed%q1_qm,                      &
          cha1(icha)%sed%rcurv_exp, cha1(icha)%sed%fp_inun_days, cha1(icha)%sed%fp_flo_fr, fp_sed_conc,     &
          trap_eff, depo_fr, fpwidth_km, ob(iob)%hin%sed, cha2(icha)%fp_dep%sed, fpdepo_mm
      
      !! sum by stream order for calibration output
      !if (cha1(icha)%dat%order <= 3) then
      !  cal_out%so1_3%num_cha = cal_out%so1_3%num_cha + 1
      !  cal_out%so1_3%sum_wyr = cal_out%so1_3%sum_wyr + w_yr
      !  cal_out%so1_3%sum_fpdep = cal_out%so1_3%sum_fpdep + fpdepo_mm
      !end if
      
      !!!! chan wyr and fp dep sums
      cal_bso%sum_wyr = cal_bso%sum_wyr + w_yr
      avg_wyr = cal_bso%sum_wyr / num_cha
      cal_bso%sum_fpdep = cal_bso%sum_fpdep + fpdepo_mm ! mm
      cal_bso%fparea = cal_bso%fparea +  (cha1(icha)%dat%chl * fpwidth_km * 5) !km
      cal_bso%fpdep_mean =  cal_bso%sum_fpdep / cal_bso%fparea
      
      
      return
      end subroutine cha_control1