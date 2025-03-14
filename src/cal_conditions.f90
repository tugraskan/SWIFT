      subroutine cal_conditions

      use maximum_data_module
      use calibration_data_module
      use hru_module1
      use channel_module1
      use climate_module1
      use reservoir_module1
      use hydrograph_module
      
      implicit none
           
      character(len=16) :: chg_parm                           !                |               
      character(len=16) :: chg_typ                            !variable        |type of change (absval, abschg, pctchg)
      character(len=1) :: cond_met                            !                |       
      character(len=1) :: pl_find                             !                |       
      integer :: ic                                           !none            |counter
      integer :: ichg_par                                     !none            |counter
      integer :: ispu                                         !none            |counter
      integer :: ielem                                        !none            |counter
      real :: chg_val                                         !                |
      real :: absmin                                          !                |minimum range for variable
      real :: absmax                                          !                |maximum change for variable
      integer :: num_db                                       !                |
      integer :: ipro
         
      do ichg_par = 1, db_mx%cal_upd
        do ispu = 1, cal_upd(ichg_par)%num_elem
          ielem = cal_upd(ichg_par)%num(ispu)
          chg_parm = cal_upd(ichg_par)%name
          chg_typ = cal_upd(ichg_par)%chg_typ
          chg_val = cal_upd(ichg_par)%val
          absmin = cal_parms(cal_upd(ichg_par)%num_db)%absmin
          absmax = cal_parms(cal_upd(ichg_par)%num_db)%absmax
          num_db = cal_upd(ichg_par)%num_db
          
          !check to see if conditions are met
          cond_met = "y"
          do ic = 1, cal_upd(ichg_par)%conds
            select case (cal_upd(ichg_par)%cond(ic)%var)
            case ("lu_mgt")
              if (cal_upd(ichg_par)%cond(ic)%targc /= pl_find) then
                cond_met = "n"
              end if
            case ("str_order")
              if (int(cal_upd(ichg_par)%cond(ic)%targ) /= cha1(ielem)%dat%order) then
                cond_met = "n"
              end if
            end select
          end do    ! ic - conditions

          if (cond_met == "y") then
            select case (chg_parm)
            case ("exco_flo")
              do ipro = 1, 5
                call cal_chg_par (hru_ex(ielem)%runoff(ipro)%flo, chg_typ, chg_val, absmin, absmax)
              end do
        
            case ("exco_sed")
              do ipro = 1, 5
                call cal_chg_par (hru_ex(ielem)%runoff(ipro)%sed, chg_typ, chg_val, absmin, absmax)
              end do
        
            case ("exco_orgn")
              do ipro = 1, 5
                call cal_chg_par (hru_ex(ielem)%runoff(ipro)%orgn, chg_typ, chg_val, absmin, absmax)
              end do
        
            case ("exco_sedp")
              do ipro = 1, 5
                call cal_chg_par (hru_ex(ielem)%runoff(ipro)%sedp, chg_typ, chg_val, absmin, absmax)
              end do
        
            case ("exco_no3")
              do ipro = 1, 5
                call cal_chg_par (hru_ex(ielem)%runoff(ipro)%no3, chg_typ, chg_val, absmin, absmax)
              end do
        
            case ("exco_solp")
              do ipro = 1, 5
                call cal_chg_par (hru_ex(ielem)%runoff(ipro)%solp, chg_typ, chg_val, absmin, absmax)
              end do

            case ("ch_width") 
              call cal_chg_par (cha1(ielem)%dat%chw, chg_typ, chg_val, absmin, absmax)
        
            case ("ch_dep")
              call cal_chg_par (cha1(ielem)%dat%chd, chg_typ, chg_val, absmin, absmax)
        
            case ("ch_slope")
              call cal_chg_par (cha1(ielem)%dat%chs, chg_typ, chg_val, absmin, absmax)
        
            case ("ch_n")
              call cal_chg_par (cha1(ielem)%dat%chn, chg_typ, chg_val, absmin, absmax)
        
            case ("ch_clay")
              call cal_chg_par (cha1(ielem)%dat%ch_clay, chg_typ, chg_val, absmin, absmax)
        
            case ("ch_sinu")
              call cal_chg_par (cha1(ielem)%dat%sinu, chg_typ, chg_val, absmin, absmax)
        
            case ("ch_cov")
              call cal_chg_par (cha1(ielem)%dat%cov, chg_typ, chg_val, absmin, absmax)
        
            case ("ch_d50")
              call cal_chg_par (cha1(ielem)%dat%d50, chg_typ, chg_val, absmin, absmax)
        
            case ("q1_qm")
              call cal_chg_par (cha1(ielem)%sed%q1_qm, chg_typ, chg_val, absmin, absmax)
        
            case ("fp_inun_days")
              call cal_chg_par (cha1(ielem)%sed%fp_inun_days, chg_typ, chg_val, absmin, absmax)
        
            case ("fp_flo_fr")
              call cal_chg_par (cha1(ielem)%sed%fp_flo_fr, chg_typ, chg_val, absmin, absmax)
        
            case ("bank_nconc")
              call cal_chg_par (cha1(ielem)%dat%ch_clay, chg_typ, chg_val, absmin, absmax)
        
            case ("bank_pconc")
              call cal_chg_par (cha1(ielem)%dat%sinu, chg_typ, chg_val, absmin, absmax)
        
            case ("bank_pbio ")
              call cal_chg_par (cha1(ielem)%dat%cov, chg_typ, chg_val, absmin, absmax)
        
            case ("ch_nsol_part")
              call cal_chg_par (cha1(ielem)%dat%d50, chg_typ, chg_val, absmin, absmax)
        
            case ("ch_psol_part")
              call cal_chg_par (cha1(ielem)%sed%q1_qm, chg_typ, chg_val, absmin, absmax)
        
            case ("n_dep_fr")
              call cal_chg_par (cha1(ielem)%sed%fp_inun_days, chg_typ, chg_val, absmin, absmax)
        
            case ("p_dep_fr")
              call cal_chg_par (cha1(ielem)%sed%fp_flo_fr, chg_typ, chg_val, absmin, absmax)
        
            case ("res_pvol")
              call cal_chg_par (res1(ielem)%res_db%pvol, chg_typ, chg_val, absmin, absmax)
   
            end select
          end if

        end do        ! ispu
      end do          ! ichg_par
      
      return
      end subroutine cal_conditions