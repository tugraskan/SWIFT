      subroutine cal_parm_select (parm, chg_parm, chg_typ, chg_val, absmin, absmax)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine finds the current paramter value based on 
!!    user defined change

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    val_cur     |variable      |current parameter value
!!                               |the standard temperature (20 degrees C)
!!    chg         |data type     |contains information on variable change
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use channel_data_module 
      use reservoir_data_module
      use hru_module1
      use channel_module1
      use reservoir_module1
      use hydrograph_module
      
      implicit none
 
      real, intent (in out) :: parm
      character(len=16), intent (in) :: chg_parm            !!!                |      
      character(len=16), intent (in) :: chg_typ             !variable        |type of change (absval, abschg, pctchg)
      real, intent (in) :: chg_val                          !                |      
      real, intent (in) :: absmin                           !                |minimum range for variable 
      real, intent (in) :: absmax                           !                |maximum change for variable
      integer :: ielem                                      !                | 
      integer :: num_db                                     !                |
      integer :: ipro

      select case (chg_parm)
      case ("exco_flo")
        do ipro = 1, 5
          parm = hru_ex(ielem)%runoff(ipro)%flo
          call cal_chg_par (parm, chg_typ, chg_val, absmin, absmax)
          hru_ex(ielem)%runoff(ipro)%flo = parm
        end do
        
      case ("exco_sed")
        do ipro = 1, 5
          parm = hru_ex(ielem)%runoff(ipro)%sed
          call cal_chg_par (parm, chg_typ, chg_val, absmin, absmax)
          hru_ex(ielem)%runoff(ipro)%sed = parm
        end do
        
      case ("exco_orgn")
        do ipro = 1, 5
          parm = hru_ex(ielem)%runoff(ipro)%orgn
          call cal_chg_par (parm, chg_typ, chg_val, absmin, absmax)
          hru_ex(ielem)%runoff(ipro)%orgn = parm
        end do
        
      case ("exco_orgp")
        do ipro = 1, 5
          parm = hru_ex(ielem)%runoff(ipro)%sedp
          call cal_chg_par (parm, chg_typ, chg_val, absmin, absmax)
          hru_ex(ielem)%runoff(ipro)%sed = parm
        end do
        
      case ("exco_no3")
        do ipro = 1, 5
          parm = hru_ex(ielem)%runoff(ipro)%no3
          call cal_chg_par (parm, chg_typ, chg_val, absmin, absmax)
          hru_ex(ielem)%runoff(ipro)%no3 = parm
        end do
        
      case ("exco_solp")
        do ipro = 1, 5
          parm = hru_ex(ielem)%runoff(ipro)%solp
          call cal_chg_par (parm, chg_typ, chg_val, absmin, absmax)
          hru_ex(ielem)%runoff(ipro)%solp = parm
        end do

      case ("ch_width") 
        parm = cha1(ielem)%dat%chw
        call cal_chg_par (parm, chg_typ, chg_val, absmin, absmax)
        cha1(ielem)%dat%chw = parm
        
      case ("ch_dep")
        parm = cha1(ielem)%dat%chd
        call cal_chg_par (parm, chg_typ, chg_val, absmin, absmax)
        cha1(ielem)%dat%chd = parm
        
      case ("ch_slope")
        parm = cha1(ielem)%dat%chs
        call cal_chg_par (parm, chg_typ, chg_val, absmin, absmax)
        cha1(ielem)%dat%chs = parm
        
      case ("ch_n")
        parm = cha1(ielem)%dat%chs
        call cal_chg_par (parm, chg_typ, chg_val, absmin, absmax)
        cha1(ielem)%dat%chs = parm
        
      case ("ch_clay")
        parm = cha1(ielem)%dat%ch_clay
        call cal_chg_par (parm, chg_typ, chg_val, absmin, absmax)
        cha1(ielem)%dat%ch_clay = parm
        
      case ("ch_sinu")
        parm = cha1(ielem)%dat%sinu
        call cal_chg_par (parm, chg_typ, chg_val, absmin, absmax)
        cha1(ielem)%dat%sinu = parm
        
      case ("res_pvol")
        parm = res1(ielem)%res_db%pvol
        call cal_chg_par (parm, chg_typ, chg_val, absmin, absmax)
        res1(ielem)%res_db%pvol = parm
   
      end select

      return
      end subroutine cal_parm_select