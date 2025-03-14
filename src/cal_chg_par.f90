      subroutine cal_chg_par (val_cur, chg_typ, chg_val, absmin, absmax)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function computes new paramter value based on 
!!    user defined change

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    val_cur     |variable      |current parameter value
!!                               |the standard temperature (20 degrees C)
!!    chg_typ     |variable      |type of change (absval, abschg, pctchg)
!!    chg_val     |variable      |amount of change
!!    parm_num    |              |calibration parameter number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chg_par     |variable      |new parameter value
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      implicit none

      real, intent (in out) :: val_cur                  !variable      |current parameter value
      real, intent (in) :: chg_val                  !variable      |amount of change
      character(len=16), intent (in) :: chg_typ     !variable      |type of change (absval, abschg, pctchg)
      real, intent (in) :: absmin                                  !minimum range for variable
      real, intent (in) :: absmax                                  !maximum change for variable

      select case (chg_typ)

      case ("absval")
        val_cur = chg_val
     
      case ("abschg")
        val_cur = val_cur + chg_val

      case ("pctchg")
        val_cur = (1. + chg_val / 100.) * val_cur
        
      case ("relchg")
        val_cur = (1. + chg_val) * val_cur
      
      end select
      
      val_cur = Max(val_cur, absmin)
      val_cur = Min(val_cur, absmax)
      
      return
      end subroutine cal_chg_par