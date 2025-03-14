      subroutine reservoir_control1 (iob)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

      use reservoir_module1
      use reservoir_data_module
      use hydrograph_module !, only : ob
      use climate_module1
      
      implicit none

      integer :: ires                   !none       |reservoir number
      integer :: ires_db                !none       |reservoir data pointer
      integer,  intent (in) :: iob      !none       |object number
      real :: residence                 !m3/m3      |residence time
      real :: trap_eff                  !frac       |Brune reservoir trap efficiency
      real :: precip_m3                 !m3         |precipitation on reservoir surface
      real :: evap_m3                   !m3         |evaporation from reservoir surface
      
      300   format((I4,4x1A,1F16.4),*(8x,1a16,8G16.4,2x))
      
      ires = ob(iob)%num
      ires_db = ob(iob)%props
      iwst = ob(iob)%wst
      res1(ires)%area_ha = ob(iob)%area_ha
            
      !! calculate Brune trap efficiency (deposition)
      if (ob(iob)%hin%flo > 1.) then
        !! ha-m / m3
        residence = 10000. * res1(ires)%res_db%pvol / ob(iob)%hin%flo
        trap_eff = residence / (.012 + 1.02 * residence)
      else
        trap_eff = 0.
      end if
      
      !! assume outflow = inflow - evap + precip
      evap_m3 =  10. * cli(iwst)%pet_mm * res1(ires)%res_db%psa         !m3=10*ha*mm
      precip_m3 =  10. * cli(iwst)%precip_mm * res1(ires)%res_db%psa    !m3=10*ha*mm
      ob(iob)%hd(1)%flo = ob(iob)%hin%flo - evap_m3 + precip_m3
      
      !! sediment and constituents leaving reservoir
      ob(iob)%hd(1)%sed = (1. - trap_eff) * ob(iob)%hin%sed
      ob(iob)%hd(1)%orgn = (1. - trap_eff) * ob(iob)%hin%orgn
      ob(iob)%hd(1)%sedp = (1. - trap_eff) * ob(iob)%hin%sedp
      ob(iob)%hd(1)%no3 = ob(iob)%hin%no3
      ob(iob)%hd(1)%solp = ob(iob)%hin%solp
      
      
      write (1977, 300) ires, res1(ires)%name, res1(ires)%area_ha,  &
          ' INFLOW     ', ob(iob)%hin,                              &
          ' OUTFLOW    ', ob(iob)%hd(1)
!!    res_flo.out file
      write (1987,*) ires, res1(ires), ob(iob)%hin%flo, residence, trap_eff
      
      return
      end subroutine reservoir_control1