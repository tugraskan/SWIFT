      subroutine hru_control1 (iob)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

      use hru_module1
      use hydrograph_module !, only : ob, hru2
      
      implicit none

      integer :: j                  !none          |same as ihru (hru number)
      integer :: ihru
      integer,  intent (in) :: iob
      real :: residence                 !m3/m3      |residence time
      real :: trap_eff                  !frac       |Brune reservoir trap efficiency
      
      ihru = ob(iob)%props
      
      write (1980, '(*(G16.4))') 'ihru: ',ihru, 'name: ', hru_dat(ihru)%name,       &
          'area_ha: ', hru_dat(ihru)%area_ha, 'precip: ',hru_dat(iob)%precip,       &
          'lum: ', hru_dat(ihru)%lum, 'hsg: ', hru_dat(ihru)%hsg,                   &
          'surf_stor: ', hru_dat(ihru)%surf_stor, 'dr: ',hru_dat(ihru)%dr
      
      !! if routing over the hru, add the runon/runoff
      if (hru_dat(ihru)%dr == "y") then
          ob(iob)%hd(2)%flo = ob(iob)%hd(2)%flo + ob(iob)%hin_aqu%flo * hru_dr(ihru)%dr(2)%flo
          ob(iob)%hd(3)%flo = ob(iob)%hd(3)%flo + ob(iob)%hin_sur%flo * hru_dr(ihru)%dr(3)%flo
          ob(iob)%hd(4)%flo = ob(iob)%hd(4)%flo + ob(iob)%hin_lat%flo * hru_dr(ihru)%dr(4)%flo
          ob(iob)%hd(5)%flo = ob(iob)%hd(5)%flo + ob(iob)%hin_til%flo * hru_dr(ihru)%dr(5)%flo
          ob(iob)%hd(1)%flo = ob(iob)%hd(3)%flo + ob(iob)%hd(4)%flo + ob(iob)%hd(5)%flo
          !! don't adjust percolation
          !! need sed, orgn, sedp, no3, solp, nh3, no2 for each hdyrograph
      end if
   
      !! if wetland, use wetland model
      if (hru_dat(ihru)%surf_stor == "y") then         
        !! calculate Brune trap efficiency (deposition)
        if (ob(iob)%hin%flo > 1.) then
          residence = hru_dat(ihru)%wet%pvol / ob(iob)%hin%flo
          trap_eff = residence / (.012 + 1.02 * residence)
        else
          trap_eff = 0.
        end if
        !! flow and constituents leaving wetland
        ob(iob)%hd(1) = trap_eff * ob(iob)%hin
        
        write (1980, *) ' INFLOW    ', ob(iob)%hin
        write (1980, *) ' OUTFLOW   ', ob(iob)%hd(1)
      end if
      
      !! if no routing or wetland, ob()%hd(:) is set from the inputs
      if (hru_dat(ihru)%dr == "n") then
        write (1980, *) ' TOTAL     ', ob(iob)%hd(1)
        write (1980, *) ' PERCO     ', ob(iob)%hd(2)
        write (1980, *) ' SURFACE   ', ob(iob)%hd(3)
        write (1980, *) ' LATERAL   ', ob(iob)%hd(4)
        write (1980, *) ' TILE      ', ob(iob)%hd(5)
      end if
      
      return
      end subroutine hru_control1
