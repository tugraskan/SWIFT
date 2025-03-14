      subroutine ru_control (iob)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!

      use ru_module1
      use hydrograph_module
      use constituent_mass_module
      
      implicit none 
      
      integer,  intent (in) :: iob
      integer :: iobe
      character (len=3) :: ihtyp         !            |
      real :: sumfrac                    !            |
      real :: sumarea                    !            |
      integer :: ielem                   !none        |counter
      integer :: ise                     !none        |counter
      integer :: ihtypno                 !            |
      integer :: ihru
      integer :: iru
      real :: ef                         !            |
      integer :: ipest                   !            |
      
      300   format((I4,4x1A,10x1F16.4),7x,5(1a16,8G16.4,10x)) 
           
      iru = ob(iob)%props
      
      ob(iob)%hd(1) = hz
      ob(iob)%hd(2) = hz
      ob(iob)%hd(3) = hz
      ob(iob)%hd(4) = hz
      ob(iob)%hd(5) = hz
      if (cs_db%num_tot > 0) then
        obcs(iob)%hd(1) = hin_csz
        obcs(iob)%hd(2) = hin_csz
        obcs(iob)%hd(3) = hin_csz
        obcs(iob)%hd(4) = hin_csz
        obcs(iob)%hd(5) = hin_csz
      end if
      
      sumfrac = 0.
      sumarea = 0.
      
      do ielem = 1, ru_def(iru)%num_tot
        ise = ru_def(iru)%num(ielem)
        iobe = ru_elem(ise)%obj
        ihru = ru_elem(ise)%obtypno
        ht1 = hz
        ht2 = hz
        ht3 = hz
        ht4 = hz
        ht5 = hz
        delrto = hz
        
        sumfrac = sumfrac + ru_elem(ise)%frac
        sumarea = sumarea + ob(iobe)%area_ha
        
        !define delivery ratio - all variables are hyd_output type

        !calculated dr = f(tconc element/ tconc sub)
        delrto = 1. .add. hz    !ru_elem(ise)%dr

        if (ru_elem(ise)%obtyp == "exc") then
          !! compute hyds for export coefficients-ht1==surface,ht2==groundwater
          ht1 = exco(ob(iobe)%props) ** delrto
          ht2 = hz
          if (ob(iobe)%area_ha > .01) then
            !per area units - mm, t/ha, kg/ha (ie hru, apex)
            ef = ru_elem(ise)%frac * ru1(iru)%da_km2 / (ob(iobe)%area_ha / 100.)
            ht1 = ef * ht1
          end if
          
        else
          !! for routing units, channels, reservoir, and recall objects use fraction
          ef = ru_elem(ise)%frac
          !! for hru's use define expansion factor to surface/soil and recharge
          if (ob(iobe)%typ == "hru") then
            !! if frac is 1.0, then the hru is not part of ru and use entire hru output
            if (ef < .99999) then
              ef = ef * ru1(iru)%da_km2 / (ob(iobe)%area_ha / 100.)
            end if
          end if
          
          !compute all hyd"s needed for routing
          do ihtypno = 1, ob(iobe)%nhyds
            if (ihtypno /=2) then
              !! apply dr to tot, surf, lat and tile
              ht1 = ob(iobe)%hd(ihtypno) ** delrto
              do ipest = 1, cs_db%num_pests
                hcs1%pest(ipest) = obcs(iobe)%hd(ihtypno)%pest(ipest)    !* delrto - don't apply dr to pests
              end do
            else
              !! don't apply dr to recharge
              ht1 = ob(iobe)%hd(ihtypno)
              !! set constituents
              do ipest = 1, cs_db%num_pests
                hcs1%pest(ipest) = obcs(iobe)%hd(ihtypno)%pest(ipest)
              end do
            end if
            ht1 = ef * ht1
            ob(iob)%hd(ihtypno) = ob(iob)%hd(ihtypno) + ht1
            !! set constituents
            do ipest = 1, cs_db%num_pests
              obcs(iob)%hd(ihtypno)%pest(ipest) = obcs(iob)%hd(ihtypno)%pest(ipest) + hcs1%pest(ipest)
            end do
          end do
          
        end if      !ru_elem(ise)%obtyp == "exc"

      end do  !element loop
      

      !! Routing Unit Output61
      write (1982, 300) ob(iob)%props, ob(iob)%name, ob(iob)%area_ha, &
        ' TOTAL      ', ob(iob)%hd(1),  &
        ' PERCO      ', ob(iob)%hd(2),  &
        ' SURFACE    ', ob(iob)%hd(3),  &
        ' LATERAL    ', ob(iob)%hd(4),  &
        ' TILE       ', ob(iob)%hd(5)
        
	  return

      end subroutine ru_control