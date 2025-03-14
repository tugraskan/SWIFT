      subroutine command
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    for every day of simulation, this subroutine steps through the command
!!    lines in the watershed configuration (.fig) file. Depending on the 
!!    command code on the .fig file line, a command loop is accessed
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: subbasin, route, routres, transfer, recmon
!!    SWAT: recepic, save, recday, recyear

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hydrograph_module
      use constituent_mass_module
      
      implicit none

      integer :: in                   !              |
      integer :: iin                   !              |
      integer :: ielem                !              |  
      integer :: iob                  !              |
      integer :: kk                   !none          |counter
      integer :: iday                 !              |
      integer :: isd                  !none          |counter
      integer :: ires                 !none          |reservoir number
      integer :: ihru
      integer :: iru
      integer :: irec                 !              |
      integer :: iout                 !none          |counter
      integer :: ihtyp                !              |
      integer :: iaq                  !none          |counter
      integer :: j                    !none          |counter
      integer :: ihyd                 !              |
      integer :: idr                  !              |
      integer :: ifirst               !              |
      integer :: iwro                 !              |
      integer :: ob_num               !              |
      real :: conv                    !              |
      real :: frac_in                 !              |
      integer :: ts1,ts2
      integer dum,i_count                    !rtb gwflow
      integer :: i_mfl,i_chan,i_hyd,chan_num !rtb gwflow; counter
      real :: sum
            
      sum = 0.
      
      300   format((I4,4x1A,10x1F16.4),7x,5(1a16,8G16.4,10x))

      icmd = sp_ob1%objs
      do while (icmd /= 0)
          
        !sum all receiving hydrographs
        !if (ob(icmd)%rcv_tot > 0) then
          ob(icmd)%hin = hz
          ob(icmd)%hin_sur = hz
          ob(icmd)%hin_lat = hz
          ob(icmd)%hin_til = hz
          ht1 = hz
          obcs(icmd)%hin = hin_csz
          obcs(icmd)%hin_sur = hin_csz
          obcs(icmd)%hin_lat = hin_csz
          obcs(icmd)%hin_til = hin_csz
          hcs1 = hin_csz
          hcs2 = hin_csz
          
          if (ob(icmd)%rcv_tot > 0) then
          do in = 1, ob(icmd)%rcv_tot
            iob = ob(icmd)%obj_in(in)
            ihyd = ob(icmd)%ihtyp_in(in)
            frac_in = ob(icmd)%frac_in(in)
            
            ! if object is not an hru, need ht1, don't need %hin_sur and %hin_lat
            ! don't have to check if it's in an ru - only hru's can be routed over
            if (ob(icmd)%typ == "hru" .or. ob(icmd)%typ == "hru_lte") then
              ! recieving hru, needs %hin_sur and %hin_lat and %hin_til to route separately in hru_control
              if (ob(icmd)%htyp_in(in) == "tot") then
                ! if total hyd coming in from hru or ru -> add both surface and lateral flows
                ! add to surface runon
                ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(3)
                if (cs_db%num_tot > 0) then
                  obcs(icmd)%hin_sur = obcs(icmd)%hin_sur + frac_in * obcs(iob)%hd(3)
                end if
                ! add to lateral soil runon
                ob(icmd)%hin_lat = ob(icmd)%hin_lat + frac_in * ob(iob)%hd(4)
                if (cs_db%num_tot > 0) then
                  obcs(icmd)%hin_lat = obcs(icmd)%hin_lat + frac_in * obcs(iob)%hd(4)
                end if
              else
                ! if hyd in is not a total hyd from an hru or ru -> add the specified hyd typ 
                select case (ob(icmd)%htyp_in(in))
                case ("tot")   ! total flow
                  ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(ihyd)
                  !add constituents
                  if (cs_db%num_tot > 0) then
                    obcs(icmd)%hin_til = obcs(icmd)%hin_til + frac_in * obcs(iob)%hd(ihyd)
                  end if
                case ("sur")   ! surface runoff
                  ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(ihyd)
                  !add constituents
                  if (cs_db%num_tot > 0) then
                    obcs(icmd)%hin_sur = obcs(icmd)%hin_sur + frac_in * obcs(iob)%hd(ihyd)
                  end if
                case ("lat")   ! lateral soil flow
                  ob(icmd)%hin_lat = ob(icmd)%hin_lat + frac_in * ob(iob)%hd(ihyd)
                  !add constituents
                  if (cs_db%num_tot > 0) then
                    obcs(icmd)%hin_lat = obcs(icmd)%hin_lat + frac_in * obcs(iob)%hd(ihyd)
                  end if
                case ("til")   ! tile flow
                  ob(icmd)%hin_til = ob(icmd)%hin_til + frac_in * ob(iob)%hd(ihyd)
                  !add constituents
                  if (cs_db%num_tot > 0) then
                    obcs(icmd)%hin_til = obcs(icmd)%hin_til + frac_in * obcs(iob)%hd(ihyd)
                  end if
                case ("aqu")   ! aquifer inflow
                  ob(icmd)%hin_aqu = ob(icmd)%hin_aqu + frac_in * ob(iob)%hd(ihyd)
                  !add constituents
                  if (cs_db%num_tot > 0) then
                    obcs(icmd)%hin_aqu = obcs(icmd)%hin_aqu + frac_in * obcs(iob)%hd(ihyd)
                  end if
                end select
              end if
              
            else
              ! all objects other than hru's
              ! fraction of organics
              ht1 = frac_in * ob(iob)%hd(ihyd)
              ob(icmd)%hin = ob(icmd)%hin + ht1

              ! fraction of constituents
              if (cs_db%num_tot > 0) then
                hcs1 = frac_in * obcs(iob)%hd(ihyd)
                obcs(icmd)%hin = obcs(icmd)%hin + hcs1
              end if
              ob(icmd)%hin_d(in) = ht1        !for hydrograph output
              obcs(icmd)%hcsin_d(in) = hcs1   !for constituent hydrograph output
            end if

          end do    ! in = 1, ob(icmd)%rcv_tot

          !convert to per area basis
          if (ob(icmd)%typ == "hru" .or. ob(icmd)%typ == "ru") then  !only convert hru and subbasin hyds for routing
            !if (ob(icmd)%ru_tot > 0) then
            !  !object is in a subbasin
            !  ielem = ob(icmd)%elem
            !  iru = ob(icmd)%ru(1)  !can only be in one subbasin if routing over
            !  conv = 100. * ru(iru)%da_km2  !* ru_elem(ielem)%frac
            !else
              conv = ob(icmd)%area_ha
            !end if
            ob(icmd)%hin_sur = ob(icmd)%hin_sur // conv
            ob(icmd)%hin_lat = ob(icmd)%hin_lat // conv
            ob(icmd)%hin_til = ob(icmd)%hin_til // conv
          end if
        end if

        ! select the next command type
        select case (ob(icmd)%typ)
            
          case ("hru")   ! hru
            ihru = ob(icmd)%num
            call hru_control1 (icmd)

          case ("ru")   ! subbasin
            iru = ob(icmd)%num
            call ru_control (icmd)

          case ("aqu")   ! aquifer
            if (ob(icmd)%dfn_tot == 0) then   !1-D use old bf recession
              call aquifer_control1 (icmd)
            end if
          
          case ("chan")   ! channel
            jrch = ob(icmd)%num
            jrchq = ob(icmd)%props2
            
          case ("res")   ! reservoir
            ires = ob(icmd)%num
            if (ob(icmd)%rcv_tot > 0) then
              call reservoir_control1 (icmd)
            end if 
  
          case ("recall")   ! recall hydrograph
            irec = ob(icmd)%num
            ob(icmd)%hd(1) = recall(irec)
                
          !case ("exco")   ! export coefficient hyds are set at start

          case ("dr")   ! delivery ratios
            ob(icmd)%hd(1) = ob(icmd)%hin ** dr(ob(icmd)%props) ! ** is an intrinsic function to multiply 
            if (cs_db%num_tot > 0) then
              idr = ob(iob)%props
              
              call constit_hyd_mult (icmd, idr)
            end if
            
          case ("outlet")  !outlet
            ob(icmd)%hd(1) = ob(icmd)%hin
            write (1981, 300) ob(icmd)%props, ob(icmd)%name, ob(icmd)%area_ha,  &
                ' INFLOW     ', ob(iob)%hin
              
          case ("chandeg")  !swatdeg channel
            isdch = ob(icmd)%num
            isd_chsur = ob(icmd)%props2
            call cha_control1 (icmd, sp_ob%chandeg)
          end select

        !set the next command
        icmd = ob(icmd)%cmd_next
        
      end do
              
        !print all hydrographs
        do icmd = 1, sp_ob%objs
          write (1983,"((A16),3x(A8),2(I4,12x))") ob(icmd)%name, ob(icmd)%typ,  &
              ob(icmd)%rcv_tot, ob(icmd)%src_tot
        
          !print all inflow hydrographs
          do iin = 1, ob(icmd)%rcv_tot
             write (1983,"(56XA4,9xA8,5xI4,11xA3,7x,*(G16.4,2x))") "in ",       &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin),              &
                 ob(icmd)%htyp_in(iin), ob(icmd)%frac_in(iin), ob(icmd)%hin_d(iin)
          end do
             
          !print all outflow hydrographs
          do iout = 1, ob(icmd)%src_tot
            ihtyp = ob(icmd)%ihtyp_out(iout)
            ht1 = ob(icmd)%frac_out(iout) * ob(icmd)%hd(ihtyp)
            write (1983,"(56XA4,9xA8,5xI4,11xA3,7x,*(G16.4,2x))") "out ",       &
                ob(icmd)%obtyp_out(iout), ob(icmd)%obtypno_out(iout),           &
                ob(icmd)%htyp_out(iout), ob(icmd)%frac_out(iout), ht1
          end do
        end do
        
      call output_hru
      call output_basin
      call output_lsu
      
      return
      end