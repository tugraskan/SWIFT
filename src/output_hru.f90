      subroutine output_hru
    
      use hru_module1
      use hydrograph_module  !, only : sp_ob, sp_ob1, ob, hru_ex, hru_dr, hru_bmp
      use input_file_module
    
      implicit none
      
      integer :: ihru           !none       |hru counter
      integer :: i              !none       |counter
      
      
      200   format((A4,10x1A5,6x1A5),6x,5(1a16,6x,8a16))                            !header format                               
      300   format((A4,1I,1F16.4),4x,*(1a16,8G16.4,6x))                             !hru format
            
      !! open hru.dat file
      open (1984,file="hru_ex.out",recl=1200)
      
      write (1984,*) bsn%name, prog                                                 !write file name      
      write (1984, 200) obj_hdr%num, obj_hdr%name,  &                               !write header
          obj_hdr%area, &
          ' TOTAL      ', hdr_exco, ' PERCO      ', hdr_exco, &
          ' SURFACE    ', hdr_exco, ' LATERAL    ', hdr_exco, &
          ' TILE       ', hdr_exco
          
      write (1984, 200) " --  ", " --  ", " ha  ",  &                               !write header units
          ('---        ', hdr_exco_unt,    i = 1, 5)               
      
      !! write hru data
      do ihru = 1, sp_ob%hru
        
        call hyd_convert__mass_to_ha (ob(ihru)%hd(1), hru_dat(ihru)%area_ha)
        call hyd_convert__mass_to_ha (ob(ihru)%hd(2), hru_dat(ihru)%area_ha)
        call hyd_convert__mass_to_ha (ob(ihru)%hd(3), hru_dat(ihru)%area_ha)
        call hyd_convert__mass_to_ha (ob(ihru)%hd(4), hru_dat(ihru)%area_ha)
        call hyd_convert__mass_to_ha (ob(ihru)%hd(5), hru_dat(ihru)%area_ha)
       
        write (1984, 300) ' HRU ', ihru, hru_dat(ihru)%area_ha, &
        ' TOTAL      ', ob(ihru)%hd(1), &
        ' PERCO      ', ob(ihru)%hd(2), &        
        ' SURFACE    ', ob(ihru)%hd(3), &      
        ' LATERAL    ', ob(ihru)%hd(4), &      
	    ' TILE       ', ob(ihru)%hd(5) 

          
      end do
      

      
      close (1984)
      
	  return
      end subroutine output_hru