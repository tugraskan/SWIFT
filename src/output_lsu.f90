      subroutine output_lsu
    
      use hru_module1
      use climate_module1
      use hydrograph_module  !, only : sp_ob, sp_ob1, ob, hru_ex, hru_dr, hru_bmp
      use input_file_module
      use maximum_data_module
      use calibration_data_module
    
      implicit none
      
      integer :: ihru           !none       |hru counter
      integer :: ielem          !none       |hru counter
      integer :: iob            !none       |object counter
      integer :: ihru_db        !none       |number of hru data
      integer :: ipro           !none       |number of process (total runoff,surf,perc,lat,tile)
      !integer :: iwst          !none       |precip gage number
      integer :: i, k           !none       |variables used to set data number
      integer :: iostat, eof
      real :: lsu_area
      real :: precip_lsu

      200   format((2xA4,1x1A5,15x1A5,10xA16),5(1a16,6x,8a16))                              !header format
      300   format((I4,4x1A,2F16.4),2x,5(1a16,8G16.4,6x))                                   !data format
      
      !! open hru.dat file
      open (107,file = "lsu_ex.out",recl=1200)
      write (107,*) bsn%name, prog                                                          !write file name
      write (107, 200) obj_hdr%num, obj_hdr%name,  &                                        !write header
          obj_hdr%area, ' PRECIPITATION  ', &
          ' TOTAL      ', hdr_exco, ' PERCO      ', hdr_exco, &
          ' SURFACE    ', hdr_exco, ' LATERAL    ', hdr_exco, &
          ' TILE       ', hdr_exco
      write (107, 200) " --  ", " --  ", " ha  "," --  ",   &                               !write header units
          ('---        ', hdr_exco_unt,    i = 1, 5)     
      
      !! sum lsu data
      do iob = 1, db_mx%lsu_out
        lsu_area = 0.
        precip_lsu = 0.
        do ielem = 1,  lsu_out(iob)%num_tot
          do ipro = 1, 5
            ihru = lsu_out(iob)%num(ielem)
            lsu_area = lsu_area + lsu_elem(ihru)%ru_frac * hru_dat(ihru)%precip
            precip_lsu = precip_lsu + lsu_elem(ihru)%ru_frac * hru_dat(ihru)%precip
            lsu_ex(iob)%hyd(ipro) = lsu_ex(iob)%hyd(ipro) + lsu_elem(ihru)%ru_frac * ob(ihru)%hd(ipro)
          end do
        end do
        
        
	  write (107, 300) iob, lsu_out(iob)%name, lsu_area, precip_lsu, &
        ' TOTAL      ', lsu_ex(iob)%hyd(1), &
        ' PERCO      ', lsu_ex(iob)%hyd(2), &
        ' SURFACE    ', lsu_ex(iob)%hyd(3), &
        ' LATERAL    ', lsu_ex(iob)%hyd(4), &
        ' TILE       ', lsu_ex(iob)%hyd(5)
      end do
      
      
      close (107)
      
	  return
      end subroutine output_lsu