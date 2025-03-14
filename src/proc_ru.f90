      subroutine proc_ru
    
      use ru_module1
      use hydrograph_module, only : sp_ob, sp_ob1, ob
    
      implicit none
      
      integer :: iru           !none       |hru counter
      integer :: iob            !none       |object counter
      integer :: ihru_db        !none       |number of hru data
      integer :: ipro           !none       |number of process (total runoff,surf,perc,lat,tile)
      integer :: iwst           !none       |precip gage number
      integer :: i, k           !none       |variables used to set data number
      integer :: iostat, eof
      character (len=80) :: titldum   !           |title of file
      
      allocate (ru1(sp_ob%ru))
      
      do iru = 1, sp_ob%ru
        ru1(iru)%obj_no = sp_ob1%ru + iru - 1
        ru1(iru)%area_ha = ob(ru1(iru)%obj_no)%area_ha
        ru1(iru)%da_km2 = ru1(iru)%area_ha / 100.
      end do
      
	  return
      end subroutine proc_ru