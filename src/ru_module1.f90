      module ru_module1
    
      implicit none

      type routing_unit1
        character(len=25) :: name = ""
        integer :: obj_no
        real :: area_ha
        real :: da_km2
      end type routing_unit1
      type (routing_unit1), dimension(:), allocatable, target :: ru1
      
      end module ru_module1