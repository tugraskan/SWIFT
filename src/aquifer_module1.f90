      module aquifer_module1
    
      implicit none

      type aquifer1
        character(len=25) :: name = ""
        integer :: obj_no
        real :: area_ha
        real :: km
      end type aquifer1
      type (aquifer1), dimension(:), allocatable, target :: aqu1

      end module aquifer_module1