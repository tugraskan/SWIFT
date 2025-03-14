      module exco_module1
    
      implicit none

      type export_coefficient1
        character(len=25) :: name = ""
        integer :: obj_no
        real :: area_ha
        real :: km
      end type export_coefficient1
      type (export_coefficient1), dimension(:), allocatable, target :: exco1

      end module exco_module1 