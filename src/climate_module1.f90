      module climate_module1
     
      implicit none

      type climate
        character(len=25) :: name = ""
        real :: precip_mm
        real :: pet_mm
      end type climate
      type (climate), dimension(:), allocatable, target :: cli

      end module climate_module1