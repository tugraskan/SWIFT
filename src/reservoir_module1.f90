      module reservoir_module1
    
      implicit none

      type reservoir_data
        real :: psa = 0.          !ha            |res surface area when res is filled to princ spillway
        real :: pvol = 0.         !ha-m          |vol of water needed to fill the res to the princ spillway (read in as ha-m
                                  !                and converted to m^3)
        real :: esa = 0.          !ha            |res surface area when res is filled to emerg spillway 
        real :: evol = 0.         !ha-m          |vol of water needed to fill the res to the emerg spillway (read in as ha-m
                                  !                and converted to m^3)
      end type reservoir_data
      
      type reservoir1
        character(len=25) :: name = ""
        integer :: obj_no
        real :: area_ha
        real :: km
        type (reservoir_data) :: res_db
      end type reservoir1
      type (reservoir1), dimension(:), allocatable, target :: res1
      
      type res_floout_header      
        character (len=12) :: ires        =   "      ires  "        
        character (len=25) :: name        =   "      name               "                                             
        character (len=12) :: obj_no      =   "    obj_no  "
        character (len=12) :: area_ha     =   "    area_ha "
		character (len=15) :: km          =   "           km  "
        character (len=15) :: psa         =   "          psa  "        
        character (len=15) :: pvol        =   "         pvol  "                                             
        character (len=15) :: esa         =   "          esa  "
        character (len=15) :: evol        =   "         evol  "
        character (len=15) :: flo         =   "          flo  "        
        character (len=15) :: residence   =   "    residence  "                                             
        character (len=15) :: trap_eff    =   "     trap_eff  "
      end type res_floout_header    
      type (res_floout_header) :: hdr_res_floout 
      
      type res_floout_unt_header
        character (len=12) :: ires        =   "            "        
        character (len=25) :: name        =   "                         "                                             
        character (len=12) :: obj_no      =   "            "
        character (len=12) :: area_ha     =   "            "
		character (len=15) :: km          =   "               "
        character (len=15) :: psa         =   "           ha  "        
        character (len=15) :: pvol        =   "          m^3  "                                             
        character (len=15) :: esa         =   "           ha  "
        character (len=15) :: evol        =   "          m^3  "
        character (len=15) :: flo         =   "          m^3  "        
        character (len=15) :: residence   =   "        m3/m3  "                                             
        character (len=15) :: trap_eff    =   "         frac  "
      end type res_floout_unt_header   
      type (res_floout_unt_header) :: hdr_res_floout_unt 

      end module reservoir_module1