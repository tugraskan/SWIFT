      module hydrograph_module
      
      implicit none
      
      character(len=80) :: prog                                          ! dynamic header variable
      integer :: mhyd                                                    !none          |max number of hydrographs
      integer :: mcmd                                                    !              |
      integer :: inum2                                                   !none          |inflow hydrograph storage location number
      integer :: jrch                                                    !none          |reach number
      integer :: jrchq                                                   !              |
      integer :: mrte                                                    !              |
      integer :: ihout                                                   !none          |outflow hydrograph storage location number
      integer :: iwst                                                    !              | 
      integer :: isdch                                                   !              |
      integer :: icmd                                                    !              |
      integer :: ich                                                     !none          |object number 
      integer :: mobj_out                                                !none          |end of loop
      integer :: isd_chsur                                               !              | 
      integer, dimension (:), allocatable :: rcv_sum                     !              |
      integer, dimension (:), allocatable :: dfn_sum                     !              |
      integer, dimension (:), allocatable :: elem_cnt                    !              |
      integer, dimension (:), allocatable :: defunit_num                 !              |
      integer, dimension (:), allocatable :: ru_seq                      !              |
      real, dimension (:), allocatable :: hyd_km2                        !              |  
      integer, dimension (:), allocatable :: ob_order                    !              |
      real, dimension(:,:,:), allocatable:: rchhr                        !              |
            
      type basin_inputs
        character(len=25) :: name
        real :: area_ls_ha = 0.
        real :: area_tot_ha = 0. 
      end type basin_inputs
      type (basin_inputs) :: bsn
      
      type hyd_output
        real :: flo = 0.               !! m^3           |volume of water
        real :: sed = 0.               !! metric tons   |sediment
        real :: orgn = 0.              !! kg N          |organic N
        real :: sedp = 0.              !! kg P          |organic P
        real :: no3 = 0.               !! kg N          |NO3-N
        real :: solp = 0.              !! kg P          |mineral (soluble P)
        real :: nh3 = 0.               !! kg N          |NH3
        real :: no2 = 0.               !! kg N          |NO2
      end type hyd_output
      
      !! hru export coefficients - dimension to number of hrus
      type hru_export_coefficients
        type (hyd_output), dimension (5) :: runoff      !export coefficients - tot,perc,surf,lat,tile
      end type hru_export_coefficients
      type (hru_export_coefficients), dimension(:), allocatable :: hru_ex

      !! hru bmp efficiencies - dimension to number in data file
      type hru_bmp_efficiencies
        type (hyd_output), dimension (5) :: eff         !bmp efficiencies - tot,perc,surf,lat,tile
      end type hru_bmp_efficiencies
      type (hru_bmp_efficiencies), dimension(:), allocatable :: hru_bmp
      
      !! hru delivery ratios when routing across the hru - dimension to number in data file
      type hru_delivery_ratios
        type (hyd_output), dimension (5) :: dr          !delivery ratios - tot,perc,surf,lat,tile
      end type hru_delivery_ratios
      type (hru_delivery_ratios), dimension(:), allocatable :: hru_dr

      type cha_exco_del_ratios
        type (hyd_output) :: bank_ero           !bank erosion
        type (hyd_output) :: bed_ero            !bed erosion
        type (hyd_output) :: bed_dep            !bed deposition
        type (hyd_output) :: ch_rout            !in channel routing losses
        type (hyd_output) :: fp_dep             !flood plain deposition
        type (hyd_output) :: instr_dr           !instream delivery ratios
        type (hyd_output) :: trans              !instream transfomations
      end type cha_exco_del_ratios
      type (cha_exco_del_ratios), dimension(:), allocatable :: cha2

      type cha_instream_del_ratios
        character(len=25) :: name = "default"
        type (hyd_output) :: instr_dr           !instream delivery ratios
      end type cha_instream_del_ratios
      type (cha_instream_del_ratios), dimension(:), allocatable :: cha_dr

      type res_exco_del_ratios
        character(len=25) :: name = "default"
        type (hyd_output) :: trap              !aquifer gain/losses
      end type res_exco_del_ratios
      type (res_exco_del_ratios), dimension(:), allocatable :: res_dr

      type aqu_exco_del_ratios
        type (hyd_output) :: trap              !aquifer gain/losses
      end type aqu_exco_del_ratios
      type (aqu_exco_del_ratios), dimension(:), allocatable :: aqu2

      type (hyd_output), dimension(:), allocatable :: recall        !treatment/industrial, gage data, or model output
      
      type (hyd_output), dimension(5) :: bsn_hru_out                !basin landscape (hru) output
      type basin_channel_budget
        type (hyd_output) :: incoming           !water, sediment, and nutrients entering channels
        type (hyd_output) :: outgoing           !water, sediment, and nutrients leaving channels
        type (hyd_output) :: bank_ero           !bank erosion
        type (hyd_output) :: bed_ero            !bed erosion
        type (hyd_output) :: bed_dep            !bed deposition
        type (hyd_output) :: fp_dep             !flood plain deposition
      end type basin_channel_budget
      type (cha_exco_del_ratios) :: bsn_ch_out
        type (hyd_output) :: outgoing           !water, sediment, and nutrients leaving channels
        type (hyd_output) :: incoming           !water, sediment, and nutrients entering channels

      type (hyd_output), dimension(5) :: bsn_ps_out                 !basin point source output
      type (hyd_output), dimension(5) :: bsn_res_out                !basin reservoir output
      
      type lsu_deliveries
        character(len=25) :: name = "default"
        type (hyd_output), dimension(5) :: hyd           !instream delivery ratios
      end type lsu_deliveries
      type (lsu_deliveries), dimension(:), allocatable :: lsu_ex
      
      type (hyd_output), dimension(:), allocatable :: exco2

      type (hyd_output), dimension(:), allocatable :: hd
      type (hyd_output), dimension(:), allocatable :: rec_d
      type (hyd_output), dimension(:), allocatable :: rec_m
      type (hyd_output), dimension(:), allocatable :: rec_y
      type (hyd_output), dimension(:), allocatable :: rec_a
      type (hyd_output), dimension(:), allocatable :: srec_d
      type (hyd_output), dimension(:), allocatable :: srec_m
      type (hyd_output), dimension(:), allocatable :: srec_y
      type (hyd_output), dimension(:), allocatable :: srec_a
      type (hyd_output), dimension(:), allocatable :: ru_d
      type (hyd_output), dimension(:), allocatable :: ru_m
      type (hyd_output), dimension(:), allocatable :: ru_y
      type (hyd_output), dimension(:), allocatable :: ru_a
      type (hyd_output) :: brec_d, brec_m, brec_y, brec_a
      type (hyd_output) :: bru_d, bru_m, bru_y, bru_a
      type (hyd_output) :: binhyd_d
      type (hyd_output) :: hz
      type (hyd_output) :: dr1
      type (hyd_output), dimension(:),allocatable :: hcnst
      type (hyd_output), dimension(:),allocatable :: hhr
      type (hyd_output) :: ht1, ht2, ht3, ht4, ht5, delrto

      type object_connectivity
        character(len=16) :: name = "default"
        character(len=8) :: typ = " "   !object type - ie hru, hru_lte, sub, chan, res, recall
        integer :: nhyds                !hru=5, chan=3 - see type hd_tot for each object
        real :: lat                     !latitude (degrees)
        real :: long                    !longitude (degrees)
        real :: elev = 100.             !elevation (m)
        real :: area_ha = 80.           !input drainag area - ha
        real :: area_ha_calc = 80.      !calculated drainage area-ha. only for checking - doesn't work if routing across landscape
        integer :: props = 1            !properties number from data base (ie hru.dat, sub.dat) - change props to data
        character (len=50) ::  wst_c    !weather station name
        integer :: wst = 1              !weather station number
        integer :: constit              !constituent data pointer to pesticides, pathogens, metals, salts
        integer :: props2               !overbank connectivity pointer to landscape units - change props2 to overbank
        character(len=16) :: ruleset    !points to the name of the dtbl in flo_con.dtl for out flow control
        integer :: flo_dtbl             !dtbl pointer for flow fraction of hydrograph
        integer :: num = 1              !spatial object number- ie hru number corresponding to sequential command number
                                        !this is the first column in hru_dat (doesn"t have to be sequential)
        integer*8 :: gis_id             !gis number for database purposes
        integer :: fired = 0            !0=not fired; 1=fired off as a command
        integer :: cmd_next = 0         !next command (object) number
        integer :: cmd_prev = 0         !previous command (object) number
        integer :: cmd_order = 0        !1=headwater,2=2nd order,etc
        integer :: src_tot = 0          !total number of outgoing (source) objects
        integer :: rcv_tot = 0          !total number of incoming (receiving) hydrographs
        integer :: dfn_tot = 0          !total number of defining objects (ie hru"s within a subbasin)
        integer :: ru_tot               !number of routing units that contain this object
        integer,  dimension (:), allocatable :: ru                  !subbasin the element is in
        integer :: elem                 !subbasins element number for this object- used for routing over (can only have one)
        integer :: flood_ch_lnk = 0     !channel the landscape unit is linked to
        integer :: flood_ch_elem = 0    !landscape unit number - 1 is nearest to stream
        integer :: flood_frac = 0       !fraction of flood flow assigned to the object
        character (len=3), dimension (:), allocatable :: obtyp_out  !outflow object type (ie 1=hru, 2=sd_hru, 3=sub, 4=chan, etc)
        integer, dimension(:), allocatable :: obtypno_out           !outflow object type name
        integer, dimension(:), allocatable :: obj_out               !outflow object
        character (len=3), dimension (:), allocatable :: htyp_out   !outflow hyd type (ie 1=tot, 2= recharge, 3=surf, etc)
        integer, dimension (:), allocatable :: ihtyp_out            !outflow hyd type (ie 1=tot, 2= recharge, 3=surf, etc)
        real, dimension (:), allocatable :: frac_out                !fraction of hydrograph
        character(len=8), dimension(:), allocatable :: obtyp_in     !inflow object type (ie 1=hru, 2=sd_hru, 3=sub, 4=chan, etc)
        integer, dimension(:), allocatable :: obtypno_in            !inflow object type number
        integer, dimension(:), allocatable :: obj_in
        character (len=3), dimension(:), allocatable :: htyp_in     !inflow hyd type (ie 1=tot, 2= recharge, 3=surf, etc)
        integer, dimension(:), allocatable :: ihtyp_in
        real, dimension(:), allocatable :: frac_in
        integer, dimension(:), allocatable :: rcvob_inhyd           !inflow hydrograph number of recieving object - used for dtbl flow fractions
        type (hyd_output) :: hin                                            !inflow hydrograph for surface runon - sum of all inflow hyds
        type (hyd_output) :: hin_sur                                        !inflow hydrograph for surface runoff - sum of all surface inflow hyds
        type (hyd_output) :: hin_lat                                        !inflow hydrograph for lateral soil flow - sum of all lateral inflow hyds
        type (hyd_output) :: hin_til                                        !inflow hydrograph for tile flow - sum of all tile inflow hyds
        type (hyd_output) :: hin_aqu                                        !inflow hydrograph for aquifer flow - sum of all aquifer inflow hyds
        type (hyd_output), dimension(:), allocatable :: hd                  !generated hydrograph (ie 1=tot, 2= recharge, 3=surf, etc)
        type (hyd_output) :: supply                                         !water supply allocation
        real :: demand                                                      !water irrigation demand (ha-m)
        integer :: day_cur = 0                                              !current hydrograph day in ts
        integer :: day_max                                                  !maximum number of days to store the hydrograph
        real :: peakrate                                                    !peak flow rate during time step - m3/s
        
        type (hyd_output), dimension(:),allocatable :: hin_d
        type (hyd_output), dimension(:),allocatable :: hin_m
        type (hyd_output), dimension(:),allocatable :: hin_y
        type (hyd_output), dimension(:),allocatable :: hin_a
        type (hyd_output), dimension(:),allocatable :: hout_m
        type (hyd_output), dimension(:),allocatable :: hout_y
        type (hyd_output), dimension(:),allocatable :: hout_a
        type (hyd_output) :: hdep_m
        type (hyd_output) :: hdep_y
        type (hyd_output) :: hdep_a
        
        integer, dimension(:), allocatable :: obj_subs                      !subbasins object number that contain this object
      end type object_connectivity
      type (object_connectivity), dimension(:), allocatable, save :: ob

      type spatial_objects
        integer :: objs = 0      !number of objects or 1st object command
        integer :: hru = 0       !1-number of hru"s or 1st hru command
        integer :: hru_lte = 0   !2-number of hru_lte"s or 1st hru_lte command
        
        integer :: ru = 0        !3-number of ru"s or 1st ru command
        integer :: gwflow = 0    !4-number of gwflow"s or 1st gwflow command !rtb gwflow
        integer :: aqu = 0       !5-number of aquifer"s or 1st aquifer command
        integer :: chan = 0      !6-number of chan"s or 1st chan command
        integer :: res = 0       !7-number of res"s or 1st res command
        integer :: recall = 0    !8-number of recdays"s or 1st recday command
        integer :: exco = 0      !11-number of exco"s or 1st export coeff command
        integer :: dr = 0        !12-number of dr"s or 1st del ratio command
        integer :: canal = 0     !13-number of canal"s or 1st canal command
        integer :: pump = 0      !14-number of pump"s or 1st pump command
        integer :: outlet = 0    !15-number of outlet"s or 1st outlet command
        integer :: chandeg = 0   !16-number of swat-deg channel"s or 1st swat-deg channel command
        integer :: aqu2d = 0     !17-number of 2D aquifer"s or 1st 2D aquifer command
        integer :: herd = 0      !18-number of herds
        integer :: wro = 0       !19-number of water rights
      end type spatial_objects
      type (spatial_objects) :: sp_ob       !total number of the object
      type (spatial_objects) :: sp_ob1      !first sequential number of the object
            
      type object_total_hydrographs
        integer :: hru = 5          !1=total 2=recharge 3=surface 4=lateral 5= tile
        integer :: hru_lte = 5      !1=total 2=recharge 3=surface 4=lateral 5= tile
        integer :: ru = 5           !1=total 2=recharge 3=surface 4=lateral 5= tile
        integer :: gwflow = 1       !1=total
        integer :: aqu = 2          !1=return flow 2=deep perc
        integer :: chan = 3         !1=total 2=recharge 3=overbank
        integer :: res = 2          !1=total 2=recharge 
        integer :: recall = 1       !1=total
        integer :: exco = 2         !1=surface 2=groundwater
        integer :: dr = 2           !1=surface 2=groundwater
        integer :: pump = 1         !1=total
        integer :: outlet = 1       !1=total
        integer :: chandeg = 3      !1=total 2=recharge 3=overbank
        integer :: aqu2d = 2        !1=return flow 3=deep perc
        integer :: herd = 1
        integer :: wro = 1
      end type object_total_hydrographs
      type (object_total_hydrographs) :: hd_tot
      
      type routing_unit_data
        character(len=16) :: name
        integer :: num_tot
        integer, dimension (:), allocatable :: num             !points to subbasin element (sub_elem)
      end type routing_unit_data
      type (routing_unit_data),dimension(:), allocatable:: ru_def
      
      type routing_unit_elements
        character(len=16) :: name
        integer :: obj = 1              !object number
        character (len=3) :: obtyp      !object type- 1=hru, 2=hru_lte, 11=export coef, etc
        integer :: obtypno = 0          !2-number of hru_lte"s or 1st hru_lte command
        real :: frac = 0                !fraction of element in ru (expansion factor)
        character(len=16) :: dr_name    !name of dr in delratio.del
        type (hyd_output) :: dr         !calculated (or input in delratio.del) dr's for element
      end type routing_unit_elements
      type (routing_unit_elements), dimension(:), allocatable :: ru_elem
      
      integer,  dimension(:), allocatable :: ielem_ru   !sequential counter for ru the hru is in

      !export coefficient is hyd_output type but not part of an object 
      type (hyd_output), dimension(:), allocatable :: dr          !delivery ratio for objects- chan, res, lu

      !delevery ratio is hyd_output type but not part of an object 
      type (hyd_output), dimension(:), allocatable :: exco        !export coefficient

      type hyd_header                                       
        character (len=17) :: flo  =    "              flo"      !! ha-m         |volume of water
        character (len=15) :: sed  =    "            sed"        !! metric tons  |sediment
        character (len=15) :: orgn =    "           orgn"        !! kg N         |organic N
        character (len=15) :: sedp =    "           sedp"        !! kg P         |organic P
        character (len=15) :: no3  =    "            no3"        !! kg N         |NO3-N
        character (len=15) :: solp =    "           solp"        !! kg P         |mineral (soluble P)
        character (len=15) :: chla =    "           chla"        !! kg           |chlorophyll-a
        character (len=15) :: nh3  =    "            nh3"        !! kg N         |NH3
        character (len=15) :: no2  =    "            no2"        !! kg N         |NO2
        character (len=15) :: cbod =    "           cbod"        !! kg           |carbonaceous biological oxygen demand
        character (len=15) :: dox  =    "            dox"        !! kg           |dissolved oxygen
        character (len=15) :: san  =    "            san"        !! tons         |detached sand
        character (len=15) :: sil  =    "            sil"        !! tons         |detached silt
        character (len=15) :: cla  =    "            cla"        !! tons         |detached clay
        character (len=15) :: sag  =    "            sag"        !! tons         |detached small ag
        character (len=15) :: lag  =    "            lag"        !! tons         |detached large ag
        character (len=15) :: grv  =    "            grv"        !! tons         |gravel
        character (len=15) :: temp =    "           null"        !! deg c        |temperature
      end type hyd_header
      type (hyd_header) :: hyd_hdr
      
      type hyd_header2                                       
        character (len=16) :: flo  =    " flo            "        !! m^3           |volume of water
        character (len=16) :: sed  =    " sed            "        !! metric tons   |sediment
        character (len=16) :: orgn =    " orgn           "        !! kg_N          |organic N
        character (len=16) :: sedp =    " sedp           "        !! kg_P          |organic P
        character (len=16) :: no3  =    " no3            "        !! kg_N          |NO3-N
        character (len=16) :: solp =    " solp           "        !! kg_P          |mineral (soluble P)
        character (len=16) :: nh3  =    " nh3            "        !! kg_N          |NH3
        character (len=16) :: no2  =    " no2            "        !! kg_N          |NO2
      end type hyd_header2
      type (hyd_header2) :: hyd_hdr2
                  
      type hyd_header_units        
        character(len=16) :: flo   =    " m^3            "         ! (m^3)
        character(len=16) :: sed   =    " t/ha           "         ! (t/ha)
        character(len=16) :: orgn  =    " kg/ha          "         ! (kg)
        character(len=16) :: orgp  =    " kg/ha          "         ! (kg)
        character(len=16) :: no3   =    " kg/ha          "         ! (kg)       
        character(len=16) :: minp  =    " kg/ha          "         ! (kg)
        character(len=16) :: nh3   =    " kg/ha          "         ! (kg)    
        character(len=16) :: no2   =    " null           "         ! (kg)
      end type hyd_header_units
      type (hyd_header_units) :: hyd_hdr_unt
            
      type :: hydcon_header
        character(len=20)   :: iob        =     " iob"          !! na   | Spatial object number
        character(len=20)   :: typ        =     " type"         !! na   | Object type
        character(len=20)   :: num        =     " num"          !! na   | Spatial object number
        character(len=20)   :: area       =     " area"         !! ha   | Input drainage area in hectares
        character(len=20)   :: area_calc  =     " area_calc"    !! ha   | Calculated drainage area in hectares 
        character(len=20)   :: rcv_tot    =     " rcv_tot"      !! na   | Total number of incoming (receiving) hydrographs
      end type hydcon_header
      type (hydcon_header) ::hydcon_hdr
      
      type :: hydcon_in_header  
        character(len=20)   :: obtyp      =     " Inflow_typ"   !! na   | Inflow object type
        character(len=20)   :: typ_num    =     " In_num"       !! na   | Inflow object
        character(len=20)   :: obj_in     =     " In_iob"       !! na   | Inflow iob
        character(len=20)   :: frac_in    =     " frac_in"      !! na   | Fraction of inflow hydrograph
        character(len=20)   :: area       =     " area"         !! ha   | Input drainage area in hectares
        character(len=20)   :: area_calc  =     " area_calc"    !! ha   | Calculated drainage area in hectares 
      end type hydcon_in_header
      type (hydcon_in_header) ::hydcon_in_hdr
      
      type :: hydcon_header_units
        character(len=20)   :: iob        =     " na"           !! na   |Spatial object number
        character(len=20)   :: typ        =     " na"           !! na   |Object type
        character(len=20)   :: num        =     " na"           !! ha   |Spatial object number
        character(len=20)   :: area       =     " ha"           !! na   |Input drainage area in hectares
        character(len=20)   :: area_calc  =     " ha"           !! ha   |Calculated drainage area in hectares (for checking)
        character(len=20)   :: rcv_tot    =     " na"           !! na   |Total number of incoming (receiving) hydrographs
      end type hydcon_header_units
      type (hydcon_header_units) ::hydcon_hdr_units
      
      type :: obj_header  
        character(len=16)   :: num        =     " num           "     !! na   | object name   
        character(len=16)   :: name       =     " name          "     !! na   | object type
        character(len=16)   :: area       =     " area          "     !! ha   | area_ha
        character(len=16)   :: inflow     =     " INFLOW        "     !! na   | Inflow
        character(len=16)   :: outflow    =     " OUTFLOW       "     !! na   | Outflow
        character(len=16)   :: bank       =     " BANK          "     !! na   | BANK
        character(len=16)   :: bed        =     " BED           "     !! na   | BED
        character(len=16)   :: bed_dep    =     " BED_DEP       "     !! na   | BED_DEP
        character(len=16)   :: fp_dep     =     " FP_DEP        "     !! na   | FP_DEP
      end type obj_header
      type (obj_header) ::obj_hdr
      
      type :: obj_header2  
        character(len=16)   :: name   =         " name"         !! na   | object name   
        character(len=16)   :: typ    =         " typ"          !! na   | object type
        character(len=16)   :: rcv_tot    =     " rcv_tot"      !! na   | recieving hydrograph total
        character(len=16)   :: src_tot    =     " src_tot"      !! na   | src hydrograph total
        character(len=16)   :: frac_in    =     " frac_in"      !! na   | fraction of hydrograph
      end type obj_header2
      type (obj_header2) ::obj_hdr2
      
      type :: obj_header3  
        character(len=16)   :: TOTAL        =       " TOTAL"        !! na   | TOTAL   
        character(len=16)   :: PERCO        =       " PERCO"        !! na   | PERCO
        character(len=16)   :: SURFACE      =       " SURFACE"      !! na   | SURFACE
        character(len=16)   :: LATERAL      =       " LATERAL"      !! na   | LATERAL
        character(len=16)   :: TILE         =       " TILE"         !! na   | TILE
      end type obj_header3
      type (obj_header3) ::obj_hdr3
    
      
      interface operator (+)
        module procedure hydout_add
      end interface
             
      interface operator (-)
        module procedure hydout_subtract
      end interface
            
      interface operator (**)
        module procedure hydout_mult
        end interface 
      
      interface operator (.add.)
        module procedure hydout_add_const
      end interface 

      interface operator (*)
        module procedure hydout_mult_const
      end interface 

      interface operator (/)
        module procedure hydout_div_const
      end interface   
             
      interface operator (//)
        module procedure hydout_div_conv
      end interface   
             
      contains

      !! function to convert mass to concentration
      subroutine hyd_convert_conc_to_mass (hyd1)
        type (hyd_output), intent (inout) :: hyd1
        ! m3/s to m3
        hyd1%flo = hyd1%flo ! * 86400.
        ! t = ppm * m3 / 1000000.
        hyd1%sed = hyd1%sed * hyd1%flo / 1000000.
        ! kg = ppm * m3 / 1000.
        hyd1%orgn = hyd1%orgn * hyd1%flo / 1000.
        hyd1%sedp = hyd1%sedp * hyd1%flo / 1000.
        hyd1%no3 = hyd1%no3 * hyd1%flo / 1000.
        hyd1%solp = hyd1%solp * hyd1%flo / 1000.
        hyd1%nh3 = hyd1%nh3 * hyd1%flo / 1000.
        hyd1%no2 = hyd1%no2 * hyd1%flo / 1000.
      end subroutine hyd_convert_conc_to_mass
      
      !! function to convert mass to concentration
      subroutine hyd_convert__mass_to_ha (hyd1, area_ha)
        real, intent (in) :: area_ha
        type (hyd_output), intent (inout) :: hyd1
        ! m3 to mm
        hyd1%flo = hyd1%flo / (area_ha * 10.)
        hyd1%sed = hyd1%sed / area_ha
        hyd1%orgn = hyd1%orgn / area_ha
        hyd1%sedp = hyd1%sedp / area_ha
        hyd1%no3 = hyd1%no3 / area_ha
        hyd1%solp = hyd1%solp / area_ha
        hyd1%nh3 = hyd1%nh3 / 1000.
        hyd1%no2 = hyd1%no2 / 1000.
      end subroutine hyd_convert__mass_to_ha
      
      !! function to convert concentration to mass
      subroutine res_convert_mass (hyd1, pvol)
        real, intent (in) :: pvol
        type (hyd_output), intent (inout) :: hyd1
        
        hyd1%flo = hyd1%flo * pvol      ! input as frac of principal
        hyd1%sed = hyd1%sed * hyd1%flo / 1000000.   ! t = ppm (g/m3) * 1 t/m3 / 1000000. g/t
        hyd1%orgn = hyd1%orgn * hyd1%flo / 1000.    ! kg = ppm * m3 / 1000.
        hyd1%sedp = hyd1%sedp * hyd1%flo / 1000.
        hyd1%no3 = hyd1%no3 * hyd1%flo / 1000.
        hyd1%solp = hyd1%solp * hyd1%flo / 1000.
        hyd1%nh3 = hyd1%nh3 * hyd1%flo / 1000.
        hyd1%no2 = hyd1%no2 * hyd1%flo / 1000.
      end subroutine res_convert_mass
      
      !! function to convert mass to concentration
      subroutine hyd_convert_mass_to_conc (hyd1)
        type (hyd_output), intent (inout) :: hyd1
        hyd1%flo = hyd1%flo
        ! ppm = 1000000. * t / m3
        hyd1%sed = 1000000. * hyd1%sed / hyd1%flo
        ! ppm = 1000. * kg / m3
        hyd1%orgn = 1000. * hyd1%orgn / hyd1%flo
        hyd1%sedp = 1000. * hyd1%sedp / hyd1%flo
        hyd1%no3 = 1000. * hyd1%no3 / hyd1%flo
        hyd1%solp = 1000. * hyd1%solp / hyd1%flo
        hyd1%nh3 = 1000. * hyd1%nh3 / hyd1%flo
        hyd1%no2 = 1000. * hyd1%no2 / hyd1%flo
      end subroutine hyd_convert_mass_to_conc
         
      !! routines for hydrograph module
      function hydout_add (hyd1, hyd2) result (hyd3)
        type (hyd_output), intent (in) :: hyd1
        type (hyd_output), intent (in) :: hyd2
        type (hyd_output) :: hyd3
        hyd3%flo = hyd1%flo + hyd2%flo
        hyd3%sed = hyd1%sed + hyd2%sed        
        hyd3%orgn = hyd1%orgn + hyd2%orgn        
        hyd3%sedp = hyd1%sedp + hyd2%sedp   
        hyd3%no3 = hyd1%no3 + hyd2%no3
        hyd3%solp = hyd1%solp + hyd2%solp
        hyd3%nh3 = hyd1%nh3 + hyd2%nh3
        hyd3%no2 = hyd1%no2 + hyd2%no2
      end function hydout_add
                     
      !! routines for hydrograph module
      function hydout_subtract (hyd1, hyd2) result (hyd3)
        type (hyd_output), intent (in) :: hyd1
        type (hyd_output), intent (in) :: hyd2
        type (hyd_output) :: hyd3
        hyd3%flo = hyd1%flo - hyd2%flo
        hyd3%sed = hyd1%sed - hyd2%sed        
        hyd3%orgn = hyd1%orgn - hyd2%orgn        
        hyd3%sedp = hyd1%sedp - hyd2%sedp   
        hyd3%no3 = hyd1%no3 - hyd2%no3
        hyd3%solp = hyd1%solp - hyd2%solp
        hyd3%nh3 = hyd1%nh3 - hyd2%nh3
        hyd3%no2 = hyd1%no2 - hyd2%no2
      end function hydout_subtract
            
      !! routines for hydrograph module
      function hydout_mult (hyd1, hyd2) result (hyd3)
        type (hyd_output), intent (in) :: hyd1
        type (hyd_output), intent (in) :: hyd2
        type (hyd_output) :: hyd3
        hyd3%flo = hyd1%flo * hyd2%flo
        hyd3%sed = hyd1%sed * hyd2%sed        
        hyd3%orgn = hyd1%orgn * hyd2%orgn        
        hyd3%sedp = hyd1%sedp * hyd2%sedp   
        hyd3%no3 = hyd1%no3 * hyd2%no3
        hyd3%solp = hyd1%solp * hyd2%solp
        hyd3%nh3 = hyd1%nh3 * hyd2%nh3
        hyd3%no2 = hyd1%no2 * hyd2%no2
      end function hydout_mult
            
      !! routines for hydrograph module
      function hydout_add_const (const, hyd1) result (hyd2)
        real, intent (in) :: const
        type (hyd_output), intent (in) :: hyd1
        type (hyd_output) :: hyd2
        hyd2%flo = const + hyd1%flo 
        hyd2%sed = const + hyd1%sed       
        hyd2%orgn = const + hyd1%orgn       
        hyd2%sedp = const + hyd1%sedp 
        hyd2%no3 = const + hyd1%no3
        hyd2%solp = const + hyd1%solp
        hyd2%nh3 = const + hyd1%nh3
        hyd2%no2 = const + hyd1%no2
      end function hydout_add_const
      
      function hydout_mult_const (const, hyd1) result (hyd2)
        type (hyd_output), intent (in) :: hyd1
        real, intent (in) :: const
        type (hyd_output) :: hyd2
        hyd2%flo = const * hyd1%flo 
        hyd2%sed = const * hyd1%sed !/ 1000.
        hyd2%orgn = const * hyd1%orgn       
        hyd2%sedp = const * hyd1%sedp 
        hyd2%no3 = const * hyd1%no3
        hyd2%solp = const * hyd1%solp
        hyd2%nh3 = const * hyd1%nh3
        hyd2%no2 = const * hyd1%no2
      end function hydout_mult_const
      
      function hydout_div_const (hyd1,const) result (hyd2)
        type (hyd_output), intent (in) :: hyd1
        real, intent (in) :: const
        type (hyd_output) :: hyd2
        hyd2%flo = hyd1%flo / const
        hyd2%sed = hyd1%sed / const
        hyd2%orgn = hyd1%orgn / const
        hyd2%sedp = hyd1%sedp / const
        hyd2%no3 = hyd1%no3 / const
        hyd2%solp = hyd1%solp / const
        hyd2%nh3 = hyd1%nh3 / const
        hyd2%no2 = hyd1%no2 / const
      end function hydout_div_const
            
      !function to convert m^3-> mm and kg(or t)->kg(or t)/ha
      function hydout_div_conv (hyd1,const) result (hyd2)
        type (hyd_output), intent (in) :: hyd1
        real, intent (in) :: const  !ha
        type (hyd_output) :: hyd2
        hyd2%flo = hyd1%flo / (10. * const)
        hyd2%sed = hyd1%sed / const
        hyd2%orgn = hyd1%orgn / const
        hyd2%sedp = hyd1%sedp / const
        hyd2%no3 = hyd1%no3 / const
        hyd2%solp = hyd1%solp / const
        hyd2%nh3 = hyd1%nh3 / const
        hyd2%no2 = hyd1%no2 / const
      end function hydout_div_conv

      end module hydrograph_module