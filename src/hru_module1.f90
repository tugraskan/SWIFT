      module hru_module1
    
      implicit none

      type topography
           character(len=13) :: name
           real :: elev = 0.         !!               |m             |elevation of HRU
           real :: slope = 0.        !!	hru_slp(:)    |m/m           |average slope steepness in HRU
           real :: slope_len = 0.    !! slsubbsn(:)   |m             |average slope length for erosion
           real :: dr_den = 0.       !!               |km/km2        |drainage density
           real :: lat_len = 0.      !! slsoil(:)     |m             |slope length for lateral subsurface flow
           real :: dis_stream = 0.   !! dis_stream(:) | m            |average distance to stream
           real :: dep_co = 1.       !!               |              |deposition coefficient
           integer :: field_db = 0   !!               |              |pointer to field.fld
           integer :: channel_db=0   !!               |              |pointer to channel.dat
      end type topography
      
      type field
           character(len=13) :: name = "default"
           real :: length = 500. !!               |m             |field length for wind erosion
           real :: wid = 100.    !!               |m             |field width for wind erosion
           real :: ang = 30.     !!               |deg           |field angle for wind erosion
      end type field
      
      type hydrology
           character(len=16) :: name
           real :: lat_ttime = 0.   !! lat_ttime(:)  |days          |days of lateral soil flow across the hillslope
           real :: lat_sed = 0.     !! lat_sed(:)    |g/L           |sediment concentration in lateral flow
           real :: canmx = 0.       !! canmx(:)      |mm H2O        |maximum canopy storage
           real :: esco = 0.        !! esco(:)       |none          |soil evaporation compensation factor
           real :: epco = 0.        !! epco(:)       |none          |plant water uptake compensation factor (0-1)
           real :: erorgn = 0.      !! erorgn(:)     |none          |organic N enrichment ratio, if left blank
                                    !!                              |the model will calculate for every event
           real :: erorgp = 0.      !! erorgp(:)     |none          |organic P enrichment ratio, if left blank
                                    !!                              |the model will calculate for every event
           real :: cn3_swf = 0.     !!               |none          |curve number adjustment factor - sw at cn3
           real :: biomix = 0.      !! biomix(:)     |none          |biological mixing efficiency.
                                    !!                              |Mixing of soil due to activity of earthworms
                                    !!                              |and other soil biota. Mixing is performed at
                                    !!                              |the end of every calendar year.
           real :: perco = 0.       !!               |0-1           |percolation coefficient - linear adjustment to daily perc
           real :: lat_orgn = 0.
           real :: lat_orgp = 0.
           real :: harg_pet  = .0023  
           real :: latq_co = 0.3    !!               |              |lateral soil flow coefficient - linear adjustment to daily lat flow
           real :: perco_lim = 1.   !!               |              |percolation coefficient-limits perc from bottom layer
      end type hydrology
      
      type snow_parameters
         character (len=16) :: name
         real :: falltmp = 0.     !deg C         |snowfall temp
         real :: melttmp = 0.5    !deg C         |snow melt base temp 
         real :: meltmx = 4.5     !mm/deg C/day  |Max melt rate for snow during year (June 21)
         real :: meltmn = 0.5     !mm/deg C/day  |Min melt rate for snow during year (Dec 21)
         real :: timp = 0.8       !none          |snow pack temp lag factor (0-1)
         real :: covmx = 25.0     !mm H20        |snow water content at full ground cover
         real :: cov50 = 0.5      !none          |frac of covmx at 50% snow cover
         real :: init_mm = 0.     !mm H20        |initial snow water content at start of simulation
      end type snow_parameters
      type (snow_parameters), dimension (:), allocatable :: snodb
      
      type subsurface_drainage_parameters
        character(len=13) :: name = "null"
        real :: depth = 0.    !! |mm            |depth of drain tube from the soil surface
        real :: time = 0.     !! |hrs           |time to drain soil to field capacity
        real :: lag = 0.      !! |hours         |drain tile lag time
        real :: radius = 0.   !! |mm            |effective radius of drains
        real :: dist = 0.     !! |mm            |distance between two drain tubes or tiles
        real :: drain_co = 0. !! |mm/day        |drainage coefficient
        real :: pumpcap = 0.  !! |mm/hr         |pump capacity 
        real :: latksat = 0.  !! !na            |multiplication factor to determine lat sat hyd conductivity for profile
      end type subsurface_drainage_parameters
      type (subsurface_drainage_parameters), dimension (:), allocatable :: sdr
              
      type landuse
          character(len=15) :: name
          integer :: cn_lu = 0
          integer :: cons_prac = 0
          real :: usle_p = 0.           !! none     | USLE equation support practice (P) factor daily
          character (len=16) :: urb_ro  !! none     | urban runoff model
                                        !!          | "usgs_reg", simulate using USGS regression eqs
                                        !!          | "buildup_washoff", simulate using build up/wash off alg 
          integer ::  urb_lu = 0        !! none     | urban land type identification number
          real :: ovn = 0.05            !! none     | Manning's "n" value for overland flow
      end type landuse
      type (landuse), dimension (:), allocatable :: luse
      
      type soil_plant_initialize
        character(len=16) :: name = ""
        real :: sw_frac
        character(len=16) :: nutc = ""
        character(len=16) :: pestc = ""
        character(len=16) :: pathc = ""
        character(len=16) :: saltc = ""
        character(len=16) :: hmetc = ""
        integer :: nut = 0
        integer :: pest = 1
        integer :: path = 1
        integer :: salt = 1
        integer :: hmet = 1
      end type soil_plant_initialize
      type (soil_plant_initialize), dimension (:), allocatable :: sol_plt_ini
        
      type hru_databases
        character(len=13) :: name = ""
        integer :: topo = 1
        integer :: hyd = 1
        integer :: soil = 1
        integer :: land_use_mgt = 1
        integer :: soil_plant_init = 1
        integer :: surf_stor = 0
        integer :: snow = 1
        integer :: field = 0
      end type hru_databases
      
      type hru_databases_char
        character(len=25) :: name = ""
        character(len=25) :: topo = ""
        character(len=25) :: hyd = ""
        character(len=25) :: soil = ""
        character(len=25) :: land_use_mgt = ""
        character(len=25) :: soil_plant_init = ""
        character(len=25) :: surf_stor = ""
        character(len=25) :: snow = ""
        character(len=25) :: field = ""
      end type hru_databases_char
        
      type hru_parms_db
        real :: co2 = 350.
      end type hru_parms_db
      
      type hydrologic_response_unit_db
        character(len=13) :: name = "default"
        type (hru_databases) :: dbs
        type (hru_databases_char) :: dbsc
        type (hru_parms_db) :: parms
      end type hydrologic_response_unit_db
      !type (hydrologic_response_unit_db), dimension(:),allocatable :: hru_db
      
      type land_use_mgt_variables
        real :: usle_p = 0.                 !! |none          |USLE equation comservation practice (P) factor
        real :: usle_ls = 0.                !! |none          |USLE equation length slope (LS) factor
        real :: usle_mult = 0.              !! |none          |product of USLE K,P,LS,exp(rock)
        real :: sdr_dep = 0.                !! |
        integer :: ldrain= 0.               !! |none          |soil layer where drainage tile is located
        real :: tile_ttime = 0.             !! |none          |Exponential of the tile flow travel time
        real :: vfsi = 0.                   !! |none          |initial SCS curve number II value
        real :: vfsratio = 0.               !! |none          |contouring USLE P factor
        real :: vfscon = 0.                 !! |none          |fraction of the total runoff from the entire field
        real :: vfsch = 0;                  !! |none          |fraction of flow entering the most concentrated 10% of the VFS.
                                            !!                     which is fully channelized
        integer :: ngrwat = 0
        real :: grwat_i = 0.                !! |none          |On/off Flag for waterway simulation
        real :: grwat_n = 0.                !! |none          |Mannings's n for grassed waterway
        real :: grwat_spcon = 0.            !! |none          |sediment transport coefficant defined by user
        real :: grwat_d = 0.                !! |m             |depth of Grassed waterway
        real :: grwat_w = 0.                !! |none          |Width of grass waterway
        real :: grwat_l = 0.                !! |km            |length of Grass Waterway
        real :: grwat_s = 0.                !! |m/m           |slope of grass waterway
        real :: bmp_flag = 0.  
        real :: bmp_sed = 0.                !! |%             | Sediment removal by BMP 
        real :: bmp_pp = 0.                 !! |%             | Particulate P removal by BMP
        real :: bmp_sp = 0.                 !! |%             | Soluble P removal by BMP
        real :: bmp_pn = 0.                 !! |%             | Particulate N removal by BMP 
        real :: bmp_sn = 0.                 !! |%             | Soluble N removal by BMP  
        real :: bmp_bac = 0.                !! |%             | Bacteria removal by BMP
      end type land_use_mgt_variables
     
      type wetland_data
        real :: psa = 0.                    !ha     |res surface area when res is filled to princ spillway
        real :: pvol = 0.                   !m^3    |vol of water needed to fill the res to the princ spillway (read in as ha-m and converted to m^3)
        real :: esa = 0.                    !ha     |res surface area when res is filled to emerg spillway 
        real :: evol = 0.                   !m^3    |vol of water needed to fill the res to the emerg spillway (read in as ha-m and converted to m^3)
      end type wetland_data

      type hydrologic_response_unit1
        character(len=25) :: name = ""
        integer :: obj_no
        real :: area_ha
        character(len=35) :: lum                !land use
        real :: slope                           !slope m/m
        character(len=25) :: hsg                !hydrologic soil group
        character(len=1) :: surf_stor           !surface storage (rice/wetland) - y/n
        character(len=1) :: bmp                 !bmp efficiencies - y/n
        character(len=1) :: dr                  !delivery routing over the hru - y/n
        real :: precip                          !average annual precip
        real :: pet                             !average annual potential ET
        type (wetland_data) :: wet              !wetland/paddy data
      end type hydrologic_response_unit1
      type (hydrologic_response_unit1), dimension(:), allocatable, target :: hru_dat
      
      type hydrologic_response_unit_mgt
        integer :: obj_no
        character(len=25) :: name = ""
        integer :: id
        character(len=50) :: landuse = ""
        real :: stir
        character(len=25) :: crop1
        character(len=25) :: crop2
        character(len=25) :: tillage
        character(len=1) :: irrig
        character(len=1) :: tiled
        character(len=25) :: terrace
        character(len=25) :: waterway
        character(len=25) :: filter
        character(len=1) :: contour
        character(len=1) :: grade_stab
        character(len=25) :: rip_buff
        character(len=1) :: cover_crop
        character(len=1) :: nut_mgt
        real :: com_n
        real :: com_p
        real :: man_n
        real :: man_p
        character(len=80) :: desc
      end type hydrologic_response_unit_mgt
      type (hydrologic_response_unit_mgt), dimension(:), allocatable, target :: hru_mgt
  
      type hydrologic_response_unit
        character(len=13) :: name = ""
        integer :: obj_no
        real :: area_ha
        real :: km
        integer :: surf_stor                    !points to res() for surface storage
        type (hru_databases) :: dbs             !database pointers
        type (hru_databases_char) :: dbsc       !database pointers
        type (hru_parms_db) :: parms            !calibration parameters
        integer :: land_use_mgt
        character(len=16) :: land_use_mgt_c
        integer :: lum_group
        character(len=16) :: lum_group_c        !land use group for soft cal and output
        character(len=16) :: region
        integer :: plant_cov
        integer :: mgt_ops
        integer :: tiledrain = 0
        integer :: septic = 0
        integer :: fstrip = 0
        integer :: grassww = 0
        integer :: bmpuser = 0
        integer :: crop_reg = 0

        !! other data
        type (topography) :: topo
        type (field) :: field
        type (hydrology) :: hyd
        type (landuse) :: luse
        type (land_use_mgt_variables) :: lumv
        type (subsurface_drainage_parameters) :: sdr
        type (snow_parameters) :: sno
        real :: snocov1, snocov2
        integer :: cur_op = 1
        real :: sno_mm                          !mm H2O        |amount of water in snow on current day
        real :: water_seep
        real :: water_evap
        character(len=1) :: wet_fp = "n"
        real :: strsa
      end type hydrologic_response_unit
      type (hydrologic_response_unit), dimension(:), allocatable, target :: hru
      type (hydrologic_response_unit), dimension(:), allocatable, target :: hru_init

      type exco_header      
          character(len=16) :: flo        =  " flow           "     ! (mm)
          character(len=16) :: sed        =  " sediment       "     ! (t/ha)
          character(len=16) :: orgn       =  " organic_n      "     ! (kg/ha)          
          character(len=16) :: orgp       =  " organic_p      "     ! (kg/ha)         
          character(len=16) :: no3        =  " nitrate_no3    "     ! (kg/ha)
          character(len=16) :: minp       =  " soluble_minp   "     ! (kg/ha)          
          character(len=16) :: nh3        =  " null           "     ! ()
          character(len=16) :: no2        =  " null           "     ! ()
      end type exco_header
      type (exco_header) :: hdr_exco
      
     type exco_header_units        
          character(len=16) :: flo        =  " mm             "       ! (m^3)
          character(len=16) :: sed        =  " t/ha           "       ! (t/ha)
          character(len=16) :: orgn       =  " kg/ha          "       ! (kg)
          character(len=16) :: orgp       =  " kg/ha          "       ! (kg)
          character(len=16) :: no3        =  " kg/ha          "       ! (kg)       
          character(len=16) :: minp       =  " kg/ha          "       ! (kg)
          character(len=16) :: nh3        =  " t/ha           "       ! (tons)    
          character(len=16) :: no2        =  " null           "       ! (kg)
      end type exco_header_units
      type (exco_header_units) :: hdr_exco_unt
      end module hru_module1