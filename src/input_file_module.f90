      module input_file_module


    implicit none

    !! file.cio input file 

    !! basin
      type input_basin
       character(len=100) :: object_cnt = "object.cnt"
       character(len=100) :: object_prt = "object.prt"
       character(len=100) :: cs_db = "constituents.cs"
      end type input_basin
      type (input_basin) :: in_basin
      	  	  
    !! climate
          type input_cli
           character(len=100) :: precip_sta = "precip.swf"
          end type input_cli
          type (input_cli) :: in_cli

    !! connect
          type input_con
           character(len=100) :: hru_con = "hru.con"
           character(len=100) :: ru_con = "rout_unit.con"
           character(len=100) :: aqu_con = "aquifer.con"
           character(len=100) :: chan_con = "chandeg.con"
           character(len=100) :: res_con = "reservoir.con"
           character(len=100) :: rec_con = "recall.con"
           character(len=100) :: out_con = "outlet.con"
          end type input_con
          type (input_con) :: in_con

    !! channel
          type input_cha 
           character(len=100) :: cha_dat = "cha_dat.swf"
           character(len=100) :: cha_dr = "cha_dr.swf"
          end type input_cha
          type (input_cha) :: in_cha

    !! reservoir
          type input_res
           character(len=100) :: res_dat = "res_dat.swf"
           character(len=100) :: res_dr = "res_dr.swf"
          end type input_res
          type (input_res) :: in_res

    !! routing unit
          type input_ru
           character(len=100) :: ru_def = "rout_unit.def"
           character(len=100) :: ru_ele = "rout_unit.ele"
          end type input_ru
          type (input_ru) :: in_ru

    !! HRU
          type input_hru
           character(len=100) :: hru_dat = "hru_dat.swf"
           character(len=100) :: hru_exco = "hru_exco.swf"
           character(len=100) :: hru_wet = "hru_wet.swf"
           character(len=100) :: hru_bmp = "hru_bmp.swf"
           character(len=100) :: hru_dr = "hru_dr.swf"
          end type input_hru
          type (input_hru) :: in_hru
	  	  
    !! recall (daily, monthly and annual)
          type input_rec 
           character(len=100) :: recall_rec = "recall.swf"
          end type input_rec
          type (input_rec) :: in_rec

    !! aquifer 
          type input_aqu
           character(len=100) :: aqu_dat = "aqu_dat.swf"
          end type input_aqu
          type (input_aqu) :: in_aqu
           
    !! landscape units
          type input_lsu
            character(len=100) :: def_lsu = "ls_unit.def"
            character(len=100) :: ele_lsu = "ls_unit.ele"
          end type input_lsu
          type (input_lsu) :: in_lsu
      
    !! exco (recall constant)
          type input_exco
           character(len=100) :: exco = "exco.exc"
           character(len=100) :: om = "exco_om.exc"
           character(len=100) :: pest = "exco_pest.exc"
           character(len=100) :: path = "exco_path.exc"
           character(len=100) :: hmet = "exco_hmet.exc"
           character(len=100) :: salt = "exco_salt.exc"
          end type input_exco
          type (input_exco) :: in_exco
	  
    !! delivery ratio
          type input_delr
           character(len=100) :: del_ratio = "delratio.del"
	       character(len=100) :: om = "dr_om.del"
	       character(len=100) :: pest = "dr_pest.del"
	       character(len=100) :: path = "dr_path.del"
	       character(len=100) :: hmet = "dr_hmet.del"
	       character(len=100) :: salt = "dr_salt.del"
          end type input_delr
          type (input_delr) :: in_delr
          
    !! root_path.swf input file used if user wants to run upstream inputs
          type swift_nam
           logical :: used = .FALSE.                ! logical flag for using swift_nam
           character(len=255) :: root_dir = ""      ! root directory for swift_nam
          end type swift_nam
          type (swift_nam) :: nam
          
end module input_file_module
