      subroutine proc_open
    
      use channel_module1
      use reservoir_module1
      use hydrograph_module

      implicit none
      integer :: i                                                      !none       |counter

      !! open output files
      200   format((2xA4,1x1A5,28x1A5),8x,*(1a20,6x8a16))               !format for header
      400   format((A16,2xA8,2(3xA),4A),*(A16,4x))                       !format for hydrograph header
            
      open (1977,file = "res.out",recl=1200)
      write (1977,*) bsn%name, prog
      write (1977, 200) obj_hdr%num, obj_hdr%name, obj_hdr%area,    &   !write header
          ' INFLOW     ', hdr_cha,    &
          ' OUTFLOW    ', hdr_cha
      write (1977, 200)  " --  ", " --  ", " ha  ",   &                 !write header units
          ('---        ', hdr_cha_unt,    i = 1, 2)  
      
      open (1978,file = "aqu.out",recl=1200)
      write (1978,*) bsn%name, prog
      write (1978, 200) obj_hdr%num, obj_hdr%name, obj_hdr%area,    &   !write header
          ' INFLOW     ', hdr_cha,    &
          ' OUTFLOW    ', hdr_cha
      write (1978, 200)  " --  ", " --  ", " ha  ",   &                 !write header units
          ('---        ', hdr_cha_unt,    i = 1, 2)  
      
      open (1979,file = "cha.out",recl=1200)
      write (1979,*) bsn%name, prog
      write(1979, 200)  obj_hdr%num, obj_hdr%name, obj_hdr%area,    &   !write header
          ' INFLOW     ', hdr_cha,    &
          ' OUTFLOW    ', hdr_cha,    &
          ' BANK       ', hdr_cha,    &
          ' BED        ', hdr_cha,    &
          ' BED DEP    ', hdr_cha,    &   
          ' FP DEP     ', hdr_cha
      write(1979, 200) " --  ", " --  ", " ha  ",   &                   !write header units
          ('---        ', hdr_cha_unt,    i = 1, 6)  
      
      open (1988,file = "cha_bank.out",recl=1200)
      write (1988,*) bsn%name, prog
      write (1988,*) hdr_cha_bnk
      write (1988,*) hdr_cha_bnk_unt
      
      open (1985,file = "cha_bed.out",recl=1200)
      write (1985,*) bsn%name, prog
      write (1985,*) hdr_cha_bed
      write (1985,*) hdr_cha_bed_unt
      
      open (1986,file = "cha_fp.out",recl=1200)
      write (1986,*) bsn%name, prog
      write (1986,*) hdr_cha_fp
      write (1986,*) hdr_cha_fp_unt
      
      open (1987,file = "res_flo.out",recl=1200)
      write (1987,*) bsn%name, prog
      write (1987,*) hdr_res_floout
      write (1987,*) hdr_res_floout_unt
      
      open (1980,file = "hru.out",recl=1200)
      write (1980,*) bsn%name, prog
      write (1980, "(14X,*(A16))") hdr_cha
      write (1980, "(14X,*(A16))") hdr_cha_unt
      
      open (1981,file = "out.out",recl=1200)
      write (1981,*) bsn%name, prog                                     !write file name
      write (1981, 200) obj_hdr%num, obj_hdr%name, obj_hdr%area,    &
          ' INFLOW     ', hyd_hdr2
      write (1981, 200) " --  ", " --  ", " ha  ",  &                   !write header units
          ('---        ', hdr_cha_unt) 
      
      open (1982,file = "ru.out",recl=1200)
      write (1982,*) bsn%name, prog                                     !write file name
      write (1982, 200) obj_hdr%num, obj_hdr%name, obj_hdr%area,    &   !write header
          ' TOTAL      ', hyd_hdr2, ' PERCO      ', hyd_hdr2, &
          ' SURFACE    ', hyd_hdr2, ' LATERAL    ', hyd_hdr2, &
          ' TILE       ', hyd_hdr2  
      write (1982, 200) " --  ", " --  ", " ha  ",  &                   !write header units
          ('---        ', hdr_cha_unt,    i = 1, 5)                                

      
      
      open (1983,file = "hyd.out",recl=1200)
      write (1983,*) bsn%name, prog
      write (1983, 400) obj_hdr2%name, obj_hdr2%typ,                &
        ' rcv_tot    ', ' src_tot    ',' ---        ',              &
          obj_hdr2%typ,' num        ',' htyp        ',              &
        ' frac        ',hyd_hdr2
      write (1983, "(125X,*(A20))")  hyd_hdr_unt
      
      !! write headers in output files

	  return
      
      end subroutine proc_open