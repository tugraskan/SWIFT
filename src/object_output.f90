      subroutine object_output
      
      use input_file_module
      use hydrograph_module
      use maximum_data_module
      
      implicit none
       
      type object_print_output
        character (len=3) :: name
        character (len=3) :: obtyp     !! object type: hru,hlt,hs,rxc,dr,out,sdc
        integer :: obtypno             !! object type number: 1=hru, 2=hru_lte, 3=channel
        character (len=6) :: hydtyp    !! hydrograph type: tot,rhg,sur,lat,til
        integer :: objno               !! object number
        integer :: hydno               !! code computes from hydtyp
        character (len=255) :: filename !! file with hydrograph output from the object
        integer :: unitno = 6100       !! filename unit number
      end type object_print_output
      type (object_print_output), dimension (:), allocatable :: ob_out
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i                    !none       |counter
      integer :: ii                   !none       !counter
      integer :: k                    !           |
      integer :: iunit                !           |
      integer :: iout                 !           |  
      integer :: ihyd                 !           |
      
      mobj_out = 0
      imax = 0

      !! read old saveconc properties
      inquire (file=in_basin%object_prt,exist=i_exist)
      if (.not. i_exist .or. in_basin%object_prt == "null") then         
        allocate (ob_out(0:0))
      else
      do
        open (107,file=(in_basin%object_prt))
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = Max(imax,i)
            mobj_out = mobj_out + 1
          end do
          
        allocate (ob_out(0:imax))
        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

        do ii = 1, mobj_out
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          backspace (107)
          read (107,*,iostat=eof) k, ob_out(i)%obtyp, ob_out(i)%obtypno, ob_out(i)%hydtyp,  &
                                                                         ob_out(i)%filename
          if (eof < 0) exit
          
          select case (ob_out(i)%obtyp)
            case ("hru")   !hru
              ob_out(i)%objno = sp_ob1%hru + ob_out(i)%obtypno - 1
              if (ob_out(i)%obtypno == 0) ob_out(i)%objno = 0
            case ("ru")   !routing unit
              ob_out(i)%objno = sp_ob1%ru + ob_out(i)%obtypno - 1
            case ("res")   !hru_lte
              ob_out(i)%objno = sp_ob1%res + ob_out(i)%obtypno - 1
            case ("cha")   !channel
              ob_out(i)%objno = sp_ob1%chan + ob_out(i)%obtypno - 1
            case ("exc")   !export coefficient
              ob_out(i)%objno = sp_ob1%exco + ob_out(i)%obtypno - 1
            case ("dr")   !delivery ratio
              ob_out(i)%objno = sp_ob1%dr + ob_out(i)%obtypno - 1
            case ("out")   !outlet
              ob_out(i)%objno = sp_ob1%outlet + ob_out(i)%obtypno - 1
            case ("sdc")   !swat-deg channel
              ob_out(i)%objno = sp_ob1%chandeg + ob_out(i)%obtypno - 1
          end select
      
          select case (ob_out(i)%hydtyp)
            case ("tot")   !total flow
               ob_out(i)%hydno = 1
            case ("rhg")   !recharge
               ob_out(i)%hydno = 2              
            case ("sur")   !surface
               ob_out(i)%hydno = 3 
            case ("lat")   !lateral
               ob_out(i)%hydno = 4
            case ("til")   !tile
               ob_out(i)%hydno = 5 
        end select
        iunit = ob_out(i)%unitno
         
        open (iunit+ii,file = ob_out(i)%filename,recl=2000)
        write (iunit+ii,*) "    OBJECT.PRT     ", ob_out(i)%filename
        write (iunit+ii,*) "  VARIABLE-UNITS  "
        iout = ob_out(i)%objno
        ihyd = ob_out(i)%hydno
        write (iunit+ii,*) ob(iout)%name, ob(iout)%typ, ob_out(i)%obtypno, ob_out(i)%hydtyp,    &
                                                                                ob(iout)%hd(ihyd)
        
        enddo    
        exit
      enddo
      endif
        
      close (107)
      
      return
      
      end subroutine object_output