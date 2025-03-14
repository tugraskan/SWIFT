      subroutine proc_hru
    
      use hru_module1
      use climate_module1
      use hydrograph_module, only : sp_ob, sp_ob1, ob, hru_ex, hru_dr, hru_bmp
      use input_file_module
    
      implicit none
      
      integer :: ihru           !none       |hru counter
      integer :: iob            !none       |object counter
      integer :: ihru_db        !none       |number of hru data
      integer :: ipro           !none       |number of process (total runoff,surf,perc,lat,tile)
      integer :: iwst           !none       |precip gage number
      integer :: i, k           !none       |variables used to set data number
      integer :: iostat, eof
      real :: runoff_mm
      logical :: i_exist        !none       |check to determine if file exists
      character (len=80) :: titldum   !           |title of file
      character(len=32)  :: hru_category !           |hru category in hru_exco used to skip over dummy names in one line format

      !! open hru.dat file
      open (107,file = in_hru%hru_dat,recl=1200)
      read (107,*) titldum
      read (107,*) ihru_db
      read (107,*) titldum   !header
      read (107,*) titldum   !units
      
      allocate (hru_dat(sp_ob%hru))
      
      !! read hru data
      do ihru = 1, sp_ob%hru
        read (107,*,iostat=eof) i
        if (eof < 0) exit
        backspace (107)
        !! read from the aquifer database file named aquifer.aqu
        read (107,*,iostat=eof) k, hru_dat(i)%name, hru_dat(i)%lum, hru_dat(i)%slope,   &
                             hru_dat(i)%hsg, hru_dat(i)%surf_stor, hru_dat(i)%dr
        if (eof < 0) exit
      end do
      close (107)
        
      !! read hru export coefficient data
      open (107,file = in_hru%hru_exco,recl=1200)
      read (107,*) titldum
      read (107,*) ihru_db
      read (107,*) titldum   !header
      read (107,*) titldum   !units
      
      allocate (hru_ex(sp_ob%hru))
      
      !! read hru data *** tu old hru_exco object format
      !do ihru = 1, sp_ob%hru
      !  read (107,*,iostat=eof) i
      !  if (eof < 0) exit
        !backspace (107)
        !! read loadings from each process 
      !    do ipro = 1, 5
      !      read (107,*,iostat=eof) hru_ex(i)%runoff(ipro)
      !    end do
      !  if (eof < 0) exit
      !end do
      !close (107)
      
      !! read hru data *** tu new hru_exco one line format 
      do ihru = 1, sp_ob%hru
      read (107,*,iostat=eof) i, &
        (hru_category, hru_ex(i)%runoff(ipro)%flo, hru_ex(i)%runoff(ipro)%sed, &
         hru_ex(i)%runoff(ipro)%orgn, hru_ex(i)%runoff(ipro)%sedp, hru_ex(i)%runoff(ipro)%no3,&
         hru_ex(i)%runoff(ipro)%solp, hru_ex(i)%runoff(ipro)%nh3, hru_ex(i)%runoff(ipro)%no2, ipro = 1, 5)
      if (eof < 0) exit
      end do
      close (107)

            
      !! set hru data to proper input data
      do iob = 1, sp_ob%hru
        ihru = ob(iob)%props
        hru_dat(iob)%obj_no = sp_ob1%hru + ihru - 1
        iwst = ob(hru_dat(iob)%obj_no)%wst
        hru_dat(iob)%precip = cli(iwst)%precip_mm
        hru_dat(iob)%area_ha = ob(hru_dat(iob)%obj_no)%area_ha
        !! convert m3=mm*ha*10, t, kg for routing
        do ipro = 1, 5
          runoff_mm = hru_ex(ihru)%runoff(ipro)%flo * hru_dat(iob)%precip
          ob(iob)%hd(ipro)%flo = runoff_mm *  hru_dat(iob)%area_ha * 10.
          ob(iob)%hd(ipro)%sed = hru_ex(ihru)%runoff(ipro)%sed * ob(iob)%hd(ipro)%flo / 1000000.
          ob(iob)%hd(ipro)%orgn = hru_ex(ihru)%runoff(ipro)%orgn * ob(iob)%hd(ipro)%flo / 1000.
          ob(iob)%hd(ipro)%sedp = hru_ex(ihru)%runoff(ipro)%sedp * ob(iob)%hd(ipro)%flo / 1000.
          ob(iob)%hd(ipro)%no3 = hru_ex(ihru)%runoff(ipro)%no3 * ob(iob)%hd(ipro)%flo / 1000.
          ob(iob)%hd(ipro)%solp = hru_ex(ihru)%runoff(ipro)%solp * ob(iob)%hd(ipro)%flo / 1000.
          ob(iob)%hd(ipro)%nh3 = hru_ex(ihru)%runoff(ipro)%nh3 * ob(iob)%hd(ipro)%flo / 1000.
          ob(iob)%hd(ipro)%no2 = hru_ex(ihru)%runoff(ipro)%no2 * ob(iob)%hd(ipro)%flo / 1000.
        end do
      end do
      
      !! read volume/areas for surface storage (wetland/rice)
      open (107,file = in_hru%hru_wet,recl=1200)
      read (107,*) titldum
      read (107,*) ihru_db
      read (107,*) titldum   !header
      read (107,*) titldum   !units
      
      !! if hru is a wetland/rice, input required data
      do ihru = 1, ihru_db
        read (107,*,iostat=eof) i
        if (eof < 0) exit
        hru_dat(i)%surf_stor = "y"
        backspace (107)
          read (107,*,iostat=eof) k, hru_dat(i)%wet
          if (eof < 0) exit
      end do
      
      close (107)
      
      !! read hru bmp efficiencies
      inquire (file=in_hru%hru_bmp, exist=i_exist)
      if (i_exist .or. in_hru%hru_bmp /= "null") then
        do
          open (107,file = in_hru%hru_bmp,recl=1200)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) ihru_db
          if (eof < 0) exit
          read (107,*,iostat=eof) titldum   !header
          if (eof < 0) exit
          read (107,*,iostat=eof) titldum   !units
          if (eof < 0) exit
      
          allocate (hru_bmp(sp_ob%hru))
        
          !! if routing over an hru, input required data
          do ihru = 1, ihru_db
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            hru_dat(i)%bmp = "y"
            backspace (107)
            do ipro = 1, 5
              read (107,*,iostat=eof) k, hru_dr(i)%dr(ipro)
              if (eof < 0) exit
            end do
          end do
      
        end do
      end if
      
      close (107)
      
      !! read hru delivery if routing over hru
      inquire (file=in_hru%hru_dr, exist=i_exist)
      if (i_exist .or. in_hru%hru_dr /= "null") then
        do
          open (107,file = in_hru%hru_dr,recl=1200)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) ihru_db
          if (eof < 0) exit
          read (107,*,iostat=eof) titldum   !header
          if (eof < 0) exit
          read (107,*,iostat=eof) titldum   !units
          if (eof < 0) exit
        
          !! if routing over an hru, input required data
          do ihru = 1, ihru_db
            read (107,*,iostat=eof) i
         if (eof < 0) exit
            hru_dat(i)%dr = "y"
            backspace (107)
            do ipro = 1, 5
              read (107,*,iostat=eof) k, hru_dr(i)%dr(ipro)
              if (eof < 0) exit
            end do
        end do
      
        end do
      end if
      
      close (107)
      
	  return
      end subroutine proc_hru