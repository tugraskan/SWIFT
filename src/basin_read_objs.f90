      subroutine basin_read_objs

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     reads in the routing information from the watershed configuration
!!     input file (.fig) and calculates the number of subbasins, reaches, 
!!     and reservoirs

      use hydrograph_module
      use input_file_module
      use organic_mineral_mass_module
      use constituent_mass_module
      
      implicit none
      
      character (len=80) :: titldum = ""        !! na           |title of file
      character (len=80) :: header  = ""        !! na           |header
      !integer :: npsu                          !!              |number of subbasins
      integer :: isp                = 0         !! na           |counter
      integer :: eof                = 0         !! na           |end of file
      logical :: i_exist                        !! na           |check to determine if file exists 

      !!- check to see if the object.cnt exists
      inquire (file=in_basin%object_cnt, exist=i_exist)
      if (.not. i_exist .or. in_basin%object_cnt == "null") then
          allocate (ob(0:0))
          allocate (obcs(0:0))
          allocate (obom(0:0))
      else
      !!- if object.cnt does exist, read in the number of spatial objects 
      do
        open (107,file=in_basin%object_cnt)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) bsn, sp_ob
        if (eof < 0) exit
      enddo
      endif

      close (107)
      !!- allocate the objects
      allocate (ob(sp_ob%objs))
      allocate (obcs(sp_ob%objs))
      allocate (obom(sp_ob%objs))
      return    
      end subroutine basin_read_objs