      subroutine cli_pmeas
    
      use climate_module1
      use input_file_module
         
      implicit none

      character (len=80) :: titldum   !             |title of file
      integer :: eof                  !             |end of file
      integer :: icli_db              !none         |number of climate gages
      integer :: icli                 !none         |counter for climate gages
      integer :: i                    !none         |counter
      integer :: k                    !none         |counter
      logical :: i_exist              !none       |check to determine if file exists
      eof = 0
      inquire (file= in_cli%precip_sta, exist=i_exist)
      open (107,file = in_cli%precip_sta, recl=1200)
      read (107,*) titldum      !title
      read (107,*) icli_db
      read (107,*) titldum      !header
      read (107,*) titldum      !units
      
      allocate (cli(icli_db))
      
      !! read hru data
      do icli = 1, icli_db
        read (107,*,iostat=eof) i
        if (eof < 0) exit
        backspace (107)
        !! read from the aquifer database file named aquifer.aqu
        read (107,*,iostat=eof) k, cli(i)%name, cli(i)%precip_mm, cli(i)%pet_mm
        if (eof < 0) exit
      end do

      close (107)
      
      return
      end subroutine cli_pmeas