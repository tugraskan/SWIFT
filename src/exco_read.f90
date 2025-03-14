      subroutine exco_read
     
      use exco_module1
      use hydrograph_module  !, only : sp_ob, exco2
         
      implicit none

      character (len=80) :: titldum   !             |title of file
      integer :: eof                  !             |end of file
      integer :: iexco_db             !none         |number of channel data inputs
      integer :: iexco                !none         |channel counter
      integer :: i                    !none         |counter
      integer :: k                    !none         |counter
      
      eof = 0
      
      open (107,file = "exco.dat",recl=1200)
      read (107,*) titldum   !title
      read (107,*) iexco_db
      read (107,*) titldum   !header
      read (107,*) titldum   !units
            
      allocate (exco1(sp_ob%exco))
      allocate (exco2(sp_ob%exco))
      
      !! read hru data
      do iexco = 1, iexco_db
        read (107,*,iostat=eof) i
        if (eof < 0) exit
        backspace (107)
        !! read from the aquifer database file named aquifer.aqu
        !read (107,*,iostat=eof) k, exco1(i)%name, exco2(i)%load
        if (eof < 0) exit
      end do

      close (107)
      
      return
      end subroutine exco_read