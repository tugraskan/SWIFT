      subroutine proc_aqu
    
      use aquifer_module1
      use hydrograph_module, only : sp_ob, aqu2
      use input_file_module
         
      implicit none

      character (len=80) :: titldum   !             |title of file
      integer :: eof                  !             |end of file
      integer :: iaqu_db              !none         |number of channel data inputs
      integer :: iaqu                 !             |channel counter
      integer :: i                    !none         |counter
      integer :: k                    !none         |counter
      
      eof = 0
      
      open (107,file = in_aqu%aqu_dat,recl=1200)
      read (107,*) titldum   !title
      read (107,*) iaqu_db
      read (107,*) titldum   !header
      read (107,*) titldum   !units
            
      allocate (aqu1(sp_ob%aqu))
      allocate (aqu2(sp_ob%aqu))
      
      !! read hru data
      do iaqu = 1, iaqu_db
        read (107,*,iostat=eof) i
        if (eof < 0) exit
        backspace (107)
        !! read from the aquifer database file named aquifer.aqu
        read (107,*,iostat=eof) k, aqu1(i)%name, aqu2(i)%trap
        if (eof < 0) exit
      end do

      close (107)
      
      return
      end subroutine proc_aqu