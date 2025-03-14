      subroutine proc_cha
    
      use channel_module1
      use hydrograph_module  !, only : sp_ob
      use input_file_module
         
      implicit none

      character (len=80) :: titldum   !             |title of file
      integer :: eof                  !             |end of file
      integer :: icha_db              !none         |number of channel data inputs
      integer :: icha                 !             |channel counter
      integer :: i                    !none         |counter
      integer :: k                    !none         |counter
      
      eof = 0
      
      open (107,file = in_cha%cha_dat,recl=1200)
      read (107,*) titldum   !title
      read (107,*) titldum   !header
      
      allocate (cha1(sp_ob%chandeg))
      allocate (cha2(sp_ob%chandeg))
      
      !! read channel data
      do icha = 1, sp_ob%chandeg
        read (107,*,iostat=eof) i
        if (eof < 0) exit
        backspace (107)
        !! read from the aquifer database file named aquifer.aqu
        read (107,*,iostat=eof) k, cha1(i)%dat
        if (eof < 0) exit
      end do

      !! read channel sediment data
      do icha = 1, sp_ob%chandeg
        read (107,*,iostat=eof) i
        if (eof < 0) exit
        backspace (107)
        !! read from the aquifer database file named aquifer.aqu
        read (107,*,iostat=eof) k, cha1(i)%sed
        if (eof < 0) exit
      end do

      close (107)

      !! read channel instream delivery data
      open (107,file = in_cha%cha_dr,recl=1200)
      read (107,*) titldum   !title
      read (107,*) i
      read (107,*) titldum   !header
      read (107,*) titldum   !units
      
      allocate (cha_dr(sp_ob%chandeg))
      
      !! read channel delivery data
      do icha = 1, sp_ob%chandeg
        read (107,*,iostat=eof) i
        if (eof < 0) exit
        backspace (107)
        !! read from the channel delivery ratio file
        read (107,*,iostat=eof) k, cha_dr(i)
        if (eof < 0) exit
      end do

      close (107)
      return
      end subroutine proc_cha