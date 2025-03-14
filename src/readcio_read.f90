       subroutine readcio_read 
    
       use input_file_module
           
       character (len=80) :: titldum    = ""        !! na        |dummy title
       character (len=15) :: name       = ""        !! na        |dummy name
       integer :: eof                   = 0         !! na        |end of file
       logical :: i_exist                           !!none       |check to determine if file exists
       
       eof = 0
       
       !!- inquire if file.cio exists !! read file.cio if it exists and get in_* variables   
       inquire (file="file_cio.swf", exist=i_exist)
       if (i_exist ) then
         open (107,file="file_cio.swf")
	   !! read file.cio if it exists and get in_* variables   
         read (107,*) titldum
       do i = 1, 31
         read (107,*,iostat=eof) name, in_basin
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_cli
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_con
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_cha
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_res
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_ru
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_hru
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_rec
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_aqu
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_lsu
         if (eof < 0) exit
      end do
      end if
      close (107)   
      
      !!- inquire if root_path.swf exists 
       inquire (file="root_path.swf", exist=i_exist)
       if (i_exist ) then
           nam%used = .TRUE.
         open (107,file="root_path.swf")
	  !! if nam%used set to .TRUE. get nam%root_dir 
         read (107,*) nam%root_dir
         end if
      close (107)
      
      return
      end subroutine readcio_read  