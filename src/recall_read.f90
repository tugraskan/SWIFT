      subroutine recall_read

      use hydrograph_module
      use input_file_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use maximum_data_module
      use exco_module
      
      implicit none      
 
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=80) :: obj_count    !        |2nd dummy header of file
      character(len=24) :: rec_name
      character(len=8) :: rec_typ
      character(len=255) :: rec_filename
	  character(len=255) :: recall_path
      integer :: imax                 !none       |end of loop
      integer :: iyr                  !           |
      integer :: jday                 !           |
      integer :: mo                   !           |
      integer :: day_mo               !           |
      integer :: eof                  !           |end of file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: nbyr                 !none       !number of years the land use occurred 
      integer :: k                    !           |
      integer :: iyrs                 !           | 
      integer :: iyr_prev             !none       |previous year
      integer :: istep                !           | 
      integer :: ipestcom_db          !none       !pointer to pestcom_db - fix*** ?? 
      integer :: ipc                  !none       |counter
      integer :: ii                   !none       |counter
      integer :: i                    !           |
      integer :: iexco_om
      integer :: ifirst               !           |
      integer :: iexo_allo = 0
      integer :: idaystep
	  integer :: icon
      
      
 
      !! read all recall files
      inquire(file=in_rec%recall_rec, exist=i_exist)
      if (i_exist .or. in_rec%recall_rec /= "null") then
        open(107, file=in_rec%recall_rec)
        read(107, *, iostat=eof) header
        if (eof < 0) then
          close(107)
          return
        endif

        imax = 0
        do while (eof == 0)
          read(107, *, iostat=eof) i
          if (eof < 0) then
            exit
          endif
          imax = max(imax, i)
        end do
        db_mx%recall_max = imax
    
        allocate(recall(0:imax))
    
        rewind(107)
        read(107, *, iostat=eof) header
        if (eof < 0) then
          close(107)
          return
        endif
    
        do ii = 1, imax
          read(107, *, iostat=eof) i
          if (eof < 0) then
            exit
          endif
          backspace(107)
          read(107, *, iostat=eof) k, rec_name, rec_typ, rec_filename
          if (eof < 0) then
            exit
		  endif
		  !!check if the file is a recall typ is 1 and swift_nam flag is true
          if (rec_typ == '1' .AND. nam%used == .TRUE.) then                                                             
            open(108, file=trim(rec_filename))
            read(108, *, iostat=eof) titldum
            if (eof < 0) then
              close(108)
              exit
			endif
			! read file to get rec_name; i.e. *_con.out
			read(108, *, iostat=eof) iyr, jday, mo, day_mo, rec_typ, rec_name
			close(108)
			! construct the path to the recall file; root_dir\HUC8\SWIFT\*_con.out
            recall_path = trim(nam%root_dir) // "\\" // trim(rec_name(:8)) // "\\" // "SWIFT" // "\\" // rec_name   
            !Check if recall_path exists
            inquire(file=recall_path, exist=i_exist)
                if (i_exist) then
					open(109, file=trim(recall_path))
					read(109, *, iostat=eof) titldum
					read(109, *, iostat=eof) titldum
					if (eof < 0) then
                        close(108)
                        exit
					endif
					! read in to get recall(i)
					read(109, *, iostat=eof) rec_name, icon, rec_typ, recall(i)
					close(109)
                else
            ! Handle the case where recall_path does not exist
                    print *, "Error: Recall file does not exist at path: ", recall_path
                exit
				endif
		    ! else read recall files as normal           
        else                                                                                                            
        read(108, *, iostat=eof) titldum
        if (eof < 0) then
          close(108)
          exit
        endif
        read(108, *, iostat=eof) iyr, jday, mo, day_mo, rec_typ, rec_name, recall(i)
        close(108)
      endif
    end do
    close(107)
  endif
  
  return
end subroutine recall_read
    