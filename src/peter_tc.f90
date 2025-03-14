      subroutine peter_tc
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

      use channel_module1
      use hydrograph_module, only : ob
      
      implicit none

      type tc_data
        real :: w_yr          !none          |channel number
        real :: m_yr          !none          |channel data pointer
        real :: te            !none          |channel number
        real :: tc            !none          |channel data pointer
        !character(len=25) :: texture
        real :: kd            !none          |channel number
        real :: dep           !none          |channel data pointer
        real :: slope         !none          |channel number
        real :: rc_w          !none          |channel data pointer
      end type tc_data
      type (tc_data) :: tc_db
      
      integer :: itc
      integer :: iter
      real :: tc_inc
      real :: time_hrs = 88.
      real :: tc_calc
      real :: hrs_calc
      character(len=25) :: header
      
      open (777,file='tc.out')
      open (107,file='tc.dat')
      read (107,*) header

      do itc = 1, 602
        read (107,*) tc_db
        
        !! substitution to solve for tc
        do iter = 1, 10000
          tc_inc = float(iter) * .001
          hrs_calc = tc_db%m_yr / (tc_db%te - tc_inc) / (.00586 * tc_inc ** -.826)
          if (abs(time_hrs - hrs_calc) < .2) exit
        end do
        
        write (777,*) itc, tc_db%w_yr, tc_db%m_yr, hrs_calc, tc_inc
      end do
      
      close (107)
      close (777)
      
      return
      end subroutine peter_tc