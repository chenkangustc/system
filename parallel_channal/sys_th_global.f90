!===================================================================================================
!
!   module for parameter object use in system calculation
!---------------------------------------------------------------------------------------------------
!   Public subroutine lists:    No
!
!   Public type lists:          No
!
!===================================================================================================
module sys_th_global
use sys_system_header
implicit none
public
type(system_th)::CIADS
!type(sys_heatexchanger)::HX1
!type(sys_upper)::upper
!type(sys_pump)::pump
end module sys_th_global