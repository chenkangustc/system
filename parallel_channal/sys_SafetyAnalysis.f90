!===================================================================================================
!
!   perform system safety analysis by 
!---------------------------------------------------------------------------------------------------
!   Public subroutine lists:    
!                               Driving_System_transient
!
!   Public type lists:          No
!
!===================================================================================================
module sys_SafetyAnalysis
 use sys_th_global
 use constants
 implicit none
  public::Driving_System_steady,Driving_System_transient
  
 contains
 !subroutine Driving_System_steady(power,fq_core)
 !  real(KREAL),intent(in):: power
 !  real(KREAL),intent(in):: fq_core 
 !  call sys_power%set(reactor%mesh,power,fq_core)
 !  call driving_reactor_steady()
   !call driving_reactor_transient()
   !call driving_()
 !end subroutine Driving_System_steady
 
 subroutine Driving_System_transient(power,fq_core,tidx,ltime,ctime)!power & fq通过接口给过
    real(KREAL),intent(in):: power
    real(KREAL),intent(in):: fq_core
    !local
    real(KREAL)::dt
    !dt=ctime-ltime
    call CIADS%reator%transient(power,fq_core,tidx,ltime,ctime)
    !call CIADS%hx%transient()
    
 end subroutine Driving_System_transient
end module sys_SafetyAnalysis
