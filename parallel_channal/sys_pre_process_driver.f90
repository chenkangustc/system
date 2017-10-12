module sys_pre_process_driver
   use sys_th_global
   use sys_re_input_global
   implicit none
   public::sys_driving_input
 contains
   subroutine  sys_driving_input(zone,layer,layer_top,layer_bottom)
      implicit none
      integer£¬intent(in) zone
      integer£¬intent(in) layer
      integer£¬intent(in) layer_top
      integer£¬intent(in) layer_bottom

      call reInputdata%set()
      call CIADS%reactor%set(zone,layer,layer_top,layer_bottom)
      call CIADS%reactor%alloc()
      
      !call CIADS%pump%input()
      !call CIADS%pump%alloc()
      !call CIADS%pump%set()
      
   end subroutine sys_driving_input
end module sys_pre_process_driver