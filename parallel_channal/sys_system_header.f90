module sys_system_header
    use sys_reactor_header
    implicit none
    type,public::system_th
        private
         type(sys_reactor)::reactor
        !type(sys_heatexchanger)::HX1
        !type(sys_upper)::upper
        !type(sys_pump)::pump
    contains
      procedure,public::set=>set_system!geom,mesh,global
      !procedure,public::alloc
    end type system_th
     private::set_system
    contains
    subroutine set_system(this,zone,layer,layer_top,layer_bottom)
     implicit none
     class(system_th)::this
     
    end subroutine set_system
end module sys_system_header