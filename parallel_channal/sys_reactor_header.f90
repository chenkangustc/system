module sys_reactor_header
     use sys_assembly_header
     use sys_re_input_global!reInputdata
    implicit none
     type,public sys_reactor!描述一个堆，默认六边形组件，每个组件结构相同，流量不同，功率不同，快堆
      !reac_mesh
      integer::zone!组件个数
      integer::layer!轴向网格
      !reac_material
      !reac_power
      real::power(zone,layer)
      !reac_thermal  pvt
      !reac_boundary
      real::flowin!堆的入口总流量
      real::Tin!堆的入口温度
      real::pout!堆的出口压力
      !描述一个组件
      type(sys_assembly),dimension::assembly(zone)
     contains
      !procedure,public::input=>input_reactor
      procedure,public::alloc=>alloc_reactor
      procedure,public::clean=>free_reactor
      procedure,public::set=>set_reactor!设置geom/mesh/fric等固定参量
      procedure,public::update=>update_boundary
      procedure,public::init=>init_thermal
     end type sys_reactor
      !private::input_reactor
      private::alloc_reactor
      private::clean_reactor
      private::set_reactor
      private::update_boundary
      private::init_thermal
    contains  
     subroutine init_thermal(this)
      implicit none
      class(sys_reactor),intent(in out)::this
      !local
      integer i
      this%flowin=reInputdata%flowin
      this%Tin=reInputdata%Tin
      this%pout=reInputdata%pout
      do i=1,this%zone,1
       call this%assembly(i)%init()
      enddo
     end subroutine init_thermal
     
     subroutine update_boundary(this,flowin,Tin,pout)
      implicit none
      class(sys_reactor),intent(in out)::this
      real,intent(in)::flowin
      real,intent(in)::Tin
      real,intent(in)::pout
      
      this%flowin=flowin
      this%Tin=Tin
      this%pout=pout
     end subroutine update_boundary
     
     subroutine set_reactor(this,zone,layer,layer_top,layer_bottom)
       implicit none
       class(sys_reactor)::this
       integer，intent(in) zone
       integer，intent(in) layer
       integer，intent(in) layer_top
       integer，intent(in) layer_bottom
       
       !local
       integer i
       integer zone_,layer_,layer_top_,layer_bottom_

       !输入卡输入的参数赋值
       zone_=zone
       layer_=layer
       layer_top_=layer_top
       layer_bottom_=layer_bottom
       
       this%zone=zone_
       this%layer=layer_
       this%flowin=reInputdata%flowin
       this%Tin=reInputdata%Tin
       this%pout=reInputdata%pout
       
       do i=1,zone_,1
          call this%assembly(i)%set()
       enddo! 1,zone 
     end subroutineset_reactor
     
     subroutine alloc_reactor(this)
      implicit none
      class(sys_reactor)::this
      !local
      integer i
      integer zone_
      zone_=this%zone
      do i=1,zone_,1
          call this%assmebly(i)%alloc()
      enddo
     end subroutine alloc_reactor
     
     subroutine Free_reactor(this)
      implicit none
      class(sys_reactor)::this
      !local
      integer i
      integer zone_
      zone_=this%zone
      do i=1,zone_,1
          call this%assmebly(i)%Free()
      enddo
     end subroutine Free_reactor
    
end module sys_reactor_header