module sys_assembly_header
    implicit none
    type,public sys_assmebly!描述一个组件的特征和使用方法
      private
      real::fric  !摩擦因子
      type(AssmGeom)::geom !assm_geom
      type(AssmMesh)::mesh !Assm_mesh
      type(AssmMaterial)::property !Assm_material 热物性和水力学参数
      type(AssmBoundary)::boundary !Assm_boundary
      real::power(zone,layer) !Assm_power
      type(AssmThermal)::Thermal  !pvt
    contains
      procedure,public::alloc=>alloc_assembly
      procedure,public::clean=>free_assembly
      procedure,public::set=>set_assembly!输入卡输入
      procedure,public::init=>init_assembly
      procedure,public::calSteady=>cal_Assembly_Steady
      procedure,public::calTransient=>cal_Assembly_Transient
    end type sys_assmebly
    
    type,private AssmGeom
      real rFuel          !元件半径
      real GasGap         !元件气隙厚度
      real ShellThick     !元件外壳厚度
      real AssmShellThick !组件外壳厚度
      real AcrossFlat     !组件外对边距（包含包壳厚度）
      real Height         !组件高度（活性区）
      integer n_pin       !元件的个数
    end type AssmGeom

    type,private Assmmesh
        integer nf
        integer ng
        integer ns
        integer ny
    end type Assmmesh
    
    type,private Assmboundary
       real Tin
       real Tout
       real uin
       real uout
       real pin
       real pout
    end type Assmboundary
    
    type,private AssmMaterial!热物性和水力学参数
        real,allocatable::rho(:,:)!热物性
        real,allocatable::shc(:,:)
        real,allocatable::ctc(:,:)
        real,allocatable::htc(:)
    end type AssmMaterial
     
    type,private AssmThermal!                
        real,allocatable::Temperature(:,:) !pvt
        real,allocatable::pressure(:,:)
        real,allocatable::velocity(:,:)
    end type AssmThermal
     private::alloc_assembly
     private::clean_assembly
     private::set_assembly
     private::init_assembly
     private::cal_Assembly_Steady
     private::cal_Assembly_Transient
    contains
     subroutine init_assembly(this)
      implicit none
      class(sys_assembly),intent(in out)::this
         !设置初始压力场
         do i=1,Ny,1
             if (i<Ny)then
                 p(i)=60000.0-5000.0*(i-1)
             elseif(i==Ny)then
                 p(i)=(2*pout+p(i-1))/3.0
             endif
         enddo
         !读入入口速度函数
          open(unit=1,file='E:\documents\doctors degree\software\tansistant\parallel_channal\uin.txt')
          read(1,*) uint
          close(1)
         !设置初始入口速度
         uin=uint(0)
         !call get_uin(k,dt,uini,uin)
         !设置入口压力
         pin=1.50*p(1)-0.5*p(2)
         !设置初始温度
         Ti(0:M,0:N)=Tic
         !初始化入口温度
         T(0,N)=Tin
         !设置热源
         q=0.0
         Do i=1,M-1,1
             Do j=1,N,1
                if(j<=Nf)  q(i,j)=2.827*1e7
             enddo
         enddo
         !print *,q
         !初速度赋值 tmin时刻的速度
         ui=uic
         ulast=ui
         !初始的物性参数和温度赋值
         Do i=0,M,1
             Do j=0,N,1
                 if (j>=0.and.j<=Nf)then !芯块热物性 UO2
                  RHOI(i,j)=10980
                  RHO(i,j)=10980
                  SHC(i,j)=300.0
                  CTC(i,j)=4.33
                  DVS(i,j)=0.0
                 elseif (j>Nf.and.j<=Nf+Ng) then!气隙热物性 He
                  RHOI(i,j)=1.785
                  RHO(i,j)=1.785
                  SHC(i,j)=1.260
                  CTC(i,j)=0.124
                  DVS(i,j)=0.0
                 elseif (j>Nf+Ng.and.j<=Nf+Ng+Ns)then!包壳热物性 Ti
                  RHOI(i,j)=7900
                  RHO(i,j)=7900
                  SHC(i,j)=502.42
                  CTC(i,j)=18.84
                  DVS(i,j)=0.0
                 else!流体物性
                  RHOI(i,j)=10470  
                  RHO(i,j)=10470
                  SHC(i,j)=159
                  CTC(i,j)=3.61
                  DVS(i,j)=5.0 !动力粘度，而非运动粘度
                 endif                 
             enddo
         enddo    
         do i=0,M,1
           RHOF(i)=RHO(i,N)
           RHOFI(i)=RHOI(i,N)
         enddo
     endsubroutine init_assembly
    
     subroutine alloc_assembly(this)
      implicit none
      class(sys_assembly),intent(in out)::this
      !local
      integer::i_allocate
      integer::M,N
      M=this%mesh%Ny+1
      N=this%mesh%Nf+this%mesh%Ng+this%mesh%Ns+1
      !check allocated first
      call this%clean()
      
      allocate(this%property%rho(0:M,0:N))
      allocate(this%property%shc(0:M,0:N))
      allocate(this%property%ctc(0:M,0:N))
      allocate(this%property%htc(0:M))
      
      allocate(this%thermal%Temperature(0:M,0:N))
      allocate(this%thermal%Pressure(1:this%mesh%Ny))
      allocate(this%thermal%Velocity(1:this%mesh%Ny-1))
     end subroutine alloc_assembly
     
     subroutine Free_assembly(this)
      implicit none
      class(sys_assembly),intent(in out)::this
      if(allocated(this%property%rho))  deallocate(this%property%rho)
      if(allocated(this%property%shc))  deallocate(this%property%shc)
      if(allocated(this%property%ctc))  deallocate(this%property%ctc)
      if(allocated(this%property%htc))  deallocate(this%property%htc)
      
      if(allocated(this%pthermal%temperature))  deallocate(this%thermal%temperature)
      if(allocated(this%pthermal%pressure))  deallocate(this%thermal%pressure)
      if(allocated(this%pthermal%Velocity))  deallocate(this%thermal%Velocity)
     end subroutine Free_assembly
     
     subroutine set_assembly(this)
      implicit none
      class(sys_assembly),intent(in out)::this
      
           this%geom%rFuel=reInputdata%xf
           this%geom%GasGap=reInputdata%xg
           this%geom%ShellThick=reInputdata%xs
           this%geom%AssmShellThick=reInputdata%xos
           this%geom%AcrossFlat=reInputdata%acf
           this%geom%Height=reInputdata%height
           this%geom%n_pin=reInputdata%npin

           this%mesh%nf=reInputdata%nf
           this%mesh%ng=reInputdata%ng
           this%mesh%ns=reInputdata%ns
           this%mesh%ny=this%layer
           
           this%fric=reInputdata%f
     end subroutine set_assmebly
     
     subroutine cal_Assembly_Transient(this,power, fq_core, tidx, ltime, ctime)!计算一个时间步长
      implicit none
      class(sys_assembly)::this
      real(KREAL), intent(in)  :: power  !(nth%na, nth%nr)
      real(KREAL), intent(in)  :: fq_core!(nth%na, nth%nr)                     ! power peak from core calculation
      integer, intent(in)      :: tidx
      real(KREAL), intent(in)  :: ltime
      real(KREAL), intent(in)  :: ctime
      !local
      
       
     end subroutine cal_Assembly_Transient
      

end module sys_assembly_header