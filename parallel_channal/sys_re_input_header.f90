module sys_re_input_header
    implicit none
    type,public sys_re_input
       private
       real xf,xg,xs,xos,acf,height,npin,nf,ng,ns,ny,f,Tin,pout,flowin,sigma,sigmab,alpha
    contains
     procedure,public::set=>set_inputdata
    end type sys_re_input
     private::set_inputdata
    contains
     subroutine set_inputdata(this)
      implicit none
      class(sys_re_input)::this
       open(unit=1,file='./re_input.txt')
       !read(1,*) xf,xg,xs,height,nf,ng,ns,ny,f,Tin,pout,Tic,uic,tmax,nt,sigma,sigmab,alpha
       read(1,*) this%xf,this%xg,this%xs,this%xos,this%acf,this%height,this%npin,this%nf,this%ng,this%ns,this%ny,this%f,this%Tin,this%pout,this%flowin,this%sigma,this%sigmab,this%alpha
       close(1)
     end subroutine set_inputdata
end module sys_re_input