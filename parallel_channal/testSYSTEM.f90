!****************************************************************************
!
!  PROGRAM: parallel_channal
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program parallel_channal
     use sys_th_global
     use sys_pre_process_driver
    implicit none
    integer zone,layer
    integer layer_top,layer_bottom
    !«∞¥¶¿Ì
    call sys_driving_input(zone,layer,layer_top,layer_bottom)
    !call Driving_System_Steady(power,fq_core)
    call Driving_System_Transient(power,fq_core,tidx,ltime,ctime)
    
    end program parallel_channal

