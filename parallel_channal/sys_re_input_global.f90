module sys_re_input_global
    !use sys_reactor_header !与全局变量冲突
    use sys_re_input_header
    implicit none
    type(sys_re_input)::reInputdata
    !type(sys_reactor)::reactor
end module sys_re_input_global