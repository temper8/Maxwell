function fmaxw_classic(v,alfa2) result(fmaxw)
    implicit none
    real*8, intent(in) :: v, alfa2
    real*8  dfmaxw
    real*8 arg,alfa,api,b,psiq,f,df,erfcc
    real*8 :: fmaxw
    real*8, parameter :: pi2sqrt=2.506628274631d0
    real*8, parameter :: pisqrt=1.77245385090552d0
    real*8, parameter :: zero=0.d0
    
    arg=-0.5d0*v**2*(1.d0+0.5d0*alfa2*v**2)
    fmaxw = dexp(arg)/pi2sqrt
    dfmaxw=-v*(1.d0+alfa2*v**2)*fmaxw
end function

subroutine fmaxw_classic(v, alfa2, fmaxw) 
    implicit none
    real*8, intent(in) :: v, alfa2
    real*8  dfmaxw
    real*8 arg,alfa,api,b,psiq,f,df,erfcc
    real*8, intent(out) :: fmaxw
    real*8, parameter :: pi2sqrt=2.506628274631d0
    real*8, parameter :: pisqrt=1.77245385090552d0
    
    arg=-0.5d0*v**2*(1.d0+0.5d0*alfa2*v**2)
    fmaxw = dexp(arg)/pi2sqrt
    dfmaxw=-v*(1.d0+alfa2*v**2)*fmaxw
end subroutine