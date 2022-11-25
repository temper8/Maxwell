SUBROUTINE FIB(A,N)
!
!   CALCULATE FIRST N FIBONACCI NUMBERS
!
   INTEGER N
   REAL*8 A(N)
   DO I=1,N
      IF (I.EQ.1) THEN
         A(I) = 0.5D0
      ELSEIF (I.EQ.2) THEN
         A(I) = 1.0D0
      ELSE 
         A(I) = A(I-1) + A(I-2)
      ENDIF
   ENDDO
END

!!!!!!!!!!!!
subroutine init_vi(vclt, vi)
   integer, parameter :: i0 = 1002
   real*8, intent(in) :: vclt
   real*8, intent(out) :: vi(i0)            
   real*8 vmax

   vmax = 2.d0*vclt
   do i=1,i0
         vi(i)=dble(i-1)*vmax/dble(i0-1)
   end do
end subroutine



subroutine init_fmaxw_classic(vclt, enorm, fi, dfi)
   integer, parameter :: i0 = 1002
   real*8, intent(in) :: vclt, enorm
   real*8, intent(out) :: fi(i0), dfi(i0)
   real*8 vi, vmax

   vmax = 2.d0*vclt
   do i=1,i0
         vi = dble(i-1)*vmax/dble(i0-1)
         if(vi < vclt) then
               fi(i) = fmaxw_classic(vi, enorm, dfi(i))
         else
               fi(i) = zero
               dfi(i) = zero
         end if
   end do
end subroutine

double precision function fmaxw_classic(v,alfa2,dfmaxw)
implicit none
real*8 v,alfa2,dfmaxw
real*8 arg,alfa,api,b,psiq,f,df,erfcc
real*8 pi2sqrt,pisqrt,zero
parameter(pi2sqrt=2.506628274631d0,pisqrt=1.77245385090552d0)
parameter(zero=0.d0)

arg=-0.5d0*v**2*(1.d0+0.5d0*alfa2*v**2)
fmaxw_classic=dexp(arg)/pi2sqrt
dfmaxw=-v*(1.d0+alfa2*v**2)*fmaxw_classic
end