subroutine c_erfc(y,x,n) bind(c,name="ferfc")
  use iso_c_binding, only: c_double, c_int
  integer(c_int), intent(in)  :: n
  real(c_double), dimension(n), intent(in)  :: x
  real(c_double), dimension(n), intent(out) :: y

  call f_erfc(y,x,n)

end subroutine

subroutine f_erfc(y, x, n)
  integer(kind=kind(0)), intent(in) :: n
  real (kind=kind(0d0)), intent(out):: y(n)
  real (kind=kind(0d0)), intent(in) :: x(n)

  integer(kind=kind(0)) i
  real (kind=kind(0d0)) xx, t, z, p

  do i= 1, n
  xx= x(i)
  z= abs(xx)
  t= 1.0/(1.0+0.5*z)
  p= -1.26551223 + &
  &      t*(1.00002368 + t*(0.37409196 + t*(0.09678418 + &
  &      t*(-0.18628806 + t*(0.27886807 + t*(-1.13520398 + &
  &      t*(1.48851587 + t*(-0.82215223 + t*0.17087277))))))))
  p= t*exp(-z*z + p)
  if (xx .lt. 0.) then
    p= 2.-p
  endif
  y(i)= p
  enddo
  return
end
