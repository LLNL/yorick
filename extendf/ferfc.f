      subroutine ferfc(y, x, n)
      integer n, i
      double precision y(n), x(n), xx, t, z, p
      do i= 1, n
         xx= x(i)
         z= abs(xx)
         t= 1.0/(1.0+0.5*z)
         p= -1.26551223 +
     &      t*(1.00002368 + t*(0.37409196 + t*(0.09678418 +
     &      t*(-0.18628806 + t*(0.27886807 + t*(-1.13520398 +
     &      t*(1.48851587 + t*(-0.82215223 + t*0.17087277))))))))
         p= t*exp(-z*z + p)
         if (xx .lt. 0.) then
            p= 2.-p
         endif
         y(i)= p
      enddo
      return
      end
