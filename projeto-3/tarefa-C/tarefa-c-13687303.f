      program Raizes
         implicit real*8 (a-h, o-z)
         x0 = -10.0d0
         e = 1e-6
         x = xnewton(x0, 6, e)
         write(*,*) x
      end program

      function f(x)
         implicit real*8 (a-h, o-z)
         f=x**3 - 4*(x**2) - 59*x + 126
         return
      end function

      function diff(x)
         implicit real*8 (a-h, o-z)
         diff = 3*x**2 - 8*x - 59
         return
      end function

      function xnewton(x0, n, e)
         implicit real*8 (a-h,o-z)
         x = x0
         do intercept=1, n, 1
            xnew = x - (f(x)/diff(x))
            if(abs(x - j) .lt. e) goto 100 
            x = xnew
         end do
 100     xnewton = x
         return
      end function