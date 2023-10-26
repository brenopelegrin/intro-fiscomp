      program Raizes
         implicit real*8 (a-h, o-z)
         x0 = -10.0d0
         e = 1e-6

         write(*,*) 'i,', 'r1,', 'r2,', 'r3'
         do i=1, 7, 1
            r1n = xnewton(x0, i, e)
            r2n = xnewton(0.0d0, i, e)
            r3n = xnewton(10.0d0, i, e)
            write(*,*) i-1, r1n, r2n, r3n
         end do
         write(*,*) '------'
         n=100
         h = real((b-a))/n
         a = -10.0d0
         b = 10.0d0
         do i=1, n-1, 1
            xold = a + i*h
            xnew = xold + h
            if(f(xnew)*f(xold) .le. 0.0d0) then
               write(*,*) 'hi'
 99            if(abs(f(xold)) - abs(f(xnew)) .gt. e) then
                  c = (xnew+xold)/2.0d0
                  if(f(xnew)*f(xold) .gt. 0.0d0) then
                     xnew = c
                  else
                     xold = c
                  end if
                  write(*,*) c
                  goto 99
               end if
            end if
         end do
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
         do iter=1, n, 1
            xnew = x - (f(x)/diff(x))
            if(abs(x - xnew) .lt. e) goto 100 
            x = xnew
         end do
 100     xnewton = x
         return
      end function