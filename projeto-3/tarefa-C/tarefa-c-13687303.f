      program Raizes
         implicit real*8 (a-h, o-z)
         x0 = -10.0d0
         e = 1e-6

         write(*,*) 'i,', 'r1n,', 'r2n,', 'r3n,', 'r1d,', 'r2d,', 'r3d'
         do i=1, 7, 1
            r1n = xnewton(x0, i, e)
            r2n = xnewton(0.0d0, i, e)
            r3n = xnewton(10.0d0, i, e)

            r1d = xdireta(-10.0d0, 0.0d0, i, e)
            r2d = xdireta(0.0d0, 5.0d0, i, e)
            r3d = xdireta(5.0d0, 10.0d0, i, e)
            
            write(*,*) i-1, r1n, r2n, r3n, r1d, r2d, r3d
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

      function xdireta(a,b,n,e)
         implicit real*8 (a-h, o-z)
         a_new = a
         b_new = b
         f_a = f(a_new)
         f_b = f(b_new)
         if(f_a*f_b .gt. 0.0d0) then
            write(*,*) "Erro: a função não muda de sinal em [a,b]."
            xdireta = 0.0d0
            return
         end if

         xinterv = abs(b_new-a_new)
         x0 = (a_new+b_new)/2.0d0
         f_x = f(x0)
         iter=1
 99      if (iter .lt. n) then
            if(xinterv .le. e) then
               xdireta = x0
               return
            end if

            if(f_a * f_x > 0.0d0) then
               a_new = x0
               f_a = f_x
            else
               b_new = x0
               f_b = f_x
            end if

            xinterv = abs(b_new-a_new)
            x0 = (a_new+b_new)/2.0d0
            f_x = f(x0)
            iter = iter + 1
            goto 99
         end if
         xdireta = x0
         return
      end function