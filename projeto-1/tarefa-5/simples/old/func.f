      program TestFunc
         parameter( eprec = 1.0E-5)
         a = 1.0
         c = 3.4
         b = aloop(a)
         print *, b
         print *, c
      end program

      function aloop(x)
         soma = x
         print *, eprec
         do i=1, 3, 1
            soma = soma + 1
         end do
         print *, a, c, d
         c = 2.1
         aloop = soma
         return
      end function