      program ProbDNA
         implicit real*16 (a-h, o-z)
         implicit integer*16 (i-n)

         pow = 1e9

         M = 4**(3*pow)
         N = 8*pow
         prod = 1

         do j=0, M+1, 1
            prod = prod * (1 - 1/(N - j))
         end do

         res = 1 - prod

         write(*,*) "Resultado: ", res
      end program