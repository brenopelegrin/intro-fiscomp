      program LogExpansaoDuplaPrecisao
         real*8 x, anterior, anovo, dlogfortran, eprec
         write(*,*) "Digite o valor de eprec: "
         read(*,*) eprec
         write(*,*) "Digite o número x para calcular ln(x): "
         read(*,*) x
c        sum( (1/n)*(1-x)^n)_1 ^infty
         anterior = -1*1.0e0 * (1.0e0-x)
         anovo = anterior + -1*(0.5e0 * (1.0e0-x)**2)
         n = 3
 100     if (abs(anovo - anterior) .gt. eprec) then
            anterior = anovo
            anovo = anterior + -1*((1.0e0/n)*((1.0e0-x)**n))
            n = n + 1
            goto 100
         end if
         dlogfortran = dlog(x)
         write(*,*) x
         write(*,*) "ln(", x, ") de precisão dupla: ", anovo
         write(*,*) "ln(", x, ") de precisão dupla do fortran: ", 
     &dlogfortran
      write(*,*) "Diferença: ", abs(dlogfortran-anovo)
      end program