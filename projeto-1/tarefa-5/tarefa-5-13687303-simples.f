      program LogExpansaoSimplesPrecisao
         parameter (eprec = 1.0E-5)
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
         elogfortran = log(x)
         write(*,*) "ln(", x, ") de precisão simples: ", anovo
         write(*,*) "ln(", x, ") de precisão simples do fortran: ",
     &elogfortran
         write(*,*) "Diferença: ", abs(elogfortran-anovo)
      end program