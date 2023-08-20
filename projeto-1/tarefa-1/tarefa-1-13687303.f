      program Bhaskara
c     Programa para calcular raízes de uma equação de segundo grau, dados os coeficientes.
      print *, "Digite os coeficientes a, b, c da equação"
      
      read(*,*) a1, b1, c1
      delta = (b1**2) - (4*a1*c1)

      if (delta .ge. 0.0e0) then
            x1 = (-b1 + sqrt(delta))/(2*a1)
            x2 = (-b1 - sqrt(delta))/(2*a1)

            if (delta .eq. 0.0e0) then
                  print *, "Discriminante = 0, existem 2 raízes reais
     &iguais."
            else
                  print *, "Discriminante > 0, existem 2 raízes reais
     &diferentes."
            end if

            print *, "Raízes:", x1, x2 
      else
            print *, "Discriminante < 0, existem 2 raízes complexas di
     &ferentes. Não há raízes reais."
      end if
      end program Bhaskara
