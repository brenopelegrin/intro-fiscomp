      program Bhaskara
c     Tarefa 1 - Intro. Fiscomp - Prof. Alcaraz
c     Aluno: Breno Henrique Pelegrin da Silva (13687303)
         write(*,100)
 100     format("Digite os coef. a, b, c da equação ax²+bx+c=0: ", $)
         read(*,*) a, b, c
         delta = b**2 - 4*a*c
         if (delta .lt. 0.0e0) then
            write(*,*) "A equação não possui raízes reais."
         else
            if (delta .eq. 0.0e0) then
               x1 = (-b + sqrt(delta))/2*a
               write(*,*) "1 raíz real:", x1
            else if (delta .gt. 0.0e0) then
               x1 = (-b + sqrt(delta))/2*a
               x2 = (-b - sqrt(delta))/2*a
               write(*,*) "2 raízes reais:", x1, x2
            end if 
         end if
      end program