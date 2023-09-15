      program tarefaA
         write(*,100)
 100     format("Digite o número N: ", $)
         read(*,*) n
         
         do j=1, 4, 1
            soma = 0
            do i=1, n, 1
               soma = soma + rand()**j
            end do
            write(*, 101) j, soma/n
 101        format("<x^", (I0), "> = ", (F5.4))
         end do
      end program