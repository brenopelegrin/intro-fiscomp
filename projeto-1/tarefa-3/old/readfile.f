      program Test
         parameter(max=16)
         dimension vetor(max)
         open(unit=50, file='input.data', status='old')
         ilinhas = 0
         do
            if (ilinhas .gt. max) then
               write(*,*) "Erro: O máximo de números suportados é:
     &         ", max
               call exit()
            end if
            read(50, *, end=10) vetor(ilinhas+1)
            ilinhas = ilinhas + 1
         end do
 10      close(unit=50)
         write(*,*) vetor
         write(*,*) ilinhas
      end program