      program MenoresNumeros
c     Tarefa 3 - Intro. Fiscomp - Prof. Alcaraz
c     Aluno: Breno Henrique Pelegrin da Silva (13687303)

c     Como não é permitido usar alocação dinâmica, então
c     define-se um valor máximo de tamanho para o vetor.
         parameter(maxlinhas = 1000)
         dimension alista(maxlinhas)
         write(*,100)
 100     format("Quantos M menores números você quer na saída: ", $) 
         read(*,*) m
c     Lê arquivo de input e coloca dentro de uma lista
         ilinhas = 0
         open(unit=50, file='entrada-3-13687303', status='old')
         do
            read(50, *, end=10) alista(ilinhas+1)
            ilinhas = ilinhas + 1
         end do
 10      close(unit=50)
c     Ordena os M primeiros e salva em um arquivo de output
         open(unit=51, file='saida-3-13687303', status='new')
         do i=1, m, 1
            amenor = alista(1)
            do j=i+1, ilinhas, 1
               if (alista(j) < amenor) then
                  aux = amenor
                  amenor = alista(j)
                  alista(j) = aux
               end if
            end do
            write(51, *) amenor
         end do
         close(unit=51)
      end program
