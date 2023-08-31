      program NumerosPrimos
c     Tarefa 4 - Intro. Fiscomp - Prof. Alcaraz
c     Aluno: Breno Henrique Pelegrin da Silva (13687303)
c         write(*,100)
c 100     format("Digite um inteiro N: ", $)
c         read(*,*) n
         n = 100
         open(unit=50, file='saida-4-13687303', status='new')
c     Se n <= 1, não existem primos.
         if (n .gt. 1) then
            iatual = 2
c     Laço while (numero_atual <= numero_maximo)
 101        if(iatual .le. n) then
c     Todo número é divisível por 1 e por ele mesmo, então
c     podemos excluir esses casos para otimizar o código.
               do i=2, iatual-1, 1
                  if(MOD(iatual, i) .eq. 0) then
                     iatual = iatual+1
                     goto 101
                  endif
               enddo
c     Como o laço concluiu sem redirecionar, então é um número
c     primo.
               write(50,*) iatual
               iatual = iatual+1
               goto 101
            endif
         else
            write(*,*) "Não existe primo <= 1"
         endif
         close(unit=50)
      end program
