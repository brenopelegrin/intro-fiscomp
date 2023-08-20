      program MenoresPrimos
      write(*,*) "Digite um inteiro N: "
      read(*,*) n
c Abre o arquivo de saída.
      open(unit=3, file='output.data', status='new')
c Se n <= 1, não existem primos.
      if (n .gt. 1) then
            iatual = 2
c Laço while (numero_atual <= numero_maximo)
 100        if(iatual .le. n) then
c Todo número é divisível por 1 e por ele mesmo, então
c Podemos excluir esses casos do laço for para otimizar o código.
                  do i=2, iatual-1, 1
                        if(MOD(iatual, i) .eq. 0) then
                              iatual = iatual+1
                              goto 100
                        endif
                  enddo
c Como o laço concluiu sem redirecionar, então é um número primo.
                  write(3,*) iatual
                  iatual = iatual+1
                  goto 100
            endif
      else
         write(*,*) "Não existe primo <= 1"
      endif
      close(unit=3)
      end program


