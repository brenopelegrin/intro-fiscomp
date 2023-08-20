      program MenoresNumeros
c     Programa para ordenar uma lista em ordem crescente e mostrar os
c     M menores numeros, usando um tamanho pré-fixado para o arquivo
      dimension vetor(1000)
      write(*,*) "Quantidade N de numeros a serem lidos:"
      read(*,*) n
      write(*,*) "Quantos M menores números você quer ver na tela?"
      read(*,*) m
c     Lê arquivo com números
      open(unit=3, file='input.data', status='old')
      read(3,*) vetor
      close(unit=3)
c     Ordenação por selection sort
      do i = 1, n
        do j = i+1, n
          if (vetor(j) < vetor(i)) then
            aux = vetor(j)
            vetor(j) = vetor(i)
            vetor(i) = aux
          else
            continue
          end if
        end do
      end do
c     Escreve os M menores valores no arquivo out
      open(unit=3, file='output.data', status='new')
      write(3,*) "M=", m
      do i = 1, m
        write(3,*) vetor(i)
      end do
      close(unit=3)
      end program
