      program Esferas
c     Calcula o volume de uma esfera em um espaço euclidiano
c     d-dimensional utilizando o método de Monte Carlo.
         real , dimension(:), allocatable :: r ! Vetor posição
         write(*,*) "Digite o numero M de pontos aleatórios:"
         read(*,*) m
         write(*,*) "Digite o numero d de dimensões:"
         read(*,*) idim
         allocate(r(idim))
         ndentro = 0
         ntotal = 0
c        Gera m pontos aleatórios
         do i=1, m, 1

c           Gera um ponto r aleatório d-dimensional limitado em [0,1]
            do j=1, idim, 1
               r(j) = rand()
            enddo

c           Calcula a distância euclidiana até o centro da esfera
            soma = 0
            do j=1, idim, 1
               soma =  soma + (r(j) - 0.5e0)**2
            enddo
            distancia = sqrt(soma)

c           Se distancia < R, então caiu dentro
            if(distancia .lt. 0.5e0) then
               ntotal = ntotal+1
               ndentro = ndentro+1
            else
               ntotal = ntotal+1
            endif
         enddo

         vol = real(ndentro)/ntotal
         write(*,*) "Volume d-dimensional:", vol
      end program