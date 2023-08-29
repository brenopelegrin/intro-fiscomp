      program EsferasMonteCarlo
c     Tarefa 7 - Intro. Fiscomp - Prof. Alcaraz
c     Aluno: Breno Henrique Pelegrin da Silva (13687303)
         write(*,100)
 100     format("Digite o numero M de pontos aleatórios: ", $)
         read(*,*) m
         write(*,101)
 101     format("Digite o numero d de dimensões: ", $)
         read(*,*) idim
         ndentro = 0
         ntotal = 0
         do i=1, m, 1
            dist = 0
c     Gera d coordenadas aleatórias e calcula a distância euclidiana
            do j=1, idim, 1
               coord = rand()
               dist = dist + (coord - 0.5e0)**2
            enddo
c     Se satisfaz a equação da d-esfera, então caiu dentro da d-esfera
            if(sqrt(dist) .lt. 0.5e0) then
               ndentro = ndentro + 1
               ntotal =  ntotal + 1
            else
               ntotal = ntotal + 1
            endif
         enddo
         volume = real(ndentro)/ntotal
         write(*,*) "Volume da d-esfera:", volume
      end program