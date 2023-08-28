      program EsferaGamma
c     Tarefa 7 (precisão simples) - Intro. Fiscomp - Prof. Alcaraz
c     Aluno: Breno Henrique Pelegrin da Silva (13687303)
c     Como não é permitido usar alocação dinâmica, então
c     define-se um valor máximo de tamanho para o vetor.
         parameter(maxdim=1000)
         dimension vetor(0:maxdim)
         pi = 4.0e0*atan(1.0e0)
c     Define os valores iniciais da função gamma(d/2 + 1)
c     do denominador da equação de volume
         vetor(0) = 1
         vetor(1) = 0.5e0*sqrt(pi)
         open(unit=50, file='dimensoes-esferas.dat', status='new')
         write(*,100)
 100     format("Digite o numero de dimensões d e o raio R: ", $) 
         read(*,*) idim, r
c     O exercício pede para escrever apenas os volumes 2,3,...,d no
c     arquivo, então d = 0 e d = 1 não serão escritos no arquivo.
         if(idim .ge. 2 .and. idim .le. maxdim) then
            do i=2, idim, 1
               vetor(i) = (i/2.0e0)*vetor(i-2)
               write(50, *) i, (pi**(i/2.0e0) * r**i)/vetor(i)
            enddo
         else if (idim .eq. 0) then
            write(*,*) "Volume:", vetor(0)
         else if (idim .eq. 1) then
            write(*,*) "Volume:", vetor(1)
         else if (idim .lt. 0) then
            write(*,*) "Erro: Não existe volume negativo."
            call exit()
         else if (idim .gt. maxdim) then
            write(*,*) "Erro: A máxima dimensão suportada é ", maxdim
            call exit()
         endif
         close(unit=50)
      end program
