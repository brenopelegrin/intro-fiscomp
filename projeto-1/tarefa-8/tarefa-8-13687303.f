      program EsferaGamma
         real, dimension(:), allocatable :: vetor
         pi = 4.D0*DATAN(1.D0)
         write(*,*) "Digite o numero de dimensões d e o raio R:"
         read(*,*) idim, r
         allocate(vetor(idim))
c        v(d) = (pi^(d/2) R^d )/f(d)
c        onde f(d) = g(d/2 + 1)
c        f(0) = 1
c        f(1) = 1/2 g(1/2) = 1/2 sqrt(pi)
c        f(d) = d/2 f(d-2), d >= 2
         vetor(0) = 1
         vetor(1) = 0.5*sqrt(pi)
         if (idim .ge. 2) then
            do i=2, idim, 1
               vetor(i) = (i/2.0e0)*vetor(i-2)
            enddo
         else if (idim .eq. 1) then
            vetor(idim) = vetor(1)
         else if (idim .eq. 0) then
            vetor(idim) = vetor(0)
         endif
         write(*,*) (pi**(idim/2.0e0) * R**idim)/vetor(idim)
      end program
