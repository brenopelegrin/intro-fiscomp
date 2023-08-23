      program Complexos
c     Tarefa 6 - Intro. Fiscomp - Prof. Alcaraz
c     Aluno: Breno Henrique Pelegrin da Silva (13687303)
         complex :: z
         write(*,100)
 100     format('Digite o valor de N: ', $)
         read(*,*) n
         pi = 4.0e0*atan(1.0e0)
         do i=1, n, 1
c     Transforma Re(z) e Im(z) em um número complexo do fortran
            zmod = 3.0e0**(1.0e0/N)
            preal = 2 + zmod*cos(2.0e0*pi*i/N)
            pimag = zmod*sin(2.0e0*pi*i/N)
            z = cmplx(preal, pimag)
            write(*,101) i, z
 101        format('z'(1i0) ' = ' (1f12.6) ' + ' (1f12.6)' i')
         enddo
      end program
