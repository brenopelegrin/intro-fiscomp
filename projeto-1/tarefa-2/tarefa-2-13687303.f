      program Triangulo
c     Programa para calcular área de um triângulo, dados
c     dois vetores do espaço R3.
      dimension v1(3)
      dimension v2(3)
      dimension vetorial(3)

      write(*,*) "Digite as componentes x,y,z do vetor v1:"
      read(*,*) v1
      write(*,*) "Digite as componentes x,y,z do vetor v2:"
      read(*,*) v2

c     Calcula o produto vetorial v1 x v2
      vetorial(1) = v1(2)*v2(3) - v2(2)*v1(3) ! i*(y1*z2 - y2*z1)
      vetorial(2) = v1(3)*v2(1) - v2(3)*v1(1) ! j*(z1*x2 - z2*x1)
      vetorial(3) = v1(1)*v2(2) - v2(1)*v1(2) ! k*(x1*y2 - x2*y1)

c     Area = 0.5*modulo(vetorial)
      area = 0.5e0 * sqrt(vetorial(1)**2 + vetorial(2)**2 +
     &vetorial(3)**2)

      write(*,*) "Área:", area
      end program
