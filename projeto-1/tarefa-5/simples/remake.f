      program LnSimples
c     Tarefa 5 (simples precisão) - Intro. Fiscomp - Prof. Alcaraz
c     Aluno: Breno Henrique Pelegrin da Silva (13687303)
         write(*,100)
 100     format("Digite x para calcular ln(x): ", $)
         read(*,*) x
         aLnFortran = log(x)

         if (x .le. 0.0e0) then
            write(*,*) "Erro: ln(x) não está definido para x <= 0."
            call exit()
         else if (x .le. 2.0e0) then
            aLn = fLnSerie(x)
         else if (x .gt. 2.0e0) then
            aLn2 = fLnSerie(2.0e0)
            iMult = 0
c     Loop while( x >= 2.0)
c     Iremos decompor ln(x) em ln(x/2*n) + n*ln(2) até que x/2*n < 2
 10         if (x .gt. 2.0e0) then
               x = x/2.0e0
               iMult = iMult + 1
               goto 10
            else
               aLn = fLnSerie(x) + iMult*aLn2
            end if
         end if

         diff = abs(aLn - aLnFortran)
         write(*,*) "ln(x) simples precisão = ", aLn
         write(*,*) "ln(x) fortran simples precisão = ",
     &aLnFortran
         write(*,*) "Diferença: ", diff

      end program
      
      function fLnSerie(x)
c        Função válida apenas para 0 <= x <= 2
         parameter( eprec = 1.0E-5)
         soma = 0.0e0
         i = 1
         do
            anterior = soma
            soma = soma - ((1.0e0-x)**i)/i
            if(abs(soma - anterior) .lt. eprec) then
               exit
            else
               i = i + 1
            end if
         end do
         fLnSerie = soma
         return 
      end function