      program LnDupla
c     Tarefa 5 (dupla precisão) - Intro. Fiscomp - Prof. Alcaraz
c     Aluno: Breno Henrique Pelegrin da Silva (13687303)
         real*8 x, aLnFortran, aLn, aLn2, diff, fLnSerie, eprec
         write(*,100)
 100     format("Digite x para calcular ln(x): ", $)
         read(*,*) x
         write(*,101)
 101     format("Digite a precisão eprec: ", $)
         read(*,*) eprec
         aLnFortran = dlog(x)

         if (x .le. 0.0d0) then
            write(*,*) "Erro: ln(x) não está definido para x <= 0."
            call exit()
         else if (x .le. 2.0d0) then
            aLn = fLnSerie(x, eprec)
         else if (x .gt. 2.0d0) then
            aLn2 = fLnSerie(2.0d0, eprec)
            iMult = 0
c     Loop while( x >= 2.0)
c     Iremos decompor ln(x) em ln(x/2*n) + n*ln(2) até que x/2*n < 2
 10         if (x .gt. 2.0d0) then
               x = x/2.0d0
               iMult = iMult + 1
               goto 10
            else
               write(*,*) x
               aLn = fLnSerie(x, eprec) + iMult*aLn2
            end if
         end if

         diff = abs(aLn - aLnFortran)
         write(*,*) "ln(x) dupla precisão = ", aLn
         write(*,*) "ln(x) fortran dupla precisão = ",
     &aLnFortran
         write(*,*) "Diferença: ", diff

      end program
      
      real*8 function fLnSerie(x, eprec)
c        Função válida apenas para 0 <= x <= 2
         real*8 soma, anterior, x, eprec
         soma = 0.0d0
         i = 1
         do
            anterior = soma
            soma = soma - ((1.0d0-x)**i)/i
            if(abs(soma - anterior) .lt. eprec) then
               exit
            else
               i = i + 1
            end if
         end do
         fLnSerie = soma
         return 
      end function
