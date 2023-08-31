      program LnDupla
         implicit real*8 (a-h, o-z)
c     Tarefa 5 (simples precisão) - Intro. Fiscomp - Prof. Alcaraz
c     Aluno: Breno Henrique Pelegrin da Silva (13687303)
         write(*,100)
 100     format("Digite o valor de eprec: ", $)
         read(*,*) eprec

         write(*,101)
 101     format("Digite x para calcular ln(x): ", $)
         read(*,*) x

         aLnFortran = dlog(x)

         if (x .le. 0.0d0) then
            write(*,*) "Erro: ln(x) não está definido para x <= 0."
            call exit()
         else if (x .le. 1.0d0) then
            aLn = fLnSerie(x, eprec)
         else if (x .gt. 1.0d0) then
c     Como a série só converge para x entre 0 e 2, podemos fazer
c     -ln(1/x) = ln(x), já que 1/x < 1 quando x > 1
            aLn = -1*fLnSerie(1/x, eprec)
         end if

         diff = abs(aLn - aLnFortran)
         write(*,*) "ln(x) dupla precisão = ", aLn
         write(*,*) "ln(x) fortran dupla precisão = ",
     &aLnFortran
         write(*,*) "Diferença: ", diff

      end program
      
      function fLnSerie(x, eprec)
c        Função válida apenas para 0 <= x <= 2
         implicit real*8 (a-h, o-z)
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