      program Andarilho
         parameter (iseed = 123)
         parameter (mand = 100000)
         parameter (npassos = 1000)
         dimension ihist(-npassos:npassos)
         character fname*50
         call srand(iseed)
         
         do ifrac=2, 5, 1
            do i=-npassos, npassos, 1
               ihist(i) = 0
            end do
            p = 1.0e0/ifrac
            xsoma = 0
            x2soma = 0
            do j=1, mand, 1
               ix = 0
               do i=1, npassos, 1
                  aleatorio = rand()
                  if(aleatorio .lt. p) then
                     ! x esquerda
                     ix = ix - 1
                  else
                     ! x direita
                     ix = ix + 1
                  end if
               end do
               xsoma = xsoma + ix
               x2soma = x2soma + ix**2
               ihist(ix) = ihist(ix) + 1
            end do

            write(*,*) "p, <x>  = ", p, ",", xsoma/mand
            write(*,*) "p, <x^2> = ", p, ",",  x2soma/mand

            !gera arquivo de histograma
            write(fname, 100) ifrac
 100        format("saida-b-13687303-hist-p1over" (I0) ".dat")
            open(unit=50, file=fname) 
            do i=-npassos, npassos, 1
               write(50,*) i, ihist(i)
            end do
            close(unit=50)
         end do

      end program
