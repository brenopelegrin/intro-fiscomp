      program Andarilho
         parameter (iseed = 123)
         parameter (nandarilhos = 100000)
         parameter (mpassos = 1000)
         dimension ihist(-mpassos:mpassos)
         character fname*15
         call srand(iseed)
         
         do ifrac=2, 5, 1
            do i=-mpassos, mpassos, 1
               ihist(i) = 0
            end do
            p = 1.0e0/ifrac
            xsoma = 0
            x2soma = 0
            do j=1, nandarilhos, 1
               ix = 0
               do i=1, mpassos, 1
                  aleatorio = rand()
                  if(aleatorio .lt. p) then
                     ! esquerda
                     ix = ix + 1
                  else
                     ! direita
                     ix = ix - 1
                  end if
               end do
               xsoma = xsoma + ix
               x2soma = x2soma + ix**2
               ihist(ix) = ihist(ix) + 1
            end do

            write(*,*) "p, <x>  = ", p, ",", xsoma/nandarilhos
            write(*,*) "p, <x^2> = ", p, ",",  x2soma/nandarilhos

            !gera arquivo de histograma
            write(fname, 100) ifrac
 100        format("hist-1-" (I0) ".dat")
            open(unit=50, file=fname) 
            do i=-mpassos, mpassos, 1
               write(50,*) i, ihist(i)
            end do
            close(unit=50)
         end do

      end program
