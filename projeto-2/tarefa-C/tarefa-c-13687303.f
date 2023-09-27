      program Andarilho2D
         parameter (iseed = 123)
         parameter (mand = 1000)
         dimension rxand(mand)
         dimension ryand(mand)

         character fname*50
         call srand(iseed)

         do l=1, 6, 1
            npassos = 10**l
            xsoma = 0
            ysoma = 0
            x2soma = 0
            y2soma = 0
            do j=1, mand, 1
               ix = 0
               iy = 0
               do i=1, npassos, 1
                  prob = rand()
                  if(prob .lt. 0.25e0) then
                     ! esquerda x
                     ix = ix - 1
                  end if
                  if(prob .ge. 0.25e0 .and. prob .lt. 0.5e0) then
                     ! direita x
                     ix = ix + 1
                  end if
                  if(prob .gt. 0.5e0 .and. prob .lt. 0.75e0) then
                     ! esquerda y
                     iy = iy - 1
                  end if
                  if(prob .ge. 0.75e0 .and. prob .lt. 1.0e0) then
                     ! direita y
                     iy = iy + 1
                  end if
               end do
               xsoma = xsoma + ix
               ysoma = ysoma + iy
               x2soma = x2soma + ix**2
               y2soma = y2soma + iy**2
               rxand(j) = ix
               ryand(j) = iy
            end do
            ravgx = real(xsoma)/mand
            ravgy = real(ysoma)/mand
            r2avg = (real(x2soma)/mand + real(y2soma)/mand)
            d2 =  r2avg - (ravgx**2 + ravgy**2)
            write(*,*) "==="
            write(*,100) l
 100        format("npassos = 1E+" (I0))
            write(*,*) "<(rx, ry)> =", ravgx, ", ", ravgy
            write(*,*) "<(rx,ry)^2> =", r2avg
            write(*,*) "Delta^2 =", d2
            write(fname, 101) npassos
 101        format("saida-c-13687303-1e", (l), ".dat")
            open(50, file=fname)
            do i=1, mand, 1
               write(50, *) rxand(i), ryand(i)
            end do
            close(50)
         end do
         write(*,*) "==="
      end program
