      program Andarilho2D
         parameter (iseed = 123)
         parameter (nand = 1000)
         dimension rxand(nand)
         dimension ryand(nand)

         character fname*50
         call srand(iseed)

         do l=1, 6, 1
            mpassos = 10**l
            xsoma = 0
            ysoma = 0
            x2soma = 0
            y2soma = 0
            do j=1, nand, 1
               ix = 0
               iy = 0
               do i=1, mpassos, 1
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
            ravgx = real(xsoma)/nand
            ravgy = real(ysoma)/nand
            r2avg = (real(x2soma)/nand + real(y2soma)/nand)
            d2 =  r2avg - (ravgx**2 + ravgy**2)
            write(*,*) "------------------------"
            write(*,*) "mpassos =", mpassos
            write(*,*) "<x> =", xsoma/nand
            write(*,*) "<y> =", ysoma/nand
            write(*,*) "<(rx, ry)> =", ravgx, ", ", ravgy
            write(*,*) "<(rx,ry)^2> =", r2avg
            write(*,*) "Delta^2 =", d2
            write(fname, 100) mpassos, nand
 100        format("pos-m-", (I0), "-n-", (I0), ".dat")
            open(50, file=fname)
            do i=1, nand, 1
               write(50, *) rxand(i), ryand(i)
            end do
            close(50)
         end do
      end program