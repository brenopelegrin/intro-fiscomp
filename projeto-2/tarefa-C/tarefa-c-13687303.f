      program Andarilho2D
         parameter (iseed = 123)
         parameter (mand = 1000)
         character fname*50
         call srand(iseed)
         do l=1, 6, 1
            npassos = 10**l
            xsoma = 0
            ysoma = 0
            x2soma = 0
            y2soma = 0
            write(fname, 101) l
 101        format("saida-c-13687303-n1e", (I0), ".dat")
            open(50, file=fname)
            do j=1, mand, 1
             ix = 0  
             iy = 0
              do i=1, npassos, 1
                prob = rand()
                ! -x
                if(prob .lt. 0.25e0) ix = ix - 1
                ! +x
                if(prob .ge. 0.25e0 .and. prob .lt. 0.5e0) ix = ix + 1
                ! -y
                if(prob .gt. 0.5e0 .and. prob .lt. 0.75e0) iy = iy - 1
                ! +y
                if(prob .ge. 0.75e0 .and. prob .lt. 1.0e0) iy = iy + 1
              end do
              xsoma = xsoma + ix
              ysoma = ysoma + iy
              x2soma = x2soma + ix**2
              y2soma = y2soma + iy**2
              write(50, *) ix, iy
            end do
            close(50)
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
         end do
         write(*,*) "==="
      end program
