      program Andarilho2D
         parameter (iseed = 123)
         parameter (nand = 100)
         parameter (lado = 10)
         parameter (mpassos = 1500)
         dimension rxand(nand)
         dimension ryand(nand)

         call srand(iseed)
         do i=1, nand, 1
            rxand(i) = 0
            ryand(i) = 0
         end do

         open(50, file='entropia.dat')
         do i=1, mpassos, 1
            do j=1, nand, 1
               prob = rand()
               if(prob .lt. 0.25e0) then
                  ! esquerda x
                  rxand(j) = rxand(j) - 1
               end if
               if(prob .ge. 0.25e0 .and. prob .lt. 0.5e0) then
                  ! direita x
                  rxand(j) =r xand(j) + 1
               end if
               if(prob .gt. 0.5e0 .and. prob .lt. 0.75e0) then
                  ! esquerda y
                  ryand(j) = ryand(j) - 1
               end if
               if(prob .ge. 0.75e0 .and. prob .lt. 1.0e0) then
                  ! direita y
                  ryand(j) = ryand(j) + 1
               end if
            end do
            
            s = 0.0e0
            do ix=-mpassos, mpassos, lado
               do iy=-mpassos, mpassos, lado
                  icount = 0
                  do j=1, nand, 1
                     if( rxand(j) .gt. ix .and. rxand(j) .lt. ix+lado
     &.and. ryand(j) .gt. iy .and. ryand(j) .lt. iy+lado) then
                        icount = icount + 1
                     end if
                  end do
                  if(icount .gt. 0) then
                     probi = real(icount)/nand
                     s = s - (probi)*log(probi)
                  end if
               end do
            end do
            write(50,*) i, s
         end do
         close(50)
      end program