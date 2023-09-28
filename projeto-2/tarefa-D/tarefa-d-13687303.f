      program AndarilhoEntropia
         parameter (iseed = 123)
         parameter (nand = 1000)
         parameter (lado = 10)
         parameter (mpassos = 2000)
         dimension irxand(nand)
         dimension iryand(nand)

         call srand(iseed)
         irxand = 0
         iryand = 0

         open(50, file='saida-d-13687303.dat')
         do i=1, mpassos, 1
            do j=1, nand, 1
              prob = rand()
            if(prob .lt. 0.25e0) then
                  ! esquerda x
                  irxand(j) = irxand(j) - 1
               end if
               if(prob .ge. 0.25e0 .and. prob .lt. 0.5e0) then
                  ! direita x
                  irxand(j) = irxand(j) + 1
               end if
               if(prob .gt. 0.5e0 .and. prob .lt. 0.75e0) then
                  ! esquerda y
                  iryand(j) = iryand(j) - 1
               end if
               if(prob .ge. 0.75e0 .and. prob .lt. 1.0e0) then
                  ! direita y
                  iryand(j) = iryand(j) + 1
               end if
            end do

            ixmin = 0
            ixmax = 0
            iymin = 0
            iymax = 0
            
            do j=1, nand, 1
c              O intuito disso é otimizar o programa, encontrando
c              o limite máximo onde devemos percorrer o reticulado
               if(irxand(j) .lt. ixmin) ixmin = irxand(j)
               if(irxand(j) .gt. ixmax) ixmax = irxand(j)
               if(iryand(j) .lt. iymin) iymin = iryand(j)
               if(iryand(j) .gt. iymax) iymax = iryand(j)
            end do
            
            s = 0.0e0
            do ix=ixmin, ixmax, lado
               do iy=iymin, iymax, lado
c                 Percorre cada quadrado i do reticulado e conta
c                 o número de andarilhos, calculando S_i
                  icount = 0
                  do j=1, nand, 1
                     if( irxand(j) .gt. ix .and. irxand(j) .lt. ix+lado
     &.and. iryand(j) .gt. iy .and. iryand(j) .lt. iy+lado) then
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
