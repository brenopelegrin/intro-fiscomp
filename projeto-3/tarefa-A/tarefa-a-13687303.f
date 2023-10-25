      program Derivadas
         implicit real*8 (a-h,o-z)
         dimension h(14)
         parameter(d1=9.79678201384)
         parameter(d2=64.09832454947)
         parameter(d3=671.51461345787)
         h = [5.0E-1, 2.0E-1, 1.0E-1, 5.0E-2, 1.0E-2, 5.0E-3, 1.0E-3,
     &5.0E-4, 1.0E-4, 5.0E-5, 1.0E-5, 1.0E-6, 1.0E-7, 1.0E-8]
         x0 = 0.5d0
         open(unit=50, file='saida-1-diff-13687303.csv')
         open(unit=51, file='saida-2-comp-13687303.csv')
         write(50,100) "h", "d1s3p", "d1f2p", "d1t2p", "d1s5p", "d2s5p"
     &,"d3a5p"
         write(51,100) "h", "d1s3p", "d1f2p", "d1t2p", "d1s5p", "d2s5p"
     &,"d3a5p"
 100     format(a, ",", a, ",", a, ",", a, ",", a, ",", a, ",", a)
         do i=1, 14, 1
            d1s3p = (fn(x0, 1, h(i)) - fn(x0, -1, h(i)))/(2*h(i))
            d1f2p = (fn(x0, 1, h(i)) - fn(x0, 0, h(i)))/(h(i))
            d1t2p = (fn(x0, 0, h(i)) - fn(x0, -1, h(i)))/(h(i))
            d1s5p = (fn(x0, -2, h(i)) - 8*fn(x0, -1, h(i))
     & + 8*fn(x0, 1, h(i)) - fn(x0, 2, h(i))) / (12*h(i))
            d2s5p = (-fn(x0, -2, h(i)) + 16*fn(x0, -1, h(i))  
     & - 30*fn(x0, 0, h(i)) + 16*fn(x0, 1, h(i)) - fn(x0, 2, h(i)))
     & / (12*(h(i)**2))
            d3a5p = (-fn(x0, -2, h(i)) + 2*fn(x0, -1, h(i))
     & - 2*fn(x0, 1, h(i)) + fn(x0, 2, h(i)))/(2*(h(i)**3))
         write(50,101) h(i), d1s3p, d1f2p, d1t2p, d1s5p, d2s5p, d3a5p
         write(51,101) h(i), abs(d1s3p-d1), abs(d1f2p-d1), abs(d1t2p-d1
     &), abs(d1s5p-d1), abs(d2s5p-d2), abs(d3a5p-d3)
 101     format(E16.2, ",", F24.11, ",", F24.11, ",", F24.11, ",",
     & F24.11, ",", F24.11, ",", F24.11)
         end do
      write(50,102) d1, d1, d1, d1, d2, d3
 102  format("exatos", ",", F24.11, ",", F24.11, ",", F24.11, ",",
     & F24.11, ",", F24.11, ",", F24.11)
      close(50)
      close(51)
      end program

      function func(x)
         implicit real*8 (a-h,o-z)
         func = exp(x/2.0d0) * tan(2.0d0*x)
         return
      end function func

      function fn(x0, n, h)
         implicit real*8 (a-h,o-z)
         fn = func(x0 + n*h)
         return
      end function fn

