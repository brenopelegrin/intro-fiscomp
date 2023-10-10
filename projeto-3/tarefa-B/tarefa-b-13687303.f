      program Integrais
         implicit real*8 (a-h, o-z)
         parameter (anlint = 0.015616236904)
         dimension n(10)

         n(1) = 12
         
         do i=2, 10, 1
            n(i) = n(i-1)*2
         end do
         x0 = 0.0d0

         open(unit=50, file='int.csv')
         open(unit=51, file='erro.csv')

         write(50,100)
         write(51,100)
 100     format("n", ",", "h", ",", "tr", ",", "sp", ",", "bl")
         tr=0
         sp=0
         bl=0
         do i=1, 10, 1
            h = 1.0d0/n(i)
            tr = trint(0.0d0, 1.0d0, n(i))
            sp = spint(0.0d0, 1.0d0, n(i))
            bl = blint(0.0d0, 1.0d0, n(i))
            write(50,102) n(i), h, tr, sp, bl
            write(51,102) n(i), h, tr, sp, bl
 102        format(I0, ",", F24.11, ",", F24.11, ",", F24.11, "," F24.1
     &1)
         end do
         write(50,*) "exatos", ",", "exatos", ",", anlint, ",", anlint,
     &",", anlint
         close(50)
         close(51)
      end program

      function func(x)
         implicit real*8 (a-h,o-z)
         pi = 4.0d0*atan(1.0d0)
         func = exp(-x) * cos(2.0d0*pi*x)
         return
      end function func

      function fn(x0, n, h)
         implicit real*8 (a-h,o-z)
         fn = func(x0 + n*h)
         return
      end function fn

      function trint(a,b,n)
         implicit real*8 (a-h,o-z)
         trint=0
         h=(b-a)/n
         do i=1, n-1, 2
            x0=a+i*h
            trint=trint+(h/2.0d0)*(fn(x0, -1, h) + 2.0d0*fn(x0, 0, h)+f
     &n(x0,1, h))
         end do
         return
      end function

      function spint(a,b,n)
         implicit real*8 (a-h,o-z)
         spint=0
         h=(b-a)/n
         do i=1, n-1, 2
            x0=a+i*h
            spint=spint+(h/3.0d0)*(fn(x0, 1, h) + 4.0d0*fn(x0, 0, h) +
     &fn(x0,-1, h))
         end do
         return
      end function

      function blint(a,b,n)
         implicit real*8 (a-h,o-z)
         blint=0
         h=(b-a)/n
         do i=0, n-4, 4
            x0=a+i*h
            blint=blint+(2.0d0*h/45.0d0)*(7.0d0*fn(x0, 0, h) + 32.0d0*f
     &n(x0,1, h) + 12.0d0*fn(x0, 2, h) + 32.0d0*fn(x0, 3, h) + 7.0d0*fn
     &(x0,4, h))
         end do
         return
      end function 

      function anlint2(x)
         implicit real*8 (a-h,o-z)
         anlint2=0
         pi = 4.0d0*atan(1.0d0)
         anlint2=(exp(-x)/(1-4*(pi**2)))*(cos(2*pi*x) - 2*pi*sin(2
     &*pi*x))
         return
      end function