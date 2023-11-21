      program TarefaB1eB2
         implicit real*8 (a-h,o-z)
         parameter (iangulos = 100)
         parameter (idivisoes = 1000)
         dimension theta(iangulos)

         pi = 4.0d0*atan(1.0d0)
         theta(1) = pi/2.0d0

         open(50, file='periodos.csv')
         write(50,100)
 100     format("theta,periodo_sim,periodo_int,periodo_apx")
         do i=1, iangulos-1, 1
            theta(i+1) = theta(i) - pi/(2*iangulos)
            periodo = 0.0d0
            do j=1, 10, 1
               periodo = periodo + simulate(theta(i), 0d0, 0d0, 0d0)
            end do
            periodo = periodo/10.0d0
            periodo_int = spint(0.0d0, pi/2.0d0, idivisoes,
     &theta(i))*4.0d0
            periodo_apx = 2.0d0*pi*(1 + (theta(i)**2)/16.0d0)
            write(50,*) theta(i), ",", periodo, ",", periodo_int, ",",
     &periodo_apx
         end do
         close(50)
      end program

      function simulate(theta0, gamma, f0, omega)
         implicit real*8 (a-h,o-z)
         parameter(ipassos = 3000)
         parameter(dt = 1e-2)
         parameter(g = 9.8d0)
         parameter(al = 9.8d0)
         parameter(m = 1.0d0)

         dimension t(ipassos), w(ipassos), e(ipassos), theta(ipassos)

         simulate = 0.0d0
         pi = 4.0d0*atan(1.0d0)

         theta(1) = theta0
         w(1) = 0.0d0
         e(1) = m*g*al*(1-cos(theta0))
         t(1) = 0.0d0
         iosc = 0

         do i=1, ipassos-1, 1
            t(i+1) = i*dt
            w(i+1) = w(i) + (- (g/al)*sin(theta(i)) - gamma*w(i) + f0*s
     &in(omega*t(i)))*dt
            if(w(i+1) * w(i) .lt. 0) iosc = iosc + 1
            if(iosc .eq. 2) then
               simulate = t(i)
               return
            end if
            theta(i+1) = mod(theta(i) + w(i+1)*dt, 2.0d0*pi)
c           E = U + K = mgl * (1 - cos(theta)) + 0.5 * m * (w*l)^2
            e(i+1) = m*g*al*(1-cos(theta(i+1)))+ 0.5d0*m*(w(i+1)*al)**2
         end do
      end function

      function funk(x, x0)
         implicit real*8 (a-h,o-z)
c        f(x) = 1/sqrt(1 - (sin(x0/2)*sin(x))^2 )
         funk = 1.0d0/sqrt(1-(sin(x0/2.0d0)**2)*(sin(x)**2))
         return
      end function

      function func(x, x0)
         implicit real*8 (a-h,o-z)
         func = 1.0d0/sqrt(cos(x) - cos(x0))
         return
      end function

      function fn(x0, n, h, b0)
         implicit real*8 (a-h,o-z)
         fn = funk(x0 + n*h, b0)
         return
      end function fn

      function spint(a,b,n,b0)
         implicit real*8 (a-h,o-z)
         spint=0.0d0
         h=(b-a)/real(n)
         do i=1, n-1, 2
            x0=a+i*h
            spint=spint+(h/3.0d0)*(fn(x0, 1, h, b0) + 4.0d0*fn(x0, 0, h
     &,b0) + fn(x0,-1, h, b0))
         end do
         return
      end function

      function trint(a,b,n,b0)
         implicit real*8 (a-h,o-z)
         trint=0.0d0
         h=(b-a)/n
         do i=1, n-1, 2
            x0=a + i*h
            trint=trint+(h/2.0d0)*(fn(x0, -1, h, b0) + 2.0d0*fn(x0, 0,
     &h, b0)+fn(x0,1,h,b0))
         end do
         return
      end function



