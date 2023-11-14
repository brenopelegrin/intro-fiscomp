      program TarefaB
         implicit real*8 (a-h,o-z)
         parameter (iangulos = 200)
         parameter (n = 1000)
         parameter (eps = 0.07)
         dimension theta(iangulos)

         pi = 4.0d0*atan(1.0d0)
         theta(1) = pi/2.0d0

         open(50, file='periodos.csv')
         open(60, file='periodos_anl.csv')
         write(50,100)
 100     format("theta,periodo")
         write(60,101)
 101     format("theta,periodo_anl")

         do i=1, iangulos-1, 1
            theta(i+1) = theta(i) - pi/(2*iangulos)
            periodo = 0.0d0
            do j=1, 10, 1
               periodo = periodo + simulate(theta(i), 0d0, 0d0, 0d0)
            end do
            periodo_anl = blint(-theta(i)+eps, theta(i)-eps, n, t0)
            periodo = periodo/10.0d0
            write(50,*) theta(i), ",", periodo
            write(60,*) theta(i), ",", periodo_anl
         end do
         close(50)
         close(60)
      end program

      function simulate(theta0, gamma, f0, omega)
         implicit real*8 (a-h,o-z)
         parameter(dt = 1e-2)
         parameter(ipassos = 5000)
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

      function func(x, t0)
         implicit real*8 (a-h,o-z)
         func = 1/(cos(x) - cos(t0))
         return
      end function

      function fn(x0, n, h, t0)
         implicit real*8 (a-h,o-z)
         fn = func(x0 + n*h, t0)
         return
      end function fn


      function blint(a,b,n,t0)
         implicit real*8 (a-h,o-z)
         blint=0
         h=(b-a)/n
         do i=0, n-4, 4
            x0=a+i*h
            blint=blint+(2.0d0*h/45.0d0)*(7.0d0*fn(x0, 0, h, t0) + 32.0
     &d0*fn(x0,1, h, t0) + 12.0d0*fn(x0, 2, h, t0) + 32.0d0*fn(x0, 3, h
     &, t0) + 7.0d0*fn(x0,4, h,t0))
         end do
         return
      end function 
