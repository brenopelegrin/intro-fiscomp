      program TarefaB3
         implicit real*8 (a-h,o-z)
         parameter(ipassos = 9000)
         parameter(g = 9.8d0)
         parameter(al = 9.8d0)
         parameter(m = 1.0d0)

         dimension t(ipassos), w(ipassos), e(ipassos), theta(ipassos)

         pi = 4.0d0*atan(1.0d0)

         theta0 = 0.14d0
         omega = 0.0d0
         gamma = 0.05d0
         f0 = 0.0d0
         dt=1e-2

         open(50, file="b3_theta_t.csv")
         write(50,100)
 100     format("t,theta")

         theta(1) = theta0
         w(1) = 0.0d0
         e(1) = m*g*al*(1-cos(theta0))
         t(1) = 0.0d0

         do i=1, ipassos-1, 1
            t(i+1) = i*dt
            w(i+1) = w(i) + (- (g/al)*sin(theta(i)) - gamma*w(i) + f0*s
     &in(omega*t(i)))*dt
            theta(i+1) = mod(theta(i) + w(i+1)*dt, 2.0d0*pi)
c           E = U + K = mgl * (1 - cos(theta)) + 0.5 * m * (w*l)^2
            e(i+1) = m*g*al*(1-cos(theta(i+1)))+ 0.5d0*m*(w(i+1)*al)**2
            write(50,*) t(i), ",", theta(i)
         end do
         close(50)
      end program