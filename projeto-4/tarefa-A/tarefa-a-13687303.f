      program TarefaA
         implicit real*8 (a-h,o-z)
         parameter(dt = 1e-2)
         parameter(ipassos = 2000)
         parameter(theta0 = 0.14d0)
         parameter(g = 9.8d0)
         parameter(al = 9.8d0)
         parameter(m = 1.0d0)
         dimension t(ipassos), w(ipassos), e(ipassos), theta(ipassos)
         dimension w_ec(ipassos), e_ec(ipassos), theta_ec(ipassos)
         pi = 4.0d0*atan(1.0d0)
         theta(1) = theta0
         theta_ec(1) = theta0
         w(1) = 0.0d0
         w_ec(1) = 0.0d0
         e(1) = m*g*al*(1-cos(theta0))
         e_ec(1) = m*g*al*(1-cos(theta0))
         t(1) = 0.0d0
         open(50, file="saida-a-13687303-theta_t.csv")
         open(51, file="saida-a-13687303-theta_ec_t.csv")
         open(60, file="saida-a-13687303-e_t.csv")
         open(61, file="saida-a-13687303-e_ec_t.csv")
         write(50,100)
 100     format("t,theta")
         write(51, 101)
 101     format("t,theta_ec")
         write(60,102)
 102     format("t,e")
         write(61, 103)
 103     format("t,e_ec")
         write(50,*) t(1), ",", theta(1)
         write(51,*) t(1), ",", theta_ec(1)
         write(60,*) t(1), ",", e(1)
         write(61,*) t(1), ",", e_ec(1)
         do i=1, ipassos-1, 1
            t(i+1) = i*dt
            w(i+1) = w(i) - (g/al)*theta(i)*dt
            w_ec(i+1) = w_ec(i) - (g/al)*theta_ec(i)*dt
            theta(i+1) = mod(theta(i) + w(i)*dt, 2.0d0*pi)
            theta_ec(i+1) = mod(theta_ec(i) + w_ec(i+1)*dt, 2.0d0*pi)
c           E = U + K = mgl * (1 - cos(theta)) + 0.5 * m * (w*l)^2
            e(i+1) = m*g*al*(1-cos(theta(i+1)))+ 0.5d0*m*(w(i+1)*al)**2
            e_ec(i+1) = m*g*al*(1-cos(theta_ec(i+1)))+0.5d0*m*(w_ec(i+1
     &)*al)**2
            write(50, *) t(i+1), ",", theta(i+1)
            write(51, *) t(i+1), ",", theta_ec(i+1)
            write(60, *) t(i+1), ",", e(i+1)
            write(61, *) t(i+1), ",", e_ec(i+1)
         end do
         close(50)
         close(51)
         close(60)
         close(61)
      end program