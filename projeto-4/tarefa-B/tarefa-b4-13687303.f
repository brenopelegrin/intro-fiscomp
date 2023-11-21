      program TarefaB4
         implicit real*8 (a-h,o-z)
         parameter(ipassos = 2500)
         parameter(g = 9.8d0)
         parameter(al = 9.8d0)
         parameter(m = 1.0d0)

         dimension t(ipassos), w(ipassos), e(ipassos), theta(ipassos),
     &f0(3)
         character fname*50
         pi = 4.0d0*atan(1.0d0)

         theta0 = 0.14d0
         omega = (2.0d0/3.0d0)
         gamma = 0.5d0
         f0 = [0.0d0, 0.5d0, 1.2d0]
         dt=0.04d0

c        Loop k_gamma para escolher qual gamma usar
c        k_gamma = 1 => gamma = 0.05 e k_gamma = 2 => gamma = 0.5
         do k_gamma = 1, 2, 1
         if(k_gamma .eq. 1) gamma = 0.05d0
         if(k_gamma .eq. 2) gamma = 0.5d0

c        Loop j_f0 para escolher qual f0 usar
c        j_f0 = 1 => f0 = 0, j_f0 = 2 => f0 = 0.5 e j_f0 = 3 => f0=1.2
         do j_f0=1, 3, 1
         write(fname,100) j_f0, k_gamma
 100     format('b4_dados_f0_', i1, '_g_', i1 ,'.csv')
         open(50, file=fname)
         write(50,101)
 101     format("t,theta,w")

         theta(1) = theta0
         w(1) = 0.0d0
         e(1) = m*g*al*(1-cos(theta0))
         t(1) = 0.0d0

         do i=1, ipassos-1, 1
            t(i+1) = i*dt
            w(i+1) = w(i) + (- (g/al)*sin(theta(i)) - gamma*w(i) + f0(j
     &_f0)*sin(omega*t(i)))*dt
            theta(i+1) = mod(theta(i) + w(i+1)*dt, 2.0d0*pi)
c           E = U + K = mgl * (1 - cos(theta)) + 0.5 * m * (w*l)^2
            e(i+1) = m*g*al*(1-cos(theta(i+1)))+ 0.5d0*m*(w(i+1)*al)**2
            write(50,*) t(i), ",", theta(i), ",", w(i)
         end do
         close(50)

         end do
         end do
      end program