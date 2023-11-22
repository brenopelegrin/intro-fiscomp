      program TarefaC
         implicit real*8 (a-h,o-z)
         parameter(ipassos = 3000)
         parameter(dt = 0.04d0)
         parameter(g = 9.8d0)
         parameter(al = 9.8d0)
         parameter(m = 1.0d0)
         parameter(iangulos = 4)

         dimension t(ipassos), w(ipassos), theta(ipassos)
         dimension f0_arr(2), theta0_arr(iangulos)
         character fname*50

         gamma = 0.5d0
         omega = 2.0d0/3.0d0
         f0_arr = [0.5d0, 1.2d0]
         do j=1, iangulos, 1
            theta0_arr(j) = 0.14d0 - 1e-3*j
         end do

         do i=1, 2, 1
            f0 = f0_arr(i)

            do j=1, iangulos, 1

            write(fname,100) i, j
 100        format("d_dados_f0_", I0,"_theta0_", I0, ".csv")
            open(50, file=fname)
            write(50,101)
 101        format("t,theta,w")
            theta0 = theta0_arr(j)
            call simulate(ipassos,dt,g,al,m,theta0,gamma,f0,omega,t,w
     &,theta)
            do k=1, ipassos, 1
               write(50,*) t(k), ",", theta(k), ",", w(k)
            end do
            close(50)

            end do
         end do
      end program

      subroutine simulate(ipassos,dt,g,al,m,theta0,gamma,f0,omega,t,w,t
     &heta)
         implicit real*8 (a-h,o-z)
         dimension t(*), w(*), theta(*)

         pi = 4.0d0*atan(1.0d0)

         theta(1) = theta0
         w(1) = 0.0d0
         t(1) = 0.0d0
         iosc = 0

         do i=1, ipassos-1, 1
            t(i+1) = i*dt
            w(i+1) = w(i) + (- (g/al)*sin(theta(i)) - gamma*w(i) + f0*s
     &in(omega*t(i)))*dt
            theta(i+1) = mod(theta(i) + w(i+1)*dt, 2.0d0*pi)
         end do
      end subroutine