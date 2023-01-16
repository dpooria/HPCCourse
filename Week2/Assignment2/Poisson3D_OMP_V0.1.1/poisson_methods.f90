module poisson_methods_m

   ! Please do not put any computational variable in this module.
   ! Pass all variables as arguments to the subroutine.
   use precision ! use dp as the data-type (and for defining constants!)
   use omp_lib, only:omp_get_wtime

   implicit none

   private
   public :: jacobi
   ! public :: gauss_seidel

contains
   subroutine jacobi(u, time, iters, error, u_old, f, delta, N, tolerance, itermax)
      implicit none
      real(dp), dimension(:, :, :), intent(inout) :: u
      real(dp), intent(out)::time, error
      integer, intent(out) :: iters
      real(dp), dimension(:, :, :), intent(in) :: f
      real(dp), intent(in) :: delta, tolerance
      real(dp), dimension(:, :, :), intent(inout) :: u_old
      integer, intent(in) :: N, itermax
      integer :: i, j, k, t
      real(dp) :: d = 99999.0
      real(dp) :: ti, tf, sqrtN3, delta2, onesixth,tol2

      onesixth = 1.0_dp / 6.0_dp
      sqrtN3 = sqrt(real(N**3, kind=dp))
      delta2 = delta**2
      tol2 = tolerance**2
      iters = 0

      ti = omp_get_wtime()
      do t = 1, itermax
         d = 0.0_dp
         !$OMP parallel default(none) shared(u_old, u, f, delta, N, delta2, onesixth, d)&
         !$OMP& private(i, j, k)
         !$OMP workshare
            u_old(2:N+1, 2:N+1, 2:N+1) = u(2:N+1, 2:N+1, 2:N+1)
         !$OMP end workshare
         !$OMP do reduction(+: d)
         do k = 2, N+1
            do j = 2, N+1
               do i = 2, N+1
                  u(i, j, k) = onesixth * (u_old(i-1, j, k) + u_old(i+1, j, k) &
                     + u_old(i, j+1, k) + u_old(i, j-1, k) &
                     + u_old(i, j, k + 1) + u_old(i, j, k - 1) + delta2 * f(i-1, j-1, k-1))
                  d = d + (u(i, j, k) - u_old(i, j, k))**2
               enddo
            enddo
         enddo
         !$OMP end do
         !$OMP end parallel
         if (d/sqrtN3 .lt. tol2) exit
      enddo
      tf = omp_get_wtime()
      
      time = tf - ti
      error = d
      iters = t - 1
   end subroutine jacobi

end module poisson_methods_m
