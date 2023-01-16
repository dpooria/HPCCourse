module poisson_methods_m

   ! Please do not put any computational variable in this module.
   ! Pass all variables as arguments to the subroutine.
   use precision ! use dp as the data-type (and for defining constants!)
   use omp_lib

   implicit none

   private
   public :: jacobi
   ! public :: gauss_seidel

contains
   subroutine jacobi(u, time, iters, error, u_old, f, delta, N, tolerance, itermax)
      implicit none
      real(dp), dimension(0:, 0:, 0:), intent(inout) :: u
      real(dp), intent(out)::time, error
      integer, intent(out) :: iters
      real(dp), dimension(:, :, :), intent(in) :: f
      real(dp), intent(in) :: delta, tolerance
      real(dp), dimension(0:, 0:, 0:), intent(inout) :: u_old
      integer, intent(in) :: N, itermax
      integer :: i, j, k, t
      real(dp) :: d = 99999.0
      real(dp) :: ti, tf
      ti = omp_get_wtime()
      do t = 1, itermax
         d = 0.0_dp
         u_old(1:N, 1:N, 1:N) = u(1:N, 1:N, 1:N)
         !$OMP parallel do default(none) shared(u_old, u, f, delta, N)&
         !$OMP& private(i, j, k) reduction(+: d)
         do k = 1, N
            do j = 1, N
               do i = 1, N
                  u(i, j, k) = 1.0_dp / 6.0_dp * (u_old(i-1, j, k) + u_old(i+1, j, k) &
                     + u_old(i, j+1, k) + u_old(i, j-1, k) &
                     + u_old(i, j, k + 1) + u_old(i, j, k - 1) + (delta**2) * f(i, j, k))
                  d = d + (u(i, j, k) - u_old(i, j, k))**2
               enddo
            enddo
         enddo
         !$OMP end parallel do
         d = sqrt(d/N**3)
         if (d .lt. tolerance) exit
      enddo
      tf = omp_get_wtime()
      time = tf - ti
      error = d
      iters = t
   end subroutine jacobi

end module poisson_methods_m
