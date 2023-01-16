module poisson_methods_m

   ! Please do not put any computational variable in this module.
   ! Pass all variables as arguments to the subroutine.
   use precision ! use dp as the data-type (and for defining constants!)
   use omp_lib, only:omp_get_wtime
   implicit none

   private
   public :: jacobi
   public :: gauss_seidel

contains
   subroutine jacobi(u, time, iters, error, f, delta, N, tolerance, itermax)
      implicit none
      real(dp), intent(out) :: time, error
      integer, intent(out) :: iters
      real(dp), dimension(0:, 0:, 0:), intent(inout) :: u
      real(dp), dimension(:, :, :), intent(in) :: f
      real(dp), intent(in) :: delta, tolerance
      real(dp), allocatable, dimension(:, :, :) :: u_old
      integer, intent(in) :: N, itermax
      integer :: i, j, k, t
      real(dp) :: d = 99999.0, t_i, t_f
      ! print*, u(0, 1, 1), u(0, 1, 101)
      allocate(u_old(0:N+1,0:N+1,0:N+1))
      u_old = u
      t_i = omp_get_wtime()
      do t = 1, itermax
         u_old(1:N, 1:N, 1:N) = u(1:N, 1:N, 1:N)
         d = 0.0_dp
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
         !check convergence
         d = sqrt(d / N**3)
         if (d .lt. tolerance) exit
      enddo
      t_f = omp_get_wtime()
      iters = t
      time = t_f - t_i
      error = d
      deallocate(u_old)
   end subroutine jacobi

   subroutine gauss_seidel(u, time, iters, error, f, delta, N, tolerance, itermax)
      implicit none
      real(dp), intent(out) :: time, error
      integer, intent(out) :: iters
      real(dp), dimension(0:, 0:, 0:), intent(inout) :: u
      real(dp), dimension(:, :, :), intent(in) :: f
      real(dp), intent(in) :: delta, tolerance
      integer, intent(in) :: N, itermax
      integer :: i, j, k, t
      real(dp) :: u_old, t_i, t_f
      real(dp) :: d = 99999.0
      t_i = omp_get_wtime() 
      do t = 1, itermax
         d = 0.0_dp
         do k = 1, N
            do j = 1, N
               do i = 1, N
                  u_old = u(i, j, k)
                  u(i, j, k) = 1.0_dp / 6.0_dp * (u(i-1, j, k) + u(i+1, j, k) &
                     + u(i, j+1, k) + u(i, j-1, k) &
                     + u(i, j, k + 1) + u(i, j, k - 1) + (delta**2) * f(i, j, k))
                  d = d + (u_old - u(i, j, k))**2
               enddo
            enddo
         enddo
         !check convergence
         d = sqrt(d/N**3)
         if (d .lt. tolerance) exit
      enddo
      t_f = omp_get_wtime()
      iters = t
      time = t_f - t_i
      error = d
   end subroutine gauss_seidel

end module poisson_methods_m
