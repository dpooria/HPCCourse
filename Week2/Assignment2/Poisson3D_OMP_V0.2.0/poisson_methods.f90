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
   subroutine jacobi(u, time, iters, error, u_old, f, &
      delta, N, tolerance, itermax)
      implicit none
      real(dp), dimension(:, :, :), intent(inout) :: u
      real(dp), intent(out) :: time, error
      integer, intent(out) :: iters
      real(dp), dimension(:, :, :), intent(in) :: f
      real(dp), intent(in) :: delta, tolerance
      real(dp), dimension(:, :, :), intent(inout) :: u_old
      integer, intent(in) :: N, itermax
      logical :: notconverged = .true.
      integer :: i, j, k, t
      real(dp) :: d=999.0_dp
      real(dp) :: ti, delta2, onesixth, tol, N3
      N3 = real(N**3, kind=dp)
      onesixth = 1.0_dp / 6.0_dp
      delta2 = delta**2
      iters = 0
      tol = (tolerance**2) * N3
      ti = omp_get_wtime()
      !$OMP parallel default(none)&
      !$OMP& shared(u, u_old, f, delta2, onesixth, iters, itermax, N, tol, d, N3, notconverged) &
      !$OMP& private(i, j, k, t)
      do t = 1, itermax
         !$OMP single
         if(notconverged) then
            iters = iters + 1
            if(d .lt. tol) then
               notconverged = .false.
            else
               d = 0.0_dp
            endif
         endif
         !$OMP end single
         !copy u to u_old
         !$OMP do
         do k = 2, N + 1
            if (notconverged) then
            do j = 2, N + 1
               do i = 2, N + 1
                     u_old(i, j, k) = u(i, j, k)
                  enddo
               enddo
            endif
         enddo
         !$OMP end do
         !Jacobi step and calculating the error
         !$OMP do reduction(+:d)
         do k = 2, N + 1
            if(notconverged) then
               do j = 2, N + 1
                  do i = 2, N + 1
                     u(i, j, k) = onesixth * (u_old(i-1, j, k) + u_old(i+1, j, k) &
                        + u_old(i, j+1, k) + u_old(i, j-1, k) &
                        + u_old(i, j, k + 1) + u_old(i, j, k - 1) + delta2 * f(i-1, j-1, k-1))
                     d = d + (u(i, j, k) - u_old(i, j, k))**2
                  enddo
               enddo
            endif
         enddo
         !$OMP end do
      enddo
      !$OMP end parallel
      time = omp_get_wtime() - ti
      error = sqrt(d/N3)
   end subroutine jacobi

   subroutine gauss_seidel(u, time, iters, error, f, delta, N, tolerance, itermax)
      implicit none
      real(dp), dimension(:, :, :), intent(inout) :: u
      real(dp), intent(out) :: time, error
      integer, intent(out) :: iters
      real(dp), dimension(:, :, :), intent(in) :: f
      real(dp), intent(in) :: delta, tolerance
      integer, intent(in) :: N, itermax
      integer :: i, j, k, t
      real(dp) :: ti, N3, delta2, tol2
      real(dp), parameter :: onesixth = 1.0_dp / 6.0_dp
      real(dp) :: d = 99999.0_dp, u_old = 0.0_dp
      logical :: notconverged=.true.
      N3 = real(N**3, kind=dp)

      delta2 = delta**2
      tol2 = N3 * (tolerance**2)
      iters = 0
      ti = omp_get_wtime()
      !$OMP parallel default(none) & 
      !$OMP& private(t, i, j, k, u_old) & 
      !$OMP& shared(iters, u, notconverged, N, tol2, delta2, f, d, itermax)
      do t = 1, itermax
         !$OMP single
         if(notconverged) then
            iters = iters + 1
            if(d .lt. tol2) then
               notconverged = .false.
            else
               d = 0.0_dp
            endif
         endif
         !$OMP end single
         !$OMP do schedule(static,1) ordered(2) reduction(+:d)
         do k = 2, N + 1
            do j = 2, N+1
               !$OMP ordered depend(sink: k-1,j) depend(sink: k,j-1)
               do i = 2, N+1
                  if(notconverged) then
                     u_old = u(i, j, k)
                     u(i, j, k) = &
                        onesixth * (u(i-1, j, k) + u(i+1, j, k) +&
                        u(i, j+1, k) + u(i, j-1, k) + &
                        u(i, j, k + 1) + u(i, j, k - 1) + &
                        delta2 * f(i-1, j-1, k-1))
                     d = d + (u_old - u(i, j, k))**2
                  endif
               enddo
               !$OMP ordered depend(source)
            enddo
         enddo
         !$OMP end do
      enddo
      !$OMP end parallel
      time = omp_get_wtime() - ti
      error = sqrt(d/N3)
   end subroutine gauss_seidel

end module poisson_methods_m

