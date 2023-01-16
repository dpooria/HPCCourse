module poisson_utils_m
   use precision, only:dp
   implicit none
contains
   subroutine check_iostat(iostat, msg)
      integer, intent(in) :: iostat
      character(len=*), intent(in) :: msg

      if ( iostat == 0 ) return

      write(*,'(a,i0,/,tr3,a)') 'ERROR = ', iostat, msg
      ! stop

   end subroutine check_iostat

   subroutine write_to_file(array, N, file_name, delta, time, &
      iters, error, full)
      implicit none
      real(dp), dimension(:, :, :), intent(in) :: array
      real(dp), intent(in) :: delta, time, error
      integer, intent(in) :: N, iters
      integer, intent(in) :: full
      character(len=*), intent(in) :: file_name
      integer :: u, i, j, k
      real(dp) :: x, y, z


      open(newunit=u, file=trim(file_name), status='replace')
      write(u, '(F0.8,A,I5,A,F0.8,A,I5)') time, ",", iters, ",",error, ",", N
      if (full == 1) then
         do k = 1, N+2
            z = real(k-1,kind=dp) * delta - 1.0_dp
            do j = 1, N+2
               y = real(j-1,kind=dp) * delta - 1.0_dp
               do i = 1, N + 2
                  x = real(i-1,kind=dp) * delta - 1.0_dp
                  write(u, '(4e16.6)') x, y, z, array(i, j, k)
               enddo
               write (u, '(10a)')
            enddo
            write (u, '(10a)')
         enddo
      endif
      close(u)
   end subroutine write_to_file


end module poisson_utils_m

