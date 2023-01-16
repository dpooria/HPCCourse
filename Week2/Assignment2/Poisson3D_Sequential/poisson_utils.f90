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

   subroutine write_to_file(array, N, file_name, delta, time,&
      iters, error, full)
      implicit none
      real(dp), dimension(0:, 0:, 0:), intent(in) :: array
      real(dp), intent(in) :: delta, time, error
      integer, intent(in) :: full
      integer, intent(in) :: N, iters
      character(len=*), intent(in) :: file_name
      integer :: u, i, j, k
      real(dp) :: x, y, z


      open(newunit=u, file=trim(file_name), status='replace')
      write(u, '(F0.6,A,I5,A,F0.6,A,I5)') time, ",", iters, ",",error, ",", N
      if (full == 1) then
         do k = 0, N+1
            z = k * delta - 1.0_dp
            do j = 0, N+1
               y = j * delta - 1.0_dp
               do i = 0, N + 1
                  x = i * delta - 1.0_dp
                  write(u, '(4e16.6)') x, y, z, array(i, j, k)
               enddo
               write (u, '(10a)')
            enddo
            write (u, '(10a)')
         enddo
         close(u)
      endif
   end subroutine write_to_file


end module poisson_utils_m

