program poisson3d

   ! Import different methods
   use precision
   use poisson_methods_m
   use poisson_utils_m

   ! Define options for user
   ! Please note that while we assign on definition, save is implicit.
   ! But please write save to show intent.

   integer, save :: N = 100
   real(dp), save :: T0 = 0._dp
   integer, save :: itermax = 1000
   real(dp), save :: tolerance = 1.e-4_dp
   integer, save :: output = 0 ! 3 for binary, 4 for VTK, 1 for Ascii
   integer, save :: algorithm = 1 ! 1 for Jacobi, 2 for Gauss-Seidel
   integer, save :: full = 0
   !  character(len=128) :: fname = 'poisson_u.dat'
   ! don't ask why, this is for historical reasons and to be compatible with the C-version
   ! of the assignment
   character(len=128) :: output_file=""
   real(dp), allocatable, dimension(:,:,:) :: u, f
   integer :: iostat
   integer :: i, j, k, iters
   real(dp) :: x, y, z, N_d, delta, time, error
   logical :: x_bool, y_bool, z_bool

   call get_command_argument(number=1, value=output_file)
   ! real(dp), parameter :: pi = 3.14159_dp
   ! Read in namelist.
   ! Call program namelist
   ! Create a file called: input.dat
   ! The content may be:
   ! &INPUT
   !   N = 100
   !   itermax = 3000
   !   T0 = 10.
   !   tolerance = 1e-5
   !   output = 0
   !   algorithm = 1
   ! /
   ! If one of the keywords are not in the
   ! input.dat namelist, then the default value
   ! will be used.
   call read_namelist()
   allocate(u(0:N+1,0:N+1,0:N+1), stat=iostat)
   !  call check_iostat(iostat, "Could not allocate 'u' matrix!")
   !  allocate(u_old(0:N+1,0:N+1,0:N+1), stat=iostat)
   call check_iostat(iostat, "Could not allocate 'u_old' matrix!")
   allocate(f(N, N, N), stat=iostat)
   call check_iostat(iostat, "Could not allocate 'f' matrix!")
   !
   !
   !
   !
   ! fill in your code here!
   !
   !
   !
   !
   !
   N_d = real(N, kind=dp)
   !  delta = 2.0_dp / (N_d - 1.0_dp)
   delta = 2.0_dp / (N_d + 1.0_dp)
   !Initialize f
   f = 0.0_dp
   do k = 1, N
      z = k * delta - 1.0_dp
      z_bool = (z .ge. -2.0_dp / 3.0_dp) .and. (z .le. 0.0_dp)
      if (z_bool) then
         do j = 1, N
            y = j * delta - 1.0_dp
            y_bool = (y .ge. -1.0_dp) .and. (y .le. -0.5_dp)
            if (y_bool) then
               do i = 1, N
                  x = i * delta - 1.0_dp
                  x_bool = (x .ge. -1.0_dp) .and. (x .le. -3.0_dp/8.0_dp)
                  if (x_bool) then
                     f(i, j, k) = 200.0_dp
                  endif
               enddo
            endif
         enddo
      endif
   enddo

   do k = 1,N
      do j = 1, N
         do i = 1, N
            u(i, j, k) = T0
            ! u(i, j, k) = 0.0_dp
         enddo
      enddo
   enddo

   do j = 0, N+1
      do i = 0, N+1
         ! Boundary conditions
         !y
         u(i, N + 1, j) = 20.0_dp
         u(i, 0, j) = 0.0_dp
         ! x
         u(N+1, i, j) = 20.0_dp
         u(0, i, j) = 20.0_dp
         ! z
         u(i, j, N + 1) = 20.0_dp
         u(i, j, 0) = 20.0_dp
      enddo
   enddo

   if (algorithm == 1) then
      call jacobi(u, time, iters, error, f, delta, N, tolerance, itermax)
   else
      call gauss_seidel(u, time, iters, error, f, delta, N, tolerance, itermax)
   endif
   open(1234, file='test.txt', action='write')
   !!!!!!!!!!!!!!!!!!!
   deallocate(f)
   ! Keep u until we have written in out
   select case ( output )
    case ( 0 ) ! pass, valid but not used value
      ! do nothing
    case ( 1 )
      call write_to_file(u, N, output_file, delta, time, &
         iters, error, full)
    case ( 3 ) ! write to binary file
      call write_binary(u)
    case ( 4 ) ! write to binary file
      call write_vtk(u)
    case default

      write(*,'(a,i0)') 'Unknown output type requested: ', output
      stop

   end select

   deallocate(u)

contains

   subroutine read_namelist()
      integer :: unit, io_err

      namelist /INPUT/ N, itermax, T0, tolerance, &
         output, algorithm, full

      ! open and read file
      unit = 128273598
      open(unit, FILE="input.dat", action='read', iostat=io_err)
      call check_iostat(io_err, &
         "Could not open file 'input.dat', perhaps it does not exist?")
      read(unit, nml=INPUT, iostat=io_err)
      call check_iostat(io_err, &
         "Error on reading name-list, please correct file content")
      close(unit)

   end subroutine read_namelist

   subroutine write_binary(u)
      ! Array to write-out
      real(dp), intent(in) :: u(:,:,:)

      ! Local variables
      character(len=*), parameter :: filename = 'poisson_u.bin'
      integer :: N
      integer :: iostat

      integer, parameter :: unit = 11249

      ! Get size of the array
      N = size(u, 1)

      ! replace == overwrite
      open(unit,file=filename,form='unformatted',access='stream',status='replace', iostat=iostat)
      call check_iostat(iostat, "Could not open file '"//filename//"'!")
      write(unit, iostat=iostat) N
      call check_iostat(iostat, "Could not write variable N")
      write(unit, iostat=iostat) u
      call check_iostat(iostat, "Could not write variable u")
      close(unit)

   end subroutine write_binary

   subroutine write_vtk(u)

      ! This is C-interoperability using bindings
      use, intrinsic :: iso_c_binding

      interface
         subroutine write_vtk_c(n, u) BIND(C, name="write_vtk")
            import c_ptr, c_int
            implicit none
            integer(c_int), value :: n
            type(c_ptr), value :: u
         end subroutine
      end interface

      ! Array to write-out
      real(dp), intent(in), target :: u(:,:,:)

      ! Local variables
      integer(c_int) :: N

      ! Get size of the array
      N = size(u, 1)

      call write_vtk_c(n, c_loc(u(1,1,1)))

   end subroutine write_vtk



end program poisson3d