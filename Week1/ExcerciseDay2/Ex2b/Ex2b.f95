PROGRAM Ex2b
   USE m_Ex2b_precision
   USE m_Ex2b
   IMPLICIT NONE
   INTEGER :: i, j, k !dummy
   REAL(MK), PARAMETER :: D = 1.0_MK, T_boundary = 1.0_MK ! Diffusion coefficient and the boundary value
   REAL(MK), PARAMETER :: Lx=1.0_MK, Ly=1.0_MK, T_f = 0.125_MK !Box and final time

   ! 10  PRINT *, 'Nx = '
   ! READ *, Nx
   ! PRINT *, 'Ny = '
   ! READ *, Ny
   Nx = 21
   Ny = 21

   ALLOCATE(temp_new(Nx, Ny))
   ALLOCATE(temp_old(Nx, Ny))


   Dx = Lx / (Nx - 1.0_MK)
   Dy = Ly / (Ny - 1.0_MK)

   fl = min(Dx, Dy)**2 / (4.0 * D)
   WRITE(*,*) 'Fourier limit is:', FL

   ! PRINT *, 'Nt = '
   ! READ *, Nt
   Nt = 200
   ! Dt = fl
   Dt = T_f / Nt
   !Check if the Fourier limit is satisfied
   !  IF (Dt .Gt. fl) THEN
   !     PRINT*, "Error Dt is greater than the Fourier limit! DT = ", Dt
   !     PRINT *, "Try again:"
   !     WRITE(*, '(10A)')
   !     GOTO 10
   !  ENDIF

   PRINT *, "Dx = ", Dx, ";Dy = ", Dy, ";Dt = ", Dt


   ! Initialize
   ! temp(:, :) = 0.0_MK
   ! temp_old(:, :) = 0.0_MK
   !Assign the Drichlet boundary conditions
   ! y
   !x
   call init_data(T_boundary=T_boundary)
   ! Euler
   DO k = 1, Nt
      ! Update the old temperature
      call copy_new_to_old()
      ! temp_old = temp
      ! Laplacian
      DO j = 2, Ny - 1
         DO i = 2, Nx - 1
            ! call euler_step(i, j)
            temp_new(i, j) = Dt * D * &
               ((temp_old(i + 1, j) - 2.0_MK * temp_old(i, j) + temp_old(i - 1, j)) / (Dx**2) &
               + (temp_old(i, j+1) - 2.0_MK * temp_old(i, j) + temp_old(i, j - 1)) / (Dy**2)) &
               + temp_old(i, j)
         ENDDO
      ENDDO
      ! Save the data
      CALL write_to_file(time_step=k)
   ENDDO

   DEALLOCATE(temp_new)
   DEALLOCATE(temp_old)

END PROGRAM Ex2b

