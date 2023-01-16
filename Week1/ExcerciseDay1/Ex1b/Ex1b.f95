PROGRAM Ex1b
   IMPLICIT NONE
   INTEGER, PARAMETER :: MKS = KIND(1.0E0)
   INTEGER, PARAMETER :: MKD = KIND(1.0D0)
   INTEGER, PARAMETER :: MK = MKS
   INTEGER :: Nx, Ny, Nt !mesh and time
   INTEGER :: i, j, k, u !dummy
   REAL(MK), PARAMETER :: D = 1.0_MK, T0 = 1.0_MK ! Diffusion constant and the boundary value
   REAL(MK), PARAMETER :: Lx=1.0_MK, Ly=1.0_MK, T_f = 0.125_MK !Box and final time
   REAL(MK) :: Dx, Dy, Dt, FL !Deltas and the Fouriere limit
   REAL(MK), DIMENSION(:, :), ALLOCATABLE :: temp, temp_old ! Temperature

   ! 10  PRINT *, 'Nx = '
   ! READ *, Nx
   ! PRINT *, 'Ny = '
   ! READ *, Ny
   Nx = 21
   Ny = 21

   ALLOCATE(temp(Nx, Ny))
   ALLOCATE(temp_old(Nx, Ny))


   Dx = Lx / (Nx - 1.0_MK)
   Dy = Ly / (Ny - 1.0_MK)

   fl = min(Dx, Dy)**2 / (4.0 * D)
   WRITE(*,*) 'Fourier Limit is:', FL

   ! PRINT *, 'Nt = '
   ! READ *, Nt
   Nt = 200
   ! Dt = fl
   Dt = T_f / Nt
   ! !Check if the Fourier limit is satisfied
   ! IF (Dt .Gt. fl) THEN
   !    PRINT*, "Error Dt is greater than the Fourier limit! DT = ", Dt
   !    PRINT *, "Try again:"
   !    WRITE(*, '(10A)')
   !    GOTO 10
   ! ENDIF

   PRINT *, "Dx = ", Dx, ";Dy = ", Dy, ";Dt = ", Dt


   ! Initialize
   ! temp(:, :) = 0.0_MK
   ! temp_old(:, :) = 0.0_MK

   DO j = 2, Ny - 1
      DO i = 2, Nx - 1
         temp(i, j) = 0.0_MK
         temp_old(i, j) = 0.0_MK
      ENDDO
   ENDDO
   !Assign the Drichlet boundary conditions
   ! y
   DO i = 1, Nx
      temp(i, 1) = T0
      temp_old(i, 1) = T0
      temp(i, Ny) = T0
      temp_old(i, Ny) = T0
   ENDDO
   !x
   Do j = 1, Ny
      temp(1, j) = T0
      temp_old(1, j) = T0
      temp(Nx, j) = T0
      temp_old(Nx, j) = T0
   ENDDO

   ! Euler

   DO k = 1, Nt
      ! Update the old temperature
      DO j = 2, Ny - 1
         DO i = 2, Nx - 1
            temp_old(i, j) = temp(i, j)
         ENDDO
      ENDDO
      ! temp_old = temp
      ! Laplacian
      DO j = 2, Ny - 1
         DO i = 2, Nx - 1
            temp(i, j) = Dt * D * &
               ((temp_old(i + 1, j) - 2.0_MK * temp_old(i, j) + temp_old(i - 1, j)) / (Dx**2) &
               + (temp_old(i, j+1) - 2.0_MK * temp_old(i, j) + temp_old(i, j - 1)) / (Dy**2)) &
               + temp_old(i, j)
         ENDDO
      ENDDO
   ENDDO
   OPEN(newunit=u, file='diff.dat', status='replace')
   DO j = 1, Ny
      DO i = 1, Nx
         WRITE(u, '(3E12.4)') REAL(i-1) * Dx, REAL(j-1)*Dy, temp(i, j)
      ENDDO
      WRITE (u, '(10A)')
   ENDDO
   CLOSE(u)
   DEALLOCATE(temp)
   DEALLOCATE(temp_old)

END PROGRAM Ex1b

