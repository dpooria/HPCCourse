
MODULE m_Ex2b
   USE m_Ex2b_precision
   IMPLICIT NONE
   INTEGER :: Nx, Ny, Nt !mesh and time
   REAL(MK) :: Dx, Dy, Dt, FL !Deltas and the Fouriere limit
   REAL(MK), DIMENSION(:, :), ALLOCATABLE :: temp_new, temp_old ! Temperature

CONTAINS
   SUBROUTINE init_data(T_boundary)
      REAL(MK), OPTIONAL :: T_boundary
      REAL(MK) :: T0 = 0.0_MK
      INTEGER :: i, j
      IF (.NOT. PRESENT(T_boundary)) THEN
         T_boundary = 1.0_MK
      ENDIF
      DO j = 1, Ny
         DO i = 1, Nx
            temp_new(i, j) = T0
            temp_old(i, j) = T0
         ENDDO
      ENDDO
      call apply_boundary_conditions(T_boundary)
   END SUBROUTINE init_data

   SUBROUTINE copy_new_to_old()
      INTEGER :: i, j
      DO j = 1, Ny
         DO i = 1, Nx
            temp_old(i, j) = temp_new(i, j)
         ENDDO
      ENDDO

   END SUBROUTINE copy_new_to_old

   SUBROUTINE apply_boundary_conditions(T_boundary)
      REAL(MK), INTENT(IN) :: T_boundary
      INTEGER :: i, j

      !   Nx = size(temp_old, dim=1)
      !   Ny = size(temp_old, dim=2)
      DO i = 1, Nx
         temp_new(i, 1) = T_boundary
         temp_old(i, 1) = T_boundary
         temp_new(i, Ny) = T_boundary
         temp_old(i, Ny) = T_boundary
      ENDDO
      Do j = 1, Ny
         temp_new(1, j) = T_boundary
         temp_old(1, j) = T_boundary
         temp_new(Nx, j) = T_boundary
         temp_old(Nx, j) = T_boundary
      ENDDO

   END SUBROUTINE apply_boundary_conditions

   SUBROUTINE write_to_file(time_step)
      INTEGER, OPTIONAL, INTENT(IN) :: time_step
      CHARACTER(LEN=128) :: fname
      INTEGER :: i, j, u, fname_len

      IF (PRESENT(time_step)) THEN
         WRITE(fname, '(A,I6.6,A)') 'diff.', time_step, '.dat'
        
      ELSE
         fname = "diff.dat"
      ENDIF
      !   Nx = size(temp_new, 1)
      !   Ny = size(temp_new, 2)
      fname_len = LEN_TRIM(fname)
      OPEN(newunit=u, file=fname(1:fname_len), status='replace')
      DO j = 1, Ny
         DO i = 1, Nx
            WRITE(u, '(3E12.4)') REAL(i-1) * Dx, REAL(j-1)*Dy, temp_new(i, j)
         ENDDO
         WRITE (u, '(10A)')
      ENDDO
      CLOSE(u)
   END SUBROUTINE write_to_file

!    SUBROUTINE euler_step(i, j, D)
!       INTEGER, INTENT(IN) :: i, j
!       REAL(MK), OPTIONAL :: D

!       IF (.NOT. PRESENT(D)) THEN
!          D = 1.0_MK
!       ENDIF
!       temp_new(i, j) = Dt * D * &
!          ((temp_old(i + 1, j) - 2.0_MK * temp_old(i, j) + temp_old(i - 1, j)) / (Dx**2) &
!          + (temp_old(i, j+1) - 2.0_MK * temp_old(i, j) + temp_old(i, j - 1)) / (Dy**2)) &
!          + temp_old(i, j)
!    END SUBROUTINE euler_step

END MODULE m_EX2b
