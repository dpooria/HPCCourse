
MODULE m_Diffusion
   USE m_Diffusion_precision, ONLY:MK
   IMPLICIT NONE
   INTEGER :: Nx=21, Ny=21, Nt=200 !mesh and time
   REAL(MK) :: Dx=0.001_MK, Dy=0.001_MK, Dt=0.002_MK, FL, D=1.0_MK !Deltas and the Fouriere limit
   REAL(MK), DIMENSION(:, :), ALLOCATABLE, TARGET :: temp_new, temp_old ! Temperature
   LOGICAL :: vb = .TRUE. !Indicating the verbosity of the outputs
   CHARACTER(LEN=128) :: output

! CONTAINS

!  SUBROUTINE euler_step(i, j, D)
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

END MODULE m_Diffusion
