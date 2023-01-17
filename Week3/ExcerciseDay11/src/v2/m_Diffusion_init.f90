MODULE m_Diffusion_init
   USE m_Diffusion_precision, ONLY: MK
   USE m_Diffusion
   USE m_Diffusion_bc, ONLY: apply_boundary_conditions
   IMPLICIT NONE
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

END MODULE m_Diffusion_init
