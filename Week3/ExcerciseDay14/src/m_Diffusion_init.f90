MODULE m_Diffusion_init
   USE m_Diffusion_precision, ONLY: MK
   USE m_Diffusion, ONLY: temp_new, temp_old
   USE m_Diffusion_bc, ONLY: apply_boundary_conditions
   USE m_Diffusion_MPI, ONLY: Nx_local, Ny_local, c_size, p_rank
   IMPLICIT NONE
CONTAINS
   SUBROUTINE init_data(T_boundary)
      REAL(MK), OPTIONAL :: T_boundary
      REAL(MK) :: T0 = 0.0_MK
      INTEGER :: i, j
      IF (.NOT. PRESENT(T_boundary)) THEN
         T0 = 1.0_MK
      ENDIF
      IF ((p_rank .EQ. 0) .OR. (p_rank .EQ. (c_size - 1))) THEN
         DO j = 1, Ny_local
            DO i = 1, Nx_loca + 1
               temp_new(i, j) = T0
               temp_old(i, j) = T0
            ENDDO
         ENDDO
      ELSE
      ENDIF
      call apply_boundary_conditions(T_boundary)
   END SUBROUTINE init_data

END MODULE m_Diffusion_init
