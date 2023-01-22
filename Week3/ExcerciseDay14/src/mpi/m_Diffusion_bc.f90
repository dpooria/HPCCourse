MODULE m_Diffusion_bc
   USE m_Diffusion_precision, ONLY: MK
   USE m_Diffusion, ONLY: temp_new, temp_old
   USE m_Diffusion_MPI, ONLY: p_rank, c_size, Nx_local, Ny_local
   IMPLICIT NONE

CONTAINS
   SUBROUTINE apply_boundary_conditions(T_boundary)
      REAL(MK), INTENT(IN) :: T_boundary
      INTEGER :: i, j
      !! y bc
      IF ((p_rank .EQ. 0) .OR. (p_rank .EQ. c_size - 1)) THEN
         DO i = 1, Nx_local + 1
            temp_new(i, 1) = T_boundary
            temp_old(i, 1) = T_boundary
            temp_new(i, Ny_local) = T_boundary
            temp_old(i, Ny_local) = T_boundary
         ENDDO
      ELSE
         DO i = 1, Nx_local + 2
            temp_new(i, 1) = T_boundary
            temp_old(i, 1) = T_boundary
            temp_new(i, Ny_local) = T_boundary
            temp_old(i, Ny_local) = T_boundary
         ENDDO
      ENDIF
      !! x bc
      IF (p_rank .EQ. 0) THEN
         Do j = 1, Ny_local
            temp_new(1, j) = T_boundary
            temp_old(1, j) = T_boundary
         ENDDO
      ELSEIF (p_rank .EQ. c_size - 1) THEN
         Do j = 1, Ny_local
            temp_new(Nx_local + 1, j) = T_boundary
            temp_old(Nx_local + 1, j) = T_boundary
         ENDDO
      ENDIF


   END SUBROUTINE apply_boundary_conditions

END MODULE m_Diffusion_bc
