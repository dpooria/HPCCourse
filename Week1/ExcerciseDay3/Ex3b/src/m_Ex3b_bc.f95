MODULE m_Ex3b_bc
   USE m_Ex3b_precision
   USE m_Ex3b
   IMPLICIT NONE

CONTAINS
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

END MODULE m_Ex3b_bc
