MODULE m_Diffusion_copy
   USE m_Diffusion_precision, ONLY: MK
   USE m_Diffusion, ONLY: temp_old, temp_new
   ! USE m_Diffusion_MPI, ONLY: Nx_local, Ny_local, p_rank, c_size
   IMPLICIT NONE
CONTAINS
   ! SUBROUTINE copy_new_to_old()
   !    INTEGER :: i, j
   !    DO j = 1, Ny
   !       DO i = 1, Nx
   !          temp_old(i, j) = temp_new(i, j)
   !       ENDDO
   !    ENDDO

   ! END SUBROUTINE copy_new_to_old
   ELEMENTAL SUBROUTINE copy_elemental(a1, a2)
      REAL(MK), INTENT(IN) :: a1
      REAL(MK), INTENT(INOUT) :: a2
      a2 = a1
   END SUBROUTINE
END MODULE m_Diffusion_copy
