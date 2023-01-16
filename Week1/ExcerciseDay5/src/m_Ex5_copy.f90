MODULE m_Ex5_copy
   USE m_Ex5_precision, ONLY: MK
   USE m_Ex5, ONLY: temp_old, temp_new, Nx, Ny
   IMPLICIT NONE
CONTAINS
   SUBROUTINE copy_new_to_old()
      INTEGER :: i, j
      DO j = 1, Ny
         DO i = 1, Nx
            temp_old(i, j) = temp_new(i, j)
         ENDDO
      ENDDO

   END SUBROUTINE copy_new_to_old
   ELEMENTAL SUBROUTINE copy_elemental(a1, a2)
      REAL(MK), INTENT(IN) :: a1
      REAL(MK), INTENT(INOUT) :: a2
      a2 = a1
   END SUBROUTINE
END MODULE m_Ex5_copy
