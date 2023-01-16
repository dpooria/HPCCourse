MODULE m_Ex3b_copy
   USE m_Ex3b
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
END MODULE m_Ex3b_copy
