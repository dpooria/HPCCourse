MODULE m_Ex3a_swap
CONTAINS
   SUBROUTINE aswap(a1, a2)
      REAL, INTENT(INOUT), DIMENSION(:) :: a1, a2
      INTEGER :: s1, s2
      REAL, DIMENSION(size(a1)):: work
      s1 = size(a1)
      s2 = size(a2)
      IF (s1 .NE. s2) THEN
         PRINT '(A,I3,A,I3)', "subroutine_swap: ERROR! mismatch size", s1, "!=", s2
         RETURN
      END IF

      work = a1
      a1 = a2
      a2 = work
   END SUBROUTINE aswap

   SUBROUTINE pswap(a1, a2)
      REAL, INTENT(INOUT), DIMENSION(:) :: a1, a2
      INTEGER :: status, s1, s2
      REAL, DIMENSION(:), POINTER:: work
      s1 = size(a1)
      s2 = size(a2)
      IF (s1 .NE. s2) THEN
         PRINT '(A,I3,A,I3)', "subroutine_swap: ERROR! mismatch size", s1, "!=", s2
         RETURN
      ENDIF
      ALLOCATE(work(s1), stat=status)
      IF(status .NE. 0) THEN
         PRINT*, "subroutine_swap: Allocation Error"
         RETURN
      ENDIF
      work = a1
      a1 = a2
      a2 = work
      DEALLOCATE(work)
   END SUBROUTINE
   SUBROUTINE head_array(array, number_of_lines, arrname)
      REAL, DIMENSION(:), INTENT(IN) :: array
      INTEGER, OPTIONAL :: number_of_lines
      CHARACTER(LEN=*), OPTIONAL :: arrname
      INTEGER :: i, n
      LOGICAL :: is_pname
      IF (PRESENT(number_of_lines)) THEN
         n = number_of_lines
      ELSE
         n = 7
      ENDIF
      is_pname = PRESENT(arrname)
      DO i = 1, n
         IF (is_pname) THEN
            PRINT '(A,A,I1, A,F5.2)', arrname, '(', i, ')=',array(i)
         ELSE
            PRINT *, array(i)
         ENDIF
      ENDDO
   END SUBROUTINE head_array
END MODULE m_Ex3a_swap


