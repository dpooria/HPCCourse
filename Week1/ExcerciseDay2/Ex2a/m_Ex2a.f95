MODULE m_Ex2a
    IMPLICIT NONE
    INTEGER :: i
contains

   SUBROUTINE sub(data, n, info)
      REAL, DIMENSION(:), POINTER :: data
      INTEGER :: n, info

      ALLOCATE(data(n), stat=info)
   END SUBROUTINE sub

   SUBROUTINE init(data, n, info)
      REAL, DIMENSION(*) :: data
      INTEGER :: n, info

      DO i = 1, n
         data(i) = -i
      ENDDO

   END SUBROUTINE init


END MODULE m_Ex2a
