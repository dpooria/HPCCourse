
PROGRAM main
   USE m_Ex3a_swap

   IMPLICIT NONE

   INTEGER, PARAMETER :: N = 8000000
   INTEGER :: i
   !This fails on my system with a stack size of 8Mb
   ! INTEGER, PARAMETER :: N = 80000000
   REAL, DIMENSION(N) :: array1, array2
   !!!Explicit Interface Fails for Modules!!!
   !    ! Interface for functions that swap the arryas using a pointer
!    INTERFACE
!       SUBROUTINE pswap(a1, a2)
!          REAL, DIMENSION(:), INTENT(INOUT) :: a1, a2
!       END SUBROUTINE pswap
!    END INTERFACE

! !    ! Interface for functions that swap the arryas using an array
!    INTERFACE
!       SUBROUTINE aswap(a1, a2)
!          REAL, DIMENSION(:), INTENT(INOUT) :: a1, a2
!       END SUBROUTINE aswap
!    END INTERFACE
   array1 = 0.0
   array2 = 1.0
   DO i = 1, 7
      PRINT '(A,I1,A,F3.1)', "array1(", i, ")=", array1(i)
   ENDDO

   PRINT*, "Before swap (first 7 elements are printed)"
   CALL head_array(array1, arrname='array1')
   CALL head_array(array2, arrname='array2')
   CALL pswap(array1, array2)
   PRINT *, "After swap with pointer"
   CALL head_array(array1, arrname='array1')
   CALL head_array(array2, arrname='array2')
   CALL aswap(array1, array2)
   PRINT *, "After swap with automatic arrays"
   CALL head_array(array1, arrname='array1')
   CALL head_array(array2, arrname='array2')

   
END PROGRAM main

