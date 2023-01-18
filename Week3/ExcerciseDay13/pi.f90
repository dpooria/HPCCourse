         PROGRAM pi_sequential

         IMPLICIT NONE
         INTEGER, PARAMETER :: MK = kind(1.0D0)
         INTEGER :: circle=0, square=0, i, N=10e8
         REAL :: pi
         REAL, DIMENSION(2) :: rand
         INTEGER :: tw1, tw2
         REAL :: tc1, tc2

         CALL SYSTEM_CLOCK(tw1)
         CALL CPU_TIME(tc1)         

         DO i=1, N
         CALL RANDOM_NUMBER(rand)
         IF (NORM2(rand) .LE. 1.0_MK) then
            circle = circle + 1
         ELSE
            square = square +1
         ENDIF
         ENDDO
          pi = 4.0_mk * circle / (N + 0.0_MK)
         print *, 'pi =', pi

         CALL SYSTEM_CLOCK(tw2)
         CALL CPU_TIME(tc2)

         print*, 'CPU time is:', tc2-tc1
         print*, 'WALL time is:', tw2-tw1

         END PROGRAM pi_sequential



























