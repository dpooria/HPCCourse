
PROGRAM pi_sequential
   IMPLICIT NONE
   INTEGER, PARAMETER :: MK = KIND(1.0D0)
   INTEGER :: circle=0, i, N=INT(1E08)
   REAL(MK) :: pi
   REAL(MK), DIMENSION(2) :: rand
   INTEGER :: tw1, tw2, seedsize, rate, t
   INTEGER, DIMENSION(:), ALLOCATABLE :: seed

   CALL RANDOM_SEED(SIZE=seedsize)
   CALL SYSTEM_CLOCK(t)
   ALLOCATE(seed(seedsize))
   DO i=1, seedsize
      seed(i) = t + 123
   ENDDO
   CALL SYSTEM_CLOCK(tw1)
   DO i=1, N
      CALL RANDOM_NUMBER(rand)
      IF ((rand(1)**2 + rand(2)**2) .LE. 1.0_MK) then
         circle = circle + 1
      ENDIF
   ENDDO
   pi = 4.0_mk * circle / (N + 0.0_MK)
   
   CALL SYSTEM_CLOCK(tw2, count_rate=rate)
   OPEN(10, file="pi_sequential_res.dat", action="write")
   WRITE(10, *) (tw2 - tw1 + 0.0_MK)/rate ,pi, N
   CLOSE(10)
END PROGRAM pi_sequential

