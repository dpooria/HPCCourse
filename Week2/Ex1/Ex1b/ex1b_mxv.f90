
PROGRAM mxv

   IMPLICIT NONE
   INTEGER, PARAMETER :: MKS = KIND(1E0)
   INTEGER, PARAMETER :: MKD = KIND(1D0)
   INTEGER, PARAMETER :: MK = MKD
   REAL(MK), ALLOCATABLE :: a(:, :), b(:), c(:), ref_val(:)
   REAL(MK) :: cpu_tik, cpu_tok
   INTEGER :: n, m, system_tik, system_tok, count_rate, n0(4), m0(3), i, j, u
   CHARACTER(LEN=128) :: argv = ''
   n = 10000
   m = 100000
   !alloc
   ALLOCATE(a(m, n), b(n))
   ALLOCATE(ref_val(m))
   !Initialize
   CALL RANDOM_NUMBER(a)
   CALL RANDOM_NUMBER(b)
   ! refrence value to ensure that the calculation is done correctly
   ref_val = MATMUL(a, b)

   !Run
   CALL CPU_TIME(cpu_tik)
   CALL SYSTEM_CLOCK(system_tik)
   CALL matrix_x_vector(c, a, b)
   CALL CPU_TIME(cpu_tok)
   CALL SYSTEM_CLOCK(system_tok, count_rate=count_rate)
   PRINT'(A,E11.6,A,E11.6,A,E11.6,A)', 'err = ', mse(ref_val, c), &
      '; Cpu Time: ', cpu_tok - cpu_tik, ' Sec; Execution Time: ', &
      REAL(system_tok - system_tik, kind=MK) / REAL(count_rate, kind=MK), ' Sec'
   DEALLOCATE(a, b, c)

   N0 = [500, 1000, 5000,10000]
   M0 = [500, 3000, 30000]
   CALL GET_COMMAND_ARGUMENT(number=1, value=argv)
   OPEN(newunit=u, file=TRIM(argv), status='replace')
   DO i = 1, SIZE(N0)
      DO j = 1, SIZE(M0)
         CALL main(N0(i), M0(j), u)
      END DO
   END DO
   CLOSE(u)
CONTAINS
   SUBROUTINE main(n, m, u)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n, m, u
      REAL(MK) :: t_e
      INTEGER :: system_tik, system_tok, count_rate
      !alloc
      ALLOCATE(a(m, n), b(n))
      ! ALLOCATE(ref_val(m))
      !Initialize
      CALL RANDOM_NUMBER(a)
      CALL RANDOM_NUMBER(b)
      ! refrence value to ensure that the calculation is done correctly
      ! ref_val = MATMUL(a, b)
      !Run
      CALL SYSTEM_CLOCK(system_tik)
      CALL matrix_x_vector(c, a, b)
      CALL SYSTEM_CLOCK(system_tok, count_rate=count_rate)
      t_e = REAL(system_tok - system_tik, kind=MK) / REAL(count_rate, kind=MK)
      WRITE(u, '(I10, F20.6, F20.6, F20.6)') m*n, log10(REAL(m*n, kind=MK)), t_e, 1.0_MK/(t_e + 0.001_MK)
      DEALLOCATE(a, b, c)
   END SUBROUTINE

   SUBROUTINE matrix_x_vector(c, a, b)
      IMPLICIT NONE
      REAL(MK), DIMENSION(:, :), INTENT(IN) :: a
      REAL(MK), DIMENSION(:), INTENT(IN) :: b
      REAL(MK), DIMENSION(:), INTENT(OUT), ALLOCATABLE :: c
      INTEGER :: m, n, i, j
      REAL(MK) :: s
      m = SIZE(a, 1)
      n = SIZE(a, 2)
      IF (n .NE. SIZE(b)) THEN
         PRINT*, "Error! Size mismatch!"
         RETURN
      ENDIF
      ALLOCATE(c(m))
      !$OMP parallel default(none) shared(a, b, c, m, n) private(i, j)
      !$OMP DO
      DO i = 1, m
         c(i) = 0.0_mk
      ENDDO
      !$OMP end DO

      DO j = 1, n
         !$OMP DO
         DO i = 1, m
            c(i) = c(i) + a(i, j) * b(j)
         ENDDO
         !$OMP end DO
      ENDDO
      !$OMP end parallel

   END SUBROUTINE matrix_x_vector


   FUNCTION mse(a1, a2) RESULT(err)
      IMPLICIT NONE
      REAL(MK) :: err
      REAL(MK), DIMENSION(:), INTENT(IN) :: a1, a2
      INTEGER :: i, n
      n = SIZE(a1, 1)
      IF (n .NE. SIZE(a2, 1)) THEN
         PRINT*, "Error! size mismatch in mse!"
         RETURN
      ENDIF

      err = 0.0_MK
      DO i = 1, n
         err = err + (a1(i) - a2(i)) ** 2
      END DO
      err = err / n
   END FUNCTIOn
END PROGRAM
