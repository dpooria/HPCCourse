
PROGRAM calculate_pi
   ! USE OMP_LIB
   IMPLICIT NONE
   INTEGER, PARAMETER :: MKS = KIND(1E0)
   INTEGER, PARAMETER :: MKD = KIND(1D0)
   INTEGER, PARAMETER :: MK = MKD
   REAL(MK) :: cpu_tik, cpu_tok, pi, e_time
   INTEGER :: system_tik, system_tok, count_rate, u, i, N0(6)
   CHARACTER(LEN=128) :: argv

   CALL CPU_TIME(cpu_tik)
   CALL SYSTEM_CLOCK(system_tik)

   CALL calc_pi(pi, N0=INT(1E9))

   CALL CPU_TIME(cpu_tok)
   CALL SYSTEM_CLOCK(system_tok, count_rate=count_rate)

   PRINT'(A,F9.6,A,E11.6,A,E11.6,A)', 'PI = ', pi, '; Cpu Time: ', cpu_tok - cpu_tik, ' Sec; Execution Time: ' &
      , REAL(system_tok - system_tik, kind=MK) / REAL(count_rate, kind=MK), ' Sec'

   ! Test out different values of N
   CALL get_command_argument(number=0, value=argv)
   WRITE(argv, '(A,A)') TRIM(argv), '_output.txt'
   OPEN(newunit=u, file=TRIM(argv), status='replace')
   N0 = [5000000, 10000000, 50000000, 100000000, 500000000, 1000000000]
   DO i = 1, 6
      CALL SYSTEM_CLOCK(system_tik)
      CALL calc_pi(pi, N0=N0(i))
      CALL SYSTEM_CLOCK(system_tok, count_rate=count_rate)
      e_time = REAL(system_tok - system_tik, kind=MK) / REAL(count_rate, kind=MK)
      WRITE(u, *) LOG10(REAL(N0(i), kind=MK)), e_time, 1.0_MK / (e_time + 1e-4)
   ENDDO
   CLOSE(u)

CONTAINS
   SUBROUTINE calc_pi(pi_, N0)
      IMPLICIT NONE
      REAL(MK), INTENT(OUT) :: pi_
      INTEGER, OPTIONAL, INTENT(IN) :: N0
      INTEGER :: N, i
      REAL(MK) :: N_d
      REAL(MK) :: s = 0.0_MK, loc_s = 0.0_MK
      IF (PRESENT(N0)) THEN
         N = N0
      ELSE
         N = 10000
      ENDIF
      N_d = REAL(N, kind=MK)
      !$OMP parallel default(none) private(loc_s,i) &
      !$OMP& shared(N, N_d, s)
      loc_s = 0.0_MK
      !$OMP do
      DO i = 1, N
         loc_s = loc_s + 4.0_MK / (1.0_MK + ((i - 0.5_MK)/N_d) ** 2)
      ENDDO
      !$OMP end do
      !$OMP critical
      s = s + loc_s
      !$OMP end critical
      !$OMP end parallel
      pi_ = s / N_d
   END SUBROUTINE calc_pi

END PROGRAM calculate_pi
