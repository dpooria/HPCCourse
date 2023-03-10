PROGRAM pi_mpi
   USE MPI
   IMPLICIT NONE
   INTEGER, PARAMETER :: MK = kind(1.0D0)
   INTEGER :: l_circle=0, g_circle=0, g_square=0, i, N
   REAL(MK) :: pi, pi_old, eps = 1e-8
   REAL(MK) :: tw1, tw2
   REAL(MK), DIMENSION(2) :: rand
   INTEGER, ALLOCATABLE, DIMENSION(:) :: seed
   INTEGER :: p_rank, c_size, ierror, m_size, req, seedsize, t
   LOGICAL:: converged = .FALSE.
   CALL MPI_INIT(ierror)
   CALL MPI_COMM_RANK(MPI_COMM_WORLD, p_rank, ierror)
   CALL MPI_COMM_SIZE(MPI_COMM_WORLD, c_size, ierror)
   l_circle = 0
   g_circle = 0
   N = 0
   CALL SYSTEM_CLOCK(t)
   CALL RANDOM_SEED(SIZE=seedsize)
   ALLOCATE(seed(seedsize))
   DO i=1, seedsize
      seed(i) = t + p_rank*123
   ENDDO

   CALL RANDOM_SEED(PUT=seed)

   tw1 = MPI_WTIME()
   i = 0
   DO
      i = i + 1
      CALL RANDOM_NUMBER(rand)
      IF ((rand(1)**2 + rand(2)**2) .LE. 1.0_MK) then
         l_circle = l_circle + 1
      ENDIF
      IF (mod(i, 1000) .EQ. 0) THEN
         CALL MPI_REDUCE(l_circle, g_circle, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierror)
         CALL MPI_REDUCE(i, N, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierror)
         IF (p_rank .EQ. 0) THEN
            pi_old = pi
            pi = 4.0_mk * g_circle / REAL(N, kind=MK)
            IF(abs(pi - pi_old) .LE. eps) THEN
               converged = .TRUE.
            ENDIF
         ENDIF
         CALL MPI_Bcast(converged, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierror)
         IF (converged) EXIT
      ENDIF
   ENDDO
   tw2 = MPI_WTIME()
   IF (p_rank .EQ. 0) THEN
      OPEN(10, file="pi_mpi_res.dat", action="write", position="append")
      WRITE(10, *) pi, c_size, tw2 - tw1, N
      CLOSE(10)
   ENDIF
   CALL MPI_FINALIZE(ierror)

END PROGRAM pi_mpi
