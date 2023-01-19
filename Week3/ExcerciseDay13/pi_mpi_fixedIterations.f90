PROGRAM pi_mpi
   USE MPI
   IMPLICIT NONE
   INTEGER, PARAMETER :: MK = kind(1.0D0)
   INTEGER :: l_circle=0, g_circle=0, i, N=INT(1E08), M, n_pp
   REAL(MK) :: pi
   REAL(MK), DIMENSION(2) :: rand
   INTEGER, ALLOCATABLE, DIMENSION(:) :: seed
   REAL(MK) :: tw1, tw2
   INTEGER :: p_rank, c_size, ierror, m_size

   CALL MPI_INIT(ierror)
   CALL MPI_COMM_RANK(MPI_COMM_WORLD, p_rank, ierror)
   CALL MPI_COMM_SIZE(MPI_COMM_WORLD, c_size, ierror)

   n_pp = N/c_size
   M = n_pp * c_size

   CALL SYSTEM_CLOCK(t)
   CALL RANDOM_SEED(SIZE=seedsize)
   ALLOCATE(seed(seedsize))
   DO i=1, seedsize
      seed(i) = t + p_rank*123
   ENDDO

   tw1 = MPI_WTIME()

   DO i=1, n_pp
      CALL RANDOM_NUMBER(rand)
      IF ((rand(1)**2 + rand(2)**2) .LE. 1.0_MK) then
         l_circle = l_circle + 1
      ENDIF
   ENDDO

   CALL MPI_REDUCE(g_circle, l_circle, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierror)
   tw2 = MPI_WTIME()

   IF (p_rank .EQ. 0) then
      do i = 1, N - M
         CALL RANDOM_NUMBER(rand)
         IF ((rand(1)**2 + rand(2)**2) .LE. 1.0_MK) then
            g_circle = g_circle + 1
         ENDIF
      enddo
      pi = 4.0_mk * g_circle / (N + 0.0_MK)
      OPEN(10, file="pi_mpi_fixedIterations_res.dat", action="write", position="append")
      WRITE(10, *) pi, c_size, tw2 - tw1, N
      CLOSE(10)
   ENDIF

   CALL MPI_FINALIZE(ierror)

END PROGRAM pi_mpi

