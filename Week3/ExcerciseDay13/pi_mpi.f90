      PROGRAM pi_mpi
         USE MPI
         IMPLICIT NONE
         INTEGER, PARAMETER :: MK = kind(1.0D0)
         INTEGER :: l_circle=0, l_square=0, g_circle=0, g_square=0, i, N=INT(1E8), M, n_pp
         REAL(MK) :: pi
         REAL(MK), DIMENSION(2) :: rand
         INTEGER, ALLOCATABLE, DIMENSION(:) :: seed
         INTEGER :: tw1, tw2
         INTEGER :: p_rank, c_size, ierror, m_size

         CALL MPI_INIT(ierror)
         CALL MPI_COMM_RANK(MPI_COMM_WORLD, p_rank, ierror)
         CALL MPI_COMM_SIZE(MPI_COMM_WORLD, c_size, ierror)

         n_pp = N/c_size
         M = n_pp * c_size

         CALL RANDOM_SEED(SIZE=seedsize)
         ALLOCATE(seed(seedsize))
         DO i=1, seedsize
            seed(i) = 1238239 + p_rank*8365
         ENDDO

         CALL RANDOM_SEED(PUT=seed)

         tw1 = MPI_WTIME()

         DO i=1, n_pp
            CALL RANDOM_NUMBER(rand)
            IF (NORM2(rand) .LE. 1.0_MK) then
               l_circle = l_circle + 1
            ELSE
               l_square = l_square +1
            ENDIF
         ENDDO

         CALL MPI_REDUCE(l_circle, g_circle, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierror)
         tw2 = MPI_WTIME()

         IF (p_rank .EQ. 0) then
            pi = 4.0_mk * g_circle / (M + 0.0_MK)
            PRINT *, 'pi =', pi
            PRINT*, 'WALL time is:', tw2-tw1
         ENDIF

         CALL MPI_FINALIZE(ierror)

      END PROGRAM pi_mpi



























