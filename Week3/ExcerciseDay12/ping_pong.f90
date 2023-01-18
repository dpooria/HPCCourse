
program mpi_ping_pong
   use mpi
   implicit none
   INTEGER, PARAMETER :: MK = KIND(1.0D0)
   INTEGER :: ierror, my_rank, num_procs, tag, dest, source, i,j, message_size
   REAL(MK) :: transfer_time, message, start_time, end_time, total_time, &
      min_bandwidth, max_bandwidth
   REAL(MK), DIMENSION(2**18) :: message, received_message
   INTEGER, DIMENSION(18) :: N

   call MPI_Init(ierror)
   call MPI_Comm_Rank(MPI_COMM_WORLD, my_rank, ierror)
   call MPI_Comm_Size(MPI_COMM_WORLD, num_procs, ierror)
   !Initialize
   message = 1.0_MK
   tag = 1
   N(1) = 1
   DO j = 2, 18
      N(j) = 2**j
   ENDDO
   IF (my_rank .EQ. 0) THEN
      OPEN(10, file="output.dat", action="write")
   ENDIF

   DO j = 1, 18
      message_size = N(j)
      total_time = 0.0_MK
      min_bandwidth = 1.0D20
      max_bandwidth = 0.0
      ! Repeat the process 10 times
      DO i = 1, 10
         IF (my_rank .EQ. 0) THEN
            !destination and source of the rank 0 is 1
            dest = 1
            source = 1
            ! just in case :)
            CALL MPI_BARRIER(MPI_COMM_WORLD, ierror)
            start_time = MPI_WTIME()
            CALL MPI_SEND(message, message_size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierror)
            CALL MPI_RECV(received_message, message_size, MPI_DOUBLE_PRECISION, source, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            end_time = MPI_WTIME()
            
            transfer_time = end_time - start_time
            total_time = total_time + transfer_time
            min_bandwidth = min(min_bandwidth, message_size/transfer_time)
            max_bandwidth = max(max_bandwidth, message_size/transfer_time)
            ! PRINT *, 'Process 0: Received message ', message(1), ' from Process 1'
         ELSEIF (my_rank .EQ. 1) THEN
            !destination and the source of rank 1 is 0
            dest = 0
            source = 0
            ! just in case :)
            CALL MPI_BARRIER(MPI_COMM_WORLD, ierror)
            CALL MPI_RECV(received_message, message_size, MPI_DOUBLE_PRECISION, source, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            CALL MPI_SEND(message, message_size, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierror)
         ENDIF

      ENDDO
      IF (my_rank .EQ. 0) THEN
         WRITE(10, *) message_size, total_time, N*10.0_MK/total_time, min_bandwidth, max_bandwidth
         WRITE(10, "(10A)")
      END IF
   ENDDO
   IF(my_rank .EQ. 0) THEN
      CLOSE(10)
   ENDIF
   CALL MPI_FINALIZE(ierror)
end program mpi_ping_pong

