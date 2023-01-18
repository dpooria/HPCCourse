PROGRAM ping_pong
   USE MPI
   IMPLICIT NONE
   INTEGER, PARAMETER :: MK = kind(1.0D0)
   INTEGER :: p_rank, c_size, ierror, ilen, m_size, i, tag_ping, tag_pong
   REAL(MK), DIMENSION(1) :: msg=1.0_MK
   INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status
   REAL(MK) :: time

   m_size = 1
   tag_ping = 10
   tag_pong = 20

   CALL MPI_INIT(ierror)
   CALL MPI_COMM_RANK(MPI_COMM_WORLD, p_rank, ierror)
!     call MPI_GET_PROCESSOR_NAME(p_name, ilen, ierror)
   CALL MPI_COMM_SIZE(MPI_COMM_WORLD, c_size, ierror)


   DO i = 1, 10
      IF(mod(p_rank, 2) == 0) THEN
         CALL MPI_Send(msg, m_size, MPI_DOUBLE_PRECISION, p_rank + 1, tag_ping, MPI_COMM_WORLD, ierror)
         CALL MPI_Recv(msg, m_size, MPI_DOUBLE_PRECISION, p_rank + 1, tag_pong, MPI_COMM_WORLD, status, ierror)
         PRINT*, "pong"
      ELSE
         CALL MPI_Recv(msg, m_size, MPI_DOUBLE_PRECISION, p_rank - 1, tag_ping, MPI_COMM_WORLD,status,ierror)
         PRINT*, "ping"
         CALL MPI_Send(msg, m_size, MPI_DOUBLE_PRECISION, p_rank - 1, tag_pong, MPI_COMM_WORLD, ierror)
      ENDIF
   ENDDO
   call MPI_FINALIZE(ierror)

END PROGRAM ping_pong

