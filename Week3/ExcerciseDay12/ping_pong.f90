PROGRAM ping_pong
   USE mpi
   IMPLICIT NONE
   INTEGER, PARAMETER :: MK = kind(1.0D0)
   INTEGER :: p_rank, c_size, ierror, ilen, m_size, i, tag_ping, tag_pong
!    CHARACTER(len=128) :: p_name
   REAL(MK), DIMENSION(1) :: msg=1.0_MK
   INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status
!    Type(MPI_Request) :: request
      real(MK) :: time

   m_size = 1
   tag_ping = 10
   tag_pong = 20

   call MPI_INIT(ierror)
   call MPI_COMM_RANK(MPI_COMM_WORLD, p_rank, ierror)
!     call MPI_GET_PROCESSOR_NAME(p_name, ilen, ierror)
   call MPI_COMM_SIZE(MPI_COMM_WORLD, c_size, ierror)


   do i = 1, 10
      IF(mod(p_rank, 2) == 0) THEN
         call MPI_SEND(msg, m_size, MPI_DOUBLE_PRECISION, 1, tag_ping, MPI_COMM_WORLD, ierror)
         call MPI_RECV(msg, m_size, MPI_DOUBLE_PRECISION, 1, tag_pong, MPI_COMM_WORLD, status, ierror)
         PRINT*, 'pong', time
      !    call MPI_BARRIER(MPI_COMM_WORLD, ierror)
      ELSE
         call MPI_RECV(msg, m_size, MPI_DOUBLE_PRECISION, 0, tag_ping, MPI_COMM_WORLD,status,ierror)
         call cpu_time(time)
         call MPI_SEND(msg, m_size, MPI_DOUBLE_PRECISION, 0, tag_pong, MPI_COMM_WORLD, ierror)
      !    call MPI_BARRIER(MPI_COMM_WORLD, ierror)
      ENDIF
   enddo
   call MPI_FINALIZE(ierror)
END PROGRAM ping_pong

