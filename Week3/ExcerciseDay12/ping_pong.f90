      PROGRAM ping_pong
      USE MPI
      IMPLICIT NONE
!,      INTEGER(8) :: MK = kind(1.0D0)
      INTEGER :: p_rank, c_size, ierror, ilen, m_size 
      CHARACTER(len=10) :: p_name
      REAL(8), PARAMETER :: msg=1, tag=1
      INTEGER, DIMENSION(MPI_STATUS_SIZE) :: stat
       
      m_size=1

      call MPI_INIT(ierror)
      call MPI_COMM_RANK(MPI_COMM_WORLD, p_rank, ierror)
!     call MPI_GET_PROCESSOR_NAME(p_name, ilen, ierror)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, c_size, ierror)



      IF(p_rank == 0) THEN
       call MPI_SEND(msg, m_size, MPI_DOUBLE_PRECISION, 1, 1, MPI_COMM_WORLD, ierror)
       call MPI_IRECV(msg, m_size, MPI_DOUBLE_PRECISION, 0, 1, MPI_COMM_WORLD,stat,ierror)
       PRINT*, 'ping'
       ELSEIF(p_rank == 1) THEN
       call MPI_IRECV(msg, m_size, MPI_DOUBLE_PRECISION, 0, 1, MPI_COMM_WORLD,stat,ierror)
       call MPI_SEND(msg, m_size, MPI_DOUBLE_PRECISION, 1, 1, MPI_COMM_WORLD, ierror)
       PRINT*, 'pong'
      ENDIF
  
       
      END PROGRAM ping_pong
 
