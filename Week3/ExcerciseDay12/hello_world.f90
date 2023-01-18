PROGRAM hello_world
      USE MPI
      
      IMPLICIT NONE
      
      INTEGER :: p_rank, c_size, ierror, ilen, i
      CHARACTER(len=128) :: p_name

      CALL MPI_INIT(ierror)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, p_rank, ierror)
      CALL MPI_GET_PROCESSOR_NAME(p_name, ilen, ierror)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, c_size, ierror)


      IF(p_rank==0) PRINT*, 'Hello World from rank ', p_rank

<<<<<<< HEAD
      DO i = 0, c_size-1
=======
      DO i = 0,10
>>>>>>> 35a44b785bc7e1106c78358a1d8263bd9eb7aeb5
      IF(i == p_rank) THEN
            WRITE(*,*) 'Process ', p_name,' is rank ', p_rank, 'of ', c_size
      END IF
      call MPI_BARRIER( MPI_COMM_WORLD, ierror)
      END DO
      call MPI_FINALIZE(ierror)
END PROGRAM HELLO_WORLD


