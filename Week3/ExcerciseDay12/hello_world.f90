PROGRAM hello_world
      USE MPI
      
      IMPLICIT NONE
      
      INTEGER :: p_rank, c_size, ierror, ilen, i
      CHARACTER(len=128) :: p_name

      CALL MPI_Init(ierror)
      CALL MPI_Comm_Rank(MPI_COMM_WORLD, p_rank, ierror)
      CALL MPI_Get_Processor_Name(p_name, ilen, ierror)
      CALL MPI_Comm_Size(MPI_COMM_WORLD, c_size, ierror)

      IF(p_rank==0) PRINT*, 'Hello World from rank ', p_rank

      DO i = 0, c_size-1
            IF(i == p_rank) THEN
                  WRITE(*,*) 'Process ', p_name,' is rank ', p_rank, 'of ', c_size
            END IF
                  CALL MPI_Barrier( MPI_COMM_WORLD, ierror)
      ENDDO
      CALL MPI_Finalize(ierror)
END PROGRAM hello_world


