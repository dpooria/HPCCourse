
MODULE m_Diffusion_MPI
    USE m_Diffusion_precision, ONLY:MK
    IMPLICIT NONE
    INTEGER :: p_rank, c_size, ierror, Nproc
    INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status
    INTEGER :: Nx_local, Ny_local
    INTEGER, PARAMETER:: tag = 123
    REAL(MK), ALLOCATABLE, DIMENSION(:) :: left_buffer, right_buffer
 
 END MODULE m_Diffusion_MPI
 