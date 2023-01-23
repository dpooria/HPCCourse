
MODULE m_Diffusion_MPI
    USE MPI
    USE m_Diffusion_precision, ONLY:MK, MKS, MKD

    IMPLICIT NONE
    INTEGER :: p_rank, c_size, ierror, Nproc
    INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status
    INTEGER :: Nx_local, Ny_local, MPI_MK
    INTEGER, PARAMETER:: tag = 123
    REAL(MK), DIMENSION(:, :), ALLOCATABLE :: send_buffer 
 
 END MODULE m_Diffusion_MPI
 