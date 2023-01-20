
MODULE m_Diffusion_MPI
    USE MPI
    USE m_Diffusion_precision, ONLY:MK, MKS, MKD

    IMPLICIT NONE
    INTEGER :: p_rank, c_size, ierror, Nproc
    INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status
    INTEGER :: Nx_local, Ny_local
    INTEGER, PARAMETER:: tag = 123
 
 END MODULE m_Diffusion_MPI
 