MODULE m_Diffusion_alloc
   USE m_Diffusion_precision, ONLY: MKS, MKD
   USE m_Diffusion_write, ONLY: output_array_to_file
   IMPLICIT NONE
   INTERFACE alloc
      MODULE PROCEDURE allocs, allocd
   END INTERFACE alloc

CONTAINS
   SUBROUTINE allocs(a, N1, N2, info)
      REAL(MKS), DIMENSION(:,:), POINTER, INTENT(INOUT) :: a
      INTEGER, INTENT(OUT) :: info
      INTEGER, INTENT(IN) :: N1, N2
      CHARACTER(LEN=128) :: fname = ''
      CHARACTER(LEN=10) :: time = ''
      IF (ASSOCIATED(a)) THEN
         PRINT*, 'Warning!: Allocating an already allocated array; making a backup!'
         ! CALL DATE_AND_TIME(date=time)
         ! WRITE(fname, "(A,A,A)") "alloc_data_bkp/bkp", TRIM(time), ".dat"
         ! CALL output_array_to_file(a, file_name=fname)
         DEALLOCATE(a)
      ENDIF
      ALLOCATE(a(N1, N2), stat=info)

   END SUBROUTINE allocs

   SUBROUTINE allocd(a, N1, N2, info)
      REAL(MKD), DIMENSION(:,:), POINTER, INTENT(INOUT) :: a
      INTEGER, INTENT(OUT) :: info
      INTEGER, INTENT(IN) :: N1, N2
      CHARACTER(LEN=128) :: fname = ''
      CHARACTER(LEN=10) :: time = ''
      IF (ASSOCIATED(a)) THEN
         PRINT*, 'Warning: Allocating an already allocated array; making a backup!'
         ! CALL DATE_AND_TIME(time=time)
         ! WRITE(fname, "(A,A,A)") "alloc_data_bkp/backup_from_m_Diffusion_alloc_", TRIM(time), ".dat"
         ! CALL output_array_to_file(a, file_name=fname)
         DEALLOCATE(a)
      ENDIF
      ALLOCATE(a(N1, N2), stat=info)

   END SUBROUTINE allocd
END MODULE m_Diffusion_alloc
