MODULE m_Ex3b_alloc
   USE m_Ex3b_precision
   USE m_Ex3b_write
   IMPLICIT NONE
   INTERFACE alloc
      MODULE PROCEDURE allocs, allocd
   END INTERFACE alloc

CONTAINS
   SUBROUTINE allocs(a, N1, N2, info)
      REAL(MKS), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: a
      INTEGER, INTENT(OUT) :: info
      INTEGER, INTENT(IN) :: N1, N2
      CHARACTER(LEN=128) :: fname = ''
      CHARACTER(LEN=10) :: time = ''
      IF (ALLOCATED(a)) THEN
         PRINT*, 'Warning!: Allocating an already allocated array; making a backup!'
         CALL DATE_AND_TIME(time=time)
         WRITE(fname, "(A,A,A)") "alloc_data_bkp/backup_from_m_Ex3b_alloc_", TRIM(time), ".dat"
         CALL write_to_file_single(array=a, file_name=fname)
         DEALLOCATE(a)
      ENDIF
      ALLOCATE(a(N1, N2), stat=info)
      
   END SUBROUTINE allocs

   SUBROUTINE allocd(a, N1, N2, info)
      REAL(MKD), DIMENSION(:,:), ALLOCATABLE, TARGET, INTENT(INOUT) :: a
      INTEGER, INTENT(OUT) :: info
      INTEGER, INTENT(IN) :: N1, N2
      CHARACTER(LEN=128) :: fname = ''
      CHARACTER(LEN=10) :: time = ''
      IF (ALLOCATED(a)) THEN
         PRINT*, 'Warning: Allocating an already allocated array; making a backup!'
         CALL DATE_AND_TIME(time=time)
         WRITE(fname, "(A,A,A)") "alloc_data_bkp/backup_from_m_Ex3b_alloc_", TRIM(time), ".dat"
         CALL write_to_file_double(array=a, file_name=fname)
         DEALLOCATE(a)
      ENDIF
      ALLOCATE(a(N1, N2), stat=info)

   END SUBROUTINE allocd
END MODULE m_Ex3b_alloc
