MODULE m_Diffusion_write
   USE m_Diffusion_precision, ONLY:MK, MKS, MKD
   USE m_Diffusion, ONLY:temp_new, temp_old, Nx, Ny, Dx, Dy
   IMPLICIT NONE

   INTERFACE output_array_to_file
      MODULE PROCEDURE write_to_file_single, write_to_file_double
   END INTERFACE

CONTAINS

   SUBROUTINE write_to_file(array, time_step, file_name, write_old_field)
      REAL(MK), DIMENSION(:, :), TARGET, OPTIONAL, INTENT(IN) :: array
      INTEGER, OPTIONAL, INTENT(IN) :: time_step
      CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: file_name
      LOGICAL, OPTIONAL, INTENT(IN) :: write_old_field
      CHARACTER(LEN=512) :: fname
      INTEGER :: i, j, u, fname_len
      REAL(MK), POINTER, DIMENSION(:, :) :: arr_ptr
      IF (PRESENT(time_step)) THEN
         IF(PRESENT(file_name)) THEN
            WRITE(fname, '(A,A,I6.6,A)') file_name, '.', time_step, '.dat'
         ELSE
            WRITE(fname, '(A,I6.6,A)') 'outputs/diff.', time_step, '.dat'
         ENDIF
      ELSE
         IF (PRESENT(file_name)) THEN
            fname_len = LEN_TRIM(file_name)
            fname(1:fname_len) = file_name(1:fname_len)
         ELSE
            fname = 'outputs/diff.dat'
         ENDIF
      ENDIF

      arr_ptr => temp_new
      IF(PRESENT(array)) THEN
         arr_ptr => array
      ELSE
         IF (PRESENT(write_old_field)) THEN
            IF(write_old_field) THEN
               arr_ptr => temp_old
            ENDIF
         ENDIF
      ENDIF
      fname_len = LEN_TRIM(fname)
      OPEN(newunit=u, file=fname(1:fname_len), status='replace')

      DO j = 1, Ny
         DO i = 1, Nx
            WRITE(u, '(F20.4,F20.4,F20.4)') REAL(i-1) * Dx, REAL(j-1)*Dy, temp_new(i, j)
         ENDDO
         WRITE (u, '(10A)')
      ENDDO
      CLOSE(u)
   END SUBROUTINE write_to_file

   SUBROUTINE write_to_file_single(array, file_name)
      REAL(MKS), DIMENSION(:, :), TARGET, INTENT(IN) :: array
      CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: file_name
      CHARACTER(LEN=512) :: fname
      INTEGER :: i, j, u, fname_len
      REAL(MKS), POINTER, DIMENSION(:, :) :: arr_ptr
      IF (PRESENT(file_name)) THEN
         fname_len = LEN_TRIM(file_name)
         fname(1:fname_len) = file_name(1:fname_len)
      ELSE
         fname = 'outputs/diff.dat'
      ENDIF

      arr_ptr => array
      fname_len = LEN_TRIM(fname)
      OPEN(newunit=u, file=fname(1:fname_len), status='replace')

      DO j = 1, Ny
         DO i = 1, Nx
            WRITE(u, '(F20.4,F20.4,F20.4)') REAL(i-1) * Dx, REAL(j-1)*Dy, temp_new(i, j)
         ENDDO
         WRITE (u, '(10A)')
      ENDDO
      CLOSE(u)
   END SUBROUTINE write_to_file_single
   SUBROUTINE write_to_file_double(array, file_name)
      REAL(MKD), DIMENSION(:, :), TARGET, INTENT(IN) :: array
      CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: file_name
      CHARACTER(LEN=512) :: fname
      INTEGER :: i, j, u, fname_len
      REAL(MKD), POINTER, DIMENSION(:, :) :: arr_ptr
      IF (PRESENT(file_name)) THEN
         ! fname_len = LEN_TRIM(file_name)
         ! fname(1:fname_len) = file_name(1:fname_len)
         fname = file_name
      ELSE
         fname = 'diff.dat'
      ENDIF

      arr_ptr => array
      fname_len = LEN_TRIM(fname)
      OPEN(newunit=u, file=fname(1:fname_len), status='replace')

      DO j = 1, Ny
         DO i = 1, Nx
            WRITE(u, '(F20.4,F20.4,F20.4)') REAL(i-1) * Dx, REAL(j-1)*Dy, temp_new(i, j)
         ENDDO
         WRITE (u, '(10A)')
      ENDDO
      CLOSE(u)
   END SUBROUTINE write_to_file_double


END MODULE m_Diffusion_write
