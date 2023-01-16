PROGRAM main
   USE m_Ex2a
   IMPLICIT NONE 
   REAL, DIMENSION(:), POINTER :: my_data
   INTEGER :: n, status
   CHARACTER(LEN=128) :: string_tmp

   n = 12
   CALL sub(my_data, n, status)
   PRINT *, 'status = ', status, ASSOCIATED(my_data)

   CALL init(my_data, n, status)


   DO i = 1, n
      WRITE(string_tmp, '(A,I2.1,A)') 'my_data(', i, ') = '
      PRINT '(A,F8.2)', TRIM(string_tmp), my_data(i)
   ENDDO

END PROGRAM main
