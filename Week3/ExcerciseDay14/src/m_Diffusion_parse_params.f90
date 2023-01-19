MODULE m_Diffusion_parse_params
   USE m_Diffusion_precision, ONLY: MK
   USE m_Diffusion, ONLY: Nt, Nx, Ny, D, Dt, output
   IMPLICIT NONE
   character(len=26), parameter, private :: &
      upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ", &
      lower = "abcdefghijklmnopqrstuvwxyz"
CONTAINS
   SUBROUTINE read_input_params(fname)
      CHARACTER(LEN=*), OPTIONAL :: fname
      CHARACTER(LEN=128) :: file_name
      CHARACTER(LEN=512) :: cbuf
      INTEGER :: u, io, ind_eq
      IF (PRESENT(fname)) THEN
         file_name = TRIM(fname)
      ELSE
         file_name = "sample_input.txt"
      ENDIF
      OPEN(newunit=u, file=TRIM(file_name), action="read")
      DO
         READ(u, '(A)', iostat=io) cbuf
         IF (io > 0) THEN
            PRINT*, "Error! unknow error happened while reading the file", file_name, "!"
            EXIT
         ELSE IF((io < 0)) THEN
            EXIT
         ELSE
            IF (LEN_TRIM(cbuf) .EQ. 0) THEN
               CYCLE
            ENDIF
            ind_eq = INDEX(cbuf, "=")
            IF (ind_eq .EQ. 0) THEN
               CYCLE
            ENDIF
            CALL assign_variable(cbuf(1:ind_eq - 1), cbuf(ind_eq + 1:LEN_TRIM(cbuf)))
         ENDIF

      ENDDO
      CLOSE(u)
   END SUBROUTINE read_input_params

   SUBROUTINE assign_variable(var_name, value_expression)
      CHARACTER(LEN=*), INTENT(in) :: var_name, value_expression
      CHARACTER(LEN=2) :: var_name_trim
      var_name_trim = TRIM(var_name)
      CALL lower_case(var_name_trim)
      IF (var_name_trim .EQ. "nt") THEN
         READ(value_expression, *) Nt
      ELSE IF (var_name_trim .EQ. "nx") THEN
         READ(value_expression,*) Nx
      ELSE IF (var_name_trim .EQ. "ny") THEN
         READ(value_expression, *) Ny
      ELSE IF (TRIM(var_name_trim) .EQ. "d") THEN
         READ(value_expression, *) D
      ELSE IF (var_name_trim .EQ. "dt") THEN
         READ(value_expression, *) Dt
      ELSE IF (TRIM(var_name_trim) .EQ. "O") THEN
         READ(value_expression, *) output
      ENDIF
   END SUBROUTINE assign_variable

   ! From http://computer-programming-forum.com/49-fortran/1aca720144542594.htm
   subroutine lower_case( str )
      IMPLICIT NONE
      integer :: i, is
      character(len=*) ::  str
      !-------------------------------
      do is = 1, len_trim( str )
         i = index( upper, str(is:is) )
         if( i .gt. 0 ) str(is:is) = lower(i:i)
      enddo
   end subroutine lower_case
END MODULE
