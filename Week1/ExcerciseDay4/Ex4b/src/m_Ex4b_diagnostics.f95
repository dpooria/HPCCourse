MODULE m_Ex4b_diagnostics
   USE m_Ex4b_precision, ONLY:MK
   USE m_Ex4b, ONLY: temp_new, Dt
   IMPLICIT NONE
   LOGICAL, SAVE :: first_time=.TRUE.
   INTEGER :: u
CONTAINS
   SUBROUTINE diagnostics(time_step)
      INTEGER, INTENT(IN) :: time_step
      REAL(MK) :: min_t
      IF (first_time) THEN
         open(newunit=u, file='diag.dat', status='replace')
         first_time = .FALSE.
      ENDIF
      min_t = MINVAL(temp_new)
      WRITE(u, '(F12.4, F12.4)') time_step * Dt, min_t
      WRITE (u, '(10A)')
   END SUBROUTINE
   SUBROUTINE close_diagnostics_file()
      CLOSE(u)
   END SUBROUTINE
END MODULE
