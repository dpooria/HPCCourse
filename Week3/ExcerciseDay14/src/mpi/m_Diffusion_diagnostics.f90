MODULE m_Diffusion_diagnostics
   USE m_Diffusion_precision, ONLY:MK
   USE m_Diffusion, ONLY: temp_new, Dt
   IMPLICIT NONE
   LOGICAL, SAVE :: first_time=.TRUE.
   INTEGER :: u
   PRIVATE first_time, u
   PUBLIC diagnostics, close_diagnostics_file
CONTAINS
   SUBROUTINE diagnostics(time_step)
      INTEGER, INTENT(IN) :: time_step
      REAL(MK) :: min_t
      IF (first_time) THEN
         u = 911
         open(u, file='diag.dat', status='replace')
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
