PROGRAM main
   IMPLICIT NONE
   REAL, DIMENSION(:), POINTER :: p
   REAL, DIMENSION(:), ALLOCATABLE, TARGET :: t
   INTEGER :: info,Nt
   Nt = 100000 ! 10000 ! does it crash for all Nt values ?
   ALLOCATE(t(Nt))
   CALL RANDOM_NUMBER(t) ! assign some value to the target
   p => t ! let p point to t
   PRINT*,' p(3) = ',p(3) ! Print some value of p
   DEALLOCATE(t,STAT=info) ! Deallocate t
   ALLOCATE(t(1000))
   p => t ! is this required ?
   CALL RANDOM_NUMBER(t)
   ! NULLIFY(p) !
   IF (ASSOCIATED(p)) THEN ! Is p associated if t is deallocated ?
      PRINT*,'p is associated'
      PRINT*,' p(3) = ',p(3) ! Does this always crash ?
   ENDIF
END PROGRAM main
