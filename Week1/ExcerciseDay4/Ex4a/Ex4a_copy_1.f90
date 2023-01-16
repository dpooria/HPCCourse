PROGRAM main
   IMPLICIT NONE
   REAL, DIMENSION(:), POINTER :: p
   REAL, DIMENSION(:), ALLOCATABLE, TARGET :: t
   INTEGER :: info,Nt
   Nt = 10000
   ALLOCATE(t(Nt))
   PRINT '(A, I2)', "SIZEOF(t(1)) = ", SIZEOF(t(1))
   ! Question: does it crash for all Nt values ?
   ! Answer: Each REAL number occupies 4 bytes of memory so depending on
   ! the available memory of the system it could crash.
   ! For example for a system with 8GB of memory -> if (Nt >= 2 * 1E9)(And even less than that)
   CALL RANDOM_NUMBER(t) ! assign some value to the target
   p => t ! let p point to t
   PRINT*,' p(3) = ',p(3) ! Print some value of p
   DEALLOCATE(t,STAT=info) ! Deallocate t
   ! ALLOCATE(t(1000))
   ! p => t ! is this required ?
   ! Apparently it's not! I was surprised to see that p is still (correctly) pointing to the new t
   ! CALL RANDOM_NUMBER(t)
   ! CALL NULLIFY(p) !
   IF (ASSOCIATED(p)) THEN ! Is p associated if t is deallocated ? Yes it is still associated even though t is deallocated. So we have to call nullify after deallocation
      PRINT*,'p is associated'
      PRINT*,' p(3) = ',p(3)! Does this always crash ? No it never crashes but it has a false behaviour as p is pointing to god know what :)
   ENDIF
END PROGRAM main
