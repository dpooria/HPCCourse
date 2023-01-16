PROGRAM test
   IMPLICIT NONE
   INTEGER :: i,j,k,n
   PRINT*, 'Enter j n'
   READ(*,*) j,n
   DO i=1,n
    IF (i.EQ.j) EXIT
    !i = i + 10 ! does this compile ?
   ENDDO
   k = i
   PRINT*,'k = ', k 
END PROGRAM test
