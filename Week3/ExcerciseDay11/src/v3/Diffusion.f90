
PROGRAM Diffusion
   USE m_Diffusion_precision, ONLY: MK, MKS
   USE m_Diffusion, ONLY: temp_new, temp_old, Nx, Ny, Nt, Dt, Dx, Dy, FL, D, vb
   USE m_Diffusion_init, ONLY: init_data
   USE m_Diffusion_write, ONLY: write_to_file
   USE m_Diffusion_copy, ONLY: copy_elemental!, copy_new_to_old
   USE m_Diffusion_alloc, ONLY: alloc
   USE m_Diffusion_diagnostics, ONLY: diagnostics, close_diagnostics_file
   USE m_Diffusion_parse_params, ONLY: read_input_params

   IMPLICIT NONE
   INTEGER :: i, j, k, info
   REAL(MK), PARAMETER :: T_boundary = 1.0_MK ! Diffusion coefficient and the boundary value
   REAL(MK), PARAMETER :: Lx=1.0_MK, Ly=1.0_MK, T_f = 0.125_MK !Box and final time
   REAL(MKS) :: cpu_tik, cpu_tok
   INTEGER:: wall_tik, wall_tok, count_rate, u
   CHARACTER(LEN=10) :: time
   CHARACTER(LEN=8) :: date
   CHARACTER(LEN=128) :: output
   CALL GET_COMMAND_ARGUMENT(number=1, value=output)
   IF(LEN_TRIM(output) .eq. 0) THEN
      CALL  get_command_argument(number=0, value=output)
      output = TRIM(output) // "_output.txt"
   ENDIF
   OPEN(newunit=u, file=output, status="replace")

   ! 10  PRINT *, 'Nx = '
   ! READ *, Nx
   ! PRINT *, 'Ny = '
   ! READ *, Ny
   CALL DATE_AND_TIME(time=time, date=date)
   IF(vb) THEN
      PRINT *, "Program Diffusion started at ", date, "-", time
   ENDIF
   WRITE(u, *) "Program Diffusion started at ", date, "-", time

   CALL read_input_params()
   IF(vb) THEN
      PRINT *, "Running the program with these input parameters:"
      PRINT *, "Nt = ", Nt, "Nx=", Nx, "Ny = ", Ny, "D=", D, "Dt=", Dt
   ENDIF
   WRITE(u, *) "Running the program with these input parameters:"
   WRITE(u, *) "Nt = ", Nt, "Nx=", Nx, "Ny = ", Ny, "D=", D, "Dt=", Dt

   ! Nx = 21
   ! Ny = 21
   !Allocate the temperature fields
   CALL alloc(temp_new, Nx, Ny, info)
   IF (info .NE. 0) THEN
      PRINT*, "ERROR!, There was an error with memory allocation!"
      WRITE(u, *) "ERROR!, There was an error with memory allocation!"
   ENDIF
   CALL alloc(temp_old, Nx, Ny, info)
   IF (info .NE. 0) THEN
      PRINT*, "ERROR!, There was an error with memory allocation!"
      WRITE(u, *) "ERROR!, There was an error with memory allocation!"
   ENDIF
   ! !!!!!!!!!!!! Example of allocating an allocated array:
   ! CALL alloc(temp_old, Nx, Ny, info)
   ! IF (info .NE. 0) THEN
   !    PRINT*, "ERROR!, There was an error with memory allocation!"
   ! ENDIF
   !!!!!!!!!!!!!!!
   Dx = Lx / (Nx - 1.0_MK)
   Dy = Ly / (Ny - 1.0_MK)

   fl = min(Dx, Dy)**2 / (4.0 * D)
   IF (vb) THEN
      WRITE(*,*) 'Fourier limit is:', FL
   ENDIF
   WRITE(u,*) 'Fourier limit is:', FL

   ! PRINT *, 'Nt = '
   ! READ *, Nt
   ! Nt = 200
   ! Dt = fl
   ! Dt = T_f / Nt
   !Check if the Fourier limit is satisfied
   !  IF (Dt .Gt. fl) THEN
   !     PRINT*, "Error Dt is greater than the Fourier limit! DT = ", Dt
   !     PRINT *, "Try again:"
   !     WRITE(*, '(10A)')
   !     GOTO 10
   !  ENDIF
   IF(vb) THEN
      PRINT *, "Dx = ", Dx, ";Dy = ", Dy, ";Dt = ", Dt
   ENDIF
   WRITE(u, *) "Dx = ", Dx, ";Dy = ", Dy, ";Dt = ", Dt

   ! Initialize
   ! temp(:, :) = 0.0_MK
   ! temp_old(:, :) = 0.0_MK
   !Assign the Drichlet boundary conditions
   ! y
   ! x
   CALL init_data(T_boundary=T_boundary)
   ! Euler
   CALL cpu_time(cpu_tik)
   CALL system_clock(wall_tik)

   DO k = 1, Nt
      ! Update the old temperature
      ! CALL copy_new_to_old()
      CALL copy_elemental(temp_new, temp_old)
      ! temp_old = temp
      ! Laplacian
      DO j = 2, Ny - 1
         DO i = 2, Nx - 1
            ! CALL euler_step(i, j)
            temp_new(i, j) = Dt * D * &
               ((temp_old(i + 1, j) - 2.0_MK * temp_old(i, j) + temp_old(i - 1, j)) / (Dx**2) &
               + (temp_old(i, j+1) - 2.0_MK * temp_old(i, j) + temp_old(i, j - 1)) / (Dy**2)) &
               + temp_old(i, j)
         ENDDO
      ENDDO
      !! Save the data
      !! Skip IO for timing
      ! CALL write_to_file(time_step=k)
      ! IF (MOD(k,10) .Eq. 0) THEN
      !    CALL diagnostics(k)
      ! ENDIF
   ENDDO
   CALL cpu_time(cpu_tok)
   CALL system_clock(wall_tok, count_rate=count_rate)
   IF(vb) THEN
      PRINT '(A,F10.4,A)', "CPU time: ", cpu_tok - cpu_tik, ' Seconds'
      PRINT '(A,F10.4,A)', "Wall clock: ", REAL(wall_tok) / count_rate - REAL(wall_tik) / count_rate, ' Seconds'
   ENDIF
   WRITE(u, '(A,F10.4,A)') "CPU time: ", cpu_tok - cpu_tik, ' Seconds'
   WRITE(u, '(A,F10.4,A)') "Wall clock: ", REAL(wall_tok) / count_rate - REAL(wall_tik) / count_rate, ' Seconds'
   CALL close_diagnostics_file()
   DEALLOCATE(temp_new)
   DEALLOCATE(temp_old)
   CALL DATE_AND_TIME(time=time, date=date)
   IF (vb) THEN
      PRINT *, "Program Diffusion ended at ", date, "-" ,time
   ENDIF
   WRITE(u, *) "Program Diffusion ended at ", date, "-" ,time
   CLOSE(u)
END PROGRAM Diffusion

