PROGRAM Diffusion
   USE MPI
   USE m_Diffusion_MPI, ONLY: p_rank, c_size, &
      status, Nx_local, Ny_local, tag, T_final
   USE m_Diffusion_precision, ONLY: MK, MKS
   USE m_Diffusion, ONLY: temp_new, temp_old, Nx, Ny, Nt, Dt, Dx, Dy, FL, D, vb
   USE m_Diffusion_init, ONLY: init_data
   USE m_Diffusion_write, ONLY: write_to_file
   USE m_Diffusion_copy, ONLY: copy_elemental!, copy_new_to_old
   USE m_Diffusion_alloc, ONLY: alloc
   USE m_Diffusion_diagnostics, ONLY: diagnostics, close_diagnostics_file
   USE m_Diffusion_parse_params, ONLY: read_input_params

   IMPLICIT NONE
   INTEGER :: i, j, k, info, u
   REAL(MK), PARAMETER :: T_boundary = 1.0_MK ! Diffusion coefficient and the boundary value
   REAL(MK), PARAMETER :: Lx=1.0_MK, Ly=1.0_MK, T_f = 0.125_MK !Box and final time
   REAL(MK):: wall_tik, wall_tok
   CHARACTER(LEN=10) :: time
   CHARACTER(LEN=8) :: date
   REAL(MK) :: Dx_coeff, Dy_coeff

   ! start parallization
   CALL MPI_INIT(ierror)
   CALL MPI_COMM_RANK(MPI_COMM_WORLD, p_rank, ierror)
   CALL MPI_COMM_SIZE(MPI_COMM_WORLD, c_size, ierror)
   vb = (p_rank .EQ. 0)
   ! CALL GET_COMMAND_ARGUMENT(number=1, value=output)
   ! IF(LEN_TRIM(output) .eq. 0) THEN
   !    CALL  get_command_argument(number=0, value=output)
   !    output = TRIM(output) // "_output.txt"
   ! ENDIF
   IF (vb) THEN
      u = 110
      OPEN(u, file=output, status="replace")
      CALL DATE_AND_TIME(time=time, date=date)
      WRITE(u, *) "Program Diffusion started at ", date, "-", time
      WRITE(u, *) "Running the program with ", c_size, " processors in parallel"
      CALL read_input_params()
      WRITE(u, *) "Running the program with these input parameters:"
      WRITE(u, *) "Nt = ", Nt, "Nx=", Nx, "Ny = ", Ny, "D=", D, "Dt=", Dt
   ENDIF
   CALL MPI_Bcast(u, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
   CALL MPI_Bcast(Nt, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
   CALL MPI_Bcast(Nx, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
   CALL MPI_Bcast(Ny, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
   CALL MPI_Bcast(D, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierror)
   CALL MPI_Bcast(Dt, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierror)
   Nproc = c_size
   Nx_local = Nx / Nproc
   Ny_local = Ny
   IF (p_rank .EQ. 0) THEN
      CALL alloc(temp_new, Nx_local + 1, Ny_local, info)
      CALL alloc(temp_old, Nx_local + 1, Ny_local, info)
      CALL alloc(T_final, Nx, Ny)
   ELSEIF (p_rank .EQ. c_size - 1) THEN
      Nx_local = Nx - Nx_local * (Nproc - 1)
      CALL alloc(temp_new, Nx_local, Ny_local, info)
      CALL alloc(temp_old, Nx_local, Ny_local, info)
   ELSE
      CALL alloc(temp_new, Nx_local + 2, Ny_local, info)
      CALL alloc(temp_old, Nx_local + 2, Ny_local, info)
   ENDIF

   Dx = Lx / (Nx - 1.0_MK)
   Dy = Ly / (Ny - 1.0_MK)

   fl = min(Dx, Dy)**2 / (4.0 * D)
   IF (vb) THEN
      WRITE(u,*) 'Fourier limit is:', FL
   ENDIF

   IF(vb) THEN
      WRITE(u, *) "Dx = ", Dx, ";Dy = ", Dy, ";Dt = ", Dt
   ENDIF
   Dx_coeff = Dt * D / Dx**2
   Dy_coeff = Dt * D / Dy**2

   ! Initialize
   !Assign the Drichlet boundary conditions
   ! y
   ! x
   CALL init_data(T_boundary=T_boundary)
   ! Euler
   wall_tik = MPI_WTime()
   DO k = 1, Nt
      CALL copy_elemental(temp_new, temp_old)
      ! ghost layers are boundaries
      IF (p_rank .EQ. 0) THEN
         DO j = 2, Ny_local - 1
            DO i = 2, Nx_local
               temp_new(i, j) = Dx_coeff * (temp_old(i + 1, j)- 2.0_MK * temp_old(i, j) + temp_old(i - 1, j)) &
                  + Dy_coeff * (temp_old(i, j+1) - 2.0_MK * temp_old(i, j) + temp_old(i, j - 1)) &
                  + temp_old(i, j)
            ENDDO
         ENDDO
      ELSE IF(p_rank .EQ. c_size - 1) THEN
         DO j = 2, Ny_local - 1
            DO i = 2, Nx_local - 1
               temp_new(i, j) = Dx_coeff * (temp_old(i + 1, j)- 2.0_MK * temp_old(i, j) + temp_old(i - 1, j)) &
                  + Dy_coeff * (temp_old(i, j+1) - 2.0_MK * temp_old(i, j) + temp_old(i, j - 1)) &
                  + temp_old(i, j)
            ENDDO
         ENDDO
      ELSE
         DO j = 2, Ny_local - 1
            DO i = 2, Nx_local + 1
               temp_new(i, j) = Dx_coeff * (temp_old(i + 1, j)- 2.0_MK * temp_old(i, j) + temp_old(i - 1, j)) &
                  + Dy_coeff * (temp_old(i, j+1) - 2.0_MK * temp_old(i, j) + temp_old(i, j - 1)) &
                  + temp_old(i, j)
            ENDDO
         ENDDO
      ENDIF
      ! update ghosts
      IF(p_rank .EQ. 0) THEN
         CALL MPI_SendRecv(temp_new(Nx_local, :), Ny_local, MPI_DOUBLE_PRECISION,&
            p_rank + 1, tag, temp_new(Nx_local + 1, :), Ny_local, MPI_DOUBLE_PRECISION, p_rank + 1, tag, &
            MPI_COMM_WORLD, status, ierror)
         !RECEIVE
      ELSEIF (p_rank .EQ. c_size - 1) THEN
         CALL MPI_SendRecv(temp_new(2, :), Ny_local, MPI_DOUBLE_PRECISION, p_rank - 1, tag, &
            temp_new(1, :), Ny_local, MPI_DOUBLE_PRECISION, p_rank - 1, tag,&
            MPI_COMM_WORLD, status, ierror)
      ELSE
         !left proc
         CALL MPI_SendRecv(temp_new(2, :), Ny_local, MPI_DOUBLE_PRECISION,&
            p_rank - 1, tag, temp_new(1, :), Ny_local, MPI_DOUBLE_PRECISION, p_rank - 1, tag, &
            MPI_COMM_WORLD, status, ierror)
         !right proc
         CALL MPI_SendRecv(temp_new(Nx_local + 1, :), Ny_local, MPI_DOUBLE_PRECISION,&
            p_rank + 1, tag, temp_new(Nx_local + 2, :), Ny_local, MPI_DOUBLE_PRECISION, p_rank + 1, tag, &
            MPI_COMM_WORLD, status, ierror)
      ENDIF
   ENDDO
ENDDO
wall_tok = MPI_WTime()
IF(vb) THEN
   WRITE(u, '(A,F10.4,A)') "Wall clock: ", wall_tok - wall_tik, ' Seconds'
   ! CALL close_diagnostics_file()
   ! PRINT '(A,F10.4,A)', "CPU time: ", cpu_tok - cpu_tik, ' Seconds'
   ! PRINT '(A,F10.4,A)', "Wall clock: ", REAL(wall_tok) / count_rate - REAL(wall_tik) / count_rate, ' Seconds'
ENDIF
 
!! Gather all   
IF(p_rank .EQ. 0) THEN
   !Recv
   
ELSEIF(p_rank .EQ. c_size - 1) THEN
   DO i = 2, Nx_local
      CALL MPI_Send(temp_new(i, :), Ny_local, MPI_DOUBLE_PRECISION, 0, tag, MPI_COMM_WORLD, ierror)
   ENDDO
ELSE
   !Send
   DO i = 2, Nx_local + 1
      CALL MPI_Send(temp_new(i, :), Ny_local, MPI_DOUBLE_PRECISION, 0, tag, MPI_COMM_WORLD, ierror)
   ENDDO
ENDIF


DEALLOCATE(temp_new)
DEALLOCATE(temp_old)
 !TODO deallocate buffers
IF (vb) THEN
   CALL DATE_AND_TIME(time=time, date=date)
   WRITE(u, *) "Program Diffusion ended at ", date, "-" ,time
   ! CALL 
   CLOSE(u)
ENDIF

 ! End parallization

CALL MPI_FINALIZE(ierror)

END PROGRAM Diffusion

