PROGRAM Diffusion
   USE MPI
   USE m_Diffusion_MPI, ONLY: p_rank, c_size, &
      status, Nx_local, Ny_local, tag, ierror, Nproc, send_buffer
   USE m_Diffusion_precision, ONLY: MK, MKS
   USE m_Diffusion, ONLY: temp_new, temp_old, Nx, Ny, Nt, Dt, Dx, Dy, FL, D, vb, output
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
   CHARACTER(LEN=1024) :: char_buffer
   REAL(MK) :: Dx_coeff, Dy_coeff, x, y

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
      CALL read_input_params()
      u = 110
      char_buffer = "Diffusion_MPI_details_"//output
      OPEN(u, file=TRIM(char_buffer), status="replace")
      CALL DATE_AND_TIME(time=time, date=date)
      WRITE(u, *) "Program Diffusion started at ", date, "-", time
      WRITE(u, *) "Running the program with ", c_size, " processors in parallel"
      WRITE(u, *) "Running the program with these input parameters:"
      WRITE(u, *) "Nt = ", Nt, "Nx=", Nx, "Ny = ", Ny, "D=", D, "Dt=", Dt
   ENDIF
   CALL MPI_Bcast(u, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
   CALL MPI_Bcast(Nt, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
   CALL MPI_Bcast(Nx, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
   CALL MPI_Bcast(Ny, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
   CALL MPI_Bcast(D, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierror)
   CALL MPI_Bcast(Dt, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierror)
   CALL MPI_Bcast(output, len_trim(output), MPI_CHAR, 0, MPI_COMM_WORLD, ierror)
   Nproc = c_size
   Nx_local = Nx / Nproc
   Ny_local = Ny
   IF (p_rank .EQ. 0) THEN
      CALL alloc(temp_new, Nx_local + 1, Ny_local, info)
      CALL alloc(temp_old, Nx_local + 1, Ny_local, info)
      ! CALL alloc(T_final, Nx, Ny)
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
         CALL MPI_SendRecv(temp_new(Nx_local, 2:Ny_local-1), Ny_local-2, MPI_DOUBLE_PRECISION,&
            p_rank + 1, tag, temp_new(Nx_local + 1, 2:Ny_local), Ny_local-2,&
             MPI_DOUBLE_PRECISION, p_rank + 1, tag, &
            MPI_COMM_WORLD, status, ierror)
         !RECEIVE
      ELSEIF (p_rank .EQ. c_size - 1) THEN
         CALL MPI_SendRecv(temp_new(2, 2:Ny_local - 1), Ny_local - 2, &
            MPI_DOUBLE_PRECISION, p_rank - 1, tag, &
            temp_new(1, 2:Ny_local - 1), Ny_local - 2,&
            MPI_DOUBLE_PRECISION, p_rank - 1, tag,&
            MPI_COMM_WORLD, status, ierror)
      ELSE
         !left proc
         CALL MPI_SendRecv(temp_new(2, 2:Ny_local - 1), Ny_local - 2, MPI_DOUBLE_PRECISION,&
            p_rank - 1, tag, temp_new(1, 2:Ny_local - 1), Ny_local - 2,&
            MPI_DOUBLE_PRECISION, p_rank - 1, tag, &
            MPI_COMM_WORLD, status, ierror)
         !right proc
         CALL MPI_SendRecv(temp_new(Nx_local + 1, 2:Ny_local - 1), &
            Ny_local - 2, MPI_DOUBLE_PRECISION,&
            p_rank + 1, tag, temp_new(Nx_local + 2, 2:Ny_local),&
             Ny_local - 2, MPI_DOUBLE_PRECISION, p_rank + 1, tag, &
            MPI_COMM_WORLD, status, ierror)
      ENDIF
   ENDDO
   
   ! !! Gather all
   IF (p_rank .EQ. 0) THEN
      OPEN(777, file=ouptut, status="replace")
      DO j = 1, Ny_local
         y = REAL(j - 1, KIND=MK) * Dy
         DO i = 1, Nx_local + 1
            x = REAL((i - 1), KIND=MK) * Dx
            WRITE(777, '(3F12.4)') x, y, temp_new(i, j)
         ENDDO
         WRITE(777, '(10A)')
      ENDDO
      ALLOCATE(send_buffer(Nx_local, Ny_local))
      DO k = 1, c_size - 2
         CALL MPI_Recv(
         send_buffer,&
            Nx_local * Ny_local, MPI_DOUBLE_PRECISION, k, tag, &
            MPI_COMM_WORLD, status, ierror)
         DO j = 1, Ny_local
            y = REAL(j - 1, KIND=MK) * Dy
            DO i = 1, Nx_local + 1
               x = REAL((i - 1) + k * Nx_local, KIND=MK) * Dx
               WRITE(777, '(3F12.4)') x, y, send_buffer(i, j)
            ENDDO
            WRITE(777, '(10A)')
         ENDDO
      ENDDO
  
      DEALLOCATE(send_buffer)

      ALLOCATE(send_buffer(Nx - Nx_local * (Nproc - 1) + 1, Ny_local))
      CALL MPI_Recv(send_buffer,&
         (Nx - Nx_local * (Nproc - 1) + 1) * Ny_local, MPI_DOUBLE_PRECISION, &
         c_size - 1, tag, &
         MPI_COMM_WORLD, status, ierror)
      DO j = 1, Ny_local
         y = REAL(j - 1, KIND=MK) * Dy
         DO i = 1, Nx_local + 1
            x = REAL((i - 1) + k * Nx_local, KIND=MK) * Dx
            WRITE(777, '(3F12.4)') x, y, send_buffer(i, j)
         ENDDO
         WRITE(777, '(10A)')
      ENDDO
      DEALLOCATE(send_buffer)
      CLOSE(777)
   ELSE
      CALL MPI_SEND(temp_new(2:Nx_local + 1, Ny_local), &
         (Nx_local * Ny_local), MPI_DOUBLE_PRECISION,&
         0, tag, MPI_COMM_WORLD, ierror)
   ENDIF
   wall_tok = MPI_WTime()
   IF(vb) THEN
      WRITE(u, '(A,F10.4,A)') "Wall clock: ", wall_tok - wall_tik, ' Seconds'
      CLOSE(u)
      ! CALL close_diagnostics_file()
      ! PRINT '(A,F10.4,A)', "CPU time: ", cpu_tok - cpu_tik, ' Seconds'
      ! PRINT '(A,F10.4,A)', "Wall clock: ", REAL(wall_tok) / count_rate - REAL(wall_tik) / count_rate, ' Seconds'
   ENDIF
   !This doesn't work
   !Write to file
   ! amode = MPI_MODE_WRONLY + MPI_MODE_CREATE
   ! info = MPI_INFO_NULL
   ! CALL MPI_FILE_OPEN(MPI_COMM_WORLD, output, amode, info, fh, ierror)
   ! x = 0.0_MK
   ! y = 0.0_MK
   ! WRITE(char_buffer, "(3F12.5,A)") x, y, temp_new(1, 1),"\n"
   ! CALL MPI_TYPE_SIZE(MPI_CHAR, size, ierror)
   
   ! disp = p_rank * (Nx/c_size) * (Ny_local) * ((LEN_TRIM(char_buffer)) * size) +&
   !     p_rank * Ny_local * 10 * size   
   
   ! CALL MPI_FILE_SEEK(fh, disp, MPI_SEEK_SET, ierror)
   ! DO j = 1, Ny_local
   !    y = REAL(j - 1, KIND=MK) * Dy
   !    DO i = 1, Nx_local
   !       x = REAL((i - 1) * (p_rank + 1), KIND=MK) * Dx
   !       WRITE(char_buffer, "(F10.4,F10.4,F10.4,A)") x, y, temp_new(i, j), " \n "
   !       CALL MPI_FILE_WRITE(fh, char_buffer, LEN_TRIM(char_buffer), &
   !          MPI_CHAR, status, ierror)
   !    ENDDO
   !    ! WRITE(char_buffer, "(10A)") "             "
   !    ! CALL MPI_FILE_WRITE(fh, char_buffer, 10, &
   !    !    MPI_CHAR, status, ierror)
   ! ENDDO

   ! CALL MPI_FILE_CLOSE(fh, ierror)
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


