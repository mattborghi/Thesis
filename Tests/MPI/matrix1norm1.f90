! $UWHPSC/codes/mpi/matrix1norm1.f90
!
! Compute 1-norm of a matrix using mpi.
! Process 0 is the master that sets things up and then sends a column
! to each worker (Processes 1, 2, ..., num_procs - 1).
!
! This version assumes there are at least as many workers as columns.

program matrix1norm1

    use mpi

    implicit none

    integer :: i,j,jj,nrows,ncols,proc_num, num_procs,ierr,nerr
    integer, dimension(MPI_STATUS_SIZE) :: status
    real(kind=8) :: colnorm
    real(kind=8), allocatable, dimension(:,:) :: a
    real(kind=8), allocatable, dimension(:) :: anorm, colvect

    call MPI_INIT(ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, proc_num, ierr)

    nerr = 0
    if (proc_num==0) then
        print *, "Input nrows, ncols"
        read *, nrows, ncols
        if (ncols > num_procs-1) then
            print *, "*** Error, this version requires ncols < num_procs = ",&
                  num_procs
            nerr = 1
            endif
        allocate(a(nrows,ncols))  ! only master process 0 needs the matrix
        a = 1.d0  ! initialize to all 1's for this test
        allocate(anorm(ncols))    ! to hold norm of each column in MPI_RECV
        endif

    ! if nerr == 1 then all processes must stop:
    call MPI_BCAST(nerr, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)

    if (nerr == 1) then
        ! Note that error message already printed by Process 0
        ! All processes must execute the MPI_FINALIZE 
        ! (Could also just have "go to 99" here.)
        call MPI_FINALIZE(ierr)
        stop
        endif
        
    call MPI_BCAST(nrows, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    call MPI_BCAST(ncols, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)

    if (proc_num > 0) then
        allocate(colvect(nrows))   ! to hold a column vector sent from master
        endif 


    
    ! -----------------------------------------
    ! code for Master (Processor 0):
    ! -----------------------------------------

    if (proc_num == 0) then

      do j=1,ncols
        call MPI_SEND(a(1,j), nrows, MPI_DOUBLE_PRECISION,&
                        j, j, MPI_COMM_WORLD, ierr)
        enddo

      do j=1,ncols
        call MPI_RECV(colnorm, 1, MPI_DOUBLE_PRECISION, &
                        MPI_ANY_SOURCE, MPI_ANY_TAG, &
                        MPI_COMM_WORLD, status, ierr)
        jj = status(MPI_TAG)
        anorm(jj) = colnorm
        enddo

      print *, "Finished filling anorm with values... "
      print *, anorm
      print *, "1-norm of matrix a = ", maxval(anorm)
      endif


    ! -----------------------------------------
    ! code for Workers (Processors 1, 2, ...):
    ! -----------------------------------------
    if (proc_num /= 0) then

        if (proc_num > ncols) go to 99   ! no work expected

        call MPI_RECV(colvect, nrows, MPI_DOUBLE_PRECISION,&
                      0, MPI_ANY_TAG, &
                      MPI_COMM_WORLD, status, ierr)

        j = status(MPI_TAG)   ! this is the column number
                              ! (should agree with proc_num)

        colnorm = sum(abs(colvect))

        call MPI_SEND(colnorm, 1, MPI_DOUBLE_PRECISION, &
                    0, j, MPI_COMM_WORLD, ierr)

        endif

99  continue   ! might jump to here if finished early
    call MPI_FINALIZE(ierr)

end program matrix1norm1


            