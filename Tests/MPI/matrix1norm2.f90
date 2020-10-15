! $UWHPSC/codes/mpi/matrix1norm2.f90
!
! Compute 1-norm of a matrix using mpi.
! Process 0 is the master that sets things up and then sends a column
! to each worker (Processes 1, 2, ..., num_procs - 1).
!
! This version allows more columns than workers.

program matrix1norm2

    use mpi

    implicit none

    integer :: i,j,jj,nrows,ncols,proc_num, num_procs,ierr,nerr
    integer :: numsent, sender, nextcol
    integer, dimension(MPI_STATUS_SIZE) :: status
    real(kind=8) :: colnorm
    real(kind=8), allocatable, dimension(:,:) :: a
    real(kind=8), allocatable, dimension(:) :: anorm, colvect

    logical :: debug

    debug = .true.

    call MPI_INIT(ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, proc_num, ierr)

    if (proc_num==0) then
        print *, "Input nrows, ncols"
        read *, nrows, ncols
        allocate(a(nrows,ncols))  ! only master process 0 needs the matrix
        a = 1.d0  ! initialize to all 1's for this test
        allocate(anorm(ncols))    ! to hold norm of each column in MPI_RECV
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

      numsent = 0 ! keep track of how many columns sent

      ! send the first batch to get all workers working:
      do j=1,min(num_procs-1, ncols)
        call MPI_SEND(a(1,j), nrows, MPI_DOUBLE_PRECISION,&
                        j, j, MPI_COMM_WORLD, ierr)
        numsent = numsent + 1
        enddo

      ! as results come back, send out more work...
      ! the variable sender tells who sent back a result and ready for more
      do j=1,ncols
        call MPI_RECV(colnorm, 1, MPI_DOUBLE_PRECISION, &
                        MPI_ANY_SOURCE, MPI_ANY_TAG, &
                        MPI_COMM_WORLD, status, ierr)
        sender = status(MPI_SOURCE)
        jj = status(MPI_TAG)
        anorm(jj) = colnorm

        if (numsent < ncols) then
            ! still more work to do, the next column will be sent and
            ! this index also used as the tag:
            nextcol = numsent + 1 
            call MPI_SEND(a(1,nextcol), nrows, MPI_DOUBLE_PRECISION,&
                            sender, nextcol, MPI_COMM_WORLD, ierr)
            numsent = numsent + 1
          else
            ! send an empty message with tag=0 to indicate this worker
            ! is done:
            call MPI_SEND(MPI_BOTTOM, 0, MPI_DOUBLE_PRECISION,&
                            sender, 0, MPI_COMM_WORLD, ierr)
          endif
            
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

        do while (.true.)
            ! repeat until message with tag==0 received...

            call MPI_RECV(colvect, nrows, MPI_DOUBLE_PRECISION,&
                          0, MPI_ANY_TAG, &
                          MPI_COMM_WORLD, status, ierr)

            j = status(MPI_TAG)   ! this is the column number
                                  ! may not be proc_num in general

            if (debug) then
                print '("+++ Process ",i4,"  received message with tag ",i6)', &
                    proc_num, j       
                endif

            if (j==0) go to 99    ! received "done" message

            colnorm = sum(abs(colvect))

            call MPI_SEND(colnorm, 1, MPI_DOUBLE_PRECISION, &
                        0, j, MPI_COMM_WORLD, ierr)

            enddo
        endif

99  continue   ! might jump to here if finished early
    call MPI_FINALIZE(ierr)

end program matrix1norm2

