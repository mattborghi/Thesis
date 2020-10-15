! $UWHPSC/codes/mpi/pisum1.f90

! Computes pi using MPI.  
! Compare to $UWHPSC/codes/openmp/pisum2.f90 

program pisum1
    use mpi
    implicit none
    integer :: ierr, numprocs, proc_num, points_per_proc, n, &
               i, istart, iend
    real (kind=8) :: x, dx, pisum, pisum_proc, pi

    call mpi_init(ierr)
    call mpi_comm_size(MPI_COMM_WORLD, numprocs, ierr)
    call mpi_comm_rank(MPI_COMM_WORLD, proc_num, ierr)

    ! Ask the user for the number of points
    if (proc_num == 0) then
        print *, "Using ",numprocs," processors"
        print *, "Input n ... "
        read *, n
    end if

    ! Broadcast to all procs; everybody gets the value of n from proc 0
    call mpi_bcast(n, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

    dx = 1.d0/n

    ! Determine how many points to handle with each proc
    points_per_proc = (n + numprocs - 1)/numprocs
    if (proc_num == 0) then   ! Only one proc should print to avoid clutter
        print *, "points_per_proc = ", points_per_proc
    end if

    ! Determine start and end index for this proc's points
    istart = proc_num * points_per_proc + 1
    iend = min((proc_num + 1)*points_per_proc, n)

    ! Diagnostic: tell the user which points will be handled by which proc
    print '("Process ",i2," will take i = ",i6," through i = ",i6)', &
          proc_num, istart, iend

    pisum_proc = 0.d0
    do i=istart,iend
        x = (i-0.5d0)*dx
        pisum_proc = pisum_proc + 1.d0 / (1.d0 + x**2)
    enddo

    call MPI_REDUCE(pisum_proc,pisum,1,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
                        MPI_COMM_WORLD,ierr)

    if (proc_num == 0) then
        pi = 4.d0 * dx * pisum 
        print *, "The approximation to pi is ",pi
        endif

    call mpi_finalize(ierr)

end program pisum1