! $UWHPSC/codes/mpi/test1.f90

program test1
    use mpi
    implicit none
    integer :: ierr, numprocs, proc_num

    call mpi_init(ierr)
    call mpi_comm_size(MPI_COMM_WORLD, numprocs, ierr)
    call mpi_comm_rank(MPI_COMM_WORLD, proc_num, ierr)

    print *, 'Hello from Process number', proc_num, &
             ' of ', numprocs, ' processes'

    call mpi_finalize(ierr)

end program test1