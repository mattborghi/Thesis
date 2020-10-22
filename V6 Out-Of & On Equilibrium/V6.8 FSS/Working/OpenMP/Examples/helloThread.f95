program helloThread
	integer :: i,j,N,M
	integer :: nthread,OMP_GET_NUM_THREADS,OMP_GET_THREAD_NUM,tid
	N = 2 
	M = 4
	nthread = 2
	call omp_set_num_threads(nthread) !requests "nthread" threads
	!$OMP PARALLEL DO
	 DO i=1,N
	 DO j=1,M
	 	TID = OMP_GET_THREAD_NUM()
	 	print *, "Printing: i = ",i," j = ",j ,' from thread: ',TID
	 END DO
	 END DO
	!$OMP END PARALLEL DO
end program helloThread