program files
use rm
implicit none

character(len=100)::file="write.dat"

integer::nrows=10,ncols=10,iniseed=3,average=1
real::npos=0.5,nneg=0.5
integer,allocatable::matriz(:,:)
!Index to loop the matrix
integer::i,j,k,IDfile1 = 35
real::rvec(1)

allocate(matriz(ncols,nrows))
matriz(:,:) = 0
do j=1,nrows
	do i = 1,ncols
		call ranmar(rvec,1,iniseed*average)
		!Initial seed set to iniseed*average so that the programs
		!runs with a different initial configuration depending 
		!on the average value
		if (rvec(1) < npos) then
			matriz(i,j) = 1
		else 
			matriz (i,j) = -1
		end if
	end do
end do

open (UNIT=IDfile1, FILE=file,ACTION='write',STATUS='replace')
!close(IDfile1)
!open (UNIT=IDfile1, FILE=file,ACTION='write',STATUS='append')
do k = 1,2
	do j = 1,nrows
		do i = 1,ncols

			if(j== nrows .AND. i== ncols) then
				write(35,'(I2.1,/)',advance='no') matriz(i,j)
				print *, i,j
			!else if(j == 1 .AND. i==1) then
			!	write(35,'(I2.1)') matriz(i,j)
			!	write(35,'(A)',advance='no') ","
			!	print *, i,j
			else
				write(35,'(I2.1)',advance='no') matriz(i,j)
				write(35,'(A)',advance='no') ","
			end if
		end do
	end do
end do
close(IDfile1)
end program files