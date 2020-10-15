program package
use rm
implicit none
integer,parameter::ncols=4,nrows=4
integer::matriz(ncols,nrows)
integer::i,j,k
integer::iniseed
real::pPlus,rvec(1),rvec2(2)
integer::posx,posy
integer,allocatable::array1(:),array2(:)

iniseed = 2
pPlus = 0.5

!Fill matrix
do i = 1,ncols
	do j = 1,nrows
		call ranmar(rvec,1,iniseed)
		if( rvec(1) < 0.5 ) then
			matriz(i,j) = 1
		else
			matriz(i,j) = -1
		end if
	end do
end do

!Print array on screen
print *, 'MATRIX--->'
do, i=1,nrows
    write(*,*) ( matriz(i,j), j=1,ncols )
enddo

call ranmar(rvec2,2,iniseed)
print *, 'rvec: ', rvec2(1),rvec2(2)
if ( rvec2(1) <  pPlus ) then
	!It is positive
	!Obtain the locations of the +1 spins
	!print *,'c=',PACK(SPREAD((/1,2,3/), DIM=2, NCOPIES=4), MASK=(A == 1.0)) 
	!print *,'d=',PACK(SPREAD((/1,2,3,4/), DIM=1, NCOPIES=3), MASK=(A == 1.0)) 
	allocate(array1(size(PACK(SPREAD((/(i, i=1,nrows, 1)/), DIM=2, NCOPIES=4), MASK=(matriz == 1)) )))
	allocate(array2(size(PACK(SPREAD((/1,2,3,4/), DIM=2, NCOPIES=4), MASK=(matriz == 1)) )))
	print *,'c=',PACK(SPREAD((/1,2,3,4/), DIM=2, NCOPIES=4), MASK=(matriz == 1)) 
	print *,'d=',PACK(SPREAD((/1,2,3,4/), DIM=1, NCOPIES=4), MASK=(matriz == 1))  
	array1 = PACK(SPREAD((/1,2,3,4/), DIM=2, NCOPIES=4), MASK=(matriz == 1)) 
	array2 = PACK(SPREAD((/1,2,3,4/), DIM=1, NCOPIES=4), MASK=(matriz == 1))  
else
	!It is negative
	allocate(array1(size(PACK(SPREAD((/1,2,3,4/), DIM=2, NCOPIES=4), MASK=(matriz == -1)) )))
	allocate(array2(size(PACK(SPREAD((/1,2,3,4/), DIM=2, NCOPIES=4), MASK=(matriz == -1))  )))
	
	print *,'c=',PACK(SPREAD((/1,2,3,4/), DIM=2, NCOPIES=4), MASK=(matriz == -1)) 
	print *,'d=',PACK(SPREAD((/1,2,3,4/), DIM=1, NCOPIES=4), MASK=(matriz == -1))  

	array1 = PACK(SPREAD((/1,2,3,4/), DIM=2, NCOPIES=4), MASK=(matriz == -1)) 
	array2 = PACK(SPREAD((/1,2,3,4/), DIM=1, NCOPIES=4), MASK=(matriz == -1))  
end if

!from 1 to size(array1)
k = 1 + FLOOR((size(array1))*rvec2(2))
posx = array1(k)
posy = array2(k)
deallocate(array1)
deallocate(array2)
print *,"posx: ",posx,"posy:",posy	

end program package