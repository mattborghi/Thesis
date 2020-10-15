program testCol

use subroutines
use rm
use iso_fortran_env
implicit none

integer::nrow = 50,ncol = 300
integer::i,j,npos,dim
integer,allocatable::matriz(:,:)
real::rvec(1)
allocate(matriz(ncol,nrow))
dim = nrow*ncol
npos = 0
!Fill matrix
do j=1,nrow
	do i = 1,ncol
		call ranmar(rvec,1,2)
		!if(rvec(1) < 0.5) then
		if (npos < 0.5*dim ) then
			matriz(i,j) = 1 
			npos = npos + 1
		else
			matriz(i,j) = -1
		end if
	end do
end do


do j = 1,nrow
	do i = 1,ncol
		write(*,*) i,j, matriz(i,j)
	end do 
end do




end program
