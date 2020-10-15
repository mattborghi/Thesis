program inimatrix

use rm
use iso_fortran_env
implicit none
integer::L = 128,i,j,seed= 2
real::rvec(1)
integer,allocatable::matriz(:,:)
real::npos,nneg,nnull,total
integer::pos,neg,null,dim
integer::posx,posy,tiros
real::probpos,probneg,probnull

dim = L*L

tiros = 100000
allocate(matriz(L,L))

npos = 0.05
nneg = 0.05
nnull = 1 - npos - nneg 

probpos = 0.33
probneg = 0.33
probnull =  1 - probpos - probneg

do i = 1, L
	do j = 1,L
		call ranmar(rvec,1,seed)
		if (rvec(1) < npos) then
			matriz(i,j) = 1
		else if (rvec(1) < (npos +nneg)) then
			matriz(i,j) = -1
		else
			matriz(i,j) = 0
		end if
		!print *, i,j,matriz(i,j)
	end do
end do

call CountPosNeg(matriz,L,L,npos,nneg,nnull)

!Probando subrutina elegir sitio
pos = 0 
neg=0
null=0
do i=1,tiros
	call elegir_sitio_Kawasaki(L,L,posx,posy,seed,probpos,probneg,matriz,0)
	if ( matriz(posx,posy) == 1) then
		pos = pos + 1
	else if(matriz(posx,posy) == -1) then
		neg = neg + 1
	else
		null = null + 1
	end if
end do
total = pos + null + neg
print*,'---------------------------------------------------'
print *,'#pos:',probpos,'=',pos/total,' #neg:',probneg,'=',neg/total,' #null:',probnull,'=',null/total

end program inimatrix


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine CountPosNeg(matriz,nrows,ncols,npos,nneg,nnull)

!Subroutine that counts the number of negative positive and null spins
!in the matrix array
!can be used whenever we want and it will display in screen the values.

integer,intent(in)::nrows,ncols
real,intent(in)::npos,nneg,nnull
integer,intent(in)::matriz(nrows,ncols)

!Index to loop the matrix
	integer::i,j,pos,neg,null,dim
	dim = nrows*ncols
	pos = 0 
	neg = 0
	null = 0

print *, 'npos',npos*dim,'nneg',nneg*dim,'nnull',nnull*dim	

	do i=1,nrows
		do j = 1,ncols
			if ( matriz(i,j) == 1) then
				pos = pos + 1
			else if(matriz(i,j) == -1) then
				neg = neg + 1
			else
				null = null + 1
			end if
		end do
	end do

print *,'#pos:',npos*dim,'=',pos,' #neg:',nneg*dim,'=',neg,' #null:',nnull*dim,'=',null
print *,'fracpos',abs(1- pos/(npos*dim)),'fracneg',abs(1- neg/(nneg*dim)),'fracnull',abs(1- null/(nnull*dim))

end subroutine CountPosNeg

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine elegir_sitio_Kawasaki(nrows,ncols,posx,posy,iniseed,probpos,probneg,matriz,average)
	use rm
	
	!This subroutine gives a random point in the square matrix of LxL
	!subject to the constrains probpos and probneg
	integer,intent(in)::nrows,ncols,iniseed,average
	integer,intent(out)::posx,posy
	real,intent(in)::probpos,probneg
	integer,intent(in)::matriz(nrows,ncols)
	integer::i
	!integer,allocatable::array1(:),array2(:)
	real::rvec(3),rvec2(2)

		!print *, 'rvec:',rvec(1),rvec(2)
		!Given values between 0-1 from rvec we want values between 1-nrows(or ncols) for each coordinate 
		!So this lines map one values to others 0-1 -> 1-nrows(or ncols)
	call ranmar(rvec,3,iniseed*average)	
	if (rvec(1) < probpos ) then
		!call ranmar(rvec,3,iniseed*average)
		posx = floor( nrows * rvec(2) ) + 1
		posy = floor( ncols * rvec(3) ) + 1
		!print *,posx,posy,matriz(posx,posy)
		do while(matriz(posx,posy) /= 1)
			call ranmar(rvec2,2,iniseed*average)
			posx = floor( nrows * rvec2(1) ) + 1
			posy = floor( ncols * rvec2(2) ) + 1
		end do			
		!allocate(array1(size(PACK(SPREAD((/(i, i=1,nrows, 1)/), DIM=2, NCOPIES=nrows), MASK=(matriz == 1)) )))
		!allocate(array2(size(PACK(SPREAD((/(i, i=1,ncols, 1)/), DIM=2, NCOPIES=ncols), MASK=(matriz == 1)) )))
			!print *,'c=',PACK(SPREAD((/1,2,3,4/), DIM=2, NCOPIES=4), MASK=(matriz == 1)) 
			!print *,'d=',PACK(SPREAD((/1,2,3,4/), DIM=1, NCOPIES=4), MASK=(matriz == 1))  
		!array1 = PACK(SPREAD((/(i, i=1,nrows, 1)/), DIM=2, NCOPIES=nrows), MASK=(matriz == 1)) 
		!array2 = PACK(SPREAD((/(i, i=1,ncols, 1)/), DIM=1, NCOPIES=ncols), MASK=(matriz == 1))  
	else if (rvec(1)< (probpos+probneg)) then
		posx = floor( nrows * rvec(2) ) + 1
		posy = floor( ncols * rvec(3) ) + 1
		do while(matriz(posx,posy) /= -1)
			call ranmar(rvec2,2,iniseed*average)
			posx = floor( nrows * rvec2(1) ) + 1
			posy = floor( ncols * rvec2(2) ) + 1
		end do
		!allocate(array1(size(PACK(SPREAD((/(i, i=1,nrows, 1)/), DIM=2, NCOPIES=nrows), MASK=(matriz == -1)) )))
		!allocate(array2(size(PACK(SPREAD((/(i, i=1,ncols, 1)/), DIM=2, NCOPIES=ncols), MASK=(matriz == -1)) )))
			!print *,'c=',PACK(SPREAD((/1,2,3,4/), DIM=2, NCOPIES=4), MASK=(matriz == 1)) 
			!print *,'d=',PACK(SPREAD((/1,2,3,4/), DIM=1, NCOPIES=4), MASK=(matriz == 1))  
		!array1 = PACK(SPREAD((/(i, i=1,nrows, 1)/), DIM=2, NCOPIES=nrows), MASK=(matriz == -1)) 
		!array2 = PACK(SPREAD((/(i, i=1,ncols, 1)/), DIM=1, NCOPIES=ncols), MASK=(matriz == -1))  
	else 
		posx = floor( nrows * rvec(2) ) + 1
		posy = floor( ncols * rvec(3) ) + 1
		do while(matriz(posx,posy) /= 0)
			call ranmar(rvec2,2,iniseed*average)
			posx = floor( nrows * rvec2(1) ) + 1
			posy = floor( ncols * rvec2(2) ) + 1
		end do
	end if
		! all random_number(u)
		!j = n + FLOOR((m+1-n)*u)
	!i = 1 + FLOOR((size(array1))*rvec(2))
	!posx = array1(i)
	!posy = array2(i)
	!deallocate(array1)
	!deallocate(array2)
	
	!print *, 'posx: ',posx,'posy',posy
end subroutine elegir_sitio_Kawasaki
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!