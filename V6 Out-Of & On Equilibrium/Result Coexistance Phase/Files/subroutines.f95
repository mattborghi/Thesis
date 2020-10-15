module subroutines
use rm
implicit none
integer, parameter :: dp2 = selected_real_kind(15, 307) !For double precision
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine SetToZeroVariables(EnergyAver,Energy2,magnetANTI,magnetAnti2,magnetAnti4,magnetFerro,magnetFerro2,magnetFerro4,length)
	
	integer,intent(in)::length
	real(dp2),intent(out)::magnetANTI(length),magnetAnti2(length),magnetAnti4(length),magnetFerro(length),magnetFerro2(length),magnetFerro4(length)
	real(dp2),intent(out)::EnergyAver(length),Energy2(length)

		EnergyAver(:) = 0.0
		Energy2(:) = 0.0
		magnetANTI(:) = 0.0
		magnetAnti2(:) = 0.0
		magnetAnti4(:) = 0.0
		magnetFerro(:) = 0.0
		magnetFerro2(:) = 0.0
		magnetFerro4(:) = 0.0

end subroutine SetToZeroVariables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine Inicializacion_DeltaE(DeltaE,Temp)
	
	real,intent(in)::Temp
	real(dp2),intent(out)::DeltaE(20)

	DeltaE(1) = exp(-1/Temp)
	DeltaE(2) = exp(-2/Temp)
	DeltaE(3) = exp(-3/Temp)
	DeltaE(4) = exp(-4/Temp)
	DeltaE(5) = exp(-5/Temp)
	DeltaE(6) = exp(-6/Temp)
	DeltaE(7) = exp(-7/Temp)
	DeltaE(8) = exp(-8/Temp)
	DeltaE(9) = exp(-9/Temp)
	DeltaE(10) = exp(-10/Temp)
	DeltaE(11) = exp(-11/Temp)
	DeltaE(12) = exp(-12/Temp)
	DeltaE(13) = exp(-13/Temp)
	DeltaE(14) = exp(-14/Temp)
	DeltaE(15) = exp(-15/Temp)
	DeltaE(16) = exp(-16/Temp)
	DeltaE(17) = exp(-17/Temp)
	DeltaE(18) = exp(-18/Temp)
	DeltaE(19) = exp(-19/Temp)
	DeltaE(20) = exp(-20/Temp)
end subroutine Inicializacion_DeltaE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine ini_alpha(alpha,alpha11,alpha13,alpha33)

	real,intent(in)::alpha11,alpha13,alpha33
	real,intent(out)::alpha(3,3)

	alpha(1,1) = alpha11
	alpha(1,2) = 0
	alpha(1,3) = alpha13
	alpha(2,1) = 0
	alpha(2,2) = 0
	alpha(2,3) = 0
	alpha(3,1) = alpha13
	alpha(3,2) = 0
	alpha(3,3) = alpha33

	!print*,"Alpha matrix: ",alpha(1,1),alpha(3,3),alpha(1,3)
end subroutine ini_alpha
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

!print *,'#pos:',npos*dim,'=',pos,' #neg:',nneg*dim,'=',neg,' #null:',nnull*dim,'=',null
end subroutine CountPosNeg
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine ini_matriz(matriz,nrows,ncols,npos,nneg,iniseed,average)
	
	!Initialize a square matrix of dimention nrowsXncols 
	!with ones, minues ones and zeros
	!that means spin-ups, spin dows and holes
	integer,intent(in)::nrows,ncols,iniseed,average
	real,intent(in)::npos,nneg
	integer,intent(out)::matriz(ncols,nrows)

	!Index to loop the matrix
	integer::i,j
	real::rvec(1)
	
	do i=1,ncols
		do j = 1,nrows
			call ranmar(rvec,1,iniseed*average)
			!Initial seed set to iniseed*average so that the programs
			!runs with a different initial configuration depending 
			!on the average value
			if (rvec(1) < npos) then
				matriz(i,j) = 1
			else if (rvec(1) < (npos+nneg)) then
				matriz (i,j) = -1
			else
				matriz(i,j) = 0
			end if
		end do
	end do

end subroutine ini_matriz
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine elegir_sitio_SpinFlip(nrows,ncols,posx,posy,iniseed,average)

	!This subroutine gives a random point in the square matrix of LxL
	integer,intent(in)::nrows,ncols,iniseed,average
	integer,intent(out)::posx,posy
	integer::i
	real::rvec(2)

	call ranmar(rvec,2,iniseed*average)
	posx = floor( ncols * rvec(1) ) + 1
	posy = floor( nrows * rvec(2) ) + 1

end subroutine elegir_sitio_SpinFlip
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine elegir_sitio_Kawasaki(nrows,ncols,posx,posy,iniseed,probpos,probneg,matriz,average)
	
	!This subroutine gives a random point in the square matrix of LxL
	!subject to the constrains probpos and probneg
	integer,intent(in)::nrows,ncols,iniseed,average
	integer,intent(out)::posx,posy
	real,intent(in)::probpos,probneg
	integer,intent(in)::matriz(ncols,nrows)
	integer::i
	!integer,allocatable::array1(:),array2(:)
	real::rvec(3),rvec2(2)

		!print *, 'rvec:',rvec(1),rvec(2)
		!Given values between 0-1 from rvec we want values between 1-nrows(or ncols) for each coordinate 
		!So this lines map one values to others 0-1 -> 1-nrows(or ncols)
	call ranmar(rvec,3,iniseed*average)	
	if (rvec(1) < probpos ) then
		posx = floor( ncols * rvec(2) ) + 1
		posy = floor( nrows * rvec(3) ) + 1
		!print *,posx,posy,matriz(posx,posy)
		do while(matriz(posx,posy) /= 1)
			call ranmar(rvec2,2,iniseed*average)
			posx = floor( ncols * rvec2(1) ) + 1
			posy = floor( nrows * rvec2(2) ) + 1
		end do			
		!allocate(array1(size(PACK(SPREAD((/(i, i=1,nrows, 1)/), DIM=2, NCOPIES=nrows), MASK=(matriz == 1)) )))
		!allocate(array2(size(PACK(SPREAD((/(i, i=1,ncols, 1)/), DIM=2, NCOPIES=ncols), MASK=(matriz == 1)) )))
			!print *,'c=',PACK(SPREAD((/1,2,3,4/), DIM=2, NCOPIES=4), MASK=(matriz == 1)) 
			!print *,'d=',PACK(SPREAD((/1,2,3,4/), DIM=1, NCOPIES=4), MASK=(matriz == 1))  
		!array1 = PACK(SPREAD((/(i, i=1,nrows, 1)/), DIM=2, NCOPIES=nrows), MASK=(matriz == 1)) 
		!array2 = PACK(SPREAD((/(i, i=1,ncols, 1)/), DIM=1, NCOPIES=ncols), MASK=(matriz == 1))  
	else !if (rvec(1) < (probpos + probneg)) then
		posx = floor( ncols * rvec(2) ) + 1
		posy = floor( nrows * rvec(3) ) + 1
		do while(matriz(posx,posy) /= -1)
			call ranmar(rvec2,2,iniseed*average)
			posx = floor( ncols * rvec2(1) ) + 1
			posy = floor( nrows * rvec2(2) ) + 1
		end do
		!allocate(array1(size(PACK(SPREAD((/(i, i=1,nrows, 1)/), DIM=2, NCOPIES=nrows), MASK=(matriz == -1)) )))
		!allocate(array2(size(PACK(SPREAD((/(i, i=1,ncols, 1)/), DIM=2, NCOPIES=ncols), MASK=(matriz == -1)) )))
			!print *,'c=',PACK(SPREAD((/1,2,3,4/), DIM=2, NCOPIES=4), MASK=(matriz == 1)) 
			!print *,'d=',PACK(SPREAD((/1,2,3,4/), DIM=1, NCOPIES=4), MASK=(matriz == 1))  
		!array1 = PACK(SPREAD((/(i, i=1,nrows, 1)/), DIM=2, NCOPIES=nrows), MASK=(matriz == -1)) 
		!array2 = PACK(SPREAD((/(i, i=1,ncols, 1)/), DIM=1, NCOPIES=ncols), MASK=(matriz == -1))  
	
	!else 
	!	posx = floor( nrows * rvec(2) ) + 1
	!	posy = floor( ncols * rvec(3) ) + 1
	!	do while(matriz(posx,posy) /= 0)
	!		call ranmar(rvec2,2,iniseed*average)
	!		posx = floor( nrows * rvec2(1) ) + 1
	!		posy = floor( ncols * rvec2(2) ) + 1
	!	end do
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
subroutine exchange(nrows,ncols,posx,posy,matriz,ip,newposx,newposy)
	
	integer,intent(in)::nrows,ncols,posx,posy,newposx,newposy
	integer,intent(in)::ip(nrows)
	integer,intent(inout)::matriz(ncols,nrows)

	integer::exchangeSpin

	exchangeSpin = matriz(posx,posy)
	matriz(posx,posy) = matriz(newposx,newposy)
	matriz(newposx,newposy) = exchangeSpin

end subroutine exchange
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine CalcEnergy(nrows,ncols,posx,posy,matriz,alpha,Delta_E,ip,im,extMagField)
	!Calculate Delta for a given point (posx,posy)
	!with periodic boundary conditions
	integer,intent(in)::nrows,ncols
	integer,intent(in)::posx,posy,matriz(ncols,nrows),ip(nrows),im(nrows)
	real,intent(out)::Delta_E
	real,intent(in)::alpha(3,3),extMagField
	
	integer::J = 1
	
	Delta_E =-J*matriz(posx,posy)* &
		( matriz(ip(posx),posy)*alpha(matriz(posx,posy)+2,matriz(ip(posx),posy)+2) &
		+ matriz(im(posx),posy)*alpha(matriz(posx,posy)+2,matriz(im(posx),posy)+2) &
	    + matriz(posx,ip(posy))*alpha(matriz(posx,posy)+2,matriz(posx,ip(posy))+2) &
	    + matriz(posx,im(posy))*alpha(matriz(posx,posy)+2,matriz(posx,im(posy))+2) ) &
	    - extMagField*matriz(posx,posy)	

	!print *,'Neighbours spin s1 =',matriz(im(posx),posy),'s2=',matriz(posx,ip(posy)),'en',posx,ip(posy),&
	!		's3=',matriz(ip(posx),posy),'s4=',matriz(posx,im(posy))
	
	!if (posx == 1 .AND. posy == 1) then
!		Delta_E = -J*matriz (posx,posy) * ( matriz (posx+1,posy) + matriz (nrows,posy) + matriz (posx,posy+1) + matriz (posx,ncols) )
!	else if (posx == 1 .AND. posy == ncols) then
!		Delta_E = -J*matriz (posx,posy) * ( matriz (posx+1,posy) + matriz (nrows,posy) + matriz (posx,1) + matriz (posx,posy-1) )
!	else if (posx == nrows .AND. posy == 1) then
!		Delta_E = -J*matriz (posx,posy) * ( matriz (1,posy) + matriz (posx-1,posy) + matriz (posx,posy+1) + matriz (posx,ncols) )
!	else if (posx == nrows .AND. posy == ncols) then
!		Delta_E = -J*matriz (posx,posy) * ( matriz (1,posy) + matriz (posx-1,posy) + matriz (posx,1) + matriz (posx,posy-1) )
!	else if (posx == 1 .AND. posy/=1 .AND. posy/=ncols) then
!		Delta_E = -J*matriz (posx,posy) * ( matriz (posx+1,posy) + matriz (nrows,posy) + matriz (posx,posy+1) + matriz (posx,posy-1) )
!	else if (posx == nrows .AND. posy/=1 .AND. posy/=ncols) then
!		Delta_E = -J*matriz (posx,posy) * ( matriz (1,posy) + matriz (posx-1,posy) + matriz (posx,posy+1) + matriz (posx,posy-1) )
!	else if (posy == 1 .AND. posx/=1 .AND. posx/=nrows) then
!		Delta_E = -J*matriz (posx,posy) * ( matriz (posx+1,posy) + matriz (posx-1,posy) + matriz (posx,posy+1) + matriz (posx,ncols) )
!	else if (posy == ncols .AND. posx/=1 .AND. posx/=nrows) then
!		Delta_E = -J*matriz (posx,posy) * ( matriz (posx+1,posy) + matriz (posx-1,posy) + matriz (posx,1) + matriz (posx,posy-1) )
!	else
!		!Calculate DeltaE for a point far from the boundaries
!		Delta_E = -J*matriz (posx,posy) * ( matriz (posx+1,posy) + matriz (posx-1,posy) + matriz (posx,posy+1) + matriz (posx,posy-1) )
!	end if

end subroutine CalcEnergy
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine CalcDeltaEnergyKawasaki(nrows,ncols,posx,posy,matriz,alpha,Delta_E,ip,im,iniseed,newposx,newposy,extMagField)
	!Calculate Delta for a given point (posx,posy)
	!with periodic boundary conditions
	integer,intent(in)::nrows,ncols,iniseed
	integer,intent(in)::posx,posy,matriz(ncols,nrows),ip(nrows),im(nrows)
	integer,intent(in)::newposx,newposy
	real,intent(in)::alpha(3,3),extMagField
	real,intent(out)::Delta_E
	real::Delta_E2,Delta_E3,rvec(1)
	integer::J = 1
	
	!Calculate the energy of the OLD state
	!call CalcEnergy(nrows,ncols,posx,posy,matriz,alpha,Delta_E,ip,im,extMagField)
	
	!Calculate the energy of a random NEIGHBOUR state 
	!print*,posx,posy,newposx,newposy
	!call CalcEnergy(nrows,ncols,newposx,newposy,matriz,alpha,Delta_E2,ip,im,extMagField) 
	!Delta_E = Delta_E + Delta_E2 + J*matriz(posx,posy)*matriz(newposx,newposy)*alpha(matriz(posx,posy)+2,matriz(newposx,newposy)+2)
	!Just (remove)add the term that it was counted twice

	!Calculate the energy of the NEW state
	!call NeighboursEnergy(nrows,ncols,posx,posy,matriz,alpha,Delta_E3,ip,im)
	!Delta_E3 = (Delta_E3 - extMagField)*matriz(newposx,newposy) 
	!call NeighboursEnergy(nrows,ncols,newposx,newposy,matriz,alpha,Delta_E2,ip,im)
	!Delta_E2 = (Delta_E2 - extMagField)*matriz(posx,posy)
	!Finally..
	!Delta_E2 = Delta_E2 + Delta_E3 + J*matriz(posx,posy)*matriz(newposx,newposy)*alpha(matriz(posx,posy)+2,matriz(newposx,newposy)+2)  
	!DeltaE = E_NEW - E_OLD
	!print*,"Btw ",matriz(posx,posy),"and",matriz(newposx,newposy)
	!print*,Delta_E2,Delta_E,Delta_E2 - Delta_E
	!Delta_E = Delta_E2 - Delta_E 

	!Method v2
	!call CalcEnergy(nrows,ncols,posx,posy,matriz,alpha,Delta_E,ip,im,extMagField)
	!Delta_E = Delta_E*(-2.0)
	!print *,'E1 = ',Delta_E
	!call CalcEnergy(nrows,ncols,newposx,newposy,matriz,alpha,Delta_E2,ip,im,extMagField)
	!Delta_E2 = Delta_E2*(-2.0)
	!print *,'E2 = ',Delta_E2
	!Delta_E = Delta_E + Delta_E2 + 4.0*alpha(1,3) 
	!print *,'E = ',Delta_E
	if ( posy == newposy .AND. ip(posx) == newposx  ) then

	Delta_E = matriz(posx,posy)* &
	( matriz(posx,ip(posy))*( alpha(matriz(posx,posy)+2,matriz(posx,ip(posy))+2) + alpha(matriz(newposx,newposy)+2,matriz(posx,ip(posy))+2) ) &
	+ matriz(im(posx),posy)*( alpha(matriz(posx,posy)+2,matriz(im(posx),posy)+2) + alpha(matriz(newposx,newposy)+2,matriz(im(posx),posy)+2) ) &
	+ matriz(posx,im(posy))*( alpha(matriz(posx,posy)+2,matriz(posx,im(posy))+2) + alpha(matriz(newposx,newposy)+2,matriz(posx,im(posy))+2) ) &
	- matriz(newposx,ip(newposy))*( alpha(matriz(posx,posy)+2,matriz(newposx,ip(newposy))+2) + alpha(matriz(newposx,newposy)+2,matriz(newposx,ip(newposy))+2) ) &
	- matriz(ip(newposx),newposy)*( alpha(matriz(posx,posy)+2,matriz(ip(newposx),posy)+2) + alpha(matriz(newposx,newposy)+2,matriz(ip(newposx),posy)+2) ) &
	- matriz(newposx,im(newposy))*( alpha(matriz(posx,posy)+2,matriz(newposx,im(newposy))+2) + alpha(matriz(newposx,newposy)+2,matriz(newposx,im(newposy))+2) ) )

	else if ( posy == newposy .AND. im(posx) == newposx  ) then

	Delta_E = matriz(posx,posy)* &
	( matriz(posx,ip(posy))*( alpha(matriz(posx,posy)+2,matriz(posx,ip(posy))+2) + alpha(matriz(newposx,newposy)+2,matriz(posx,ip(posy))+2) ) &
	+ matriz(ip(posx),posy)*( alpha(matriz(posx,posy)+2,matriz(ip(posx),posy)+2) + alpha(matriz(newposx,newposy)+2,matriz(ip(posx),posy)+2) ) &
	+ matriz(posx,im(posy))*( alpha(matriz(posx,posy)+2,matriz(posx,im(posy))+2) + alpha(matriz(newposx,newposy)+2,matriz(posx,im(posy))+2) ) &
	- matriz(newposx,ip(newposy))*( alpha(matriz(posx,posy)+2,matriz(newposx,ip(newposy))+2) + alpha(matriz(newposx,newposy)+2,matriz(newposx,ip(newposy))+2) ) &
	- matriz(im(newposx),newposy)*( alpha(matriz(posx,posy)+2,matriz(im(newposx),posy)+2) + alpha(matriz(newposx,newposy)+2,matriz(im(newposx),posy)+2) ) &
	- matriz(newposx,im(newposy))*( alpha(matriz(posx,posy)+2,matriz(newposx,im(newposy))+2) + alpha(matriz(newposx,newposy)+2,matriz(newposx,im(newposy))+2) ) )
	
	else if (posx == newposx .AND. ip(posy) == newposy  ) then

	Delta_E = matriz(posx,posy)* &
	( matriz(ip(posx),posy)*( alpha(matriz(posx,posy)+2,matriz(ip(posx),posy)+2) + alpha(matriz(newposx,newposy)+2,matriz(ip(posx),posy)+2) ) &
	+ matriz(im(posx),posy)*( alpha(matriz(posx,posy)+2,matriz(im(posx),posy)+2) + alpha(matriz(newposx,newposy)+2,matriz(im(posx),posy)+2) ) &
	+ matriz(posx,im(posy))*( alpha(matriz(posx,posy)+2,matriz(posx,im(posy))+2) + alpha(matriz(newposx,newposy)+2,matriz(posx,im(posy))+2) ) &
	- matriz(newposx,ip(newposy))*( alpha(matriz(posx,posy)+2,matriz(newposx,ip(newposy))+2) + alpha(matriz(newposx,newposy)+2,matriz(newposx,ip(newposy))+2) ) &
	- matriz(ip(newposx),newposy)*( alpha(matriz(posx,posy)+2,matriz(ip(newposx),posy)+2) + alpha(matriz(newposx,newposy)+2,matriz(ip(newposx),posy)+2) ) &
	- matriz(im(newposx),newposy)*( alpha(matriz(posx,posy)+2,matriz(im(newposx),newposy)+2) + alpha(matriz(newposx,newposy)+2,matriz(im(newposx),newposy)+2) ) )

	else !posx == newposx .AND. (posy-1) == newposy 

	Delta_E = matriz(posx,posy)* &
	( matriz(ip(posx),posy)*( alpha(matriz(posx,posy)+2,matriz(ip(posx),posy)+2) + alpha(matriz(newposx,newposy)+2,matriz(ip(posx),posy)+2) ) &
	+ matriz(im(posx),posy)*( alpha(matriz(posx,posy)+2,matriz(im(posx),posy)+2) + alpha(matriz(newposx,newposy)+2,matriz(im(posx),posy)+2) ) &
	+ matriz(posx,ip(posy))*( alpha(matriz(posx,posy)+2,matriz(posx,ip(posy))+2) + alpha(matriz(newposx,newposy)+2,matriz(posx,ip(posy))+2) ) &
	- matriz(newposx,im(newposy))*( alpha(matriz(posx,posy)+2,matriz(newposx,im(newposy))+2) + alpha(matriz(newposx,newposy)+2,matriz(newposx,im(newposy))+2) ) &
	- matriz(ip(newposx),newposy)*( alpha(matriz(posx,posy)+2,matriz(ip(newposx),posy)+2) + alpha(matriz(newposx,newposy)+2,matriz(ip(newposx),posy)+2) ) &
	- matriz(im(newposx),newposy)*( alpha(matriz(posx,posy)+2,matriz(im(newposx),newposy)+2) + alpha(matriz(newposx,newposy)+2,matriz(im(newposx),newposy)+2) ) )


	end if
	!if (posx == 1 .AND. posy == 1) then
!		Delta_E = CalcEnergy(nrows,ncols,1,1,matriz,Delta_E) + CalcEnergy(nrows,ncols,2,1,matriz,Delta_E)
!	else if (posx == 1 .AND. posy == ncols) then
!		Delta_E = -J*(matriz(posx+1,posy) - matriz (posx,posy) ) * ( matriz (nrows,posy) + matriz (posx,1) + matriz (posx,posy-1) )
!	else if (posx == nrows .AND. posy == 1) then
!		Delta_E = -J*(matriz(1,posy) - matriz (posx,posy) ) * ( matriz (posx-1,posy) + matriz (posx,posy+1) + matriz (posx,ncols) )
!	else if (posx == nrows .AND. posy == ncols) then
!		Delta_E = -J*(matriz(1,posy) - matriz (posx,posy) ) * ( matriz (posx-1,posy) + matriz (posx,1) + matriz (posx,posy-1) )
!	else if (posx == 1 .AND. posy/=1 .AND. posy/=ncols) then
!		Delta_E = -J*(matriz(posx+1,posy) - matriz (posx,posy) ) * ( matriz (nrows,posy) + matriz (posx,posy+1) + matriz (posx,posy-1) )
!	else if (posx == nrows .AND. posy/=1 .AND. posy/=ncols) then
!		Delta_E = -J*(matriz(1,posy) - matriz (posx,posy) )* ( matriz (posx-1,posy) + matriz (posx,posy+1) + matriz (posx,posy-1) )
!	else if (posy == 1 .AND. posx/=1 .AND. posx/=nrows) then
!		Delta_E = -J*(matriz(posx+1,posy) - matriz (posx,posy) ) * ( matriz (posx-1,posy) + matriz (posx,posy+1) + matriz (posx,ncols) )
!	else if (posy == ncols .AND. posx/=1 .AND. posx/=nrows) then
!		Delta_E = -J*(matriz(posx+1,posy) - matriz (posx,posy) )* ( matriz (posx-1,posy) + matriz (posx,1) + matriz (posx,posy-1) )
!	else
!		!Calculate DeltaE for a point far from the boundaries
!		Delta_E = -J*(matriz(posx+1,posy) - matriz (posx,posy) ) * ( matriz (posx-1,posy) + matriz (posx,posy+1) + matriz (posx,posy-1) )
!	end if

end subroutine CalcDeltaEnergyKawasaki
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine CalcDeltaEnergySpinFlip(nrows,ncols,posx,posy,matriz,Delta_E,ip,im,alpha)

	integer,intent(in)::nrows,ncols
	integer,intent(in)::posx,posy,matriz(ncols,nrows),ip(nrows),im(nrows)
	real,intent(out)::Delta_E
	real,intent(in)::alpha(3,3)
	integer::J = 1
	integer::matrizInv

	matrizInv = -matriz(posx,posy)
	!Delta_E =2*J*matriz(posx,posy) * ( matriz(ip(posx),posy) + matriz(im(posx),posy) + matriz(posx,ip(posy)) + matriz(posx,im(posy)))

	!print *,im(1),ip(nrows)
	!print *, matriz(posx,posy),matrizInv
	!print *, posx,posy,ip(posx),ip(posy),im(posx),im(posy)
	Delta_E = -J*matrizInv* &
		( matriz(ip(posx),posy)*alpha(matrizInv+2,matriz(ip(posx),posy)+2) &
		+ matriz(im(posx),posy)*alpha(matrizInv+2,matriz(im(posx),posy)+2) &
	    + matriz(posx,ip(posy))*alpha(matrizInv+2,matriz(posx,ip(posy))+2) &
	    + matriz(posx,im(posy))*alpha(matrizInv+2,matriz(posx,im(posy))+2) ) &
		+ J*matriz(posx,posy)* &
		( matriz(ip(posx),posy)*alpha(matriz(posx,posy)+2,matriz(ip(posx),posy)+2) &
		+ matriz(im(posx),posy)*alpha(matriz(posx,posy)+2,matriz(im(posx),posy)+2) &
	    + matriz(posx,ip(posy))*alpha(matriz(posx,posy)+2,matriz(posx,ip(posy))+2) &
	    + matriz(posx,im(posy))*alpha(matriz(posx,posy)+2,matriz(posx,im(posy))+2) )
	!print *,Delta_E
end subroutine CalcDeltaEnergySpinFlip
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine CalculationsINequilibrium(nrows,ncols,matriz,alpha,Delta_E,ip,im,temp,& 
	EnergyAver,EnergyAver2,averIndex,extMagField,magnetANTI,magnetAnti2,magnetAnti4,magnetFerro,magnetFerro2,magnetFerro4)

	integer,intent(in)::nrows,ncols,matriz(nrows,ncols),ip(nrows),im(nrows),averIndex
	real,intent(in)::alpha(3,3)
	real,intent(in)::temp,extMagField
	real,intent(out)::Delta_E
	integer::i,j
	integer::dim,black,white,pos,neg
	integer::npp,nmm,nzz
	!real::beta
	real(dp2)::Energy,magnetBlack,magnetWhite,antiMagnet,magnetPos,magnetNeg,Magnetizacion
	real(dp2),intent(out)::EnergyAver,EnergyAver2,magnetANTI,magnetFerro,magnetFerro2,magnetAnti2,magnetAnti4,magnetFerro4

	!beta = 1.0/temp

	dim = nrows*ncols
	magnetWhite = 0.0
	magnetBlack = 0.0
	magnetPos = 0.0
	magnetNeg = 0.0
	white = 0
	black = 0
	pos = 0
	neg = 0
	Energy = 0.0
	antiMagnet = 0.0

	do i = 1, ncols
		do j = 1, nrows
			call CalcEnergy(nrows,ncols,i,j,matriz,alpha,Delta_E,ip,im,extMagField)
			Energy = Energy + Delta_E
			!Calculate AntiFerro Quantities------
			!Ambos pares o ambos impares?
			if ( (mod(i,2) == 0 .AND. mod(j,2) == 0) .OR.(mod(i,2) /= 0 .AND. mod(j,2) /= 0) ) then
				magnetBlack = magnetBlack + matriz(i,j)
				black = black + 1
			else
				magnetWhite = magnetWhite + matriz(i,j)
				white = white + 1
			end if 
		end do
	end do
	!-----AntiFerro quantities----------------
	!<mB>
	magnetBlack = magnetBlack / real(black)
	!<mW>
	magnetWhite = magnetWhite / real(white)
	!inter 
	antiMagnet = abs(magnetBlack - magnetWhite) / real(2)
	!<mANTI>
	magnetANTI = magnetANTI + antiMagnet

	magnetAnti2 = magnetAnti2 + antiMagnet**2

	magnetAnti4 = magnetAnti4 + antiMagnet**4
	!------------------------------------------------
	! E 
	Energy = Energy/(dim*2.0)
	!Se divide por dos nuevamente para no contar dos veces interacciones entre spines
	!<E>
	EnergyAver = EnergyAver + Energy

	EnergyAver2 = EnergyAver2 + Energy**2 
	!------------------------------------------------
	Magnetizacion = sum(matriz(1:ncols,1:nrows))/real(dim*1.0)
	!<m>F
	magnetFerro = magnetFerro + abs(Magnetizacion)

	magnetFerro2 = magnetFerro2 + Magnetizacion**2

	magnetFerro4 = magnetFerro4 + Magnetizacion**4
	
end subroutine CalculationsINequilibrium
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine Write2File ( Energy,Energy2,magnetANTI,magnetAnti2,magnetAnti4,magnetFerro,magnetFerro2,magnetFerro4,promediateCounter,average,TempArray,sizeTemp,Alfa,DstyNpos,folderName,fileOutput)
	
	integer,intent(in)::promediateCounter,average,sizeTemp
	!Write the important values in a file
	real,intent(in)::DstyNpos,Alfa
	real,intent(in)::TempArray(sizeTemp)
	character(len=100),intent(in):: folderName,FileOutput
	!integer,intent(in)::time
	real(dp2),intent(in)::magnetANTI(sizeTemp),magnetAnti2(sizeTemp),magnetAnti4(sizeTemp),magnetFerro(sizeTemp),magnetFerro2(sizeTemp),magnetFerro4(sizeTemp)
	real(dp2),intent(in)::Energy(sizeTemp),Energy2(sizeTemp)

	integer::j
	real::SusceptAnti,cumulantAnti,SusceptFerro,cumulantFerro,beta,C_v
	!Promediate the values
	!call prom1d(EnergyAver,average,AverEnergy)
	!call prom1d(EnergyAver2,average,AverEnergy2)
	!call prom1d(MagnetizationAver,average,AverMagnet)
	!call prom1d(MagnetizationAver2,average,AverMagnet2)
	!call prom1d(MagnetizationAver4,average,AverMagnet4)
	open(unit=1,file=trim(folderName)//'/'//trim(fileOutput),access="append",action="write")
	do j = 1,sizeTemp
			beta = 1/real(TempArray(j))
			!MagnetAnti
			SusceptAnti = beta*(magnetAnti2(j)/real(promediateCounter*average) - (magnetANTI(j)/real(promediateCounter*average))**2 )
			cumulantAnti = 1 - ( magnetAnti4(j)/real(promediateCounter*average) / real( 3*( (magnetAnti2(j)/real(promediateCounter*average))**2) ) )
			!MagnetFerro
			SusceptFerro = beta*(magnetFerro2(j)/real(promediateCounter*average) - (magnetFerro(j)/real(promediateCounter*average))**2 )
			cumulantFerro = 1 - ( magnetFerro4(j)/real(promediateCounter*average) / real( 3*( (magnetFerro2(j)/real(promediateCounter*average))**2) ) )
			!C_v
			C_v = (beta**2)*( Energy2(j)/real(promediateCounter*average) - (Energy(j)/real(promediateCounter*average))**2)
			
			write(1,*) Alfa,',',DstyNpos,',',TempArray(j),',',Energy(j)/real(promediateCounter*average),',',C_v,',',magnetANTI(j)/real(promediateCounter*average),',',SusceptAnti,',',cumulantAnti,&
				',',magnetFerro(j)/real(promediateCounter*average),',',SusceptFerro,',',cumulantFerro
	end do
	close(1)
end subroutine Write2File
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine WriteSpinConf(matriz,nrows,ncols)
	
	integer,intent(in)::ncols,nrows
	integer, intent(in)::matriz(ncols,nrows)
	
	integer::index1,index2

	do index1=1,nrows
		do index2=1,ncols
			write(*,*) index2,index1,matriz(index2,index1)
		end do
	end do
end subroutine WriteSpinConf
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine StripSpaces(string)
    character(len=*) :: string
    integer :: stringLen 
    integer :: last, actual

    stringLen = len (string)
    last = 1
    actual = 1

    do while (actual < stringLen)
        if (string(last:last) == ' ') then
            actual = actual + 1
            string(last:last) = string(actual:actual)
            string(actual:actual) = ' '
        else
            last = last + 1
            if (actual < last) &
                actual = last
        endif
    end do
end subroutine StripSpaces
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module subroutines
