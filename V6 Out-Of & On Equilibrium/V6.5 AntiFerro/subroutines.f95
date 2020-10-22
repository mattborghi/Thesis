module subroutines
use rm
implicit none
integer, parameter :: dp2 = selected_real_kind(15, 307) !For double precision
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine SetToZeroVariables(MagnetizationAver,MagnetizationAver2, &
								MagnetizationAver4,EnergyAver,EnergyAver2,magnetANTI,magnetANTI2,magnetANTI4,magnetFerro,magnetFerro2,magnetFerro4,f1,f2,f3,f4,length)
	
	integer,intent(in)::length
	real(dp2),intent(out)::MagnetizationAver(length),MagnetizationAver2(length),MagnetizationAver4(length),magnetANTI(length),magnetANTI2(length),magnetANTI4(length),magnetFerro(length),magnetFerro2(length),magnetFerro4(length)
	real(dp2),intent(out)::EnergyAver(length),EnergyAver2(length)
	real(dp2),intent(out)::f1(2,length),f2(2,length),f3(2,length),f4(2,length)

		MagnetizationAver(:) = 0.0
		MagnetizationAver2(:) = 0.0
		MagnetizationAver4(:) = 0.0
		EnergyAver(:) = 0.0
		EnergyAver2(:) = 0.0
		magnetANTI(:) = 0.0
		magnetANTI2(:) = 0.0
		magnetANTI4(:) = 0.0
		magnetFerro(:) = 0.0
		magnetFerro2(:) = 0.0
		magnetFerro4(:) = 0.0
		f1(:,:) = 0.0
		f2(:,:) = 0.0
		f3(:,:) = 0.0
		f4(:,:) = 0.0

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

print *,'#pos:',npos*dim,'=',pos,' #neg:',nneg*dim,'=',neg,' #null:',nnull*dim,'=',null
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
subroutine elegir_sitio_SpinFlip(nrows,ncols,posx,posy,iniseed,matriz,average)

	!This subroutine gives a random point in the square matrix of LxL
	integer,intent(in)::nrows,ncols,iniseed,average
	integer,intent(out)::posx,posy
	integer,intent(in)::matriz(ncols,nrows)
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
subroutine NeighboursEnergy(nrows,ncols,posx,posy,matriz,alpha,Delta_E,ip,im)
	!Calculate Delta for a given point (posx,posy)
	!with periodic boundary conditions
	integer,intent(in)::nrows,ncols
	integer,intent(in)::posx,posy,matriz(nrows,ncols),ip(nrows),im(nrows)
	real,intent(in)::alpha(3,3)
	real,intent(out)::Delta_E
	integer::J = 1
		
	Delta_E =-J* ( matriz(ip(posx),posy)*alpha(matriz(posx,posy)+2,matriz(ip(posx),posy)+2) &
	             + matriz(im(posx),posy)*alpha(matriz(posx,posy)+2,matriz(im(posx),posy)+2) &
	             + matriz(posx,ip(posy))*alpha(matriz(posx,posy)+2,matriz(posx,ip(posy))+2) &
	             + matriz(posx,im(posy))*alpha(matriz(posx,posy)+2,matriz(posx,im(posy))+2) )	

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

end subroutine NeighboursEnergy
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
subroutine CalcDeltaEnergySpinFlip(nrows,ncols,posx,posy,matriz,alpha,Delta_E,ip,im,iniseed,extMagField)
	!Calculate Delta for a given point (posx,posy)
	!with periodic boundary conditions
	integer,intent(in)::nrows,ncols,iniseed
	integer,intent(in)::posx,posy,matriz(nrows,ncols),ip(nrows),im(nrows)
	integer::newmatriz
	real,intent(in)::alpha(3,3),extMagField
	real,intent(out)::Delta_E
	real::Delta_E2,Delta_E3,rvec(1)
	integer::J = 1
	call ranmar(rvec,1,iniseed)
	!Calculate the energy of the OLD state
	call CalcEnergy(nrows,ncols,posx,posy,matriz,alpha,Delta_E,ip,im,extMagField)

	!Energy of new state Efin = -Eini
	! --> DeltaE = Efin - Eini = -Eini - Eini = -2Eini
	Delta_E = -2*Delta_E

end subroutine CalcDeltaEnergySpinFlip
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine CalculationsINequilibrium(nrows,ncols,matriz,alpha,Delta_E,ip,im,temp,& 
	MagnetizationAver,MagnetizationAver2,MagnetizationAver4,&
	EnergyAver,EnergyAver2,averIndex,extMagField,magnetANTI,magnetANTI2,magnetANTI4,magnetFerro,magnetFerro2,magnetFerro4)

	integer,intent(in)::nrows,ncols,matriz(nrows,ncols),ip(nrows),im(nrows),averIndex
	real,intent(in)::alpha(3,3)
	real,intent(in)::temp,extMagField
	real,intent(out)::Delta_E
	integer::i,j
	integer::dim,black,white,pos,neg
	integer::npp,nmm,nzz
	!real::beta
	real(dp2)::Energy,Magnetization,magnetBlack,magnetWhite,antiMagnet,magnetPos,magnetNeg,ferroMagnet
	real(dp2),intent(out)::EnergyAver,EnergyAver2,MagnetizationAver,MagnetizationAver2, MagnetizationAver4,magnetANTI,magnetANTI2,magnetANTI4,magnetFerro,magnetFerro2,magnetFerro4

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
	ferroMagnet = 0.0

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
			!Calculate Ferro Quantities-----------
			call HowManySpins(matriz,ncols,nrows,ip,im,npp,nmm,nzz,i,j)
			if ( matriz(i,j) == 1 ) then
				pos = pos + 1
				if (npp >= 2) then
					magnetPos = magnetPos + matriz(i,j)
				end if
			else  !matriz(i,j) == -1
				neg = neg + 1
				if (nmm >= 2) then
					magnetNeg = magnetNeg + matriz(i,j)
				end if
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
	!<mANTI2>
	magnetANTI2 = magnetANTI2 + antiMagnet**2 
	!<mANTI4>
	magnetANTI4 = magnetANTI4 + antiMagnet**4
	!------Ferro quantities ------------------------
	!<m>pos
	magnetPos = magnetPos / real(pos)
	!<m>neg
	magnetNeg = magnetNeg / real(neg)
	!inter 
	ferroMagnet = abs(magnetPos - magnetNeg) / real(2)
	!<m>Ferro
	magnetFerro = magnetFerro + ferroMagnet
	!<m2>Ferro
	magnetFerro2 = magnetFerro2 + ferroMagnet**2 
	!<m4>ferro
	magnetFerro4 = magnetFerro4 + ferroMagnet**4
	!------------------------------------------------
	! E 
	Energy = Energy/(dim*2.0)
	!Se divide por dos nuevamente para no contar dos veces interacciones entre spines
	!<E>
	EnergyAver = EnergyAver + Energy
	!<E2>
	EnergyAver2 = EnergyAver2 + Energy**2
	
	Magnetization = sum(matriz(1:nrows,1:ncols))/(dim*1.0)
	
	!<m>
	MagnetizationAver = MagnetizationAver + Magnetization
	!<m2>
	MagnetizationAver2 = MagnetizationAver2 + Magnetization**2
	!<m4>
	MagnetizationAver4 = MagnetizationAver4 +  Magnetization**4

end subroutine CalculationsINequilibrium
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine Write2File ( beta, Magnetization,Magnetization2, &
					Magnetization4,Energy,Energy2,magnetANTI,magnetANTI2,magnetANTI4,magnetFerro,magnetFerro2,magnetFerro4,npoints,average )
	
	integer,intent(in)::npoints,average
	!Write the important values in a file
	real,intent(in)::beta
	!integer,intent(in)::time
	real(dp2),intent(in)::Magnetization(npoints), Magnetization2(npoints),Magnetization4(npoints),magnetANTI(npoints),magnetANTI2(npoints),magnetANTI4(npoints),magnetFerro(npoints),magnetFerro2(npoints),magnetFerro4(npoints)
	real(dp2),intent(in)::Energy(npoints),Energy2(npoints)

	real(dp2):: Cumulant,Suceptibility,C_v,ANTIcumulant,ANTIsuscept,FerroCumulant,FerroSuscept
	integer::j
	!Promediate the values
	!call prom1d(EnergyAver,average,AverEnergy)
	!call prom1d(EnergyAver2,average,AverEnergy2)
	!call prom1d(MagnetizationAver,average,AverMagnet)
	!call prom1d(MagnetizationAver2,average,AverMagnet2)
	!call prom1d(MagnetizationAver4,average,AverMagnet4)

	do j = 1,npoints
		Suceptibility = beta*( (Magnetization2(j)/real(average)) - (Magnetization(j)/real(average))**2 )
		Cumulant = 1 - ( (Magnetization4(j)/real(average)) / ( 3*( (Magnetization2(j)/real(average))**2) ) )
		C_v = (beta**2)*( (Energy2(j)/real(average)) - (Energy(j)/real(average))**2)
		ANTIsuscept = beta*( (magnetANTI2(j)/real(average)) - (magnetANTI(j)/real(average))**2 )
		ANTIcumulant = 1 - ( (magnetANTI4(j)/real(average)) / ( 3*( (magnetANTI2(j)/real(average))**2) ) )
		FerroSuscept = beta*( (magnetFerro2(j)/real(average)) - (magnetFerro(j)/real(average))**2 )
		FerroCumulant = 1 - ( (magnetFerro4(j)/real(average)) / ( 3*( (magnetFerro2(j)/real(average))**2) ) )

			write(1,*) j,",",abs(Magnetization(j)/real(average)),",",Magnetization2(j)/real(average),",",Suceptibility,",",Cumulant,",",magnetANTI(j)/real(average),",",magnetANTI2(j)/real(average),",",magnetANTI4(j)/real(average),",",ANTIsuscept,",",ANTIcumulant,&
						",",magnetFerro(j)/real(average),",",magnetFerro2(j)/real(average),",",magnetFerro4(j)/real(average),",",FerroSuscept,",",FerroCumulant
			write(33,*) j,",", Energy(j)/real(average),",", Energy2(j)/real(average),",",C_v
	end do
end subroutine Write2File
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine prom1d(array,dim_array,prome)

integer,intent(in)::dim_array		!dimension de la variable a promediar,cant de veces que se "tira" la particula
real(dp2),intent(in)::array(dim_array)				!variable a promediar
real(dp2), intent(out)::prome		!resultado del promedio
real(dp2)::s
integer::i
!veamos primero la sumatoria
s=0
do i=1,dim_array
	s=s+array(i)
end do

prome=(s/dim_array)

end subroutine prom1d
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine prom1dp(array,dim_array,prome)

integer,intent(in)::dim_array		!dimension de la variable a promediar,cant de veces que se "tira" la particula
real,intent(in)::array(dim_array)				!variable a promediar
real, intent(out)::prome		!resultado del promedio
real::s
integer::i
!veamos primero la sumatoria
s=0
do i=1,dim_array
	s=s+array(i)
end do

prome=(s/dim_array)

end subroutine prom1dp
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine prom1dReal2Int(array,dim_array,prome)

integer,intent(in)::dim_array		!dimension de la variable a promediar,cant de veces que se "tira" la particula
integer,intent(in)::array(dim_array)				!variable a promediar
real, intent(out)::prome		!resultado del promedio
real::s
integer::i
!veamos primero la sumatoria
s=0
do i=1,dim_array
	s=s+array(i)
end do

prome=(s/dim_array)

end subroutine prom1dReal2Int
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine New_Temp(indexTemp,currentTemp,Tmax,Tmin,TempInterval)
	
	integer,intent(in)::indexTemp
	real,intent(in)::TempInterval,Tmax,Tmin
	real,intent(out)::currentTemp

	!From Tmax to Tmin in TempInterval steps
	!IndexTemp goes from 1 to NumofTempPoints...
	!.. at IndexTemp = 1 currentTemp = Tmax
	!.. at IndexTemp = NumofTempPoints currentTemp = Tmin 
	currentTemp = Tmax - TempInterval*( indexTemp - 1 )
 
end subroutine New_Temp
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine HowManySpins(matriz,ncols,nrows,ip,im,npp,nmm,nzz,index1,index2)
	!This subroutine counts the number of positive,negative and zero spins around a fixed point (index1,index2)
	!The result is stored in the variable npp,nmm and nzz
	!Its values can be 0,1,2,3,4.
	integer,intent(in)::ncols,nrows,index1,index2
	integer,intent(in)::ip(nrows),im(nrows)
	integer,intent(in)::matriz(ncols,nrows)
	integer,intent(out)::npp,nmm,nzz

	npp = 0
	nmm = 0
	nzz = 0

	if (matriz(ip(index1),index2) == 1) then
		npp = npp + 1
	else if(matriz(ip(index1),index2) == -1) then
		nmm = nmm + 1
	else 
		nzz = nzz + 1 
	end if

	if (matriz(im(index1),index2) == 1) then
		npp = npp + 1
	else if(matriz(im(index1),index2) == -1) then
		nmm = nmm + 1
	else 
		nzz = nzz + 1
	end if

	if (matriz(index1,ip(index2)) == 1) then
		npp = npp + 1
	else if(matriz(index1,ip(index2)) == -1) then
		nmm = nmm + 1
	else
		nzz = nzz + 1
	end if

	if (matriz(index1,im(index2)) == 1) then
		npp = npp + 1
	else if(matriz(index1,im(index2)) == -1) then
		nmm = nmm + 1
	else 
		nzz = nzz + 1
	end if

end subroutine HowManySpins
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine CellInteraction(matriz,ncols,nrows,ip,im,averIndex, &
							ef1,ef2,ef3,ef4)
	
	integer,intent(in)::ncols,nrows,averIndex
	!integer,intent(in)::time
	integer,intent(in)::ip(nrows),im(nrows)
	integer,intent(in)::matriz(ncols,nrows)
	real(dp2),intent(out)::ef1(2),ef2(2),ef3(2),ef4(2)
	integer(dp2)::if1(2),if2(2),if3(2),if4(2)
	integer::index1,index2,i,dim
	integer::npp,nmm,nzz

	dim = nrows*ncols

	npp  = 0
	nmm  = 0
	nzz  = 0

	if1(:) = 0
	if2(:) = 0
	if3(:) = 0
	if4(:) = 0
	
	!Loop over each cell
	do index1=1,nrows
		do index2=1,ncols
			call HowManySpins (matriz,ncols,nrows,ip,im,npp,nmm,nzz,index1,index2)
			
			!Is the cell negative, positive or zero?
			if (matriz(index1,index2) == 1) then
				!It is positive. 
				if1(2) = if1(2) + npp
				if (npp == 2) then
					if2(2) = if2(2) + 1
				else if(npp == 3) then
					if2(2) = if2(2) + 3
					if3(2) = if3(2) + 1
				else if (npp == 4) then
					if2(2) = if2(2) + 6
					if3(2) = if3(2) + 4
					if4(2) = if4(2) + 1
				end if
			else !if(matriz(index1,index2) == -1) then
				!It is negative. Are the neighbours also negative?
				!f1 
				if1(1) = if1(1) + nmm
				if (nmm == 2) then
					if2(1) = if2(1) + 1
				else if(nmm == 3) then
					if2(1) = if2(1) + 3
					if3(1) = if3(1) + 1
				else if (nmm == 4) then
					if2(1) = if2(1) + 6
					if3(1) = if3(1) + 4
					if4(1) = if4(1) + 1
				end if
			!else
				!It is zero.  
			end if
		end do
	end do	

	do i = 1,2 
		ef1(i) = ef1(i) + if1(i)/(4.0*dim)
		ef2(i) = ef2(i) + if2(i)/(6.0*dim)
		ef3(i) = ef3(i) + if3(i)/(4.0*dim)
		ef4(i) = ef4(i) + if4(i)/(1.0*dim)
	end do
end subroutine CellInteraction
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine WriteInteractions(IDfile,f1,f2,f3,f4,nrows,ncols,npoints,average,contador)

integer,intent(in)::ncols,nrows,npoints,IDfile,average
real(dp2),intent(in)::f1(2,npoints),f2(2,npoints),f3(2,npoints),f4(2,npoints)
integer(dp2),intent(in)::contador(average)
integer::j

do j = 1,npoints
	!Write to the file				
	write(IDfile,*) contador(j),",",j,",",f1(2,j)/average,",",f1(1,j)/average,",",f2(2,j)/average,",",&
			f2(1,j)/average,",",f3(2,j)/average,",",f3(1,j)/average,",",f4(2,j)/average,",",f4(1,j)/average
end do

end subroutine WriteInteractions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module subroutines
