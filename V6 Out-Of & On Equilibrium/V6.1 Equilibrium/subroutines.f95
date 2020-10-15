module subroutines
use rm
implicit none
integer, parameter :: dp2 = selected_real_kind(15, 307) !For double precision
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine SetToZeroVariables(MagnetizationAver,MagnetizationAver2, &
								MagnetizationAver4,EnergyAver,EnergyAver2,&
								InterPos,InterNeg,InterPN,InterZero,&
								InterPos2,InterNeg2,InterPN2,InterZero2,InterPZ,InterNZ)
	
	real(dp2),intent(out)::MagnetizationAver,MagnetizationAver2,MagnetizationAver4
	real(dp2),intent(out)::EnergyAver,EnergyAver2
	integer(dp2),intent(out)::InterPos,InterNeg,InterPN,InterZero
	integer(dp2),intent(out)::InterPos2,InterNeg2,InterPN2
	integer(dp2),intent(out)::InterZero2,InterPZ,InterNZ

		MagnetizationAver  = 0.0
		MagnetizationAver2 = 0.0
		MagnetizationAver4 = 0.0
		EnergyAver 			= 0.0
		EnergyAver2 		= 0.0
		InterPos2 = 0.0
		InterNeg = 0.0
		InterPN = 0.0
		InterZero = 0.0
		InterPos2 = 0.0
		InterNeg2 = 0.0
		InterPN2 = 0.0
		InterZero2 = 0.0
		InterPZ = 0.0
		InterNZ = 0.0
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
	alpha(3,1) = alpha(1,3)
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
	integer,intent(out)::matriz(nrows,ncols)

	!Index to loop the matrix
	integer::i,j
	real::rvec(1)
	
	do i=1,nrows
		do j = 1,ncols
			call ranmar(rvec,1,iniseed*average)
			!Initial seed set to iniseed* so that the programs
			!runs with a different initial configuration depending 
			!on the  value
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
	integer,intent(in)::matriz(nrows,ncols)
	integer::i
	real::rvec(2)

	call ranmar(rvec,2,iniseed*average)
	posx = floor( nrows * rvec(1) ) + 1
	posy = floor( ncols * rvec(2) ) + 1

end subroutine elegir_sitio_SpinFlip
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine elegir_sitio_Kawasaki(nrows,ncols,posx,posy,iniseed,probpos,probneg,matriz,average)
	
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
	else if (rvec(1) < (probpos +probneg)) then
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
	integer,intent(in)::posx,posy,matriz(nrows,ncols),ip(nrows),im(nrows)
	real,intent(out)::Delta_E
	real,intent(in)::alpha(3,3),extMagField
	
	integer::J = 1
	
	Delta_E =-J*matriz(posx,posy)* &
		( matriz(ip(posx),posy)*alpha(matriz(posx,posy)+2,matriz(ip(posx),posy)+2) &
		+ matriz(im(posx),posy)*alpha(matriz(posx,posy)+2,matriz(im(posx),posy)+2) &
	    + matriz(posx,ip(posy))*alpha(matriz(posx,posy)+2,matriz(posx,ip(posy))+2) &
	    + matriz(posx,im(posy))*alpha(matriz(posx,posy)+2,matriz(posx,im(posy))+2) ) &
	    - extMagField*matriz(posx,posy)	

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
	integer,intent(in)::posx,posy,matriz(nrows,ncols),ip(nrows),im(nrows)
	integer,intent(in)::newposx,newposy
	real,intent(in)::alpha(3,3),extMagField
	real,intent(out)::Delta_E
	real::Delta_E2,Delta_E3,rvec(1)
	integer::J = 1
	
	!Calculate the energy of the OLD state
	call CalcEnergy(nrows,ncols,posx,posy,matriz,alpha,Delta_E,ip,im,extMagField)
	
	!Calculate the energy of a random NEIGHBOUR state 
	!print*,posx,posy,newposx,newposy
	call CalcEnergy(nrows,ncols,newposx,newposy,matriz,alpha,Delta_E2,ip,im,extMagField) 
	Delta_E = Delta_E + Delta_E2 + J*matriz(posx,posy)*matriz(newposx,newposy)*alpha(matriz(posx,posy)+2,matriz(newposx,newposy)+2)
	!Just (remove)add the term that it was counted twice

	!Calculate the energy of the NEW state
	call NeighboursEnergy(nrows,ncols,posx,posy,matriz,alpha,Delta_E3,ip,im)
	Delta_E3 = (Delta_E3 - extMagField)*matriz(newposx,newposy) 
	call NeighboursEnergy(nrows,ncols,newposx,newposy,matriz,alpha,Delta_E2,ip,im)
	Delta_E2 = (Delta_E2 - extMagField)*matriz(posx,posy)
	!Finally..
	Delta_E2 = Delta_E2 + Delta_E3 + J*matriz(posx,posy)*matriz(newposx,newposy)*alpha(matriz(posx,posy)+2,matriz(newposx,posy)+2)  
	!DeltaE = E_NEW - E_OLD
	!print*,"Btw ",matriz(posx,posy),"and",matriz(newposx,newposy)
	!print*,Delta_E2,Delta_E,Delta_E2 - Delta_E
	Delta_E = Delta_E2 - Delta_E 

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
subroutine CalculationsINequilibrium(nrows,ncols,matriz,alpha,Delta_E,ip,im,& 
	MagnetizationAver,MagnetizationAver2,MagnetizationAver4,&
	EnergyAver,EnergyAver2,extMagField)

	integer,intent(in)::nrows,ncols,matriz(nrows,ncols),ip(nrows),im(nrows)
	real,intent(in)::alpha(3,3)
	real,intent(in)::extMagField
	real,intent(out)::Delta_E
	integer::i,j
	integer::dim
	!real::beta
	real(dp2)::Energy,Magnetization
	real(dp2),intent(inout)::EnergyAver,EnergyAver2,MagnetizationAver,&
		MagnetizationAver2, MagnetizationAver4

	!beta = 1.0/temp

	dim = nrows*ncols
	Energy = 0.0
	do i = 1, nrows
		do j = 1, ncols
			call CalcEnergy(nrows,ncols,i,j,matriz,alpha,Delta_E,ip,im,extMagField)
			Energy = Energy + Delta_E
		end do
	end do
	Energy = Energy/(dim*2.0)

	!<E> 
	EnergyAver = EnergyAver + Energy
	!Se divide por dos nuevamente para no contar dos veces interacciones entre spines
	!<E2>
	EnergyAver2 = EnergyAver2 + Energy**2
	
	Magnetization = sum(matriz(1:nrows,1:ncols))/(dim*1.0)
	!<m>
	MagnetizationAver = MagnetizationAver + Magnetization
	!<m2>
	MagnetizationAver2 = MagnetizationAver2 + Magnetization**2
	!<m4>
	MagnetizationAver4 = MagnetizationAver4 + Magnetization**4

end subroutine CalculationsINequilibrium
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
subroutine Write2File ( currentTemp, Magnetization,Magnetization2, &
					Magnetization4,Energy,Energy2,npoints,averIndex)
	
	integer,intent(in)::npoints,averIndex
	!Write the important values in a file
	real,intent(in)::currentTemp
	!integer,intent(in)::time
	real(dp2),intent(inout)::Magnetization, Magnetization2,Magnetization4
	real(dp2),intent(inout)::Energy,Energy2
	real(dp2):: Cumulant,Suceptibility,C_v
	!real(dp2)::EnergyAver,EnergyAver2,MagnetizationAver,MagnetizationAver2,MagnetizationAver4
	real::beta
	!integer::j
	!real::dim
	
	beta = 1/currentTemp

	Suceptibility = beta*( (Magnetization2/npoints) - (Magnetization/npoints)**2 )
	Cumulant = 1 - ( (Magnetization4/npoints) / ( 3*( (Magnetization2/npoints)**2) ) )
	C_v = (beta**2)*( (Energy2/npoints) - (Energy/npoints)**2)
	
	write(33,*) currentTemp,",",Energy/npoints,",",Energy2/npoints,",",C_v,",",averIndex
	write(1,*) currentTemp,",",abs(Magnetization/npoints),",", Magnetization2/npoints,",", Suceptibility,"," , Cumulant,",",averIndex

end subroutine Write2File
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
subroutine NewTemp(indexTemp,currentTemp,Tmax,Tmin,NumofTempPoints)
	
	integer,intent(in)::indexTemp,NumofTempPoints
	real,intent(in)::Tmax,Tmin
	real,intent(out)::currentTemp

	!From Tmax to Tmin in TempInterval steps
	!IndexTemp goes from 1 to NumofTempPoints...
	!.. at IndexTemp = 1 currentTemp = Tmax
	!.. at IndexTemp = NumofTempPoints currentTemp = Tmin 
	if (NumofTempPoints /= 1) then
		currentTemp = Tmax - indexTemp*(Tmax-Tmin)/(NumofTempPoints-1)
	else 
		currentTemp = Tmax
	end if
 
end subroutine NewTemp
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine WriteSpinConf(matriz,nrows,ncols)
	
	integer,intent(in)::ncols,nrows
	integer, intent(in)::matriz(nrows,ncols)
	
	integer::index1,index2

	do index1=1,nrows
		do index2=1,ncols
			write(*,*) index1,index2,matriz(index1,index2)
		end do
	end do
end subroutine WriteSpinConf
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine CellInteraction(matriz,ncols,nrows,ip,im,averIndex, &
							interPos,interNeg,interPN,interZ)
	
	integer,intent(in)::ncols,nrows,averIndex
	!integer,intent(in)::time
	integer,intent(in)::ip(nrows),im(nrows)
	integer,intent(in)::matriz(ncols,nrows)
	!I am passing the function just the elements of the array. NOT the whole array
	integer(dp2),intent(out)::interPos,interNeg,interPN,interZ
	integer(dp2)::iPos,iNeg,iMezcla,iZero
	integer::index1,index2

	iPos 	= 0
	iNeg 	= 0
	iZero 	= 0
	iMezcla = 0	
	
	!Loop over each cell
	do index1=1,nrows
		do index2=1,ncols
			!Is the cell negative,positive or zero?
			if (matriz(index1,index2) == 1) then
				!It is positive. Are the neighbours also positive?
				if ( matriz(ip(index1),index2) == 1 .AND. matriz(im(index1),index2) == 1 .AND. matriz(index1,ip(index2)) == 1 &
					.AND. matriz(index1,im(index2)) == 1 ) then
					iPos = iPos + 1
				else
					iMezcla = iMezcla + 1
				end if 
			else if (matriz(index1,index2) == -1) then
				!It is negative. Are the neighbours also negative?
				if ( matriz(ip(index1),index2) == -1 .AND. matriz(im(index1),index2) == -1 .AND. matriz(index1,ip(index2)) == -1 &
					.AND. matriz(index1,im(index2)) == -1 ) then
					iNeg = iNeg + 1
				else
					iMezcla = iMezcla + 1
				end if
			else 
				!It is zero.
				if ( matriz(ip(index1),index2) == 0 .AND. matriz(im(index1),index2) == 0 .AND. matriz(index1,ip(index2)) == 0 &
					.AND. matriz(index1,im(index2)) == 0 ) then
					iZero = iZero + 1
				else
					iMezcla = iMezcla + 1
				end if
			end if
		end do
	end do	
	
	interPos = iPos
	interNeg = iNeg
	interPN  = iMezcla
	interZ = iZero

end subroutine CellInteraction
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
subroutine CellInteractionV2(matriz,ncols,nrows,ip,im,averIndex, &
							interPos2,interNeg2,interPN2,interZ2,interPZ,interNZ)
	
	integer,intent(in)::ncols,nrows,averIndex
	!integer,intent(in)::time
	integer,intent(in)::ip(nrows),im(nrows)
	integer,intent(in)::matriz(ncols,nrows)
	integer(dp2),intent(out)::interPos2,interNeg2,interPN2,interZ2,interPZ,interNZ
	integer(dp2)::iPos,iNeg,iPN,iPZ,iNZ,iZero
	integer::index1,index2
	integer::npp,nmm,nzz

	npp  = 0
	nmm  = 0
	nzz  = 0

	iPos = 0 !Counts the number of interactions ++
	iNeg = 0 !Counts the number of interactions --
	iPN  = 0 !Counts the number of interactions +-
	iPZ  = 0 !Counts the number of interactions +0
	iNZ  = 0 !Counts the number of interactions -0
	iZero = 0!Counts the number of interactions 00
	
	!Loop over each cell
	do index1=1,nrows
		do index2=1,ncols
			call HowManySpins (matriz,ncols,nrows,ip,im,npp,nmm,nzz,index1,index2)
			!Is the cell negative, positive or zero?
			if (matriz(index1,index2) == 1) then
				!It is positive. 
				iPos = iPos + npp
				iPN = iPN + nmm
				iPZ = iPZ + nzz
			else if(matriz(index1,index2) == -1) then
				!It is negative. Are the neighbours also negative?
				iPN = iPN + npp
				iNeg = iNeg + nmm
				iNZ = iNZ + nzz
			else
				!It is zero. 
				iZero = iZero + nzz
				iNZ = iNZ + nmm
				iPZ = iPZ + npp 
			end if
		end do
	end do	

	interPos2 = iPos
	interNeg2 = iNeg
	interPN2  = iPN
	interZ2 = iZero
	interPZ = iPZ
	interNZ = iNZ
end subroutine CellInteractionV2
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine WriteInteractions(IDfile,interPos,interNeg,interPN,interZero,npos,nrows,ncols,npoints,currentTemp)

integer,intent(in)::ncols,nrows,npoints,IDfile
real, parameter::pi = 3.1415927
real,intent(in)::npos,currentTemp
integer(dp2),intent(in)::interPos,interNeg,interPN,interZero
real(dp2)::rIdeal,rMeasured,rho

rIdeal    = nrows*ncols*sqrt(npos/pi) 
rMeasured = InterPN/npoints / (2*pi)
rho       = 1.0 - abs(rIdeal - rMeasured)/rIdeal

!Write to the file				
write(IDfile,*) currentTemp,interPos/npoints,interNeg/npoints,interPN/npoints,interZero/npoints ,rIdeal, rMeasured, rho

end subroutine WriteInteractions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine WriteInteractionsV2(IDfile,interPos,interNeg,interPN,interZero,interPZ,interNZ,nrows,ncols,npoints,currentTemp)

integer,intent(in)::ncols,nrows,npoints,IDfile
real,intent(in)::currentTemp
integer(dp2),intent(in)::interPos,interNeg,interPN,interZero,interPZ,interNZ
			
write(IDfile,*) currentTemp,interPos/npoints,interNeg/npoints,interPN/npoints,interZero/npoints,interPZ/npoints,interNZ/npoints


end subroutine WriteInteractionsV2

end module subroutines
