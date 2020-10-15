module subroutines
use rm
implicit none
integer, parameter :: dp2 = selected_real_kind(15, 307) !For double precision
contains


 !_______RANDOM NUMBER GENERATING FUNCTION______!

 double precision function ran1(idum)
 implicit none
 double precision :: r(97)
 integer, intent(IN) :: idum
 save
 integer, parameter :: M1=259200,IA1=7141,IC1=54773
 real, parameter :: RM1=1.0d0/M1
 integer, parameter :: M2=134456,IA2=8121,IC2=28411
 real, parameter :: RM2=1.0d0/M2
 integer, parameter :: M3=243000,IA3=4561,IC3=51349
 integer :: IX1, IX2, IX3, jjj
 integer :: iff=0
 if (idum < 0 .or. iff == 0) then
 iff = 1
 IX1 = mod(IC1-idum,M1)
 IX1 = mod(IA1*IX1+IC1,M1)
 IX2 = mod(IX1,M2)
 IX1 = mod(IA1*IX1+IC1,M1)
 IX3 = mod(IX1,M3)
 do jjj = 1,97
 IX1 = mod(IA1*IX1+IC1,M1)
 IX2 = mod(IA2*IX2+IC2,M2)
 r(jjj) = (dfloat(IX1)+dfloat(IX2)*RM2)*RM1
 end do
 end if
 IX1 = mod(IA1*IX1+IC1,M1)
 IX2 = mod(IA2*IX2+IC2,M2)
 IX3 = mod(IA3*IX3+IC3,M3)
 jjj = 1+(97*IX3)/M3
 if (jjj > 97 .or. jjj < 1)  read( *, * ) 
 ran1 = r(jjj)
 r(jjj) = (dfloat(IX1)+dfloat(IX2)*RM2)*RM1
 end function ran1



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
subroutine ini_matriz(matriz,nrows,ncols,iniseed,average)
	
	!Initialize a square matrix of dimention nrowsXncols 
	!with ones, minues ones and zeros
	!that means spin-ups, spin dows and holes
	integer,intent(in)::nrows,ncols,iniseed,average
	integer,intent(out)::matriz(ncols,nrows)

	!Index to loop the matrix
	integer::i,j
	real::rvec(1)
	
	do i=1,ncols
		do j = 1,nrows
			!call ranmar(rvec,1,iniseed*average)
			!Initial seed set to iniseed*average so that the programs
			!runs with a different initial configuration depending 
			!on the average value
			!if (rvec(1) < npos) then
				matriz(i,j) = 1
			!else !if (rvec(1) < (npos+nneg)) then
				!matriz (i,j) = -1
			!else
			!	matriz(i,j) = 0
			!end if
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

	!call ranmar(rvec,2,iniseed*average)
	posx = nint( (ncols-1) * ran1(5) ) + 1
	posy = nint( (nrows-1) * ran1(5) ) + 1

end subroutine elegir_sitio_SpinFlip
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine CalcEnergy(nrows,ncols,posx,posy,matriz,alpha,Delta_E,ip,im)
	!Calculate Delta for a given point (posx,posy)
	!with periodic boundary conditions
	integer,intent(in)::nrows,ncols
	integer,intent(in)::posx,posy,matriz(ncols,nrows),ip(nrows),im(nrows)
	real(dp2),intent(out)::Delta_E
	real,intent(in)::alpha(3,3)
	
	!integer::J = 1
	
	Delta_E = - matriz(posx,posy)*( matriz(ip(posx),posy) * alpha(matriz(posx,posy)+2 , matriz(ip(posx),posy)+2) &
		+ matriz(im(posx),posy) * alpha(matriz(posx,posy)+2 , matriz(im(posx),posy)+2) &
	    + matriz(posx,ip(posy)) * alpha(matriz(posx,posy)+2 , matriz(posx,ip(posy))+2) &
	    + matriz(posx,im(posy)) * alpha(matriz(posx,posy)+2 , matriz(posx,im(posy))+2) )	

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
subroutine CalcDeltaEnergySpinFlip(nrows,ncols,posx,posy,matriz,Delta_E,ip,im,alpha)

	integer,intent(in)::nrows,ncols
	integer,intent(in)::posx,posy,matriz(ncols,nrows),ip(nrows),im(nrows)
	real(dp2),intent(out)::Delta_E
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
subroutine calculationsInEquilibrium(nrows,ncols,matriz,alpha,ip,im,temp,& 
	EnergyAver,EnergyAver2,averIndex,extMagField,magnetANTI,magnetAnti2,magnetAnti4,magnetFerro,magnetFerro2,magnetFerro4)

	integer,intent(in)::nrows,ncols,matriz(nrows,ncols),ip(nrows),im(nrows),averIndex
	real,intent(in)::alpha(3,3)
	real,intent(in)::temp,extMagField
	integer::i,j
	integer::dim,black,white,pos,neg
	integer::npp,nmm,nzz
	!real::beta
	real(dp2)::Energy,magnetBlack,magnetWhite,antiMagnet,magnetPos,magnetNeg,Magnetizacion,instE
	real(dp2),intent(inout)::EnergyAver,EnergyAver2,magnetANTI,magnetFerro,magnetFerro2,magnetAnti2,magnetAnti4,magnetFerro4

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
	instE = 0.0

	do i = 1, ncols
		do j = 1, nrows
			call CalcEnergy(nrows,ncols,i,j,matriz,alpha,instE,ip,im)
			!Energy = Energy - matriz(i,j)*(matriz(ip(i),j)+matriz(im(i),j)+matriz(i,ip(j))+matriz(i,im(j)))
			Energy = Energy + instE
			!print *, 'instE: ',instE, 'Energia: ',Energy
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
	!print *, 'Energia: ',Energy, 'Dividido: ',Energy/(dim*2.0)
	Energy = Energy/(dim*2.0)
	!Se divide por dos nuevamente para no contar dos veces interacciones entre spines
	!<E>
	print *, 'Energy: ',Energy, 'Energy2: ',Energy**2!, 'EnergyAver: ',EnergyAver
	EnergyAver = EnergyAver + Energy
	!print*, 'EnergyAver: ',EnergyAver

	!print *, 'Energy2: ',EnergyAver2, 'Energy: ', Energy, 'Energy**2:', Energy**2
	EnergyAver2 = EnergyAver2 + Energy**2 
	!print *, 'EnergyAver2: ',EnergyAver2
	!------------------------------------------------
	Magnetizacion = sum(matriz(1:ncols,1:nrows))/real(dim*1.0)
	!<m>F
	magnetFerro = magnetFerro + Magnetizacion

	magnetFerro2 = magnetFerro2 + Magnetizacion**2

	magnetFerro4 = magnetFerro4 + Magnetizacion**4
	
end subroutine calculationsInEquilibrium
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
	real(dp2)::SusceptAnti,cumulantAnti,SusceptFerro,cumulantFerro,beta,C_v
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
			C_v = (beta**2)*( Energy2(j)/real(promediateCounter*average) - (Energy(j)/real(promediateCounter*average))**2 )
			!print *, 'beta: ',beta,' energy2: ',Energy2(j),' energy: ',Energy(j),' promediateCounter: ',promediateCounter, ' average: ',average
			!print *, 'C_v: ',C_v
			write(1,*) Alfa,',',DstyNpos,',',TempArray(j),',',Energy(j)/real(promediateCounter*average),',',Energy2(j)/real(promediateCounter*average),',',C_v,',',magnetANTI(j)/real(promediateCounter*average),',',SusceptAnti,',',cumulantAnti,&
				',',abs(magnetFerro(j))/real(promediateCounter*average),',',SusceptFerro,',',cumulantFerro
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
