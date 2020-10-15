module subroutines
use rm
implicit none
integer, parameter :: dp2 = selected_real_kind(15, 307) !For doble precision
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine SetToZeroVariables(MagnetizationAver,MagnetizationAver2,MagnetizationAver4,EnergyAver,EnergyAver2,length)

	integer,intent(in)::length
	real(dp2),intent(out)::MagnetizationAver(length),MagnetizationAver2(length),MagnetizationAver4(length)
	real(dp2),intent(out)::EnergyAver(length),EnergyAver2(length)

	MagnetizationAver(:) = 0.0
	MagnetizationAver2(:) = 0.0
	MagnetizationAver4(:) = 0.0
	EnergyAver(:) = 0.0
	EnergyAver2(:) = 0.0

	!print *,'Variables a cero'

end subroutine SetToZeroVariables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine Inicializacion_DeltaE(DeltaE,Temp)
	
	real,intent(in)::Temp
	real,intent(out)::DeltaE(20)

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine ini_matriz(matriz,nrows,ncols,nneg,iniseed,averageIndex)
	
	!Initialize a square matrix of dimention nrowsXncols 
	!with ones
	!that means all spin-ups
	integer,intent(in)::nrows,ncols,nneg,iniseed,averageIndex
	integer,intent(out)::matriz(ncols,nrows)
	!Index to loop the matrix
	integer::i,j,k

	!Initialize all spins up
	
	do i=1,ncols
		do j = 1,nrows
			matriz(i,j) = 1	
		end do
	end do

	!Now increment the number of nneg
	k = nneg
	!If k=0 then m=1
	!If k=1 o
	do while (k /= 0) 	
		!Return a random position in matrix and flip it if its not already a down spin
		call elegir_sitio(nrows,ncols,i,j,iniseed,averageIndex)
		if (matriz(i,j) == 1) then
			matriz(i,j) = -1
			k = k - 1
		end if 
	end do	

end subroutine ini_matriz
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine elegir_sitio(nrows,ncols,posx,posy,iniseed,average)
	
	!This subroutine gives a random point in the square matrix of LxL
	integer,intent(in)::nrows,ncols,iniseed,average
	integer,intent(out)::posx,posy

	real::rvec(2)
	call ranmar(rvec,2,iniseed*average)

	!Given values between 0-1 for rvec we want values between 1-nrows/ncols for each coordinate 
	!So this lines map one values to others 0-1 -> 1-nrows/ncols
	posx = floor( ncols * rvec(1) ) +1
	posy = floor( nrows * rvec(2) ) +1

end subroutine elegir_sitio
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine CalcDeltaEnergy(nrows,ncols,posx,posy,matriz,alpha,Delta_E,ip,im,currentField)

	integer,intent(in)::nrows,ncols
	integer,intent(in)::posx,posy,matriz(ncols,nrows),ip(nrows),im(nrows)
	real,intent(out)::Delta_E
	real,intent(in)::alpha(3,3),currentField
	integer::J = 1
	integer::matrizInv
	matrizInv = -matriz(posx,posy)
	!Delta_E =2*J*matriz(posx,posy) * ( matriz(ip(posx),posy) + matriz(im(posx),posy) + matriz(posx,ip(posy)) + matriz(posx,im(posy)))

	Delta_E = -J*matrizInv* &
		( matriz(ip(posx),posy)*alpha(matrizInv+2,matriz(ip(posx),posy)+2) &
		+ matriz(im(posx),posy)*alpha(matrizInv+2,matriz(im(posx),posy)+2) &
	    + matriz(posx,ip(posy))*alpha(matrizInv+2,matriz(posx,ip(posy))+2) &
	    + matriz(posx,im(posy))*alpha(matrizInv+2,matriz(posx,im(posy))+2) ) &
		+ J*matriz(posx,posy)* &
		( matriz(ip(posx),posy)*alpha(matriz(posx,posy)+2,matriz(ip(posx),posy)+2) &
		+ matriz(im(posx),posy)*alpha(matriz(posx,posy)+2,matriz(im(posx),posy)+2) &
	    + matriz(posx,ip(posy))*alpha(matriz(posx,posy)+2,matriz(posx,ip(posy))+2) &
	    + matriz(posx,im(posy))*alpha(matriz(posx,posy)+2,matriz(posx,im(posy))+2) ) + 2*currentField*matriz(posx,posy)

end subroutine CalcDeltaEnergy
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine CalcEnergy(nrows,ncols,posx,posy,matriz,Delta_E,ip,im,alpha,currentField)
	!Calculate Delta for a given point (posx,posy)
	!with periodic boundary conditions
	integer,intent(in)::nrows,ncols
	integer,intent(in)::posx,posy,matriz(ncols,nrows),ip(nrows),im(nrows)
	real,intent(out)::Delta_E
	real,intent(in)::alpha(3,3),currentField
	integer::J = 1

	!Delta_E =2*J*matriz(posx,posy) * ( matriz(ip(posx),posy) + matriz(im(posx),posy) + matriz(posx,ip(posy)) + matriz(posx,im(posy)))

	Delta_E =-J*matriz(posx,posy)* &
		( matriz(ip(posx),posy)*alpha(matriz(posx,posy)+2,matriz(ip(posx),posy)+2) &
		+ matriz(im(posx),posy)*alpha(matriz(posx,posy)+2,matriz(im(posx),posy)+2) &
	    + matriz(posx,ip(posy))*alpha(matriz(posx,posy)+2,matriz(posx,ip(posy))+2) &
	    + matriz(posx,im(posy))*alpha(matriz(posx,posy)+2,matriz(posx,im(posy))+2) ) - currentField*matriz(posx,posy)

end subroutine CalcEnergy
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine CalcQuantities(nrows,ncols,matriz,Delta_E,EnergyAver,EnergyAver2,MagnetizationAver,MagnetizationAver2,MagnetizationAver4,ip,im,alpha,averageIndex,currentField)

	integer,intent(in)::nrows,ncols,matriz(ncols,nrows),averageIndex
	real,intent(out)::Delta_E
	integer,intent(in)::ip(nrows),im(nrows)
	!Elements of the arrays
	real,intent(in)::alpha(3,3),currentField
	real(dp2),intent(inout)::EnergyAver,EnergyAver2
	real(dp2),intent(inout)::MagnetizationAver,MagnetizationAver2,MagnetizationAver4
	integer::i,j
	integer::dim
	real(dp2)::Energy,Magnetization

	dim = nrows*ncols
	Energy = 0.0
	do i = 1, nrows
		do j = 1, ncols
			call CalcEnergy(nrows,ncols,i,j,matriz,Delta_E,ip,im,alpha,currentField)
			Energy = Energy + Delta_E
		end do
	end do
	! E 
	Energy = Energy/real(2.0)!
	!Se divide por dos nuevamente para no contar dos veces interacciones entre spines
	! M
	Magnetization = sum(matriz(1:ncols,1:nrows))/real(1.0)
	
	!<E> and <E2>; <m>, <m2> and <m4>

	EnergyAver = EnergyAver + Energy
	EnergyAver2 = EnergyAver2 + Energy**2
	MagnetizationAver = MagnetizationAver + abs(Magnetization)
	MagnetizationAver2 = MagnetizationAver2 + Magnetization**2
	MagnetizationAver4 = MagnetizationAver4 + Magnetization**4

	
	!write(*,*) "Energy: ",Energy," EnergyAv: ", EnergyAver, " EnergyAver2: ", EnergyAver2
	
end subroutine CalcQuantities
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine Write2File ( Magnetization,Magnetization2,Magnetization4,Energy,Energy2,MagnetizationINI,Magnetization2INI,Magnetization4INI,EnergyINI,Energy2INI,length,average )
	!Write the important values in a file
	integer,intent(in)::length,average
	real(dp2),intent(in)::Magnetization(length), Magnetization2(length),Magnetization4(length)
	real(dp2),intent(in)::Energy(length),Energy2(length)
	real(dp2)::MagnetizationINI,Magnetization2INI,Magnetization4INI,EnergyINI,Energy2INI
	integer::j

	write(1,'(I6,x,I6,x,F16.6,x,F16.6,x,F16.6,x,F16.6,x,F16.6)') average,0+1,MagnetizationINI/average,Magnetization2INI/average,Magnetization4INI/average,EnergyINI/average,Energy2INI/average
	do j = 1,length		
		write(1,'(I6,x,I6,x,F16.6,x,F16.6,x,F16.6,x,F16.6,x,F16.6)') average,j+1,Magnetization(j)/average,Magnetization2(j)/average,Magnetization4(j)/average,Energy(j)/average,Energy2(j)/average
	end do
end subroutine Write2File
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine WriteSpinConf(matriz,nrows,ncols)
	
	integer,intent(in)::ncols,nrows
	integer, intent(in)::matriz(ncols,nrows)
	
	integer::index1,index2

	do index1=1,ncols
		do index2=1,nrows
			write(*,*) index1,index2,matriz(index1,index2)
		end do
	end do
end subroutine WriteSpinConf
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module subroutines
