module subroutines
use rm
implicit none
integer, parameter :: dp2 = selected_real_kind(15, 307) !For doble precision
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
subroutine SetToZeroVariables(MagnetizationAver,EnergyAver,antiMagnetization,absMagnetizationAver)

	integer,intent(out)::MagnetizationAver,absMagnetizationAver
	real,intent(out)::antiMagnetization,EnergyAver

	MagnetizationAver = 0
	absMagnetizationAver = 0
	EnergyAver = 0.0
	antiMagnetization = 0.0
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
	    + matriz(posx,im(posy))*alpha(matriz(posx,posy)+2,matriz(posx,im(posy))+2) ) 

end subroutine CalcEnergy
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine CalcQuantities(nrows,ncols,matriz,Delta_E,EnergyAver,MagnetizationAver,antiMagnetization,absMagnetization,ip,im,alpha,averageIndex,currentField)

	integer,intent(in)::nrows,ncols,matriz(ncols,nrows),averageIndex
	real,intent(out)::Delta_E
	integer,intent(in)::ip(nrows),im(nrows)
	!Elements of the arrays
	real,intent(in)::alpha(3,3),currentField
	real,intent(inout)::EnergyAver,antiMagnetization
	integer,intent(inout)::MagnetizationAver,absMagnetization
	integer::i,j
	integer::dim
	real(dp2)::Energy,Magnetization,antiMagnet
	integer:: white,black
	real::magnetBlack,magnetWhite
	dim = nrows*ncols
	Energy = 0.0
	magnetWhite = 0.0
	magnetBlack = 0.0
	white = 0
	black = 0
	antiMagnet = 0.0
	do i = 1, nrows
		do j = 1, ncols
			call CalcEnergy(nrows,ncols,i,j,matriz,Delta_E,ip,im,alpha,currentField)
			Energy = Energy + Delta_E
			if ( (mod(i,2) == 0 .AND. mod(j,2) == 0) .OR.(mod(i,2) /= 0 .AND. mod(j,2) /= 0) ) then
				magnetBlack = magnetBlack + matriz(i,j)
				black = black + 1
			else
				magnetWhite = magnetWhite + matriz(i,j)
				white = white + 1
			end if 
		end do
	end do
	! E 
	Energy = Energy/real(2.0)
	!Se divide por dos nuevamente para no contar dos veces interacciones entre spines
	!-----AntiFerro quantities----------------
	!<mB>
	magnetBlack = magnetBlack / real(black)
	!<mW>
	magnetWhite = magnetWhite / real(white)
	!inter 
	antiMagnet = abs(magnetBlack - magnetWhite) / real(2)
	!<mANTI>
	antiMagnetization = antiMagnetization + (antiMagnet)
	!------------------------------------------------
	!------------------------------------------------
	! M
	Magnetization = sum(matriz(1:ncols,1:nrows))
	!print *, Magnetization
	!<E> and <E2>; <m>, <m2> and <m4>

	EnergyAver = EnergyAver + Energy

	MagnetizationAver = MagnetizationAver + Magnetization

	absMagnetization = absMagnetization + abs(Magnetization)
	
	!write(*,*) "Energy: ",Energy," EnergyAv: ", EnergyAver, " EnergyAver2: ", EnergyAver2
	
end subroutine CalcQuantities
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
