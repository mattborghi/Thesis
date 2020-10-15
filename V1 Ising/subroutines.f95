module subroutines
use rm
implicit none
integer, parameter :: dp2 = selected_real_kind(15, 307) !For doble precision
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine SetToZeroVariables(MagnetizationAver,MagnetizationAver2, &
								MagnetizationAver4,EnergyAver,EnergyAver2,average)

	integer,intent(in)::average
	real(dp2),intent(out)::MagnetizationAver(average),MagnetizationAver2(average),MagnetizationAver4(average)
	real(dp2),intent(out)::EnergyAver(average),EnergyAver2(average)

		MagnetizationAver(:) = 0.0
		MagnetizationAver2(:) = 0.0
		MagnetizationAver4(:) = 0.0
		EnergyAver(:) = 0.0
		EnergyAver2(:) = 0.0

end subroutine SetToZeroVariables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine Inicializacion_DeltaE(DeltaE,Temp)
	
	real,intent(in)::Temp
	real,intent(out)::DeltaE(2)

	DeltaE(1) = exp(-4/Temp)
	DeltaE(2) = exp(-8/Temp)

end subroutine Inicializacion_DeltaE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine ini_matriz(matriz,nrows,ncols)
	
	!Initialize a square matrix of dimention nrowsXncols 
	!with ones
	!that means all spin-ups
	integer,intent(in)::nrows,ncols
	integer,intent(out)::matriz(nrows,ncols)

	!Index to loop the matrix
	integer::i,j
	real::rvec(1)
	
	do i=1,nrows
		do j = 1,ncols
			matriz (i,j) = 1	
		end do
	end do

end subroutine ini_matriz
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine elegir_sitio(nrows,ncols,posx,posy)
	
	!This subroutine gives a random point in the square matrix of LxL
	integer,intent(in)::nrows,ncols
	integer,intent(out)::posx,posy

	real::rvec(2)
	call ranmar(rvec,2)

	!Given values between 0-1 for rvec we want values between 1-nrows/ncols for each coordinate 
	!So this lines map one values to others 0-1 -> 1-nrows/ncols
	posx = floor( nrows * rvec(1) ) +1
	posy = floor( ncols * rvec(2) ) +1

end subroutine elegir_sitio
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine CalcEnergy(nrows,ncols,posx,posy,matriz,Delta_E,ip,im)
	!Calculate Delta for a given point (posx,posy)
	!with periodic boundary conditions
	integer,intent(in)::nrows,ncols
	integer,intent(in)::posx,posy,matriz(nrows,ncols),ip(nrows),im(nrows)
	integer,intent(out)::Delta_E
	integer::J = 1

	Delta_E =2*J*matriz(posx,posy) * ( matriz(ip(posx),posy) + matriz(im(posx),posy) + matriz(posx,ip(posy)) + matriz(posx,im(posy)))

end subroutine CalcEnergy
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine CalculationsINequilibrium(nrows,ncols,matriz,Delta_E,EnergyAver,EnergyAver2, &
									MagnetizationAver,MagnetizationAver2,MagnetizationAver4,k,average,ip,im)

	integer,intent(in)::nrows,ncols,matriz(nrows,ncols),k,average
	integer,intent(out)::Delta_E
	integer,intent(in)::ip(nrows),im(nrows)
	real(dp2),intent(inout)::EnergyAver(average),EnergyAver2(average)
	real(dp2),intent(inout)::MagnetizationAver(average),MagnetizationAver2(average),MagnetizationAver4(average)
	integer::i,j
	integer::dim
	real(dp2)::Energy,Magnetization

	dim = nrows*ncols
	Energy = 0.0
	do i = 1, nrows
		do j = 1, ncols
			call CalcEnergy(nrows,ncols,i,j,matriz,Delta_E,ip,im)
			Energy = Energy - Delta_E/2
			!Divido por (-2) para que me devuelva la energía del spin i,j
		end do
	end do
	! E 
	Energy = Energy/(dim*2.0)
	!Se divide por dos nuevamente para no contar dos veces interacciones entre spines
	
	!<E>
	EnergyAver(k) = EnergyAver(k) + Energy
	!<E2>
	EnergyAver2(k) = EnergyAver2(k) + Energy**2
	
	!write(*,*) "Energy: ",Energy," EnergyAv: ", EnergyAver, " EnergyAver2: ", EnergyAver2

	Magnetization = sum(matriz(1:nrows,1:ncols))/(dim*1.0)
	!<m>
	MagnetizationAver(k) = MagnetizationAver(k) + Magnetization
	!<m2>
	MagnetizationAver2(k) = MagnetizationAver2(k) + Magnetization**2
	!<m4>
	MagnetizationAver4(k) = MagnetizationAver4(k) + Magnetization**4

end subroutine CalculationsINequilibrium
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine Write2File ( promediateCounter, beta,currentTemp, MagnetizationAver,MagnetizationAver2, &
						MagnetizationAver4,EnergyAver,EnergyAver2,average )
	!Write the important values in a file
	integer,intent(in)::promediateCounter,average
	real,intent(in)::beta, currentTemp
	real(dp2),intent(in)::MagnetizationAver(average), MagnetizationAver2(average),MagnetizationAver4(average)
	real(dp2),intent(in)::EnergyAver(average),EnergyAver2(average)

	real(dp2):: Cumulant,Suceptibility,C_v
	real(dp2):: AverEnergy,AverEnergy2,AverMagnet,AverMagnet2,AverMagnet4

	!Promediate the values
	call prom1d(EnergyAver,average,AverEnergy)
	call prom1d(EnergyAver2,average,AverEnergy2)
	call prom1d(MagnetizationAver,average,AverMagnet)
	call prom1d(MagnetizationAver2,average,AverMagnet2)
	call prom1d(MagnetizationAver4,average,AverMagnet4)

	Suceptibility = beta*(AverMagnet2/promediateCounter - (AverMagnet/promediateCounter)**2 )
	Cumulant = 1 - ( AverMagnet4/promediateCounter / ( 3*( (AverMagnet2/promediateCounter)**2) ) )
	C_v = (beta**2)*( AverEnergy2/promediateCounter - (AverEnergy/promediateCounter)**2)
	
		write(1,*) currentTemp, abs(AverMagnet/promediateCounter), AverMagnet2/promediateCounter, Suceptibility , Cumulant
		write(33,*) currentTemp, AverEnergy/promediateCounter, AverEnergy2/promediateCounter, C_v

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
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
end module subroutines
