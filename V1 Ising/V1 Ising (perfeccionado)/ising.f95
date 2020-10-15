!-------------------------------------------------------------------------
!Author: Matias Pablo, Borghi Orue
!Title: 2D Monte Carlo simulation of the Ising Model 
!Version: 1 
!Comments:  $ gfortran ranmar_seed.f95 subroutines.f95 ising.f95 -o runv1
!INPUTS: None
!OUTPUTS: Two dat files: 'Magnetization30L.dat' and 'Energy30L.dat'
!-------------------------------------------------------------------------
program ising

use subroutines
use rm
implicit none
	integer, parameter :: dp = selected_real_kind(15, 307) 
	integer::nrows,ncols, average
	integer::posx,posy,dim
	integer,allocatable::ip(:),im(:)
	integer:: MCS, EquilSteps, NumofTempPoints
	integer:: i,k,indexTemp !Dummy indeces
	integer :: index1, index2
	integer::Delta_E
	integer,allocatable::matriz(:,:)
	integer::promediateCounter = 0
	real::rvec(1)
	real(dp)::MagnetizationAver, MagnetizationAver2,MagnetizationAver4,EnergyAver,EnergyAver2
	real,dimension(2)::DeltaE
	real::Tmax ,Tmin,TempInterval,currentTemp,beta
	character(len=100)::MagFile ='Magnetization20L.dat', EnergFile = 'Energy20L.dat'

	! Read in input parameters from file "ising.in"
	open(unit=11,file="ConfigurationFile",status="old",action="read")
	read(11,*);read(11,*) nrows
	read(11,*);read(11,*) ncols
	read(11,*);read(11,*) MCS
	read(11,*);read(11,*) EquilSteps
	read(11,*);read(11,*) Tmax
	read(11,*);read(11,*) Tmin
	read(11,*);read(11,*) TempInterval
	read(11,*);read(11,*) average
	close(11)
	! Set the dimensions of the matrix of spin arrays. This program uses
	! periodic boundary conditions, so the first two rows and columns are
	! the same as the last two.
	allocate(matriz(nrows,ncols))

	!Change this to take in count that the matrix can be rectangular
	allocate(im(nrows))
	allocate(ip(nrows))

	do i = 1,nrows
		ip(i) = i+1
		im(i) = i-1 
	end do
	ip(nrows) = 1
	im(1) = nrows

	dim = nrows*ncols
	!MCS = 20000!dim*10
	!EquilSteps = 10000!int (0.5*MCS)
	
	if ( EquilSteps >= MCS ) then
		STOP 'Change the value of EquilSteps in the Configuration File! It can not be greater than MCS'
	end if 

	print *, "MCS = ",MCS, " EquilSteps = ", EquilSteps
	NumofTempPoints = int ( (Tmax - Tmin)/TempInterval ) + 1 
	!If the file exists overwrite erase it.
	open (UNIT=1, FILE=MagFile,ACTION='write',STATUS='replace')
	write(1,*) "Temperature AverMagnetization AverMagnetization^2 Suceptibility Cumulant"
	open (UNIT=33, FILE=EnergFile,ACTION='write',STATUS='replace')
	write(33,*) "Temperature AverEnergy AverEnergy^2 C_v"

	!Initialize the matrix
	!with spin-up initial values
	call ini_matriz(matriz,nrows,ncols)
	!call WriteSpinConf(matriz,nrows,ncols)
	do indexTemp = 1,NumofTempPoints !Loop over all the temperatures from Tmax to Tmin
		!Change the temperature value
		call New_Temp(indexTemp,currentTemp,Tmax,Tmin,TempInterval) 
		beta = 1.0/currentTemp
		print *, "Current Temperature T = ", currentTemp
		
		!Initialize some variables to zero
		call SetToZeroVariables(MagnetizationAver,MagnetizationAver2, &
									MagnetizationAver4,EnergyAver,EnergyAver2)

		!Define the values of DeltaE knowing the Temperature
		!Only two possible values
		! 1) DeltaE(1) = exp(-4/Temp)
		! 2) DeltaE(2) = exp(-8/Temp)
		call Inicializacion_DeltaE(DeltaE,currentTemp)
		promediateCounter = 0

		do k = 1 , average
			print *, 'k = ',k,'/',average
			!Choose a random point (posx,posy) where we begin calculating
			call elegir_sitio(nrows,ncols,posx,posy)

			!Sum over one MonteCarlo time //dim = 1MC
			!Sum over A MonteCarlo time: A*dim 
			!In the next loop we flip a sping according to some conditions
			do i=1,MCS
				!Calculate the Magnetization and Energy after equilibrium
				if (i > EquilSteps ) then
					promediateCounter = promediateCounter + 1
					call CalculationsINequilibrium (nrows,ncols,matriz,Delta_E,EnergyAver,EnergyAver2, &
										MagnetizationAver,MagnetizationAver2,MagnetizationAver4,k,ip,im)

				end if
				!Calculate DeltaE for a given point (posx,posy)
				call CalcEnergy(nrows,ncols,posx,posy,matriz,Delta_E,ip,im)
				call ranmar(rvec,1)
				! ..Flipping Spin..
				!If the new Energy spin-state is negative with flip the spin
				!If not it will depend on the probability exp(-DeltaE/Temp) > random point
				select case (Delta_E)
					case (-8)
						matriz(posx,posy) = - matriz(posx,posy)
					case (-4)
						matriz(posx,posy) = - matriz(posx,posy)
					case (0)
						matriz(posx,posy) = - matriz(posx,posy)
					case (4)
						if( DeltaE(1) > rvec(1) ) then
							matriz(posx,posy) = - matriz(posx,posy)
						end if
					case (8)
						if( DeltaE(2) > rvec(1) ) then
							matriz(posx,posy) = - matriz(posx,posy)
						end if
				end select

				!Choose a new random point (posx,posy) where we begin calculating
				call elegir_sitio(nrows,ncols,posx,posy)
				!----------------------------------------------------------------
				!-----PRINTF IN DYNAMIC LATTICE ---------------------------------
				!do index1=1,nrows
				!	do index2=1,ncols
				!		write(*,*) index1,index2,matriz(index1,index2)
				!	end do
				!end do
				!_----------------------------------------------------------------
			end do !End loop of the MC steps
			!call WriteSpinConf(matriz,nrows,ncols)
		end do ! End loop of averages
		!Write the results in a file
		call Write2File(promediateCounter, beta,currentTemp, MagnetizationAver,MagnetizationAver2, &
						MagnetizationAver4,EnergyAver,EnergyAver2,average)
	end do !End loop of Temperatures

	close(1)
	close(33)
end program ising


