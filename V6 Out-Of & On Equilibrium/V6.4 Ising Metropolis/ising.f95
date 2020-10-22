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
use iso_fortran_env
implicit none
	integer, parameter :: dp = selected_real_kind(15, 307) 
	integer::nrows,ncols, average,iniseed
	integer::posx,posy,dim
	integer,allocatable::ip(:),im(:)
	integer:: MCS, EquilSteps, NumofTempPoints
	integer:: i,j,k,indexTemp,printInfo !Dummy indeces
	integer::index1,index2
	real::Delta_E
	integer,allocatable::matriz(:,:)
	integer::promediateCounter = 0
	real::rvec(1)
	real(dp),allocatable::MagnetizationAver(:), MagnetizationAver2(:),MagnetizationAver4(:),EnergyAver(:),EnergyAver2(:)
	real,dimension(20)::DeltaE
	real,allocatable::alpha(:,:)
	real::alpha11,alpha13,alpha33
	real::Tmax ,Tmin,TempInterval,currentTemp,beta
	character(len=10):: stringL
	character(len=100)::MagFile ='Magnetization20L.dat', EnergFile = 'Energy20L.dat'
	character(len=100):: folderName,FileOutput='OutputFile.log'

	! Read in input parameters from file "ising.in"
	open(unit=11,file="ConfigurationFile",status="old",action="read")
	read(11,*);read(11,*) printInfo
	read(11,*);read(11,*) nrows
	read(11,*);read(11,*) ncols
	read(11,*);read(11,*) iniseed
	read(11,*);read(11,*) MCS
	read(11,*);read(11,*) EquilSteps
	read(11,*);read(11,*) Tmax
	read(11,*);read(11,*) Tmin
	read(11,*);read(11,*) TempInterval
	read(11,*);read(11,*) average
	read(11,*);read(11,*) alpha11
	read(11,*);read(11,*) alpha13
	read(11,*);read(11,*) alpha33
	close(11)

	NumofTempPoints = int ( (Tmax - Tmin)/TempInterval ) + 1 

	! Set the dimensions of the matrix of spin arrays. This program uses
	! periodic boundary conditions, so the first two rows and columns are
	! the same as the last two.
	allocate(matriz(nrows,ncols))
	allocate(alpha(3,3))

	allocate(MagnetizationAver(NumofTempPoints))
	allocate(MagnetizationAver2(NumofTempPoints))
	allocate(MagnetizationAver4(NumofTempPoints))
	allocate(EnergyAver(NumofTempPoints))
	allocate(EnergyAver2(NumofTempPoints))

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

	write(stringL,'(I3.3)') nrows
	folderName = 'L='//trim(stringL)

	open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),status="replace",action="write")
	write(2,*) 'Parameters in this simulation'
	write(2,*) 'L=',nrows
	write(2,*) 'Tmax=',Tmax
	write(2,*) 'Tmin=',Tmin
	write(2,*) 'TempInterval=',TempInterval
	write(2,*) 'Alpha=',alpha11,' ',alpha13,' ',alpha33
	write(2,*) 'MCS=',MCS
	write(2,*) 'aver=',average
	
	if ( EquilSteps >= MCS ) then
		write(2,*) 'Error! Change the value of EquilSteps in the Configuration File! It can not be greater than MCS'
		call EXIT(0)
	end if 

	write(2,*) 'Semilla RANMAR: ',iniseed
	write(2,*) "MCS = ",MCS, " EquilSteps = ", EquilSteps
	
	!If the file exists overwrite erase it.
	open (UNIT=1, FILE=trim(folderName)//'/'//trim(MagFile),ACTION='write',STATUS='replace')
	write(1,*) "Temperature, AverMagnetization, AverMagnetization^2, Suceptibility, Cumulant"
	open (UNIT=33, FILE=trim(folderName)//'/'//trim(EnergFile),ACTION='write',STATUS='replace')
	write(33,*) "Temperature, AverEnergy, AverEnergy^2, C_v"

	call ini_alpha(alpha,alpha11,alpha13,alpha33)
	!Initialize the matrix
	!with spin-up initial values
	call ini_matriz(matriz,nrows,ncols)
	close(2)

	if (printInfo /= 0) then
		call WriteSpinConf(matriz,nrows,ncols)
	end if
	
	!Initialize some variables to zero
	call SetToZeroVariables(MagnetizationAver,MagnetizationAver2,MagnetizationAver4,EnergyAver,EnergyAver2,NumofTempPoints)
	!call WriteSpinConf(matriz,nrows,ncols)
	do k = 1 , average
		open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),access="append",action="write")
		write(2,*) 'k = ',k,'/',average	
		close(2)
				
		do indexTemp = 1,NumofTempPoints !Loop over all the temperatures from Tmax to Tmin
			promediateCounter = 0
			!Change the temperature value
			call New_Temp(indexTemp,currentTemp,Tmax,TempInterval) 
			beta = 1.0/currentTemp
			open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),access="append",action="write")
			write(2,*), "Current Temperature T = ", currentTemp
			close(2)
			!Define the values of DeltaE knowing the Temperature
			!Only two possible values
			! 1) DeltaE(1) = exp(-4/Temp)
			! 2) DeltaE(2) = exp(-8/Temp)
			call Inicializacion_DeltaE(DeltaE,currentTemp)

			!Choose a random point (posx,posy) where we begin calculating
			call elegir_sitio(nrows,ncols,posx,posy,iniseed,k)

			!Sum over one MonteCarlo time //dim = 1MC
			!Sum over A MonteCarlo time: A*dim 
			!In the next loop we flip a sping according to some conditions
			do i=1,MCS

				do j = 1,dim

					!Calculate DeltaE for a given point (posx,posy)
					call CalcDeltaEnergy(nrows,ncols,posx,posy,matriz,Delta_E,ip,im,alpha)
					
					!call ranmar(rvec,iniseed*verage)
					! ..Flipping Spin..
					!If the new Energy spin-state is negative with flip the spin
					!If not it will depend on the probability exp(-DeltaE/Temp) > random point
					if (Delta_E <= 0) then
						matriz(posx,posy) = - matriz(posx,posy)
					else 
						call ranmar(rvec,1,iniseed*k) 
						if (Delta_E == int(Delta_E) .AND. Delta_E<=20) then
							if (rvec(1) < DeltaE(int(Delta_E))) then
									matriz(posx,posy) = -matriz(posx,posy)
							end if
						else	
							if ( rvec(1) < exp(-Delta_E/currentTemp) ) then
								matriz(posx,posy) = -matriz(posx,posy)
							end if
						end if
						
					end if

					!Choose a new random point (posx,posy) where we begin calculating
					call elegir_sitio(nrows,ncols,posx,posy,iniseed,k)

					!----------------------------------------------------------------
					!-----PRINTF IN DYNAMIC LATTICE ---------------------------------
					if (printInfo == 1) then
						call WriteSpinConf(matriz,nrows,ncols)
					end if
					!-----------------------------------------------------------------

				end do !End j loop

				!Calculate the Magnetization and Energy after equilibrium
				if (i > EquilSteps ) then
					promediateCounter = promediateCounter + 1
					call CalculationsINequilibrium (nrows,ncols,matriz,Delta_E,EnergyAver(indexTemp),EnergyAver2(indexTemp), &
										MagnetizationAver(indexTemp),MagnetizationAver2(indexTemp),MagnetizationAver4(indexTemp),ip,im,alpha)

					if (printInfo == 2) then 
						call WriteSpinConf(matriz,nrows,ncols)
					else if (printInfo == 3 .AND. mod(i,MCS/10) == 0 .AND. k==1) then
						call WriteSpinConf(matriz,nrows,ncols)
					end if

				end if
				
			end do !End loop of the MC steps
			!call WriteSpinConf(matriz,nrows,ncols)
		end do ! End loop of Temperatures
	end do !End loop of averages
	!Write the results in a file
	call Write2File(promediateCounter,Tmax,TempInterval, MagnetizationAver,MagnetizationAver2,MagnetizationAver4,EnergyAver,EnergyAver2,NumofTempPoints,average)
	
	open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),access="append",action="write")
	write(2,*), "Ended Program"
	close(2)

	print *, 'promediateCounter = ',promediateCounter
	
	close(1)
	close(33)
end program ising


