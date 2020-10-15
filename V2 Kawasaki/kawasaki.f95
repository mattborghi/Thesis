!-------------------------------------------------------------------------
!Author: Matias Pablo, Borghi Orue
!Title: 2D Monte Carlo simulation of the Ising Model 
!Version: 1 
!Comments:  $ gfortran ranmar_seed.f95 subroutines.f95 kawasaki.f95 -o runv1
!INPUTS: ConfigurationFile
!OUTPUTS: Two dat files: 'Magnetization30L.dat' and 'Energy30L.dat'
!-------------------------------------------------------------------------
program kawasaki

use subroutines
use rm
implicit none
	
	integer, parameter :: dp = selected_real_kind(15, 307) !For doble precision
	integer::nrows,ncols, average,iniseed
	integer::posx,posy,dim
	real::npos,nneg !Sum of thes values = 1
	integer:: MCS, EquilSteps, NumofTempPoints
	integer:: i,k,indexTemp !Dummy indeces
	integer :: index1, index2
	integer::exchangeSpin
	integer,allocatable::matriz(:,:)
	integer,allocatable::ip(:),im(:)
	integer::promediateCounter = 0
	real,allocatable::alpha(:,:)
	integer,allocatable::InterPos(:),InterNeg(:),InterPN(:)
	real::promInterPos,promInterNeg,promInterPN
	real,allocatable::rIdeal(:),rMeasured(:),rho(:)
	real::promIdealR,promMeasuredR,promRho
	real::Delta_E
	real::rvec(1)
	real(dp),allocatable::MagnetizationAver(:), MagnetizationAver2(:),MagnetizationAver4(:),EnergyAver(:),EnergyAver2(:)
	real,dimension(20)::DeltaE
	real::Tmax,Tmin,TempInterval,currentTemp,beta
	character(len=100)::MagFile ='Magnetization20L.dat', EnergFile = 'Energy20L.dat',InterFile='Inter20L.dat'

	! Read in input parameters from file "ising.in"
	open(unit=11,file="ConfigurationFile",status="old",action="read")
	read(11,*);read(11,*) iniseed
	read(11,*);read(11,*) npos
	read(11,*);read(11,*) nneg
	read(11,*);read(11,*) nrows
	read(11,*);read(11,*) ncols
	read(11,*);read(11,*) MCS
	read(11,*);read(11,*) EquilSteps
	read(11,*);read(11,*) Tmax
	read(11,*);read(11,*) Tmin
	read(11,*);read(11,*) TempInterval
	read(11,*);read(11,*) average
	close(11)

	! Allocate the space for some important quantities.
	allocate(matriz(nrows,ncols))
	allocate(alpha(3,3))

	allocate(MagnetizationAver(average))
	allocate(MagnetizationAver2(average))
	allocate(MagnetizationAver4(average))	
	allocate(EnergyAver(average))
	allocate(EnergyAver2(average))

	!Needs to be improved. Now it is made for square lattices
	allocate(im(nrows))
	allocate(ip(nrows))

	allocate(InterPN(average))
	allocate(InterPos(average))
	allocate(InterNeg(average))

	allocate(rIdeal(average))
	allocate(rMeasured(average))
	allocate(rho(average))

	!Fill ip & im arrays
	do i = 1,nrows
		ip(i) = i+1
		im(i) = i-1 
	end do
	ip(nrows) = 1
	im(1) = nrows

	dim = nrows*ncols
	
	!MCS = dim*10
	!EquilSteps = int (0.5*MCS)

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
	open (UNIT=34, FILE=InterFile,ACTION='write',STATUS='replace')
	write(34,*) "Temperature InterPos InterNeg InterPN IdealR MeasuredR Rho"
	

	!Initialize the matrix
	!with spin-up initial values
	call ini_matriz(matriz,nrows,ncols,npos,nneg,iniseed)
	call ini_alpha(alpha)
	!Write final spin configurations in a file
	call WriteSpinConf(matriz,nrows,ncols)
	!call CountPosNeg(matriz,nrows,ncols,npos,nneg)
	

	do indexTemp = 1,NumofTempPoints !Loop over all the temperatures from Tmax to Tmin
		!Change the temperature value
		call New_Temp(indexTemp,currentTemp,Tmax,Tmin,TempInterval) 
		beta = 1.0/currentTemp
		print *, "Current Temperature T = ", currentTemp
		
		!Initialize some variables to zero
		call SetToZeroVariables(MagnetizationAver,MagnetizationAver2, &
									MagnetizationAver4,EnergyAver,EnergyAver2,average)

		!Define the values of DeltaE knowing the Temperature
		!Only two possible values
		! 1) DeltaE(1) = exp(-4/Temp)
		! 2) DeltaE(2) = exp(-8/Temp)
		call Inicializacion_DeltaE(DeltaE,currentTemp)

		do k = 1 , average
			!print *, "Average loop k = ", k,"/",average
			promediateCounter = 0
			!Choose a random point (posx,posy) where we begin calculating
			call elegir_sitio(nrows,ncols,posx,posy,iniseed)

			!Sum over one MonteCarlo time //dim = 1MC
			!Sum over A MonteCarlo time: A*dim 
			!In the next loop we flip a sping according to some conditions
			do i=1,MCS
				!Calculate the Magnetization and Energy after equilibrium
				if (i > EquilSteps ) then
					promediateCounter = promediateCounter + 1
					call CalculationsINequilibrium (nrows,ncols,matriz,alpha,Delta_E,EnergyAver,EnergyAver2, &
										MagnetizationAver,MagnetizationAver2,MagnetizationAver4,k,average,ip,im)

				end if
				!Calculate DeltaE for a given point (posx,posy)
				call CalcDeltaEnergy(nrows,ncols,posx,posy,matriz,alpha,Delta_E,ip,im)
				!print *, "Delta E: ",Delta_E
				! ..Flipping Spin..
				!If the new Energy spin-state is negative with flip the spin
				!If not it will depend on the probability exp(-DeltaE/Temp) > random point
				if (Delta_E <= 0) then
					call exchange(nrows,ncols,posx,posy,matriz,ip)
				else
					call ranmar(rvec,1,iniseed) 
					if (Delta_E == int(Delta_E) .AND. Delta_E<=20) then
						if (rvec(1) < DeltaE(int(Delta_E))) then
							call exchange(nrows,ncols,posx,posy,matriz,ip)
						end if
					else	
						if ( rvec(1) < exp(-Delta_E/currentTemp) ) then
							call exchange(nrows,ncols,posx,posy,matriz,ip)
						end if
					end if
				end if

				!Choose a new random point (posx,posy) where we begin calculating
				call elegir_sitio(nrows,ncols,posx,posy,iniseed)
				!----------------------------------------------------------------
				!-----PRINT SITES IN DYNAMIC LATTICE ----------------------------
				!do index1=1,nrows
				!	do index2=1,ncols
				!		write(*,*) index1,index2,matriz(index1,index2)
				!	end do
				!end do
				!----------------------------------------------------------------
			end do !End loop of the MC steps
			call CellInteraction(matriz,nrows,ncols,currentTemp,ip,im,k,average,InterPos,InterNeg, &
						InterPN,rIdeal,rMeasured,rho,npos)
			!Write final spin configurations in a file
			call WriteSpinConf(matriz,nrows,ncols)
		end do ! End loop of averages
		!Write the results in a file
		call Write2File(promediateCounter, beta,currentTemp, MagnetizationAver,MagnetizationAver2, &
						MagnetizationAver4,EnergyAver,EnergyAver2,average)
		!Sabe in file the Interaction arrays
		call prom1dReal2Int(InterPos,average,promInterPos)
		call prom1dReal2Int(InterNeg,average,promInterNeg)
		call prom1dReal2Int(InterPN,average,promInterPN)
		call prom1dp(rIdeal,average,promIdealR)
		call prom1dp(rMeasured,average,promMeasuredR)
		call prom1dp(rho,average,promRho)
		write(34,*) currentTemp,promInterPos,promInterNeg,promInterPN, rIdeal, rMeasured, rho
	end do !End loop of Temperatures
	!Write final spin configurations in a file
	!call WriteSpinConf(matriz,nrows,ncols)
		
	close(1)
	close(33)
	close(34)
end program kawasaki


