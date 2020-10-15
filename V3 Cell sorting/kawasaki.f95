!-------------------------------------------------------------------------
!Author: Matias Pablo, Borghi Orue
!Title: 2D Monte Carlo simulation of the Ising Model 
!Version: 1 
!Comments:  $ gfortran ranmar_seed.f95 subroutines.f95 kawasaki.f95 -o runv1
!DynamicLattice: $./runv1 | ./dllinux.exe -1 -nx 20 -ny 20 -grid -z -1 1
!INPUTS: ConfigurationFile
!OUTPUTS: Two dat files: 'Magnetization30L.dat' and 'Energy30L.dat'
!-------------------------------------------------------------------------
program kawasaki

use subroutines
use rm
use iso_fortran_env
implicit none
	
	integer, parameter :: dp = selected_real_kind(15, 307) !For double precision
	integer::nrows,ncols,iniseed,npoints
	integer::average
	integer::posx,posy,dim
	real::npos,nneg !Sum of thes values = 1
	integer(kind=int64):: MCS, EquilSteps!, NumofTempPoints
	integer(kind=int64):: i!,indexTemp !Dummy indeces
	integer:: k !average index
	integer :: index1, index2
	integer::exchangeSpin
	integer,allocatable::matriz(:,:)
	integer,allocatable::ip(:),im(:)
	integer::promediateCounter 
	real,allocatable::alpha(:,:)
	integer,allocatable::interPos(:),interNeg(:),interPN(:)
	!integer::interPos,interNeg,interPN
	
	!real::rIdeal,rMeasured,rho
	!real :: start, finish !For measuring time
	real::Delta_E
	real::rvec(1)
	real(dp),allocatable::Magnetization(:), Magnetization2(:),Magnetization4(:),Energy(:),Energy2(:)
	real,dimension(20)::DeltaE
	real::currentTemp,beta
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
	read(11,*);read(11,*) currentTemp
	read(11,*);read(11,*) npoints
	read(11,*);read(11,*) average
	close(11)

	! Allocate the space for some important quantities.
	allocate(matriz(nrows,ncols))
	allocate(alpha(3,3))

	allocate(Magnetization(npoints))
	allocate(Magnetization2(npoints))
	allocate(Magnetization4(npoints))	
	allocate(Energy(npoints))
	allocate(Energy2(npoints))

	!Needs to be improved. Now it is made for square lattices
	allocate(im(nrows))
	allocate(ip(nrows))

	allocate(interPN(npoints))
	allocate(interPos(npoints))
	allocate(interNeg(npoints))

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

	!1MCS = dim = nrows*ncols = 400
	! 10 000 MCS = 4M 
	print *, "MCS = ",MCS, " EquilSteps = ", EquilSteps
	print *, "Npoints = ",npoints

	!If the file exists overwrite erase it.
	open (UNIT=1, FILE=MagFile,ACTION='write',STATUS='replace')
	write(1,*) "Tiempo AverMagnetization AverMagnetization^2 Suceptibility Cumulant"
	open (UNIT=33, FILE=EnergFile,ACTION='write',STATUS='replace')
	write(33,*) "Tiempo AverEnergy AverEnergy^2 C_v"
	open (UNIT=34, FILE=InterFile,ACTION='write',STATUS='replace')
	write(34,*) "Tiempo InterPos InterNeg InterPN IdealR MeasuredR Rho"
	

	!Initialize the matrix
	!with spin-up initial values
	
	call ini_alpha(alpha)
	
	
	!call CountPosNeg(matriz,nrows,ncols,npos,nneg)
	!call cpu_time(start)

	beta = 1.0/currentTemp
	print *, "Current Temperature T = ", currentTemp,"beta = ",beta
	!print *, 'npoints: ',npoints
	
	!Initialize some variables to zero
	call SetToZeroVariables(Magnetization,Magnetization2, &
								Magnetization4,Energy,Energy2,npoints)

	!Define the values of DeltaE knowing the Temperature
	!Only two possible values
	! 1) DeltaE(1) = exp(-4/Temp)
	! 2) DeltaE(2) = exp(-8/Temp)
	call Inicializacion_DeltaE(DeltaE,currentTemp)

	do k = 1 , average
		call ini_matriz(matriz,nrows,ncols,npos,nneg,iniseed,average)	
		!Write initial spin configurations in screen for DynamicLattice
		call WriteSpinConf(matriz,nrows,ncols)
		print *, "Average loop k = ", k,"/",average
		promediateCounter = 0
		!Choose a random point (posx,posy) where we begin calculating
		call elegir_sitio(nrows,ncols,posx,posy,iniseed,average)

		!Sum over one MonteCarlo time //dim = 1MC
		!Sum over A MonteCarlo time: A*dim 
		!In the next loop we flip a sping according to some conditions
		do i=1,MCS*dim
			!Calculate the Magnetization and Energy after equilibrium
			if ( i > EquilSteps*dim .AND. mod(i,(MCS-EquilSteps)*dim/npoints) == 0 ) then 
				!From all the EquilSteps I keep just a certain number of points (npoints)
				!Write intermediate spin configurations in screen for DynamicLattice
				!call WriteSpinConf(matriz,nrows,ncols)
				promediateCounter = promediateCounter + 1
				!print *,'Enter at time: ',i,'for ',promediateCounter
				!promediateCounter goes from 1 to npoints

				!Now I average only the equil array at current time
				call CalculationsINequilibrium (nrows,ncols,matriz,alpha,Delta_E,ip,im,currentTemp,&
					Magnetization(promediateCounter),Magnetization2(promediateCounter),Magnetization4(promediateCounter), &
					Energy(promediateCounter),Energy2(promediateCounter),k)

				call CellInteraction(matriz,nrows,ncols,ip,im,npos,k,&
					interPos(promediateCounter),InterNeg(promediateCounter),interPN(promediateCounter))
				!call WriteSpinConf(matriz,nrows,ncols)

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
				call ranmar(rvec,1,iniseed*average) 
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
			call elegir_sitio(nrows,ncols,posx,posy,iniseed,average)
			!----------------------------------------------------------------
			!-----PRINT SITES IN DYNAMIC LATTICE ----------------------------
			!do index1=1,nrows
			!	do index2=1,ncols
			!		write(*,*) index1,index2,matriz(index1,index2)
			!	end do
			!end do
			!----------------------------------------------------------------
		end do !End loop of the MC steps
	end do ! End loop of averages
	
	!Save in file the Interaction arrays
	!call prom1dReal2Int(InterPos,average,promInterPos)
	!call prom1dReal2Int(InterNeg,average,promInterNeg)
	!call prom1dReal2Int(InterPN,average,promInterPN)
	!call prom1dp(rIdeal,average,promIdealR)
	!call prom1dp(rMeasured,average,promMeasuredR)
	!call prom1dp(rho,average,promRho)
	
	!Write the results in a file
	call Write2File(beta, Magnetization,Magnetization2, &
					Magnetization4,Energy,Energy2,npoints)
	call WriteInteractions(interPos,interNeg,interPN,npos,nrows,ncols,npoints)
	!call cpu_time(finish)
    !print '("Time = ",f6.3," seconds.")',finish-start

	!Write final spin configurations in screen for DynamicLattice
	call WriteSpinConf(matriz,nrows,ncols)
	close(1)
	close(33)
	close(34)
end program kawasaki


