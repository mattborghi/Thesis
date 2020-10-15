!---------------------------------------------------------------------------------
!Author: Matias Pablo, Borghi Orue
!Title: 2D Monte Carlo simulation of the Modified Ising Model 
!Version: 3 
!Comments:  $ gfortran ranmar_seed.f95 subroutines.f95 kawasaki.f95 -o runv1
!DynamicLattice: $./runv1 | ./dllinux.exe -1 -nx 20 -ny 20 -grid -z -1 1
!INPUTS: ConfigurationFile
!OUTPUTS: Three dat files: 'Magnetization30L.dat', 'Energy30L.dat' and 'Inter.dat'
!----------------------------------------------------------------------------------
program kawasaki

use subroutines
use rm
use iso_fortran_env
implicit none
	
	integer, parameter :: dp = selected_real_kind(15, 307) !For double precision
	integer::nrows,ncols,iniseed,npoints
	integer::average
	integer::posx,posy,newposx,newposy,dim
	real::npos,nneg !Sum of thes values = 1
	real::probpos !Probability of picking a positive spin
	real::extMagField
	integer(kind=int64):: MCS, EquilSteps!, NumofTempPoints
	integer(kind=int64):: i!,indexTemp !Dummy indeces
	integer::k,printOPT,spinDyn
	integer :: index1, index2
	integer::exchangeSpin
	integer,allocatable::matriz(:,:)
	integer,allocatable::ip(:),im(:)
	integer::promediateCounter 
	integer::IDfile1,IDfile2
	real,allocatable::alpha(:,:)
	real::alpha11,alpha13,alpha33
	integer(dp),allocatable::InterPos(:),InterNeg(:),InterPN(:)
	!Type interaction v2:  4N values
	integer(dp),allocatable::InterPos2(:),InterNeg2(:),InterPN2(:)
	!integer::interPos,interNeg,interPN
	!real,allocatable::rIdeal(:),rMeasured(:),rho(:)
	!real::rIdeal,rMeasured,rho
	real::Delta_E
	real::rvec(1)
	real(dp),allocatable::Magnetization(:), Magnetization2(:),Magnetization4(:),Energy(:),Energy2(:)
	real(dp),dimension(20)::DeltaE
	real::currentTemp,beta
	character(len=100)::MagFile ='Magnetization20L.dat', EnergFile = 'Energy20L.dat'
	character(len=100)::InterFile='Inter20L.dat',InterFile2='Inter220L.dat'

	! Read in input parameters from file "ising.in"
	open(unit=11,file="ConfigurationFile",status="old",action="read")
	read(11,*);read(11,*) printOPT
	read(11,*);read(11,*) spinDyn
	read(11,*);read(11,*) iniseed
	read(11,*);read(11,*) npos
	read(11,*);read(11,*) nneg
	read(11,*);read(11,*) probpos
	read(11,*);read(11,*) nrows
	read(11,*);read(11,*) ncols
	read(11,*);read(11,*) MCS
	read(11,*);read(11,*) EquilSteps
	read(11,*);read(11,*) currentTemp
	read(11,*);read(11,*) npoints
	read(11,*);read(11,*) average
	read(11,*);read(11,*) alpha11
	read(11,*);read(11,*) alpha13
	read(11,*);read(11,*) alpha33
	read(11,*);read(11,*) extMagField
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

	allocate(InterPN(npoints))
	allocate(InterPos(npoints))
	allocate(InterNeg(npoints))

	!Allocate interaction arrays v2
	allocate(InterPN2(npoints))
	allocate(InterPos2(npoints))
	allocate(InterNeg2(npoints))

	!allocate(rIdeal(average))
	!allocate(rMeasured(average))
	!allocate(rho(average))

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

	!If the file exists overwrite erase it.
	!FileIDs
	IDfile1 = 34
	IDfile2 = 35
	open (UNIT=1, FILE=MagFile,ACTION='write',STATUS='replace')
	write(1,*) "Tiempo AverMagnetization AverMagnetization^2 Suceptibility Cumulant"
	open (UNIT=33, FILE=EnergFile,ACTION='write',STATUS='replace')
	write(33,*) "Tiempo AverEnergy AverEnergy^2 C_v"
	open (UNIT=IDfile1, FILE=InterFile,ACTION='write',STATUS='replace')
	write(IDfile1,*) "Tiempo InterPos InterNeg InterPN IdealR MeasuredR Rho"
	open (UNIT=IDfile2, FILE=InterFile2,ACTION='write',STATUS='replace')
	write(IDfile2,*) "Tiempo InterPos2 InterNeg2 InterPN2"
		

	call ini_alpha(alpha,alpha11,alpha13,alpha33)

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
		!Initialize the matrix
		!with spin-up initial values
		call ini_matriz(matriz,nrows,ncols,npos,nneg,iniseed,k)
		!Write initial spin configurations in screen for DynamicLattice
		if(printOPT == 1) then
			call WriteSpinConf(matriz,nrows,ncols)
		end if
		!call CountPosNeg(matriz,nrows,ncols,npos,nneg)
	
		print *, "Average loop k = ", k,"/",average
		promediateCounter = 0
		!Choose a random point (posx,posy) where we begin calculating
		if (spinDyn == 1) then
			call elegir_sitio_SpinFlip(nrows,ncols,posx,posy,iniseed,matriz,k)
		else !spinDyn==2 
			call elegir_sitio_Kawasaki(nrows,ncols,posx,posy,iniseed,probpos,matriz,k)
		end if

		!Sum over one MonteCarlo time //dim = 1MC
		!Sum over A MonteCarlo time: A*dim 
		!In the next loop we flip a sping according to some conditions
		do i=1,MCS*dim

			!Calculate the Magnetization and Energy after equilibrium
			if ( i > EquilSteps*dim .AND. mod(i,(MCS-EquilSteps)*dim/npoints) == 0 ) then !mod(i,MCS/npoints) == 0
				
				!print *,'Enter at time: ',i,'for ',promediateCounter+1
				promediateCounter = promediateCounter + 1
				call CalculationsINequilibrium (nrows,ncols,matriz,alpha,Delta_E,ip,im,currentTemp,&
					Magnetization(promediateCounter),Magnetization2(promediateCounter),Magnetization4(promediateCounter),&
					Energy(promediateCounter),Energy2(promediateCounter),k,extMagField)

				call CellInteraction(matriz,nrows,ncols,ip,im,npos,k,&
					interPos(promediateCounter),interNeg(promediateCounter),interPN(promediateCounter))
				
				call CellInteractionV2(matriz,nrows,ncols,ip,im,npos,k,&
					interPos2(promediateCounter),interNeg2(promediateCounter),interPN2(promediateCounter))
				!call WriteSpinConf(matriz,nrows,ncols)
			end if
			!Calculate DeltaE for a given point (posx,posy)
			if (spinDyn == 1) then !SpinFlip Dynamics
				call CalcDeltaEnergySpinFlip(nrows,ncols,posx,posy,matriz,alpha,Delta_E,ip,im,iniseed,extMagField)
				!print *,Delta_E,'for i=',i
				if (Delta_E <= 0) then
					matriz(posx,posy) = -matriz(posx,posy)
					
				else 
					call ranmar(rvec,1,iniseed*average) 
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
				
			else !spinDyn == 2
				!print *, "Delta E: ",Delta_E 
				call CalcDeltaEnergyKawasaki(nrows,ncols,posx,posy,matriz,alpha,Delta_E,ip,im,iniseed,newposx,newposy,extMagField)
				! ..Flipping Spin..
				!If the new Energy spin-state is negative with flip the spin
				!If not it will depend on the probability exp(-DeltaE/Temp) > random point
				if (Delta_E <= 0) then
					call exchange(nrows,ncols,posx,posy,matriz,ip,newposx,newposy)
					!print*,"Fue menor a cero"
				else
					call ranmar(rvec,1,iniseed*average) 
					if (Delta_E == int(Delta_E) .AND. Delta_E<=20) then
						if (rvec(1) < DeltaE(int(Delta_E))) then
							call exchange(nrows,ncols,posx,posy,matriz,ip,newposx,newposy)
						end if
					else	
						if ( rvec(1) < exp(-Delta_E/currentTemp) ) then
							call exchange(nrows,ncols,posx,posy,matriz,ip,newposx,newposy)
						end if
					end if
				end if
			end if

			!Choose a new random point (posx,posy) where we begin calculating
			if (spinDyn == 1) then
				call elegir_sitio_SpinFlip(nrows,ncols,posx,posy,iniseed,matriz,k)
			else !spinDyn==2 
				call elegir_sitio_Kawasaki(nrows,ncols,posx,posy,iniseed,probpos,matriz,k)
			end if
			
			!----------------------------------------------------------------
			!-----PRINT SITES IN DYNAMIC LATTICE ----------------------------
			if(printOPT == 2) then
				do index1=1,nrows
					do index2=1,ncols
						write(*,*) index1,index2,matriz(index1,index2)
					end do
				end do
			end if
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
	call WriteInteractions(IDfile1,interPos,interNeg,interPN,npos,nrows,ncols,npoints)

	!Write the results of interactions v2 in a file
	call WriteInteractions(IDfile2,interPos2,interNeg2,interPN2,npos,nrows,ncols,npoints)

	!Write final spin configurations in screen for DynamicLattice
	if(printOPT == 1) then
		call WriteSpinConf(matriz,nrows,ncols)
	end if

	close(1)
	close(33)
	close(34)
end program kawasaki


