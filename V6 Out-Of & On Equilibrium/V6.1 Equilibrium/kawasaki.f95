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
	real::npos,nneg,nnull !Sum of thes values = 1
	real::probpos,probneg,probnull !Probability of picking a positive spin
	real::extMagField
	integer(kind=int64):: MCS, EquilSteps
	integer(kind=int64):: i!,indexTemp !Dummy indeces
	integer::k,printOPT,spinDyn,pointStart
	integer::t, NumofTempPoints
	real::Tmax,Tmin,currentTemp,beta
	integer :: index1, index2
	integer::exchangeSpin
	integer,allocatable::matriz(:,:)
	integer,allocatable::ip(:),im(:)
	integer::promediateCounter 
	integer::IDfile1,IDfile2
	real,allocatable::alpha(:,:)
	real::alpha11,alpha13,alpha33
	logical::cond
	integer(dp)::InterPos,InterNeg,InterPN,InterZero
	!Type interaction v2:  4N values
	integer(dp)::InterPos2,InterNeg2,InterPN2,InterZero2,InterPZ,InterNZ
	!integer::interPos,interNeg,interPN
	!real,allocatable::rIdeal(:),rMeasured(:),rho(:)
	!real::rIdeal,rMeasured,rho
	real::Delta_E
	real::rvec(1)
	real(dp)::Magnetization, Magnetization2,Magnetization4,Energy,Energy2
	real(dp),dimension(20)::DeltaE

	character(len=100)::MagFile ='Magnetization20L.dat', EnergFile = 'Energy20L.dat'
	character(len=100)::InterFile='Inter20L.dat',InterFile2='Inter220L.dat'
	character(len=100)::TimeFile='TimeAver.dat'

	! Read in input parameters from file "ising.in"
	open(unit=11,file="ConfigurationFile",status="old",action="read")
	read(11,*);read(11,*) printOPT
	read(11,*);read(11,*) spinDyn
	read(11,*);read(11,*) iniseed
	read(11,*);read(11,*) npos
	read(11,*);read(11,*) nneg
	read(11,*);read(11,*) probpos
	read(11,*);read(11,*) probneg
	read(11,*);read(11,*) nrows
	read(11,*);read(11,*) ncols
	read(11,*);read(11,*) MCS
	read(11,*);read(11,*) EquilSteps
	read(11,*);read(11,*) Tmin
	read(11,*);read(11,*) Tmax
	read(11,*);read(11,*) NumofTempPoints
	read(11,*);read(11,*) pointStart
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

	!Needs to be improved. Now it is made for square lattices
	allocate(im(nrows))
	allocate(ip(nrows))

	!Fill ip & im arrays
	do i = 1,nrows
		ip(i) = i+1
		im(i) = i-1 
	end do
	ip(nrows) = 1
	im(1) = nrows

	dim = nrows*ncols

	!Determine the number of holes and check for erros
	if ( (npos + nneg) > 1.0 ) then
		STOP 'Wrong numbers for particle densities: npos + nneg should be <= 1.0.'
	else
		nnull = 1.0 -npos - nneg
	end if

	!Determine the 'picking frequency' for different particle types
	if ((probpos + probneg) > 1.0) then
		STOP 'Wrong numbers for particle picking frequencies: probpos + probneg should be <= 1.0.'
	else 
		probnull = 1.0 - probpos - probnull
	end if 
	
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
	write(1,*) "Temperature,Magnetization,Magnetization^2,Suceptibility,Cumulant,Average"
	open (UNIT=33, FILE=EnergFile,ACTION='write',STATUS='replace')
	write(33,*) "Temperature, Energy,Energy^2,C_v,Average"
	open (UNIT=IDfile1, FILE=InterFile,ACTION='write',STATUS='replace')
	write(IDfile1,*) "Temperature, Tiempo ,InterPos, InterNeg, InterPN, InterZero, IdealR, MeasuredR, Rho"
	open (UNIT=IDfile2, FILE=InterFile2,ACTION='write',STATUS='replace')
	write(IDfile2,*) "Temperature, Tiempo, InterPos2 ,InterNeg2, InterPN2, InterZero ,InterPN, InterNZ"
		

	call ini_alpha(alpha,alpha11,alpha13,alpha33)
	!print *, 'npoints: ',npoints


	!PromediateCounter is a variable that goes from 1 to npoints
	!Counting the time varible
	!promediateCounter = 0

	do k = 1 , average
		print *, "Average loop k = ", k,"/",average
		
		!Initialize the matrix
		!with spin-up initial values
		call ini_matriz(matriz,nrows,ncols,npos,nneg,iniseed,k)
		!call CountPosNeg(matriz,nrows,ncols,npos,nneg,nnull)		

		do t = 0, (NumofTempPoints-1)
			!Write initial spin configurations in screen for DynamicLattice
			if(printOPT == 1) then
				call WriteSpinConf(matriz,nrows,ncols)
			end if
			!Initialize some variables to zero
			call SetToZeroVariables(Magnetization,Magnetization2, &
									Magnetization4,Energy,Energy2,&
									InterPos,InterNeg,InterPN,InterZero,&
									InterPos2,InterNeg2,InterPN2,InterZero2,InterPZ,InterNZ)

			!Change the Temperature
			call NewTemp(t,currentTemp,Tmax,Tmin,NumofTempPoints)
			if (currentTemp /= 0) then 
				beta = 1.0/currentTemp
			else
				STOP 'NULL Temperature values are not allowed!!'
			end if
			print *, "Current Temperature T = ", currentTemp,"beta = ",beta

			!Define the values of DeltaE knowing the Temperature
			!Only two possible values
			! 1) DeltaE(1) = exp(-4/Temp)
			! 2) DeltaE(2) = exp(-8/Temp)
			call Inicializacion_DeltaE(DeltaE,currentTemp)

		
			!Choose a random point (posx,posy) where we begin calculating
			if (spinDyn == 1) then
				!Spinflip it is not done with probpos or probneg because of the dissipation of the spins. At low temperature 
				!npos or nneg -> 0 so it is not possible to select spins with the requiered probability.
				call elegir_sitio_SpinFlip(nrows,ncols,posx,posy,iniseed,matriz,k)
			else !spinDyn==2 
				call elegir_sitio_Kawasaki(nrows,ncols,posx,posy,iniseed,probpos,probneg,matriz,k)
			end if

			!Sum over one MonteCarlo time //dim = 1MC
			!Sum over A MonteCarlo time: A*dim 
			!In the next loop we flip a sping according to some conditions
			do i=1,MCS*dim
				!Check if equilibrium of out-of-equilibrium MC simulation
				if (pointStart == 0) then
					!Out-of-equilibrium MC Simulations
					if (mod(i,(MCS)*dim/npoints) == 0) then
						cond = .TRUE.
					else
						cond = .FALSE.
					end if
				else !pointStart == 1
					!Equilibrium MC Simulations
					if (i > EquilSteps*dim .AND. mod(i,(MCS-EquilSteps)*dim/npoints) == 0) then
						cond = .TRUE.
					else
						cond = .FALSE.
					end if
				end if

				!Calculate the Magnetization and Energy
				if ( cond ) then !mod(i,MCS/npoints) == 0
					
					!print *,'Enter at time: ',i,'for ',promediateCounter+1
					promediateCounter = promediateCounter + 1
					call CalculationsINequilibrium (nrows,ncols,matriz,alpha,Delta_E,ip,im,&
						Magnetization,Magnetization2,Magnetization4,&
						Energy,Energy2,extMagField)

					!call CellInteraction(matriz,nrows,ncols,ip,im,k,&
					!	InterPos,InterNeg,InterPN,&
					!	InterZero)
					
					!call CellInteractionV2(matriz,nrows,ncols,ip,im,k,&
					!	InterPos2,InterNeg2,InterPN2,&
					!	InterZero2,InterPZ,interNZ)
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
					!Calculate the position of the neighbour to exchange
					call ranmar(rvec,1,iniseed)
					!A number between 1 and 4
					rvec(1) = floor( 4 * rvec(1) ) + 1
					!print*,"rvec: ",rvec(1)
					if ( rvec(1) == 1) then
						newposx = ip(posx)
						newposy = posy
						!print*,"En uno"
					else if ( rvec(1) == 2 ) then
						newposx = im(posx)
						newposy = posy
						!print*,"En dos"
					else if( rvec(1) == 3) then
						newposx = posx
						newposy = ip(posy)
					else
						newposx = posx
						newposy = im(posy)
					end if

					!Dont compute DeltaE for Kawasaki if both spins to flip have the same sign.
					if (matriz(posx,posy) /= matriz(newposx,newposy)) then
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
				end if

				!Choose a new random point (posx,posy) where we begin calculating
				if (spinDyn == 1) then
					call elegir_sitio_SpinFlip(nrows,ncols,posx,posy,iniseed,matriz,k)
				else !spinDyn==2 
					call elegir_sitio_Kawasaki(nrows,ncols,posx,posy,iniseed,probpos,probneg,matriz,k)
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

			!Write the results in a file
			call Write2File(currentTemp, Magnetization,Magnetization2, &
							Magnetization4,Energy,Energy2,npoints,k)
			!call WriteInteractions(IDfile1,interPos,interNeg,interPN,InterZero,npos,nrows,ncols,npoints,currentTemp)

			!Write the results of interactions v2 in a file
			!call WriteInteractionsV2(IDfile2,interPos2,interNeg2,interPN2,InterZero2,InterPZ,InterNZ,nrows,ncols,npoints,currentTemp)

		end do ! End loop of temperatures
	
	!Save in file the Interaction arrays
	!call prom1dReal2Int(InterPos,average,promInterPos)
	!call prom1dReal2Int(InterNeg,average,promInterNeg)
	!call prom1dReal2Int(InterPN,average,promInterPN)
	!call prom1dp(rIdeal,average,promIdealR)
	!call prom1dp(rMeasured,average,promMeasuredR)
	!call prom1dp(rho,average,promRho)

	end do !End loop of averages
	
	!Write final spin configurations in screen for DynamicLattice
	if(printOPT == 1) then
		call WriteSpinConf(matriz,nrows,ncols)
	end if


	close(1)
	close(33)
	close(IDfile1)
	close(IDfile2)
end program kawasaki


