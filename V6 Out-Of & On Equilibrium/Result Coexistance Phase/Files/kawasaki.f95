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
	integer::nrows,ncols,iniseed
	integer::average
	integer::posx,posy,newposx,newposy,dim
	real::npos,nneg,nnull !Sum of thes values = 1
	real::probpos,probneg,probnull !Probability of picking a positive spin
	real::extMagField
	integer(kind=int64):: MCS, EquilSteps,exchangeCounter = 0!, NumofTempPoints
	integer(kind=int64):: i,j!,indexTemp !Dummy indeces
	integer::k,printOPT,spinDyn,pointStart
	integer :: index1, index2
	integer::exchangeSpin
	integer,allocatable::matriz(:,:)
	integer,allocatable::ip(:),im(:)
	integer::promediateCounter 
	integer::counter,t,alfa,prob
	real,allocatable::alpha(:,:)
	real::alpha11,alpha13,alpha33
	logical::cond
	!Type interaction v2:  4N values
	!integer::interPos,interNeg,interPN
	!real,allocatable::rIdeal(:),rMeasured(:),rho(:)
	!real::rIdeal,rMeasured,rho
	real::Delta_E,Delta_E2,Delta_E3
	real::rvec(1)
	real(dp),allocatable::Energy(:),Energy2(:),magnetANTI(:),magnetAnti2(:),magnetAnti4(:),magnetFerro(:),magnetFerro2(:),magnetFerro4(:)
	real(dp),dimension(20)::DeltaE
	real::currentTemp,beta
	character(len=100):: folderName,FileOutput='OutputFile.log',currentOutput='currentStatus.log'
	logical::dir_e
	character(len=10):: stringL,stringMCS,stringAver,stringTemp,stringNpos,stringProbpos,stringAlpha

	!real,allocatable::temps(:) !19
	!real::TempMax=0.002,TempMin=0.00001,step=0.00001
	real::temps(1) = (/0.5/)
	real::alphaVals(1) = (/-1.0/) !21
	real::posDsty(1) =(/0.5/) !6

	!print *, 'size = ', int( (TempMax-TempMin)/real(step) )
	!allocate( temps( int( (TempMax-TempMin)/real(step) ) ) )
	!do t = 1, int( (TempMax-TempMin)/real(step) )
	!	temps(t) = TempMin + step*t
		!print *, temps(t)
	!end do
	!print *,temps
	!temps = (/ 0.5,0.7,0.9,1.0,1.1,1.3,1.6,2.0,2.3,2.5,3.0/)
	!temps = (/1.0/)
	
	!alphaVals = (/3.0,2.9,2.8,2.7,2.6,2.5,2.4,2.3,2.2,2.1,2.0,1.9,1.8,1.7,1.6,1.5,1.4,1.3,1.2,1.1,1.0 /)
	!alphaVals = (/-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9 /)
	!alphaVals = (/-1.0 /)
	
	!posDsty = (/ 0.2,0.3,0.4,0.5,0.6,0.7/)
	!posDsty =(/0.5/)
	!print *, 'dim = ',size(temps)
	!do t = 1,size(temps)
	!	print*, 'Element ',t,'Value = ',temps(t)
	!end do
	! Read in input parameters from file "ising.in"
	open(unit=11,file="ConfigurationFile",status="old",action="read")
	read(11,*);read(11,*) printOPT
	read(11,*);read(11,*) spinDyn
	read(11,*);read(11,*) iniseed
	read(11,*);read(11,*) probpos
	read(11,*);read(11,*) probneg
	read(11,*);read(11,*) nrows
	read(11,*);read(11,*) ncols
	read(11,*);read(11,*) MCS
	read(11,*);read(11,*) EquilSteps
	read(11,*);read(11,*) pointStart
	read(11,*);read(11,*) average
	read(11,*);read(11,*) alpha11
	read(11,*);read(11,*) alpha33
	read(11,*);read(11,*) extMagField
	close(11)

	! Allocate the space for some important quantities.
	allocate(matriz(nrows,ncols))
	allocate(alpha(3,3))
	
	allocate(Energy(size(temps)))
	allocate(Energy2(size(temps)))
	allocate(magnetANTI(size(temps)))
	allocate(magnetAnti2(size(temps)))
	allocate(magnetAnti4(size(temps)))
	allocate(magnetFerro(size(temps)))
	allocate(magnetFerro2(size(temps)))
	allocate(magnetFerro4(size(temps)))

	!Needs to be improved. Now it is made for square lattices
	allocate(im(nrows))
	allocate(ip(nrows))

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

	!Create the folder name
	!Cast integer/real to string
	write(stringL,'(I3.3)') nrows

	dim = nrows*ncols
	folderName = 'L='//trim(stringL)
	inquire(FILE=trim(folderName)//'/.', EXIST=dir_e)
	!print *, .FALSE. .EQV. dir_e
	!call exit(0)
	if(.FALSE. .EQV. dir_e) then !Create the folder if it does not exists
		call system('mkdir -p ' // trim(folderName) )
	end if		
	!folderName = 'L='//trim(stringL)//'/'//trim(totalOutput)

	open(unit=20,file=trim(folderName)//'/'//trim(currentOutput),status="replace",action="write")
	close(20)
	open(unit=1,file=trim(folderName)//'/'//trim(FileOutput),status="replace",action="write")
	close(1)
	
	open(unit=20,file=trim(folderName)//'/'//trim(currentOutput),access="append",action="write")

	!Determine the number of holes and check for erros
	if ( (npos + nneg) > 1.0 ) then
		WRITE(20,*) 'ERROR! Wrong numbers for particle densities: npos + nneg should be <= 1.0.'
		call EXIT(0)
	else
		nnull = 1.0 -npos - nneg
	end if

	!Determine the 'picking frequency' for different particle types
	if ((probpos + probneg) > 1.0) then
		!STOP 'Wrong numbers for particle picking frequencies: probpos + probneg should be <= 1.0.'
		write(20,*) 'ERROR! Wrong numbers for particle picking frequencies: probpos + probneg should be <= 1.0.'
		call EXIT(0)
	else 
		probnull = 1.0 - probpos - probnull
	end if 
	
	!MCS = dim*10
	!EquilSteps = int (0.5*MCS)

	if ( EquilSteps >= MCS ) then
		write(20,*) 'Error! Change the value of EquilSteps in the Configuration File! It can not be greater than MCS'
		call EXIT(0)
	end if 

	write(20,*) 'Parameters in this simulation'
	write(20,*) 'L=',nrows
	!write(20,*) 'Temps=',temps
	write(20,*) 'Alphadd=',alpha11,' ','Alphauu=',' ',alpha33
	!write(20,*) 'Alphaud=', alphaVals
	!write(20,*) 'N+=',posDsty
	write(20,*) 'p+=',probpos
	write(20,*) 'MCS=',MCS
	write(20,*) 'aver=',average

	close(20)

	open (UNIT=1, FILE=trim(folderName)//'/'//trim(FileOutput),ACTION='write',STATUS='replace')
		write(1,*) "Alfa,posDsty,Temperatura,Energia,Cv,magnetANTI,SusceptAnti,cumulantAnti,magnetFerro,SusceptFerro,cumulantFerro"
	close(1)

	do prob=1,size(posDsty)
		open(unit=20,file=trim(folderName)//'/'//trim(currentOutput),access="append",action="write")
			write(20,'(A,F4.1)') 'At Density= ',posDsty(prob)
		close(20)
	do alfa = 1,size(alphaVals)
		open(unit=20,file=trim(folderName)//'/'//trim(currentOutput),access="append",action="write")
			write(20,'(A,F4.1)') 'At alfa= ',alphaVals(alfa)
		close(20)
		!Ini alpha
		call ini_alpha(alpha,alpha11,alphaVals(alfa),alpha33)
		!Initialize some variables to zero
		call SetToZeroVariables(Energy,Energy2,magnetANTI,magnetAnti2,magnetAnti4,magnetFerro,magnetFerro2,magnetFerro4,size(temps))
		!with spin-up initial values
		call ini_matriz(matriz,nrows,ncols,posDsty(prob),1.0-posDsty(prob),iniseed,k)
	do k = 1 , average

	do t=1,size(temps)
		if (k==1 ) then
		open(unit=20,file=trim(folderName)//'/'//trim(currentOutput),access="append",action="write")
			write(20,*) 'At T=',temps(t)
		close(20)
		end if

	beta = 1.0/real(temps(t))
	!write(2,*) "Current Temperature T = ", temps(t),"beta = ",beta
	!print *, 'npoints: ',npoints
	!close(20) !Closing OutputFile.dat
	!Define the values of DeltaE knowing the Temperature
	!Only two possible values
	! 1) DeltaE(1) = exp(-4/Temp)
	! 2) DeltaE(2) = exp(-8/Temp)
	call Inicializacion_DeltaE(DeltaE,temps(t))

	
		!Initialize the matrix
		!call WriteSpinConf(matriz,nrows,ncols)
		!Write initial spin configurations in screen for DynamicLattice
	
		!call CountPosNeg(matriz,nrows,ncols,npos,nneg,nnull)
		promediateCounter = 0
		!Choose a random point (posx,posy) where we begin calculating
	
			!Spinflip it is not done with probpos or probneg because of the dissipation of the spins. At low temperature 
			!npos or nneg -> 0 so it is not possible to select spins with the requiered probability.
		call elegir_sitio_SpinFlip(nrows,ncols,posx,posy,iniseed,k)
		
		!Sum over one MonteCarlo time //dim = 1MC
		!Sum over A MonteCarlo time: A*dim 
		!In the next loop we flip a sping according to some conditions
		do i=1,MCS

			do j = 1, dim

				!Calculate DeltaE for a given point (posx,posy)
				!print *,im(1),i,j,posx,posy
				call CalcDeltaEnergySpinFlip(nrows,ncols,posx,posy,matriz,Delta_E,ip,im,alpha)
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
						if ( rvec(1) < exp(-Delta_E/real(temps(t))) ) then
							matriz(posx,posy) = -matriz(posx,posy)
						end if
					end if
				end if
			
				call elegir_sitio_SpinFlip(nrows,ncols,posx,posy,iniseed,k)
				!----------------------------------------------------------------
			end do !End loop dim
		
			!Calculate the Magnetization and Energy after equilibrium
			if ( i > EquilSteps ) then !mod(i,MCS/npoints) == 0
				
				!print *,'Enter at time: ',i,'for ',promediateCounter+1
				promediateCounter = promediateCounter + 1
	
				call CalculationsINequilibrium (nrows,ncols,matriz,alpha,Delta_E,ip,im,temps(t),&
					Energy(t),Energy2(t),k,extMagField,magnetANTI(t),magnetAnti2(t),magnetAnti4(t),&
					magnetFerro(t),magnetFerro2(t),magnetFerro4(t))
	
				!Store the time desired to measure. They are not equidistant, so we need to do this
				!call WriteSpinConf(matriz,nrows,ncols)
			end if

		end do !End loop of the MC steps	
	
	end do ! End loop of temperatures

!	open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),access='append',action='write')
!		write(2,*) 'Accepted = ', exchangeCounter/real(MCS*dim*average)
!	close(2)


end do !End of average loop 
	!Write the results in a file		
	call Write2File(Energy,Energy2,magnetANTI,magnetAnti2,magnetAnti4,magnetFerro,magnetFerro2,magnetFerro4,promediateCounter,average,temps,size(temps),alphaVals(alfa),posDsty(prob),folderName,FileOutput)

end do !End loop of alpha vals
end do !End loop of density values
end program kawasaki


