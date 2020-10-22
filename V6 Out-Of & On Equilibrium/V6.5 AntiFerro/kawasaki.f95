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
	integer(kind=int64):: MCS, EquilSteps,exchangeCounter = 0!, NumofTempPoints
	integer(kind=int64):: i,j!,indexTemp !Dummy indeces
	integer::k,printOPT,spinDyn,pointStart
	integer :: index1, index2
	integer::exchangeSpin
	integer,allocatable::matriz(:,:)
	integer,allocatable::ip(:),im(:)
	integer::promediateCounter 
	integer::IDfile1,IDfile2,counter
	integer(kind=int64),allocatable::contador(:)
	real,allocatable::alpha(:,:)
	real::alpha11,alpha13,alpha33
	logical::cond
	!Type interaction v2:  4N values
	real(dp),allocatable::f1(:,:),f2(:,:),f3(:,:),f4(:,:)
	!integer::interPos,interNeg,interPN
	!real,allocatable::rIdeal(:),rMeasured(:),rho(:)
	!real::rIdeal,rMeasured,rho
	real::Delta_E,Delta_E2,Delta_E3
	real::rvec(1)
	real(dp),allocatable::Magnetization(:), Magnetization2(:),Magnetization4(:),Energy(:),Energy2(:),magnetANTI(:),magnetANTI2(:),magnetANTI4(:),magnetFerro(:),magnetFerro2(:),magnetFerro4(:)
	real(dp),dimension(20)::DeltaE
	real::currentTemp,beta
	character(len=100)::MagFile ='Magnetization20L.dat', EnergFile = 'Energy20L.dat'
	character(len=100)::InterFile='Inter20L.dat',spinStepsFile='spinSteps.dat'
	character(len=100):: folderName,FileOutput='OutputFile.log'
	logical::dir_e
	character(len=10):: stringL,stringMCS,stringAver,stringTemp,stringNpos,stringProbpos,stringAlpha

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
	read(11,*);read(11,*) currentTemp
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

	allocate(Magnetization(npoints))
	allocate(Magnetization2(npoints))
	allocate(Magnetization4(npoints))	
	allocate(Energy(npoints))
	allocate(Energy2(npoints))
	allocate(magnetANTI(npoints))
	allocate(magnetANTI2(npoints))
	allocate(magnetANTI4(npoints))
	allocate(magnetFerro(npoints))
	allocate(magnetFerro2(npoints))
	allocate(magnetFerro4(npoints))

	!Needs to be improved. Now it is made for square lattices
	allocate(im(nrows))
	allocate(ip(nrows))

	allocate(f1(2,npoints)) !Second index is fup.up f(2,:), First fdown.down f(1,:)
	allocate(f2(2,npoints))
	allocate(f3(2,npoints))
	allocate(f4(2,npoints))

	allocate(contador(npoints))
	contador(:) = 0
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

	
	!Create the folder name
	!Cast integer/real to string
	write(stringL,'(I3.3)') nrows
	write(stringMCS,'(I8.8)') MCS
	write(stringAver,'(I2)') average
	write(stringAlpha,'(3F3.0)') alpha11,alpha13,alpha33
	write(stringTemp,'(F5.1)') currentTemp
	write(stringNpos,'(F3.1)') npos
	write(stringProbpos,'(F3.1)') probpos
	!Merge the strings
	!ie., name: L=20 alfa 111 t=0.9 n+=0.5 p+=0.5 aver=10 mcs=500mil 
	folderName = 'L='//trim(stringL)//'/'//'Temp='//trim(stringTemp)
	!		'N+='//trim(stringNpos)//'P+='//trim(stringProbpos)//'/'//'MCS='//trim(stringMCS)//'Aver='//trim(stringAver)
	!Does the folder already exist?
	!inquire(FILE=trim(folderName)//'/.', EXIST=dir_e)

	!write(*,*) 'Does the folder ',trim(folderName),' exists?'
	!STOP 'Lo terminé'
	!if(dir_e) then
	open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),status="replace",action="write")
	write(2,*) 'Parameters in this simulation'
	write(2,*) 'L=',nrows
	write(2,*) 'Temp=',currentTemp
	write(2,*) 'Alpha=',alpha11,' ',alpha13,' ',alpha33
	write(2,*) 'N+=',npos
	write(2,*) 'p+=',probpos
	write(2,*) 'MCS=',MCS
	write(2,*) 'aver=',average
		!write(2,*) 'Directory already exists!'
	!else
		!call system('mkdir -p ' // trim(folderName) )
		!open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),status="replace",action="write")
		!write(2,*) 'Folder created!'
	!end if
	
	!Determine the number of holes and check for erros
	if ( (npos + nneg) > 1.0 ) then
		WRITE(2,*) 'ERROR! Wrong numbers for particle densities: npos + nneg should be <= 1.0.'
		call EXIT(0)
	else
		nnull = 1.0 -npos - nneg
	end if

	!Determine the 'picking frequency' for different particle types
	if ((probpos + probneg) > 1.0) then
		!STOP 'Wrong numbers for particle picking frequencies: probpos + probneg should be <= 1.0.'
		write(2,*) 'ERROR! Wrong numbers for particle picking frequencies: probpos + probneg should be <= 1.0.'
		call EXIT(0)
	else 
		probnull = 1.0 - probpos - probnull
	end if 
	
	!MCS = dim*10
	!EquilSteps = int (0.5*MCS)

	if ( EquilSteps >= MCS ) then
		write(2,*) 'Error! Change the value of EquilSteps in the Configuration File! It can not be greater than MCS'
		call EXIT(0)
	end if 

	write(2,*) 'Semilla RANMAR: ',iniseed
	!1MCS = dim = nrows*ncols = 400
	! 10 000 MCS = 4M 
	write(2,*) "MCS = ",MCS, " EquilSteps = ", EquilSteps!,"printOPT = ",printOPT

	!If the file exists overwrite erase it.
	!FileIDs
	IDfile1 = 34
	IDfile2 = 35
	open (UNIT=1, FILE=trim(folderName)//'/'//trim(MagFile),ACTION='write',STATUS='replace')
	write(1,*) "Tiempo",",","AverMagnetization",",","AverMagnetization^2",",","Suceptibility",",","Cumulant",",","magnetANTI",",","magnetANTI2",",","magnetANTI4",",","SusceptANTI",",","CumulantANTI",",","magnetFerro",",","magnetFerro2",",","magnetFerro4",",","SusceptFerro",",","CumulantFerro"
	open (UNIT=33, FILE=trim(folderName)//'/'//trim(EnergFile),ACTION='write',STATUS='replace')
	write(33,*) "Tiempo",",","  AverEnergy",",","  AverEnergy^2",",","  C_v"
	open (UNIT=IDfile2, FILE=trim(folderName)//'/'//trim(InterFile),ACTION='write',STATUS='replace')
	write(IDfile2,*) "Tiempo,indexTime,f1up,f1down,f2up,f2down,f3up,f3down,f4up,f4down"
	open (UNIT=IDfile1, FILE=trim(folderName)//'/'//trim(spinStepsFile),ACTION='write',STATUS='replace')

	call ini_alpha(alpha,alpha11,alpha13,alpha33)

	beta = 1.0/currentTemp
	write(2,*) "Current Temperature T = ", currentTemp,"beta = ",beta
	!print *, 'npoints: ',npoints
	!Initialize some variables to zero
	call SetToZeroVariables(Magnetization,Magnetization2, Magnetization4,Energy,Energy2,magnetANTI,magnetANTI2,magnetANTI4,magnetFerro,magnetFerro2,magnetFerro4,f1,f2,f3,f4,npoints)
	close(2) !Closing OutputFile.dat
	!Define the values of DeltaE knowing the Temperature
	!Only two possible values
	! 1) DeltaE(1) = exp(-4/Temp)
	! 2) DeltaE(2) = exp(-8/Temp)
	call Inicializacion_DeltaE(DeltaE,currentTemp)

	do k = 1 , average
		!Initialize the matrix
		!with spin-up initial values
		call ini_matriz(matriz,nrows,ncols,npos,nneg,iniseed,k)
		!call WriteSpinConf(matriz,nrows,ncols)
		!Write initial spin configurations in screen for DynamicLattice
		if(printOPT /= 0 .AND. printOPT /= 4) then
			call WriteSpinConf(matriz,nrows,ncols)
		end if
		!call CountPosNeg(matriz,nrows,ncols,npos,nneg,nnull)
		open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),access="append",action="write")
		write(2,*) "Average loop k = ", k,"/",average
		close(2)
		promediateCounter = 0
		counter = 0
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
		do i=1,MCS

			do j = 1, dim

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
					
				else !spinDyn == 2 SPIN EXCHANGE DYNAMICS
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
						!print*,"En tres"
						newposx = posx
						newposy = ip(posy)
					else
						!print *,"En cuatro"
						newposx = posx
						newposy = im(posy)
					end if
					!print *,'Spin elegido s= ',matriz(posx,posy),'en',posx,posy,'Nuevo s=',matriz(newposx,newposy),'en',newposx,newposy
					!Dont compute DeltaE for Kawasaki if both spins to flip have the same sign.
					if (matriz(posx,posy) /= matriz(newposx,newposy)) then
						!print *, "Delta E: ",Delta_E 
						call CalcDeltaEnergyKawasaki(nrows,ncols,posx,posy,matriz,alpha,Delta_E,ip,im,iniseed,newposx,newposy,extMagField)
						! ..Flipping Spin..
						!If the new Energy spin-state is negative with flip the spin
						!If not it will depend on the probability exp(-DeltaE/Temp) > random point
						!print *,'DeltaE: ',Delta_E
						!if (abs(Delta_E) == 16.0) then
							!print *, 'Delta E:', Delta_E
							!print *, 'Old = ',posx,posy,'New = ',newposx,newposy
							!print *, matriz(posx,posy),matriz(ip(posx),posy),matriz(im(posx),posy),matriz(posx,ip(posy)),matriz(posx,im(posy))
							!print *, matriz(newposx,newposy),matriz(ip(newposx),newposy),matriz(im(newposx),newposy), & 
							!	matriz(newposx,ip(newposy)),matriz(newposx,im(newposy))
							!if (j>30) then
							!	STOP 'stop'
							!end if
						!	call CalcEnergy(nrows,ncols,posx,posy,matriz,alpha,Delta_E,ip,im,extMagField)
						!	call CalcEnergy(nrows,ncols,newposx,newposy,matriz,alpha,Delta_E2,ip,im,extMagField) 
						!	Delta_E = Delta_E + Delta_E2 + &
						!		matriz(posx,posy)*matriz(newposx,newposy)*alpha(matriz(posx,posy)+2,matriz(newposx,newposy)+2)
						!	print *,'Einicial:',Delta_E

						!	call NeighboursEnergy(nrows,ncols,posx,posy,matriz,alpha,Delta_E3,ip,im)
						!	Delta_E3 = (Delta_E3 - extMagField)*matriz(newposx,newposy) 
						!	print *,Delta_E3,matriz(posx,posy),matriz(ip(posx),posy),matriz(im(posx),posy),matriz(posx,ip(posy)),matriz(posx,im(posy))
						!	call NeighboursEnergy(nrows,ncols,newposx,newposy,matriz,alpha,Delta_E2,ip,im)
						!	Delta_E2 = (Delta_E2 - extMagField)*matriz(posx,posy)
						!	print *,Delta_E2
						!	Delta_E2 = Delta_E2 + Delta_E3 + &
						!		matriz(posx,posy)*matriz(newposx,newposy)*alpha(matriz(posx,posy)+2,matriz(newposx,newposy)+2) 
						!	print *,'Efinal:',Delta_E2
						!	STOP 'Encontre uno'
						!end if
						
						if (Delta_E <= 0) then
							call exchange(nrows,ncols,posx,posy,matriz,ip,newposx,newposy)
							!print*,"Fue menor a cero"
							exchangeCounter = exchangeCounter + 1
						else
							call ranmar(rvec,1,iniseed*average) 
							if (Delta_E == int(Delta_E) .AND. Delta_E<=20) then
								if (rvec(1) < DeltaE(int(Delta_E))) then
									call exchange(nrows,ncols,posx,posy,matriz,ip,newposx,newposy)
									exchangeCounter = exchangeCounter + 1
								end if
							else	
								if ( rvec(1) < exp(-Delta_E/currentTemp) ) then
									call exchange(nrows,ncols,posx,posy,matriz,ip,newposx,newposy)
									exchangeCounter = exchangeCounter + 1
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
					!print *,'Spin elegido s= ',matriz(posx,posy)
				end if
				!----------------------------------------------------------------
			end do !End loop dim

			if (pointStart == 0) then
				if (mod(i,(MCS/npoints)) == 0) then
					cond = .TRUE.
				else
					cond = .FALSE.
				end if
			else !pointStart == 1
				if (i > EquilSteps .AND. mod(i,(MCS-EquilSteps)/npoints) == 0) then
					cond = .TRUE.
				else
					cond = .FALSE.
				end if
			end if


			!Calculate the Magnetization and Energy after equilibrium
			if ( cond ) then !mod(i,MCS/npoints) == 0
				
				!print *,'Enter at time: ',i,'for ',promediateCounter+1
				promediateCounter = promediateCounter + 1
				call CalculationsINequilibrium (nrows,ncols,matriz,alpha,Delta_E,ip,im,currentTemp,&
					Magnetization(promediateCounter),Magnetization2(promediateCounter),Magnetization4(promediateCounter),&
					Energy(promediateCounter),Energy2(promediateCounter),k,extMagField,magnetANTI(promediateCounter),magnetANTI2(promediateCounter),magnetANTI4(promediateCounter),&
					magnetFerro(promediateCounter),magnetFerro2(promediateCounter),magnetFerro4(promediateCounter) )
				
				call CellInteraction(matriz,nrows,ncols,ip,im,k,&
					f1(:,promediateCounter),f2(:,promediateCounter),f3(:,promediateCounter),f4(:,promediateCounter))

				!Store the time desired to measure. They are not equidistant, so we need to do this
				contador(promediateCounter) = i
				!call WriteSpinConf(matriz,nrows,ncols)
			end if

			!----------------------------------------------------------------
			!-----PRINT SITES IN DYNAMIC LATTICE ----------------------------
			if(printOPT == 2 .OR. (printOPT == 3 .AND. ( mod(i,(MCS)/10) == 0) ) ) then
				do index2=1,nrows
					do index1=1,ncols
						write(*,*) index1,index2,matriz(index1,index2)
					end do
				end do
			end if
			!printOPT == 4 saves the intermediate values in a file
			if ( (printOPT == 4 .OR. printOPT==5) .AND.  (mod(i,(MCS)/10) == 0)  ) then
				do index2 = 1,nrows
					do index1 = 1,ncols
						if(index2 == nrows .AND. index1 == ncols) then
							write(IDfile1,'(I2.1,/)',advance='no') matriz(index1,index2)
						else
							write(IDfile1,'(I2.1)',advance='no') matriz(index1,index2)
							write(IDfile1,'(A)',advance='no') ","
						end if
						if(printOPT==5) then
							write(*,*) index1,index2,matriz(index1,index2)
						end if
					end do
				end do
			end if
			if ( (printOPT == 0 .OR. printOPT == 4) .AND. (mod(i,(MCS)/10) == 0)) then
				open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),access='append',action='write')		
					counter = counter + 1
					write(2,*) 'Step ',counter, '/ 10'
				close(2)
			end if

		end do !End loop of the MC steps	
	
	end do ! End loop of averages

	open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),access='append',action='write')
		write(2,*) 'Accepted = ', exchangeCounter/real(MCS*dim*average)
	close(2)

	
	!Save in file the Interaction arrays
	!call prom1dReal2Int(InterPos,average,promInterPos)
	!call prom1dReal2Int(InterNeg,average,promInterNeg)
	!call prom1dReal2Int(InterPN,average,promInterPN)
	!call prom1dp(rIdeal,average,promIdealR)
	!call prom1dp(rMeasured,average,promMeasuredR)
	!call prom1dp(rho,average,promRho)

	!Write the results in a file		
	call Write2File(beta, Magnetization,Magnetization2, &
					Magnetization4,Energy,Energy2,magnetANTI,magnetANTI2,magnetANTI4,magnetFerro,magnetFerro2,magnetFerro4,npoints,average)
	!Write the results of interactions v2 in a file
	call WriteInteractions(IDfile2,f1,f2,f3,f4,nrows,ncols,npoints,average,contador)

	!Write final spin configurations in screen for DynamicLattice
	if(printOPT /= 0 .AND. printOPT /= 4) then
		call WriteSpinConf(matriz,nrows,ncols)
	end if

	close(1)
	close(33)
	close(IDfile1)
	close(IDfile2)
end program kawasaki


