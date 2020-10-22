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
	integer::posx,posy,newposx,newposy,dim,newDim,newNcols
	real::npos,nneg,nnull !Sum of thes values = 1
	real::probpos,probneg,probnull !Probability of picking a positive spin
	real::extMagField
	integer(kind=int64):: MCS, EquilSteps,spintaken=0,ccont=0 !, NumofTempPoints
	integer(kind=int64):: i,j,counter!,indexTemp !Dummy indeces
	integer(kind=int64),allocatable::contador(:)
	integer::printOPT,spinDyn,pointStart
	integer :: index1, index2
	integer::exchangeSpin,k
	integer,allocatable::matriz(:,:)
	integer,allocatable::ip(:),im(:)
	real(dp),allocatable::f1(:,:,:),f2(:,:,:),f3(:,:,:),f4(:,:,:),dsty(:,:,:)
	integer::promediateCounter
	integer::IDfile1,IDfile2,IDfile3
	real,allocatable::alpha(:,:)
	real::alpha11,alpha13,alpha33
	logical::cond
	!integer(dp),allocatable::InterPos(:),InterNeg(:),InterPN(:),InterZero(:)
	!Type interaction v2:  4N values
	!integer(dp),allocatable::InterPos2(:),InterNeg2(:),InterPN2(:),InterZero2(:),InterPZ(:),InterNZ(:)
	!integer::interPos,interNeg,interPN
	!real,allocatable::rIdeal(:),rMeasured(:),rho(:)
	!real::rIdeal,rMeasured,rho
	real::Delta_E
	real::rvec(1)
	real(dp),allocatable::Magnetization(:), Magnetization2(:),Magnetization4(:),Energy(:),Energy2(:)
	real(dp),dimension(20)::DeltaE
	real::currentTemp,beta
	character(len=100)::MagFile ='Magnetization20L.dat', EnergFile = 'Energy20L.dat'
	character(len=100)::InterFile='Inter20L.dat',InterFile2 = 'Roughness.dat'
	character(len=100):: folderName,FileOutput='OutputFile.log',spinFile="spinSteps.dat"
	character(len=10):: stringL,stringM,stringTemp

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
	allocate(matriz(ncols,nrows))
	allocate(alpha(3,3))

	allocate(Magnetization(npoints))
	allocate(Magnetization2(npoints))
	allocate(Magnetization4(npoints))
	allocate(Energy(npoints))
	allocate(Energy2(npoints))

	!Needs to be improved. Now it is made for square lattices
	!Just with this because I have pbc at posy = 0 and posy = nrows
	allocate(im(nrows))
	allocate(ip(nrows))

	!allocate(InterPN(npoints))
	!allocate(InterPos(npoints))
	!allocate(InterNeg(npoints))
	!allocate(InterZero(npoints))

	!ntimes = 20
	allocate(f1(2,npoints,ncols))
	allocate(f2(2,npoints,ncols))
	allocate(f3(2,npoints,ncols))
	allocate(f4(2,npoints,ncols))
	allocate(dsty(2,npoints,ncols))

	allocate(contador(npoints))
	contador(:) = 0
	!Allocate interaction arrays v2
	!allocate(InterPN2(npoints))
	!allocate(InterPos2(npoints))
	!allocate(InterNeg2(npoints))
	!allocate(InterZero2(npoints))
	!allocate(interPZ(npoints))
	!allocate(interNZ(npoints))

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
	write(stringM,'(I4.4)') ncols
	write(stringTemp,'(F5.1)') currentTemp

	!Merge the strings
	!ie., name: L=20 alfa 111 t=0.9 n+=0.5 p+=0.5 aver=10 mcs=500mil 
	folderName = 'L='//trim(stringL)//'/'//'Temp='//trim(stringTemp)

	open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),status="replace",action="write")
	write(2,*) 'Parameters in this simulation'
	write(2,*) 'L=',nrows
	write(2,*) 'M=',ncols
	write(2,*) 'Temp=',currentTemp
	write(2,*) 'Alpha=',alpha11,' ',alpha13,' ',alpha33
	write(2,*) 'N+=',npos
	write(2,*) 'p+=',probpos
	write(2,*) 'MCS=',MCS
	write(2,*) 'aver=',average

	!Determine the number of holes and check for erros
	if ( (npos + nneg) > 1.0 ) then
		write(2,*) 'Wrong numbers for particle densities: npos + nneg should be <= 1.0.'
		call EXIT(0)
	else
		nnull = 1.0 -npos - nneg
	end if

	!Determine the 'picking frequency' for different particle types
	if ((probpos + probneg) > 1.0) then
		write(2,*) 'Wrong numbers for particle picking frequencies: probpos + probneg should be <= 1.0.'
		call EXIT(0)
	else 
		probnull = 1.0 - probpos - probnull
	end if 
	
	!MCS = dim*10
	!EquilSteps = int (0.5*MCS)

	if ( EquilSteps >= MCS ) then
		write(2,*) 'Change the value of EquilSteps in the Configuration File! It can not be greater than MCS'
		call EXIT(0)
	end if 

	if ( npoints > MCS ) then
		write(2,*) 'Change the value of npoints in the Configuration File! It can not be greater than MCS'
		call EXIT(0)
	end if 

	write(2,*) 'Semilla RANMAR: ',iniseed
	!1MCS = dim = nrows*ncols = 400
	! 10 000 MCS = 4M 
	write(2,*) "MCS = ",MCS, " EquilSteps = ", EquilSteps

	!If the file exists overwrite erase it.
	!FileIDs
	IDfile1 = 34
	IDfile2 = 35
	IDfile3 = 36
	open (UNIT=1, FILE=trim(folderName)//'/'//trim(MagFile),ACTION='write',STATUS='replace')
	write(1,*) "Average",",","Tiempo",",","AverMagnetization",",","AverMagnetization^2",",","Suceptibility",",","Cumulant"
	open (UNIT=33, FILE=trim(folderName)//'/'//trim(EnergFile),ACTION='write',STATUS='replace')
	write(33,*) "Average",",","Tiempo",",","  AverEnergy",",","  AverEnergy^2",",","  C_v"
	open (UNIT=IDfile1,FILE=trim(folderName)//'/'//trim(InterFile),ACTION='write',STATUS='replace')
	write(IDfile1,*) "Tiempo,indexTime,ncolIndex,f1up,f1down,f2up,f2down,f3up,f3down,f4up,f4down,densityUP,densityDOWN"
	!write(IDfile1,*) "Average",",","Tiempo",",","InterPos",",","InterNeg",",","InterPN",",","InterZero",",", &
	!		"IdealR",",","MeasuredR",","," Rho"
	open (UNIT=IDfile2, FILE=trim(folderName)//'/'//trim(InterFile2),ACTION='write',STATUS='replace')
	write(IDfile2,*) "Tiempo,indexTime,<m>,<m2>,Wf1,d<m>,d<m2>,Wdsty"
	open (UNIT=IDfile3, FILE=trim(folderName)//'/'//trim(spinFile),ACTION='write',STATUS='replace')
	write(IDfile3,*) "Tiempo,x,y,spin"
	close(IDfile3)

	open(unit=37,file=trim(folderName)//'/test.dat',status="replace",action="write")
	close(37)


	call ini_alpha(alpha,alpha11,alpha13,alpha33)

	beta = 1.0/currentTemp
	write(2,*) "Current Temperature T = ", currentTemp,"beta = ",beta
	!print *, 'npoints: ',npoints
	!Initialize some variables to zero
	call SetToZeroVariables(Magnetization,Magnetization2,Magnetization4,Energy,Energy2, &
							f1,f2,f3,f4, dsty, ncols, npoints)
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
		newNcols = 10
		if (spinDyn == 1) then
			!Spinflip it is not done with probpos or probneg because of the dissipation of the spins. At low temperature 
			!npos or nneg -> 0 so it is not possible to select spins with the requiered probability.
			call elegir_sitio_SpinFlip(nrows,ncols,posx,posy,iniseed,matriz,k,newNcols)
		else !spinDyn==2 
			call elegir_sitio_Kawasaki(nrows,ncols,posx,posy,iniseed,probpos,probneg,matriz,k,newNcols,folderName,FileOutput)
		end if

		!Sum over one MonteCarlo time //dim = 1MC
		!Sum over A MonteCarlo time: A*dim 
		!In the next loop we flip a sping according to some conditions
		newDim = newNcols*nrows
		do i=1,MCS
			j = 1
			if (j <= ncols) then
				if ( matriz(ncols/2 - newNcols/2 +1,j) == -1 .OR. matriz(ncols/2 + newNcols/2,j) == 1  ) then
					newNcols = newNcols + 10
					newDim = nrows*newNcols
					j = ncols
				end if
				j = j + 1
			end if
			!if(printOPT == 0 ) then
			!	print *,'MCS = ',i, 'ncols =',newNcols
			!end if
			!print *,'pos values between', ncols/2 - newNcols/2,' and ',ncols/2+ newNcols/2
			if(newNcols >= ncols) then
				!print *, newNcols,ncols
				open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),access="append",action="write")
				write(2,*) 'Error: cannot have newNcols > ncols'
				close(2)
				call EXIT(0)
			end if
			do j = 1,newDim
				ccont=ccont + 1
				!print *,'pos', posx, posy
				!if (pointStart == 0) then
				!	if (mod(counter,(MCS)*dim/ntimes) == 0) then
				!		cond = .TRUE.
				!	else
				!		cond = .FALSE.
				!	end if
				!else !pointStart == 1
				!	if (i > EquilSteps*dim .AND. mod(i,(MCS-EquilSteps)*dim/npoints) == 0) then
				!		cond = .TRUE.
				!	else
				!		cond = .FALSE.
				!	end if
				!end if

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
					
				else !if(posy /= 1 .AND. posy /= 2 .AND. posy /= ncols .AND. posy /= (ncols -1) ) then 
				!spinDyn == 2
					!Calculate the position of the neighbour to exchange
					call ranmar(rvec,1,iniseed)
					!A number between 1 and 4
					!If the spin chosen is in the right x-border
					if (posx == ncols) then
					!A number between 1 and 3. cant be 4 because doesnt have that neighbour
						rvec(1) = floor( 3 * rvec(1) ) + 1
					!If the spin chosen is in the left x-border
					else if(posx == 1) then
					!A number between 2 and 4 is chosen. cant be 1 because doesnt have that neighbour.
						rvec(1) = floor( 3 * rvec(1) ) + 2
					else !If the spin chosen is not in the x-borders. All 4 values are possible.
						!print *,'Here'
						rvec(1) = floor( 4 * rvec(1) ) + 1
					end if
					!print*,"rvec: ",rvec(1)
					if ( rvec(1) == 1) then
						newposx = posx-1
						newposy = posy
						!print*,"En uno"
					else if ( rvec(1) == 2 ) then
						newposx = posx
						newposy = ip(posy)
						!print*,"En dos"
					else if( rvec(1) == 3) then
						newposx = posx
						newposy = im(posy)
					else !rvec(1) == 4
						newposx = posx+1
						newposy = posy
					end if
					!print *, posx, posx+1,posx-1,posy,ip(posy),im(posy)
					!print *, 'rvec', rvec(1)
					!print *,'new pos', newposx, newposy
					!Dont compute DeltaE for Kawasaki if both spins to flip have the same sign.
					if (matriz(posx,posy) /= matriz(newposx,newposy)) then
						!print *, "Delta E: ",Delta_E 
						call CalcDeltaEnergyKawasaki(nrows,ncols,posx,posy,matriz,alpha,Delta_E,ip,im,iniseed,newposx,newposy,extMagField)
						! ..Flipping Spin..
						!If the new Energy spin-state is negative with flip the spin
						!If not it will depend on the probability exp(-DeltaE/Temp) > random point
						if (Delta_E <= 0) then
							call exchange(nrows,ncols,posx,posy,matriz,ip,newposx,newposy)
							spintaken = spintaken + 1
							!print*,"Fue menor a cero"
						else
							call ranmar(rvec,1,iniseed*average) 
							if (Delta_E == int(Delta_E) .AND. Delta_E<=20) then
								if (rvec(1) < DeltaE(int(Delta_E))) then
									call exchange(nrows,ncols,posx,posy,matriz,ip,newposx,newposy)
									spintaken = spintaken + 1
								end if
							else	
								if ( rvec(1) < exp(-Delta_E/currentTemp) ) then
									call exchange(nrows,ncols,posx,posy,matriz,ip,newposx,newposy)
									spintaken = spintaken + 1
								end if
							end if
						end if
					end if
				end if

				!Choose a new random point (posx,posy) where we begin calculating
				if (spinDyn == 1) then
					call elegir_sitio_SpinFlip(nrows,ncols,posx,posy,iniseed,matriz,k,newNcols)
				else !spinDyn==2 
					call elegir_sitio_Kawasaki(nrows,ncols,posx,posy,iniseed,probpos,probneg,matriz,k,newNcols,folderName,FileOutput)
				end if
				
				!----------------------------------------------------------------
				!-----PRINT SITES IN DYNAMIC LATTICE ----------------------------
				if(printOPT == 2 .OR. (printOPT == 3 .AND. ( mod(i,(MCS)*dim/10) == 0) ) ) then
					do index1=1,nrows
						do index2=1,ncols
							write(*,*) index2,index1,matriz(index2,index1)
						end do
					end do
				end if
				!----------------------------------------------------------------
				if ( (printOPT == 0 .OR. printOPT == 4) .AND. (mod(i,(MCS)/10) == 0) .AND. j==newDim) then
					open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),access='append',action='write')		
					counter = counter + 1
					write(2,*) 'Step ',counter, '/ 10'
					close(2)
				end if
			end do !Resized monte carlo step

			if (  mod(i,MCS/3) == 0 .AND. k==1) then
				!open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),access='append',action='write')		
				!write(2,*) 'Enter at t= ',i	
				call WriteSpinSteps(IDfile3,matriz,ncols,nrows,i,folderName,spinFile,ip,im)
				!close(2)
			end if											

			!Calculate the Magnetization and Energy after equilibrium
			if ( mod(i,MCS/npoints) == 0 ) then 
				
				!if ( printOPT == 0 ) then
				!	print *,'Enter at time: ',i,'for ',promediateCounter+1
				!end if
				promediateCounter = promediateCounter + 1
				call CalculationsINequilibrium (nrows,ncols,matriz,alpha,Delta_E,ip,im,currentTemp,&
					Magnetization(promediateCounter),Magnetization2(promediateCounter),Magnetization4(promediateCounter),&
					Energy(promediateCounter),Energy2(promediateCounter),k,extMagField)


				call CellInteraction(matriz,nrows,ncols,ip,im,k,&
				f1(:,promediateCounter,:),f2(:,promediateCounter,:),f3(:,promediateCounter,:),f4(:,promediateCounter,:),&
					dsty(:,promediateCounter,:))
				!call CellInteraction(matriz,nrows,ncols,ip,im,k,&
				!	InterPos(promediateCounter),InterNeg(promediateCounter),InterPN(promediateCounter),&
				!	InterZero(promediateCounter))

				!Store the time desired to measure. They are not equidistant, so we need to do this
				contador(promediateCounter) = i
				!call CellInteractionV2(matriz,nrows,ncols,ip,im,k,&
				!	InterPos2(promediateCounter),InterNeg2(promediateCounter),InterPN2(promediateCounter),&
				!	InterZero2(promediateCounter),InterPZ(promediateCounter),interNZ(promediateCounter))
				if (printOPT /= 0 .AND. printOPT/= 4) then
			   		call WriteSpinConf(matriz,nrows,ncols)
			   	end if
			end if

		end do !End loop of the MC steps	
	end do ! End loop of averages

	open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),access='append',action='write')		
	write(2,*) 'Acceptance ratio = ',spintaken/real(ccont)
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
					Magnetization4,Energy,Energy2,npoints,average)
	
	call WriteInteractions(IDfile1,f1,f2,f3,f4,dsty,nrows,ncols,npoints,average,contador)

	!call WriteInteractions(IDfile1,interPos,interNeg,interPN,InterZero,npos,nrows,ncols,npoints,average)

	!Write the results of interactions v2 in a file
	!call WriteInteractionsV2(IDfile2,interPos2,interNeg2,interPN2,InterZero2,InterPZ,InterNZ,nrows,ncols,npoints,average)

	call CalcRoughness(IDfile2,f1,dsty,nrows,ncols,npoints,average,contador)
	!Write final spin configurations in screen for DynamicLattice
	if(printOPT /= 0 .AND. printOPT /= 4) then
		call WriteSpinConf(matriz,nrows,ncols)
	end if

	close(1)
	close(33)
	close(IDfile1)
	close(IDfile2)
end program kawasaki


