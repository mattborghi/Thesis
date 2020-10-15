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
	integer::ncols,iniseed
	integer::average,MCS
	integer::posx,posy,dim
	integer::npos,nneg !Sum of thes values = 1
	integer::printOPT
	integer :: index1, index2
	integer,allocatable::matriz(:,:)
	integer,allocatable::ip(:),im(:)
	integer::i,j,k 
	real,allocatable::alpha(:,:)
	real::alpha11,alpha33
	!Type interaction v2:  4N values
	!integer::interPos,interNeg,interPN
	!real,allocatable::rIdeal(:),rMeasured(:),rho(:)
	!real::rIdeal,rMeasured,rho
	real::Delta_E
	real::rvec(1)
	real(dp),allocatable::Magnetization(:), Magnetization2(:),Magnetization4(:),Energy(:),Energy2(:)
	real(dp)::MagnetizationINI=0.0,Magnetization2INI=0.0,Magnetization4INI=0.0,EnergyINI=0.0,Energy2INI=0.0
	real,dimension(20)::DeltaE
	real::beta
	character(len=100)::MagFile ='Magnetization.dat'
	character(len=100):: folderName,FileOutput='OutputFile.log'
	character(len=10):: stringL
	
	!---------------------------------------
	real::currentTemp=1.22,currentExtMagnet=0.0,alpha13=1,inimagnet=1.0
	integer::nrows=32

	ncols=nrows

	!If inimagnet = 1 all spin ups. 
	!If inimagnet = 0 half-spins up, half down
	!Else inimagnet should be a real close to 0, so that indicates the number of spin downs in proportion to spin ups.
	!ie, 0.1 in a N=20x20 system, will be 40 spins to turn down(or up) relative to the m=0 system.
	!--------------
	
	! Read in input parameters from file "ising.in"
	open(unit=11,file="ConfigurationFile",status="old",action="read")
	read(11,*);read(11,*) printOPT
	read(11,*);read(11,*) iniseed
	read(11,*);read(11,*) MCS
	read(11,*);read(11,*) average
	read(11,*);read(11,*) alpha11
	read(11,*);read(11,*) alpha33
	close(11)

	
	! Allocate the space for some important quantities.
	allocate(matriz(nrows,ncols))
	allocate(alpha(3,3))

	allocate(Magnetization(MCS))
	allocate(Magnetization2(MCS))
	allocate(Magnetization4(MCS))	
	allocate(Energy(MCS))
	allocate(Energy2(MCS))

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

	dim = nrows*ncols

	if (inimagnet /= 0.0 .AND. inimagnet/=1.0) then
		!npos = int(nrows**2/2.0 + inimagnet * nrows**2)
		!nneg = nrows**2 - npos		
		nneg = int(nrows**2*((1-inimagnet)/real(2.0)))
		npos = nrows**2 - nneg
		!print *, 'inimagnet Neither 0 nor 1'
		!print *, 'npos = ',npos,' nneg= ',nneg
	else if(inimagnet == 0.0) then
		npos = int(nrows**2/2.0)
		nneg = npos
		!print *, 'inimagnet = 0'
		!print *, 'npos = ',npos,' nneg= ',nneg
	else !inimagnet==1.0
		npos= int(nrows**2)
		nneg=0
		!print *, 'inimagnet = 1'
		!print *, 'npos = ',npos,' nneg= ',nneg
	end if

	!Create the folder name
	!Cast integer/real to string
	write(stringL,'(I3.3)') nrows
	!Merge the strings
	!ie., name: L=20 alfa 111 t=0.9 n+=0.5 p+=0.5 aver=10 mcs=500mil 
	folderName = 'L='//trim(stringL)
	!		'N+='//trim(stringNpos)//'P+='//trim(stringProbpos)//'/'//'MCS='//trim(stringMCS)//'Aver='//trim(stringAver)
	!Does the folder already exist?
	!inquire(FILE=trim(folderName)//'/.', EXIST=dir_e)

	!write(*,*) 'Does the folder ',trim(folderName),' exists?'
	!STOP 'Lo terminé'
	!if(dir_e) then
	open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),status="replace",action="write")
	write(2,*) 'Parameters in this simulation'
	write(2,*) 'L=',nrows
	write(2,*) 'npos=',npos
	write(2,*) 'nneg=',nneg
	write(2,*) 'Temp=',currentTemp
	write(2,*) 'Bext=',currentExtMagnet
	write(2,*) 'Alpha=',alpha11,' ',alpha13,' ',alpha33
	write(2,*) 'MCS=',MCS
	write(2,*) 'aver=',average
		!write(2,*) 'Directory already exists!'
	!else
		!call system('mkdir -p ' // trim(folderName) )
		!open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),status="replace",action="write")
		!write(2,*) 'Folder created!'
	!end if
	
	!MCS = dim*10
	!EquilSteps = int (0.5*MCS)
	write(2,*) 'Semilla RANMAR: ',iniseed

	if ( (npos+nneg) /= nrows**2) then
		write(2,*) 'ERROR:npos+nneg neq nros*ncols!'
		call exit(0)
	end if

	call ini_alpha(alpha,alpha11,alpha13,alpha33)

	beta = 1.0/currentTemp
	!print *, 'MCS: ',MCS
	!Initialize some variables to zero
	call SetToZeroVariables(Magnetization,Magnetization2, Magnetization4,Energy,Energy2,MCS)
	close(2) !Closing OutputFile.dat
	!Define the values of DeltaE knowing the Temperature
	!Only two possible values
	! 1) DeltaE(1) = exp(-4/Temp)
	! 2) DeltaE(2) = exp(-8/Temp)
	call Inicializacion_DeltaE(DeltaE,currentTemp)
	!open (UNIT=20, FILE=trim(folderName)//'/'//trim('data.dat'),ACTION='write',STATUS='replace')
	!close(20)
	do k = 1 , average
		!Initialize the matrix
		!with spin-up initial values
		call ini_matriz(matriz,nrows,ncols,nneg,iniseed,k)
		call CalcQuantities(nrows,ncols,matriz,Delta_E,EnergyINI,Energy2INI,MagnetizationINI,Magnetization2INI,Magnetization4INI,ip,im,alpha,k,currentExtMagnet)
		!call WriteSpinConf(matriz,nrows,ncols)
		!Write initial spin configurations in screen for DynamicLattice
		if(printOPT /= 0 .AND. printOPT /= 4) then
			call WriteSpinConf(matriz,nrows,ncols)
		end if
		!call CountPosNeg(matriz,nrows,ncols,npos,nneg,nnull)
		open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),access="append",action="write")
			write(2,*) "Average loop k = ", k,"/",average
		close(2)
		!Choose a random point (posx,posy) where we begin calculating	
		!Spinflip it is not done with probpos or probneg because of the dissipation of the spins. At low temperature 
		!npos or nneg -> 0 so it is not possible to select spins with the requiered probability.
		call elegir_sitio(nrows,ncols,posx,posy,iniseed,k)
	
		!Sum over one MonteCarlo time //dim = 1MC
		!Sum over A MonteCarlo time: A*dim 
		!In the next loop we flip a sping according to some conditions
		do i=1,MCS
			!if(mod(i,1000) == 0) then
				!print *,'MCS=',i
			!end if

			!if (i ==1 .OR. i==2 .OR. i==4 .OR. i==6 .OR. i==10 .OR. i==20 .OR. i==40 .OR. i==100 .OR. i==100000) then
			!	open (UNIT=20, FILE=trim(folderName)//'/'//trim('data.dat'),ACTION='write',access='append')
			!		do index2 = 1,nrows
			!			do index1 = 1,ncols
			!				write(20,'(I7,A,I3,A,I3,A,I3)') i,',',index1,',',index2,',',matriz(index1,index2)
			!			end do
			!		end do 
			!	close(20)
			!end if 
			do j = 1, dim

			!Calculate DeltaE for a given point (posx,posy)
			
				call CalcDeltaEnergy(nrows,ncols,posx,posy,matriz,alpha,Delta_E,ip,im,currentExtMagnet)
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
				
				call elegir_sitio(nrows,ncols,posx,posy,iniseed,k)
			!----------------------------------------------------------------
			end do !End loop dim

			!Calculate the Magnetization and Energy after equilibrium
			call CalcQuantities(nrows,ncols,matriz,Delta_E,Energy(i),Energy2(i),Magnetization(i),Magnetization2(i),Magnetization4(i),ip,im,alpha,k,currentExtMagnet)

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
						if(printOPT==5) then
							write(*,*) index1,index2,matriz(index1,index2)
						end if
					end do
				end do
			end if
			

		end do !End loop of the MC steps	

		!After finishing each run	
		!Write the results in a file	
		open (UNIT=1, FILE=trim(folderName)//'/'//trim(MagFile),ACTION='write',STATUS='replace')
		write(1,*) "Average MCS Magnetization Magnetization^2 Magnetization^4 Energy Energy^2"
		call Write2File(Magnetization,Magnetization2,Magnetization4,Energy,Energy2,MagnetizationINI,Magnetization2INI,Magnetization4INI,EnergyINI,Energy2INI,MCS,k)
		close(1)	
	
	end do ! End loop of averages

	!Write final spin configurations in screen for DynamicLattice
	if(printOPT /= 0 .AND. printOPT /= 4) then
		call WriteSpinConf(matriz,nrows,ncols)
	end if

end program kawasaki


