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
	integer :: index1, index2,index3
	integer::exchangeSpin,matrizInv
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
	real::energia
	real::rvec(1)
	real:: magnetWhite,magnetBlack,antiMagnet,Magnetizacion
	integer::white,black
	real(dp)::magnetAbsFerroINI=0.0,magnetFerroINI=0.0,magnetFerro2INI=0.0,magnetFerro4INI=0.0,EnergyINI=0.0,Energy2INI=0.0
	real(dp)::magnetAntiINI=0.0,magnetAnti2INI=0.0,magnetAnti4INI=0.0
	real(dp),allocatable:: Energy(:),Energy2(:),magnetAnti(:),magnetAnti2(:),magnetAnti4(:)
	real(dp),allocatable:: magnetAbsFerro(:),magnetFerro(:),magnetFerro2(:),magnetFerro4(:)
	real(dp),dimension(20)::DeltaE
	real(dp)::Delta_E
	real::currentTemp,beta
	real::invDim
	character(len=100):: MagFile,folderName,FileOutput='OutputFile.log',currentOutput='currentStatus.log'
	logical::dir_e
	character(len=10):: stringL,stringTemp,stringAlpha

	!real,allocatable::temps(:) !19
	!real::TempMax=6,TempMin=1,step=0.1
	!real::temps(20) = (/0.02252,0.02254,0.02256,0.02258,0.02262,0.02264,0.02266,0.02268,0.02272,0.02274,0.02276,0.02278,0.02282,0.02284,0.02286,0.02288,0.02292,0.02294,0.02296,0.02298/)
	real::temps(1) = (/2.38/)
	real::alphaVals(1) = (/1/) !21
	real::posDsty(1) =(/0.5/) !6
	real :: inimagnet=1.0

	ncols = 20
	nrows = ncols

	!print *, '# of temperature values = ', int( (TempMax-TempMin)/real(step) )
	!allocate( temps( int( (TempMax-TempMin)/real(step) ) ) )
	!do t = 1, int( (TempMax-TempMin)/real(step) )
	!	temps(t) = TempMax - step*t
	!	print *, temps
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
	!	print*, 'Element ',t,'Value = ',temps
	!end do
	! Read in input parameters from file "ising.in"
	open(unit=11,file="ConfigurationFile",status="old",action="read")
	read(11,*);read(11,*) printOPT
	read(11,*);read(11,*) spinDyn
	read(11,*);read(11,*) iniseed
	read(11,*);read(11,*) probpos
	read(11,*);read(11,*) probneg
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

	!Needs to be improved. Now it is made for square lattices
	allocate(im(nrows))
	allocate(ip(nrows))

	allocate(magnetAbsFerro(MCS))
	allocate(magnetFerro(MCS))
	allocate(magnetFerro2(MCS))
	allocate(magnetFerro4(MCS))	
	allocate(Energy(MCS))
	allocate(Energy2(MCS))
	allocate(magnetAnti(MCS))
	allocate(magnetAnti2(MCS))
	allocate(magnetAnti4(MCS))	

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

	dim = nrows*ncols
	folderName = 'L='//trim(stringL)
	inquire(FILE=trim(folderName)//'/.', EXIST=dir_e)
	!print *, .FALSE. .EQV. dir_e
	!call exit(0)
	if(.FALSE. .EQV. dir_e) then !Create the folder if it does not exists
		call system('mkdir -p ' // trim(folderName) )
	end if		
	!folderName = 'L='//trim(stringL)//'/'//trim(totalOutput)

!	open(unit=20,file=trim(folderName)//'/'//trim(currentOutput),status="replace",action="write")
!	close(20)
!	open(unit=1,file=trim(folderName)//'/'//trim(FileOutput),status="replace",action="write")
!	close(1)

	write(stringTemp,'(F7.4)') temps(1)
	write(stringAlpha,'(F7.2)') alphaVals(1)
	MagFile = 'a='//trim(stringAlpha)//'T='//trim(stringTemp)//'.dat'
	call StripSpaces(MagFile)
	
!	open(unit=20,file=trim(folderName)//'/'//trim(currentOutput),access="append",action="write")
	!Determine the number of holes and check for erros
	
	!MCS = dim*10
	!EquilSteps = int (0.5*MCS)

!	write(20,*) 'Parameters in this simulation'
!	write(20,*) 'L=',nrows
	!write(20,*) 'Temps=',temps
!	write(20,*) 'Alphadd=',alpha11,' ','Alphauu=',' ',alpha33
	!write(20,*) 'Alphaud=', alphaVals
	!write(20,*) 'N+=',posDsty
!	write(20,*) 'p+=',probpos
!	write(20,*) 'MCS=',MCS
!	write(20,*) 'aver=',average

!	close(20)

	!open (UNIT=1, FILE=trim(folderName)//'/'//trim(FileOutput),ACTION='write',STATUS='replace')
	!	write(1,*) "Alfa,posDsty,Temperatura,Energia,Cv,magnetAnti,SusceptAnti,cumulantAnti,magnetFerro,SusceptFerro,cumulantFerro"
	!close(1)
	do alfa = 1,size(alphaVals)
	!	open(unit=20,file=trim(folderName)//'/'//trim(currentOutput),access="append",action="write")
	!		write(20,'(A,F4.1)') 'At alfa= ',alphaVals(alfa)
	!	close(20)
		!Ini alpha
		!call ini_alpha(alpha,alpha11,alphaVals(alfa),alpha33)
		alpha(1,1) = 1
		alpha(1,3) = alphaVals(1)
		alpha(3,1) = alphaVals(1)
		alpha(3,3) = 1
		!Initialize some variables to zero
		!call SetToZeroVariables(Energy,Energy2,magnetAnti,magnetAnti2,magnetAnti4,magnetFerro,magnetFerro2,magnetFerro4,size(temps))		

	do t=1,size(temps)
	!	if (k==1 ) then
	!	open(unit=20,file=trim(folderName)//'/'//trim(currentOutput),access="append",action="write")
			!write(*,*) 'At T=',temps(t)
	!	close(20)
	!	end if
	beta = 1.0/real(temps(t))

	Energy(:) = 0.0
	Energy2(:) = 0.0
	magnetAnti(:) = 0.0
	magnetFerro(:) = 0.0
	magnetAbsFerro(:) = 0.0
	magnetAnti2(:) = 0.0
	magnetAnti4(:) = 0.0
	magnetFerro2(:) = 0.0
	magnetFerro4(:) = 0.0
	EnergyINI = 0.0
	Energy2INI = 0.0
	magnetAntiINI = 0.0
	magnetAnti2INI = 0.0
	magnetAnti4INI = 0.0
	magnetFerroINI = 0.0
	magnetFerro2INI = 0.0
	magnetFerro4INI = 0.0
	magnetAbsFerroINI = 0.0
	!write(2,*) "Current Temperature T = ", temps,"beta = ",beta
	!print *, 'npoints: ',npoints
	!close(20) !Closing OutputFile.dat
	!Define the values of DeltaE knowing the Temperature
	!Only two possible values
	! 1) DeltaE(1) = exp(-4/Temp)
	! 2) DeltaE(2) = exp(-8/Temp)
	!call Inicializacion_DeltaE(DeltaE,temps)
	do k = 1,average

		!with spin-up initial values
		!call ini_matriz(matriz,nrows,ncols,iniseed,k)
		!do i=1,ncols
		!	do j = 1,nrows
		!		if (ran1(5) < 0.5) then
		!			matriz(i,j) = 1
		!		else 
		!			matriz(i,j) = -1
		!		end if
		!	end do
		!end do
		do index1=1,ncols
			do index2 = 1,nrows
				matriz(index1,index2) = 1	
			end do
		end do

		!Now increment the number of nneg
		index3 = nneg
		!If k=0 then m=1
		!If k=1 o
		do while (index3 /= 0) 	
			!Return a random position in matrix and flip it if its not already a down spin
			index1 = nint( (ncols-1) * ran1(5) ) + 1
			index2 = nint( (nrows-1) * ran1(5) ) + 1
			if (matriz(index1,index2) == 1) then
				matriz(index1,index2) = -1
				index3 = index3 - 1
			end if 
		end do	
		! **********QUANTITIES AT t=0 *************
		energia = 0.0
		magnetWhite = 0.0
		magnetBlack = 0.0
		white = 0
		black = 0
		antiMagnet = 0.0
		do index1 = 1,nrows
		do index2 = 1,ncols
			energia = energia & 
			- matriz(index1,index2)*( matriz(ip(index1),index2) * alpha(matriz(index1,index2)+2 , matriz(ip(index1),index2)+2) &
			+ matriz(im(index1),index2) * alpha(matriz(index1,index2)+2 , matriz(im(index1),index2)+2) &
			+ matriz(index1,ip(index2)) * alpha(matriz(index1,index2)+2 , matriz(index1,ip(index2))+2) &
			+ matriz(index1,im(index2)) * alpha(matriz(index1,index2)+2 , matriz(index1,im(index2))+2) ) 
		!print *, 'DeltaE: ',Delta_E
			if ( (mod(index1,2) == 0 .AND. mod(index2,2) == 0) .OR.(mod(index1,2) /= 0 .AND. mod(index2,2) /= 0) ) then
				magnetBlack = magnetBlack + matriz(index1,index2)
				black = black + 1
			else
				magnetWhite = magnetWhite + matriz(index1,index2)
				white = white + 1
			end if 
		enddo
		enddo
		energia = energia/real(2)!(2.0*ncols*nrows)
		EnergyINI = EnergyINI + energia
		Energy2INI = Energy2INI + energia**2
		!print *, 'Energy: ',energia, 'Energy2: ',energia**2

		!-----AntiFerro quantities----------------
		!<mB>
		magnetBlack = magnetBlack / real(black)
		!<mW>
		magnetWhite = magnetWhite / real(white)
		!inter 
		antiMagnet = abs(magnetBlack - magnetWhite) / real(2)
		!<mANTI>
		magnetAntiINI = magnetAntiINI + antiMagnet

		magnetAnti2INI = magnetAnti2INI + antiMagnet**2

		magnetAnti4INI = magnetAnti4INI + antiMagnet**4
		!------------------------------------------------
		!------------------------------------------------
		Magnetizacion = sum(matriz(1:ncols,1:nrows))!/real(dim)
		!<m>F
		magnetFerroINI = magnetFerroINI + Magnetizacion

		magnetAbsFerroINI = magnetAbsFerroINI + abs(Magnetizacion)

		magnetFerro2INI = magnetFerro2INI + Magnetizacion**2

		magnetFerro4INI = magnetFerro4INI + Magnetizacion**4

		!Initialize the matrix
		!call WriteSpinConf(matriz,nrows,ncols)
		!Write initial spin configurations in screen for DynamicLattice
	
		!call CountPosNeg(matriz,nrows,ncols,npos,nneg,nnull)
		!promediateCounter = 0
		!Choose a random point (posx,posy) where we begin calculating
	
		!Spinflip it is not done with probpos or probneg because of the dissipation of the spins. At low temperature 
		!npos or nneg -> 0 so it is not possible to select spins with the requiered probability.
		

		!Sum over one MonteCarlo time //dim = 1MC
		!Sum over A MonteCarlo time: A*dim 
		!In the next loop we flip a sping according to some conditions
		do i=1,MCS

			!if (mod(i,MCS/10) == 0) then
				!print *, 'Current step: ',i,'/',MCS
				!do index1 = 1,ncols
				!do index2 = 1,nrows
				!write(*,*) index1,index2,matriz(index1,index2)
				!enddo
				!enddo
			!end if

			
			do j = 1, dim

				!call elegir_sitio_SpinFlip(nrows,ncols,posx,posy,iniseed,k)
				posx = nint( (ncols-1) * ran1(5) ) + 1
				posy = nint( (nrows-1) * ran1(5) ) + 1

				!Calculate DeltaE for a given point (posx,posy)
				!print *,im(1),i,j,posx,posy
				
				!call CalcDeltaEnergySpinFlip(nrows,ncols,posx,posy,matriz,Delta_E,ip,im,alpha)
				matrizInv = -matriz(posx,posy)
	
				Delta_E = -matrizInv* &
				( matriz(ip(posx),posy)*alpha(matrizInv+2,matriz(ip(posx),posy)+2) &
				+ matriz(im(posx),posy)*alpha(matrizInv+2,matriz(im(posx),posy)+2) &
			    + matriz(posx,ip(posy))*alpha(matrizInv+2,matriz(posx,ip(posy))+2) &
			    + matriz(posx,im(posy))*alpha(matrizInv+2,matriz(posx,im(posy))+2) ) &
				+ matriz(posx,posy)* &
				( matriz(ip(posx),posy)*alpha(matriz(posx,posy)+2,matriz(ip(posx),posy)+2) &
				+ matriz(im(posx),posy)*alpha(matriz(posx,posy)+2,matriz(im(posx),posy)+2) &
			    + matriz(posx,ip(posy))*alpha(matriz(posx,posy)+2,matriz(posx,ip(posy))+2) &
			    + matriz(posx,im(posy))*alpha(matriz(posx,posy)+2,matriz(posx,im(posy))+2) )


				!print *, 'DeltaE: ',Delta_E
				if (-beta*Delta_E > dlog( ran1(5)+1.0d-10 ) )then
					matriz(posx,posy) = -matriz(posx,posy)
				end if

!				if (Delta_E <= 0) then
!					matriz(posx,posy) = -matriz(posx,posy)
!				else 
!					call ranmar(rvec,1,iniseed*average) 
!					if (Delta_E == int(Delta_E) .AND. Delta_E<=20) then
!						if (rvec(1) < DeltaE(int(Delta_E))) then
!								matriz(posx,posy) = -matriz(posx,posy)
!						end if
!					else	
!						if ( rvec(1) < exp(-beta*Delta_E ) ) then
!							matriz(posx,posy) = -matriz(posx,posy)
!						end if
!					end if
!				end if
			
				!call elegir_sitio_SpinFlip(nrows,ncols,posx,posy,iniseed,k)
				
				!----------------------------------------------------------------
			end do !End loop dim
		!Calculate the Magnetization and Energy after equilibrium
			!if ( i > EquilSteps) then !mod(i,MCS/npoints) == 0
				!print *,'Enter at time: ',i,'for ',promediateCounter+1
				!promediateCounter = promediateCounter + 1
	
				!call calculationsInEquilibrium (nrows,ncols,matriz,alpha,ip,im,temps,&
				!	Energy,Energy2,k,extMagField,magnetAnti,magnetAnti2,magnetAnti4,&
				!	magnetFerro,magnetFerro2,magnetFerro4)
				energia = 0.0
				magnetWhite = 0.0
				magnetBlack = 0.0
				white = 0
				black = 0
				antiMagnet = 0.0
				do index1 = 1,nrows
				do index2 = 1,ncols
					energia = energia & 
					- matriz(posx,posy)*( matriz(ip(posx),posy) * alpha(matriz(posx,posy)+2 , matriz(ip(posx),posy)+2) &
					+ matriz(im(posx),posy) * alpha(matriz(posx,posy)+2 , matriz(im(posx),posy)+2) &
	    			+ matriz(posx,ip(posy)) * alpha(matriz(posx,posy)+2 , matriz(posx,ip(posy))+2) &
	    			+ matriz(posx,im(posy)) * alpha(matriz(posx,posy)+2 , matriz(posx,im(posy))+2) ) 
				!print *, 'DeltaE: ',Delta_E
					if ( (mod(index1,2) == 0 .AND. mod(index2,2) == 0) .OR.(mod(index1,2) /= 0 .AND. mod(index2,2) /= 0) ) then
						magnetBlack = magnetBlack + matriz(index1,index2)
						black = black + 1
					else
						magnetWhite = magnetWhite + matriz(index1,index2)
						white = white + 1
					end if 
				enddo
				enddo
				energia = energia/real(2.0)!/(2.0*ncols*nrows)
				Energy(i) = Energy(i) + energia
				Energy2(i) = Energy2(i) + energia**2
				!print *, 'Energy: ',energia, 'Energy2: ',energia**2

				!-----AntiFerro quantities----------------
				!<mB>
				magnetBlack = magnetBlack / real(black)
				!<mW>
				magnetWhite = magnetWhite / real(white)
				!inter 
				antiMagnet = abs(magnetBlack - magnetWhite) / real(2)
				!<mANTI>
				magnetAnti(i) = magnetAnti(i) + antiMagnet

				magnetAnti2(i) = magnetAnti2(i) + antiMagnet**2

				magnetAnti4(i) = magnetAnti4(i) + antiMagnet**4
				!------------------------------------------------
				!------------------------------------------------
				Magnetizacion = sum(matriz(1:ncols,1:nrows))!/real(dim)
				!<m>F
				magnetFerro(i) = magnetFerro(i) + Magnetizacion

				magnetAbsFerro(i) = magnetAbsFerro(i) + abs(Magnetizacion)

				magnetFerro2(i) = magnetFerro2(i) + Magnetizacion**2

				magnetFerro4(i) = magnetFerro4(i) + Magnetizacion**4

				!Store the time desired to measure. They are not equidistant, so we need to do this
				!call WriteSpinConf(matriz,nrows,ncols)
			!end if


			
		end do !End loop of the MC steps	
	invDim = 1.0/real(nrows*ncols*k)
	open(unit=1,file=trim(folderName)//'/'//trim(MagFile),status="replace",action="write")
	write(1,*) "average MCS AbsMagnetization Magnetization Magnetization2 Magnetization4 antiMagnetization antiMagnetization2 antiMagnetization4 Energy Energy2"
	write(1,'(I6,x,I6,x,F12.6,x,F12.6,x,F16.6,x,F16.6,x,F9.6,x,F9.6,x,F9.6,x,F9.6,x,F16.6)') k,0+1,magnetAbsFerroINI*invDim,(magnetFerroINI)*invDim,magnetFerro2INI,magnetFerro4INI*invDim,magnetAntiINI*invDim,magnetAnti2INI,magnetAnti4INI*invDim,EnergyINI*invDim,Energy2INI*invDim
	do j = 1,MCS		
		write(1,'(I6,x,I6,x,F12.6,x,F12.6,x,F16.6,x,F16.6,x,F9.6,x,F9.6,x,F9.6,x,F9.6,x,F16.6)') k,j+1,magnetAbsFerro(j)*invDim,magnetFerro(j)*invDim,magnetFerro2(j)*invDim,magnetFerro4(j)*invDim,magnetAnti(j)*invDim,magnetAnti2(j),magnetAnti4(j)*invDim,Energy(j)*invDim,Energy2(j)*invDim
	end do
	close(1)
	end do ! End loop of averages

	end do ! End loop of temperatures

!	open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),access='append',action='write')
!		write(2,*) 'Accepted = ', exchangeCounter/real(MCS*dim*average)
!	close(2)

	!Write the results in a file		
	!call Write2File(Energy,Energy2,magnetAnti,magnetAnti2,magnetAnti4,magnetFerro,magnetFerro2,magnetFerro4,promediateCounter,average,temps,size(temps),alphaVals(alfa),posDsty(prob),folderName,FileOutput)
end do !End loop of alpha vals
end program kawasaki


