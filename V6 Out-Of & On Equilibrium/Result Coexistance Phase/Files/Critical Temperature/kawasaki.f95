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
	real(dp)::Energy,Energy2,magnetAnti,magnetAnti2,magnetAnti4
	real(dp):: magnetAbsFerro,magnetFerro,magnetFerro2,magnetFerro4
	real(dp),dimension(20)::DeltaE
	real(dp)::Delta_E
	real::currentTemp,beta
	character(len=100):: folderName,FileOutput='OutputFile.log',currentOutput='currentStatus.log'
	logical::dir_e
	character(len=10):: stringL

	!real,allocatable::temps(:) !19
	!real::TempMax=4,TempMin=0.5,step=0.1
	!real::temps(5) = (/2.25,2.26,2.27,2.28,2.29/)
	!real::temps(5) = (/2.255,2.265,2.275,2.285,2.295/)
	real::temps(1) = (/4/)
	real::alphaVals(1) = (/-3/) !21
	real::posDsty(1) =(/0.5/) !6

	ncols = 150
	nrows = ncols

	!print *, '# of temperature values = ', int( (TempMax-TempMin)/real(step) )
	!allocate( temps( int( (TempMax-TempMin)/real(step) ) ) )
	!do t = 1, int( (TempMax-TempMin)/real(step) )
	!	temps(t) = TempMin + step*t
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

!	open(unit=20,file=trim(folderName)//'/'//trim(currentOutput),status="replace",action="write")
!	close(20)
!	open(unit=1,file=trim(folderName)//'/'//trim(FileOutput),status="replace",action="write")
!	close(1)
	
!	open(unit=20,file=trim(folderName)//'/'//trim(currentOutput),access="append",action="write")

	!Determine the number of holes and check for erros
	!MCS = dim*10
	!EquilSteps = int (0.5*MCS)

	if ( EquilSteps >= MCS ) then
!		write(20,*) 'Error! Change the value of EquilSteps in the Configuration File! It can not be greater than MCS'
		call EXIT(0)
	end if 

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
	!	write(1,*) "Alfa,posDsty,Temperatura,Energia,Cv,magnetANTI,SusceptAnti,cumulantAnti,magnetFerro,SusceptFerro,cumulantFerro"
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
		!call SetToZeroVariables(Energy,Energy2,magnetANTI,magnetAnti2,magnetAnti4,magnetFerro,magnetFerro2,magnetFerro4,size(temps))		
		do i=1,ncols
			do j = 1,nrows
				!if (ran1(5) < 0.5) then
					matriz(i,j) = 1
				!else 
				!	matriz(i,j) = -1
				!end if
			end do
		end do

	do t=1,size(temps)
	!	if (k==1 ) then
	!	open(unit=20,file=trim(folderName)//'/'//trim(currentOutput),access="append",action="write")
			write(*,*) 'At T=',temps(t)
	!	close(20)
	!	end if

		!with spin-up initial values
		!call ini_matriz(matriz,nrows,ncols,iniseed,k)
		


	beta = 1.0/real(temps(t))

	Energy = 0.0
	Energy2 = 0.0
	magnetANTI = 0.0
	magnetFerro = 0
	magnetAnti2 = 0.0
	magnetAnti4 = 0.0
	magnetFerro2 = 0
	magnetFerro4 = 0
	magnetAbsFerro = 0
	!write(2,*) "Current Temperature T = ", temps,"beta = ",beta
	!print *, 'npoints: ',npoints
	!close(20) !Closing OutputFile.dat
	!Define the values of DeltaE knowing the Temperature
	!Only two possible values
	! 1) DeltaE(1) = exp(-4/Temp)
	! 2) DeltaE(2) = exp(-8/Temp)
	!call Inicializacion_DeltaE(DeltaE,temps)

	
		!Initialize the matrix
		!call WriteSpinConf(matriz,nrows,ncols)
		!Write initial spin configurations in screen for DynamicLattice
	
		!call CountPosNeg(matriz,nrows,ncols,npos,nneg,nnull)
		promediateCounter = 0
		!Choose a random point (posx,posy) where we begin calculating
	
			!Spinflip it is not done with probpos or probneg because of the dissipation of the spins. At low temperature 
			!npos or nneg -> 0 so it is not possible to select spins with the requiered probability.
		

		!Sum over one MonteCarlo time //dim = 1MC
		!Sum over A MonteCarlo time: A*dim 
		!In the next loop we flip a sping according to some conditions
		do i=1,MCS

			if (mod(i,MCS/100) == 0) then
				
				if(printOPT == 1) then
					do index1 = 1,nrows
						do index2 = 1,ncols
							write(*,*) index1,index2,matriz(index1,index2) 
						end do
					end do
				end if
				!print *, 'Current step: ',i,'/',MCS
				!do index1 = 1,ncols
				!do index2 = 1,nrows
				!write(*,*) index1,index2,matriz(index1,index2)
				!enddo
				!enddo
			end if

			!Calculate the Magnetization and Energy after equilibrium
			if ( i > EquilSteps) then !mod(i,MCS/npoints) == 0
				!print *,'Enter at time: ',i,'for ',promediateCounter+1
				promediateCounter = promediateCounter + 1
	
				!call calculationsInEquilibrium (nrows,ncols,matriz,alpha,ip,im,temps,&
				!	Energy,Energy2,k,extMagField,magnetANTI,magnetAnti2,magnetAnti4,&
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
				energia = energia/(2.0)!(2.0*ncols*nrows)
				Energy = Energy + energia
				Energy2 = Energy2 + energia**2
				!print *, 'Energy: ',energia, 'Energy2: ',energia**2

				!-----AntiFerro quantities----------------
				!<mB>
				magnetBlack = magnetBlack / real(black)
				!<mW>
				magnetWhite = magnetWhite / real(white)
				!inter 
				antiMagnet = abs(magnetBlack - magnetWhite) / real(2)
				!<mANTI>
				magnetANTI = magnetANTI + antiMagnet

				magnetAnti2 = magnetAnti2 + antiMagnet**2

				magnetAnti4 = magnetAnti4 + antiMagnet**4
				!------------------------------------------------
				!------------------------------------------------
				Magnetizacion = sum(matriz(1:ncols,1:nrows))!/real(dim)
				!<m>F
				magnetFerro = magnetFerro + Magnetizacion

				magnetAbsFerro = magnetAbsFerro + abs(Magnetizacion)

				magnetFerro2 = magnetFerro2 + Magnetizacion**2

				magnetFerro4 = magnetFerro4 + Magnetizacion**4

				!Store the time desired to measure. They are not equidistant, so we need to do this
				!call WriteSpinConf(matriz,nrows,ncols)
			end if


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
		
			
		end do !End loop of the MC steps	
	
	open(unit=1,file=trim(folderName)//'/'//trim(fileOutput),access="append",action="write")
		write(1,'(F5.2,x,F8.5,x,F8.5,x,F8.5,x,F8.5,x,F8.5,x,F10.5,x,F10.5,x,F8.5,x,F8.5,x,F10.5,x,F8.5)') alphaVals(1),temps(t),Energy/real(promediateCounter*dim),(beta**2)*( Energy2/real(promediateCounter) - (Energy/real(promediateCounter))**2 )/real(dim),&
			magnetAbsFerro/real(promediateCounter*dim),magnetFerro/real(promediateCounter*dim),beta*(magnetFerro2/real(promediateCounter) - (magnetFerro/real(promediateCounter))**2 )/real(dim), &
			beta*(magnetFerro2/real(promediateCounter) - (magnetAbsFerro/real(promediateCounter))**2 )/real(dim),1 - ( magnetFerro4/real(promediateCounter) / real( 3*( (magnetFerro2/real(promediateCounter))**2) ) ),&
			magnetANTI/real(promediateCounter),beta*(magnetAnti2/real(promediateCounter) - (magnetANTI/real(promediateCounter))**2 )*real(dim),1 - ( magnetAnti4/real(promediateCounter) / real( 3*( (magnetAnti2/real(promediateCounter))**2) ) )
	close(1)

	end do ! End loop of temperatures

!	open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),access='append',action='write')
!		write(2,*) 'Accepted = ', exchangeCounter/real(MCS*dim*average)
!	close(2)

	!Write the results in a file		
	!call Write2File(Energy,Energy2,magnetANTI,magnetAnti2,magnetAnti4,magnetFerro,magnetFerro2,magnetFerro4,promediateCounter,average,temps,size(temps),alphaVals(alfa),posDsty(prob),folderName,FileOutput)
end do !End loop of alpha vals
end program kawasaki


