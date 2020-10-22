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
	integer::printOPT,matrizInv
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
	real::Energy,antiMagnetization
	integer::Magnetization,AbsMagnetization
	real,dimension(20)::DeltaE
	real::beta
	character(len=100)::MagFile
	character(len=100):: folderName
	character(len=10):: stringL,stringTemp,stringAlpha
	
	!---------------------------------------
	real::currentTemp=2.20,currentExtMagnet=0.0,alpha13=1,inimagnet=0.04
	integer::nrows=20

	ncols=nrows
	print *, 'At T= ', currentTemp
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
	!if(dir_e) then		!write(2,*) 'Directory already exists!'
	!else
		!call system('mkdir -p ' // trim(folderName) )
		!open(unit=2,file=trim(folderName)//'/'//trim(FileOutput),status="replace",action="write")
		!write(2,*) 'Folder created!'
	!end if
	write(stringTemp,'(F7.4)') currentTemp
	write(stringAlpha,'(F7.2)') alpha13
	MagFile = 'a='//trim(stringAlpha)//'T='//trim(stringTemp)//'.dat'
	call StripSpaces(MagFile)
	!MCS = dim*10
	!EquilSteps = int (0.5*MCS)
	call ini_alpha(alpha,alpha11,alpha13,alpha33)

	beta = 1.0/currentTemp
	!print *, 'MCS: ',MCS
	!Initialize some variables to zero
	!Define the values of DeltaE knowing the Temperature
	!Only two possible values
	! 1) DeltaE(1) = exp(-4/Temp)
	! 2) DeltaE(2) = exp(-8/Temp)
	!call Inicializacion_DeltaE(DeltaE,currentTemp)
	!open (UNIT=20, FILE=trim(folderName)//'/'//trim('data.dat'),ACTION='write',STATUS='replace')
	!close(20)
		!Initialize the matrix
		!with spin-up initial values
		
		!Choose a random point (posx,posy) where we begin calculating	
		!Spinflip it is not done with probpos or probneg because of the dissipation of the spins. At low temperature 
		!npos or nneg -> 0 so it is not possible to select spins with the requiered probability.
		!call elegir_sitio(nrows,ncols,posx,posy,iniseed,k)
	
		!Sum over one MonteCarlo time //dim = 1MC
		!Sum over A MonteCarlo time: A*dim 
		!In the next loop we flip a sping according to some conditions
		call ini_matriz(matriz,nrows,ncols,nneg,iniseed,k)
		
		!open (UNIT=1, FILE=trim(folderName)//'/'//trim(MagFile),ACTION='write',form='unformatted',status='old')
		
		do i=1,MCS
		
			call SetToZeroVariables(Magnetization,Energy,antiMagnetization,AbsMagnetization)
			!Calculate the Magnetization and Energy after equilibrium
			call CalcQuantities(nrows,ncols,matriz,Delta_E,Energy,Magnetization,antiMagnetization,AbsMagnetization,ip,im,alpha,k,currentExtMagnet)

		do j = 1, dim

			!Calculate DeltaE for a given point (posx,posy)
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
			!----------------------------------------------------------------
		end do !End loop dim

		!After finishing each run	
		!Write the results in a file	
		
		!write(1) i,Magnetization,absMagnetization,antiMagnetization,Energy
		open(unit=1,file=trim(folderName)//'/'//trim(MagFile),access="append",action="write")
		write(1,'(I7,x,I7,x,I7,x,F8.6,x,F16.3)') i,Magnetization,absMagnetization,antiMagnetization,Energy
		close(1)
			
	end do !End loop of the MC steps	

	!close(1)		

end program kawasaki


