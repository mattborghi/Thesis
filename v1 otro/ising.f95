program ising ! 2D Monte Carlo Simulation of Ising Model
! Lisa Larrimore, lisal@sccs.swarthmore.edu
! 3 May 2002
! Physics 114 Final Project
! This program is adapted from the Ising Model program written in
! BASIC by Elaine Chandler that appears on p. 184 of David Chandler’s
! Introduction to Modern Statistical Mechanics.

 ! The input parameters for this program are in "ising.in", and they
 ! allow the size, length, and initial configuration of the simulation
 ! to be changed. See comments in file.

 ! This program has three output files:
 !
 ! "spin-array" Contains snapshots of the spin lattice at the end of
 ! each temperature run (or throughout the middle of the
 ! run, if only looking at one temperature). Can be
 ! visualized with the IDL program see_spins.pro
 !
 ! "magnetization" Contains four columns: each temperature, the
 ! average magnetization at that temp, the ave magnetizaion
 ! squared at that temp, and the susceptibility.
 !
 ! "energy" Contains four columns: each temperature, the
 ! average energy at that temp, the ave energy squared
 ! at that temp, and the heat capacity.

 implicit none

 ! Variable declarations:
 integer :: i, j, m, n, m2, n2 ! dummy integers
 integer, allocatable :: A(:,:) ! matrix containing spins
 integer :: nrows, ncols ! number of rows and cols of A
 real :: temp, beta ! temperature, inverse temperature
 integer :: ConfigType ! starting configuration type
 integer :: npass ! number of passes for MC algorithm
 integer :: ipass ! the current pass number
 integer :: nequil ! number of equilibration steps
 integer :: trial_spin ! values of changed spin
 real :: high_temp ! starting temp for scan
 real :: low_temp ! final temp for scan
 real :: temp_interval ! interval between scan points
 integer :: nscans ! number of scans (each at diff T)
 integer :: iscan ! current scan number
 logical :: MovieOn ! set to .true. to make movie of 1 temp
 real :: deltaU ! change in energy between 2 configs
 real :: deltaU1 ! energy changes for lattice gas
 real :: log_eta ! log of random number to compare to
 real :: magnetization ! magnetization of all spins in lattice
 real :: magnetization_ave ! cumulative average magnetization
 real :: magnetization2_ave ! cumulative average of mag. squared
 real :: energy ! energy of all spins in lattice
 real :: energy_ave ! cumulative average of energy
 real :: energy2_ave ! cumulative average of energy squared
 integer :: output_count ! # times things have been added to averages

 print*, "________________MONTE CARLO 2D ISING MODEL________________"
 print*, "Monte Carlo Statistics for 2D Ising Model with"
 print*, " periodic boundary conditions."
 print*, "The critical temperature is approximately 2.3, as seen on"
 print*, " Chandler p. 123."

 ! Read in input parameters from file "ising.in"
 open(UNIT=11,FILE='ising.in',STATUS='old',ACTION='read')
 read(11,*);read(11,*) nrows
 read(11,*);read(11,*) ncols
 read(11,*);read(11,*) npass
 read(11,*);read(11,*) nequil
 read(11,*);read(11,*) high_temp
 read(11,*);read(11,*) low_temp
 read(11,*);read(11,*) temp_interval
 read(11,*);read(11,*) ConfigType
 read(11,*);read(11,*) MovieOn
 close(11)

 ! Set the dimensions of the matrix of spin arrays. This program uses
 ! periodic boundary conditions, so the first two rows and columns are
 ! the same as the last two.
 allocate(A(nrows+2,ncols+2))

 ! Open output files:
 open(unit=32,file='spin-array',status='replace',action='write')
 write(32,*) nrows
 write(32,*) ncols
 nscans = int((high_temp - low_temp)/temp_interval) + 1
 if (MovieOn) then
 write(32,*) 51
 write(32,*) 1
 else
 write(32,*) nscans
 write(32,*) 2
 endif

 open(unit=33,file='magnetization',status='replace',action='write')
 write(33,*) "temp ave_magnetization ave_magnetization^2 susceptibility"
 open(unit=34,file='energy',status='replace',action='write')
 write(34,*) "temp ave_energy ave_energy^2 C_v"

 scan_loop: do iscan = 1, nscans
 temp = high_temp - temp_interval*(iscan-1)
 print*, "Running program for T =", temp

 ! Initialize variables
 beta = 1.0/temp
 output_count = 0
 energy_ave = 0.0
 energy2_ave = 0.0
 magnetization_ave = 0.0
 magnetization2_ave = 0.0

 ! Set up the initial spin configuration.
 select case(ConfigType)
 case(1) ! checkerboard setup
 
 !A(1,1) = 1
 do i = 1, nrows+2
 !A(i+1,1) = -A(i,1)
 
 do j = 1, ncols+2
 A(i,j) = 1
 !A(:,j+1) = -A(:,j)
 enddo
 enddo
 ! (note: the requirement that nrows and ncols are even is to
 ! ensure that the first two rows/cols start out the same as the
 ! last two)
 case(2) ! interface
 do i = 1, nrows+2
 do j = 1, (ncols+2)/2
 A(i,j) = 1
 enddo
 do j = (ncols+2)/2 + 1, ncols+2
 A(i,j) = -1
 enddo
 enddo
 case(3) ! unequal interface
 do i = 1, nrows+2
 do j = 1, (ncols+2)/4
 A(i,j) = 1
 enddo
 do j = (ncols+2)/4 + 1, ncols+2
 A(i,j) = -1
 enddo
 enddo
 case default
 print*, "Error! Check ConfigType parameter in ising.in"
 stop
 end select

 ! Main loop containing Monte Carlo algorithm:
 MC_passes: do ipass = 0, npass

 ! If MovieOn is .true., write the spin array to an output every
 ! npass/50 steps.
 if ((MovieOn) .and. (mod(ipass,npass/50) == 0)) then
 do i = 2, nrows+1
 do j = 2, ncols+1
 write(32,*) A(i,j)
 enddo
 enddo
 endif

 ! If ipass is greater than nequil (the number of equilibration steps),
 ! calculate the magnetization and energy:
 if (ipass > nequil) then
 output_count = output_count + 1
 magnetization = sum(A(2:nrows+1,2:nrows+1))/(ncols*nrows*1.0)
 magnetization_ave = magnetization_ave + magnetization
 magnetization2_ave = magnetization2_ave + magnetization**2
 energy = 0.0
 do i = 2, nrows + 1
 do j = 2, ncols + 1
 energy = energy - A(m,n)*(A(m-1,n)+A(m+1,n)+A(m,n-1)+A(m,n+1))
 enddo
 enddo
 ! Divide the energy by the total number of spins to get the ave
 ! energy per spin, and divide by 2 to account for double counting.
 energy = energy/(ncols*nrows*2.0)
 energy_ave = energy_ave + energy
 energy2_ave = energy2_ave + energy**2
 endif

 ! Randomly choose a spin to change:
 m = nint((nrows-1)*ran1(5) + 2) ! choose a random row
 n = nint((ncols-1)*ran1(5) + 2) ! choose a random column
 trial_spin = -A(m,n) ! trial spin value

 ! Find change in energy (deltaU) due to trial move.
 ! If exp(-beta*deltaU) > eta, where eta is random, accept move:
 deltaU = -trial_spin*(A(m-1,n)+A(m+1,n)+A(m,n-1)+A(m,n+1))*2
 log_eta = dlog(ran1(5) + 1.0d-10) ! random number 0-1 (+ tiny offset)
 if (-beta*deltaU > log_eta) then
 A(m,n) = trial_spin
 if (m == 2) A(nrows+2,n) = trial_spin
 if (m == nrows+1) A(1,n) = trial_spin
 if (n == 2) A(m,ncols+2) = trial_spin
 if (n == ncols+1) A(m,1) = trial_spin
 endif

 enddo MC_passes

 ! Write final spin array to output file
 if (.not. MovieOn) then
 do i = 2, nrows + 1
 do j = 2, ncols + 1
 write(32,*) A(i,j)
 enddo
 enddo
 endif
 write(33,*) temp, abs(magnetization_ave/output_count), &
 magnetization2_ave/output_count, &
 beta*(magnetization2_ave/output_count - (magnetization_ave/output_count)**2)
 write(34,*) temp, energy_ave/output_count, energy2_ave/output_count, &
 (beta**2)*(energy2_ave/output_count - (energy_ave/output_count)**2)

 enddo scan_loop

 close(32)
 close(33)
 close(34)

 print*, "Program ising.f90 complete!"
 print*, "Look at ’spin-array’ with IDL program see_spins.pro"

 contains


 !_______RANDOM NUMBER GENERATING FUNCTION______!

 double precision function ran1(idum)
 implicit none
 double precision :: r(97)
 integer, intent(IN) :: idum
 save
 integer, parameter :: M1=259200,IA1=7141,IC1=54773
 real, parameter :: RM1=1.0d0/M1
 integer, parameter :: M2=134456,IA2=8121,IC2=28411
 real, parameter :: RM2=1.0d0/M2
 integer, parameter :: M3=243000,IA3=4561,IC3=51349
 integer :: IX1, IX2, IX3, jjj
 integer :: iff=0
 if (idum < 0 .or. iff == 0) then
 iff = 1
 IX1 = mod(IC1-idum,M1)
 IX1 = mod(IA1*IX1+IC1,M1)
 IX2 = mod(IX1,M2)
 IX1 = mod(IA1*IX1+IC1,M1)
 IX3 = mod(IX1,M3)
 do jjj = 1,97
 IX1 = mod(IA1*IX1+IC1,M1)
 IX2 = mod(IA2*IX2+IC2,M2)
 r(jjj) = (dfloat(IX1)+dfloat(IX2)*RM2)*RM1
 end do
 end if
 IX1 = mod(IA1*IX1+IC1,M1)
 IX2 = mod(IA2*IX2+IC2,M2)
 IX3 = mod(IA3*IX3+IC3,M3)
 jjj = 1+(97*IX3)/M3
 if (jjj > 97 .or. jjj < 1) write(*,*) "asd"
 ran1 = r(jjj)
 r(jjj) = (dfloat(IX1)+dfloat(IX2)*RM2)*RM1
 end function ran1

 end program ising