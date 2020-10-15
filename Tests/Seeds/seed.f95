! $ gfortran ranmar_seed.f95 seed.f95 -o runv1;./runv1
program seed
use rm
real::rvec(2)
integer::ini,num
ini = 2
num= 2
print *,'Initial seed: ',ini
call ranmar(rvec,num,ini)
print*,'Random Number: ',rvec(1),rvec(2)


call ranmar(rvec,1,ini)
print*,'Another Random Number: ',rvec(1)


end program seed