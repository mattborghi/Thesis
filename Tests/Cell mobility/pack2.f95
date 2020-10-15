program test 
real, dimension(3,4) :: A 
A(:,1) = (/1.,0.,0./) 
A(:,2) = (/0.,1.,1./) 
A(:,3) = (/0.,1.,1./) 
A(:,4) = (/1.,0.,1./) 
print *,'c=',PACK(SPREAD((/1,2,3/), DIM=2, NCOPIES=4), MASK=(A == 1.0)) 
print *,'d=',PACK(SPREAD((/1,2,3,4/), DIM=1, NCOPIES=3), MASK=(A == 1.0)) 
end program test 