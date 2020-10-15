program doloop
  implicit none
  integer :: i
  real :: a(2:20) = (/(i*0.1, i=2,20)/)
  real:: b(-20:20) = (/(i*0.1,i=20,-20,-1)/)

  real,allocatable::c(:)
  print *, a,b

  allocate(c(size(a)+size(b)))
  print *, 'C: -> size: ',size(c)
  print *, 'A: -> size: ',size(a)
  print *, 'B: -> size: ',size(b)

  c(1:size(a)) = a
  c(size(a)+1:size(c)) = b

  print *, 'PRINTING C'
  print *, c

end program doloop