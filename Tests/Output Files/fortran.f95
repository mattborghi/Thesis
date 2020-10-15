program fortran

implicit none

character(len=100)::inpf = "InputFile.dat"
character(len=100)::of,inter1='(I3.0)'!,inter2='(I3.0)'
character(5)::x1,x2 !Necessary for some compilers
integer::nrow,ncol
open(unit=30,file=inpf,action='write',status='replace')
write(30,*) "nrow "
write(30,*) "10"
write(30,*) "ncol"
write(30,*) "10"
close(30)

open(unit=30,file=inpf,action='read',status='old')
read(30,*);read(30,*) nrow
read(30,*);read(30,*) ncol
close(30)

!Change output file name according to the input file
write(x1,inter1) nrow
write(x2,inter1) ncol
of = "nrow"//trim(x1)//"&ncol"//trim(x2)//".dat"
open(unit=31,file=of,action='write',status='replace')
write(31,*) "Just write sth. Not necessary"
close(31)
end program fortran