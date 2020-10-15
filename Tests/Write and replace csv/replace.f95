program replace 
implicit none

character(len=100)::file="archivo.csv"
real::Magnetization(2) = (/2.5,  1.3/),Cumulant(2) = (/1.5, 0.3/)
real::array3(2,2)
integer::oldint
integer::i
character(len=100)::smth

open (UNIT=1, FILE=file,ACTION='write',STATUS='replace')
print *, 'Writing initial values to the file..'
write(1,*) 'Step Magnetization Cumulant' 
call fileWriting(1,Magnetization,Cumulant,size(Magnetization))
close(1)

open(unit=1,FILE=file,action="read")
print *,'Reading written values..'
read(unit=1,fmt='(A)') smth
print *,'Read header file:'
print *, smth
print *,'Reading data..'
do i=1,size(Magnetization)
	read(unit=1,fmt='(I6,x,F8.6,x,F8.6)') oldint,array3(1,i),array3(2,i)
	write(*,'(I6,3x,F8.6,3x,F8.6)') oldint,array3(1,i),array3(2,i)
	!print *, oldint,oldMagnet(i), oldCumulant(i)
end do
close(1)
!call exit(0)

print *, '---------------------------'
open(unit=1,FILE=file,status='replace',action="write")
print *, 'Rewriting the initial values..'
write(1,*) 'Step Magnetization Cumulant' 
call fileWriting(2,array3(1,:)*2,array3(2,:)*2,size(array3(1,:)))
close(1)

end program replace

subroutine fileWriting(step,Array1,Array2,length)

integer,intent(in):: length,step
real,intent(in)::Array2(length),Array1(length)

integer::i 

do i=1,length
	write(1,'(I6,x,F8.6,x,F8.6)') step,Array1(i), Array2(i)
end do

end subroutine fileWriting