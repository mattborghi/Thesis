program folder
implicit none

real::Temp = 0.9, Np = 0.5, pp = 0.5
integer:: L=20, MCS=250000, aver=10
real:: alpha(3,3)
character(len=100)::folderName
character(len=10):: stringL,stringMCS,stringAver,stringTemp,stringNp,stringPp,stringAlpha
logical::dir_e

alpha(1,1) = 1
alpha(2,2) = 0
alpha(3,3) = 2

!Create the folder name
!Cast integer/real to string
write(stringL,'(I2)') L
write(stringMCS,'(I6)') MCS
write(stringAver,'(I2)') aver
write(stringAlpha,'(F2.0,F2.0,F2.0)') alpha(1,1),alpha(2,2),alpha(3,3)
write(stringTemp,'(F3.1)') Temp
write(stringNp,'(F3.1)') Np
write(stringPp,'(F3.1)') pp
!Merge the strings
!ie., name: L=20 alfa 111 t=0.9 n+=0.5 p+=0.5 aver=10 mcs=500mil 
folderName = 'L='//trim(stringL)//'Alfa='//trim(stringAlpha)//'Temp='//trim(stringTemp)//&
		'N+='//trim(stringNp)//'P+='//trim(stringPp)//'MCS='//trim(stringMCS)//'Aver='//trim(stringAver)
!Does the folder already exist?
inquire(FILE=trim(folderName)//'/.', EXIST=dir_e)
write(*,*) 'Does the folder ',trim(folderName),' exists?'
if(dir_e) then
	write(*,*) 'Directory already exists!'
else
	write(*,*) 'Folder created!'
	call system('mkdir -p ' // trim(folderName) )
end if

open(unit=2,file="file.dat",status="replace",action="write")
write(2,*) 'Write sth...'
close(2)

open(unit=2,file="file.dat",access="append",action="write")
write(2,*) 'Writing again...'
close(2)


end program folder