program elegir_sitio
use rm
implicit none

real::rvec(1)
integer::seed=3
integer::D=1000,newNcols = 10
integer::posx,i,val(11),minvalue,maxvalue
integer,allocatable::matriz(:,:)
val(:) = 0
allocate(matriz(D,20))
matriz(:,:) = 1
minvalue = D/2-newNcols/2 +1
maxvalue = D/2+newNcols/2
print *,'La dimension es ', D
print *, 'la mitad ',D/2, 'y la nueva longitud',newNcols
print *, 'Quiero numeros entre ',maxvalue,' y ',maxvalue

matriz(minvalue,10) = -1

!if( matriz(minvalue,:) == -1 ) then
!	print*,'Cambiar MCS!'
!end if

do i = 1 , 1000000
	call ranmar(rvec,1,seed)
	!print *, 'Numero: ',rvec(1)
	posx = floor( (newNcols)*rvec(1)) + D/2 - newNcols/2 +1 
	
	SELECT CASE(posx)
		CASE (495)
			val(1) = val(1) + 1
		CASE (496)
			val(2) = val(2) + 1
		CASE (497)
			val(3) = val(3) + 1
		CASE (498)
			val(4) = val(4) + 1
		CASE (499)
			val(5) = val(5) + 1
		CASE (500)
			val(6) = val(6) + 1
		CASE (501)
			val(7) = val(7) + 1
		CASE (502)
			val(8) = val(8) + 1
		CASE (503)
			val(9) = val(9) + 1
		CASE (504)
			val(10) = val(10) + 1
		CASE (505)
			val(11) = val(11) + 1
	END SELECT 
	
	if( posx < minvalue .OR. posx > maxvalue ) then
		print*,'Me fui'
	end if
	if( posx==minvalue .OR. posx==maxvalue) then
		print *,'valor: ', posx
	end if
	!print *,'Numero aleatorio: ',posx
	!if (posx == 505) then
	!	STOP '505'
	!end if
end do
!print *,val
!print * ,float(val)/1000000

end program elegir_sitio