!subroutine imprimir(i)
!	integer,parameter::dp = selected_real_kind(15, 307) !For doble precision
!	real(dp),intent(out)::i
!	i = 1
!end subroutine imprimir

program doublepres
use sub
implicit none

	integer,parameter::dp = selected_real_kind(15, 307) !For doble precision
	real(dp)::a,b
	real::c=2 !Single precision
	a=1
	write(*,*) "a = ",a
	call imprimir(b)
	print *, "en la function i = ", b
	write(*,*) "La suma entre ",2/b,"y ",c," es = ", 2/b+c

end program doublepres

