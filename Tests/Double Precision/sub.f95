module sub

implicit none
integer,parameter::dp2 = selected_real_kind(15, 307) !For doble precision
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine imprimir(i)
	real(dp2),intent(out)::i
	i = 3
end subroutine imprimir

end module sub