module subroutines
use rm
implicit none
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine variables_a_cero(promedio,mag,m2,m4,energ,energ2,prom,prom_M2,prom_M4,prom_energia,prom_energia2,u4)

	integer,intent(in)::promedio
	real,intent(out)::mag(promedio),m2(promedio),m4(promedio),energ(promedio),energ2(promedio),u4
	real,intent(out)::prom,prom_M2,prom_m4,prom_energia,prom_energia2

		mag=0
		m2 = 0
		m4 = 0
		energ=0
		energ2=0
		prom=0
		prom_M2=0
		prom_M4 = 0
		prom_energia=0
		prom_energia2=0
		u4=0

end subroutine variables_a_cero
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine Inicializacion_DeltaE(DeltaE,Temp)
	
	real,intent(in)::Temp
	real,intent(out)::DeltaE(2)

	DeltaE(1) = exp(-4/Temp)
	DeltaE(2) = exp(-8/Temp)


end subroutine Inicializacion_DeltaE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine ini_matriz(matriz,L)
	
	integer,intent(in)::L
	integer,intent(out)::matriz(L,L)

	integer::i,j

	do i=1,L
		do j = 1,L
				!Inicializamos la matriz con todos los spines para arriba
				matriz (i,j) = 1			
		end do
	end do

end subroutine ini_matriz

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1111
subroutine elegir_sitio(L,posx,posy)
	
	integer,intent(in)::L
	integer,intent(out)::posx,posy

	real::rvec(2)

	!Para el valor inicial elegimos un mapeo desde 0-1 -> 1-L

	call ranmar(rvec,2)

	posx = int( (L-1) * rvec(1) + 1 )

	posy = int( (L-1) * rvec(2) + 1 )

end subroutine elegir_sitio
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine calc_delta_energia(L,posx,posy,matriz,Delta_E)

	integer,intent(in)::L
	integer,intent(in)::posx,posy,matriz(L,L)
	integer,intent(out)::Delta_E

	if (posx == 1 .AND. posy == 1) then
		Delta_E = 2*matriz (posx,posy) * ( matriz (posx+1,posy) + matriz (L,posy) + matriz (posx,posy+1) + matriz (posx,L) )
	else if (posx == 1 .AND. posy == L) then
		Delta_E = 2*matriz (posx,posy) * ( matriz (posx+1,posy) + matriz (L,posy) + matriz (posx,1) + matriz (posx,posy-1) )
	else if (posx == L .AND. posy == 1) then
		Delta_E = 2*matriz (posx,posy) * ( matriz (1,posy) + matriz (posx-1,posy) + matriz (posx,posy+1) + matriz (posx,L) )
	else if (posx == L .AND. posy == L) then
		Delta_E = 2*matriz (posx,posy) * ( matriz (1,posy) + matriz (posx-1,posy) + matriz (posx,1) + matriz (posx,posy-1) )
	else if (posx == 1 .AND. posy/=1 .AND. posy/=L) then
		Delta_E = 2*matriz (posx,posy) * ( matriz (posx+1,posy) + matriz (L,posy) + matriz (posx,posy+1) + matriz (posx,posy-1) )
	else if (posx == L .AND. posy/=1 .AND. posy/=L) then
		Delta_E = 2*matriz (posx,posy) * ( matriz (1,posy) + matriz (posx-1,posy) + matriz (posx,posy+1) + matriz (posx,posy-1) )
	else if (posy == 1 .AND. posx/=1 .AND. posx/=L) then
		Delta_E = 2*matriz (posx,posy) * ( matriz (posx+1,posy) + matriz (posx-1,posy) + matriz (posx,posy+1) + matriz (posx,L) )
	else if (posy == L .AND. posx/=1 .AND. posx/=L) then
		Delta_E = 2*matriz (posx,posy) * ( matriz (posx+1,posy) + matriz (posx-1,posy) + matriz (posx,1) + matriz (posx,posy-1) )
	else
		Delta_E = 2*matriz (posx,posy) * ( matriz (posx+1,posy) + matriz (posx-1,posy) + matriz (posx,posy+1) + matriz (posx,posy-1) )
	end if

end subroutine calc_delta_energia
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine magnetizacion(L,matriz,mag,M2,M4,energ,energ2,promedio,k,Delta_E)

	integer,intent(in)::L,promedio,matriz(L,L),k
	integer,intent(out)::Delta_E
	real,intent(inout)::m2(promedio),m4(promedio),mag(promedio),energ(promedio),Energ2(promedio)
	integer::i,j
	real::mag_temp,energ_temp
	mag_temp = 0
	energ_temp = 0
	do i = 1, L
		do j = 1, L
			mag_temp = mag_temp + matriz(i,j)
			call calc_delta_energia(L,i,j,matriz,Delta_E)
			energ_temp = energ_temp - Delta_E/4
			!Divido por (-2) para que me devuelva la energía del spin i,j
			!Se divide por dos nuevamente para no contar dos veces interacciones entre spines
		end do
	end do
	
	!<E>
	Energ(k) = energ_temp
	!<E2>
	Energ2(k) = energ_temp**2
	!<m>
	mag(k) = mag_temp
	!<m2>
	M2(k) = mag_temp**2
	!<m4>
	M4(k) = mag_temp**4

end subroutine magnetizacion
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11
subroutine promediar(promedio,mag,m2,m4,energ,energ2,prom_magnet,prom_M2,prom_M4,prom_energia,prom_energia2,u4)

	integer,intent(in)::promedio
	real,intent(in)::mag(promedio),m2(promedio),m4(promedio),energ(promedio),energ2(promedio)
	real,intent(inout)::prom_magnet,prom_M2,prom_M4,u4,prom_energia,prom_energia2

	!calculo el promedio de la magnetizacion
	call prom1d(mag,promedio,prom_magnet)
	call prom1d(M2,promedio,prom_M2)
	call prom1d(M4,promedio,prom_M4)
	call prom1d(energ,promedio,prom_energia)
	call prom1d(energ2,promedio,prom_energia2)

end subroutine promediar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine funciones_especiales(prom,prom_energia,prom_energia2,prom_M2,Temp,suceptibility,cap_calorifica)
	
	real,intent(in)::prom_M2,prom,prom_energia2,Temp,prom_energia
	real,intent(out)::suceptibility,cap_calorifica

	!<x>
	suceptibility = (prom_M2 - prom**2)/Temp
	!<c>
	cap_calorifica = (prom_energia2 - prom_energia**2)/Temp**2

end subroutine funciones_especiales
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine escritura (filename,dim,Temp,prom_magnet,prom_M2,prom_M4,prom_energia,suceptibility,cap_calorifica,prom_energia2)

	character(len=20),intent(in)::filename
	integer,intent(in)::dim
	real,intent(in)::prom_M2,prom_M4,prom_magnet,Temp,suceptibility,prom_energia,cap_calorifica,prom_energia2

	real:: cumulant
	cumulant = 1 - ( prom_M4 / ( 3*(prom_M2**2) ) )

	open (UNIT=1, FILE=filename,ACTION='write',POSITION='append')
		write(1,*) Temp,prom_magnet/dim,prom_energia/dim,suceptibility/dim,cap_calorifica/dim,cumulant,prom_M2,prom_M4
	close(1)

end subroutine escritura
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine prom1d(array,dim_array,prome)

	integer,intent(in)::dim_array		!dimension de la variable a promediar,cant de veces que se "tira" la particula
	real,intent(in)::array(dim_array)				!variable a promediar
	real, intent(out)::prome		!resultado del promedio
	real::s
	integer::i


!veamos primero la sumatoria
s=0
do i=1,dim_array
	s=s+array(i)
end do

prome=(s/dim_array)

end subroutine prom1d
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine aumento_temp(j,Temp,T_inicial)
	
	integer,intent(in)::j
	real,intent(in)::T_inicial
	real,intent(out)::Temp

	Temp = 0.012*j + T_inicial


end subroutine aumento_temp
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
end module subroutines
