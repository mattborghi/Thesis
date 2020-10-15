program ising

use subroutines
use rm
implicit none

	integer,parameter::L=20,T_max = 500,promedio=25
	integer::posx,posy,dim
	integer:: i,j,k
	integer::Delta_E
	integer::matriz(L,L)
	real::rvec(1)
	real::prom,prom_M2,prom_M4,Temp,mag(promedio),m2(promedio),m4(promedio),DeltaE(2),U4
	real::suceptibility,cap_calorifica
	real::energ(promedio),prom_energia,energ2(promedio),prom_energia2
	real::T_inicial
	character(len=25)::filename='magnetization30L.dat'
	dim = L*L
	T_inicial=0
	Temp = T_inicial

	open (UNIT=1, FILE=filename,ACTION='write',STATUS='replace')
	close(1)

	do j = 1,T_max !T_max no es una temperatura sino un parámetro que mide el numero de temperaturas intermedias que se miden
		
		call variables_a_cero(promedio,mag,m2,m4,energ,energ2,prom,prom_M2,prom_M4,prom_energia,prom_energia2,u4)

		call Inicializacion_DeltaE(DeltaE,Temp)

		do k = 1, promedio
			
			!Inicializamos la matriz 
			!Dandole estados iniciales up a los espines del problema
			call ini_matriz(matriz,L)

			!Elegimos un (posx,posy) aleatorio donde empieza a recorrer los espines
			call elegir_sitio(L,posx,posy)

			!Sumatoria que se realiza en un tiempo de Monte Carlo //dim = 1MC
			do i=1,200*dim
				!Calculamos el delta de energia
				call calc_delta_energia(L,posx,posy,matriz,Delta_E)
				call ranmar(rvec,1)
				select case (Delta_E)
					case (-8)
						matriz(posx,posy) = - matriz(posx,posy)
					case (-4)
						matriz(posx,posy) = - matriz(posx,posy)
					case (4)
						if( DeltaE(1) > rvec(1)) then
							matriz(posx,posy) = - matriz(posx,posy)
						end if
					case (8)
						if( DeltaE(2) > rvec(1) ) then
							matriz(posx,posy) = - matriz(posx,posy)
						end if
				end select

				!call nuevo_paso(posx,posy)
				call elegir_sitio(L,posx,posy)
			end do

			!Luego de los pasos montecarlo, calculamos la magnetizacion
			call magnetizacion (L,matriz,mag,M2,M4,energ,energ2,promedio,k,Delta_E) 

		end do
		
		call promediar(promedio,mag,m2,m4,energ,energ2,prom,prom_M2,prom_M4,prom_energia,prom_energia2,u4)
		call funciones_especiales(prom,prom_energia,prom_energia2,prom_M2,Temp,suceptibility,cap_calorifica)
		call escritura(filename,dim,Temp,prom,prom_M2,prom_M4,prom_energia,suceptibility,cap_calorifica,prom_energia2)
		call aumento_temp(j,Temp,T_inicial) 
	end do

end program ising


