#Load with the code
# $ gnuplot
# $ load 'cumulante.gnuplot'
set datafile separator ','
#---------------------------------------------------------------
#Filenames
#Input files
file = 'Magnetization20L.dat'
#Output files
Susceptib = 'SusceptibilidadL.png'
#---------------------------------------------------------------

set terminal wxt 4 enhanced font "Helvetica,15" size 900,650

	folder10 = 'L=020 MCS=50/'
	folder20 = 'L=050 MCS=50 aver=2000/'
	folder50 = 'L=100/'
	folder100 = 'L=150/'
	#Title
	set title "Susceptibilidad para alfa=1,10,1 en función de L" 

	plot folder10.file u 1:4 t 'L=020' w lp ls 7 pt 6 lc rgb "cyan",\
		folder20.file u 1:4 t 'L=050' w lp ls 7 pt 6 lc rgb "red", \
	 	folder50.file u 1:4 t 'L=100' w lp ls 7 pt 6 lc rgb "blue", \
 	 	folder100.file u 1:4 t 'L=150' w lp ls 7 pt 6 lc rgb "green"

#Key
set key right top box 3
set key width 3
#Label
set xlabel "kT/J"
set ylabel "Susceptibilidad"
#Range
set xrange [8:16]
#set yrange auto
#Export graph
set term pngcairo enhanced size 900,650
set output Susceptib
replot


#----------EN REDES PEQUEÑAS --------------------------------
#Output files
Susceptib2 = 'SusceptibilidadLpeq.png'

	folder1 = 'L=010/'
	folder2 = 'L=020/'
	folder3 = 'L=030/'

	#Title
	set title "Susceptibilidad para alfa=1,10,1 en función de L \n para redes pequeñas" 

	plot folder1.file u 1:4 t 'L=010' w lp ls 7 pt 6 lc rgb "cyan",\
		folder2.file u 1:4 t 'L=020' w lp ls 7 pt 6 lc rgb "red", \
	 	folder3.file u 1:4 t 'L=030' w lp ls 7 pt 6 lc rgb "blue"

#Key
set key right top box 3
set key width 3
#Label
set xlabel "kT/J"
set ylabel "Susceptibilidad"
#Range
#set xrange [8:16]
#set yrange auto
#Export graph
set term pngcairo enhanced size 900,650
set output Susceptib2
replot