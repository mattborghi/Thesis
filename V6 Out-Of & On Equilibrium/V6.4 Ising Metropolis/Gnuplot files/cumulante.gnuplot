#Load with the code
# $ gnuplot
# $ load 'cumulante.gnuplot'
set datafile separator ','
#---------------------------------------------------------------
alfa=10

#Filenames
#Input files
file = 'Magnetization20L.dat'
#Output files
Cumulant = 'Cumulante.png'
#---------------------------------------------------------------

set terminal wxt 4 enhanced font "Helvetica,15" size 900,650


if (alfa == 1){
	folder20 = '../alfa 1 1 1/L=020/'
	folder50 = '../alfa 1 1 1/L=050/'
	folder100 = '../alfa 1 1 1/L=100/'
	#Title
	set title "Cumulante 4to para alfa=1,1,1 -> Tc" 

	plot folder20.file u 1:5 t 'L=020' w lp ls 7 pt 6 lc rgb "red", \
	 folder50.file u 1:5 t 'L=050' w lp ls 7 pt 6 lc rgb "blue", \
 	 folder100.file u 1:5 t 'L=100' w lp ls 7 pt 6 lc rgb "green"

} else{
	folder10 = '../alfa 1 10 1/L=020 MCS=500/'
	folder20 = '../alfa 1 10 1/L=050 MCS=500 aver=2000/'
	folder50 = '../alfa 1 10 1/L=100/'
	folder100 = '../alfa 1 10 1/L=150/'
	#Title
	set title "Cumulante 4to para alfa=1,10,1 -> Tc" 

	plot folder10.file u 1:5 t 'L=020' w lp ls 7 pt 6 lc rgb "cyan",\
		folder20.file u 1:5 t 'L=050' w lp ls 7 pt 6 lc rgb "red", \
	 	folder50.file u 1:5 t 'L=100' w lp ls 7 pt 6 lc rgb "blue", \
 	 	folder100.file u 1:5 t 'L=150' w lp ls 7 pt 6 lc rgb "green"
}


#Key
set key right top box 3
set key width 3
#Label
set xlabel "kT/J"
set ylabel "4th Cumulant"
#Range
#set xrange [12:14]
#set yrange auto
#Export graph
set term pngcairo enhanced size 900,650
set output Cumulant
replot

#------------RESCALED------------------------------------------

set terminal wxt 4 enhanced font "Helvetica,15" size 900,650

#Output file
Cumulant = 'ResizedCumulante.png'

if (alfa == 1){
	#Title
	set title "Reescaleo Cumulante 4to para alfa=1,1,1 -> Tc" 

	plot folder20.file u 1:5 t 'L=020' w lp ls 7 pt 6 lc rgb "red", \
	 folder50.file u 1:5 t 'L=050' w lp ls 7 pt 6 lc rgb "blue", \
 	 folder100.file u 1:5 t 'L=100' w lp ls 7 pt 6 lc rgb "green"
 	 #Range
	set xrange [2:3]

} else{
	#Title
	set title "Reescaleo Cumulante 4to para alfa=1,10,1 -> Tc" 

	plot folder10.file u 1:5 t 'L=020' w lp ls 7 pt 6 lc rgb "cyan",\
		folder20.file u 1:5 t 'L=500' w lp ls 7 pt 6 lc rgb "red", \
	 	folder50.file u 1:5 t 'L=100' w lp ls 7 pt 6 lc rgb "blue", \
 	 	folder100.file u 1:5 t 'L=150' w lp ls 7 pt 6 lc rgb "green"
 	 	#Range
		set xrange [11:13]

}


#Key
set key left bottom box 3
set key width 3
#Label
set xlabel "kT/J"
set ylabel "4th Cumulant"
#set yrange auto
#Export graph
set term pngcairo enhanced size 900,650
set output Cumulant
replot


