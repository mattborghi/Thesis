#Load with the code
# $ gnuplot
# $ load 'cumulante.gnuplot'
set datafile separator ','
#---------------------------------------------------------------

#Filenames
#Input files
file = 'Magnetization20L.dat'
#Output files
Cumulant = 'CumulantePeq.png'
#---------------------------------------------------------------

set terminal wxt 4 enhanced font "Helvetica,15" size 900,650


folder10 = '../alfa 1 10 1/Initial Conditions T=0/L=010/'
folder20 = '../alfa 1 10 1/Initial Conditions T=0/L=020/'
folder50 = '../alfa 1 10 1/Initial Conditions T=0/L=030/'

#folder10 = '../L=010/'
#folder20 = '../L=020/'
#folder50 = '../L=030/'

#Title
set title "Cumulante 4to para alfa=1,10,1 -> Tc \n para redes pequeñas" 

plot folder10.file u 1:5 t 'L=010' w lp ls 7 pt 6 lc rgb "cyan" lw 3.0,\
	folder20.file u 1:5 t 'L=020' w lp ls 7 pt 6 lc rgb "red" lw 3.0, \
 	folder50.file u 1:5 t 'L=030' w lp ls 7 pt 6 lc rgb "blue" lw 3.0

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
Cumulant = 'ResizedCumulantePeq.png'


#Title
set title "Reescaleo Cumulante 4to para alfa=1,10,1 -> Tc" 

plot folder10.file u 1:5 t 'L=010' w lp ls 7 pt 6 lc rgb "cyan" lw 3.0,\
	folder20.file u 1:5 t 'L=020' w lp ls 7 pt 6 lc rgb "red" lw 3.0,  \
 	folder50.file u 1:5 t 'L=030' w lp ls 7 pt 6 lc rgb "blue" lw 3.0
 	
#Range
set xrange [12:13]
#vertical line
#set arrow from 12.45, graph 0 to 12.45, graph 1 nohead
set grid 
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


