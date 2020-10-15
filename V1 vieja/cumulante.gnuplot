#Load with the code
# $ gnuplot
# $ load 'cumulante.gnuplot'
#---------------------------------------------------------------
#Filenames
#Input files
fileSize = '20L.dat'
fileSizee = '30L.dat'
fileSizeee = '50L.dat'
fileMagnetization = 'Magnetization'
#Output files
folder = 'Graphs/'
Cumulant = 'Cumulante.png'
#---------------------------------------------------------------
set terminal wxt 4 enhanced font "Helvetica,15" size 900,650
plot fileMagnetization.fileSize u 1:5 t '20x20' w lp ls 7 pt 6 lc rgb "red", \
	 fileMagnetization.fileSizee u 1:5 t '30x30' w lp ls 7 pt 6 lc rgb "blue", \
 	 fileMagnetization.fileSizeee u 1:5 t '50x50' w lp ls 7 pt 6 lc rgb "green"
#Key
set key right top box 3
#Title
set title "Cumulante 4to -> Tc" 
#Label
set xlabel "kT/J"
set ylabel "4th Cumulant"
#Range
set xrange [2.55:3]
#set yrange auto
#Export graph
set term pngcairo enhanced size 900,650
set output folder.Cumulant
replot