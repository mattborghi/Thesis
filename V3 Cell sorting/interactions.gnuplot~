#Load with the code
# $ gnuplot
# $ load 'equilibrio.gnuplot'
#---------------------------------------------------------------
#Filenames
#Input files
fileInter = 'Inter20L.dat'
#Output files
folder = 'Graphs/'
Inter = 'Inter.png'
#---------------------------------------------------------------
set terminal wxt enhanced font "Helvetica,15" size 900,650
#Grid
set grid
#Set axis
set auto xy
#Key
set key on top right box
#Label
set xlabel "kT/J"
set ylabel "Interactions"
#Plot
plot fileInter u 1:2 t 'InterPos' w lp ls 7 pt 6 lc rgb "red", \
	 fileInter u 1:3 t 'InterNeg' w lp ls 7 pt 6 lc rgb "blue", \
 	 fileInter u 1:4 t 'InterPN'  w lp ls 7 pt 6 lc rgb "green", \
 	 fileInter u 1:5 t 'InterSum' w lp ls 7 pt 6 lc rgb "cyan"
#Export graph
set term pngcairo enhanced size 900,650
set output folder.Inter
replot