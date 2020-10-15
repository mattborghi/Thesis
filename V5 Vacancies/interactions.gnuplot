#Load with the code
# $ gnuplot
# $ load 'equilibrio.gnuplot'
#---------------------------------------------------------------
#Filenames
#Input files
fileInter = 'Inter20L.dat'
fileInterV2 = 'Inter220L.dat'
#Output files
folder = 'Graphs/'
Inter = 'Inter.png'
InterV2 = 'InterV2.png'
#---------------------------------------------------------------
set terminal wxt enhanced font "Helvetica,15" size 900,650
#Grid
set grid
#Set axis
set auto xy
#Key
set key on top right box
#Label
set xlabel "Time steps"
set ylabel "Interactions"
#Plot
plot fileInter u ($1):2 t 'InterPos' w lp ls 7 pt 6 lc rgb "red", \
	 fileInter u ($1):3 t 'InterNeg' w lp ls 7 pt 6 lc rgb "blue", \
 	 fileInter u ($1):4 t 'InterPN'  w lp ls 7 pt 6 lc rgb "green", \
 	 fileInter u ($1):5 t 'InterZero'  w lp ls 7 pt 6 lc rgb "black"
#Export graph
set term pngcairo enhanced size 900,650
set output folder.Inter
replot

#---------------------------------------------------------------
set terminal wxt 2 enhanced font "Helvetica,15" size 900,650
#Grid
set grid
#Set axis
set auto xy
#Key
set key on top right box
#Label
set xlabel "Time steps"
set ylabel "Interactions V2"
#Plot
plot fileInterV2 u ($1):2 t 'Inter ++' w lp ls 7 pt 6 lc rgb "red", \
	 fileInterV2 u ($1):3 t 'Inter --' w lp ls 7 pt 6 lc rgb "blue", \
 	 fileInterV2 u ($1):4 t 'Inter -+'  w lp ls 7 pt 6 lc rgb "green", \
 	 fileInterV2 u ($1):5 t 'Inter °°'  w lp ls 7 pt 6 lc rgb "black", \
 	 fileInterV2 u ($1):6 t 'Inter °+'  w lp ls 7 pt 6 lc rgb "magenta",\
 	 fileInterV2 u ($1):7 t 'Inter °-'  w lp ls 7 pt 6 lc rgb "cyan", \
#Export graph
set term pngcairo enhanced size 900,650
set output folder.InterV2
replot