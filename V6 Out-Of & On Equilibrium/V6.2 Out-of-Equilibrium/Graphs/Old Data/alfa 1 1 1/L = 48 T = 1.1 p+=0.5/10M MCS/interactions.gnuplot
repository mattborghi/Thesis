#Load with the code
# $ gnuplot
# $ load 'equilibrio.gnuplot'
#---------------------------------------------------------------
#Filenames
#Input files
fileInter = 'Inter20L.dat'
#Output files
folder = 'Graphs/'
Interf1 = 'Interf1.png'
Interf2 = 'Interf2.png'
Interf3 = 'Interf3.png'
Interf4 = 'Interf4.png'
Interf = 'Interf.png'
#InterV2 = 'InterV2.png'
#---------------------------------------------------------------
set terminal wxt enhanced font "Helvetica,15" size 900,650
#Separator
set datafile separator ','
#Grid
set grid
#Set axis
set auto xy
#Key
set key on top right box
#Label
set xlabel "Time steps"
set ylabel "Interactions"
#Title
set title "f1 interactions"
#Plot f1
plot fileInter u (log($1)):2 t 'InterPos' w lp ls 7 pt 6 lc rgb "red", \
	 fileInter u (log($1)):3 t 'InterNeg' w lp ls 7 pt 6 lc rgb "blue"
 	 
#Export graph
set term pngcairo enhanced size 900,650
set output folder.Interf1
replot

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
#Title
set title "f2 interactions"
#Plot f2
plot fileInter u (log($1)):4 t 'InterPos' w lp ls 7 pt 6 lc rgb "red", \
	 fileInter u (log($1)):5 t 'InterNeg' w lp ls 7 pt 6 lc rgb "blue"
 	 
#Export graph
set term pngcairo enhanced size 900,650
set output folder.Interf2
replot
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
#Title
set title "f3 interactions"
#Plot f3
plot fileInter u (log($1)):6 t 'InterPos' w lp ls 7 pt 6 lc rgb "red", \
	 fileInter u (log($1)):7 t 'InterNeg' w lp ls 7 pt 6 lc rgb "blue"
 	 
#Export graph
set term pngcairo enhanced size 900,650
set output folder.Interf3
replot
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
#Title
set title "f4 interactions"
#Plot f4
plot fileInter u (log($1)):8 t 'InterPos' w lp ls 7 pt 6 lc rgb "red", \
	 fileInter u (log($1)):9 t 'InterNeg' w lp ls 7 pt 6 lc rgb "blue"
 	 
#Export graph
set term pngcairo enhanced size 900,650
set output folder.Interf4
replot

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
#Title
set title "Interactions"
#Plot sum f
plot fileInter u (log($1)):($2+$3) t 'f1' w lp ls 7 pt 6 lc rgb "red", \
	 fileInter u (log($1)):($4+$5) t 'f2' w lp ls 7 pt 6 lc rgb "blue", \
	 fileInter u (log($1)):($6+$7) t 'f3' w lp ls 7 pt 6 lc rgb "cyan", \
	 fileInter u (log($1)):($8+$9) t 'f4' w lp ls 7 pt 6 lc rgb "green"
#Export graph
set term pngcairo enhanced size 900,650
set output folder.Interf
replot