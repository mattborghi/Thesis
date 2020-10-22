set datafile separator ','

#---------------------------------------------------------------
#Filenames
folder1 = '../L=100/Temp=  1.0 alfa=1,-10,1 ANTIFERRO/MCS=100mil aver=10/'
folder2 = '../L=100/Temp=  1.0 alfa=1,-1.85,1/aver=100/'
folder3 = '../L=100/Temp=  1.0 alfa=1,1,1 FERRO/aver=1/'
folder4 = '../L=100/Temp=  1.0 alfa=1,-1.8,1/aver=50/'
folder5 = '../L=100/Temp=  1.0 alfa=1,-1.9,1/aver=50/'

#Input files
fileMagnetization = 'Magnetization20L.dat'

AntiMagnetization = 'AntiMagnetizacion.png'
#---------------------------------------------------------------
set terminal wxt enhanced font "Helvetica,15" size 900,650

#Grid
set grid
#Set axis
set auto xy
#Key
set key on center right box
#Label
set xlabel "Tiempo [MCS]"
set ylabel "Anti-Magnetización"
#Plot
plot folder1.fileMagnetization u 1:6 t 'alfa=1,-10,1' w lp ls 7 pt 7 lc rgb "red", \
	folder5.fileMagnetization u 1:6 t 'alfa=1,-1.9,1' w lp ls 7 pt 7 lc rgb "brown", \
	folder2.fileMagnetization u 1:6 t 'alfa=1,-1.85,1' w lp ls 7 pt 7 lc rgb "green", \
	folder4.fileMagnetization u 1:6 t 'alfa=1,-1.8,1' w lp ls 7 pt 7 lc rgb "cyan", \
	folder3.fileMagnetization u 1:6 t 'alfa=1,1,1' w lp ls 7 pt 7 lc rgb "blue"

#Export graph
set term pngcairo enhanced size 900,650
set output AntiMagnetization
replot
#-----------LOGSCALE---------------------
set logscale xy

AntiMagnetization = 'LogAntiMagnetizacion.png'

set terminal wxt enhanced font "Helvetica,15" size 900,650
#Label
set xlabel "Log Tiempo [MCS]"
set ylabel "Log Anti-Magnetización"
#Key
set key on top left box
#Plot
plot folder1.fileMagnetization u 1:6 t 'alfa=1,-10,1' w lp ls 7 pt 7 lc rgb "red", \
	folder5.fileMagnetization u 1:6 t 'alfa=1,-1.9,1' w lp ls 7 pt 7 lc rgb "brown", \
	folder2.fileMagnetization u 1:6 t 'alfa=1,-1.85,1' w lp ls 7 pt 7 lc rgb "green", \
	folder4.fileMagnetization u 1:6 t 'alfa=1,-1.8,1' w lp ls 7 pt 7 lc rgb "cyan"#, \
	#folder3.fileMagnetization u 1:6 t 'alfa=1,1,1' w lp ls 7 pt 7 lc rgb "blue"

#Export graph
set term pngcairo enhanced size 900,650
set output AntiMagnetization
replot
