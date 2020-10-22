#Load with the code
# $ gnuplot
# $ load 'equilibrio.gnuplot'
set datafile separator ','
#---------------------------------------------------------------
#Filenames
folder = '../L=100/Temp=  1.0 alfa=1,-10,1 ANTIFERRO/MCS=100mil aver=10/'
#Input files
fileEnergy = 'Energy20L.dat'
fileMagnetization = 'Magnetization20L.dat'
#Output files
Cv = 'CapCalorfica.png'
Susceptibility = 'Susceptibilidad.png'
Magnetization = 'Magnetizacion.png'
AntiSusceptibility = 'AntiSusceptibilidad.png'
AntiMagnetization = 'AntiMagnetizacion.png'
Energy = 'Energia.png'
#---------------------------------------------------------------
set terminal wxt enhanced font "Helvetica,15" size 900,650
#Grid
set grid
#Set axis
set auto xy
#Key
set key on top right box
#Label
set xlabel "Tiempo [MCS]"
set ylabel "Capacidad Calorifica"
#Plot
plot folder.fileEnergy u 1:4 t 'Capacidad Calorifica' w lp ls 7 pt 7 lc rgb "red"
#Export graph
set term pngcairo enhanced size 900,650
set output folder.Cv
replot
#################################################################
set terminal wxt 2 enhanced font "Helvetica,15" size 900,650
plot folder.fileMagnetization u 1:4 t 'Susceptibilidad' w lp ls 7 pt 7 lc rgb "blue"
#Key
set key on top right box
#Label
set xlabel "Tiempo [MCS]"
set ylabel "Susceptibilidad"
#Export graph
set term pngcairo enhanced size 900,650
set output folder.Susceptibility
replot
#################################################################
set terminal wxt 3 enhanced font "Helvetica,15" size 900,650
plot folder.fileMagnetization u 1:2 t 'Magnetizacion' w lp ls 7 pt 7 lc rgb "green"
#Key 
set key on right top box
#Label
set xlabel "Tiempo [MCS]"
set ylabel "Magnetizacion"
#Export graph
set term pngcairo enhanced size 900,650
set output folder.Magnetization
replot
#################################################################
set terminal wxt 4 enhanced font "Helvetica,15" size 900,650
plot folder.fileEnergy u 1:2 t 'Energia' w lp ls 7 pt 7 lc rgb "brown"
#Key
set key on left top box
#Label
set xlabel "Tiempo [MCS]"
set ylabel "Energia"
#Export graph
set term pngcairo enhanced size 900,650
set output folder.Energy
replot
##-----------------ANTI QUANTITIES-------------------------------
#################################################################
set terminal wxt 2 enhanced font "Helvetica,15" size 900,650
plot folder.fileMagnetization u 1:9 t 'Anti-Susceptibilidad' w lp ls 7 pt 7 lc rgb "blue"
#Key
set key on top right box
#Label
set xlabel "Time [MCS]"
set ylabel "Anti-Susceptibilidad"
#Export graph
set term pngcairo enhanced size 900,650
set output folder.AntiSusceptibility
replot
#################################################################
set terminal wxt 3 enhanced font "Helvetica,15" size 900,650
plot folder.fileMagnetization u 1:6 t 'Anti-Magnetizacion' w lp ls 7 pt 7 lc rgb "green"
#Key 
set key on right top box
#Label
set xlabel "Tiempo [MCS]"
set ylabel "Anti-Magnetizacion"
#Export graph
set term pngcairo enhanced size 900,650
set output folder.AntiMagnetization
replot
#################################################################
#-------LOGSCALE ANTI QUANTITIES-------------------------
set logscale xy
AntiSusceptibility = 'LogAntiSusceptibility.png'
AntiMagnetization = 'LogAntiMagnetization.png'
#################################################################
set terminal wxt 2 enhanced font "Helvetica,15" size 900,650
plot folder.fileMagnetization u 1:9 t 'Anti-Susceptibilidad' w lp ls 7 pt 7 lc rgb "blue"
#Key
set key on top right box
#Label
set xlabel "Time [MCS]"
set ylabel "Anti-Susceptibilidad"
#Export graph
set term pngcairo enhanced size 900,650
set output folder.AntiSusceptibility
replot
#################################################################
set terminal wxt 3 enhanced font "Helvetica,15" size 900,650
plot folder.fileMagnetization u 1:6 t 'Anti-Magnetizacion' w lp ls 7 pt 7 lc rgb "green"
#Key 
set key on right top box
#Label
set xlabel "Tiempo [MCS]"
set ylabel "Anti-Magnetizacion"
#Export graph
set term pngcairo enhanced size 900,650
set output folder.AntiMagnetization
replot
#################################################################


