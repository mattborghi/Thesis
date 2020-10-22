#Load with the code
# $ gnuplot
# $ load 'equilibrio.gnuplot'
#---------------------------------------------------------------
set datafile separator ","
#Filenames
#Input files
fileEnergy = 'Energy20L.dat'
fileMagnetization = 'Magnetization20L.dat'
#Output files
folder = ''
Cv = 'CapCalorfica.png'
Susceptibility = 'Susceptibilidad.png'
Magnetization = 'Magnetizacion.png'
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
set xlabel "Time steps"
set ylabel "Heat capacity"
#Plot
#plot fileEnergy u ($1):4 t 'Capacidad Calorifica' w p ls 7 pt 7 lc rgb "red"
#Export graph
#set term pngcairo enhanced size 900,650
#set output folder.Cv
#replot
#################################################################
set terminal wxt 2 enhanced font "Helvetica,15" size 900,650
#plot fileMagnetization u ($1):4 t 'Susceptibilidad' w p ls 7 pt 7 lc rgb "blue"
#Key
set key on top right box
#Label
set xlabel "Time steps"
set ylabel "Susceptibility"
#Export graph
#set term pngcairo enhanced size 900,650
#set output folder.Susceptibility
#replot
#################################################################
set terminal wxt 3 enhanced font "Helvetica,15" size 900,650
#plot fileMagnetization u ($1):2 t 'Magnetizacion' w p ls 7 pt 7 lc rgb "green"
#Key 
set key on right top box
#Label
set xlabel "Time steps"
set ylabel "Magnetization"
#Export graph
#set term pngcairo enhanced size 900,650
#set output folder.Magnetization
#replot
#################################################################
set terminal wxt 4 enhanced font "Helvetica,15" size 900,650
plot fileEnergy u ($1):2 t 'Energia' w p ls 7 pt 7 lc rgb "brown"
#Key
set key on left top box
#Label
set xlabel "Time steps"
set ylabel "Energy"
#Export graph
set term pngcairo enhanced size 900,650
set output folder.Energy
replot