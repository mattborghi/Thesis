#set datafile separator ","
##Filenames
##Input files
#fileEnergy = 'Energy20L.dat'#

#fileMagnetization = 'Magnetization20L.dat'
##Output files
#folder = 'Graphs/'
#Cv = 'CapCalorfica.png'
#Susceptibility = 'Susceptibilidad.png'
#Magnetization = 'Magnetizacion.png'
#Energy = 'Energia.png'
##---------------------------------------------------------------
#set terminal wxt enhanced font "Helvetica,15" size 900,650
##Grid
#set grid
##Set axis
#set auto xy
##Key
#set key on top right box
##Label
#set xlabel "Time steps"
#set ylabel "Heat capacity"
##Plot
#plot fileEnergy u ($1):4 t 'Capacidad Calorifica' w p ls 7 pt 7 lc rgb "red"
##Export graph
#set term pngcairo enhanced size 900,650
#set output folder.Cv
#replot


fileInput(x,y) = sprintf("nrow%3.0d&ncol%3.0d.dat",x,y)
fileGraph(x,y) = sprintf("nrow%3.0d&ncol%3.0d.png",x,y)
fileRead = "InputFile.dat" #To read the values of nrows and ncols


set terminal wxt enhanced font "Helvetica,15" size 900,650
plot fileInput(10,10) u 1:2
#Export graph
set term pngcairo enhanced size 900,650
set output fileGraph(10,10)
replot