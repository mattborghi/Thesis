set terminal wxt enhanced font "Helvetica,15" size 900,650
#Grid
set grid
#Set axis
set auto xy
#Key
set key on top right box
#Label
set xlabel "kT/J"
set ylabel "Calorific capacity"
#Plot
plot 'magnetization30L.dat' u 1:5 t 'Capacidad Calorifica' w p ls 7 pt 6 lc rgb "red"
#Export graph
set term pngcairo enhanced size 900,650
set output "capcalorfica.png"
replot
#################################################################
set terminal wxt 2 enhanced font "Helvetica,15" size 900,650
plot 'magnetization30L.dat' u 1:4 t 'Susceptibilidad' w p ls 7 pt 6 lc rgb "blue"
#Key
set key on top right box
#Label
set xlabel "kT/J"
set ylabel "Susceptibility"
#Export graph
set term pngcairo enhanced size 900,650
set output "susceptibilidad.png"
replot
#################################################################
set terminal wxt 3 enhanced font "Helvetica,15" size 900,650
plot 'magnetization30L.dat' u 1:2 t 'Magnetizacion' w p ls 7 pt 6 lc rgb "green"
#Key 
set key on right top box
#Label
set xlabel "kT/J"
set ylabel "Magnetization"
#Export graph
set term pngcairo enhanced size 900,650
set output "magnetizacion.png"
replot
#################################################################
set terminal wxt 4 enhanced font "Helvetica,15" size 900,650
plot 'magnetization30L.dat' u 1:3 t 'Energia' w p ls 7 pt 6 lc rgb "brown"
#Key
set key on left top box
#Label
set xlabel "kT/J"
set ylabel "Energy"
#Export graph
set term pngcairo enhanced size 900,650
set output "energia.png"
replot