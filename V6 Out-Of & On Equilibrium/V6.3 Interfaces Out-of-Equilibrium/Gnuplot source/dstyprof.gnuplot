#Plot the density profiles for L=24 Temp=2.3
set datafile separator ','
file = "Graphs new/L=512/Temp=5.0/Inter20L.dat"

#Label
set xlabel "m"
set ylabel "fup(m)"
#Grid
set grid
#Title
set title 'Perfil de densidad f1up'
#Key
set key right center box

#f1up vs m
plot  file u (($2 == 1)? $3: 1/0):4 t 't=6 MCS' w l lc rgb "red" lw 3,\
	file u (($2 == 2500)? $3: 1/0):4 t 't=15mil MCS' w l lc rgb "blue" lw 3,\
	file u (($2 == 5000)? $3: 1/0):4 t 't=30mil MCS' w l lc rgb "black" lw 3
#Export graph
set term pngcairo enhanced size 900,650
set output "dstyf1up.png"
replot

#-----------------------------------------------

set title 'Perfil de densidad f4up'

#f4up vs m
plot  file u (($2 == 1)? $3: 1/0):10 t 't=6 MCS' w l lc rgb "red" lw 3,\
	file u (($2 == 2500)? $3: 1/0):10 t 't=15mil MCS' w l lc rgb "blue" lw 3,\
	file u (($2 == 5000)? $3: 1/0):10 t 't=30mil MCS' w l lc rgb "black" lw 3
#Export graph
set term pngcairo enhanced size 900,650
set output "dstyf4up.png"
replot

########################################################

#Title
set title 'Perfil de densidad de f1'
#f1 vs m
plot  file u (($2 == 1)? $3: 1/0):(1-($4+$5)) t 't=6 MCS' w l lc rgb "red" lw 3,\
	file u (($2 == 2500)? $3: 1/0):(1-($4+$5)) t 't=15mil MCS' w l lc rgb "blue" lw 3,\
	file u (($2 == 5000)? $3: 1/0):(1-($4+$5)) t 't=30mil MCS' w l lc rgb "black" lw 3
#Export graph
set term pngcairo enhanced size 900,650
set output "dstyf1.png"
replot

#-----------------------------------------------

set title 'Perfil de densidad f4'

#f4 vs m
plot  file u (($2 == 1)? $3: 1/0):(1-($10+$11)) t 't=6 MCS' w l lc rgb "red" lw 3,\
	file u (($2 == 2500)? $3: 1/0):(1-($10+$11)) t 't=15mil MCS' w l lc rgb "blue" lw 3,\
	file u (($2 == 5000)? $3: 1/0):(1-($10+$11)) t 't=30mil MCS' w l lc rgb "black" lw 3
#Export graph
set term pngcairo enhanced size 900,650
set output "dstyf4.png"
replot

###############################################################
###############################################################

#Density profiles of spins

set title 'Perfil de densidad de espines positivos'

#spin+ vs m
plot  file u (($2 == 1)? $3: 1/0):12 t 't=6 MCS' w l lc rgb "red" lw 3,\
	file u (($2 == 2500)? $3: 1/0):12 t 't=15mil MCS' w l lc rgb "blue" lw 3,\
	file u (($2 == 5000)? $3: 1/0):12 t 't=30mil MCS' w l lc rgb "black" lw 3
#Export graph
set term pngcairo enhanced size 900,650
set output "dstyup.png"
replot

#-------------------------------------------------------


set title 'Perfil de densidad de espines negativos'

#spin+ vs m
plot  file u (($2 == 1)? $3: 1/0):13 t 't=6 MCS' w l lc rgb "red" lw 3,\
	file u (($2 == 2500)? $3: 1/0):13 t 't=15mil MCS' w l lc rgb "blue" lw 3,\
	file u (($2 == 5000)? $3: 1/0):13 t 't=30mil MCS' w l lc rgb "black" lw 3
#Export graph
set term pngcairo enhanced size 900,650
set output "dstydown.png"
replot
