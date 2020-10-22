																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																							
set datafile separator ','
file1 = 'segVStemp.dat'
file2 = 'segVStempextra.dat'

plot file1 u 2:3 w lp lw 3.0 lc rgb 'blue' t 'alpha 1,1,1',\
	file2 u 2:3 w lp lw 3.0 lc rgb 'red' t 'alpha 1,10,1' 

set xlabel 'Temperature [J/kB]'
set ylabel 'f1 segregation: f1 up + f1 down'							

#Export graph
set term pngcairo dashed enhanced size 900,650
set output "segf1.png"
replot

#-------RESCALED USING Tc---------

#Export graph
set term pngcairo dashed enhanced size 900,650
set output "Rescaledsegf1.png"

set key box on top right
set title 'Rescaled Segregation vs Temp fol L=020'

Tc1 = 2.269
Tc10 = 12.46

plot file1 u ($2/Tc1):3 w lp lw 3.0 lc rgb 'blue' t 'alpha 1,1,1 Tc = 2.269',\
	file2 u ($2/Tc10):3 w lp lw 3.0 lc rgb 'red' t 'alpha 1,10,1 Tc = 12' 

set xlabel 'T/Tc(alfa) [J/kB]'
set ylabel 'f1 segregation: f1 up + f1 down'							

replot
