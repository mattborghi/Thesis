#L fijo W(t) Varias Temp

set datafile separator ','

set key box on top left
set title 'Ancho de la interfaz para L=512 en función de la temperatura'
set xlabel 'Tiempo [MCS]'
set ylabel '<W>:Ancho de la interfaz'
set grid

file1 = "../Graphs new/L=512/Temp=1.0/Roughness.dat"
file2 = "../Graphs new/L=512/Temp=2.3/Roughness.dat"
file3 = "../Graphs new/L=512/Temp=5.0/Roughness.dat"

plot file1 u 1:8 w l t 'Temp=1.0 J/kb' lc rgb 'blue' lw 3.0,\
	file2 u 1:8 w l t 'Temp=2.269 J/kb' lc rgb 'green' lw 3.0,\
	file3 u 1:8 w l t 'Temp=5.0 J/kb' lc rgb 'red' lw 3.0

#Export graph
set term pngcairo dashed enhanced size 900,650
set output "L=512WtVariasTemp.png"
replot

######################
set xlabel 'Log Tiempo [MCS]'
set ylabel 'Log <W>:Ancho de la interfaz'

set logscale xy
plot file1 u 1:8 w l t 'Temp=1.0 J/kb' lc rgb 'blue' lw 3.0,\
	file2 u 1:8 w l t 'Temp=2.269 J/kb' lc rgb 'green' lw 3.0,\
	file3 u 1:8 w l t 'Temp=5.0 J/kb' lc rgb 'red' lw 3.0

#Export graph
set term pngcairo dashed enhanced size 900,650
set output "LogL=512WtVariasTemp.png"
replot
