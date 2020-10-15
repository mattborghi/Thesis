set terminal pdfcairo enhanced color font 'Verdana,10'
set output 'IsingM1.pdf'
file = 'Magnetization.dat'
set grid
set key on box
f(x) = -a*x + b
fit [1.5:3] f(x) file u (log10($2)):(log10($3)) via a,b
set title 'L=128 M0=1 -> C.I. Ordenada'
set xlab 'Log Tiempo [MCS]'
set ylab 'Log Magnetización'
set label 1 at 1,-0.2 'Ajuste: f(x)=-a*x+b, a=0.058(1)' front
#set output 'IsingM1.pdf'
plot file u (log10($2)):(log10($3)) w p ps 0.7 pt 7 lc rgb 'blue' t 'Magnetizacion',f(x) w l lw 3 lc rgb 'red' t 'Ajuste lineal'
set output