#set terminal png
set terminal pdfcairo enhanced font 'Verdana,10'
file = 'Magnetization.dat'

f(x) = a*x + b
set grid

#set output 'MagnetTotal.pdf'
#set title 'L=050 M0=0.04'
#set xlab 'Tiempo [MCS]'
#set ylab 'Magnetización'
#plot [:20000] file u 2:3 w lp
#set output

set output 'Magnet.pdf' 
fit [2:3.5] f(x) file u (log10($2)):(log10($3)) via a,b
set title 'L=150 M0=0.04 -> C.I. Desordenada'
set xlab 'Log Tiempo [MCS]'
set ylab 'Log Magnetización'
set key on box left
#set label 1 at 10,0.06 'Fit f(x)=a*x+b, a=0.1798(4)'
set label 2 at 0.5,-1.2 'Ajuste f(x)=a*x+b, a=0.19(1)' front
plot [:4.3] file u (log10($2)):(log10($3)) w p t 'Magnetización',f(x) w l lw 3 t 'Ajuste lineal'
#replot
set output 