#set terminal png
set terminal pdfcairo enhanced font 'Verdana,10'
file = 'Magnetization.dat'

#f(x) = a*x + b
set grid

set output 'Magnet.pdf'
set title 'L=064 M0=0.0048'
set xlab 'Tiempo [MCS]'
set ylab 'Magnetización'
plot [:100] file u 2:3 w lp ps 0.7 pt 7
set output

#set output 'Magnet.pdf' 
#fit [1.5:2.5] f(x) file u (log10($2)):(log10($3)) via a,b
#set title 'L=050 M0=0.04 -> C.I. Desordenada'
#set label 1 at 10,0.06 'Fit f(x)=a*x+b, a=0.1798(4)'
#set label 2 at 0.5,-1.2 'Fit f(x)=a*x+b, a=0.2055(5)'
#plot [:4.3] file u (log10($2)):(log10($3)) w lp,f(x) w l lw 3
#replot
#set output 