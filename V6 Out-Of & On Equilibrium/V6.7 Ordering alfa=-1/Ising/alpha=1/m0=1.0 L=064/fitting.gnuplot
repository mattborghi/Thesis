set terminal pdfcairo enhanced color font 'Verdana,10'
#set term png size 1024,768 enhanced font "Helvetica,20"
set output 'IsingM1.pdf'
file = 'Magnetization.dat'
set grid
f(x) = a*x + b
fit [1:3] f(x) file u (log10($2)):(log10($3)) via a,b
set title 'L=064 M0=1.0 -> C.I. Ordenada'
set label 1 at 1,-0.2 'Fit f(x)=-a*x+b, a=0.0571(1)'
set xlab 'Log Tiempo [MCS]'
set ylab 'Log Magnetización'
plot file u (log10($2)):(log10($3)) w lp pt 7 ps 0.5 lc rgb 'blue',f(x) w l lw 3 lc rgb 'red'
set output