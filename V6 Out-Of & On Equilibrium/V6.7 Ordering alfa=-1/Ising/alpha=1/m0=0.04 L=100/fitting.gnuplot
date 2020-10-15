#set terminal png
set terminal pdfcairo enhanced font 'Verdana,10'
set output 'Magnet.pdf' 

file = 'Magnetization.dat'

f(x) = a*x + b
set grid
fit [2:3] f(x) file u (log10($2)):(log10($3)) via a,b
set title 'L=100 M0=0.04 -> C.I. Desordenada'
#set label 1 at 10,0.06 'Fit f(x)=a*x+b, a=0.1798(4)'
set label 2 at 0.5,-1.2 'Fit f(x)=a*x+b, a=0.1798(4)'
plot file u (log10($2)):(log10($3)) w lp,f(x) w l lw 3
#replot
set output 