set terminal pdfcairo
set output 'IsingM1Log.pdf'
file = 'Magnetization.dat'
set grid
#f(x) = -a*x + b
#fit [1:3] f(x) file u (log10($2)):(log10($3)) via a,b
set title 'L=075 M0=1 -> Ordenado'
#set label 1 at 1,-0.2 'Fit f(x)=-a*x+b, a=0.0582(5)'
set xlab 'Log Tiempo[MCS]'
set ylab 'Log Magnetización'
plot file u (log10($2)):(log10($3)) w lp ps 0.7 pt 7#,f(x) w l lw 3
set output

set output 'IsingM1.pdf'
file = 'Magnetization.dat'
set grid
#f(x) = -a*x + b
#fit [1:3] f(x) file u (log10($2)):(log10($3)) via a,b
set title 'L=075 M0=1 -> Ordenado'
#set label 1 at 1,-0.2 'Fit f(x)=-a*x+b, a=0.0582(5)'
set xlab 'Tiempo[MCS]'
set ylab 'Magnetización'
plot [:100] file u 2:3 w lp ps 0.7 pt 7#,f(x) w l lw 3
set output