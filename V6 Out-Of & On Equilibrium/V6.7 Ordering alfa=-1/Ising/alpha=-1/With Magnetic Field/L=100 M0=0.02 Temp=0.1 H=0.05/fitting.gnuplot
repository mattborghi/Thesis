set terminal pdfcairo

set output 'IsingM1.Log.pdf'
file = 'Magnetization.dat'
set grid
#f(x) = -a*x + b
#fit [1:3] f(x) file u (log10($2)):(log10($3)) via a,b
set title 'L=100 M0=0.02 Temp=0.1 H=0.05'
#set label 1 at 1,-0.2 'Fit f(x)=-a*x+b, a=0.0582(5)'
set xlab 'Log Tiempo[MCS]'
set ylab 'Log Magnetización'
set yrange [-0.5:0.5]
plot file u (log10($2)):(log10($3)) w lp ps 0.7 pt 7#,f(x) w l lw 3
set output

set output 'IsingM1.pdf'
file = 'Magnetization.dat'
set grid
#f(x) = -a*x + b
#fit [1:3] f(x) file u (log10($2)):(log10($3)) via a,b
#set label 1 at 1,-0.2 'Fit f(x)=-a*x+b, a=0.0582(5)'
set xlab 'Log Tiempo[MCS]'
set ylab 'Log Magnetización'
set yrange [-0.5:1.5]
plot [:100] file u 2:3 w lp ps 0.7 pt 7#,f(x) w l lw 3
set output