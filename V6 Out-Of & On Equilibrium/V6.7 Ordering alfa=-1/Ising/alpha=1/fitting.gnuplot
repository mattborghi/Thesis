set terminal pdfcairo enhanced color font 'Verdana,10'
#set term png size 1024,768 enhanced font "Helvetica,20"
set output 'Magnetization.M0=1.pdf'
folder1 = 'm0=1.0 L=064/'
folder2 = 'm0=1.0 L=128/'
file = 'Magnetization.dat'
set grid
#f(x) = a*x + b
#fit [1:3] f(x) folder1.file u (log10($2)):(log10($3)) via a,b
set title 'M0=1.0 -> C.I. Ordenada'
#set label 1 at 1,-0.2 'Fit f(x)=-a*x+b, a=0.0571(1)'
set xlab 'Log Tiempo [MCS]'
set ylab 'Log Magnetización'
plot folder1.file u (log10($2)):(log10($3)) w lp t 'L=064' pt 7 ps 0.5 lc rgb 'blue',\
	folder2.file u (log10($2)):(log10($3)) w lp t 'L=128' pt 7 ps 0.5 lc rgb 'red'
set output


#---------DESORDENADA----------------

set output 'Magnetization.M0peq.pdf'
folder1 = 'm0=0.04 L=050/'
folder2 = 'm0=0.04 L=100/'
folder3 = 'm0=0.04 L=150/'
file = 'Magnetization.dat'
set grid
#f(x) = a*x + b
#fit [1:3] f(x) folder1.file u (log10($2)):(log10($3)) via a,b
set title 'Desordenada'
#set label 1 at 1,-0.2 'Fit f(x)=-a*x+b, a=0.0571(1)'
set xlab 'Log Tiempo [MCS]'
set ylab 'Log Magnetización'
plot folder1.file u (log10($2)):(log10($3)) w lp t 'L=064' pt 7 ps 0.5 lc rgb 'blue',\
	folder2.file u (log10($2)):(log10($3)) w lp t 'L=100' pt 7 ps 0.5 lc rgb 'red',\
	folder3.file u (log10($2)):(log10($3)) w lp t 'L=150' pt 7 ps 0.5 lc rgb 'green'
set output