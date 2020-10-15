set terminal pdfcairo
set output 'Magnetizacion.M0=1.pdf'
folder1 = 'L=128 m0=1/'
folder2 = 'L=075 m0=1/'
folder3 = 'L=175 m0=1/'
folder4 = 'L=064 m0=0.002/'
file = 'Magnetization.dat'
set grid
#f(x) = -a*x + b
#fit [1:3] f(x) file u (log10($2)):(log10($3)) via a,b
set title 'M0=1 -> Ordenado'
#set label 1 at 1,-0.2 'Fit f(x)=-a*x+b, a=0.0582(5)'
set xlab 'Log Tiempo[MCS]'
set ylab 'Log Magnetización'
plot folder2.file u (log10($2)):(log10($3)) w lp t 'L=075' ps 0.7 pt 7 lc rgb 'red',folder1.file u (log10($2)):(log10($3)) w lp t 'L=128' ps 0.7 pt 7 lc rgb 'green',\
	folder3.file u (log10($2)):(log10($3)) w lp t 'L=175' ps 0.7 pt 7 lc rgb 'blue'
set output

set output 'Magnetizacion.Desordenado.pdf'
folder1 = 'L=108 m0=0.01/'
folder2 = 'L=064 m0=0.002/'
#folder3 = 'L=175 m0=1/'
file = 'Magnetization.dat'
set grid
set title 'Desordenado'
set xlab 'Log Tiempo[MCS]'
set ylab 'Log Magnetización'
plot folder2.file u (log10($2)):(log10($3)) w lp t 'L=064 m0=0.01' ps 0.7 pt 7 lc rgb 'red',folder1.file u (log10($2)):(log10($3)) w lp t 'L=108 m0=0.002' ps 0.7 pt 7 lc rgb 'green'#,\
	#folder3.file u (log10($2)):(log10($3)) w lp t 'L=175' ps 0.7 pt 7 lc rgb 'blue'
set output

set output 'Magnetizacion.Combined.Log.pdf'
folder2 = 'L=075 m0=1/'
folder2 = 'L=064 m0=0.002/'
#folder3 = 'L=175 m0=1/'
file = 'Magnetization.dat'
set grid
set title 'Combined'
set xlab 'Log Tiempo[MCS]'
set ylab 'Log Magnetización'
plot folder2.file u (log10($2)):(log10($3)) w lp t 'L=064 m0=0.002' ps 0.7 pt 7 lc rgb 'red',folder1.file u (log10($2)):(log10($3)) w lp t 'L=075 m0=1' ps 0.7 pt 7 lc rgb 'green'#,\
	#folder3.file u (log10($2)):(log10($3)) w lp t 'L=175' ps 0.7 pt 7 lc rgb 'blue'
set output

set output 'Magnetizacion.Combined.pdf'
folder2 = 'L=075 m0=1/'
folder2 = 'L=064 m0=0.002/'
#folder3 = 'L=175 m0=1/'
file = 'Magnetization.dat'
set grid
set title 'Combined'
set xlab 'Tiempo[MCS]'
set ylab 'Magnetización'
plot [:100] folder2.file u 2:3 w lp t 'L=064 m0=0.002' ps 0.7 pt 7 lc rgb 'red', folder1.file u 2:3 w lp t 'L=075 m0=1' ps 0.7 pt 7 lc rgb 'green'#,\
	#folder3.file u (log10($2)):(log10($3)) w lp t 'L=175' ps 0.7 pt 7 lc rgb 'blue'
set output