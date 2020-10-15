set terminal pdfcairo

folder1 = 'L=064 M0=1 Temp=0.1 H=0.1/'
folder2 = 'L=100 M0=0.02 Temp=0.1 H=0.05/'
folder22 = 'L=100 M0=0.02 Temp=0.1 H=0.1/'
folder3 = 'L=128 M0=0.04 Temp=0.1 H=0.1/'
folder4 = 'L=256 M0=0.02 Temp=0.1 H=0.1/'
folder5 = 'L=256 M0=0.004 Temp=0.1 H=0.1/'
folder6 = 'L=256 M0=0.004 Temp=0.05 H=0.1/'
folder7 = 'L=512 M0=1 Temp=0.1 H=0.1/'
folder8 = 'L=256 M0=1 Temp=0.1 H=0.1/'
folder9 = 'L=128 M0=1 Temp=0.1 H=0.1/'


#--------Size Dependance-----------------------------

set output 'SizeDpendance.Log.pdf'
file = 'Magnetization.dat'
set grid
#f(x) = -a*x + b
#fit [1:3] f(x) file u (log10($2)):(log10($3)) via a,b
set title 'M0=1 Temp=0.1 H=0.1'
#set label 1 at 1,-0.2 'Fit f(x)=-a*x+b, a=0.0582(5)'
set xlab 'Log Tiempo[MCS]'
set ylab 'Log Magnetización'
set yrange [-3:0.5]
plot folder1.file u (log10($2)):(log10($3)) w lp ps 0.7 pt 7 t 'L=064',folder9.file u (log10($2)):(log10($3)) w lp ps 0.7 pt 7 t 'L=128',\
	folder8.file u (log10($2)):(log10($3)) w lp ps 0.7 pt 7 t 'L=256',folder7.file u (log10($2)):(log10($3)) w lp ps 0.7 pt 7 t 'L=512'
set output

set output 'SizeDpendance.pdf'
file = 'Magnetization.dat'
set grid
#f(x) = -a*x + b
#fit [1:3] f(x) file u (log10($2)):(log10($3)) via a,b
#set title 'L=256 M0=0.004 Temp=0.1 H=0.1'
#set label 1 at 1,-0.2 'Fit f(x)=-a*x+b, a=0.0582(5)'
set xlab 'Tiempo[MCS]'
set ylab 'Magnetización'
set yrange [-0.5:1.5]
plot [:100] folder1.file u 2:3 w lp ps 0.7 pt 7 t 'L=064', folder7.file u 2:3 w lp ps 0.7 pt 7 t 'L=512',\
	folder8.file u 2:3 w lp ps 0.7 pt 7 t 'L=256', folder9.file u 2:3 w lp ps 0.7 pt 7 t 'L=128'
set output

#---------M0 DEPENDANCE----------------

set output 'M0Dpendance.Log.pdf'
file = 'Magnetization.dat'
set grid
#f(x) = -a*x + b
#fit [1:3] f(x) file u (log10($2)):(log10($3)) via a,b
set title 'L=256 Temp=0.1 H=0.1'
#set label 1 at 1,-0.2 'Fit f(x)=-a*x+b, a=0.0582(5)'
set xlab 'Log Tiempo[MCS]'
set ylab 'Log Magnetización'
set yrange [-3:0.5]
plot folder4.file u (log10($2)):(log10($3)) w lp ps 0.7 pt 7 t 'm0=0.02',folder5.file u (log10($2)):(log10($3)) w lp ps 0.7 pt 7 t 'm0=0.004',\
	folder8.file u (log10($2)):(log10($3)) w lp ps 0.7 pt 7 t 'm0=1.0'
set output

set output 'M0Dpendance.pdf'
file = 'Magnetization.dat'
set grid
#f(x) = -a*x + b
#fit [1:3] f(x) file u (log10($2)):(log10($3)) via a,b
#set title 'L=256 M0=0.004 Temp=0.1 H=0.1'
#set label 1 at 1,-0.2 'Fit f(x)=-a*x+b, a=0.0582(5)'
set xlab 'Tiempo[MCS]'
set ylab 'Magnetización'
set yrange [-0.5:1.5]
plot [:100] folder5.file u 2:3 w lp ps 0.7 pt 7 t 'm0=0.004',folder4.file u 2:3 w lp ps 0.7 pt 7 t 'm0=0.02',\
	folder8.file u 2:3 w lp ps 0.7 pt 7 t 'm0=1.0'
set output

#------------TEMP DEPENDANCE--------------------


set output 'TempDpendance.Log.pdf'
file = 'Magnetization.dat'
set grid
#f(x) = -a*x + b
#fit [1:3] f(x) file u (log10($2)):(log10($3)) via a,b
set title 'L=256 M0=0.004 H=0.1'
#set label 1 at 1,-0.2 'Fit f(x)=-a*x+b, a=0.0582(5)'
set xlab 'Log Tiempo[MCS]'
set ylab 'Log Magnetización'
set yrange [-3:0.5]
plot folder6.file u (log10($2)):(log10($3)) w lp ps 0.7 pt 7 t 'Temp=0.05',folder5.file u (log10($2)):(log10($3)) w lp ps 0.7 pt 7 t 'Temp=0.1'
set output

set output 'TempDpendance.pdf'
file = 'Magnetization.dat'
set grid
#f(x) = -a*x + b
#fit [1:3] f(x) file u (log10($2)):(log10($3)) via a,b
#set title 'L=256 M0=0.004 Temp=0.1 H=0.1'
#set label 1 at 1,-0.2 'Fit f(x)=-a*x+b, a=0.0582(5)'
set xlab 'Tiempo[MCS]'
set ylab 'Magnetización'
set yrange [-0.5:1.5]
plot [:100] folder6.file u 2:3 w lp ps 0.7 pt 7 t 'Temp=0.05', folder5.file u 2:3 w lp ps 0.7 pt 7 t 'Temp=0.1'
set output


#--------------EXTERNAL FIELD DEPENDANCE-----------

set output 'HDpendance.Log.pdf'
file = 'Magnetization.dat'
set grid
#f(x) = -a*x + b
#fit [1:3] f(x) file u (log10($2)):(log10($3)) via a,b
set title 'L=100 M0=0.02 Temp=0.1 '
#set label 1 at 1,-0.2 'Fit f(x)=-a*x+b, a=0.0582(5)'
set xlab 'Log Tiempo[MCS]'
set ylab 'Log Magnetización'
set yrange [-3:0.5]
plot folder2.file u (log10($2)):(log10($3)) w lp ps 0.7 pt 7 t 'H=0.05',folder22.file u (log10($2)):(log10($3)) w lp ps 0.7 pt 7 t 'H=0.1'
set output

set output 'HDpendance.pdf'
file = 'Magnetization.dat'
set grid
#f(x) = -a*x + b
#fit [1:3] f(x) file u (log10($2)):(log10($3)) via a,b
#set title 'L=256 M0=0.004 Temp=0.1 H=0.1'
#set label 1 at 1,-0.2 'Fit f(x)=-a*x+b, a=0.0582(5)'
set xlab 'Tiempo[MCS]'
set ylab 'Magnetización'
set yrange [-0.5:1.5]
plot [:100] folder2.file u 2:3 w lp ps 0.7 pt 7 t 'H=0.05', folder22.file u 2:3 w lp ps 0.7 pt 7 t 'H=0.1'
set output