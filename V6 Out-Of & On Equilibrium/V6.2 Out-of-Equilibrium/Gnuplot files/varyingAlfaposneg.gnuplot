#Export graph
set term pngcairo dashed enhanced size 900,650
set output "simpleF1AlfaT=1L=20.png"

set datafile separator ','

file1="../L=020/Temp=  1.0/111/Inter20L.dat"
file3="../L=020/Temp=  1.0/151/Inter20L.dat"
file5="../L=020/Temp=  1.0/121/Inter20L.dat"
file8="../L=020/Temp=  1.0/11001/Inter20L.dat"
file9="../L=020/Temp=  1.0/1101/Inter20L.dat"

#set key box on top left
set key off
set grid
set xlabel 'Log10 Tiempo [MCS]'
set ylabel 'Log10 Segregación f1up'
#
set title 'Segregacion a T=1.0 J/kb y L=20' 

#colors from: https://www2.uni-hamburg.de/Wiss/FB/15/Sustainability/schneider/gnuplot/colors.htm

set style line 1 lt rgb "0x000080" lw 3 #color:navy
set label 1 '{/Symbol a} = 1,1,1' at 5,-0.33 rotate by 3 center tc ls 1 font "Helvetica Bold,12"

set style line 3 lt rgb "#00FF0000" lw 3 #red
set label 3 '{/Symbol a} = 1,5,1' at 5,-0.415 rotate by 0 center tc ls 3 font "Helvetica Bold,12"

set style line 5 lt rgb "cyan" lw 3
set label 5 '{/Symbol a} = 1,2,1' at 5,-0.38 rotate by 0 center tc ls 5 font "Helvetica Bold,12"

set style line 2 lt rgb "#0000FF00" lw 1 #green
set label 2 '{/Symbol a} = 1,100,1' at 4,-0.425 rotate by 0 center tc ls 2 font "Helvetica Bold,12"

set style line 4 lt rgb "#000000FF" lw 3 #blue
set label 4 '{/Symbol a} = 1,10,1' at 5.5,-0.425 rotate by 0 center tc ls 4 font "Helvetica Bold,12"

#set yrange [-0.43:-0.3]

plot file1 u (log10($1)):(log10($3)) w l t 'Alfa 1,1,1' ls 1,\
	file3 u (log10($1)):(log10($3)) w l t 'Alfa 1,5,1' ls 3,\
	file5 u (log10($1)):(log10($3)) w l t 'Alfa 1,2,1' ls 5,\
	file9 u (log10($1)):(log10($3)) w l t 'Alfa 1,10,1' ls 4,\
	file8 u (log10($1)):(log10($3)) w l t 'Alfa 1,100,1' ls 2

replot

##Export graph
#set term pngcairo dashed enhanced size 900,650
#set output "simpleF4AlfaT=1L=20.png"#

#set ylabel 'Log10 Segregación f4up'
##
#set title 'Segregacion a T=1.0 J/kb y L=20' #

#plot file1 u (log10($1)):(log10($9)) w l t 'Alfa 1,1,1' ls 1,\
#	file3 u (log10($1)):(log10($9)) w lp t 'Alfa 1,5,1' ls 3,\
#	file5 u (log10($1)):(log10($9)) w l t 'Alfa 1,2,1' ls 5,\
#	file9 u (log10($1)):(log10($9)) w lp t 'Alfa 1,10,1' ls 4,\
#	file8 u (log10($1)):(log10($9)) w lp t 'Alfa 1,100,1' ls 2#

#replot