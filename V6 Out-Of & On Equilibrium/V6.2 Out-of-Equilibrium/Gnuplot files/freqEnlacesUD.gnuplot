#Export graph
set term pngcairo dashed enhanced size 900,650
set output "FreqEnlacesUD.T=1L=20.png"

set datafile separator ','

file1="../L=020/Temp=  1.0/111/Inter20L.dat"
file2="../L=020/Temp=  1.0/121/Inter20L.dat"
file3="../L=020/Temp=  1.0/151/Inter20L.dat"
file4="../L=020/Temp=  1.0/1101/Inter20L.dat"
file5="../L=020/Temp=  1.0/11001/Inter20L.dat"

#set key box on top left
set key off
set grid
set xlabel 'Log10 Tiempo [MCS]'
set ylabel 'Log10 Enlaces fud'
#
set title 'Enlaces up-down a T=1.0 J/kb y L=20' 

#colors from: https://www2.uni-hamburg.de/Wiss/FB/15/Sustainability/schneider/gnuplot/colors.htm

set style line 1 lt rgb "0x000080" lw 3 #color:navy
set label 1 '{/Symbol a} = 1,1,1' at 5,-1 rotate by 0 center tc ls 1 font "Helvetica Bold,12"

set style line 3 lt rgb "#00FF0000" lw 5 #red
set label 3 '{/Symbol a} = 1,2,1' at 5,-0.8 rotate by 0 center tc ls 3 font "Helvetica Bold,12"

set style line 5 lt rgb "cyan" lw 3
set label 5 '{/Symbol a} = 1,5,1' at 5.5,-0.7 rotate by 0 center tc ls 5 font "Helvetica Bold,12"

set style line 2 lt rgb "#0000FF00" lw 3 #green
#set label 2 '{/Symbol a} = 1,10,1' at 5,-0.57 rotate by 0 center tc ls 2 font "Helvetica Bold,12"

set style line 4 lt rgb "#000000FF" lw 1 #blue
#set label 4 '{/Symbol a} = 1,100,1' at 5.5,-0.57 rotate by 0 center tc ls 4 font "Helvetica Bold,12"

#set yrange [-0.43:-0.3]

plot file1 u (log10($1)):(log10(1-$3-$4)) w l t 'Alfa 1,1,1' ls 1,\
	file2 u (log10($1)):(log10(1-$3-$4)) w l t 'Alfa 1,2,1' ls 3,\
	file3 u (log10($1)):(log10(1-$3-$4)) w l t 'Alfa 1,5,1' ls 5,\
	file4 u (log10($1)):(log10(1-$3-$4)) w l t 'Alfa 1,10,1' ls 2,\
	file5 u (log10($1)):(log10(1-$3-$4)) w l t 'Alfa 1,100,1' ls 4
	
replot

