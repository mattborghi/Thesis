#Export graph
set term pngcairo dashed enhanced size 900,650
set output "extremeSegAlfaT=1L=20.png"

set datafile separator ','

file1="../L=020/Temp=  1.0/111/Inter20L.dat"
file2="../L=020/Temp=  1.0/210/Inter20L.dat"
file3="../L=020/Temp=  1.0/1011/Inter20L.dat"
file4="../L=020/Temp=  1.0/211/Inter20L.dat"
file5="../L=020/Temp=  1.0/11-2/Inter20L.dat"
file6="../L=020/Temp=  1.0/101-10/Inter20L.dat"
file7="../L=020/Temp=  1.0/65-5/Inter20L.dat"

#set key box on top left
set key off
set grid
set xlabel 'Log10 Tiempo [MCS]'
set ylabel 'Log10 Segregación f1'
#
set title 'Segregacion a T=1.0 J/kb y L=20' 

#colors from: https://www2.uni-hamburg.de/Wiss/FB/15/Sustainability/schneider/gnuplot/colors.htm

set style line 1 lt rgb "0x000080" lw 3 #color:navy
set label 1 '{/Symbol a} = 1,1,1' at 5,-0.36 rotate by 0 center tc ls 1 font "Helvetica Bold,12"

set style line 3 lt rgb "#00FF0000" lw 3 #red
set label 3 '{/Symbol a} = 2,1,0' at 5,-0.31 rotate by 0 center tc ls 3 font "Helvetica Bold,12"

set style line 5 lt rgb "cyan" lw 3
set label 5 '{/Symbol a} = 10,1,1' at 5,-0.53 rotate by 0 center tc ls 5 font "Helvetica Bold,12"

set style line 2 lt rgb "#0000FF00" lw 3 #green
set label 2 '{/Symbol a} = 2,1,1' at 5,-0.42 rotate by 0 center tc ls 2 font "Helvetica Bold,12"

set style line 4 lt rgb "#000000FF" lw 3 #blue
set label 4 '{/Symbol a} = 1,1,-2' at 5,-0.58 rotate by 0 center tc ls 4 font "Helvetica Bold,12"

set style line 6 lt rgb "magenta" lw 3
set label 6 '{/Symbol a} = 10,1,-10' at 5,-0.62 rotate by 0 center tc ls 6 font "Helvetica Bold,12"

set style line 7 lt rgb "black" lw 3
set label 7 '{/Symbol a} = 6,5,-5' at 4,-0.54 rotate by 0 center tc ls 7 font "Helvetica Bold,12"

#set yrange [-0.43:-0.3]

plot file1 u (log10($1)):(log10($3)) w l t 'Alfa 1,1,1' ls 1,\
	file2 u (log10($1)):(log10($3)) w l t 'Alfa 2,1,0' ls 3,\
	file3 u (log10($1)):(log10($3)) w l t 'Alfa 10,1,1' ls 5,\
	file4 u (log10($1)):(log10($3)) w l t 'Alfa 2,1,1' ls 2,\
	file5 u (log10($1)):(log10($3)) w l t 'Alfa 1,1,-2' ls 4,\
	file6 u (log10($1)):(log10($3)) w l t 'Alfa 10,1,-10' ls 6,\
	file7 u (log10($1)):(log10($3)) w l t 'Alfa 6,5,-5' ls 7
	
replot

