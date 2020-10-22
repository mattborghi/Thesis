#Export graph
set term pngcairo dashed enhanced size 900,650
set output "alfaT=1L=20.png"

set datafile separator ','

file1="../L=020/Temp=  1.0/111/Inter20L.dat"
file2="../L=020/Temp=  1.0/555/Inter20L.dat"
file3="../L=020/Temp=  1.0/151/Inter20L.dat"
file4="../L=020/Temp=  1.0/10.51/Inter20L.dat"
file5="../L=020/Temp=  1.0/121/Inter20L.dat"
file6="../L=020/Temp=  1.0/11.51/Inter20L.dat"
file7="../L=020/Temp=  1.0/101/Inter20L.dat"
file8="../L=020/Temp=  1.0/11001/Inter20L.dat"
file9="../L=020/Temp=  1.0/0.50.50.5/Inter20L.dat"
file10="../L=020/Temp=  1.0/0.750.750.75/Inter20L.dat"
file11="../L=020/Temp=  1.0/0.250.250.25/Inter20L.dat"
file12="../L=020/Temp=  1.0/012/Inter20L.dat"
file13="../L=020/Temp=  1.0/210/Inter20L.dat"

#set key box on top left
set key off
set grid
set xlabel 'Log10 Tiempo [MCS]'
set ylabel 'Log10 Segregación f1'
#
set title 'Segregacion a T=1.0 J/kb y L=20' 

#colors from: https://www2.uni-hamburg.de/Wiss/FB/15/Sustainability/schneider/gnuplot/colors.htm

set style line 1 lt rgb "0x000080" lw 3 #color:navy
set label 1 '{/Symbol a} = 1,1,1' at 6,-0.335 rotate by 0 center tc ls 1 

set style line 2 lt rgb "blue" lw 3
set label 2 '{/Symbol a} = 5,5,5' at 5.5,-0.43 rotate by 0 center tc ls 2 font "Helvetica Bold,8"

set style line 3 lt rgb "black" lw 3
set label 3 '{/Symbol a} = 1,5,1' at 6,-0.59 rotate by 0 center tc ls 3 font "Helvetica Bold,8"

set style line 4 lt rgb "green" lw 3
set label 4 '{/Symbol a} = 1,0.5,1' at 6,-0.36 rotate by 0 center tc ls 4

set style line 5 lt rgb "red" lw 3
set label 5 '{/Symbol a} = 1,2,1' at 6,-0.55 rotate by 0 center tc ls 5 font "Helvetica Bold,8"

set style line 6 lt rgb "brown" lw 3
set label 6 '{/Symbol a} = 1,1.5,1' at 6,-0.345 rotate by 0 center tc ls 6

set style line 7 lt rgb "magenta" lw 3
set label 7 '{/Symbol a} = 1,0,1' at 6,-0.39 rotate by 0 center tc ls 7

set style line 8 lt rgb "coral" lw 3
set label 8 '{/Symbol a} = 0.5,0.5,0.5' at 4.5,-0.365 rotate by 0 center tc ls 8

set style line 9 lt rgb "0x00FFFF" lw 1 #color:aqua
set label 9 '{/Symbol a} = 0.75,0.75,0.75' at 2.7,-0.38 rotate by 30 center tc ls 9

set style line 10 lt rgb "0xDC143C" lw 1 #color:crimson
set label 10 '{/Symbol a} = 0.25,0.25,0.25' at 5,-0.49 rotate by 0 center tc ls 10 font "Helvetica Bold,8"

set style line 11 lt rgb "0xBDB76B" lw 1 #color:darkkhaki 
set label 11 '{/Symbol a} = 0,1,2' at 6,-0.33 rotate by 0 center tc ls 11

set style line 12 lt rgb "0xFF8C00" lw 1 #color:darkorange 
set label 12 '{/Symbol a} = 2,1,0' at 5.5,-0.32 rotate by 0 center tc ls 12


set multiplot
set yrange [-0.43:-0.3]
set size 1,1
set origin 0,0
#set logscale x
#set logscale y

plot file1 u (log10($1)):(log10($3)) w l t 'Alfa 1,1,1' ls 1,\
	file2 u (log10($1)):(log10($3)) w l t 'Alfa 5,5,5' ls 2,\
	file3 u (log10($1)):(log10($3)) w l t 'Alfa 1,5,1' ls 3,\
	file4 u (log10($1)):(log10($3)) w l t 'Alfa 1,0.5,1' ls 4,\
	file5 u (log10($1)):(log10($3)) w l t 'Alfa 1,2,1' ls 5,\
	file6 u (log10($1)):(log10($3)) w l t 'Alfa 1,1.5,1' ls 6,\
	file7 u (log10($1)):(log10($3)) w l t 'Alfa 1,0,1' ls 7,\
	file9 u (log10($1)):(log10($3)) w l t 'Alfa 0.5,0.5,0.5' ls 8,\
	file10 u (log10($1)):(log10($3)) w l t 'Alfa 0.75,0.75,0.75' ls 9,file11 u (log10($1)):(log10($3)) w l t 'Alfa 0.25,0.25,0.25' ls 10,\
	file13 u (log10($1)):(log10($3)) w l t 'Alfa 2,1,0' ls 12,\
	file12 u (log10($1)):(log10($3)) w l t 'Alfa 0,1,2' ls 11

#Options for lower plot
set size 0.5,0.3
set origin 0.08,0.63
set xrange [2:6.3]
set autoscale y
#set xlabel ""
set ylabel ""
set title ''
set xtics nomirror#scale 0.5#offset 0,graph 0.05
set ytics nomirror#scale 0.5#offset 0,graph 0.05
unset label 1
unset label 4
unset label 6
unset label 7
unset label 8
unset label 9
unset label 11
unset label 12

plot file1 u (log10($1)):(log10($3)) w l t 'Alfa 1,1,1' ls 1,\
	file2 u (log10($1)):(log10($3)) w l t 'Alfa 5,5,5' ls 2,\
	file3 u (log10($1)):(log10($3)) w l t 'Alfa 1,5,1' ls 3,\
	file4 u (log10($1)):(log10($3)) w l t 'Alfa 1,0.5,1' ls 4,\
	file5 u (log10($1)):(log10($3)) w l t 'Alfa 1,2,1' ls 5,\
	file6 u (log10($1)):(log10($3)) w l t 'Alfa 1,1.5,1' ls 6,\
	file7 u (log10($1)):(log10($3)) w l t 'Alfa 1,0,1' ls 7,\
	file9 u (log10($1)):(log10($3)) w l t 'Alfa 0.5,0.5,0.5' ls 8,\
	file10 u (log10($1)):(log10($3)) w l t 'Alfa 0.75,0.75,0.75' ls 9,file11 u (log10($1)):(log10($3)) w l t 'Alfa 0.25,0.25,0.25' ls 10,\
	file13 u (log10($1)):(log10($3)) w l t 'Alfa 2,1,0' ls 12,\
	file12 u (log10($1)):(log10($3)) w l t 'Alfa 0,1,2' ls 11

unset multiplot

replot

######################################

#Export graph
set term pngcairo enhanced size 900,650
set output "alfa111T=1L=20.png"

set size 1,1
set origin 0,0

set datafile separator ','

#set key box on top left
set grid
set title 'Alfa 1,1,1 Segregación variando densidad y mobilidad para L=20 y Temp=1 J/kb'
set ylabel 'Log Segregación f1'

unset label 2
unset label 3
unset label 5
unset label 10

file1="../L=020/Temp=  1.0/111/Inter20L.dat"
file8="../L=020/Temp=  1.0/111/mu=99/Inter20L.dat"
file9="../L=020/Temp=  1.0/111/N+=0.7/Inter20L.dat"
file2="../L=020/Temp=  1.0/111/N+=0.6/Inter20L.dat"
file3="../L=020/Temp=  1.0/111/N+=0.3/Inter20L.dat"

set style line 1 lt rgb "orange" lw 3 #color:navy
set label 1 '{/Symbol m} = 1, N+=0.5' at 5.25,-0.32 rotate by 0 center tc ls 1 font "Helvetica Bold,12"

set style line 2 lt rgb "blue" lw 3
set label 2 '{/Symbol m} = 99, N+=0.5' at 5.25,-0.38 rotate by 0 center tc ls 2 font "Helvetica Bold,12"

set style line 3 lt rgb "black" lw 3
set label 3 '{/Symbol m} = 1, N+=0.7' at 5.25,-0.18 rotate by 0 center tc ls 3 font "Helvetica Bold,12"

set style line 4 lt rgb "green" lw 3
set label 4 '{/Symbol m} = 1, N+=0.6' at 5.25,-0.235 rotate by 0 center tc ls 4 font "Helvetica Bold,12"

set style line 5 lt rgb "red" lw 3
set label 5 '{/Symbol m} = 1, N+=0.3' at 5.25,-0.54 rotate by 0 center tc ls 5 font "Helvetica Bold,12"

plot file1 u (log10($1)):(log10($3)) w l t 'Alfa 1,1,1' ls 1,\
	file8 u (log10($1)):(log10($3)) w l t 'Alfa 1,1,1 mu=99' ls 2,\
	file9 u (log10($1)):(log10($3)) w l t 'Alfa 1,1,1 n+=0.7' ls 3,\
	file2 u (log10($1)):(log10($3)) w l t 'Alfa 1,1,1 n+=0.6' ls 4,\
	file3 u (log10($1)):(log10($3)) w l t 'Alfa 1,1,1 n+=0.3' ls 5

replot