set datafile separator ','

file2 = "L=050/Temp=2.5/test.dat"

time = 2499

#set xrange [400:600]
set title 'f1up'
set xlabel 'm [nrows]'
set ylabel 'f1up'
plot file2 u (($1==time)? $2:1/0):3 w lp pt 7 ps 1
#Export graph
set term pngcairo enhanced size 900,650 background rgb 'white'
set output "f1up.png"
replot

#set xrange restore

set title 'f1dn'
set xlabel 'm [nrows]'
set ylabel 'f1up'
plot file2 u (($1==time)? $2:1/0):4 w lp pt 7 ps 1
#Export graph
set term pngcairo enhanced size 900,650 background rgb 'white'
set output "f1dn.png"
replot

set title 'f1v2'
set xlabel 'm [nrows]'
set ylabel 'f1up v2'
plot file2 u (($1==time)? $2:1/0):5 w lp pt 7 ps 1
#Export graph
set term pngcairo enhanced size 900,650 background rgb 'white'
set output "f1upv2.png"
replot

set key off

set title 'f1 mezcla'
set xlabel 'm [nrows]'
set ylabel 'f1up mezcla'
plot file2 u (($1==time)? $2:1/0):(($2<501)? $3:(1.0-$4)) w lp pt 7 ps 1,\

#Export graph
set term pngcairo enhanced size 900,650 background rgb 'white'
set output "f1upMezcla.png"
replot

