set datafile separator ','

set terminal wxt enhanced background rgb 'cyan'

set palette model RGB defined (0 "red",1 "blue",2 "green")

file = "../Graphs new/L=512/Temp=5.0/spinSteps.dat"
set key off
set xlabel 'm [ncols]'
set ylabel 'l [nrows]'
set xrange [500:1500]
set yrange [1:512]


set title 'Spin steps at t=10mil MCS'
plot file u (($1==10000)? $2:1/0):3:4 w p palette pt 7 ps .5

#Export graph
set term pngcairo enhanced size 900,650 background rgb 'cyan'
set output "spinStep10milMCS.png"
replot



set title 'Spin steps at t=20mil MCS'
plot file u (($1==20000)? $2:1/0):3:4 w p palette pt 7 ps .5

#Export graph
set term pngcairo enhanced size 900,650 background rgb 'cyan'
set output "spinStep20milMCS.png"
replot



set title 'Spin steps at t=30mil MCS'
plot file u (($1==30000)? $2:1/0):3:4 w p palette pt 7 ps .5

#Export graph
set term pngcairo enhanced size 900,650 background rgb 'cyan'
set output "spinStep30milMCS.png"
replot

set xrange restore
