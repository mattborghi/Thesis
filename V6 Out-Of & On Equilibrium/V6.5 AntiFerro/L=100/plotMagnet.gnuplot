set datafile separator ','

file = 'MagnetAntivsTemp.csv'

set xlabel 'alfa'
set ylabel '<m>Anti'

plot file u (($2==1.0)?$1:1/0):3 w lp lc rgb 'red'

