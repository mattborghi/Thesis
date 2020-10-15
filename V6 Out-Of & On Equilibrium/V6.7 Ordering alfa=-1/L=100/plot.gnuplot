set term pdfcairo
set output 'magnet.pdf'

set grid
unset key
set xlabel 'Tiempo [MCS/sitio]'
set ylabel 'Magnetización/spin'
set xrange [-0.10:10100]
plot 'Magnetization1.dat' u 2:(abs($3)) w lp pt 7 ps 0.2, 'Magnetization2.dat' u 2:(abs($3)) w lp pt 7 ps 0.2

set output


set output 'enermagnet.pdf'

set grid
set key box on right center
set xlabel 'Tiempo [MCS/sitio]'
set ylabel 'm,u'
set xrange [-0.10:10100]
plot 'Magnetization1.dat' u 2:(abs($3)) w lp pt 7 ps 0.2 t 'Magnetización', 'Magnetization1.dat' u 2:6 w lp pt 7 ps 0.2 t 'Energía'

set output

