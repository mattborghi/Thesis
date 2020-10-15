set term pdfcairo
set output 'quadraticFit.pdf'

set datafile separator ','

f = 'OutputFile.log'
set xlabel 'Temperatura'
set ylabel 'Cumulante'
set grid 
f(x) = a*x**2+b*x+c
set yrange [-0.1:0.8]
fit [0.22:0.23] f(x) f u 3:8 via a,b,c

plot f u 3:8 w p pt 7 ps 0.7 t 'Datos', f(x) w l lw 2.0 t sprintf( "%.1f(%.1f)x^2+%.1f(%.1f)x+%.1f(%.1f)",a,a_err,b,b_err,c,c_err)
set output