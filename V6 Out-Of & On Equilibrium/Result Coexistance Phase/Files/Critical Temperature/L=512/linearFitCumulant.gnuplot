set term pdfcairo
set output 'linearFit.pdf'

set datafile separator ','

f = 'OutputFile.log'
set xlabel 'Temperatura'
set ylabel 'Cumulante'
set grid 
f(x) = a*x+b
set yrange [0:1]
fit [0.226:0.228] f(x) f u 3:8 via a,b

plot [0.226:0.228] f u 3:8 w p pt 7 ps 0.7 t 'Datos', f(x) w l lw 2.0 t sprintf( "%.1f(%.1f)x+%.1f(%.1f)",a,a_err,b,b_err)
set output