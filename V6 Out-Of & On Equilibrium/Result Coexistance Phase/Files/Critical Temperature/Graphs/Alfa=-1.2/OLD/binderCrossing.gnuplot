set term pdfcairo
set output 'binderCrossing.pdf'
file = "binderCrossing.dat"

f(x) = a*x + b

fit f(x) file u (1/$1):2 via a,b
set grid
set key left
set xlab 'Tamaño de la red^{⁻1}'
set ylab 'Temperatura Crítica'
set label 1 at 100,0.2268 'T_c = 0.22761'
plot file u (1/$1):2 w p pt 7 ps 2 t 'Puntos', f(x) w l lw 3.0 lc rgb 'red' t 'Ajuste T_c(L) = T_c + AL^{-1}'
set output