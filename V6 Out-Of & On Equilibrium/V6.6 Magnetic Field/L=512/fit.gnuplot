set term pdfcairo
set output 'magnetfit.pdf'
H = 0.1015
f(x) = tanh(H/x)
set title 'Ajuste magnetización L=512 y H=0.1015'
set xlab 'Temperatura'
set ylab 'Magnetización'
set grid
plot 'fitmagnet.data' u ((($2>0.1)&($2<0.103))?$1:1/0):3 t 'Datos simulación' w p ps 0.7 pt 7,f(x) w l lw 2.0 t 'f(T,H=0.1015)=tanh(H/T)'
set output