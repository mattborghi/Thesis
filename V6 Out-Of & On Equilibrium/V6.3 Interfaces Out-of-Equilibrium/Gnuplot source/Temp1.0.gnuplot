#Estudio del ancho de la interfaz en funcion del tiempo
file1 = "../Graphs new/L=006/Temp=1.0/Roughness.dat"
file2 = "../Graphs new/L=025/Temp=1.0/Roughness.dat"
file3 = "../Graphs new/L=032/Temp=1.0/Roughness.dat"
#file4 = "../Graphs new/L=128/Temp=1.0 MCS=30mil Aver=30/Roughness.dat"
#file5 = "../Graphs new/L=512/Temp=1.0 MCS=60mil aver=15/Roughness.dat"
fileOutput = "FitResults.log"
set datafile separator ','


#Label
set xlabel "Log Tiempo [MCS]"
set ylabel "Log W(t)"
#Grid
set grid
#Title
set title 'LogLog Plot Rugosidad vs t a T=1.0 J/kB'
#Key
set key center left box
set key width 3
#set yrange [1.0:2.0]

FIT_MAXITER = 10

set for [i=1:7] linetype i dt i
set style line 1 lt 1 lc rgb "red" lw 3
set style line 2 lt 1 lc rgb "black" lw 3
set style line 3 lt 1 lc rgb "yellow" lw 3
set style line 4 lt 1 lc rgb "green" lw 3
set style line 5 lt 1 lc rgb "blue" lw 3
set style line 6 lt 5 lc rgb "cyan" lw 3
set style line 7 lt 5 lc rgb "brown" lw 3

#h(x) = 0.5*x - 0.5
plot file1 u (log10($1)):(log10($8)) t 'L=006' w l ls 1,\
    file2 u (log10($1)):(log10($8)) t 'L=025' w l ls 2,\
	file3 u (log10($1)):(log10($8)) t 'L=032' w l ls 3#,\
	#file4 u (log10($1)):(log10($8)) t 'L=128' w l ls 4,\
	#file5 u (log10($1)):(log10($8)) t 'L=512' w l ls 5#,\
	#h(x) w l ls 6 t 'Recta pendiente 0.5'
	#f(x) lw 3 lt 2 lc rgb 'yellow' t sprintf("y=A*x**B, A=%.3f +- %.4f; B=%.3f +- %.3f",a,a_err,b,b_err)#,h(x) lw 3.0
	
#Export graph
set term pngcairo dashed enhanced size 900,650
set output "logT=1.0Wt.png"
replot


#Label
set xlabel "Tiempo [MCS]"
set ylabel "W(t)"
#Grid
set grid
#Title
set title 'Plot Rugosidad vs t a T=1.0 J/kB'


#No LogLog plot
plot file1 u 1:8 t 'L=006' w l ls 1,\
    file2 u 1:8 t 'L=025' w l ls 2,\
	file3 u 1:8 t 'L=032' w l ls 3#,\
	#file4 u 1:8 t 'L=128' w l ls 4,\
	#file5 u 1:8 t 'L=512' w l ls 5
#Export graph
set term pngcairo dashed enhanced size 900,650
set output "T=1.0Wt.png"
replot


#Title
set title 'Log-Log Plot Rugosidad vs t a T=1.0 J/kB L=6'
#Key
set key on top left box
set key width -3
set yrange [-1.5:0.5]
#Ajuste para el de L=6 (aver=2mil)
#L=6
i(x) = j*x+k
f(x) = b
fit [1.5:3] f(x) file1 u  (log10($1)):(log10($8)) via b
fit [4:4.7] i(x) file1 u  (log10($1)):(log10($8)) via j,k
plot file1 u (log10($1)):(log10($8)) t 'L=006' w l ls 3,\
		i(x) ls 6 t sprintf("y=A*x+B, A=%.1e +- %.1e; B=%.3f +- %.3f",j,j_err,k,k_err),\
		f(x) ls 7 t sprintf("z=D, D=%.1e +- %.3e",b,b_err)
#Export graph
set term pngcairo dashed enhanced size 900,650
set output "FitWtT=1L=6.png"
replot


#############################################

set datafile separator ','

file1 = "../Graphs new/L=006/Temp=1.0/Roughness.dat"
file4 = "../Graphs new/L=012/Temp=1.0 MCS=250mil aver=100/Roughness.dat"
file2 = "../Graphs new/L=025/Temp=1.0 MCS=250mil aver=250/Roughness.dat"
file3 = "../Graphs new/L=032/Temp=1.0 MCS=250mil aver=250/Roughness.dat"

#Title
set title 'Ancho de la interfaz vs t a T=1.0 J/kB'
set key box on top left
set key width 3
set grid
set autoscale xy

plot file4 u 1:8 t 'L=012' w l ls 4 lw 3.0,\
    file2 u 1:8 t 'L=025' w l ls 2 lw 3.0,\
	file3 u 1:8 t 'L=032' w l ls 3 lw 3.0
	#file1 u 1:8 t 'L=006' w l ls 1 lw 3.0,\

#Export graph
set term pngcairo dashed enhanced size 900,650
set output "WtT=1Longer.png"
replot
