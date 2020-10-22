#Estudio del ancho de la interfaz en funcion del tiempo
file1 = "../Graphs new/L=006/Temp=2.3/Roughness.dat"
file2 = "../Graphs new/L=012/Temp=2.3/Roughness.dat"
file3 = "../Graphs new/L=024/Temp=2.3/Roughness.dat"
file4 = "../Graphs new/L=032/Temp=2.3/Roughness.dat"
file5 = "../Graphs new/L=048/Temp=2.3/Roughness.dat"
file6 = "../Graphs new/L=128/Temp=2.3/Roughness.dat"
file7 = "../Graphs new/L=256/Temp=2.3/Roughness.dat"
fileOutput = "FitResults.log"
set datafile separator ','


#Label
set xlabel "Log Tiempo [MCS]"
set ylabel "Log W(t)"
#Grid
set grid
#Title
set title 'LogLog Plot Rugosidad vs t a T=Tc=2.269J/kB'
#Key
set key on top left box
#set yrange [1.0:2.0]

FIT_MAXITER = 10
#L=6
f(x) = a*x+b
fit [3:6] f(x) file1 u  (log10($1)):(log10($5)) via a,b
#L=12
g(x) = c*x+d
fit [3:6] g(x) file2 u  (log10($1)):(log10($5)) via c,d
#L=24
h(x) = e*x+f
fit [3:6] h(x) file3 u  (log10($1)):(log10($5)) via e,f
#L=32
i(x) = j*x+k
fit [3:6] i(x) file4 u  (log10($1)):(log10($5)) via j,k
#L=48
l(x) = m*x+n
fit [3:6] l(x) file5 u  (log10($1)):(log10($5)) via m,n
#L=128
o(x) = p*x+q
fit [3:6] o(x) file6 u  (log10($1)):(log10($5)) via p,q
#L=256
r(x) = s*x+t
fit [3:6] r(x) file7 u  (log10($1)):(log10($5)) via s,t



set print fileOutput
print 'L, Roughnees, Rough_Err'
print 6,",",a,",",a_err
print 12,",",c,",",c_err
print 24,",",e,",",e_err
print 32,",",j,",",j_err
print 48,",",m,",",m_err
print 128,",",p,",",p_err
print 256,",",s,",",s_err
set print

set for [i=1:8] linetype i dt i
set style line 1 lt 1 lc rgb "red" lw 3
set style line 2 lt 1 lc rgb "black" lw 3
set style line 3 lt 1 lc rgb "yellow" lw 3
set style line 4 lt 1 lc rgb "blue" lw 3
set style line 6 lt 1 lc rgb "brown" lw 3
set style line 5 lt 5 lc rgb "cyan" lw 3
set style line 7 lt 1 lc rgb "magenta" lw 3
set style line 8 lt 1 lc rgb "green" lw 3

h(x) = 0.5*x - 0.5
plot file1 u (log10($1)):(log10($8)) t 'L=006' w l ls 1,\
	file2 u (log10($1)):(log10($8)) t 'L=012' w l ls 2,\
	file3 u (log10($1)):(log10($8)) t 'L=024' w l ls 3,\
	file4 u (log10($1)):(log10($8)) t 'L=032' w l ls 4,\
	file5 u (log10($1)):(log10($8)) t 'L=048' w l ls 6,\
	file6 u (log10($1)):(log10($8)) t 'L=128' w l ls 7,\
	file7 u (log10($1)):(log10($8)) t 'L=256' w l ls 8,\
	h(x) w l ls 5 t 'Recta pendiente 0.5'
	#f(x) lw 3 lt 2 lc rgb 'yellow' t sprintf("y=A*x**B, A=%.3f +- %.4f; B=%.3f +- %.3f",a,a_err,b,b_err)#,h(x) lw 3.0
	
#Export graph
set term pngcairo dashed enhanced size 900,650
set output "logTcWt.png"
replot

#Label
set xlabel "Tiempo [MCS] (x10^4)"
set ylabel "W(t)"
set key width 3
#Title
set title 'Plot Rugosidad vs t a T=Tc=2.269J/kB'

plot file1 u ($1/10000):5 t 'L=006' w l ls 1,\
	file2 u ($1/10000):5 t 'L=012' w l ls 2,\
	file3 u ($1/10000):5 t 'L=024' w l ls 3,\
	file4 u ($1/10000):5 t 'L=032' w l ls 4,\
	file5 u ($1/10000):5 t 'L=048' w l ls 6,\
	file6 u ($1/10000):5 t 'L=128' w l ls 7,\
	file7 u ($1/10000):5 t 'L=256' w l ls 8
#Export graph
set term pngcairo dashed enhanced size 900,650
set output "TcWt.png"
replot


#Label
set xlabel "Log Tiempo [MCS]"
set ylabel "Log W(t)"
set key width 3
#Title
set title 'Log-Log Plot Rugosidad vs t a T=Tc=2.269J/kB para L=256'

plot file7 u (log10($1)):(log10($8)) t 'L=256' w l ls 8,\
	r(x) w l ls 5 t sprintf("y=A*x**B, A=%.4f +- %.4f; B=%.4f +- %.4f",s,s_err,t,t_err) 
	
#Export graph
set term pngcairo dashed enhanced size 900,650
set output "TcWtL=256.png"
replot
