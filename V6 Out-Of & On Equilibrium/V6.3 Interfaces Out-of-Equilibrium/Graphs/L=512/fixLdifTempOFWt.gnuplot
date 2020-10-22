file1 = "Temp=1.0 MCS=60mil aver=15/Roughness.dat"
#file1 = "Temp=1.0 MCS=30mil/Roughness.dat"
#file2 = "Temp=2.5 MCS=100mil/Roughness.dat"
file2 = "Temp=2.5 MCS=30mil/Roughness.dat"
file3 = "Temp=5.0 MCS=30mil/Roughness.dat"

set datafile separator ','

set title 'LogLog Rugosidad vs t para L=512 a dist Temp'
set key on top left box
set key width 3

set grid 

#Label
set xlabel "Log Tiempo [MCS]"
set ylabel "Log W(t)"

set for [i=1:6] linetype i dt i
set style line 1 lt 1 lc rgb "red" lw 3
set style line 2 lt 1 lc rgb "black" lw 3
set style line 3 lt 1 lc rgb "yellow" lw 3
set style line 4 lt 1 lc rgb "green" lw 3
set style line 5 lt 1 lc rgb "blue" lw 3
set style line 6 lt 5 lc rgb "cyan" lw 3


plot file1 u (log10($1)):(log10($5)) t 'Temp=1.0' w l ls 1,\
	file2 u (log10($1)):(log10($5)) t 'Temp=2.5' w l ls 2,\
	file3 u (log10($1)):(log10($5)) t 'Temp=5.0' w l ls 3	

#Export graph
set term pngcairo dashed enhanced size 900,650
set output "LogWtL=512.png"
replot


set title 'Rugosidad vs t para L=512 a dist Temp'
#Label
set xlabel "Tiempo [MCS]"
set ylabel "W(t)"

plot [0:30000] file1 u 1:5 t 'Temp=1.0',\
	file2 u 1:5 t 'Temp=2.5',\
	file3 u 1:5 t 'Temp=5.0'

#Export graph
set term pngcairo dashed enhanced size 900,650
set output "WtL=512.png"
replot
