set term pdfcairo
set datafile separator ','
folder = 'Graphs/'
Tc = 2.269
inputFile = 'OutputFile.dat'
set output folder.'Suscept.pdf'
#Suscept
plot inputFile u 3:($1 == 32? ($12):1/0) w p pt 7 ps 1 t 'L=032', inputFile u 3:($1==64?($12):1/0) w p pt 7 ps 1 t 'L=064',\
	inputFile u 3:($1==128?($12):1/0) w p pt 7 ps 1 t 'L=128',inputFile u 3:($1==256?($12):1/0) w p pt 7 ps 1 t 'L=256',\
	inputFile u 3:($1==512?($12):1/0) w p pt 7 ps 1 t 'L=512',inputFile u 3:($1==1024?($12):1/0) w p pt 7 ps 1 t 'L=1024',\
	inputFile u 3:($1==2048?($12):1/0) w p pt 7 ps 1 t 'L=2048'
set print

set output folder.'closeSuscept.pdf'
#Suscept
plot [2.25:2.3] inputFile u 3:($1 == 32? ($12):1/0) w p pt 7 ps 1 t 'L=032', inputFile u 3:($1==64?($12):1/0) w p pt 7 ps 1 t 'L=064',\
	inputFile u 3:($1==128?($12):1/0) w p pt 7 ps 1 t 'L=128',inputFile u 3:($1==256?($12):1/0) w p pt 7 ps 1 t 'L=256',\
	inputFile u 3:($1==512?($12):1/0) w p pt 7 ps 1 t 'L=512',inputFile u 3:($1==1024?($12):1/0) w p pt 7 ps 1 t 'L=1024',\
	inputFile u 3:($1==2048?($12):1/0) w p pt 7 ps 1 t 'L=2048'
set print

set output folder.'collapseSuscept.pdf'
#Suscept
exp = -7/4
plot [-5:5] inputFile u (32*($3-Tc)/Tc):($1 == 32? ($12*32**exp):1/0) w p pt 7 ps 1 t 'L=032', inputFile u (64*($3-Tc)/Tc):($1==64?($12*64**exp):1/0) w p pt 5 ps 1 t 'L=064',\
	inputFile u (128*($3-Tc)/Tc):($1==128?($12*128**exp):1/0) w p pt 8 ps 1 t 'L=128',inputFile u (256*($3-Tc)/Tc):($1==256?($12*256**exp):1/0) w p pt 13 ps 1 t 'L=256',\
	inputFile u (512*($3-Tc)/Tc):($1==512?($12*512**exp):1/0) w p pt 11 ps 1 t 'L=512',inputFile u (1024*($3-Tc)/Tc):($1==1024?($12*1024**exp):1/0) w p pt 11 ps 1 t 'L=1024',\
	inputFile u (2048*($3-Tc)/Tc):($1==2048?($12*2048**exp):1/0) w p pt 11 ps 1 t 'L=2048'
set print


set output folder.'Cumulant.pdf'
#Cumulant
plot inputFile u 3:($1 == 32? ($13):1/0) w p pt 7 ps 0.7 t 'L=032', inputFile u 3:($1==64?($13):1/0) w p pt 7 ps 0.7 t 'L=064',\
	inputFile u 3:($1==128?($13):1/0) w p pt 7 ps 0.7 t 'L=128',inputFile u 3:($1==256?($13):1/0) w p pt 7 ps 0.7 t 'L=256',\
	inputFile u 3:($1==512?($13):1/0) w p pt 7 ps 0.7 t 'L=512',inputFile u 3:($1==1024?($13):1/0) w p pt 7 ps 0.7 t 'L=1024',\
	inputFile u 3:($1==2048?($13):1/0) w p pt 7 ps 0.7 t 'L=2048'
set print


set output folder.'closeCumulant.pdf'
# close Cumulant
set key left bottom
plot [2.245:2.295] inputFile u 3:($1 == 32? ($13):1/0) w p pt 7 ps 0.7 t 'L=032', inputFile u 3:($1==64?($13):1/0) w p pt 7 ps 0.7 t 'L=064',\
	inputFile u 3:($1==128?($13):1/0) w p pt 7 ps 0.7 t 'L=128',inputFile u 3:($1==256?($13):1/0) w p pt 7 ps 0.7 t 'L=256',\
	inputFile u 3:($1==512?($13):1/0) w p pt 7 ps 0.7 t 'L=512',inputFile u 3:($1==1024?($13):1/0) w p pt 7 ps 0.7 t 'L=1024',\
	inputFile u 3:($1==2048?($13):1/0) w p pt 7 ps 0.7 t 'L=2048'
set print

set output folder.'collapseCumulant.pdf'
# Cumulant collapse
plot [-5:5] inputFile u (32*($3-Tc)/Tc):($1 == 32? ($13):1/0) w p pt 7 ps 1 t 'L=032', inputFile u (64*($3-Tc)/Tc):($1==64?($13):1/0) w p pt 5 ps 1 t 'L=064',\
	inputFile u (128*($3-Tc)/Tc):($1==128?($13):1/0) w p pt 9 ps 1 t 'L=128',inputFile u (256*($3-Tc)/Tc):($1==256?($13):1/0) w p pt 13 ps 1 t 'L=256',\
	inputFile u (512*($3-Tc)/Tc):($1==512?($13):1/0) w p pt 11 ps 1 t 'L=512',inputFile u (1024*($3-Tc)/Tc):($1==1024?($13):1/0) w p pt 11 ps 1 t 'L=1024',\
	inputFile u (2048*($3-Tc)/Tc):($1==2048?($13):1/0) w p pt 11 ps 1 t 'L=2048'
set print

set output folder.'closecorrelationTime.pdf'
# Correlation time
plot [2.2:2.3] inputFile u 3:($1 == 32? ($6):1/0) w lp pt 7 ps 1 t 'L=032', inputFile u 3:($1==64?($6):1/0) w lp pt 7 ps 1 t 'L=064',\
	inputFile u 3:($1==128?($6):1/0) w lp pt 7 ps 1 t 'L=128', inputFile u 3:($1==256?($6):1/0) w lp pt 7 ps 1 t 'L=256',\
	inputFile u 3:($1==512?($6):1/0) w lp pt 7 ps 1 t 'L=512',inputFile u 3:($1==1024?($6):1/0) w lp pt 7 ps 3 t 'L=1024',\
	inputFile u 3:($1==2048?($6):1/0) w lp pt 7 ps 3 t 'L=2048'
set print

set output folder.'correlationTime.pdf'
# Correlation time
plot inputFile u 3:($1 == 32? ($6):1/0) w lp pt 7 ps 1 t 'L=032', inputFile u 3:($1==64?($6):1/0) w lp pt 7 ps 1 t 'L=064',\
	inputFile u 3:($1==128?($6):1/0) w lp pt 7 ps 1 t 'L=128', inputFile u 3:($1==256?($6):1/0) w lp pt 7 ps 1 t 'L=256',\
	inputFile u 3:($1==512?($6):1/0) w lp pt 7 ps 1 t 'L=512',inputFile u 3:($1==1024?($6):1/0) w lp pt 7 ps 3 t 'L=1024',\
	inputFile u 3:($1==2048?($6):1/0) w lp pt 7 ps 1 t 'L=2048'
set print

set output folder.'Magnetization.pdf'
# Magnetization
plot inputFile u 3:($1 == 32? ($11):1/0) w p pt 7 ps 1 t 'L=032', inputFile u 3:($1==64?($11):1/0) w p pt 7 ps 1 t 'L=064',\
	inputFile u 3:($1==128?($11):1/0) w p pt 7 ps 1 t 'L=128', inputFile u 3:($1==256?($11):1/0) w p pt 7 ps 1 t 'L=256',\
	inputFile u 3:($1==512?($11):1/0) w p pt 7 ps 1 t 'L=512',inputFile u 3:($1==1024?($11):1/0) w p pt 7 ps 1 t 'L=1024',\
	inputFile u 3:($1==2048?($11):1/0) w p pt 7 ps 1 t 'L=2048'
set print

set output folder.'collapseMagnetization.pdf'
# Magnetization
exp = 1/8
plot [-5:5] inputFile u (32*($3-Tc)/Tc):($1 == 32? ($11*32**exp):1/0) w p pt 7 ps 1 t 'L=032', inputFile u (64*($3-Tc)/Tc):($1==64?($11*64**exp):1/0) w p pt 5 ps 1 t 'L=064',\
	inputFile u (128*($3-Tc)/Tc):($1==128?($11*128**exp):1/0) w p pt 9 ps 1 t 'L=128', inputFile u (256*($3-Tc)/Tc):($1==256?($11*256**exp):1/0) w p pt 13 ps 1 t 'L=256',\
	inputFile u (512*($3-Tc)/Tc):($1==512?($11*512**exp):1/0) w p pt 11 ps 1 t 'L=512',inputFile u (1024*($3-Tc)/Tc):($1==1024?($11*1024**exp):1/0) w p pt 11 ps 1 t 'L=1024',\
	inputFile u (2048*($3-Tc)/Tc):($1==2048?($11*2048**exp):1/0) w p pt 11 ps 1 t 'L=2048'
set print


set output folder.'Energy.pdf'
# Energy
plot inputFile u 3:($1 == 32? ($9):1/0) w p pt 7 ps 1 t 'L=032', inputFile u 3:($1==64?($9):1/0) w p pt 7 ps 1 t 'L=064',\
	inputFile u 3:($1==128?($9):1/0) w p pt 7 ps 1 t 'L=128', inputFile u 3:($1==256?($9):1/0) w p pt 7 ps 1 t 'L=256',\
	inputFile u 3:($1==512?($9):1/0) w p pt 7 ps 1 t 'L=512', inputFile u 3:($1==1024?($9):1/0) w p pt 7 ps 3 t 'L=1024',\
	inputFile u 3:($1==2048?($9):1/0) w p pt 7 ps 3 t 'L=2048'
set print

set output folder.'heatCapacity.pdf'
# Heat capacity
plot inputFile u 3:($1 == 32? ($10):1/0) w lp pt 7 ps 1 t 'L=032', inputFile u 3:($1==64?($10):1/0) w lp pt 7 ps 1 t 'L=064',\
	inputFile u 3:($1==128?($10):1/0) w lp pt 7 ps 1 t 'L=128', inputFile u 3:($1==256?($10):1/0) w lp pt 7 ps 1 t 'L=256',\
	inputFile u 3:($1==512?($10):1/0) w lp pt 7 ps 1 t 'L=512',inputFile u 3:($1==1024?($10):1/0) w lp pt 7 ps 1 t 'L=1024',\
	inputFile u 3:($1==2048?($10):1/0) w lp pt 7 ps 1 t 'L=2048'
set print


set output folder.'closeheatCapacity.pdf'
# Heat capacity
plot [2:3] inputFile u 3:($1 == 32? ($10):1/0) w lp pt 7 ps 1 t 'L=032', inputFile u 3:($1==64?($10):1/0) w lp pt 7 ps 1 t 'L=064',\
	inputFile u 3:($1==128?($10):1/0) w lp pt 7 ps 1 t 'L=128', inputFile u 3:($1==256?($10):1/0) w lp pt 7 ps 1 t 'L=256',\
	inputFile u 3:($1==512?($10):1/0) w lp pt 7 ps 1 t 'L=512',inputFile u 3:($1==1024?($10):1/0) w lp pt 7 ps 1 t 'L=1024',\
	inputFile u 3:($1==2048?($10):1/0) w lp pt 7 ps 1 t 'L=2048'
set print

set output folder.'collapseheatCapacity.pdf'
# Heat capacity
exp = -0.2
plot [-5:5] inputFile u (32*($3-Tc)/Tc):($1 == 32? ($10*32**exp):1/0) w p pt 7 ps 1 t 'L=032', inputFile u (64*($3-Tc)/Tc):($1==64?($10*64**exp):1/0) w p pt 5 ps 1 t 'L=064',\
	inputFile u (128*($3-Tc)/Tc):($1==128?($10*128**exp):1/0) w p pt 9 ps 1 t 'L=128', inputFile u (256*($3-Tc)/Tc):($1==256?($10*256**exp):1/0) w p pt 13 ps 1 t 'L=256',\
	inputFile u (512*($3-Tc)/Tc):($1==512?($10*512**exp):1/0) w p pt 11 ps 1 t 'L=512',inputFile u (1024*($3-Tc)/Tc):($1==1024?($10*1024**exp):1/0) w p pt 11 ps 1 t 'L=1024',\
	inputFile u (2048*($3-Tc)/Tc):($1==2048?($10*2048**exp):1/0) w p pt 11 ps 1 t 'L=2048'
set print

#Fit dynamic exponent
set output folder.'dynamicExponent.pdf'
set label 'z=0.24(1)' at 7,1.5
g(x) = a*x + b
fit [:6.3] g(x) inputFile u (log($1)):($3==2.27?(log($6)):1/0) via a,b
plot [:10] inputFile u (log($1)):(($3==2.27)?(log($6)):1/0) w p pt 7 ps 3, g(x) w l lw 3
set print


set output