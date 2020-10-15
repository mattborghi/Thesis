#set datafile separator ','
set term pdfcairo
set fit errorvariables

f(x) = a*x**3 + b*x**2 + c*x + d
g(x) = e*x**3 + f*x**2 + g*x + h 
folder = 'Graphs/Alfa=0.5/Fit/'
inputFile = 'OutputFile.dat'
xmin = 1.69
xmax = 1.71
L1 = 128
L2 = 64
fit [xmin:xmax] f(x) inputFile u 3:($1 == L1? ($13):1/0) via a,b,c,d  
fit [xmin:xmax] g(x) inputFile u 3:($1 == L2? ($13):1/0) via e,f,g,h

set output folder.'L='.L1.'y'.L2.'.pdf'
set grid xtics ytics mytics
set mytics 2
set ytics .01
set xtics .002
set grid
plot [xmin:xmax] inputFile u 3:($1 == L1? ($13):1/0) w p pt 7 ps 2 t 'L='.L1,f(x) w l lw 2 lc rgb 'blue' t 'Polinomio grado 3', \
	inputFile u 3:($1==L2?($13):1/0) w p pt 7 ps 2 t 'L='.L2, g(x) w l lw 2 lc rgb 'green' t 'Polinomio grado 3'
set print

set output

set print folder.'L='.L1.'y'.L2.'.fit.dat'
print 'a ',a,a_err
print 'b ',b,b_err
print 'c ',c,c_err
print 'd ',d,d_err
print 'e ',e,e_err
print 'f ',f,f_err
print 'g ',g,g_err
print 'h ',h,h_err 
set print

