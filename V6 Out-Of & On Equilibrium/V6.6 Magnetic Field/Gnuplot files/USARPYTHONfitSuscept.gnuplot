set term png
#Save fit values in variables with the name var_err
set fit errorvariables

folder= 'L=064'

file = 'fitmagnet.data'

#Me interesan b<m> y e<m^2> para a H->0
f(x) = a*x + b
g(x) = c*x**2+d*x+e

set print 'fitSuscept.data'
print 'Temperature, Magnetization,MagnetError,Magnetization2,Magnet2Error'


#File 6
fit [:0.02] f(x) file6 u 1:2 via a,b
fit [:0.02] g(x) file6 u 1:($2**2) via c,d,e

set output 'Temp0.01.png'
set multiplot layout 1,2 title "Temp = 0.01"
set xtics rotate
set title "Magnetization"
unset key
plot file6 u 1:2 w lp ps 2 pt 7,f(x) w l lw 3
set title "Magnetization2"
unset key
plot file6 u 1:($2**2) w lp ps 2 pt 7,g(x) w l lw 3
unset multiplot
replot

print '0.01 ',b,b_err,e,e_err

#File1
fit [:0.06] f(x) file1 u 1:2 via a,b
fit [:0.06] g(x) file1 u 1:($2**2) via c,d,e

set output 'Temp0.1.png'
set multiplot layout 1,2 title "Temp = 0.1"
set xtics rotate
set title "Magnetization"
unset key
plot file1 u 1:2 w lp ps 2 pt 7,f(x) w l lw 3 
set title "Magnetization2"
unset key
plot file1 u 1:($2**2) w lp ps 2 pt 7,g(x) w l lw 3
unset multiplot
replot

print '0.1 ',b,b_err,e,e_err

#File2
fit [:0.1] f(x) file2 u 1:2 via a,b
fit [:0.1] g(x) file2 u 1:($2**2) via c,d,e

set output 'Temp0.2.png'
set multiplot layout 1,2 title "Temp = 0.2"
set xtics rotate
set title "Magnetization"
unset key
plot file2 u 1:2 w lp ps 2 pt 7,f(x) w l lw 3
set title "Magnetization2"
unset key
plot file2 u 1:($2**2) w lp ps 2 pt 7,g(x) w l lw 3
unset multiplot
replot

print '0.2 ',b,b_err,e,e_err
#File3


fit [:0.1] f(x) file3 u 1:2 via a,b
fit [:0.1] g(x) file3 u 1:($2**2) via c,d,e

set output 'Temp0.3.png'
set multiplot layout 1,2 title "Temp = 0.3"
set xtics rotate
set title "Magnetization"
unset key
plot file3 u 1:2 w lp ps 2 pt 7,f(x) w l lw 3
set title "Magnetization2"
unset key
plot file3 u 1:($2**2) w lp ps 2 pt 7,g(x) w l lw 3
unset multiplot
replot

print '0.3 ',b,b_err,e,e_err

#File4

fit f(x) file4 u 1:2 via a,b
fit g(x) file4 u 1:($2**2) via c,d,e

set output 'Temp0.4.png'
set multiplot layout 1,2 title "Temp = 0.4"
set xtics rotate
set title "Magnetization"
unset key
plot file4 u 1:2 w lp ps 2 pt 7,f(x) w l lw 3
set title "Magnetization2"
unset key
plot file4 u 1:($2**2) w lp ps 2 pt 7,g(x) w l lw 3
unset multiplot
replot

print '0.4 ',b,b_err,e,e_err
#File 5

fit f(x) file5 u 1:2 via a,b
fit g(x) file5 u 1:($2**2) via c,d,e

set output 'Temp1.0.png'
set multiplot layout 1,2 title "Temp = 1.0"
set xtics rotate
set title "Magnetization"
unset key
plot file5 u 1:2 w lp ps 2 pt 7,f(x) w l lw 3
set title "Magnetization2"
unset key
plot file5 u 1:($2**2) w lp ps 2 pt 7,g(x) w l lw 3
unset multiplot
replot

print '1.0 ',b,b_err,e,e_err


set print

