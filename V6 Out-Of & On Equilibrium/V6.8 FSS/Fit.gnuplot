set terminal pdfcairo

#L=32
file ='FitResults/cumulante32.dat'
f(x) = a*x**3 + b*x**2 + c*x + d
fit f(x) file u 1:2 via a,b,c,d

set fit errorvariables
set print "FitResults/fitL=32.dat"
print 'Var 	Error'
print a,a_err
print b,b_err
print c,c_err
print d,d_err
set print

#L=64
file2 ='FitResults/cumulante64.dat'
g(x) = e*x**3 + m*x**2 + n*x + o
fit g(x) file2 u 1:2 via e,m,n,o

set fit errorvariables
set print "FitResults/fitL=64.dat"
print 'Var 	Error'
print e,e_err
print m,m_err
print n,n_err
print o,o_err
set print

set output 'FitResults/CumulanteL=32y64.pdf'
plot file u 1:2 w p pt 7 ps 2 t 'Cumulante L=32', f(x) w l lw 3 t 'Ajuste poly L=32',file2 u 1:2 w p pt 7 ps 2 t 'Cumulante L=64', g(x) w l lw 3 t 'Ajuste poly L=64'
set print



#L=128
file ='FitResults/cumulante128.dat'
f(x) = a*x**3 + b*x**2 + c*x + d
fit f(x) file u 1:2 via a,b,c,d

set fit errorvariables
set print "FitResults/fitL=128.dat"
print 'Var 	Error'
print a,a_err
print b,b_err
print c,c_err
print d,d_err
set print

set output 'FitResults/CumulanteL=64y128.pdf'
plot file2 u 1:2 w p pt 7 ps 2 t 'Cumulante L=64', g(x) w l lw 3 t 'Ajuste poly L=64', file u 1:2 w p pt 7 ps 2 t 'Cumulante L=128', f(x) w l lw 3 t 'Ajuste poly L=128'
set print


#L=256
file2 ='FitResults/cumulante256.dat'
g(x) = e*x**3 + m*x**2 + n*x + o
fit g(x) file2 u 1:2 via e,m,n,o

set fit errorvariables
set print "FitResults/fitL=256.dat"
print 'Var 	Error'
print e,e_err
print m,m_err
print n,n_err
print o,o_err
set print

set output 'FitResults/CumulanteL=128y256.pdf'
plot file u 1:2 w p pt 7 ps 2 t 'Cumulante L=128', f(x) w l lw 3 t 'Ajuste poly L=128',file2 u 1:2 w p pt 7 ps 2 t 'Cumulante L=256', g(x) w l lw 3 t 'Ajuste poly L=256'
set print

#L=512
file ='FitResults/cumulante512.dat'
f(x) = a*x**3 + b*x**2 + c*x + d
fit f(x) file u 1:2 via a,b,c,d

set fit errorvariables
set print "FitResults/fitL=512.dat"
print 'Var 	Error'
print a,a_err
print b,b_err
print c,c_err
print d,d_err
set print


set output 'FitResults/CumulanteL=256y512.pdf'
plot file2 u 1:2 w p pt 7 ps 2 t 'Cumulante L=256', g(x) w l lw 3 t 'Ajuste poly L=256', file u 1:2 w p pt 7 ps 2 t 'Cumulante L=512', f(x) w l lw 3 t 'Ajuste poly L=512'
set print


set output 'FitResults/junkpdf.pdf'
set print