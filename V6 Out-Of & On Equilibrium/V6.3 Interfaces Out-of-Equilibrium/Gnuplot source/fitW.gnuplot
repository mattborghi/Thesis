#Load with the code
# $ gnuplot
# $ load 'equilibrio.gnuplot'
#---------------------------------------------------------------
#Filenames
#Input files
fileInter = 'Inter20L.dat'
#Output files
folder = 'Graphs/'
Interf1 = 'Interf1.png'
Interf2 = 'Interf2.png'
Interf3 = 'Interf3.png'
Interf4 = 'Interf4.png'
Interf = 'Interf.png'
#InterV2 = 'InterV2.png'
ncols = 2000
npoints = 1000 
#---------------------------------------------------------------
set terminal wxt enhanced font "Helvetica,15" size 900,650
#Separator
set datafile separator ','
#Grid
set grid
#Set axis
set auto xy
#Key
set key on top right box
#Label
set xlabel "Time steps"
set ylabel "Interactions"
#Title
set title "f1 interactions"
#Plot f1

#Numero de ptos en W(t)
N = 20
g(x) = int( (npoints-1)/(N-1) *x + ( (N - npoints )/(N-1) ) )

# define the functions depending on the current number
fstr(n) = sprintf("f%d(x) = A%d*erf(sqrt(pi)*(x-x%d)/(2*W%d))",n, n,n,n)

# The fitting string for a specific file and the related function
fitstr(n) = sprintf("fit f%d(x) fileInter u ( ($2 == g(%d) ) ? ($3 - ncols/2) : 1/0 ):($4*2-1) via A%d,x%d,W%d",n,n,n,n,n)
#[-ncols/4:ncols/4]

printing(n) = sprintf("print W%d,W%d_err",n,n)

param1(n) = sprintf("A%d = -1",n)
param2(n) = sprintf("x%d =  1",n) 
param4(n) = sprintf("W%d =  1",n)

plotting(n) = sprintf("plot fileInter u ( ($2 == g(%d) ) ? ($3 -ncols/2) : 1/0 ):($4*2-1) t 'Rescaled', f%d(x) lw 3.0",n,n)

Justplotting(n) = sprintf("plot fileInter u ( ($2 == g(%d) ) ? ($3 -ncols/2) : 1/0 ):($4*2-1) t 'Rescaled' ",n)

outputfile(n) = sprintf("set output 'Graphs/fileInter%d.png'",n)

set print 'FitResults.log'
print 'Time, Roughnees, Rough_Err'
# Do all the fits
do for [i=1:N] {
	#Initial values
	eval param1(i) 
	eval param2(i)
	eval param4(i)

	eval(fstr(i))
    eval(fitstr(i))
    eval(printing(i))
}
set print

do for[i=1:N]{
	eval(plotting(i))
    #eval(Justplotting(i))
    set term pngcairo enhanced size 900,650
   	eval(outputfile(i))
    replot
}


widthf(n) = sprintf("plot fileInter u ( ( $2 == g(%d) ) ? ($1) : 1/0 ):W%d",n)

#plot fileInter u ( ( $2 == g(1) ) ? ($1) : 1/0 ):W1