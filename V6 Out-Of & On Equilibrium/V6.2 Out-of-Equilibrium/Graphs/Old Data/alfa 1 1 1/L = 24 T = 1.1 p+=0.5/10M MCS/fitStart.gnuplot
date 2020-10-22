#Fitting the whole set of data and plotting within the plot
set datafile separator ','

file = 'Inter20L.dat'
folder = 'Graphs/'
Interf1 = 'Interf1Fit.png'
Interf2 = 'Interf2Fit.png'
Interf3 = 'Interf3Fit.png'
Interf4 = 'Interf4Fit.png'
InterfFit = 'InterfFit.png'
#f(x) = a*log(x)+b

inifit = exp(5)


set terminal wxt enhanced font "Helvetica,15" size 900,650
#Grid
set grid
#Set axis
set auto xy
#Key
set key on bottom right box
#Label
set xlabel "Time steps"
set ylabel "Interactions"
#Title
set title "f1 interactions"

#f1up
f(x) = a
fit [inifit:] f(x) file u 1:2 via a
#f1down
g(x) = b
fit [inifit:] g(x) file u 1:3 via b
plot file u (log($1)):3 t 'f1dn' w lp ls 7 pt 6 lc rgb "blue", g(x) lw 3.0, file u (log($1)):2 t 'f1up' w lp ls 7 pt 6 lc rgb "red", f(x) lw 3.0
#Export graph
set term pngcairo enhanced size 900,650
set output folder.Interf1
replot

#Title
set title "f2 interactions"
#f2up
f(x) = d
fit [inifit:] f(x) file u 1:4 via d
#f2down
g(x) = e
fit [inifit:] g(x) file u 1:5 via e
plot file u (log($1)):4 t 'f2up' w lp ls 7 pt 6 lc rgb "red", f(x) lw 3.0,file u (log($1)):5 t 'f2dn' w lp ls 7 pt 6 lc rgb "blue", g(x) lw 3.0
#Export graph
set term pngcairo enhanced size 900,650
set output folder.Interf2
replot


#Title
set title "f3 interactions"
#f3up
f(x) = g
fit [inifit:] f(x) file u 1:6 via g
#f2down
g(x) = h
fit [inifit:] g(x) file u 1:7 via h
plot file u (log($1)):6 t 'f3up' w lp ls 7 pt 6 lc rgb "red", f(x) lw 3.0,file u (log($1)):7 t 'f3dn' w lp ls 7 pt 6 lc rgb "blue", g(x) lw 3.0
#Export graph
set term pngcairo enhanced size 900,650
set output folder.Interf3
replot


#Title
set title "f4 interactions"
#f4up
f(x) = j
fit [inifit:] f(x) file u 1:8 via j
#f4down
g(x) = k
fit [inifit:] g(x) file u 1:9 via k
plot file u (log($1)):8 t 'f4up' w lp ls 7 pt 6 lc rgb "red", f(x) lw 3.0,file u (log($1)):9 t 'f4dn' w lp ls 7 pt 6 lc rgb "blue", g(x) lw 3.0
#Export graph
set term pngcairo enhanced size 900,650
set output folder.Interf4
replot


#Title
set title "f-interactions"
#f1
h(x) = c
fit [inifit:] h(x) file u 1:($2+$3) via c
#f2
i(x) = f
fit [inifit:] i(x) file u 1:($4+$5) via f
#f3
j(x) = i
fit [inifit:] j(x) file u 1:($6+$7) via i
#f4
k(x) = l
fit [inifit:] k(x) file u 1:($8+$9) via l

plot file u (log($1)):($2+$3) t 'f1' w lp ls 7 pt 6 lc rgb "black", h(x) lw 3.0 lc rgb "red", \
	 file u (log($1)):($4+$5) t 'f2' w lp ls 7 pt 6 lc rgb "blue", i(x) lw 3.0 lc rgb "red", \
	 file u (log($1)):($6+$7) t 'f3' w lp ls 7 pt 6 lc rgb "green", j(x) lw 3.0 lc rgb "red", \
	 file u (log($1)):($8+$9) t 'f4' w lp ls 7 pt 6 lc rgb "cyan", k(x) lw 3.0 lc rgb "red"
#Export graph
set term pngcairo enhanced size 900,650
set output folder.InterfFit
replot

set print 'FitResults.log'
print 'f1up = ',a,a_err
print 'f1dn = ',b,b_err
print 'f1   = ',c,c_err
print 'f2up = ',d,d_err
print 'f2dn = ',e,e_err
print 'f2   = ',f,f_err
print 'f3up = ',g,g_err
print 'f3dn = ',h,h_err
print 'f3   = ',i,i_err
print 'f4up = ',j,j_err
print 'f4dn = ',k,k_err
print 'f4   = ',l,l_err
set print