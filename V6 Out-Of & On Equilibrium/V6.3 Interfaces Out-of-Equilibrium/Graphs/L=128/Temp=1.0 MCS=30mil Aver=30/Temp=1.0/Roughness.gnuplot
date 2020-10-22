set datafile separator ','
file = 'Roughness.dat'
f(x) = a*x+b
ini = 11
end = 15
fit [ini:end] f(x) file u (log($1)):(log($5)) via a,b 
plot file u (log($1)):(log($5)), f(x)
set term pngcairo enhanced size 900,650
set output 'Graphs/RoughnessEq.png'
replot