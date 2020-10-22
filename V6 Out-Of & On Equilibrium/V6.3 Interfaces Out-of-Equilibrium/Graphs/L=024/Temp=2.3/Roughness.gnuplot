set datafile separator ','
file = 'Roughness.dat'
f(x) = a*x+b
ini = 5
end = 6
fit [ini:end] f(x) file u (log10($1)):(log10($5)) via a,b 
plot file u (log10($1)):(log10($5)), f(x)
set term pngcairo enhanced size 900,650
set output 'Graphs/RoughnessEq.png'
replot