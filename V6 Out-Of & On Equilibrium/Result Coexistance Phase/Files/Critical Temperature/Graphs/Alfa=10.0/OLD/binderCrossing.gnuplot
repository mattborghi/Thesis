file = "binderCrossing.dat"

f(x) = a*x + b

fit f(x) file u 1:2 via a,b

plot file u 1:2 w lp, f(x) w l lw 3.0
