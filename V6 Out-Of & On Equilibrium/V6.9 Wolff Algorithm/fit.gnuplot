set datafile separator ','

f(x) = a*x**3 + b*x**2 + c*x + d
g(x) = e*x**3 + f*x**2 + g*x + h 

fit [2.245:2.281] f(x) 'OutputFile.csv' u 3:($1 == 512? ($13):1/0) via a,b,c,d  
fit [2.245:2.281] g(x) 'OutputFile.csv' u 3:($1 == 256? ($13):1/0) via e,f,g,h

plot [2.245:2.295] 'OutputFile.csv' u 3:($1 == 512? ($13):1/0) w p pt 7 ps 2 t 'L=512',f(x) w l lw 2 lc rgb 'blue', 'OutputFile.csv' u 3:($1==256?($13):1/0) w p pt 7 ps 2 t 'L=256', g(x) w l lw 2 lc rgb 'green'