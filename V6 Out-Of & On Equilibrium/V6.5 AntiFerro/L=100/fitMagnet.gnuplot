set datafile separator ','
folder='alfa=1,10,1/'
file = 'Magnetization20L.dat'
ouput = 'AntiMagnet.png'
fitfile = 'fit_parameters.txt'
subfolder1='Temp=  0.5/'
subfolder2='Temp=  0.7/'
subfolder3='Temp=  0.9/'
subfolder4='Temp=  1.0/'
subfolder5='Temp=  1.1/'
subfolder6='Temp=  1.3/'
subfolder7='Temp=  1.6/'
subfolder8='Temp=  2.0/'
subfolder9='Temp=  2.3/'
subfolder10='Temp=  2.5/'
subfolder11='Temp=  3.0/'
subfolder12='Temp=  5.0/'
subfolder13='Temp= 10.0/'
subfolder14='Temp= 15.0/'
subfolder15='Temp= 20.0/'
subfolder16='Temp= 25.0/'
subfolder17='Temp= 50.0/'
subfolder18='Temp= 75.0/'
subfolder19='Temp=100.0/'

subfolder = "subfolder1 subfolder2 subfolder3 subfolder4 subfolder5 subfolder6 subfolder7 subfolder8 subfolder9 subfolder10 subfolder11 subfolder12 subfolder13 subfolder14 subfolder15 subfolder16 subfolder17 subfolder18"

fitstr(n) = sprintf("fit [ini:] f(x) folder.subfolder%d.file u 1:6 via b",n)
#print(n) = sprintf("print folder.subfolder%d.file",n)
plot(n) = sprintf("plot folder.subfolder%d.file u 1:6,f(x) lw 3 lc rgb 'black' ",n)
output(n) = sprintf("set output folder.subfolder%d.ouput",n)

set print folder.fitfile
do for [i=1:19]{#words(subfolder)
	f(x)=b
	ini = 9000
	eval(fitstr(i))
	#fit [ini:] f(x) folder.word(subfolder,i).file u 1:6 via b
	eval(plot(i))
	
	#eval(print(i))
	print b,b_err
	
	#Export graph
	set term pngcairo enhanced size 900,650
	eval(output(i))
	replot
	
}

set print
