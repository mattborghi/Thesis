back = '../'

do for [t = 1:5]{
	if (t==1){
		folder = 'L=032/'
		print('L=032')
	}
	if (t==2){
		folder = 'L=064/'	
		print('L=064')
	}
	if (t==3){
		folder = 'L=128/'
		print('L=128')
	}
	if (t==4){
		folder = 'L=256/'	
		print('L=256')
	}
	if (t==5){
		folder = 'L=512/'
		print('L=512')
	}
	

	source = 'Equilibrium.dat'
	graphs = 'Graphs/'
	set term pdfcairo


	#Magnetization
	set output back.graphs.folder.'Magnetization.pdf'
	plot back.folder.source u 1:4:5 with yerrorbars, back.folder.source u 1:4 w p pt 7 ps 1
	set print 

	#Energy
	set output back.graphs.folder.'Energy.pdf'
	plot back.folder.source u 1:6:7 with yerrorbars, back.folder.source u 1:6 w p pt 7 ps 1
	set print 

	#Suscept
	set output back.graphs.folder.'Susceptibility.pdf'
	plot back.folder.source u 1:8:9 with yerrorbars, back.folder.source u 1:8 w p pt 7 ps 1
	set print 

	#SusceptAbs
	set output back.graphs.folder.'AbsSusceptibility.pdf'
	plot back.folder.source u 1:($10):($11) with yerrorbars, back.folder.source u 1:($10) w p pt 7 ps 1
	set print

	#Cv
	set output back.graphs.folder.'HeatCapacity.pdf'
	plot back.folder.source u 1:($12):($13) with yerrorbars, back.folder.source u 1:($12) w p pt 7 ps 1
	set print

	#Cumulant
	set output back.graphs.folder.'Cumulant.pdf'
	plot back.folder.source u 1:($14):($15) with yerrorbars, back.folder.source u 1:($14) w p pt 7 ps 1
	set print

set output



}

