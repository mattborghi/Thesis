back = '../'
source = 'Equilibrium.dat'
graphs = 'Graphs/'
subgraphs = 'Collapse/'
set term pdfcairo

do for [t = 1:4]{
	if (t==1){
		La = 32
		Lb = 64
		folder1 = 'L=032/'
		folder2 = 'L=064/'
		
		print('L=032 y L=064')
	}
	if (t==2){
		La = 64
		Lb = 128
		folder1 = 'L=064/'
		folder2 = 'L=128/'
		
		print('L=064 y L=128')
	}
	if (t==3){
		La = 128
		Lb = 256
		folder1 = 'L=128/'
		folder2 = 'L=256/'
		
		print('L=128 y L=256')
	}
	if (t==4){
		La = 256
		Lb = 512
		folder1 = 'L=256/'
		folder2 = 'L=512/'
		
		print('L=256 y L=512')
	}

	subsubgraphs = 'L='.La.Lb.'/'
	#Magnetization Collapse
	Tc = 2.269
	exp = 1/8
	set output back.graphs.subgraphs.subsubgraphs.'MagnetizationCollapse.pdf'
	plot back.folder1.source u (($1-Tc)/Tc*La):($4*La**(exp)):($5*La**(exp)) with yerrorbars lc rgb 'red' t 'Err L='.La, \
	     back.folder1.source u (($1-Tc)/Tc*La):($4*La**(exp)) w p pt 7 ps 1 lc rgb 'red' t folder1, \
	     back.folder2.source u (($1-Tc)/Tc*Lb):($4*Lb**(exp)):($5*Lb**(exp)) with yerrorbars lc rgb 'blue' t 'Err L='.Lb, \
	     back.folder2.source u (($1-Tc)/Tc*Lb):($4*Lb**(exp)) w p pt 7 ps 1 lc rgb 'blue' t folder2
	set print 

	#Energy Collapse
	#set output back.graphs.folder.'Energy.pdf'
	#plot back.folder.source u 1:6:7 with yerrorbars, back.folder.source u 1:6 w p pt 7 ps 1
	#set print 

	#Suscept Collapse
	exp = -7/4
	set output back.graphs.subgraphs.subsubgraphs.'SusceptibilityCollapse.pdf'
	plot back.folder1.source u (($1-Tc)/Tc*La):($8*La**(exp)):($9*La**(exp)) with yerrorbars lc rgb 'red' t 'Err L='.La, \
		 back.folder1.source u (($1-Tc)/Tc*La):($8*La**(exp)) w p pt 7 ps 1 lc rgb 'red' t folder1, \
		 back.folder2.source u (($1-Tc)/Tc*Lb):($8*Lb**(exp)):($9*Lb**(exp)) with yerrorbars lc rgb 'blue' t 'Err L='.Lb, \
		 back.folder2.source u (($1-Tc)/Tc*Lb):($8*Lb**(exp)) w p pt 7 ps 1 lc rgb 'blue' t folder2
	set print 

	#SusceptAbs Collapse
	set output back.graphs.subgraphs.subsubgraphs.'AbsSusceptibilityCollapse.pdf'
	plot back.folder1.source u (($1-Tc)/Tc*La):($10*La**(exp)):($11*La**(exp)) with yerrorbars lc rgb 'red' t 'Err L='.La, \
		 back.folder1.source u (($1-Tc)/Tc*La):($10*La**(exp)) w p pt 7 ps 1 lc rgb 'red' t folder1, \
		 back.folder2.source u (($1-Tc)/Tc*Lb):($10*Lb**(exp)):($11*Lb**(exp)) with yerrorbars lc rgb 'blue' t 'Err L='.Lb, \
		 back.folder2.source u (($1-Tc)/Tc*Lb):($10*Lb**(exp)) w p pt 7 ps 1 lc rgb 'blue' t folder2
	set print

	#Cv Collapse
	exp = 0
	set output back.graphs.subgraphs.subsubgraphs.'HeatCapacityCollapse.pdf'
	plot back.folder1.source u (($1-Tc)/Tc*La):($12*La**(exp)):($13*La**(exp)) with yerrorbars lc rgb 'red' t 'Err L='.La, \
		 back.folder1.source u (($1-Tc)/Tc*La):($12*La**(exp)) w p pt 7 ps 1 lc rgb 'red' t folder1, \
		 back.folder2.source u (($1-Tc)/Tc*Lb):($12*Lb**(exp)):($13*Lb**(exp)) with yerrorbars lc rgb 'blue' t 'Err L='.Lb, \
		 back.folder2.source u (($1-Tc)/Tc*Lb):($12*Lb**(exp)) w p pt 7 ps 1 lc rgb 'blue' t folder2
	set print

	#Cumulant Collapse
	set output back.graphs.subgraphs.subsubgraphs.'CumulantCollapse.pdf'
	plot back.folder1.source u (($1-Tc)/Tc*La):($14):($15) with yerrorbars lc rgb 'red' t 'Err L='.La, \
		 back.folder1.source u (($1-Tc)/Tc*La):($14) w p pt 7 ps 1 lc rgb 'red' t folder1, \
		 back.folder2.source u (($1-Tc)/Tc*Lb):($14):($15) with yerrorbars lc rgb 'blue' t 'Err L='.Lb, \
		 back.folder2.source u (($1-Tc)/Tc*Lb):($14) w p pt 7 ps 1 lc rgb 'blue' t folder2
	set print

	set output

}