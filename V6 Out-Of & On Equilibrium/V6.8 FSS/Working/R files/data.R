source("myFunctions.R")

dimensiones <- c(032,064,128,256,512)
		
for (j in seq(length(dimensiones))) {

	L <- dimensiones[j]
	cat(sprintf("L = %d\n",L))
	dim <- L*L

	if (L == 32){
		thres <- 1000
		file <- c('a=1.00T=2.2500.data','a=1.00T=2.2600.data','a=1.00T=2.2700.data','a=1.00T=2.2800.data','a=1.00T=2.2900.data','a=1.00T=2.9200.dat','a=1.00T=2.8200.dat',
				'a=1.00T=2.7200.dat','a=1.00T=2.6200.dat','a=1.00T=2.5200.dat','a=1.00T=2.4200.dat','a=1.00T=2.3200.dat','a=1.00T=2.2200.dat','a=1.00T=2.1200.dat',
				'a=1.00T=2.0200.dat', 'a=1.00T=1.9200.dat','a=1.00T=1.8200.dat','a=1.00T=1.7200.dat','a=1.00T=1.6200.dat','a=1.00T=1.5200.dat','a=1.00T=1.4200.dat',
				'a=1.00T=1.3200.dat', 'a=1.00T=1.2200.dat','a=1.00T=1.1200.dat','a=1.00T=1.0200.dat','a=1.00T=0.9200.dat')

		temps <- c(2.25,2.26,2.27,2.28,2.29,2.92,2.82,2.72,2.62,2.52,2.42,2.32,2.22,2.12,2.02,1.92,1.82,1.72,1.62,1.52,1.42,1.32,1.22,1.12,1.02,0.92)

	} else if (L == 64){
		thres <- 4000
		file <- c('a=1.00T=2.2500.data','a=1.00T=2.2600.data','a=1.00T=2.2700.data','a=1.00T=2.2800.data','a=1.00T=2.2900.data','a=1.00T=2.8200.dat','a=1.00T=2.7200.dat',
				'a=1.00T=2.6200.dat','a=1.00T=2.5200.dat','a=1.00T=2.4200.dat','a=1.00T=2.3200.dat','a=1.00T=2.2200.dat',
				'a=1.00T=2.1200.dat','a=1.00T=2.0200.dat','a=1.00T=1.9200.dat','a=1.00T=1.8200.dat','a=1.00T=1.7200.dat','a=1.00T=1.6200.dat','a=1.00T=1.5200.dat',
				'a=1.00T=1.4200.dat','a=1.00T=1.3200.dat', 'a=1.00T=1.2200.dat','a=1.00T=1.1200.dat','a=1.00T=1.0200.dat','a=1.00T=0.9200.dat')

		temps <- c(2.25,2.26,2.27,2.28,2.29,2.82,2.72,2.62,2.52,2.42,2.32,2.22,2.12,2.02,1.92,1.82,1.72,1.62,1.52,1.42,1.32,1.22,1.12,1.02,0.92)

	} else if (L==128){
		thres <- 20000
		file <- c('a=1.00T=2.2500.data','a=1.00T=2.2600.data','a=1.00T=2.2700.data','a=1.00T=2.2800.data','a=1.00T=2.2900.data','a=1.00T=2.8200.dat','a=1.00T=2.7200.dat',
				'a=1.00T=2.6200.dat','a=1.00T=2.5200.dat','a=1.00T=2.4200.dat','a=1.00T=2.3200.dat','a=1.00T=2.2200.dat',
				'a=1.00T=2.1200.dat','a=1.00T=2.0200.dat','a=1.00T=1.9200.dat','a=1.00T=1.8200.dat','a=1.00T=1.7200.dat','a=1.00T=1.6200.dat','a=1.00T=1.5200.dat',
				'a=1.00T=1.4200.dat','a=1.00T=1.3200.dat', 'a=1.00T=1.2200.dat','a=1.00T=1.1200.dat','a=1.00T=1.0200.dat','a=1.00T=0.9200.dat')

		temps <- c(2.25,2.26,2.27,2.28,2.29,2.82,2.72,2.62,2.52,2.42,2.32,2.22,2.12,2.02,1.92,1.82,1.72,1.62,1.52,1.42,1.32,1.22,1.12,1.02,0.92)

	} else if ( L==256 ){
		thres <- 120000
		file <- c('a=1.00T=2.2500.data','a=1.00T=2.2600.data','a=1.00T=2.2700.data','a=1.00T=2.2800.data','a=1.00T=2.2900.data','a=1.00T=2.8200.dat','a=1.00T=2.7200.dat',
				'a=1.00T=2.6200.dat','a=1.00T=2.5200.dat','a=1.00T=2.4200.dat','a=1.00T=2.3200.dat','a=1.00T=2.2200.dat','a=1.00T=2.1200.dat','a=1.00T=2.0200.dat',
				'a=1.00T=1.9200.dat','a=1.00T=1.8200.dat','a=1.00T=1.7200.dat','a=1.00T=1.6200.dat','a=1.00T=1.5200.dat',
				'a=1.00T=1.4200.dat', 'a=1.00T=1.2200.dat','a=1.00T=1.1200.dat','a=1.00T=1.0200.dat','a=1.00T=0.9200.dat')
				#,'a=1.00T=1.3200.dat'
		temps <- c(2.25,2.26,2.27,2.28,2.29,2.82,2.72,2.62,2.52,2.42,2.32,2.22,2.12,2.02,1.92,1.82,1.72,1.62,1.52,1.42,1.22,1.12,1.02,0.92)#1.32
	} else if (L == 512){
		thres <- 10000
		file <- c('a=1.00T=2.2500.data','a=1.00T=2.2600.data','a=1.00T=2.2700.data','a=1.00T=2.2800.data','a=1.00T=2.2900.data','a=1.00T=2.9200.dat','a=1.00T=2.8200.dat',
		'a=1.00T=2.7200.dat','a=1.00T=2.6200.dat','a=1.00T=2.5200.dat','a=1.00T=2.4200.dat','a=1.00T=2.3200.dat','a=1.00T=2.2200.dat','a=1.00T=2.1200.dat','a=1.00T=2.0200.dat',
		'a=1.00T=1.9200.dat','a=1.00T=1.8200.dat','a=1.00T=1.7200.dat','a=1.00T=1.6200.dat','a=1.00T=1.4200.dat','a=1.00T=1.3200.dat', 'a=1.00T=1.2200.dat','a=1.00T=1.1200.dat',
		'a=1.00T=1.0200.dat')
		#,'a=1.00T=1.5200.dat',
		#,'a=1.00T=0.9200.dat'
		#
		temps <- c(2.25,2.26,2.27,2.28,2.29,2.92,2.82,2.72,2.62,2.52,2.42,2.32,2.22,2.12,2.02,1.92,1.82,1.72,1.62,1.42,1.32,1.22,1.12,1.02)#,1.52,0.92
	}
	longitud <- length(file)

	df <- data.frame( Temperatura = double(), Magnetization = double(), ErrMagnetization = double(), AbsMagnetization = double(), ErrAbsMagnetization = double(), 
		Energy = double(), ErrEnergy = double(), Susceptibility = double(), ErrSusceptibility = double(), SusceptibilityAbs = double(), ErrSusceptibilityAbs = double(),
		HeatCapacity = double(), ErrHeatCapacity = double(), Cumulant = double(), ErrCumulant = double(),
		AntiMagnetization = double(), ErrAntiMagnetization = double(), AntiSusceptibility = double(), ErrAntiSusceptibility = double(), 
		AntiCumulant = double(), ErrAntiCumulant = double(), stringsAsFactors = F)

	for (i in seq( longitud ) ) {
		
		root <- sprintf("../L=%03d/%s",L,file[i] )
		#print(root)
		cat ( sprintf('At T = %6.3f\n',temps[i]) )
		data <- read.csv( root ,stringsAsFactors=F,header=F,sep='')
		names(data)<-c("MCS","Magnetization","AbsMagnetization","AntiMagnetization","Energy")
		beta <- 1/temps[i]
		
		# *********************************************
		# Steady from 5000MCS/site
		# *********************************************

		meanMagnet <- mean ( data$Magnetization[data$MCS>thres] ) # <M>
		sdMagnet <- sd( data$Magnetization[data$MCS>thres] )

		meanMagnet2 <- mean ( data$Magnetization[data$MCS>thres]^2 ) # <M^2>

		meanMagnet4 <- mean ( data$Magnetization[data$MCS>thres]^4 ) # <M^4>

		meanAbsMagnet <- mean ( data$AbsMagnetization[data$MCS>thres] ) # <|M|>
		sdAbsMagnet <- sd( data$AbsMagnetization[data$MCS>thres] )

		meanEnergy <- mean( data$Energy[data$MCS>thres] ) # <E>
		sdEnergy <- sd( data$Energy[data$MCS>thres] )

		meanEnergy2 <- mean( data$Energy[data$MCS>thres]^2 ) # <E^2>

		meanAntiMagnet <- mean( data$AntiMagnetization[data$MCS>thres] ) #<M>ANTI
		sdAntiMagnet <- sd(data$AntiMagnetization[data$MCS>thres])

		meanAntiMagnet2 <- mean ( data$AntiMagnetization[data$MCS>thres]^2 ) # <M^2ANTI>

		meanAntiMagnet4 <- mean ( data$AntiMagnetization[data$MCS>thres]^4 ) # <M^4>ANTI


		# *********************************************
		# Per spin (PS) quantities
		# *********************************************

		meanMagnetPS <- meanMagnet/dim # <m>
		sdMagnetPS <- sd( data$Magnetization[data$MCS>thres]/dim )

		meanMagnet2PS <- meanMagnet2/dim # <m^2>

		meanMagnet4PS <- meanMagnet4/dim # <m^4>

		meanAbsMagnetPS <- meanAbsMagnet/dim # <|m|>
		sdAbsMagnetPS <- sd( data$AbsMagnetization[data$MCS>thres]/dim )

		meanEnergyPS <- meanEnergy/dim # <e>
		sdEnergyPS <- sd( data$Energy[data$MCS>thres]/dim )

		meanEnergy2PS <- meanEnergy2/dim # <e^2>

		# *********************************************
		# Derived Quantities 
		# *********************************************

		Cv <- (beta^2)*( meanEnergy2 - meanEnergy^2 )

		CvPS <- Cv/dim
		sdCvPS <- calculateErrorCapCalor( data$Energy[data$MCS>thres] ,data$Energy[data$MCS>thres]^2, temps[i],dim)

		Suscept <- beta* ( meanMagnet2 - meanMagnet^2 )

		SusceptPS <- Suscept/dim
		sdSusceptPS <- calculateErrorSuscept( data$Magnetization[data$MCS>thres],data$Magnetization[data$MCS>thres]^2, temps[i],dim)

		SusceptAbs <- beta* ( meanMagnet2 - meanAbsMagnet^2 )

		SusceptAbsPS <- SusceptAbs/dim
		sdSusceptAbsPS <- 0

		Cumulant <- 1 - ( meanMagnet4 /( 3*meanMagnet2^2 ) )
		sdCumulant <- calculateErrorCumulant(data$Magnetization[data$MCS>thres]^2,data$Magnetization[data$MCS>thres]^4)

		AntiSuscept <- beta*( meanAntiMagnet2 - meanAntiMagnet^2 )
		sdAntiSuscept <- 0

		AntiCumulant <- 1 - ( meanAntiMagnet4 /( 3*meanAntiMagnet2^2 ) )
		sdAntiCumulant <- 0

		#fill df
		df[i,] <- c( temps[i] , meanMagnetPS, sdMagnetPS, meanAbsMagnetPS, sdAbsMagnetPS, meanEnergyPS, sdEnergyPS, 
			SusceptPS, sdSusceptPS, SusceptAbsPS, sdSusceptAbsPS, CvPS, sdCvPS, Cumulant, sdCumulant,
			meanAntiMagnet, sdAntiMagnet, AntiSuscept, sdAntiSuscept, AntiCumulant, sdAntiCumulant )

	}

	root <- sprintf("../L=%03d/Equilibrium.dat",L )
	write.table (df, file=root ,sep = '\t',row.names = F)

}

