
file <- c('a=1.00T=2.9200.dat','a=1.00T=2.8200.dat','a=1.00T=2.7200.dat','a=1.00T=2.6200.dat','a=1.00T=2.5200.dat','a=1.00T=2.4200.dat','a=1.00T=2.3200.dat',
			'a=1.00T=2.2200.dat','a=1.00T=2.1200.dat','a=1.00T=2.0200.dat','a=1.00T=1.9200.dat','a=1.00T=1.8200.dat','a=1.00T=1.7200.dat','a=1.00T=1.6200.dat',
			'a=1.00T=1.5200.dat','a=1.00T=1.4200.dat','a=1.00T=1.3200.dat','a=1.00T=1.2200.dat','a=1.00T=1.1200.dat','a=1.00T=1.0200.dat','a=1.00T=0.9200.dat')

L <- 20
dim <- L*L
high_temp <- 2.92
temp_interval <- 0.1


 

for (i in seq(length(file))) {

	temp <- high_temp - temp_interval*(i-1)
	beta <- 1/temp
	data <- read.csv( file[i] ,stringsAsFactors=F,header=T,sep='')
	



}


# *********************************************
# Steady from 5000MCS/site
# *********************************************

thres <- 10000

meanMagnet <- mean ( data$Magnetization[data$MCS>thres] ) # <M>

meanMagnet2 <- mean ( data$Magnetization[data$MCS>thres]^2 ) # <M^2>

meanMagnet4 <- mean ( data$Magnetization[data$MCS>thres]^4 ) # <M^4>

meanAbsMagnet <- mean ( data$AbsMagnetization[data$MCS>thres] ) # <|M|>

meanEnergy <- mean( data$Energy[data$MCS>thres] ) # <E>

meanEnergy2 <- mean( data$Energy[data$MCS>thres]^2 ) # <E^2>

# *********************************************
# Per spin (PS) quantities
# *********************************************

meanMagnetPS <- meanMagnet/dim # <m>

meanMagnet2PS <- meanMagnet2/dim # <m^2>

meanMagnet4PS <- meanMagnet4/dim # <m^4>

meanAbsMagnetPS <- meanAbsMagnet/dim # <|m|>

meanEnergyPS <- meanEnergy/dim # <e>

meanEnergy2PS <- meanEnergy2/dim # <e^2>

# *********************************************
# Derived Quantities 
# *********************************************

Cv <- (beta^2)*( meanEnergy2 - meanEnergy^2 )

CvPS <- Cv/dim

Suscept <- beta* ( meanMagnet2 - meanMagnet^2 )

SusceptPS <- Suscept/dim

SusceptAbs <- beta* ( meanMagnet2 - meanAbsMagnet^2 )

SusceptAbsPS <- SusceptAbs/dim

Cumulant <- 1 - ( meanMagnet4 /( 3*meanMagnet2^2 ) )
