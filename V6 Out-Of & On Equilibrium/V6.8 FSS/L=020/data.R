
file <- c('a=1.00T=2.3000.data')


data <- read.csv( file ,stringsAsFactors=F,header=T,sep='')
names(data)<-c("MCS","Magnetization","AbsMagnetization","Energy")
L <- 20
dim <- L*L
temp <- 2.300
beta <- 1/temp

# *********************************************
# Steady from 5000MCS/site
# *********************************************

thres <- 10000

meanMagnet <- mean ( data$Magnetization[data$MCS>thres] ) # <M>
sdMagnet <- sd( data$Magnetization[data$MCS>thres] )

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
