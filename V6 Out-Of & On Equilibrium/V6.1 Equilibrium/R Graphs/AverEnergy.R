library(ggplot2)

file1 <- "../Energy20L.dat"
file2 <- "../Magnetization20L.dat"

datEnergy <- read.csv(file1,sep=",",stringsAsFactors=FALSE,header=TRUE)
datMagnet <- read.csv(file2,sep=",",stringsAsFactors=FALSE,header=TRUE)
#How many averages?
maxAv = max(datEnergy$Average)
Temps = unique(datEnergy$Temperature)
#Plot Energy vs Time for both avearges in same plot
#plot (datEnergy$Tiempo,datEnergy$AverEnergy,col=datEnergy$Average)
#or
#qplot(datEnergy$Tiempo, datEnergy$AverEnergy, colour = datEnergy$Average)

#vector of dimension of the number of points
AverEnergy <- vector("numeric",length(Temps))
AverMagnet <- vector("numeric",length(Temps))
AverCv <- vector("numeric",length(Temps))
AverSuscep <- vector("numeric",length(Temps))

for (i in seq(length(Temps))){
	for(j in seq(maxAv)){
		#Sum the elements
		AverEnergy[i] = AverEnergy[i] + datEnergy[datEnergy$Temperature == Temps[i] & 
			datEnergy$Average== j,"Energy"]

		AverMagnet[i] = AverMagnet[i] + datMagnet[datMagnet$Temperature == Temps[i] & 
			datMagnet$Average== j,"Magnetization"]	

		AverCv[i] = AverCv[i] + datEnergy[datEnergy$Temperature == Temps[i] & 
			datEnergy$Average== j,"C_v"]	

		AverSuscep[i] = AverSuscep[i] + datMagnet[datMagnet$Temperature == Temps[i] & 
			datMagnet$Average== j,"Suceptibility"]		
	}
	AverEnergy[i] = AverEnergy[i]/maxAv
	AverMagnet[i] = AverMagnet[i]/maxAv
	AverCv[i] = AverCv[i]/maxAv
	AverSuscep[i] = AverSuscep[i]/maxAv
}

png('AverEnergy.png')
plot (datEnergy$Temperature,datEnergy$Energy,col=datEnergy$Average)
lines(Temps,AverEnergy,col="red",pch=20,cex=1.0,bg="red")
dev.off()

png('AverMagnet.png')
plot (datMagnet$Temperature,datMagnet$Magnetization,col=datMagnet$Average)
lines(Temps,AverMagnet,col="red",pch=20,cex=1.0,bg="red")
dev.off()

png('AverCv.png')
plot (datEnergy$Temperature,datEnergy$C_v,col=datEnergy$Average)
lines(Temps,AverCv,col="red",pch=20,cex=1.0,bg="red")
dev.off()

png('AverSuscep.png')
plot (datMagnet$Temperature,datMagnet$Suceptibility,col=datMagnet$Average)
lines(Temps,AverCv,col="red",pch=20,cex=1.0,bg="red")
dev.off()