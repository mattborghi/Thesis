library(ggplot2)

file1 <- "Energy20L.dat"

datEnergy <- read.csv(file1,sep=",",stringsAsFactors=FALSE,header=TRUE)

#How many averages?
maxAv = max(datEnergy$Average)
maxTime = max(datEnergy$Tiempo)
#Plot Energy vs Time for both avearges in same plot
#plot (datEnergy$Tiempo,datEnergy$AverEnergy,col=datEnergy$Average)
#or
#qplot(datEnergy$Tiempo, datEnergy$AverEnergy, colour = datEnergy$Average)

#vector of dimension of the number of points
AverEnergy <- vector("numeric",maxTime)
for (i in seq(maxTime)){
	for(j in seq(maxAv)){
		#Sum the elements
		AverEnergy[i] = AverEnergy[i] + datEnergy[datEnergy$Tiempo == i & 
			datEnergy$Average== j,"AverEnergy"]	
	}
	AverEnergy[i] = AverEnergy[i]/maxAv
}

png('AverEnergy.png')
plot (datEnergy$Tiempo,datEnergy$AverEnergy,col=datEnergy$Average)
points(seq(maxTime),AverEnergy,col="white",pch=6,cex=0.1)
dev.off()