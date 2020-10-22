library(ggplot2)

file1 <- "Inter220L.dat"

datInter <- read.csv(file1,sep=",",stringsAsFactors=FALSE,header=TRUE)

#How many averages?
maxAv = max(datInter$Average)
maxTime = max(datInter$Tiempo)
#Plot Energy vs Time for both avearges in same plot
#plot (datEnergy$Tiempo,datEnergy$AverEnergy,col=datEnergy$Average)
#or
#qplot(datEnergy$Tiempo, datEnergy$AverEnergy, colour = datEnergy$Average)

#vector of dimension of the number of points
AverNumPos <- vector("numeric",maxTime)
for (i in seq(maxTime)){
	for(j in seq(maxAv)){
		#Sum the elements
		AverNumPos[i] = AverNumPos[i] + datInter[datInter$Tiempo == i & 
			datInter$Average== j,"InterPos2"]	
	}
	AverNumPos[i] = AverNumPos[i]/maxAv
}

png('AverInterPos.png')
plot (datInter$Tiempo,datInter$InterPos2,col=datInter$Average)
points(seq(maxTime),AverNumPos,col="white",pch=6,cex=0.1)
dev.off()