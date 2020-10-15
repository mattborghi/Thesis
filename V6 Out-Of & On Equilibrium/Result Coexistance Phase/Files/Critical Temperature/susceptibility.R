file32 <- 'L=032/OutputFile.log'
file64 <- 'L=064/OutputFile.log'
file128 <- 'L=128/OutputFile.log'
file256 <- 'L=256/OutputFile.log'
file512 <- 'L=512/OutputFile.log'

data32	<- read.csv(file32,stringsAsFactors=F,header=T,sep=',')
data64	<- read.csv(file64,stringsAsFactors=F,header=T,sep=',')
data128 <- read.csv(file128,stringsAsFactors=F,header=T,sep=',')
data256 <- read.csv(file256,stringsAsFactors=F,header=T,sep=',')
data512 <- read.csv(file512,stringsAsFactors=F,header=T,sep=',')

temps <- unique(data128$Temperatura)

#Fixed Alfa
alphaVals <- unique(data32$Alfa)

dataL32 <- data32[data32$Alfa==alphaVals[1],]
	dataL32 <- dataL32[with(dataL32,order(Temperatura)),]
dataL64 <- data64[data64$Alfa==alphaVals[1],]
	dataL64 <- dataL64[with(dataL64,order(Temperatura)),]
dataL128 <- data128[data128$Alfa==alphaVals[1],]
	dataL128 <- dataL128[with(dataL128,order(Temperatura)),]
dataL256 <- data256[data256$Alfa==alphaVals[1],]
	dataL256 <- dataL256[with(dataL256,order(Temperatura)),]
dataL512 <- data512[data512$Alfa==alphaVals[1],]
	dataL512 <- dataL512[with(dataL512,order(Temperatura)),]

pdf('Susceptibility.pdf')
#plot(dataL512$Temperatura,dataL512$SusceptAnti*(512^2)/(10^3),type='o',pch=20,lty=5,col='green',xlim=c(0.225,0.23),xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad (L^2/10^3)"),main=c("Susceptibilidad vs Temp alfaud=-1.2"))
plot(dataL256$Temperatura,dataL256$SusceptAnti*(256^2)/(10^3),type='o',pch=19,lty=4,col='blue',xlim=c(0.22,0.25))
points(dataL128$Temperatura,dataL128$SusceptAnti*(128^2)/(10^3),type='o',pch=18,lty=3,col='red')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
points(dataL32$Temperatura,dataL32$SusceptAnti*(32^2)/(10^3),type='o',pch=16,lty=1,col='cyan')
points(dataL64$Temperatura,dataL64$SusceptAnti*(64^2)/(10^3),type='o',pch=17,lty=2,col='magenta')
legend(x=0.24,y=5,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()