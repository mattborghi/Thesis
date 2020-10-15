file <- 'OutputFile.log'

data <- read.csv(file,stringsAsFactors=F,header=T,sep=',')
#Density 0.5
#datab <- data[data$posDsty==0.5,]
temps <- unique(data$Temperatura)

dataT06 <- data[data$Temperatura==temps[1],]
	dataT06 <- dataT06[with(dataT06,order(Alfa)),]
dataT1 <- data[data$Temperatura==1.0,]
	dataT1 <- dataT1[with(dataT1,order(Alfa)),]
dataT13 <- data[data$Temperatura==temps[8],]
	dataT13 <- dataT13[with(dataT13,order(Alfa)),]
dataT16 <- data[data$Temperatura==temps[11],]
	dataT16 <- dataT16[with(dataT16,order(Alfa)),]
dataT19 <- data[data$Temperatura==temps[14],]
	dataT19 <- dataT19[with(dataT19,order(Alfa)),]
dataT2 <- data[data$Temperatura==2.0,]
	dataT2 <- dataT2[with(dataT2,order(Alfa)),]
dataT3 <- data[data$Temperatura==3.0,]
	dataT3 <- dataT3[with(dataT3,order(Alfa)),]
dataT4 <- data[data$Temperatura==4.0,]
	dataT4 <- dataT4[with(dataT4,order(Alfa)),]
dataT5 <- data[data$Temperatura==5.0,]
	dataT5 <- dataT5[with(dataT5,order(Alfa)),]

#Magnetization Anti

png('magnetAFvsAlfa.png')
plot(dataT06$Alfa,dataT06$magnetANTI,pch=15,type='o',col='blue',ylim=c(0,1),xlab='Alpha ud',ylab='Magnetizacion AntiFerro')
lines(dataT1$Alfa,dataT1$magnetANTI,pch=16,type='o',col='cyan')
#lines(dataT13$Alfa,dataT13$magnetANTI,type='o',col='magenta')
#lines(dataT16$Alfa,dataT16$magnetANTI,type='o',col='violet')
#lines(dataT19$Alfa,dataT19$magnetANTI,type='o',col='brown')
lines(dataT2$Alfa,dataT2$magnetANTI,pch=17,type='o',col='green')
lines(dataT3$Alfa,dataT3$magnetANTI,pch=18,type='o',col='orange')
lines(dataT4$Alfa,dataT4$magnetANTI,pch=8,type='o',col='red')
lines(dataT5$Alfa,dataT5$magnetANTI,pch=12,type='o',col='black')
#legend(x=-1.3,y=1.0,legend=c('T=0.6','T=1.0','T=1.3','T=1.6','T=1.9','T=2.0','T=3.0','T=4.0','T=5.0'),col=c('blue','cyan','cyan','magenta','violet','brown','orange','red','black'))
legend(x=-1.3,y=1.0,legend=c('T=0.6','T=1.0','T=2.0','T=3.0','T=4.0','T=5.0'),col=c('blue','cyan','green','orange','red','black'),pch=c(15,16,17,18,8,12))
title(main='MagnetAF vs Alfa para L=040')
dev.off()

#Susceptibility Anti

png('susceptAFvsAlfa.png')
plot(dataT06$Alfa,dataT06$SusceptAnti,pch=15,type='o',col='blue',ylim=c(0,0.06),xlab='Alpha ud',ylab='Susceptibilidad AntiFerro')
lines(dataT1$Alfa,dataT1$SusceptAnti,pch=16,type='o',col='cyan')
lines(dataT2$Alfa,dataT2$SusceptAnti,pch=17,type='o',col='green')
lines(dataT3$Alfa,dataT3$SusceptAnti,pch=18,type='o',col='orange')
lines(dataT4$Alfa,dataT4$SusceptAnti,pch=8,type='o',col='red')
lines(dataT5$Alfa,dataT5$SusceptAnti,pch=12,type='o',col='black')
legend(x=-3,y=0.06,legend=c('T=0.6','T=1.0','T=2.0','T=3.0','T=4.0','T=5.0'),col=c('blue','cyan','green','orange','red','black'),pch=c(15,16,17,18,8,12))
title(main='susceptAntiF vs Alfa para L=040')
dev.off()

#Cumulant Anti

png('cumulantAFvsAlfa.png')
plot(dataT06$Alfa,dataT06$cumulantAnti,pch=15,type='o',col='blue',ylim=c(0,0.7),xlab='Alpha ud',ylab='Cumulante AntiFerro')
lines(dataT1$Alfa,dataT1$cumulantAnti,pch=16,type='o',col='cyan')
lines(dataT2$Alfa,dataT2$cumulantAnti,pch=17,type='o',col='green')
lines(dataT3$Alfa,dataT3$cumulantAnti,pch=18,type='o',col='orange')
lines(dataT4$Alfa,dataT4$cumulantAnti,pch=8,type='o',col='red')
lines(dataT5$Alfa,dataT5$cumulantAnti,pch=12,type='o',col='black')
legend(x=-1.4,y=0.6,legend=c('T=0.6','T=1.0','T=2.0','T=3.0','T=4.0','T=5.0'),col=c('blue','cyan','green','orange','red','black'),pch=c(15,16,17,18,8,12))
title(main='CumulantAntiF vs Alfa para L=040')
dev.off()

#Magnetization Ferro

png('magnetFvsAlfa.png')
plot(dataT06$Alfa,dataT06$magnetFerro,pch=15,type='o',col='blue',ylim=c(0,1),xlab='Alpha ud',ylab='Magnetizacion Ferro')
lines(dataT1$Alfa,dataT1$magnetFerro,pch=16,type='o',col='cyan')
lines(dataT2$Alfa,dataT2$magnetFerro,pch=17,type='o',col='green')
lines(dataT3$Alfa,dataT3$magnetFerro,pch=18,type='o',col='orange')
lines(dataT4$Alfa,dataT4$magnetFerro,pch=8,type='o',col='red')
lines(dataT5$Alfa,dataT5$magnetFerro,pch=12,type='o',col='black')
legend(x=-3,y=1,legend=c('T=0.6','T=1.0','T=2.0','T=3.0','T=4.0','T=5.0'),col=c('blue','cyan','green','orange','red','black'),pch=c(15,16,17,18,8,12))
title(main='MagnetF vs Alfa para L=040')
dev.off()

#Susceptibility Ferro

png('susceptFvsAlfa.png')
plot(dataT06$Alfa,dataT06$SusceptFerro,pch=15,type='o',col='blue',ylim=c(0,0.05),xlab='Alpha ud',ylab='Susceptibilidad Ferro')
lines(dataT1$Alfa,dataT1$SusceptFerro,pch=16,type='o',col='cyan')
lines(dataT2$Alfa,dataT2$SusceptFerro,pch=17,type='o',col='green')
lines(dataT3$Alfa,dataT3$SusceptFerro,pch=18,type='o',col='orange')
lines(dataT4$Alfa,dataT4$SusceptFerro,pch=8,type='o',col='red')
lines(dataT5$Alfa,dataT5$SusceptFerro,pch=12,type='o',col='black')
legend(x=-3,y=0.05,legend=c('T=0.6','T=1.0','T=2.0','T=3.0','T=4.0','T=5.0'),col=c('blue','cyan','green','orange','red','black'),pch=c(15,16,17,18,8,12))
title(main='susceptF vs Alfa para L=040')
dev.off()

#Cumulant Ferro

png('cumulantFvsAlfa.png')
plot(dataT06$Alfa,dataT06$cumulantFerro,pch=15,type='o',col='blue',ylim=c(0,1.0),xlab='Alpha ud',ylab='Cumulante Ferro')
lines(dataT1$Alfa,dataT1$cumulantFerro,pch=16,type='o',col='cyan')
lines(dataT2$Alfa,dataT2$cumulantFerro,pch=17,type='o',col='green')
lines(dataT3$Alfa,dataT3$cumulantFerro,pch=18,type='o',col='orange')
lines(dataT4$Alfa,dataT4$cumulantFerro,pch=8,type='o',col='red')
lines(dataT5$Alfa,dataT5$cumulantFerro,pch=12,type='o',col='black')
legend(x=-3.0,y=0.4,legend=c('T=0.6','T=1.0','T=2.0','T=3.0','T=4.0','T=5.0'),col=c('blue','cyan','green','orange','red','black'),pch=c(15,16,17,18,8,12))
title(main='CumulantF vs Alfa para L=040')
dev.off()

#-------------------------------

#Fixed Alfa
alphaVals <- unique(data$Alfa)

dataA19 <- data[data$Alfa==alphaVals[12],]
	dataA19 <- dataA19[with(dataA19,order(Temperatura)),]
dataA3 <- data[data$Alfa==alphaVals[1],]
	dataA3 <- dataA3[with(dataA3,order(Temperatura)),]
dataA1 <- data[data$Alfa==alphaVals[21],]
	dataA1 <- dataA1[with(dataA1,order(Temperatura)),]

dataA0 <- data[data$Alfa==alphaVals[31],]
	dataA0 <- dataA0[with(dataA0,order(Temperatura)),]
dataA1p <- data[data$Alfa==alphaVals[61],]
	dataA1p <- dataA1p[with(dataA1p,order(Temperatura)),]
dataA2p <- data[data$Alfa==alphaVals[51],]
	dataA2p <- dataA2p[with(dataA2p,order(Temperatura)),]
dataA3p <- data[data$Alfa==alphaVals[41],]
	dataA3p <- dataA3p[with(dataA3p,order(Temperatura)),]


#Magnetizacion AntiFerro

png('magnetAFvsTemp.png')
plot(dataA3$Temperatura,dataA3$magnetANTI,type='o',pch=15,col='green',ylim=c(0,1),xlab='Temperatura',ylab='Magnetizacion AntiFerro')
lines(dataA19$Temperatura,dataA19$magnetANTI,type='o',pch=16,col='red')
lines(dataA1$Temperatura,dataA1$magnetANTI,type='o',pch=17,col='blue')
lines(dataA0$Temperatura,dataA0$magnetANTI,type='o',pch=18,col='orange')
lines(dataA1p$Temperatura,dataA1p$magnetANTI,type='o',pch=8,col='black')
lines(dataA2p$Temperatura,dataA2p$magnetANTI,type='o',pch=12,col='cyan')
lines(dataA3p$Temperatura,dataA3p$magnetANTI,type='o',pch=9,col='brown')
legend(x=3.5,y=1.0,legend=c('a=-3.0','a=-1.9','a=-1.0','a=+0.0','a=+1.0','a=+2.0','a=+3.0'),col=c('green','red','blue','orange','black','cyan','brown'),pch=c(15,16,17,18,8,12,9))
title(main='MagnetAF vs Temp para L=040')
dev.off()

#Susceptibilidad AntiFerro

png('susceptAFvsTemp.png')
plot(dataA3$Temperatura,dataA3$SusceptAnti,pch=15,type='o',col='green',ylim=c(0,0.02),xlab='Temperatura',ylab='Susceptiblidad AntiFerro')
lines(dataA19$Temperatura,dataA19$SusceptAnti,pch=16,type='o',col='red')
lines(dataA1$Temperatura,dataA1$SusceptAnti,pch=17,type='o',col='blue')
lines(dataA0$Temperatura,dataA0$SusceptAnti,pch=18,type='o',col='orange')
lines(dataA1p$Temperatura,dataA1p$SusceptAnti,pch=8,type='o',col='black')
lines(dataA2p$Temperatura,dataA2p$SusceptAnti,type='o',pch=12,col='cyan')
lines(dataA3p$Temperatura,dataA3p$SusceptAnti,type='o',pch=9,col='brown')
legend(x=4.0,y=0.02,legend=c('a=-3.0','a=-1.9','a=-1.0','a=+0.0','a=+1.0','a=+2.0','a=+3.0'),col=c('green','red','blue','orange','black','cyan','brown'),pch=c(15,16,17,18,8,12,9))
title(main='SusceptAF vs Temp para L=040')
dev.off()

#Cumulante AntiFerro

png('cumulantAFvsTemp.png')
plot(dataA3$Temperatura,dataA3$cumulantAnti,pch=15,type='o',col='green',ylim=c(0,1.0),xlab='Temperatura',ylab='Cumulante AntiFerro')
lines(dataA19$Temperatura,dataA19$cumulantAnti,pch=16,type='o',col='red')
lines(dataA1$Temperatura,dataA1$cumulantAnti,pch=17,type='o',col='blue')
lines(dataA0$Temperatura,dataA0$cumulantAnti,pch=18,type='o',col='orange')
lines(dataA1p$Temperatura,dataA1p$cumulantAnti,pch=8,type='o',col='black')
lines(dataA2p$Temperatura,dataA2p$cumulantAnti,type='o',pch=12,col='cyan')
lines(dataA3p$Temperatura,dataA3p$cumulantAnti,type='o',pch=9,col='brown')
legend(x=4.0,y=1.0,legend=c('a=-3.0','a=-1.9','a=-1.0','a=+0.0','a=+1.0','a=+2.0','a=+3.0'),col=c('green','red','blue','orange','black','cyan','brown'),pch=c(15,16,17,18,8,12,9))
title(main='CumulantAF vs Temp para L=040')
dev.off()

#Magnetizacion Ferro

png('magnetFvsTemp.png')
plot(dataA3$Temperatura,dataA3$magnetFerro,pch=15,type='o',col='green',ylim=c(0,1),xlab='Temperatura',ylab='Magnetizacion Ferro')
lines(dataA19$Temperatura,dataA19$magnetFerro,pch=16,type='o',col='red')
lines(dataA1$Temperatura,dataA1$magnetFerro,pch=17,type='o',col='blue')
lines(dataA0$Temperatura,dataA0$magnetFerro,pch=18,type='o',col='orange')
lines(dataA1p$Temperatura,dataA1p$magnetFerro,pch=8,type='o',col='black')
lines(dataA2p$Temperatura,dataA2p$magnetFerro,type='o',pch=12,col='cyan')
lines(dataA3p$Temperatura,dataA3p$magnetFerro,type='o',pch=9,col='brown')
legend(x=3.6,y=0.8,legend=c('a=-3.0','a=-1.9','a=-1.0','a=+0.0','a=+1.0','a=+2.0','a=+3.0'),col=c('green','red','blue','orange','black','cyan','brown'),pch=c(15,16,17,18,8,12,9))
title(main='MagnetF vs Temp para L=040')
dev.off()

#Susceptibilidad Ferro

png('susceptFvsTemp.png')
plot(dataA3$Temperatura,dataA3$SusceptFerro,pch=15,type='o',col='green',ylim=c(0,0.03),xlab='Temperatura',ylab='Susceptiblidad Ferro')
lines(dataA19$Temperatura,dataA19$SusceptFerro,pch=16,type='o',col='red')
lines(dataA1$Temperatura,dataA1$SusceptFerro,pch=17,type='o',col='blue')
lines(dataA0$Temperatura,dataA0$SusceptFerro,pch=18,type='o',col='orange')
lines(dataA1p$Temperatura,dataA1p$SusceptFerro,pch=8,type='o',col='black')
lines(dataA2p$Temperatura,dataA2p$SusceptFerro,type='o',pch=12,col='cyan')
lines(dataA3p$Temperatura,dataA3p$SusceptFerro,type='o',pch=9,col='brown')
legend(x=4.0,y=0.03,legend=c('a=-3.0','a=-1.9','a=-1.0','a=+0.0','a=+1.0','a=+2.0','a=+3.0'),col=c('green','red','blue','orange','black','cyan','brown'),pch=c(15,16,17,18,8,12,9))
title(main='SusceptF vs Temp para L=040')
dev.off()

#Cumulante Ferro

png('cumulantFvsTemp.png')
plot(dataA3$Temperatura,dataA3$cumulantFerro,pch=15,type='o',col='green',ylim=c(-0.1,1.0),xlab='Temperatura',ylab='Cumulante Ferro')
lines(dataA19$Temperatura,dataA19$cumulantFerro,pch=16,type='o',col='red')
lines(dataA1$Temperatura,dataA1$cumulantFerro,pch=17,type='o',col='blue')
lines(dataA0$Temperatura,dataA0$cumulantFerro,pch=18,type='o',col='orange')
lines(dataA1p$Temperatura,dataA1p$cumulantFerro,pch=8,type='o',col='black')
lines(dataA2p$Temperatura,dataA2p$cumulantAnti,type='o',pch=12,col='cyan')
lines(dataA3p$Temperatura,dataA3p$cumulantAnti,type='o',pch=9,col='brown')
legend(x=1.0,y=0.8,legend=c('a=-3.0','a=-1.9','a=-1.0','a=+0.0','a=+1.0','a=+2.0','a=+3.0'),col=c('green','red','blue','orange','black','cyan','brown'),pch=c(15,16,17,18,8,12,9))
title(main='CumulantF vs Temp para L=040')
dev.off()

#-------------------------------------------------

alphaMaxsAnti <- array(dim=length(temps))
alphaMaxsFerro <- array(dim=length(temps))

for (i in temps) {
	#print(i) #-> the value of temp
	interData <- data[data$Temperatura==i,]
	#maxSuscept <- max(interData$SusceptAnti)
	alphaMaxsAnti[which(i==temps)] <- interData[interData$SusceptAnti==max(interData$SusceptAnti),]$Alfa
	alphaMaxsFerro[which(i==temps)] <- interData[interData$SusceptFerro==max(interData$SusceptFerro),]$Alfa
}
#Max AntiFerro vs Temps
png('maxSusceptAFvsTemp.png')
plot(temps,alphaMaxsAnti,pch=20,col='blue',type='o',xlab='Temperatura',ylab='Alfa ud')
title(main='maxSusceptAF vs Temp para L=040')
dev.off()
#Max Ferro vs Temps
png('maxSusceptFvsTemp.png')
plot(temps,alphaMaxsFerro,pch=20,col='blue',type='o',xlab='Temperatura',ylab='Alfa ud')
title(main='maxSusceptF vs Temp para L=040')
dev.off()

png('maxSusceptvsTemp.png')
plot(temps,alphaMaxsAnti,pch=20,col='blue',type='o',xlab='Temperatura',ylab='Alfa ud',ylim=c(-3,3))
lines(temps,alphaMaxsFerro,pch=18,col='red',type='o')
title(main='max Susceptibilidad vs Temp para L=040')
legend(x=1,y=3,legend=c('max Susceptibilidad AntiFerro','max Susceptibilidad Ferro'),col=c('blue','red'),pch=c(20,18))
dev.off()
