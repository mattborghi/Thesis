file <- 'OutputFile.log'

data <- read.csv(file,stringsAsFactors=F,header=T,sep=',')
#Density 0.5
#datab <- data[data$posDsty==0.5,]
temps <- unique(data$Temperatura)

dataT06 <- data[data$Temperatura==temps[1],]
dataT1 <- data[data$Temperatura==1.0,]
dataT13 <- data[data$Temperatura==temps[8],]
dataT16 <- data[data$Temperatura==temps[11],]
dataT19 <- data[data$Temperatura==temps[14],]
dataT2 <- data[data$Temperatura==2.0,]
dataT3 <- data[data$Temperatura==3.0,]
dataT4 <- data[data$Temperatura==4.0,]
dataT5 <- data[data$Temperatura==5.0,]

#Magnetization Anti

png('magnetAFvsAlfa.png')
plot(dataT06$Alfa,dataT06$magnetANTI,type='o',col='blue',ylim=c(0,1),xlab='Alpha ud',ylab='Magnetizacion AntiFerro')
lines(dataT1$Alfa,dataT1$magnetANTI,type='o',col='cyan')
#lines(dataT13$Alfa,dataT13$magnetANTI,type='o',col='magenta')
#lines(dataT16$Alfa,dataT16$magnetANTI,type='o',col='violet')
#lines(dataT19$Alfa,dataT19$magnetANTI,type='o',col='brown')
lines(dataT2$Alfa,dataT2$magnetANTI,type='o',col='green')
lines(dataT3$Alfa,dataT3$magnetANTI,type='o',col='orange')
lines(dataT4$Alfa,dataT4$magnetANTI,type='o',col='red')
lines(dataT5$Alfa,dataT5$magnetANTI,type='o',col='black')
#legend(x=-1.3,y=1.0,legend=c('T=0.6','T=1.0','T=1.3','T=1.6','T=1.9','T=2.0','T=3.0','T=4.0','T=5.0'),fill=c('blue','cyan','cyan','magenta','violet','brown','orange','red','black'))
legend(x=-1.3,y=1.0,legend=c('T=0.6','T=1.0','T=2.0','T=3.0','T=4.0','T=5.0'),fill=c('blue','cyan','green','orange','red','black'))
title(main='MagnetAF vs Alfa para L=040')
dev.off()

#Susceptibility Anti

png('susceptAFvsAlfa.png')
plot(dataT06$Alfa,dataT06$SusceptAnti,type='o',col='blue',ylim=c(0,0.06),xlab='Alpha ud',ylab='Susceptibilidad AntiFerro')
lines(dataT1$Alfa,dataT1$SusceptAnti,type='o',col='cyan')
lines(dataT2$Alfa,dataT2$SusceptAnti,type='o',col='green')
lines(dataT3$Alfa,dataT3$SusceptAnti,type='o',col='orange')
lines(dataT4$Alfa,dataT4$SusceptAnti,type='o',col='red')
lines(dataT5$Alfa,dataT5$SusceptAnti,type='o',col='black')
legend(x=-3,y=0.06,legend=c('T=0.6','T=1.0','T=2.0','T=3.0','T=4.0','T=5.0'),fill=c('blue','cyan','green','orange','red','black'))
title(main='susceptAntiF vs Alfa para L=040')
dev.off()

#Cumulant Anti

png('cumulantAFvsAlfa.png')
plot(dataT06$Alfa,dataT06$cumulantAnti,type='o',col='blue',ylim=c(0,0.7),xlab='Alpha ud',ylab='Cumulante AntiFerro')
lines(dataT1$Alfa,dataT1$cumulantAnti,type='o',col='cyan')
lines(dataT2$Alfa,dataT2$cumulantAnti,type='o',col='green')
lines(dataT3$Alfa,dataT3$cumulantAnti,type='o',col='orange')
lines(dataT4$Alfa,dataT4$cumulantAnti,type='o',col='red')
lines(dataT5$Alfa,dataT5$cumulantAnti,type='o',col='black')
legend(x=-1.4,y=0.6,legend=c('T=0.6','T=1.0','T=2.0','T=3.0','T=4.0','T=5.0'),fill=c('blue','cyan','green','orange','red','black'))
title(main='CumulantAntiF vs Alfa para L=040')
dev.off()

#Magnetization Ferro

png('magnetFvsAlfa.png')
plot(dataT06$Alfa,dataT06$magnetFerro,type='o',col='blue',ylim=c(0,0.03),xlab='Alpha ud',ylab='Magnetizacion Ferro')
lines(dataT1$Alfa,dataT1$magnetFerro,type='o',col='cyan')
lines(dataT2$Alfa,dataT2$magnetFerro,type='o',col='green')
lines(dataT3$Alfa,dataT3$magnetFerro,type='o',col='orange')
lines(dataT4$Alfa,dataT4$magnetFerro,type='o',col='red')
lines(dataT5$Alfa,dataT5$magnetFerro,type='o',col='black')
legend(x=-3,y=0.03,legend=c('T=0.6','T=1.0','T=2.0','T=3.0','T=4.0','T=5.0'),fill=c('blue','cyan','green','orange','red','black'))
title(main='MagnetF vs Alfa para L=040')
dev.off()

#Susceptibility Ferro

png('susceptFvsAlfa.png')
plot(dataT06$Alfa,dataT06$SusceptFerro,type='o',col='blue',ylim=c(0,0.0005),xlab='Alpha ud',ylab='Susceptibilidad Ferro')
lines(dataT1$Alfa,dataT1$SusceptFerro,type='o',col='cyan')
lines(dataT2$Alfa,dataT2$SusceptFerro,type='o',col='green')
lines(dataT3$Alfa,dataT3$SusceptFerro,type='o',col='orange')
lines(dataT4$Alfa,dataT4$SusceptFerro,type='o',col='red')
lines(dataT5$Alfa,dataT5$SusceptFerro,type='o',col='black')
legend(x=-3,y=0.0005,legend=c('T=0.6','T=1.0','T=2.0','T=3.0','T=4.0','T=5.0'),fill=c('blue','cyan','green','orange','red','black'))
title(main='susceptF vs Alfa para L=040')
dev.off()

#Cumulant Ferro

png('cumulantFvsAlfa.png')
plot(dataT06$Alfa,dataT06$cumulantFerro,type='o',col='blue',ylim=c(0,1.0),xlab='Alpha ud',ylab='Cumulante Ferro')
plot(dataT1$Alfa,dataT1$cumulantFerro,type='o',col='cyan')
lines(dataT2$Alfa,dataT2$cumulantFerro,type='o',col='green')
lines(dataT3$Alfa,dataT3$cumulantFerro,type='o',col='orange')
lines(dataT4$Alfa,dataT4$cumulantFerro,type='o',col='red')
lines(dataT5$Alfa,dataT5$cumulantFerro,type='o',col='black')
legend(x=-3.0,y=0.4,legend=c('T=0.6','T=1.0','T=2.0','T=3.0','T=4.0','T=5.0'),fill=c('blue','cyan','green','orange','red','black'))
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

#Magnetizacion AntiFerro

png('magnetAFvsTemp.png')
plot(dataA3$Temperatura,dataA3$magnetANTI,type='o',col='green',ylim=c(0,1),xlab='Temperatura',ylab='Magnetizacion AntiFerro')
lines(dataA19$Temp,dataA19$magnetANTI,type='o',col='red')
lines(dataA1$Temp,dataA1$magnetANTI,type='o',col='blue')
legend(x=3.5,y=1.0,legend=c('a=-3.0','a=-1.9','a=-1.0'),fill=c('green','red','blue'))
title(main='MagnetAF vs Temp para L=040')
dev.off()

#Susceptibilidad AntiFerro

png('susceptAFvsTemp.png')
plot(dataA3$Temperatura,dataA3$SusceptAnti,type='o',col='green',ylim=c(0,0.02),xlab='Temperatura',ylab='Susceptiblidad AntiFerro')
lines(dataA19$Temperatura,dataA19$SusceptAnti,type='o',col='red')
lines(dataA1$Temperatura,dataA1$SusceptAnti,type='o',col='blue')
legend(x=4.0,y=0.02,legend=c('a=-3.0','a=-1.9','a=-1.0'),fill=c('green','red','blue'))
title(main='SusceptAF vs Temp para L=040')
dev.off()

#Cumulante AntiFerro

png('cumulantAFvsTemp.png')
plot(dataA3$Temperatura,dataA3$cumulantAnti,type='o',col='green',ylim=c(0,1.0),xlab='Temperatura',ylab='Cumulante AntiFerro')
lines(dataA19$Temperatura,dataA19$cumulantAnti,type='o',col='red')
lines(dataA1$Temperatura,dataA1$cumulantAnti,type='o',col='blue')
legend(x=4.0,y=1.0,legend=c('a=-3.0','a=-1.9','a=-1.0'),fill=c('green','red','blue'))
title(main='CumulantAF vs Temp para L=040')
dev.off()

#Magnetizacion Ferro

png('magnetFvsTemp.png')
plot(dataA3$Temperatura,dataA3$magnetFerro,type='o',col='green',ylim=c(0,0.03),xlab='Temperatura',ylab='Magnetizacion Ferro')
lines(dataA19$Temp,dataA19$magnetFerro,type='o',col='red')
lines(dataA1$Temp,dataA1$magnetFerro,type='o',col='blue')
legend(x=3.5,y=0.03,legend=c('a=-3.0','a=-1.9','a=-1.0'),fill=c('green','red','blue'))
title(main='MagnetF vs Temp para L=040')
dev.off()

#Susceptibilidad Ferro

png('susceptFvsTemp.png')
plot(dataA3$Temperatura,dataA3$SusceptFerro,type='o',col='green',ylim=c(0,0.00038),xlab='Temperatura',ylab='Susceptiblidad Ferro')
lines(dataA19$Temperatura,dataA19$SusceptFerro,type='o',col='red')
lines(dataA1$Temperatura,dataA1$SusceptFerro,type='o',col='blue')
legend(x=4.0,y=0.00035,legend=c('a=-3.0','a=-1.9','a=-1.0'),fill=c('green','red','blue'))
title(main='SusceptF vs Temp para L=040')
dev.off()

#Cumulante Ferro

png('cumulantFvsTemp.png')
plot(dataA3$Temperatura,dataA3$cumulantFerro,type='o',col='green',ylim=c(-0.1,1.0),xlab='Temperatura',ylab='Cumulante Ferro')
lines(dataA19$Temperatura,dataA19$cumulantFerro,type='o',col='red')
lines(dataA1$Temperatura,dataA1$cumulantFerro,type='o',col='blue')
legend(x=1.0,y=0.8,legend=c('a=-3.0','a=-1.9','a=-1.0'),fill=c('green','red','blue'))
title(main='CumulantF vs Temp para L=040')
dev.off()

#-------------------------------------------------

alphaMaxs <- array(dim=length(temps))

for (i in temps) {
	#print(i) #-> the value of temp
	interData <- data[data$Temperatura==i,]
	#maxSuscept <- max(interData$SusceptAnti)
	alphaMaxs[which(i==temps)] <- interData[interData$SusceptAnti==max(interData$SusceptAnti),]$Alfa
}

png('maxSusceptAFvsTemp.png')
plot(temps,alphaMaxs,pch=20,col='blue',type='o',xlab='Temperatura',ylab='Alfa ud')
title(main='maxSusceptAF vs Temp para L=040')
dev.off()
