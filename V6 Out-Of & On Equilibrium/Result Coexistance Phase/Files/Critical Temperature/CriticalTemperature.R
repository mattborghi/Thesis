file128 <- 'L=128/OutputFile.log'
file256 <- 'L=256/OutputFile.log'
file512 <- 'L=512/OutputFile.log'

data128 <- read.csv(file128,stringsAsFactors=F,header=T,sep=',')
data256 <- read.csv(file256,stringsAsFactors=F,header=T,sep=',')
data512 <- read.csv(file512,stringsAsFactors=F,header=T,sep=',')
#Density 0.5
#datab <- data[data$posDsty==0.5,]
temps <- unique(data128$Temperatura)

#Fixed Alfa
alphaVals <- unique(data128$Alfa)

#Alfa = 10
dataL128 <- data128[data128$Alfa==alphaVals[2],]
	dataL128 <- dataL128[with(dataL128,order(Temperatura)),]
dataL256 <- data256[data256$Alfa==alphaVals[2],]
	dataL256 <- dataL256[with(dataL256,order(Temperatura)),]
dataL512 <- data512[data512$Alfa==alphaVals[2],]
	dataL512 <- dataL512[with(dataL512,order(Temperatura)),]

plot(dataL128$Temperatura,dataL128$cumulantFerro,type='o',col='red')
lines(dataL256$Temperatura,dataL256$cumulantFerro,type='o',col='blue')
lines(dataL512$Temperatura,dataL512$cumulantFerro,type='o',col='green')

#Alfa = -0.8
dataL128 <- data128[data128$Alfa==alphaVals[1],]
	dataL128 <- dataL128[with(dataL128,order(Temperatura)),]
dataL256 <- data256[data256$Alfa==alphaVals[1],]
	dataL256 <- dataL256[with(dataL256,order(Temperatura)),]
dataL512 <- data512[data512$Alfa==alphaVals[1],]
	dataL512 <- dataL512[with(dataL512,order(Temperatura)),]

plot(dataL128$Temperatura,dataL128$cumulantFerro,type='o',col='red')
lines(dataL256$Temperatura,dataL256$cumulantFerro,type='o',col='blue')
lines(dataL512$Temperatura,dataL512$cumulantFerro,type='o',col='green')


#Alfa = -1.2
dataL128 <- data128[data128$Alfa==alphaVals[3],]
	dataL128 <- dataL128[with(dataL128,order(Temperatura)),]
dataL256 <- data256[data256$Alfa==alphaVals[3],]
	dataL256 <- dataL256[with(dataL256,order(Temperatura)),]
dataL512 <- data512[data512$Alfa==alphaVals[3],]
	dataL512 <- dataL512[with(dataL512,order(Temperatura)),]

plot(dataL128$Temperatura,dataL128$cumulantAnti,type='o',col='red')
lines(dataL256$Temperatura,dataL256$cumulantAnti,type='o',col='blue')
lines(dataL512$Temperatura,dataL512$cumulantAnti,type='o',col='green')


