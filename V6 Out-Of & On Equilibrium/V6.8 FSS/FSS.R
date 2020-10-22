#OPEN FILES----

#FOLDER
folder32 <- 'L=032/'
folder64 <- 'L=064/'
folder128 <- 'L=128/'
folder256 <- 'L=256/'
folder512 <- 'L=512/'

#FILES
file1 <- 'a=1.00T=2.2500.dat'
file2 <- 'a=1.00T=2.2600.dat'
file3 <- 'a=1.00T=2.2700.dat'
file4 <- 'a=1.00T=2.2800.dat'
file5 <- 'a=1.00T=2.2900.dat'

# ********************************************
# MY FUNCTION ERR VALS
# ********************************************
calculateErrorCapCalor <- function(Energy,Energy2,Temperatura){

	numOfElements <- round( length(Energy)/2 ) 
	numofRepetitions <- 10
	cv <- array(0, dim=numofRepetitions)

	for (i in seq(numofRepetitions)) {

		#Pick numofelements elements with repetition
		pickedE <- sample(Energy,numOfElements,replace=T)
		pickedE2 <- sample(Energy2,numOfElements,replace=T)

		cv[i] <- ( mean(pickedE2) - mean(pickedE)^2 )/Temperatura^2
	}
	error <- sqrt( mean(cv^2) - mean(cv)^2 ) #Eq 47 pag 40 Tesis Bootstrap method
	
	return(error)
}
calculateErrorSuscept <- function(Magnet,Magnet2,Temperatura){

	numOfElements <- round( length(Magnet)/2 ) 
	numofRepetitions <- 10
	X <- array(0, dim=numofRepetitions)

	for (i in seq(numofRepetitions)) {

		#Pick numofelements elements with repetition
		pickedM <- sample(Magnet,numOfElements,replace=T)
		pickedM2 <- sample(Magnet2,numOfElements,replace=T)

		X[i] <- ( mean(pickedM2) - mean(pickedM)^2 )/Temperatura
	}
	error <- sqrt( mean(X^2) - mean(X)^2 ) #Eq 47 pag 40 Tesis Bootstrap method
	return(error)
}
calculateErrorCumulant <- function(Magnet2,Magnet4){

	numOfElements <- round( length(Magnet2)/2 ) 
	numofRepetitions <- 10
	U4 <- array(0, dim=numofRepetitions)

	for (i in seq(numofRepetitions)) {

		#Pick numofelements elements with repetition
		pickedM2 <- sample(Magnet2,numOfElements,replace=T)
		pickedM4 <- sample(Magnet4,numOfElements,replace=T)

		U4[i] <- 1 - mean(pickedM4)/ ( 3*mean(pickedM2)^2 )
	}
	error <- sqrt( mean(U4^2) - mean(U4)^2 ) #Eq 47 pag 40 Tesis Bootstrap method
	return(error)
}

#Magnet
meanMagnet <- array(0,dim=c(5,5))
dimnames(meanMagnet) = list( 
c("T=2.25", "T=2.26","T=2.27","T=2.28","T=2.29"),         # row names 
c("L=032", "L=064", "L=128","L=256","L=512")) # column names 
sdMagnet <- array(0,dim=c(5,5))
dimnames(sdMagnet) = list( 
c("T=2.25", "T=2.26","T=2.27","T=2.28","T=2.29"),         # row names 
c("L=032", "L=064", "L=128","L=256","L=512")) # column names 

#Magnet2
meanMagnet2 <- array(0,dim=c(5,5))
dimnames(meanMagnet2) = list( 
c("T=2.25", "T=2.26","T=2.27","T=2.28","T=2.29"),         # row names 
c("L=032", "L=064", "L=128","L=256","L=512")) # column names 
sdMagnet2 <- array(0,dim=c(5,5))
dimnames(sdMagnet2) = list( 
c("T=2.25", "T=2.26","T=2.27","T=2.28","T=2.29"),         # row names 
c("L=032", "L=064", "L=128","L=256","L=512")) # column names 

#Magnet4
meanMagnet4 <- array(0,dim=c(5,5))
dimnames(meanMagnet4) = list( 
c("T=2.25", "T=2.26","T=2.27","T=2.28","T=2.29"),         # row names 
c("L=032", "L=064", "L=128","L=256","L=512")) # column names 
sdMagnet4 <- array(0,dim=c(5,5))
dimnames(sdMagnet4) = list( 
c("T=2.25", "T=2.26","T=2.27","T=2.28","T=2.29"),         # row names 
c("L=032", "L=064", "L=128","L=256","L=512")) # column names 

#Energia
meanEnerg <- array(0,dim=c(5,5))
dimnames(meanEnerg) = list( 
c("T=2.25", "T=2.26","T=2.27","T=2.28","T=2.29"),         # row names 
c("L=032", "L=064", "L=128","L=256","L=512")) # column names 
sdEnerg <- array(0,dim=c(5,5))
dimnames(sdEnerg) = list( 
c("T=2.25", "T=2.26","T=2.27","T=2.28","T=2.29"),         # row names 
c("L=032", "L=064", "L=128","L=256","L=512")) # column names 

#Energia2
meanEnerg2 <- array(0,dim=c(5,5))
dimnames(meanEnerg2) = list( 
c("T=2.25", "T=2.26","T=2.27","T=2.28","T=2.29"),         # row names 
c("L=032", "L=064", "L=128","L=256","L=512")) # column names 
sdEnerg2 <- array(0,dim=c(5,5))
dimnames(sdEnerg2) = list( 
c("T=2.25", "T=2.26","T=2.27","T=2.28","T=2.29"),         # row names 
c("L=032", "L=064", "L=128","L=256","L=512")) # column names 

#Cap Calorifica
sdCapCalorifica <- array(0,dim=c(5,5))
dimnames(sdCapCalorifica) = list( 
c("T=2.25", "T=2.26","T=2.27","T=2.28","T=2.29"),         # row names 
c("L=032", "L=064", "L=128","L=256","L=512")) # column names 

#Susceptibilidad
sdSuscept <- array(0,dim=c(5,5))
dimnames(sdSuscept) = list( 
c("T=2.25", "T=2.26","T=2.27","T=2.28","T=2.29"),         # row names 
c("L=032", "L=064", "L=128","L=256","L=512")) # column names 

#Cumulante
sdCumulante <- array(0,dim=c(5,5))
dimnames(sdCumulante) = list( 
c("T=2.25", "T=2.26","T=2.27","T=2.28","T=2.29"),         # row names 
c("L=032", "L=064", "L=128","L=256","L=512")) # column names 

Temperatura <- c(2.25,2.26,2.27,2.28,2.29)

indFile <- 0

for (size in c(32,64,128,256,512)) {
	indFile <- indFile + 1
	if (size == 32){
		folder <- folder32
	}else if (size==64){
		folder <- folder64
	} else if (size==128){
		folder<- folder128
	}else if (size==256){
		folder <- folder256
	}else{
		folder <- folder512
	}
	indTemp <- 0

	cat( sprintf('For size: %d\n', size) )
	for (temp in seq(5)) {
		indTemp <- indTemp + 1
		cat( sprintf("\tLooping temps: %s\n",temp) )

		if (temp == 1){
			file <- file1
		}else if (temp==2){
			file <- file2
		} else if (temp==3){
			file<- file3
		}else if (temp==4){
			file <- file4
		}else{
			file <- file5
		}
		root <- sprintf('%s%s',folder,file)
		cat(sprintf("\tRoot: %s\n",root))
		
		data <- read.csv( root ,stringsAsFactors=F,header=T,sep='')
		#head(data)
		#plot (data$MCS,data$Magnetization)

		magnet <- data$Magnetization[data$MCS >= 20000]
		magnet2 <- data$Magnetization2[data$MCS >= 20000]
		magnet4 <- data$Magnetization4[data$MCS >= 20000]
		energ <- data$Energy[data$MCS >= 20000]
		energ2 <- data$Energy2[data$MCS >= 20000]
		#sum from 5000MCS till end
		longitud <- length( magnet )
		summed <- sum ( magnet )
		meanM <- summed/longitud # idem to mean( filtered )
		sdevM <- sqrt(sum( (meanM - magnet )^2)/(longitud-1)) #idem as sd(filtered)

		meanMagnet[indTemp,indFile] <- meanM
		sdMagnet[indTemp,indFile] <- sdevM

		meanM2 <- mean( magnet2 )
		sdevM2 <- sd(magnet2)

		meanMagnet2[indTemp,indFile] <- meanM2
		sdMagnet2[indTemp,indFile] <- sdevM2

		meanM4 <- mean( magnet4 )
		sdevM4 <- sd(magnet4)

		meanMagnet4[indTemp,indFile] <- meanM4
		sdMagnet4[indTemp,indFile] <- sdevM4

		meanE <- mean( energ )
		sdevE <- sd(energ)

		meanEnerg[indTemp,indFile] <- meanE
		sdEnerg[indTemp,indFile] <- sdevE

		meanE2 <- mean( energ2 )
		sdevE2 <- sd(energ2)

		meanEnerg2[indTemp,indFile] <- meanE2
		sdEnerg2[indTemp,indFile] <- sdevE2

		sdCapCalorifica [indTemp,indFile] <- calculateErrorCapCalor(energ,energ2,Temperatura[temp]) 
		sdSuscept [indTemp,indFile] <-  calculateErrorSuscept(magnet,magnet2,Temperatura[temp])
		sdCumulante [indTemp,indFile] <- calculateErrorCumulant(magnet2,magnet4)

	}

}

# *****************************************************************
# CANTIDADES DE INTERES
# *****************************************************************
# *****************************************************************
# MAGNETIZACION
# *****************************************************************
Tc <- 2.269
#Plot L=32
pdf('Graphs/Magnetization.pdf')

yVals <- meanMagnet[,1]
errs <- sdMagnet[,1]

#x<- 1:(length( yVals ))

plot(Temperatura, yVals ,ylim=c(0,1), #ylim=range(c(yVals-errs, yVals+errs))
    pch=16, xlab="Temperatura", ylab="Magnetización +/- SD",col='red',lty=2,
    main="Scatter plot with std.dev error bars",type='o'
)
# hack: we draw arrows but with very special "arrowheads"
arrows(Temperatura, yVals-errs, Temperatura, yVals+errs, length=0.05, angle=90, code=3)

#L=64
yVals <- meanMagnet[,2]
errs <- sdMagnet[,2]

points(Temperatura, yVals , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=17,col='blue',lty=2,type='o'
)
arrows(Temperatura, yVals-errs, Temperatura, yVals+errs, length=0.05, angle=90, code=3)

#L=128
yVals <- meanMagnet[,3]
errs <- sdMagnet[,3]

points(Temperatura, yVals , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=18,col='seagreen1',lty=3,type='o'
)
arrows(Temperatura, yVals-errs, Temperatura, yVals+errs, length=0.05, angle=90, code=3)

#L=256
yVals <- meanMagnet[,4]
errs <- sdMagnet[,4]

points(Temperatura, yVals , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=19,col='cyan',lty=4,type='o'
)
arrows(Temperatura, yVals-errs, Temperatura, yVals+errs, length=0.05, angle=90, code=3)

#L=512
yVals <- meanMagnet[,5]
errs <- sdMagnet[,5]

points(Temperatura, yVals , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=20,col='black',lty=5,type='o'
)
arrows(Temperatura, yVals-errs, Temperatura, yVals+errs, length=0.05, angle=90, code=3)

legend(x=2.28,y=1.0,legend=c('L=032','L=064','L=128','L=256','L=512'),col=c('red','blue','seagreen1','cyan','black'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()
# *****************************************************************
# SUSCEPTIBILIDAD
# *****************************************************************

#Plot L=32
pdf('Graphs/Susceptibilidad.pdf')

yVals <- meanMagnet2[,1] - meanMagnet[,1]^2
errs <- sdSuscept[,1]

plot(Temperatura, yVals/Temperatura ,ylim=c(0,0.05), #ylim=range(c(yVals-errs, yVals+errs))
    pch=16, xlab="Temperatura", ylab="Susceptibilidad",col='red',lty=2,
    main="Scatter plot",type='o'
)
arrows(Temperatura, yVals/Temperatura-errs, Temperatura, yVals/Temperatura+errs, length=0.05, angle=90, code=3)

#L=64
yVals <- meanMagnet2[,2] - meanMagnet[,2]^2
errs <- sdSuscept[,2]

points(Temperatura, yVals/Temperatura , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=17,col='blue',lty=2,type='o'
)
arrows(Temperatura, yVals/Temperatura-errs, Temperatura, yVals/Temperatura+errs, length=0.05, angle=90, code=3)

#L=128
yVals <- meanMagnet2[,3] - meanMagnet[,3]^2
errs <- sdSuscept[,3]

points(Temperatura, yVals/Temperatura , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=18,col='seagreen1',lty=3,type='o'
)
arrows(Temperatura, yVals/Temperatura-errs, Temperatura, yVals/Temperatura+errs, length=0.05, angle=90, code=3)

#L=256
yVals <- meanMagnet2[,4] - meanMagnet[,4]^2
errs <- sdSuscept[,4]

points(Temperatura, yVals/Temperatura , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=19,col='cyan',lty=4,type='o'
)
arrows(Temperatura, yVals/Temperatura-errs, Temperatura, yVals/Temperatura+errs, length=0.05, angle=90, code=3)

#L=512
yVals <- meanMagnet2[,5] - meanMagnet[,5]^2
errs <- sdSuscept[,5]

points(Temperatura, yVals/Temperatura , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=20,col='black',lty=5,type='o'
)
arrows(Temperatura, yVals/Temperatura-errs, Temperatura, yVals/Temperatura+errs, length=0.05, angle=90, code=3)

legend(x=2.28,y=0.05,legend=c('L=032','L=064','L=128','L=256','L=512'),col=c('red','blue','seagreen1','cyan','black'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()
# *****************************************************************
# ENERGIA
# *****************************************************************
#Plot L=32
pdf('Graphs/Energia.pdf')

yVals <- meanEnerg[,1]
errs <- sdEnerg[,1]

#x<- 1:(length( yVals ))

plot(Temperatura, yVals ,ylim=c(-1.5,-1.3), #ylim=range(c(yVals-errs, yVals+errs))
    pch=16, xlab="Temperatura", ylab="Energia +/- SD",col='red',lty=2,
    main="Scatter plot with std.dev error bars",type='o'
)
# hack: we draw arrows but with very special "arrowheads"
arrows(Temperatura, yVals-errs, Temperatura, yVals+errs, length=0.05, angle=90, code=3)

#L=64
yVals <- meanEnerg[,2]
errs <- sdEnerg[,2]

points(Temperatura, yVals , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=17,col='blue',lty=2,type='o'
)
arrows(Temperatura, yVals-errs, Temperatura, yVals+errs, length=0.05, angle=90, code=3)

#L=128
yVals <- meanEnerg[,3]
errs <- sdEnerg[,3]

points(Temperatura, yVals , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=18,col='seagreen1',lty=3,type='o'
)
arrows(Temperatura, yVals-errs, Temperatura, yVals+errs, length=0.05, angle=90, code=3)

#L=256
yVals <- meanEnerg[,4]
errs <- sdEnerg[,4]

points(Temperatura, yVals , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=19,col='cyan',lty=4,type='o'
)
arrows(Temperatura, yVals-errs, Temperatura, yVals+errs, length=0.05, angle=90, code=3)

#L=512
yVals <- meanEnerg[,5]
errs <- sdEnerg[,5]

points(Temperatura, yVals , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=20,col='black',lty=5,type='o'
)
arrows(Temperatura, yVals-errs, Temperatura, yVals+errs, length=0.05, angle=90, code=3)

legend(x=2.25,y=-1.3,legend=c('L=032','L=064','L=128','L=256','L=512'),col=c('red','blue','seagreen1','cyan','black'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()

# *****************************************************************
# CAPACIDAD CALORIFICA
# *****************************************************************

#Plot L=32
pdf('Graphs/CapacidadCalorifica.pdf')

yVals <- meanEnerg2[,1] - meanEnerg[,1]^2
#errs <- sqrt( 4*sdEnerg[,1]^2+sdEnerg2[,1]^2 )/Temperatura^2
errs <- sdCapCalorifica[,1] 

plot(Temperatura, yVals/Temperatura^2 ,ylim=c(1e-4,5e-3), #ylim=range(c(yVals-errs, yVals+errs))
    pch=16, xlab="Temperatura", ylab="Susceptibilidad",col='red',lty=2,
    main="Scatter plot",type='o'
)
arrows(Temperatura, yVals/Temperatura^2-errs, Temperatura, yVals/Temperatura^2+errs, length=0.05, angle=90, code=3)

#L=64
yVals <- meanEnerg2[,2] - meanEnerg[,2]^2
errs <- sdCapCalorifica[,2]

points(Temperatura, yVals/Temperatura^2 , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=17,col='blue',lty=2,type='o'
)
arrows(Temperatura, yVals/Temperatura^2-errs, Temperatura, yVals/Temperatura^2+errs, length=0.05, angle=90, code=3)

#L=128
yVals <- meanEnerg2[,3] - meanEnerg[,3]^2
errs <- sdCapCalorifica[,3]

points(Temperatura, yVals/Temperatura^2 , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=18,col='seagreen1',lty=3,type='o'
)
arrows(Temperatura, yVals/Temperatura^2-errs, Temperatura, yVals/Temperatura^2+errs, length=0.05, angle=90, code=3)

#L=256
yVals <- meanEnerg2[,4] - meanEnerg[,4]^2
errs <- sdCapCalorifica[,4]

points(Temperatura, yVals/Temperatura^2 , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=19,col='cyan',lty=4,type='o'
)
arrows(Temperatura, yVals/Temperatura^2-errs, Temperatura, yVals/Temperatura^2+errs, length=0.05, angle=90, code=3)

#L=512
yVals <- meanEnerg2[,5] - meanEnerg[,5]^2
errs <- sdCapCalorifica[,5]

points(Temperatura, yVals/Temperatura^2 , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=20,col='black',lty=5,type='o'
)
arrows(Temperatura, yVals/Temperatura^2-errs, Temperatura, yVals/Temperatura^2+errs, length=0.05, angle=90, code=3)

legend(x=2.28,y=4e-3,legend=c('L=032','L=064','L=128','L=256','L=512'),col=c('red','blue','seagreen1','cyan','black'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()
# *****************************************************************
# CUMULANTE
# *****************************************************************
pdf('Graphs/Cumulante.pdf')
	cumulante32 <- 1- meanMagnet4[,1]/(3*meanMagnet2[,1]^2)
plot (Temperatura, cumulante32,pch=16,col='red',ylim=c(0.2,0.7),type='o',lty=1)
	errs <- sdCumulante[,1]
	arrows(Temperatura, cumulante32-errs, Temperatura, cumulante32+errs, length=0.05, angle=90, code=3)
	#
	cumulante64 <- 1- meanMagnet4[,2]/(3*meanMagnet2[,2]^2)
points (Temperatura, cumulante64,pch=17,col='cyan',type='o',lty=2)
	errs <- sdCumulante[,2]
	arrows(Temperatura, cumulante64-errs, Temperatura, cumulante64+errs, length=0.05, angle=90, code=3)
	#
	cumulante128 <- 1- meanMagnet4[,3]/(3*meanMagnet2[,3]^2)
points (Temperatura, cumulante128,pch=18,col='green',type='o',lty=3)
	errs <- sdCumulante[,3]
	arrows(Temperatura, cumulante128-errs, Temperatura, cumulante128+errs, length=0.05, angle=90, code=3)
	#
	cumulante256 <- 1- meanMagnet4[,4]/(3*meanMagnet2[,4]^2)
points (Temperatura, cumulante256,pch=19,col='seagreen1',type='o',lty=4)
	errs <- sdCumulante[,4]
	arrows(Temperatura, cumulante256-errs, Temperatura, cumulante256+errs, length=0.05, angle=90, code=3)
	#
	cumulante512 <- 1- meanMagnet4[,5]/(3*meanMagnet2[,5]^2)
points (Temperatura, cumulante512,pch=20,col='blue',type='o',lty=5)
	errs <- sdCumulante[,5]
	arrows(Temperatura, cumulante512-errs, Temperatura, cumulante512+errs, length=0.05, angle=90, code=3)
	#
legend(x=2.25,y=0.55,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('red','cyan','green','seagreen1','blue'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()

for (i in seq(5)) {
	#print(i)
	cumulante <- 1- meanMagnet4[,i]/(3*meanMagnet2[,i]^2)
	df <- data.frame (Temperatura, cumulante)
	if (i == 1){
		file <- 'FitResults/cumulante32.dat'
	}else if(i==2){
		file <- 'FitResults/cumulante64.dat'
	}else if(i==3){
		file <- 'FitResults/cumulante128.dat'
	}else if(i==4){
		file <- 'FitResults/cumulante256.dat'
	}else{
		file <- 'FitResults/cumulante512.dat'		
	}

	write.table(df,file=file,sep="\t",row.names=FALSE)

}
# *****************************************************************
# CUMULANTE COLAPSADO
# *****************************************************************
pdf('Graphs/CumulanteColapsado.pdf')
plot (32*(Temperatura-Tc)/Tc, 1- meanMagnet4[,1]/(3*meanMagnet2[,1]^2),pch=16,col='red',ylim=c(0.2,0.7),type='o',lty=1,xlim=c(-2,2))
points (64*(Temperatura-Tc)/Tc, 1- meanMagnet4[,2]/(3*meanMagnet2[,2]^2),pch=17,col='cyan',type='o',lty=2)
points (128*(Temperatura-Tc)/Tc, 1- meanMagnet4[,3]/(3*meanMagnet2[,3]^2),pch=18,col='green',type='o',lty=3)
points (256*(Temperatura-Tc)/Tc, 1- meanMagnet4[,4]/(3*meanMagnet2[,4]^2),pch=19,col='seagreen1',type='o',lty=4)
points (512*(Temperatura-Tc)/Tc, 1- meanMagnet4[,5]/(3*meanMagnet2[,5]^2),pch=20,col='blue',type='o',lty=5)
legend(x=-0.2,y=0.4,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('red','cyan','green','seagreen1','blue'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()

# *****************************************************************
# SUSCEPTIBILIDAD COLAPSADA
# *****************************************************************
exp <- -7/4
#Plot L=32
pdf('Graphs/SusceptibilidadColapsada.pdf')

yVals <- meanMagnet2[,1] - meanMagnet[,1]^2

plot(32*(Temperatura-Tc)/Tc, yVals/Temperatura*32^(exp), ylim=c(1e-6,3e-5),#ylim=range(c(yVals-errs, yVals+errs))
    pch=16, xlab="(Temperatura-Tc)/Tc", ylab="Susceptibilidad Reducida",col='red',lty=2,
    main="Scatter plot",type='o',xlim=c(-2,2)
)

#L=64
yVals <- meanMagnet2[,2] - meanMagnet[,2]^2

points(64*(Temperatura-Tc)/Tc, yVals/Temperatura*64^(exp) , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=17,col='blue',lty=2,type='o'
)

#L=128
yVals <- meanMagnet2[,3] - meanMagnet[,3]^2

points(128*(Temperatura-Tc)/Tc, yVals/Temperatura*128^(exp) , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=18,col='seagreen1',lty=3,type='o'
)

#L=256
yVals <- meanMagnet2[,4] - meanMagnet[,4]^2

points(256*(Temperatura-Tc)/Tc, yVals/Temperatura*256^(exp) , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=19,col='cyan',lty=4,type='o'
)

#L=512
yVals <- meanMagnet2[,5] - meanMagnet[,5]^2

points(512*(Temperatura-Tc)/Tc, yVals/Temperatura*512^(exp) , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=20,col='black',lty=5,type='o'
)

legend(x=1,y=2e-5,legend=c('L=032','L=064','L=128','L=256','L=512'),col=c('red','blue','seagreen1','cyan','black'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()
# *****************************************************************
# MAGNETIZACION COLAPSADA
# *****************************************************************
exp <- 1/8
#Plot L=32
pdf('Graphs/MagnetizacionColapsada.pdf')

yVals <- meanMagnet[,1]

plot(32*(Temperatura-Tc)/Tc, yVals*32^(exp), #ylim=c(1e-6,3e-5),#ylim=range(c(yVals-errs, yVals+errs))
    pch=16, xlab="(Temperatura-Tc)/Tc", ylab="Magnetizacion Reducida",col='red',lty=2,
    main="Scatter plot",type='o',xlim=c(-2,2)
)

#L=64
yVals <- meanMagnet[,2]

points(64*(Temperatura-Tc)/Tc, yVals*64^(exp) , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=17,col='blue',lty=2,type='o'
)

#L=128
yVals <- meanMagnet[,3]

points(128*(Temperatura-Tc)/Tc, yVals*128^(exp) , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=18,col='seagreen1',lty=3,type='o'
)

#L=256
yVals <- meanMagnet[,4]

points(256*(Temperatura-Tc)/Tc, yVals*256^(exp) , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=19,col='cyan',lty=4,type='o'
)

#L=512
yVals <- meanMagnet[,5]

points(512*(Temperatura-Tc)/Tc, yVals*512^(exp) , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=20,col='black',lty=5,type='o'
)

legend(x=1,y=2e-5,legend=c('L=032','L=064','L=128','L=256','L=512'),col=c('red','blue','seagreen1','cyan','black'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()

# *****************************************************************
# CAPACIDAD CALORIFICA COLAPSADA
# *****************************************************************
exp <- 1.5
#Plot L=32
pdf('Graphs/CapacidadCalorificaColaspada.pdf')

yVals <- meanEnerg2[,1] - meanEnerg[,1]^2

plot(32*(Temperatura-Tc)/Tc, yVals/(Temperatura^2)*32^(exp), ylim=c(0.05,0.4),#ylim=range(c(yVals-errs, yVals+errs))
    pch=16, xlab="(Temperatura-Tc)/Tc", ylab="Capacidad Calorifica Reducida",col='red',lty=2,
    main="Scatter plot",type='o',xlim=c(-2,2)
)

#L=64
yVals <- meanEnerg2[,2] - meanEnerg[,2]^2

points(64*(Temperatura-Tc)/Tc, yVals/(Temperatura^2)*64^(exp) , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=17,col='blue',lty=2,type='o'
)

#L=128
yVals <- meanEnerg2[,3] - meanEnerg[,3]^2

points(128*(Temperatura-Tc)/Tc, yVals/(Temperatura^2)*128^(exp) , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=18,col='seagreen1',lty=3,type='o'
)

#L=256
yVals <- meanEnerg2[,4] - meanEnerg[,4]^2

points(256*(Temperatura-Tc)/Tc, yVals/(Temperatura^2)*256^(exp) , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=19,col='cyan',lty=4,type='o'
)

#L=512
yVals <- meanEnerg2[,5] - meanEnerg[,5]^2

points(512*(Temperatura-Tc)/Tc, yVals/(Temperatura^2)*512^(exp) , #ylim=range(c(yVals-errs, yVals+errs)),
    pch=20,col='black',lty=5,type='o'
)

legend(x=1,y=0.4,legend=c('L=032','L=064','L=128','L=256','L=512'),col=c('red','blue','seagreen1','cyan','black'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()
