folder064 <- '../L=064/'
folder128 <- '../L=128/'
folder256 <- '../L=256/'
folder512 <- '../L=512/'

file <- 'Graphs/fitted.data'

#Define 4-Percent function
P4 <- function(number){
	return (fourvalue <- number*4/100)
}
#Define Reduced Temperature function
TauTemp <- function(beta){
	return (tautemperature <- (1-tanh(beta)))
}

#Load to data frames
data064 <- read.csv(paste(folder064,file,sep=""),header=F,stringsAsFactors=F,sep="\t")
data128 <- read.csv(paste(folder128,file,sep=""),header=F,stringsAsFactors=F,sep="\t")
data256 <- read.csv(paste(folder256,file,sep=""),header=F,stringsAsFactors=F,sep="\t")
data512 <- read.csv(paste(folder512,file,sep=""),header=F,stringsAsFactors=F,sep="\t")

#Fix Column names
names(data064) <- c("Temperatura","Magnetizacion","Magnetizacion2")
names(data128) <- c("Temperatura","Magnetizacion","Magnetizacion2")
names(data256) <- c("Temperatura","Magnetizacion","Magnetizacion2")
names(data512) <- c("Temperatura","Magnetizacion","Magnetizacion2")

#Delete first row
data064 <- data064[2:(nrow(data064)),]
data128 <- data128[2:(nrow(data128)),]
data256 <- data256[2:(nrow(data256)),]
data512 <- data512[2:(nrow(data512)),]

#Change the Temperature column as numeric
data064$Temperatura <- as.numeric(data064$Temperatura)
data128$Temperatura <- as.numeric(data128$Temperatura)
data256$Temperatura <- as.numeric(data256$Temperatura)
data512$Temperatura <- as.numeric(data512$Temperatura)

#Beta columns 
beta064 <- data064$Temperatura^(-1)
beta128 <- data128$Temperatura^(-1)
beta256 <- data256$Temperatura^(-1)
beta512 <- data512$Temperatura^(-1)

#Susceptibilities
Suscept064 <- beta064*(data064$Magnetizacion2-data064$Magnetizacion^2)
Suscept128 <- beta128*(data128$Magnetizacion2-data128$Magnetizacion^2)
Suscept256 <- beta256*(data256$Magnetizacion2-data256$Magnetizacion^2)
Suscept512 <- beta512*(data512$Magnetizacion2-data512$Magnetizacion^2)

#Tau Values
TauTemp064 <- TauTemp(beta064)
TauTemp128 <- TauTemp(beta128)
TauTemp256 <- TauTemp(beta256)
TauTemp512 <- TauTemp(beta512)

#Plot in the same graph
#Maximum and minimum Values
Xmax <- max(TauTemp064,TauTemp128,TauTemp256,TauTemp512)
Xmin <- min(TauTemp064,TauTemp128,TauTemp256,TauTemp512)

Ymax <- max(Suscept064,Suscept128,Suscept256,Suscept512)
Ymin <- min(Suscept064,Suscept128,Suscept256,Suscept512)


#Zero-Field Susceptibility L=064
plot(TauTemp064,Suscept064,pch=17,col='green',main="Susceptibilidad",xlab=c("1-tanh(beta)"),ylab=c("Susceptibilidad"),xlim=c(Xmin+P4(Xmin),Xmax+P4(Xmax)),ylim=c(Ymin+P4(Ymin),Ymax+P4(Ymax)))
#Zero-Field Susceptibility L=128
points(TauTemp128,Suscept128,pch=18,col='blue')
#Zero-Field Susceptibility L=256
points(TauTemp256,Suscept256,pch=19,col='red')
points(TauTemp512,Suscept512,pch=20,col='black')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
Yleg <- (Ymax+Ymin)/2
legend(x=Xmax/2,y=Yleg,col=c("green","blue","red","black"),pch=c(17,18,19,20),legend=c("L=064","L=128","L=256","L=512"))

#Save Reduced Suscept
dev.copy(pdf,'../Susceptibility.pdf')
dev.off()

#-------------------

ReducedSuscept064 <- beta064*(data064$Magnetizacion2)
ReducedSuscept128 <- beta128*(data128$Magnetizacion2)
ReducedSuscept256 <- beta256*(data256$Magnetizacion2)
ReducedSuscept512 <- beta512*(data512$Magnetizacion2)


Ymax <- max(ReducedSuscept064,ReducedSuscept128,ReducedSuscept256,ReducedSuscept512)
Ymin <- min(ReducedSuscept064,ReducedSuscept128,ReducedSuscept256,ReducedSuscept512)

#Plot Reduced Susceptibility
#Zero-Field Susceptibility L=064
plot(TauTemp064,ReducedSuscept064,pch=17,col='green',main="Reduced Susceptibility",xlim=c(Xmin+P4(Xmin),Xmax+P4(Xmax)),ylim=c(Ymin+P4(Ymin),Ymax+P4(Ymax)),xlab=c("1-tanh(beta)"),ylab=c("Susceptibilidad Reducida"))
#Zero-Field Susceptibility L=128
points(TauTemp128,ReducedSuscept128,pch=18,col='blue')
#Zero-Field Susceptibility L=256
points(TauTemp256,ReducedSuscept256,pch=19,col='red')
points(TauTemp512,ReducedSuscept512,pch=20,col='black')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
legend(x=Xmax/2,y=Ymax/2,col=c("green","blue","red","black"),pch=c(17,18,19,20),legend=c("L=064","L=128","L=256","L=512"))
#Save Reduced Suscept
dev.copy(pdf,'../ReducedSuscep.pdf')
dev.off()

#--------------------

#Collapse

#Plot COLLAPSED Reduced Susceptibility

#Scalled Columns-----
L064 <- 64^2
ScalledTemp064 <- L064*(1-tanh(beta064))/(tanh(beta064)^0.5)
ScalledSusc064 <- (ReducedSuscept064+1)/(2*L064/(tanh(beta064)^0.5))

L128 <- 128^2
ScalledTemp128 <- L128*(1-tanh(beta128))/(tanh(beta128)^0.5)
ScalledSusc128 <- (ReducedSuscept128+1)/(2*L128/(tanh(beta128)^0.5))

L256 <- 256^2
ScalledTemp256 <- L256*(1-tanh(beta256))/(tanh(beta256)^0.5)
ScalledSusc256 <- (ReducedSuscept256+1)/(2*L256/(tanh(beta256)^0.5))

L512 <- 512^2
ScalledTemp512 <- L512*(1-tanh(beta512))/(tanh(beta512)^0.5)
ScalledSusc512 <- (ReducedSuscept512+1)/(2*L512/(tanh(beta512)^0.5))
#Maximum and minimum Values
Xmax <- max(ScalledTemp064,ScalledTemp128,ScalledTemp256,ScalledTemp512)
Xmin <- min(ScalledTemp064,ScalledTemp128,ScalledTemp256,ScalledTemp512)

Ymax <- max(ScalledSusc064,ScalledSusc128,ScalledSusc256,ScalledSusc512)
Ymin <- min(ScalledSusc064,ScalledSusc128,ScalledSusc256,ScalledSusc512)

#Zero-Field Susceptibility L=064
plot(ScalledTemp064,ScalledSusc064,xlim=c(Xmin+P4(Xmin),Xmax+P4(Xmax)),ylim=c(Ymin+P4(Ymin),Ymax+P4(Ymax)),type='p',pch=17,col='green',main='Collapsed Reduced Susceptibility',xlab=c("Temperatura Colapsada"),ylab=c("Susceptibilidad Reducida Colapsada"))
#,xlim=c(min(ScalledTemp064),max(ScalledTemp064))
#Zero-Field Susceptibility L=128
points(ScalledTemp128,ScalledSusc128,pch=18,col='blue')
#Zero-Field Susceptibility L=256
points(ScalledTemp256,ScalledSusc256,pch=19,col='red')
points(ScalledTemp512,ScalledSusc512,pch=20,col='black')
#Legend
legend(x=Xmax/2,y=Ymax/2,col=c("green","blue","red","black"),pch=c(17,18,19,20),legend=c("L=064","L=128","L=256","L=512"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 

#Save Collapsed
dev.copy(pdf,'../CollapsedRedSuscep.pdf')
dev.off()


#Plot COLLAPSED Total Susceptibility

#Scalled Columns-----
ScalledSusc064 <- (Suscept064+1)/(2*L064/(tanh(beta064)^0.5))

ScalledSusc128 <- (Suscept128+1)/(2*L128/(tanh(beta128)^0.5))

ScalledSusc256 <- (Suscept256+1)/(2*L256/(tanh(beta256)^0.5))

ScalledSusc512 <- (Suscept512+1)/(2*L512/(tanh(beta512)^0.5))

#Maximum and minimum Values
Xmax <- max(ScalledTemp064,ScalledTemp128,ScalledTemp256,ScalledTemp512)
Xmin <- min(ScalledTemp064,ScalledTemp128,ScalledTemp256,ScalledTemp512)

Ymax <- max(ScalledSusc064,ScalledSusc128,ScalledSusc256,ScalledSusc512)
Ymin <- min(ScalledSusc064,ScalledSusc128,ScalledSusc256,ScalledSusc512)
Yleg <- (Ymax+Ymin)/2
#Zero-Field Susceptibility L=064
plot(ScalledTemp064,ScalledSusc064,xlim=c(Xmin+P4(Xmin),Xmax+P4(Xmax)),ylim=c(Ymin+P4(Ymin),Ymax+P4(Ymax)),type='p',pch=17,col='green',main='Collapsed Susceptibility',xlab=c("Temperatura Colapsada"),ylab=c("Susceptibilidad Colapsada"))
#,xlim=c(min(ScalledTemp064),max(ScalledTemp064))
#Zero-Field Susceptibility L=128
points(ScalledTemp128,ScalledSusc128,pch=18,col='blue')
#Zero-Field Susceptibility L=256
points(ScalledTemp256,ScalledSusc256,pch=19,col='red')
points(ScalledTemp512,ScalledSusc512,pch=20,col='black')
#Legend
legend(x=Xmax/2,y=Yleg,col=c("green","blue","red","black"),pch=c(17,18,19,20),legend=c("L=064","L=128","L=256","L=512"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 

#Save Collapsed
dev.copy(pdf,'../CollapsedSuscep.pdf')
dev.off()

#---------------MAGNETIZATION----------------------------------
Magnet064 <- data064$Magnetizacion
Magnet128 <- data128$Magnetizacion
Magnet256 <- data256$Magnetizacion
Magnet512 <- data512$Magnetizacion

#Maximum and minimum Values
Xmax <- max(TauTemp064,TauTemp128,TauTemp256,TauTemp512)
Xmin <- min(TauTemp064,TauTemp128,TauTemp256,TauTemp512)

Ymax <- max(Magnet064,Magnet128,Magnet256,Magnet512)
Ymin <- min(Magnet064,Magnet128,Magnet256,Magnet512)
Yleg <- (Ymax+Ymin)/2
#Zero-Field Susceptibility L=064
plot(TauTemp064,Magnet064,xlim=c(Xmin+P4(Xmin),Xmax+P4(Xmax)),ylim=c(Ymin+P4(Ymin),Ymax+P4(Ymax)),type='p',pch=17,col='green',main='Magnetización',xlab=c("Temperatura Reducida"),ylab=c("Magnetización"))
#,xlim=c(min(ScalledTemp064),max(ScalledTemp064))
#Zero-Field Susceptibility L=128
points(TauTemp128,Magnet128,pch=18,col='blue')
#Zero-Field Susceptibility L=256
points(TauTemp256,Magnet256,pch=19,col='red')
points(TauTemp512,Magnet512,pch=20,col='black')
#Legend
legend(x=Xmax/2,y=Yleg,col=c("green","blue","red","black"),pch=c(17,18,19,20),legend=c("L=064","L=128","L=256","L=512"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 

#Save Collapsed
dev.copy(pdf,'../Magnetization.pdf')
dev.off()


#---------------MAGNETIZATION CUADRADA----------------------------------
Magnetsqr064 <- data064$Magnetizacion2
Magnetsqr128 <- data128$Magnetizacion2
Magnetsqr256 <- data256$Magnetizacion2
Magnetsqr512 <- data512$Magnetizacion2

#Maximum and minimum Values
Xmax <- max(TauTemp064,TauTemp128,TauTemp256,TauTemp512)
Xmin <- min(TauTemp064,TauTemp128,TauTemp256,TauTemp512)

Ymax <- max(Magnetsqr064,Magnetsqr128,Magnetsqr256,Magnetsqr512)
Ymin <- min(Magnetsqr064,Magnetsqr128,Magnetsqr256,Magnetsqr512)
Yleg <- (Ymax+Ymin)/2
#Zero-Field Susceptibility L=064
plot(TauTemp064,Magnetsqr064,xlim=c(Xmin+P4(Xmin),Xmax+P4(Xmax)),ylim=c(Ymin+P4(Ymin),Ymax+P4(Ymax)),type='p',pch=17,col='green',main='Magnetización^2',xlab=c("Temperatura Reducida"),ylab=c("Magnetización^2"))
#,xlim=c(min(ScalledTemp064),max(ScalledTemp064))
#Zero-Field Susceptibility L=128
points(TauTemp128,Magnetsqr128,pch=18,col='blue')
#Zero-Field Susceptibility L=256
points(TauTemp256,Magnetsqr256,pch=19,col='red')
points(TauTemp512,Magnetsqr512,pch=20,col='black')
#Legend
legend(x=Xmax/2,y=Yleg,col=c("green","blue","red","black"),pch=c(17,18,19,20),legend=c("L=064","L=128","L=256","L=512"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 

#Save Collapsed
dev.copy(pdf,'../MagnetizationSQR.pdf')
dev.off()
