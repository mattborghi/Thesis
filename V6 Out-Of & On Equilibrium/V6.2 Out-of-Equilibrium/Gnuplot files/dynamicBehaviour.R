file12<-"Graphs/alfa 1 1 1/L = 12 T = 1.1 p+=0.5/10M MCS/Inter20L.dat"
file24<-"Graphs/alfa 1 1 1/L = 24 T = 1.1 p+=0.5/10M MCS/Inter20L.dat"
file32<-"Graphs/alfa 1 1 1/L = 32 T = 1.1 p+=0.5/10M MCS/Inter20L.dat"
file48<-"Graphs/alfa 1 1 1/L = 48 T = 1.1 p+=0.5/10M MCS/Inter20L.dat"
file64<-"Graphs/alfa 1 1 1/L = 64 T = 1.1 p+=0.5/10M MCS/Inter20L.dat"
file128<-"Graphs/alfa 1 1 1/L = 128 T = 1.1 p+=0.5/10M MCS/Inter20L.dat"

data12 <- read.csv(file12,sep=",",stringsAsFactors=F,header=T)
data24 <- read.csv(file24,sep=",",stringsAsFactors=F,header=T)
data32 <- read.csv(file32,sep=",",stringsAsFactors=F,header=T)
data48 <- read.csv(file48,sep=",",stringsAsFactors=F,header=T)
data64 <- read.csv(file64,sep=",",stringsAsFactors=F,header=T)
data128 <- read.csv(file128,sep=",",stringsAsFactors=F,header=T)

#f1 L = 12
f1_12<-data12[,"f1up"] + data12[,"f1down"]
#f1 L = 24
f1_24<-data24[,"f1up"] + data24[,"f1down"]
#etc..
f1_32<-data32[,"f1up"] + data32[,"f1down"]
f1_48<-data48[,"f1up"] + data48[,"f1down"]
f1_64<-data64[,"f1up"] + data64[,"f1down"]
f1_128<-data128[,"f1up"] + data128[,"f1down"]

#f2 L = 12
f2_12<-data12[,"f2up"] + data12[,"f2down"]
f2_24<-data24[,"f2up"] + data24[,"f2down"]
f2_32<-data32[,"f2up"] + data32[,"f2down"]
f2_48<-data48[,"f2up"] + data48[,"f2down"]
f2_64<-data64[,"f2up"] + data64[,"f2down"]
f2_128<-data128[,"f2up"] + data128[,"f2down"]

#f3 L = 12
f3_12<-data12[,"f3up"] + data12[,"f3down"]
f3_24<-data24[,"f3up"] + data24[,"f3down"]
f3_32<-data32[,"f3up"] + data32[,"f3down"]
f3_48<-data48[,"f3up"] + data48[,"f3down"]
f3_64<-data64[,"f3up"] + data64[,"f3down"]
f3_128<-data128[,"f3up"] + data128[,"f3down"]

#f4 L = 12
f4_12<-data12[,"f4up"] + data12[,"f4down"]
f4_24<-data24[,"f4up"] + data24[,"f4down"]
f4_32<-data32[,"f4up"] + data32[,"f4down"]
f4_48<-data48[,"f4up"] + data48[,"f4down"]
f4_64<-data64[,"f4up"] + data64[,"f4down"]
f4_128<-data128[,"f4up"] + data128[,"f4down"]


#Plot f1 vs t
pdf("Graphs/Dynamic Behaviour/dynf1.pdf")
plot(log10(data12$Tiempo),log10(f1_12),pch=20,col="black",xlab="Log Tiempo",ylab="Log Segregación f1",ylim=c(log10(0.8),log10(1.0)),main="f1 vs t")
points(log10(data12$Tiempo),log10(f1_24),pch=20,col="red")
points(log10(data12$Tiempo),log10(f1_32),pch=20,col="blue")
points(log10(data12$Tiempo),log10(f1_48),pch=20,col="green")
points(log10(data12$Tiempo),log10(f1_64+0.03),pch=20,col="cyan")
points(log10(data12$Tiempo),log10(f1_128+0.03),pch=20,col="magenta")
grid(20)
legend(x=log(1),y=log(1),c("L=12","L=24","L=32","L=48","L=64","L=128"),col=c("black","red","blue","green","cyan","magenta"),pch=20)
dev.off()

#Plot f2 vs t
pdf("Graphs/Dynamic Behaviour/dynf2.pdf")
plot(log(data12$Tiempo),log(f2_12),pch=20,col="black",xlab="Log Tiempo",ylab="Log Segregación f2",ylim=c(log(0.7),log(1.0)),main="f2 vs t")
points(log(data12$Tiempo),log(f2_24),pch=20,col="red")
points(log(data12$Tiempo),log(f2_32),pch=20,col="blue")
points(log(data12$Tiempo),log(f2_48),pch=20,col="green")
points(log(data12$Tiempo),log(f2_64+0.05),pch=20,col="cyan")
points(log(data12$Tiempo),log(f2_128+0.05),pch=20,col="magenta")
grid(20)
legend(x=0,y=log(1),c("L=12","L=24","L=32","L=48","L=64","L=128"),col=c("black","red","blue","green","cyan","magenta"),pch=20)
dev.off()

#Plot f3 vs t
pdf("Graphs/Dynamic Behaviour/dynf3.pdf")
plot(log(data12$Tiempo),log(f3_12),pch=20,col="black",xlab="Log Tiempo",ylab="Log Segregación f3",ylim=c(log(0.6),log(1.0)),main="f3 vs t")
points(log(data12$Tiempo),log(f3_24),pch=20,col="red")
points(log(data12$Tiempo),log(f3_32),pch=20,col="blue")
points(log(data12$Tiempo),log(f3_48),pch=20,col="green")
points(log(data12$Tiempo),log(f3_64+0.07),pch=20,col="cyan")
points(log(data12$Tiempo),log(f3_128+0.07),pch=20,col="magenta")
grid(20)
legend(x=0,y=log(1),c("L=12","L=24","L=32","L=48","L=64","L=128"),col=c("black","red","blue","green","cyan","magenta"),pch=20)
dev.off()

#Plot f4 vs t
pdf("Graphs/Dynamic Behaviour/dynf4.pdf")
plot(log(data12$Tiempo),log(f4_12),pch=20,col="black",xlab="Log Tiempo",ylab="Log Segregación f4",ylim=c(log(0.5),log(1.0)),main="f4 vs t")
points(log(data12$Tiempo),log(f4_24),pch=20,col="red")
points(log(data12$Tiempo),log(f4_32),pch=20,col="blue")
points(log(data12$Tiempo),log(f4_48),pch=20,col="green")
points(log(data12$Tiempo),log(f4_64+0.08),pch=20,col="cyan")
points(log(data12$Tiempo),log(f4_128+0.08),pch=20,col="magenta")
grid(20)
legend(x=0,y=log(1),c("L=12","L=24","L=32","L=48","L=64","L=128"),col=c("black","red","blue","green","cyan","magenta"),pch=20)
dev.off()