#f1up vs Temp for different L's
file <- "../Data/inter 111.csv"
data <- read.csv(file,header=T,sep=",",stringsAsFactors=F)
sizeL <-20
pdf("../Graphs/segL.pdf")
dataL <- data[data$Temp == 1.3 & data$p. == 0.5 & data$N. == 0.5,]
dataLorder <- dataL[order(dataL$L),]
plot(dataLorder$Temp,dataLorder$f1up,col=palette(rainbow(length(dataLorder$L))),pch=20,xlab="Segregación",ylab="Temperatura")
legend(x=1.4,y=0.48,dataLorder$L,pch=20,col=palette(rainbow(length(dataLorder$L))))
grid(20)
dev.off()

#f1,f2,f3,f4 vs Temp for L=40 .AND. p+=0.5,0.9
#We are interested only in the temperatures where segregation is formed. That is T<Tc.
pdf("../Graphs/diffmob.pdf")
data40 <- data[data$L == sizeL & data$p. == 0.5 & data$N. == 0.5,]
plot(data40$Temp,data40$f1,ylim=c(0.0,1.0),xlim=c(0.85,2.8),pch=25,col="black",log="x",xlab="Log Temp",ylab="f1,f2,f3,f4 vs Temp",main=paste("L = ",sizeL," p+ = 0.5 .AND. 0.9"),type='o')
points(data40$Temp,data40$f2,pch=25,col="red",type='o')
points(data40$Temp,data40$f3,pch=25,col="green",type='o')
points(data40$Temp,data40$f4,pch=25,col="blue",type='o')
#now p+=0.9
data40 <- data[data$L ==sizeL & data$p. == 0.9 & data$N. == 0.5,]
points(data40$Temp,data40$f1,pch=15,col="black",type='o',lty=2)
points(data40$Temp,data40$f2,pch=15,col="red",type='o',lty=2)
points(data40$Temp,data40$f3,pch=15,col="green",type='o',lty=2)
points(data40$Temp,data40$f4,pch=15,col="blue",type='o',lty=2)
grid(20)
legend(x=1.0,y=0.4,c("f1","f2","f3","f4","f1","f2","f3","f4"),ncol=2,pch=c(25,25,25,25,15,15,15,15),col=c("black","red","green","blue","black","red","green","blue"),title=c(expression(paste(mu,"=1",mu,"=9"))))
dev.off()

#f1,f2,f3,f4 vs Temp for L=40 .AND. p+=0.5
pdf("../Graphs/diffNoMob.pdf")
data40 <- data[data$L ==sizeL & data$p. == 0.5 & data$N. == 0.5,]
plot(data40$Temp,data40$f1,ylim=c(0.0,1.0),pch=20,col="black",log="x",xlab="Log Temp",ylab="Segregación",main=paste("L = ",sizeL," p+ = 0.5"),type='o')
points(data40$Temp,data40$f2,pch=20,col="red",type='o')
points(data40$Temp,data40$f3,pch=20,col="green",type='o')
points(data40$Temp,data40$f4,pch=20,col="blue",type='o')
legend(x=20,y=1.0,c("f1","f2","f3","f4"),pch=20,col=c("black","red","green","blue")) 	
grid(20)
dev.off()

#f1,f2,f3,f4 vs Temp for L=40 .AND. p+=0.9
pdf("../Graphs/diffMob.pdf")
data40 <- data[data$L ==sizeL & data$p. == 0.5 & data$N. == 0.5,]
plot(data40$Temp,data40$f1,ylim=c(0.0,1.0),pch=20,col="black",log="x",xlab="Log Temp",ylab="Segregación",main=paste("L = ",sizeL," p+ = 0.9"),type='o')
points(data40$Temp,data40$f2,pch=20,col="red",type='o')
points(data40$Temp,data40$f3,pch=20,col="green",type='o')
points(data40$Temp,data40$f4,pch=20,col="blue",type='o')
legend(x=20,y=1.0,c("f1","f2","f3","f4"),pch=20,col=c("black","red","green","blue")) 	
grid(20)
dev.off()

#f1 vs Temp for L=40 .AND. p+=0.5,0.9
pdf("../Graphs/f1.pdf")
data40 <- data[data$L ==sizeL & data$p. == 0.5 & data$N. == 0.5,]
plot(data40$Temp,data40$f1,ylim=c(0.4,1.0),pch=20,col="black",log="x",xlab="Log Temp",ylab="f1",main=paste("L = ",sizeL," p+ = 0.5 .AND. 0.9"),type='o')
data40 <- data[data$L ==sizeL & data$p. == 0.9 & data$N. == 0.5,]
points(data40$Temp,data40$f1,pch=19,col="green",type='o')
grid(20)
dev.off()

#f1up vs Temp for L=40 .AND. p+=0.5,0.9
pdf("../Graphs/f1up.pdf")
data40 <- data[data$L ==sizeL & data$p. == 0.5 & data$N. == 0.5,]
plot(data40$Temp,data40$f1up,ylim=c(0.0,1.0),pch=20,col="black",log="x",xlab="Log Temp",ylab="f1",main=paste("L = ",sizeL," p+ = 0.5 .AND. 0.9"),type='o')
data40 <- data[data$L ==sizeL & data$p. == 0.9 & data$N. == 0.5,]
points(data40$Temp,data40$f1up,pch=19,col="green",type='o')
grid(20)
dev.off()

#f1dn vs Temp for L=40 .AND. p+=0.5,0.9
pdf("../Graphs/f1dn.pdf")
data40 <- data[data$L ==sizeL & data$p. == 0.5 & data$N. == 0.5,]
plot(data40$Temp,data40$f1dn,ylim=c(0.0,1.0),pch=20,col="black",log="x",xlab="Log Temp",ylab="f1",main=paste("L = ",sizeL," p+ = 0.5 .AND. 0.9"),type='o')
data40 <- data[data$L ==sizeL & data$p. == 0.9 & data$N. == 0.5,]
points(data40$Temp,data40$f1dn,pch=19,col="green",type='o')
grid(20)
dev.off()

#f1 vs Temp for L=20 .AND. N+=0.1,0.5,0.9 .AND. p+=0.1,0.5,0.9
pdf("../Graphs/f1Seg.pdf")
data40 <- data[data$L==sizeL & data$p. == 0.5 & data$N. == 0.5 & data$Temp < 5,]
plot(data40$Temp,data40$f1,ylim=c(0.8,1.0),pch=19,col="black",log="x",xlab="Log Temp",ylab="f1",main=paste("f1 vs Temp for L= ",sizeL,".AND. N+=0.1,0.5,0.9 .AND. p+=0.1,0.5,0.9"),type='o')
data40 <- data[data$L==sizeL & data$p. == 0.5 & data$N. == 0.1,]
points(data40$Temp,data40$f1,pch=19,col="red",type='o')
data40 <- data[data$L==sizeL & data$p. == 0.9 & data$N. == 0.1,]
points(data40$Temp,data40$f1,pch=19,col="blue",type='o')
data40 <- data[data$L==sizeL & data$p. == 0.5 & data$N. == 0.9,]
points(data40$Temp,data40$f1,pch=19,col="cyan",type='o')
data40 <- data[data$L==sizeL & data$p. == 0.9 & data$N. == 0.9,]
points(data40$Temp,data40$f1,pch=19,col="green",type='o')
grid(20)
legend(x=1.6,y=1.0,c("p+=0.5 N+=0.5","p+=0.5 N+=0.1","p+=0.9 N+=0.1","p+=0.5 N+=0.9","p+=0.9 N+=0.9"),pch=19,col=c("black","red","blue","cyan","green")) 	
dev.off()

#f1 vs Temp for L=20 .AND. N+=0.1,0.5,0.9 .AND. p+=0.1,0.5,0.9
pdf("../Graphs/f1SegNoFull.pdf")
data40 <- data[data$L==sizeL & data$p. == 0.5 & data$N. == 0.1,]
plot(data40$Temp,data40$f1,ylim=c(0.87,0.97),pch=19,col="red",log="x",xlab="Log Temp",ylab="f1",main=paste("f1 vs Temp for L= ",sizeL,".AND. N+=0.1,0.5,0.9 .AND. p+=0.1,0.5,0.9"),type='o')
data40 <- data[data$L==sizeL & data$p. == 0.9 & data$N. == 0.1,]
points(data40$Temp,data40$f1,pch=19,col="blue",type='o')
data40 <- data[data$L==sizeL & data$p. == 0.5 & data$N. == 0.9,]
points(data40$Temp,data40$f1,pch=19,col="cyan",type='o')
data40 <- data[data$L==sizeL & data$p. == 0.9 & data$N. == 0.9,]
points(data40$Temp,data40$f1,pch=19,col="green",type='o')
grid(20)
legend(x=0.9,y=0.92,c("p+=0.5 N+=0.1","p+=0.9 N+=0.1","p+=0.5 N+=0.9","p+=0.9 N+=0.9"),pch=19,col=c("red","blue","cyan","green")) 	
dev.off()

#f1 vs Temp for L=20 .AND. N+=0.5 .AND. p+=0.5 for different alpha
pdf("../Graphs/f1Alpha.pdf")
data40 <- data[data$L==sizeL & data$p. == 0.5 & data$N. == 0.5 & data$Temp<5,]
plot(data40$Temp,data40$f1,ylim=c(0.8,1.0),pch=19,col="red",log="x",xlab="Log Temp",ylab="f1",main=paste("f1 vs Temp for L= ",sizeL,".AND. N+=0.5 .AND. p+=0.5"),type='o')
file1 <- "../Data/inter 012.csv"
data2 <- read.csv(file1,header=T,sep=",",stringsAsFactors=F)
data40 <- data2[data2$L==sizeL & data2$p. == 0.5 & data2$N. == 0.5,]
points(data40$Temp,data40$f1,pch=19,col="blue",type='o')
file2 <- "../Data/inter 210.csv"
data3 <- read.csv(file2,header=T,sep=",",stringsAsFactors=F)
data40 <- data3[data3$L==sizeL & data3$p. == 0.5 & data3$N. == 0.5,]
points(data40$Temp,data40$f1,pch=19,col="cyan",type='o')
grid(20)
legend(x=1.6,y=1.0,c("alfa 1 1 1","alfa 0 1 2","alfa 2 1 0"),pch=19,col=c("red","blue","cyan")) 	
dev.off()

#f2 vs Temp for L=40 .AND. p+=0.5,0.9
pdf("../Graphs/f2.pdf")
data40 <- data[data$L ==sizeL & data$p. == 0.5 & data$N. == 0.5,]
plot(data40$Temp,data40$f2,ylim=c(0.2,1.0),pch=20,col="black",log="x",xlab="Log Temp",ylab="f2",main=paste("L = ",sizeL," p+ = 0.5 .AND. 0.9"),type='o')
data40 <- data[data$L ==sizeL & data$p. == 0.9 & data$N. == 0.5,]
points(data40$Temp,data40$f2,pch=19,col="green",type='o')
grid(20)
dev.off()

#f3 vs Temp for L=40 .AND. p+=0.5,0.9
pdf("../Graphs/f3.pdf")
data40 <- data[data$L ==sizeL & data$p. == 0.5 & data$N. == 0.5,]
plot(data40$Temp,data40$f3,ylim=c(0.0,1.0),pch=20,col="black",log="x",xlab="Log Temp",ylab="f3",main=paste("L = ",sizeL," p+ = 0.5 .AND. 0.9"),type='o')
data40 <- data[data$L ==sizeL & data$p. == 0.9 & data$N. == 0.5,]
points(data40$Temp,data40$f3,pch=19,col="green",type='o')
grid(20)
dev.off()

#f4 vs Temp for L=40 .AND. p+=0.5,0.9
pdf("../Graphs/f4.pdf")
data40 <- data[data$L ==sizeL & data$p. == 0.5 & data$N. == 0.5,]
plot(data40$Temp,data40$f4,ylim=c(0.0,1.0),pch=20,col="black",log="x",xlab="Log Temp",ylab="f4",main=paste("L = ",sizeL," p+ = 0.5 .AND. 0.9"),type='o')
data40 <- data[data$L ==sizeL & data$p. == 0.9 & data$N. == 0.5,]
points(data40$Temp,data40$f4,pch=19,col="green",type='o')
grid(20)
dev.off()

