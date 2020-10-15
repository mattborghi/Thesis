require(ggplot2)

file32 <- 'L=032/OutputFile.log'
file64 <- 'L=064/OutputFile.log'
file128 <- 'L=128/OutputFile.log'
file256 <- 'L=256/OutputFile.log'
file512 <- 'L=512/OutputFile.log'

data32	<- read.csv(file32,stringsAsFactors=F,header=T,sep='')
data64	<- read.csv(file64,stringsAsFactors=F,header=T,sep='')
data128 <- read.csv(file128,stringsAsFactors=F,header=T,sep='')
data256 <- read.csv(file256,stringsAsFactors=F,header=T,sep='')
data512 <- read.csv(file512,stringsAsFactors=F,header=T,sep='')


#Names
names(data32) <- c("Alfa", "Temperatura", "Energy", "Cv", "AbsmagnetFerro", "magnetFerro", "AbssusceptFerro", "susceptFerro", "cumulantFerro", "magnetAnti", "susceptAnti","cumulantAnti")
names(data64) <- c("Alfa", "Temperatura", "Energy", "Cv", "AbsmagnetFerro", "magnetFerro", "AbssusceptFerro", "susceptFerro", "cumulantFerro", "magnetAnti", "susceptAnti","cumulantAnti")
names(data128) <- c("Alfa", "Temperatura", "Energy", "Cv", "AbsmagnetFerro", "magnetFerro", "AbssusceptFerro", "susceptFerro", "cumulantFerro", "magnetAnti", "susceptAnti","cumulantAnti")
names(data256) <- c("Alfa", "Temperatura", "Energy", "Cv", "AbsmagnetFerro", "magnetFerro", "AbssusceptFerro", "susceptFerro", "cumulantFerro", "magnetAnti", "susceptAnti","cumulantAnti")
names(data512) <- c("Alfa", "Temperatura", "Energy", "Cv", "AbsmagnetFerro", "magnetFerro", "AbssusceptFerro", "susceptFerro", "cumulantFerro", "magnetAnti", "susceptAnti","cumulantAnti")

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

dataL32 <- cbind(dataL32,size='dataL32')
dataL64 <- cbind(dataL64,size='dataL64')
dataL128 <- cbind(dataL128,size='dataL128')
dataL256 <- cbind(dataL256,size='dataL256')
dataL512 <- cbind(dataL512,size='dataL512')

#Merge all the dataframes
df <- rbind(dataL32,dataL64,dataL128,dataL256)#dataL32,dataL64,dataL128,dataL256,dataL512
p <- ggplot(df, aes(Temperatura, cumulantAnti,colour=size)) + labs(x =("Temperatura"),y = ("Cumulante U_4"))
mainplot <- p %+% subset(df, Temperatura > 0 & Temperatura<100) +
	geom_point() + geom_line()+ #ylim(0.55,0.7) +
	scale_color_manual(values=c("red", "blue", "green","cyan","brown"),labels=c("032","064","128","256","512"))#,"brown"
mainplot$labels$colour <- 'Tamaño de red'
pdf("cumulant.pdf")#,"512"
print(mainplot)
dev.off()


Tc <- 2.269
#Cumulant
plot(dataL32$Temperatura,dataL32$cumulantFerro,pch=20,type='o',col='red',lty=1,xlim=c(0.5,4),ylim=c(0,0.7))
points(dataL64$Temperatura,dataL64$cumulantFerro,pch=20,type='o',col='blue',lty=1)
points(dataL128$Temperatura,dataL128$cumulantFerro,pch=20,type='o',col='seagreen1',lty=1)
points(dataL256$Temperatura,dataL256$cumulantFerro,pch=20,type='o',col='brown',lty=1)
points(dataL512$Temperatura,dataL512$cumulantFerro,pch=20,type='o',col='cyan',lty=1)
legend(x=0.5,y=0.4,legend=c("L=032","L=064","L=128","L=256","L=512"),pch=c(20,20,20,20,20),lty=c(1,1,1,1,1),col=c("red","blue","seagreen1","brown","cyan"))
#Cumulant Colapse
L <- 32
plot( L*(dataL32$Temperatura-Tc)/Tc,dataL32$cumulantFerro,pch=20,type='o',col='red',lty=1)
L<-64
points( L*(dataL64$Temperatura-Tc)/Tc,dataL64$cumulantFerro,pch=20,type='o',col='blue',lty=1)
L<-128
points( L*(dataL128$Temperatura-Tc)/Tc,dataL128$cumulantFerro,pch=20,type='o',col='seagreen1',lty=1)
L<- 256
points( L*(dataL256$Temperatura-Tc)/Tc,dataL256$cumulantFerro,pch=20,type='o',col='brown',lty=1)
L<- 512
points( L*(dataL512$Temperatura-Tc)/Tc,dataL512$cumulantFerro,pch=20,type='o',col='cyan',lty=1)
legend(x=0.5,y=0.4,legend=c("L=032","L=064","L=128","L=256","L=512"),pch=c(20,20,20,20,20),lty=c(1,1,1,1,1),col=c("red","blue","seagreen1","brown","cyan"))

#Magnet
plot(dataL32$Temperatura,dataL32$magnetFerro,pch=20,type='o',col='red',lty=1,xlim=c(0.5,4),ylim=c(0,0.7))
points(dataL64$Temperatura,dataL64$magnetFerro,pch=20,type='o',col='blue',lty=1)
points(dataL128$Temperatura,dataL128$magnetFerro,pch=20,type='o',col='seagreen1',lty=1)
points(dataL256$Temperatura,dataL256$magnetFerro,pch=20,type='o',col='brown',lty=1)
points(dataL512$Temperatura,dataL512$magnetFerro,pch=20,type='o',col='cyan',lty=1)
legend(x=0.5,y=0.4,legend=c("L=032","L=064","L=128","L=256","L=512"),pch=c(20,20,20,20,20),lty=c(1,1,1,1,1),col=c("red","blue","seagreen1","brown","cyan"))


#MagnetCollapse
exp <- 1/8
L <- 32
plot( L*(dataL32$Temperatura-Tc)/Tc,dataL32$magnetFerro*L^exp,pch=20,type='o',col='red',lty=1,xlim=c(0.5,4),ylim=c(0,0.7))
L<- 64
points( L*(dataL64$Temperatura-Tc)/Tc,dataL64$magnetFerro*L^exp,pch=20,type='o',col='blue',lty=1)
L <-128
points( L*(dataL128$Temperatura-Tc)/Tc,dataL128$magnetFerro*L^exp,pch=20,type='o',col='seagreen1',lty=1)
L <- 256
points( L*(dataL256$Temperatura-Tc)/Tc,dataL256$magnetFerro*L^exp,pch=20,type='o',col='brown',lty=1)
L<-512
points( L*(dataL512$Temperatura-Tc)/Tc,dataL512$magnetFerro*L^exp,pch=20,type='o',col='cyan',lty=1)
legend(x=0.5,y=0.4,legend=c("L=032","L=064","L=128","L=256","L=512"),pch=c(20,20,20,20,20),lty=c(1,1,1,1,1),col=c("red","blue","seagreen1","brown","cyan"))

