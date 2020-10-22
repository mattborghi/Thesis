#file8 	<- 'L=008/OutputFile.log'
#file16 	<- 'L=016/OutputFile.log'
file32  <- 'L=032/OutputFile.log'
file64 	<- 'L=064/OutputFile.log'
file128 <- 'L=128/OutputFile.log'
file256 <- 'L=256/OutputFile.log'
file512 <- 'L=512/OutputFile.log'

#data8	<- read.csv(file8,stringsAsFactors=F,header=T,sep=',')
#data16	<- read.csv(file16,stringsAsFactors=F,header=T,sep=',')
data32	<- read.csv(file32,stringsAsFactors=F,header=F,sep='')
data64	<- read.csv(file64,stringsAsFactors=F,header=F,sep='')
data128 <- read.csv(file128,stringsAsFactors=F,header=F,sep='')
data256 <- read.csv(file256,stringsAsFactors=F,header=F,sep='')
data512 <- read.csv(file512,stringsAsFactors=F,header=F,sep='')


#Names
names(data32) <- c("Alfa", "Temperatura", "Energy", "Cv", "AbsmagnetFerro", "magnetFerro", "AbssusceptFerro", "susceptFerro", "cumulantFerro", "magnetAnti", "susceptAnti","cumulantAnti")
names(data64) <- c("Alfa", "Temperatura", "Energy", "Cv", "AbsmagnetFerro", "magnetFerro", "AbssusceptFerro", "susceptFerro", "cumulantFerro", "magnetAnti", "susceptAnti","cumulantAnti")
names(data128) <- c("Alfa", "Temperatura", "Energy", "Cv", "AbsmagnetFerro", "magnetFerro", "AbssusceptFerro", "susceptFerro", "cumulantFerro", "magnetAnti", "susceptAnti","cumulantAnti")
names(data256) <- c("Alfa", "Temperatura", "Energy", "Cv", "AbsmagnetFerro", "magnetFerro", "AbssusceptFerro", "susceptFerro", "cumulantFerro", "magnetAnti", "susceptAnti","cumulantAnti")
names(data512) <- c("Alfa", "Temperatura", "Energy", "Cv", "AbsmagnetFerro", "magnetFerro", "AbssusceptFerro", "susceptFerro", "cumulantFerro", "magnetAnti", "susceptAnti","cumulantAnti")

#Density 0.5
#datab <- data[data$posDsty==0.5,]
temps <- unique(data128$Temperatura)

#Fixed Alfa
alphaVals <- unique(data128$Alfa)
#


# ***************************************************
# ALFA = 10.0
# ***************************************************
elem <- 3

dataL8 <- data8[data8$Alfa==alphaVals[elem],]
	dataL8 <- dataL8[with(dataL8,order(Temperatura)),]
dataL16 <- data16[data16$Alfa==alphaVals[elem],]
	dataL16 <- dataL16[with(dataL16,order(Temperatura)),]
dataL32 <- data32[data32$Alfa==alphaVals[elem],]
	dataL32 <- dataL32[with(dataL32,order(Temperatura)),]
dataL64 <- data64[data64$Alfa==alphaVals[elem],]
	dataL64 <- dataL64[with(dataL64,order(Temperatura)),]
dataL128 <- data128[data128$Alfa==alphaVals[elem],]
	dataL128 <- dataL128[with(dataL128,order(Temperatura)),]
dataL256 <- data256[data256$Alfa==alphaVals[elem],]
	dataL256 <- dataL256[with(dataL256,order(Temperatura)),]
dataL512 <- data512[data512$Alfa==alphaVals[elem],]
	dataL512 <- dataL512[with(dataL512,order(Temperatura)),]#

pdf('Graphs/Alfa=10.0/Cumulant.pdf')
#plot(dataL128$Temperatura,dataL128$cumulantFerro,type='o',pch=18,lty=3,col='red',xlim=c(12.44,12.51),xlab=c("Temperatura [J/kB]"),ylab=c("Cumulante"),main=c("Cumulante vs Temp alfaud=10.0"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(dataL256$Temperatura,dataL256$cumulantFerro,type='o',pch=19,lty=4,col='blue')
#lines(dataL512$Temperatura,dataL512$cumulantFerro,type='o',pch=20,lty=5,col='green')
#lines(dataL32$Temperatura,dataL32$cumulantFerro,type='o',pch=16,lty=1,col='cyan')
#lines(dataL64$Temperatura,dataL64$cumulantFerro,type='o',pch=17,lty=2,col='magenta')
plot(dataL8$Temperatura,dataL8$cumulantFerro,type='o',pch=16,lty=1,col='purple',xlim=c(10,15))#
lines(dataL16$Temperatura,dataL16$cumulantFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=10,y=0.6,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()#

pdf('Graphs/Alfa=10.0/Susceptibility.pdf')
#plot(dataL128$Temperatura,dataL128$susceptFerro,type='o',pch=18,lty=3,col='red',xlim=c(12.2,13.0),xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad"),main=c("Susceptibilidad vs Temp alfaud=10.0"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(dataL256$Temperatura,dataL256$susceptFerro,type='o',pch=19,lty=4,col='blue')
#lines(dataL512$Temperatura,dataL512$susceptFerro,type='o',pch=20,lty=5,col='green')
#lines(dataL32$Temperatura,dataL32$susceptFerro,type='o',pch=16,lty=1,col='cyan')
#lines(dataL64$Temperatura,dataL64$susceptFerro,type='o',pch=17,lty=2,col='magenta')
plot(dataL8$Temperatura,dataL8$susceptFerro,type='o',pch=16,lty=1,col='purple',xlim=c(10,15))
lines(dataL16$Temperatura,dataL16$susceptFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=10,y=0.005,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()#

pdf('Graphs/Alfa=10.0/Magnetization.pdf')#,xlim=c(0.225,0.23)
#plot(dataL512$Temperatura,dataL512$magnetAnti,type='o',pch=20,lty=5,col='green')
#plot(dataL128$Temperatura,dataL128$magnetAnti,type='o',pch=18,lty=3,col='red',ylim=c(0,1),xlab=c("Temperatura [J/kB]"),ylab=c("Magnetización por espín"),main=c("Magnetización vs Temp alfaud=-1.2"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(dataL256$Temperatura,dataL256$magnetAnti,type='o',pch=19,lty=4,col='blue')
#lines(dataL32$Temperatura,dataL32$magnetAnti,type='o',pch=16,lty=1,col='cyan')
#lines(dataL64$Temperatura,dataL64$magnetAnti,type='o',pch=17,lty=2,col='magenta')
plot(dataL8$Temperatura,dataL8$magnetFerro,type='o',pch=16,lty=1,col='purple',xlim=c(10,14))
lines(dataL16$Temperatura,dataL16$magnetFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=10,y=0.8,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

pdf('Graphs/Alfa=10.0/CapacidadCalorifica.pdf')#,xlim=c(0.225,0.23)ylim=c(0,1)
#plot(dataL512$Temperatura,dataL512$Cv,type='o',pch=20,lty=5,col='green')
#plot(dataL128$Temperatura,dataL128$Cv*128^2,type='o',ylim=c(0.5,3.5),pch=18,lty=3,col='red',xlab=c("Temperatura [J/kB]"),ylab=c("Capacidad Calorifica"),main=c("Cap Calorifica vs Temp alfaud=-1.2"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(dataL256$Temperatura,dataL256$Cv*256^2,type='o',pch=19,lty=4,col='blue')
#lines(dataL32$Temperatura,dataL32$Cv*32^2,type='o',pch=16,lty=1,col='cyan')
#lines(dataL64$Temperatura,dataL64$Cv*64^2,type='o',pch=17,lty=2,col='magenta')
plot(dataL8$Temperatura,dataL8$Cv,type='o',pch=16,lty=1,col='purple',xlim=c(10,14))
lines(dataL16$Temperatura,dataL16$Cv,type='o',pch=17,lty=2,col='seagreen1')
legend(x=10,y=0.015,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

Tc <- 12.48
lim <- 1
pdf('Graphs/Alfa=10.0/CumulantCollapse.pdf')
#plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$cumulantFerro,type='o',pch=18,lty=3,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("Colapso Cumulante"),main=c("Colapso Cumulante vs Temp alfaud=10.0"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$cumulantFerro,type='o',pch=19,lty=4,col='blue')
#lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$cumulantFerro,type='o',pch=20,lty=5,col='green')
#lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$cumulantFerro,type='o',pch=16,lty=1,col='cyan')
#lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$cumulantFerro,type='o',pch=17,lty=2,col='magenta')
plot(8*(dataL8$Temperatura-Tc)/Tc,dataL8$cumulantFerro,type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim))
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$cumulantFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=-1,y=0.6,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()#


#Magnetization Collapse
pdf('Graphs/Alfa=10.0/MagnetizationCollapse.pdf')
#plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$magnetAnti*(128)^(1/8),type='o',pch=18,lty=3,col='red',xlim=c(-10,10),xlab=c("L*(T-Tc)/Tc"),ylab=c("m*L^(1/8)"),main=c("Magnetización colapsada alfaud=-1.2"),sub=c("Tc=0.2267"))
#lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$magnetAnti*(512)^(1/8),type='o',pch=20,lty=5,col='green')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$magnetAnti*(256)^(1/8),type='o',pch=19,lty=4,col='blue')
#lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$magnetAnti*(32)^(1/8),type='o',pch=16,lty=1,col='cyan')
#lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$magnetAnti*(64)^(1/8),type='o',pch=17,lty=2,col='magenta')
plot(8*(dataL8$Temperatura-Tc)/Tc,dataL8$cumulantFerro,type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim))
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$cumulantFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=-1,y=0.6,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

#Susceptibilidad Collapse
exp <- 7/4
pdf('Graphs/Alfa=10.0/SusceptibilityCollapse.pdf')
#plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$susceptAnti*128^(-exp),type='o',pch=18,lty=3,col='red',xlim=c(-10,10),xlab=c("L*(T-Tc)/Tc"),ylab=c("Susceptibilidad*L^(-7/4)"),main=c("Susceptibilidad colapsada alfaud=-1.2"),sub=c("Tc=0.2267"))
#lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$susceptAnti*512^(-exp),type='o',pch=20,lty=5,col='green')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$susceptAnti*256^(-exp),type='o',pch=19,lty=4,col='blue')
#lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$susceptAnti*32^(-exp),type='o',pch=16,lty=1,col='cyan')
#lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$susceptAnti*64^(-exp),type='o',pch=17,lty=2,col='magenta')
plot(8*(dataL8$Temperatura-Tc)/Tc,dataL8$susceptFerro*8^(-exp),type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim))
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$susceptFerro*16^(-exp),type='o',pch=17,lty=2,col='seagreen1')
legend(x=-1,y=0.6,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

#Cap calorifica Collapse
exte <- 1.5
pdf('Graphs/Alfa=10.0/CapCalorificaCollapse.pdf')
#plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$Cv*128^(exte),ylim=c(0,0.4),type='o',pch=18,lty=3,col='red',xlim=c(-10,10),xlab=c("L*(T-Tc)/Tc"),ylab=c("Cap Calorifica"),main=c("Cap Calorifica colapsada alfaud=-1.2"),sub=c("Tc=0.2267"))
#lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$Cv*512^(exte),type='o',pch=20,lty=5,col='green')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$Cv*256^(exte),type='o',pch=19,lty=4,col='blue')
#lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$Cv*32^(exte),type='o',pch=16,lty=1,col='cyan')
#lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$Cv*64^(exte),type='o',pch=17,lty=2,col='magenta')
plot(8*(dataL8$Temperatura-Tc)/Tc,dataL8$cumulantFerro,type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim))
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$cumulantFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=-1,y=0.6,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()


# ***************************************************
# ALFA = -0.8
# ***************************************************

elem<- 6

dataL8 <- data8[data8$Alfa==alphaVals[elem],]
	dataL8 <- dataL8[with(dataL8,order(Temperatura)),]
dataL16 <- data16[data16$Alfa==alphaVals[elem],]
	dataL16 <- dataL16[with(dataL16,order(Temperatura)),]
dataL32 <- data32[data32$Alfa==alphaVals[8],]
	dataL32 <- dataL32[with(dataL32,order(Temperatura)),]
dataL64 <- data64[data64$Alfa==alphaVals[8],]
	dataL64 <- dataL64[with(dataL64,order(Temperatura)),]
dataL128 <- data128[data128$Alfa==alphaVals[8],]
	dataL128 <- dataL128[with(dataL128,order(Temperatura)),]
dataL256 <- data256[data256$Alfa==alphaVals[8],]
	dataL256 <- dataL256[with(dataL256,order(Temperatura)),]
dataL512 <- data512[data512$Alfa==alphaVals[8],]
	dataL512 <- dataL512[with(dataL512,order(Temperatura)),]#

pdf('Graphs/Alfa=-0.8/Cumulant.pdf')
plot(dataL128$Temperatura,dataL128$cumulantFerro,type='o',pch=18,lty=3,col='red',xlim=c(0.225,0.23),ylim=c(0.5,0.7),xlab=c("Temperatura [J/kB]"),ylab=c("Cumulante"),main=c("Cumulante vs Temp alfaud=-0.8"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$cumulantFerro,type='o',pch=19,lty=4,col='blue')
lines(dataL512$Temperatura,dataL512$cumulantFerro,type='o',pch=20,lty=5,col='green')
lines(dataL32$Temperatura,dataL32$cumulantFerro,type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$cumulantFerro,type='o',pch=17,lty=2,col='magenta')
legend(x=0.225,y=0.6,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()#

pdf('Graphs/Alfa=-0.8/Susceptibility.pdf')
plot(dataL512$Temperatura,dataL512$susceptFerro*(512^2)/(10^3),type='o',pch=20,lty=5,col='green',xlim=c(0.225,0.23),xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad (L^2/10^3)"),main=c("Susceptibilidad vs Temp alfaud=-0.8"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL128$Temperatura,dataL128$susceptFerro*(128^2)/(10^3),type='o',pch=18,lty=3,col='red')
lines(dataL256$Temperatura,dataL256$susceptFerro*(256^2)/(10^3),type='o',pch=19,lty=4,col='blue')
lines(dataL32$Temperatura,dataL32$susceptFerro*(32^2)/(10^3),type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$susceptFerro*(64^2)/(10^3),type='o',pch=17,lty=2,col='magenta')
legend(x=0.225,y=15,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()#

#Checking Tc with FSS Tc=0.22755
Tc <- 0.22755
pdf("Graphs/Alfa=-0.8/CumulantCollapse.pdf")
plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$cumulantFerro,type='p',pch=18,lty=3,col='red',xlim=c(-10,10),xlab=c("L*(T-Tc)/Tc"),ylab=c("Cumulante U4"),main=c("Colapso U4 para alfa alfa=-0.8"),sub=c("Tc=0.2755"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$cumulantFerro,type='p',pch=16,lty=1,col='cyan')
lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$cumulantFerro,type='p',pch=19,lty=4,col='blue')
lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$cumulantFerro,type='p',pch=20,lty=5,col='green')
lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$cumulantFerro,type='p',pch=17,lty=2,col='magenta')
legend(x=-10,y=0.3,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()


# ***************************************************
# ALFA = -1.2
# ***************************************************
elem <- 5
rep <- 2

dataL8 <- data8[data8$Alfa==alphaVals[elem],]
	dataL8 <- dataL8[with(dataL8,order(Temperatura)),]
dataL16 <- data16[data16$Alfa==alphaVals[elem],]
	dataL16 <- dataL16[with(dataL16,order(Temperatura)),]
dataL32 <- data32[data32$Alfa==alphaVals[elem],]
	dataL32 <- dataL32[with(dataL32,order(Temperatura)),]
dataL64 <- data64[data64$Alfa==alphaVals[elem],]
	dataL64 <- dataL64[with(dataL64,order(Temperatura)),]
dataL128 <- data128[data128$Alfa==alphaVals[elem],]
	dataL128 <- dataL128[with(dataL128,order(Temperatura)),]
dataL256 <- data256[data256$Alfa==alphaVals[elem],]
	dataL256 <- dataL256[with(dataL256,order(Temperatura)),]
dataL512 <- data512[data512$Alfa==alphaVals[elem],]
	dataL512 <- dataL512[with(dataL512,order(Temperatura)),]

pdf('Graphs/Alfa=-1.2/Cumulant.pdf')
plot(dataL128$Temperatura,dataL128$cumulantAnti,type='o',pch=18,lty=3,col='red',ylim=c(0.1,0.7),xlim=c(0.225,0.23),xlab=c("Temperatura [J/kB]"),ylab=c("Cumulante"),main=c("Cumulante vs Temp alfaud=-1.2"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$cumulantAnti,type='o',pch=19,lty=4,col='blue')
#lines(dataL512$Temperatura,dataL512$cumulantAnti,type='o',pch=20,lty=5,col='green')
lines(dataL32$Temperatura,dataL32$cumulantAnti,type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$cumulantAnti,type='o',pch=17,lty=2,col='magenta')
lines(dataL8$Temperatura,dataL8$cumulantAnti,type='o',pch=16,lty=1,col='purple')
lines(dataL16$Temperatura,dataL16$cumulantAnti,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

pdf('Graphs/Alfa=-1.2/Susceptibility.pdf')#xlim=c(0.225,0.23)
#plot(dataL512$Temperatura,dataL512$susceptAnti,type='o',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad (L^2/10^3)"),main=c("Susceptibilidad vs Temp alfaud=-1.2"))
plot(dataL256$Temperatura,dataL256$susceptAnti,type='o',pch=c(19,rep(NA, rep)),lty=4,col='blue',xlab=c("Temperatura [J/kB]"),ylim=c(0,0.22),ylab=c("Susceptibilidad"),main=c("Susceptibilidad vs Temp alfaud=-1.2"))
lines(dataL128$Temperatura,dataL128$susceptAnti,type='o',pch=c(18,rep(NA, rep)),lty=3,col='red')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL32$Temperatura,dataL32$susceptAnti,type='o',pch=c(16,rep(NA, rep)),lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$susceptAnti,type='o',pch=c(17,rep(NA, rep)),lty=2,col='magenta')
lines(dataL8$Temperatura,dataL8$susceptAnti,type='o',pch=16,lty=1,col='purple')
lines(dataL16$Temperatura,dataL16$susceptAnti,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

pdf('Graphs/Alfa=-1.2/Magnetization.pdf')#,xlim=c(0.225,0.23)
#plot(dataL512$Temperatura,dataL512$magnetAnti,type='o',pch=20,lty=5,col='green')
plot(dataL128$Temperatura,dataL128$magnetAnti,type='o',pch=18,lty=3,col='red',ylim=c(0,1),xlab=c("Temperatura [J/kB]"),ylab=c("Magnetización por espín"),main=c("Magnetización vs Temp alfaud=-1.2"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$magnetAnti,type='o',pch=19,lty=4,col='blue')
lines(dataL32$Temperatura,dataL32$magnetAnti,type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$magnetAnti,type='o',pch=17,lty=2,col='magenta')
lines(dataL8$Temperatura,dataL8$magnetAnti,type='o',pch=16,lty=1,col='purple')
lines(dataL16$Temperatura,dataL16$magnetAnti,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

pdf('Graphs/Alfa=-1.2/CapacidadCalorifica.pdf')#,xlim=c(0.225,0.23)ylim=c(0,1)
#plot(dataL512$Temperatura,dataL512$Cv,type='o',pch=20,lty=5,col='green')
plot(dataL128$Temperatura,dataL128$Cv*128^2,type='o',ylim=c(0.5,3.5),pch=18,lty=3,col='red',xlab=c("Temperatura [J/kB]"),ylab=c("Capacidad Calorifica"),main=c("Cap Calorifica vs Temp alfaud=-1.2"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$Cv*256^2,type='o',pch=19,lty=4,col='blue')
lines(dataL32$Temperatura,dataL32$Cv*32^2,type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$Cv*64^2,type='o',pch=17,lty=2,col='magenta')
lines(dataL8$Temperatura,dataL8$Cv,type='o',pch=16,lty=1,col='purple')
lines(dataL16$Temperatura,dataL16$Cv,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()


#Checking Tc with FSS Tc=0.22755
Tc <- 0.2267
lim <- 10
pdf("Graphs/Alfa=-1.2/CumulantCollapse.pdf")
plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$cumulantAnti,type='p',pch=18,col='red',xlim=c(-lim,lim),xlab=c("L*(T-Tc)/Tc"),ylab=c("Cumulante U4"),main=c("Colapso U4 para alfa alfa=-1.2"),sub=c("Tc=0.2267"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$cumulantAnti,type='p',pch=16,col='cyan')
lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$cumulantAnti,type='p',pch=19,col='blue')
lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$cumulantAnti,type='p',pch=20,col='green')
lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$cumulantAnti,type='p',pch=17,col='magenta')
lines(8*(dataL8$Temperatura-Tc)/Tc,dataL8$cumulantAnti,type='o',pch=16,lty=1,col='purple')
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$cumulantAnti,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

#Magnetization Collapse
pdf('Graphs/Alfa=-1.2/MagnetizationCollapse.pdf')
plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$magnetAnti*(128)^(1/8),type='o',pch=18,lty=3,col='red',xlim=c(-lim,lim),xlab=c("L*(T-Tc)/Tc"),ylab=c("m*L^(1/8)"),main=c("Magnetización colapsada alfaud=-1.2"),sub=c("Tc=0.2267"))
lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$magnetAnti*(512)^(1/8),type='o',pch=20,lty=5,col='green')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$magnetAnti*(256)^(1/8),type='o',pch=19,lty=4,col='blue')
lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$magnetAnti*(32)^(1/8),type='o',pch=16,lty=1,col='cyan')
lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$magnetAnti*(64)^(1/8),type='o',pch=17,lty=2,col='magenta')
lines(8*(dataL8$Temperatura-Tc)/Tc,dataL8$magnetAnti*8^(1/8),type='o',pch=16,lty=1,col='purple')
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$magnetAnti*16^(1/8),type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

#Susceptibilidad Collapse
exp <- 7/4
pdf('Graphs/Alfa=-1.2/SusceptibilityCollapse.pdf')
plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$susceptAnti*128^(-exp),type='o',pch=18,lty=3,col='red',xlim=c(-lim,lim),xlab=c("L*(T-Tc)/Tc"),ylab=c("Susceptibilidad*L^(-7/4)"),main=c("Susceptibilidad colapsada alfaud=-1.2"),sub=c("Tc=0.2267"))
lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$susceptAnti*512^(-exp),type='o',pch=20,lty=5,col='green')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$susceptAnti*256^(-exp),type='o',pch=19,lty=4,col='blue')
lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$susceptAnti*32^(-exp),type='o',pch=16,lty=1,col='cyan')
lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$susceptAnti*64^(-exp),type='o',pch=17,lty=2,col='magenta')
lines(8*(dataL8$Temperatura-Tc)/Tc,dataL8$susceptAnti*8^(-exp),type='o',pch=16,lty=1,col='purple')
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$susceptAnti*16^(-exp),type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

#Cap calorifica Collapse
exte <- 1.5
pdf('Graphs/Alfa=-1.2/CapCalorificaCollapse.pdf')
plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$Cv*128^(exte),ylim=c(0,0.4),type='o',pch=18,lty=3,col='red',xlim=c(-10,10),xlab=c("L*(T-Tc)/Tc"),ylab=c("Cap Calorifica"),main=c("Cap Calorifica colapsada alfaud=-1.2"),sub=c("Tc=0.2267"))
lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$Cv*512^(exte),type='o',pch=20,lty=5,col='green')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$Cv*256^(exte),type='o',pch=19,lty=4,col='blue')
lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$Cv*32^(exte),type='o',pch=16,lty=1,col='cyan')
lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$Cv*64^(exte),type='o',pch=17,lty=2,col='magenta')
lines(8*(dataL8$Temperatura-Tc)/Tc,dataL8$Cv*8^(exte),type='o',pch=16,lty=1,col='purple')
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$Cv*16^(exte),type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()


# ***************************************************
# ALFA = -1.5
# ***************************************************

elem <-7

dataL8 <- data8[data8$Alfa==alphaVals[elem],]
	dataL8 <- dataL8[with(dataL8,order(Temperatura)),]
dataL16 <- data16[data16$Alfa==alphaVals[elem],]
	dataL16 <- dataL16[with(dataL16,order(Temperatura)),]
dataL32 <- data32[data32$Alfa==alphaVals[3],]
	dataL32 <- dataL32[with(dataL32,order(Temperatura)),]
dataL64 <- data64[data64$Alfa==alphaVals[3],]
	dataL64 <- dataL64[with(dataL64,order(Temperatura)),]
dataL128 <- data128[data128$Alfa==alphaVals[3],]
	dataL128 <- dataL128[with(dataL128,order(Temperatura)),]
dataL256 <- data256[data256$Alfa==alphaVals[3],]
	dataL256 <- dataL256[with(dataL256,order(Temperatura)),]
dataL512 <- data512[data512$Alfa==alphaVals[3],]
	dataL512 <- dataL512[with(dataL512,order(Temperatura)),]#

pdf('Graphs/Alfa=-1.5/Cumulant.pdf')
plot(dataL128$Temperatura,dataL128$cumulantAnti,type='o',pch=18,xlim=c(0.54,0.6),col='red',xlab=c("Temperatura [J/kB]"),ylab=c("Cumulante"),main=c("Cumulante vs Temp alfaud=-1.5"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$cumulantAnti,type='o',pch=19,col='blue')
lines(dataL512$Temperatura,dataL512$cumulantAnti,type='o',pch=20,col='green')
lines(dataL32$Temperatura,dataL32$cumulantAnti,type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$cumulantAnti,type='o',pch=17,lty=2,col='magenta')
legend(x=0.55,y=0.3,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()#

pdf('Graphs/Alfa=-1.5/Susceptibility.pdf')
plot(dataL512$Temperatura,dataL512$susceptAnti*(512^2)/(10^3),xlim=c(0.55,0.7),ylim=c(0,0.5),type='o',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad (L^2/10^3)"),main=c("Susceptibilidad vs Temp alfaud=-1.5"))
lines(dataL128$Temperatura,dataL128$susceptAnti*(128^2)/(10^3),type='o',pch=18,lty=3,col='red')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$susceptAnti*(256^2)/(10^3),type='o',pch=19,lty=4,col='blue')
lines(dataL32$Temperatura,dataL32$susceptAnti*(32^2)/(10^3),type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$susceptAnti*(64^2)/(10^3),type='o',pch=17,lty=2,col='magenta')
legend(x=0.65,y=0.3,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()#

#Checking Tc with FSS Tc=0.22755
Tc <- 0.57
pdf("Graphs/Alfa=-1.5/CumulantCollapse.pdf")
plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$cumulantAnti,type='p',pch=18,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("Cumulante U4"),main=c("Colapso U4 para alfa alfa=-1.5"),sub=c("Tc=0.2755"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$cumulantAnti,type='p',pch=16,col='cyan')
lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$cumulantAnti,type='p',pch=19,col='blue')
lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$cumulantAnti,type='p',pch=20,col='green')
lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$cumulantAnti,type='p',pch=17,col='magenta')
legend(x=30,y=0.3,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()#



# ***************************************************
# ALFA = -0.5
# ***************************************************

elem<-8

dataL8 <- data8[data8$Alfa==alphaVals[elem],]
	dataL8 <- dataL8[with(dataL8,order(Temperatura)),]
dataL16 <- data16[data16$Alfa==alphaVals[elem],]
	dataL16 <- dataL16[with(dataL16,order(Temperatura)),]
dataL32 <- data32[data32$Alfa==alphaVals[4],]
	dataL32 <- dataL32[with(dataL32,order(Temperatura)),]
dataL64 <- data64[data64$Alfa==alphaVals[4],]
	dataL64 <- dataL64[with(dataL64,order(Temperatura)),]
dataL128 <- data128[data128$Alfa==alphaVals[4],]
	dataL128 <- dataL128[with(dataL128,order(Temperatura)),]
dataL256 <- data256[data256$Alfa==alphaVals[4],]
	dataL256 <- dataL256[with(dataL256,order(Temperatura)),]
dataL512 <- data512[data512$Alfa==alphaVals[4],]
	dataL512 <- dataL512[with(dataL512,order(Temperatura)),]#

pdf('Graphs/Alfa=-0.5/Cumulant.pdf')
plot(dataL128$Temperatura,dataL128$cumulantFerro,type='o',pch=18,col='red',xlim=c(0.5,0.7),xlab=c("Temperatura [J/kB]"),ylab=c("Cumulante"),main=c("Cumulante vs Temp alfaud=-0.5"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$cumulantFerro,type='o',pch=19,col='blue')
lines(dataL512$Temperatura,dataL512$cumulantFerro,type='o',pch=20,col='green')
lines(dataL32$Temperatura,dataL32$cumulantFerro,type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$cumulantFerro,type='o',pch=17,lty=2,col='magenta')
legend(x=0.5,y=0.3,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()#

pdf('Graphs/Alfa=-0.5/Susceptibility.pdf')
plot(dataL512$Temperatura,dataL512$susceptFerro*(512^2)/(10^3),xlim=c(0.5,0.7),ylim=c(0,0.5),type='o',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad (L^2/10^3)"),main=c("Susceptibilidad vs Temp alfaud=-0.5"))
lines(dataL128$Temperatura,dataL128$susceptFerro*(128^2)/(10^3),type='o',pch=18,lty=3,col='red')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$susceptFerro*(256^2)/(10^3),type='o',pch=19,lty=4,col='blue')
lines(dataL32$Temperatura,dataL32$susceptFerro*(32^2)/(10^3),type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$susceptFerro*(64^2)/(10^3),type='o',pch=17,lty=2,col='magenta')
legend(x=0.65,y=0.3,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()#

Tc <- 0.58
pdf('Graphs/Alfa=-0.5/CumulantCollapse.pdf')
plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$cumulantFerro,type='p',pch=18,col='red',xlim=c(-10,10),xlab=c("L*(T-Tc)/Tc"),ylab=c("Colapso Cumulante"),main=c("Colapso Cumulante vs Temp alfaud=-0.5"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$cumulantFerro,type='p',pch=19,col='blue')
lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$cumulantFerro,type='p',pch=20,col='green')
lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$cumulantFerro,type='p',pch=16,col='cyan')
lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$cumulantFerro,type='p',pch=17,col='magenta')
legend(x=5,y=0.5,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()#



# ***************************************************
# ALFA = 0.0
# ***************************************************

elem <-9

dataL8 <- data8[data8$Alfa==alphaVals[elem],]
	dataL8 <- dataL8[with(dataL8,order(Temperatura)),]
dataL16 <- data16[data16$Alfa==alphaVals[elem],]
	dataL16 <- dataL16[with(dataL16,order(Temperatura)),]
dataL32 <- data32[data32$Alfa==alphaVals[5],]
	dataL32 <- dataL32[with(dataL32,order(Temperatura)),]
dataL64 <- data64[data64$Alfa==alphaVals[5],]
	dataL64 <- dataL64[with(dataL64,order(Temperatura)),]
dataL128 <- data128[data128$Alfa==alphaVals[5],]
	dataL128 <- dataL128[with(dataL128,order(Temperatura)),]
dataL256 <- data256[data256$Alfa==alphaVals[5],]
	dataL256 <- dataL256[with(dataL256,order(Temperatura)),]
dataL512 <- data512[data512$Alfa==alphaVals[5],]
	dataL512 <- dataL512[with(dataL512,order(Temperatura)),]#

pdf('Graphs/Alfa=0.0/Cumulant.pdf')
plot(dataL128$Temperatura,dataL128$cumulantFerro,type='o',pch=18,col='red',xlim=c(1,1.3),xlab=c("Temperatura [J/kB]"),ylab=c("Cumulante"),main=c("Cumulante vs Temp alfaud=0.0"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$cumulantFerro,type='o',pch=19,col='blue')
lines(dataL512$Temperatura,dataL512$cumulantFerro,type='o',pch=20,col='green')
lines(dataL32$Temperatura,dataL32$cumulantFerro,type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$cumulantFerro,type='o',pch=17,lty=2,col='magenta')
legend(x=1.25,y=0.6,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()#

pdf('Graphs/Alfa=0.0/Susceptibility.pdf')
plot(dataL512$Temperatura,dataL512$susceptFerro*(512^2)/(10^3),xlim=c(1.1,1.3),ylim=c(0,0.4),type='o',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad (L^2/10^3)"),main=c("Susceptibilidad vs Temp alfaud=0.0"))
lines(dataL128$Temperatura,dataL128$susceptFerro*(128^2)/(10^3),type='o',pch=18,lty=3,col='red')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$susceptFerro*(256^2)/(10^3),type='o',pch=19,lty=4,col='blue')
lines(dataL32$Temperatura,dataL32$susceptFerro*(32^2)/(10^3),type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$susceptFerro*(64^2)/(10^3),type='o',pch=17,lty=2,col='magenta')
legend(x=1.2,y=0.3,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()#

Tc <- 1.13
pdf('Graphs/Alfa=0.0/CumulantCollapse.pdf')
plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$cumulantFerro,type='p',pch=18,col='red',xlim=c(-10,10),xlab=c("L*(T-Tc)/Tc"),ylab=c("Colapso Cumulante"),main=c("Colapso Cumulante vs Temp alfaud=0.0"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$cumulantFerro,type='p',pch=19,col='blue')
lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$cumulantFerro,type='p',pch=20,col='green')
lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$cumulantFerro,type='p',pch=16,col='cyan')
lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$cumulantFerro,type='p',pch=17,col='magenta')
legend(x=5,y=0.5,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()#
#


# ***************************************************
# ALFA = 0.5
# ***************************************************

elem <- 10

dataL8 <- data8[data8$Alfa==alphaVals[elem],]
	dataL8 <- dataL8[with(dataL8,order(Temperatura)),]
dataL16 <- data16[data16$Alfa==alphaVals[elem],]
	dataL16 <- dataL16[with(dataL16,order(Temperatura)),]
dataL32 <- data32[data32$Alfa==alphaVals[elem],]
	dataL32 <- dataL32[with(dataL32,order(Temperatura)),]
dataL64 <- data64[data64$Alfa==alphaVals[elem],]
	dataL64 <- dataL64[with(dataL64,order(Temperatura)),]
dataL128 <- data128[data128$Alfa==alphaVals[elem],]
	dataL128 <- dataL128[with(dataL128,order(Temperatura)),]
dataL256 <- data256[data256$Alfa==alphaVals[elem],]
	dataL256 <- dataL256[with(dataL256,order(Temperatura)),]
dataL512 <- data512[data512$Alfa==alphaVals[elem],]
	dataL512 <- dataL512[with(dataL512,order(Temperatura)),]#

pdf('Graphs/Alfa=0.5/Cumulant.pdf')
plot(dataL128$Temperatura,dataL128$cumulantFerro,type='o',pch=18,col='red',xlab=c("Temperatura [J/kB]"),ylab=c("Cumulante"),main=c("Cumulante vs Temp alfaud=0.5"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$cumulantFerro,type='o',pch=19,col='blue')
lines(dataL512$Temperatura,dataL512$cumulantFerro,type='o',pch=20,col='green')
lines(dataL32$Temperatura,dataL32$cumulantFerro,type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$cumulantFerro,type='o',pch=17,lty=2,col='magenta')
lines(dataL8$Temperatura,dataL8$cumulantFerro,type='o',pch=16,lty=1,col='purple')
lines(dataL16$Temperatura,dataL16$cumulantFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=1,y=0.4,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()#

pdf('Graphs/Alfa=0.5/Susceptibility.pdf')
plot(dataL512$Temperatura,dataL512$susceptFerro,type='o',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad (L^2/10^3)"),main=c("Susceptibilidad vs Temp alfaud=0.5"))
lines(dataL128$Temperatura,dataL128$susceptFerro*(128^2)/(10^3),type='o',pch=18,lty=3,col='red')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$susceptFerro*(256^2)/(10^3),type='o',pch=19,lty=4,col='blue')
lines(dataL32$Temperatura,dataL32$susceptFerro*(32^2)/(10^3),type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$susceptFerro*(64^2)/(10^3),type='o',pch=17,lty=2,col='magenta')
plot(dataL8$Temperatura,dataL8$susceptFerro,type='o',pch=16,lty=1,col='purple')
lines(dataL16$Temperatura,dataL16$susceptFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()#

Tc <- 1.7
pdf('Graphs/Alfa=0.5/CumulantCollapse.pdf')
plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$cumulantFerro,type='p',pch=18,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("Colapso Cumulante"),main=c("Colapso Cumulante vs Temp alfaud=0.5"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$cumulantFerro,type='p',pch=19,col='blue')
lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$cumulantFerro,type='p',pch=20,col='green')
lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$cumulantFerro,type='p',pch=16,col='cyan')
lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$cumulantFerro,type='p',pch=17,col='magenta')
lines(8*(dataL8$Temperatura-Tc)/Tc,dataL8$cumulantFerro,type='o',pch=16,lty=1,col='purple')
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$cumulantFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=-50,y=0,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()#


# ***************************************************
# ALFA = -3.0
# ***************************************************
elem <- 2
dataL8 <- data8[data8$Alfa==alphaVals[elem],]
	dataL8 <- dataL8[with(dataL8,order(Temperatura)),]
dataL16 <- data16[data16$Alfa==alphaVals[elem],]
	dataL16 <- dataL16[with(dataL16,order(Temperatura)),]
dataL32 <- data32[data32$Alfa==alphaVals[elem],]
	dataL32 <- dataL32[with(dataL32,order(Temperatura)),]
dataL64 <- data64[data64$Alfa==alphaVals[elem],]
	dataL64 <- dataL64[with(dataL64,order(Temperatura)),]
dataL128 <- data128[data128$Alfa==alphaVals[elem],]
	dataL128 <- dataL128[with(dataL128,order(Temperatura)),]
dataL256 <- data256[data256$Alfa==alphaVals[elem],]
	dataL256 <- dataL256[with(dataL256,order(Temperatura)),]
dataL512 <- data512[data512$Alfa==alphaVals[elem],]
	dataL512 <- dataL512[with(dataL512,order(Temperatura)),]#

pdf('Graphs/Alfa=-3.0/Cumulant.pdf')
#plot(dataL128$Temperatura,dataL128$cumulantAnti,type='o',pch=18,col='red',xlim=c(2.2,2.3),xlab=c("Temperatura [J/kB]"),ylab=c("Cumulante"),main=c("Cumulante vs Temp alfaud=-3.0"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(dataL256$Temperatura,dataL256$cumulantAnti,type='o',pch=19,col='blue')
#lines(dataL512$Temperatura,dataL512$cumulantAnti,type='o',pch=20,col='green')
#lines(dataL32$Temperatura,dataL32$cumulantAnti,type='o',pch=16,lty=1,col='cyan')
#lines(dataL64$Temperatura,dataL64$cumulantAnti,type='o',pch=17,lty=2,col='magenta')
plot(dataL8$Temperatura,dataL8$cumulantAnti,type='o',pch=16,lty=1,col='purple')
lines(dataL16$Temperatura,dataL16$cumulantAnti,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()#

pdf('Graphs/Alfa=-3.0/Susceptibility.pdf')
#plot(dataL512$Temperatura,dataL512$susceptAnti*(512^2)/(10^3),xlim=c(2.2,2.3),type='o',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad (L^2/10^3)"),main=c("Susceptibilidad vs Temp alfaud=-3.0"))
#lines(dataL128$Temperatura,dataL128$susceptAnti*(128^2)/(10^3),type='o',pch=18,lty=3,col='red')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(dataL256$Temperatura,dataL256$susceptAnti*(256^2)/(10^3),type='o',pch=19,lty=4,col='blue')
#lines(dataL32$Temperatura,dataL32$susceptAnti*(32^2)/(10^3),type='o',pch=16,lty=1,col='cyan')
#lines(dataL64$Temperatura,dataL64$susceptAnti*(64^2)/(10^3),type='o',pch=17,lty=2,col='magenta')
plot(dataL8$Temperatura,dataL8$susceptAnti,type='o',pch=16,lty=1,col='purple')
lines(dataL16$Temperatura,dataL16$susceptAnti,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()#

Tc <- 2.269
pdf('Graphs/Alfa=-3.0/CumulantCollapse.pdf')
plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$cumulantAnti,type='p',pch=18,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("Colapso Cumulante"),main=c("Colapso Cumulante vs Temp alfaud=-3.0"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$cumulantAnti,type='p',pch=19,col='blue')
lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$cumulantAnti,type='p',pch=20,col='green')
lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$cumulantAnti,type='p',pch=16,col='cyan')
lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$cumulantAnti,type='p',pch=17,col='magenta')
plot(8*(dataL8$Temperatura-Tc)/Tc,dataL8$Cv*8^(exte),type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim))
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$Cv*16^(exte),type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()#

#Magnetization Collapse
pdf('Graphs/Alfa=-3.0/MagnetizationCollapse.pdf')
plot(512*(dataL512$Temperatura-Tc)/Tc,dataL512$magnetAnti*(512)^(-1/8),type='o',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Magnetización Colapsada"),main=c("Magnetización vs Temp alfaud=-3.0"))
lines(128*(dataL128$Temperatura-Tc)/Tc,dataL128$magnetAnti*(128)^(-1/8),type='o',pch=18,lty=3,col='red')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$magnetAnti*(256)^(-1/8),type='o',pch=19,lty=4,col='blue')
lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$magnetAnti*(32)^(-1/8),type='o',pch=16,lty=1,col='cyan')
lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$magnetAnti*(64)^(-1/8),type='o',pch=17,lty=2,col='magenta')
plot(8*(dataL8$Temperatura-Tc)/Tc,dataL8$Cv*8^(exte),type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim))
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$Cv*16^(exte),type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

#Susceptibilidad Collapse
pdf('Graphs/Alfa=-3.0/SusceptibilityCollapse.pdf')
plot(512*(dataL512$Temperatura-Tc)/Tc,dataL512$susceptAnti*512^(7/4),type='o',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad Colapsada"),main=c("Susceptibilidad vs Temp alfaud=-3.0"))
lines(128*(dataL128$Temperatura-Tc)/Tc,dataL128$susceptAnti*128^(7/4),type='o',pch=18,lty=3,col='red')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$susceptAnti*256^(7/4),type='o',pch=19,lty=4,col='blue')
lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$susceptAnti*32^(7/4),type='o',pch=16,lty=1,col='cyan')
lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$susceptAnti*64^(7/4),type='o',pch=17,lty=2,col='magenta')
plot(8*(dataL8$Temperatura-Tc)/Tc,dataL8$Cv*8^(exte),type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim))
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$Cv*16^(exte),type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()



# ***************************************************
# ALFA = 1 -> ISING USUAL
# ***************************************************
elem <- 1	
rep <- 2

dataL8 <- data8[data8$Alfa==alphaVals[elem],]
	dataL8 <- dataL8[with(dataL8,order(Temperatura)),]
dataL16 <- data16[data16$Alfa==alphaVals[elem],]
	dataL16 <- dataL16[with(dataL16,order(Temperatura)),]
dataL32 <- data32[data32$Alfa==alphaVals[elem],]
	dataL32 <- dataL32[with(dataL32,order(Temperatura)),]
dataL64 <- data64[data64$Alfa==alphaVals[elem],]
	dataL64 <- dataL64[with(dataL64,order(Temperatura)),]
dataL128 <- data128[data128$Alfa==alphaVals[elem],]
	dataL128 <- dataL128[with(dataL128,order(Temperatura)),]
dataL256 <- data256[data256$Alfa==alphaVals[elem],]
	dataL256 <- dataL256[with(dataL256,order(Temperatura)),]
dataL512 <- data512[data512$Alfa==alphaVals[elem],]
	dataL512 <- dataL512[with(dataL512,order(Temperatura)),]

pdf('Graphs/Alfa=1.0/Cumulant.pdf')
plot(dataL128$Temperatura,dataL128$cumulantFerro,type='o',pch=18,lty=3,col='red',ylim=c(0.1,0.7),xlab=c("Temperatura [J/kB]"),ylab=c("Cumulante"),main=c("Cumulante vs Temp alfaud=-1.2"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$cumulantFerro,type='o',pch=19,lty=4,col='blue')
#lines(dataL512$Temperatura,dataL512$cumulantAnti,type='o',pch=20,lty=5,col='green')
lines(dataL32$Temperatura,dataL32$cumulantFerro,type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$cumulantFerro,type='o',pch=17,lty=2,col='magenta')
#plot(dataL8$Temperatura,dataL8$cumulantFerro,type='o',pch=16,lty=1,col='purple',xlab=c("Temperatura [J/kB]"),ylab=c("Cumulante"),ylim=c(0,0.65))
#lines(dataL16$Temperatura,dataL16$cumulantFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

pdf('Graphs/Alfa=1.0/Susceptibility.pdf')#xlim=c(0.225,0.23)
#plot(dataL512$Temperatura,dataL512$susceptFerro,type='o',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad (L^2/10^3)"),main=c("Susceptibilidad vs Temp alfaud=-1.2"))
plot(dataL256$Temperatura,dataL256$susceptFerro,type='o',pch=c(19,rep(NA, rep)),lty=4,col='blue',xlab=c("Temperatura [J/kB]"),ylim=c(0,0.22),ylab=c("Susceptibilidad"),main=c("Susceptibilidad vs Temp alfaud=-1.2"))
lines(dataL128$Temperatura,dataL128$susceptFerro,type='o',pch=c(18,rep(NA, rep)),lty=3,col='red')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL32$Temperatura,dataL32$susceptFerro,type='o',pch=c(16,rep(NA, rep)),lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$susceptFerro,type='o',pch=c(17,rep(NA, rep)),lty=2,col='magenta')
#plot(dataL8$Temperatura,dataL8$susceptFerro,type='o',pch=16,lty=1,col='purple',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad"))
#lines(dataL16$Temperatura,dataL16$susceptFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=4,y=0.025,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

pdf('Graphs/Alfa=1.0/Magnetization.pdf')#,xlim=c(0.225,0.23)
#plot(dataL512$Temperatura,dataL512$magnetAnti,type='o',pch=20,lty=5,col='green')
#plot(dataL128$Temperatura,dataL128$magnetAnti,type='o',pch=18,lty=3,col='red',ylim=c(0,1),xlab=c("Temperatura [J/kB]"),ylab=c("Magnetización por espín"),main=c("Magnetización vs Temp alfaud=-1.2"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(dataL256$Temperatura,dataL256$magnetAnti,type='o',pch=19,lty=4,col='blue')
#lines(dataL32$Temperatura,dataL32$magnetAnti,type='o',pch=16,lty=1,col='cyan')
#lines(dataL64$Temperatura,dataL64$magnetAnti,type='o',pch=17,lty=2,col='magenta')
plot(dataL8$Temperatura,dataL8$magnetFerro,type='o',pch=16,lty=1,col='purple',ylim=c(0,1),xlab=c("Temperatura [J/kB]"),ylab=c("Magnetización"))
lines(dataL16$Temperatura,dataL16$magnetFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

pdf('Graphs/Alfa=1.0/CapacidadCalorifica.pdf')#,xlim=c(0.225,0.23)ylim=c(0,1)
#plot(dataL512$Temperatura,dataL512$Cv,type='o',pch=20,lty=5,col='green')
#plot(dataL128$Temperatura,dataL128$Cv*128^2,type='o',ylim=c(0.5,3.5),pch=18,lty=3,col='red',xlab=c("Temperatura [J/kB]"),ylab=c("Capacidad Calorifica"),main=c("Cap Calorifica vs Temp alfaud=-1.2"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(dataL256$Temperatura,dataL256$Cv*256^2,type='o',pch=19,lty=4,col='blue')
#lines(dataL32$Temperatura,dataL32$Cv*32^2,type='o',pch=16,lty=1,col='cyan')
#lines(dataL64$Temperatura,dataL64$Cv*64^2,type='o',pch=17,lty=2,col='magenta')
plot(dataL8$Temperatura,dataL8$Cv,type='o',pch=16,lty=1,col='purple',xlab=c("Temperatura [J/kB]"),ylab=c("Capacidad calorífica"))
lines(dataL16$Temperatura,dataL16$Cv,type='o',pch=17,lty=2,col='seagreen1')
legend(x=4,y=0.015,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()


#Checking Tc with FSS Tc=0.22755
Tc <- 2.269185
lim <- 10#xlim=c(-limite,limite)
pdf("Graphs/Alfa=1.0/CumulantCollapse.pdf")
#plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$cumulantAnti,type='p',pch=18,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("Cumulante U4"),main=c("Colapso U4 para alfa alfa=-1.2"),sub=c("Tc=0.2267"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$cumulantAnti,type='p',pch=16,col='cyan')
#lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$cumulantAnti,type='p',pch=19,col='blue')
#lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$cumulantAnti,type='p',pch=20,col='green')
#lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$cumulantAnti,type='p',pch=17,col='magenta')
plot(8*(dataL8$Temperatura-Tc)/Tc,dataL8$cumulantFerro,type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim),xlab=c("L*(T-Tc)/Tc"),ylab=c("Cumulante"),main=c("Cumulante colapsado"),ylim=c(0,0.65))
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$cumulantFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=-10,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

#Magnetization Collapse
pdf('Graphs/Alfa=1.0/MagnetizationCollapse.pdf')
#plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$magnetAnti*(128)^(1/8),type='o',pch=18,lty=3,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("m*L^(1/8)"),main=c("Magnetización colapsada alfaud=-1.2"),sub=c("Tc=0.2267"))
#lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$magnetAnti*(512)^(1/8),type='o',pch=20,lty=5,col='green')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$magnetAnti*(256)^(1/8),type='o',pch=19,lty=4,col='blue')
#lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$magnetAnti*(32)^(1/8),type='o',pch=16,lty=1,col='cyan')
#lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$magnetAnti*(64)^(1/8),type='o',pch=17,lty=2,col='magenta')
plot(8*(dataL8$Temperatura-Tc)/Tc,dataL8$magnetFerro*(8)^(1/8),type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim),yrange=c(0,1.5),xlab=c("L*(T-Tc)/Tc"),ylab=c("m*L^(1/8)"),main=c("Magnetización colapsada"))
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$magnetFerro*(16)^(1/8),type='o',pch=17,lty=2,col='seagreen1')
legend(x=1,y=5,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()


#Susceptibilidad Collapse
pdf('Graphs/Alfa=1.0/SusceptibilityCollapse.pdf')
#plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$susceptAnti*128^(-7/4),type='o',pch=18,lty=3,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("Susceptibilidad*L^(-7/4)"),main=c("Susceptibilidad colapsada alfaud=-1.2"),sub=c("Tc=0.2267"))
#lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$susceptAnti*512^(-7/4),type='o',pch=20,lty=5,col='green')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$susceptAnti*256^(-7/4),type='o',pch=19,lty=4,col='blue')
#lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$susceptAnti*32^(-7/4),type='o',pch=16,lty=1,col='cyan')
#lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$susceptAnti*64^(-7/4),type='o',pch=17,lty=2,col='magenta')
exp <- 7/4
plot(8*(dataL8$Temperatura-Tc)/Tc,(dataL8$susceptFerro)*8^(-exp),type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim),xlab=c("L*(T-Tc)/Tc"),ylab=c("X*L^(-7/4)"),main=c("Susceptibilidad colapsada"))
lines(16*(dataL16$Temperatura-Tc)/Tc,(dataL16$susceptFerro)*16^(-exp),type='o',pch=17,lty=2,col='seagreen1')
legend(x=-lim,y=5e-4,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

#Cap calorifica Collapse
exte <- 1.5
pdf('Graphs/Alfa=1.0/CapCalorificaCollapse.pdf')
#plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$Cv*128^(exte),ylim=c(0,0.4),type='o',pch=18,lty=3,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("Cap Calorifica"),main=c("Cap Calorifica colapsada alfaud=-1.2"),sub=c("Tc=0.2267"))
#lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$Cv*512^(exte),type='o',pch=20,lty=5,col='green')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$Cv*256^(exte),type='o',pch=19,lty=4,col='blue')
#lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$Cv*32^(exte),type='o',pch=16,lty=1,col='cyan')
#lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$Cv*64^(exte),type='o',pch=17,lty=2,col='magenta')
plot(8*(dataL8$Temperatura-Tc)/Tc,dataL8$Cv*8^(exte),type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim),xlab=c("L*(T-Tc)/Tc"),ylab=c("c*L^(1.5)"),main=c("Capacidad calorífica reducida"))
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$Cv*16^(exte),type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()



# ***************************************************
# ALFA = -12
# ***************************************************
elem <- 4
rep <- 2
#Alfa = -1.2
dataL8 <- data8[data8$Alfa==alphaVals[elem],]
	dataL8 <- dataL8[with(dataL8,order(Temperatura)),]
dataL16 <- data16[data16$Alfa==alphaVals[elem],]
	dataL16 <- dataL16[with(dataL16,order(Temperatura)),]
dataL32 <- data32[data32$Alfa==alphaVals[elem],]
	dataL32 <- dataL32[with(dataL32,order(Temperatura)),]
dataL64 <- data64[data64$Alfa==alphaVals[elem],]
	dataL64 <- dataL64[with(dataL64,order(Temperatura)),]
dataL128 <- data128[data128$Alfa==alphaVals[elem],]
	dataL128 <- dataL128[with(dataL128,order(Temperatura)),]
dataL256 <- data256[data256$Alfa==alphaVals[elem],]
	dataL256 <- dataL256[with(dataL256,order(Temperatura)),]
dataL512 <- data512[data512$Alfa==alphaVals[elem],]
	dataL512 <- dataL512[with(dataL512,order(Temperatura)),]

pdf('Graphs/Alfa=-12.0/Cumulant.pdf')#ylim=c(0.1,0.7),xlim=c(0.225,0.23),
#plot(dataL128$Temperatura,dataL128$cumulantAnti,type='o',pch=18,lty=3,col='red',xlab=c("Temperatura [J/kB]"),ylab=c("Cumulante"),main=c("Cumulante vs Temp alfaud=-1.2"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(dataL256$Temperatura,dataL256$cumulantAnti,type='o',pch=19,lty=4,col='blue')
#lines(dataL512$Temperatura,dataL512$cumulantAnti,type='o',pch=20,lty=5,col='green')
#lines(dataL32$Temperatura,dataL32$cumulantAnti,type='o',pch=16,lty=1,col='cyan')
#lines(dataL64$Temperatura,dataL64$cumulantAnti,type='o',pch=17,lty=2,col='magenta')
plot(dataL8$Temperatura,dataL8$cumulantAnti,type='o',pch=16,lty=1,col='purple')
lines(dataL16$Temperatura,dataL16$cumulantAnti,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

pdf('Graphs/Alfa=-12.0/Susceptibility.pdf')#xlim=c(0.225,0.23)
#plot(dataL512$Temperatura,dataL512$susceptAnti,type='o',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad (L^2/10^3)"),main=c("Susceptibilidad vs Temp alfaud=-1.2"))
#plot(dataL256$Temperatura,dataL256$susceptAnti,type='o',pch=c(19,rep(NA, rep)),lty=4,col='blue',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad"),main=c("Susceptibilidad vs Temp alfaud=-1.2"))
#lines(dataL128$Temperatura,dataL128$susceptAnti,type='o',pch=c(18,rep(NA, rep)),lty=3,col='red')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(dataL32$Temperatura,dataL32$susceptAnti,type='o',pch=c(16,rep(NA, rep)),lty=1,col='cyan')
#lines(dataL64$Temperatura,dataL64$susceptAnti,type='o',pch=c(17,rep(NA, rep)),lty=2,col='magenta')
plot(dataL8$Temperatura,dataL8$susceptAnti,type='o',pch=16,lty=1,col='purple')
lines(dataL16$Temperatura,dataL16$susceptAnti,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

pdf('Graphs/Alfa=-12.0/Magnetization.pdf')#,xlim=c(0.225,0.23)
#plot(dataL512$Temperatura,dataL512$magnetAnti,type='o',pch=20,lty=5,col='green')
#plot(dataL128$Temperatura,dataL128$magnetAnti,type='o',pch=18,lty=3,col='red',ylim=c(0,1),xlab=c("Temperatura [J/kB]"),ylab=c("Magnetización por espín"),main=c("Magnetización vs Temp alfaud=-1.2"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(dataL256$Temperatura,dataL256$magnetAnti,type='o',pch=19,lty=4,col='blue')
#lines(dataL32$Temperatura,dataL32$magnetAnti,type='o',pch=16,lty=1,col='cyan')
#lines(dataL64$Temperatura,dataL64$magnetAnti,type='o',pch=17,lty=2,col='magenta')
plot(dataL8$Temperatura,dataL8$magnetAnti,type='o',pch=16,lty=1,col='purple')
lines(dataL16$Temperatura,dataL16$magnetAnti,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

pdf('Graphs/Alfa=-12.0/CapacidadCalorifica.pdf')#,xlim=c(0.225,0.23)ylim=c(0,1)
#plot(dataL512$Temperatura,dataL512$Cv,type='o',pch=20,lty=5,col='green')
#plot(dataL128$Temperatura,dataL128$Cv*128^2,type='o',ylim=c(0.5,3.5),pch=18,lty=3,col='red',xlab=c("Temperatura [J/kB]"),ylab=c("Capacidad Calorifica"),main=c("Cap Calorifica vs Temp alfaud=-1.2"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(dataL256$Temperatura,dataL256$Cv*256^2,type='o',pch=19,lty=4,col='blue')
#lines(dataL32$Temperatura,dataL32$Cv*32^2,type='o',pch=16,lty=1,col='cyan')
#lines(dataL64$Temperatura,dataL64$Cv*64^2,type='o',pch=17,lty=2,col='magenta')
plot(dataL8$Temperatura,dataL8$Cv,type='o',pch=16,lty=1,col='purple')
lines(dataL16$Temperatura,dataL16$Cv,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()


#Checking Tc with FSS Tc=0.22755
Tc <- 0.2267
lim <- 2
pdf("Graphs/Alfa=-12.0/CumulantCollapse.pdf")
#plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$cumulantAnti,type='p',pch=18,col='red',xlim=c(-lim,lim),xlab=c("L*(T-Tc)/Tc"),ylab=c("Cumulante U4"),main=c("Colapso U4 para alfa alfa=-1.2"),sub=c("Tc=0.2267"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$cumulantAnti,type='p',pch=16,col='cyan')
#lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$cumulantAnti,type='p',pch=19,col='blue')
#lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$cumulantAnti,type='p',pch=20,col='green')
#lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$cumulantAnti,type='p',pch=17,col='magenta')
plot(8*(dataL8$Temperatura-Tc)/Tc,dataL8$cumulantAnti,type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim))
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$cumulantAnti,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

#Magnetization Collapse
pdf('Graphs/Alfa=-12.0/MagnetizationCollapse.pdf')
#plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$magnetAnti*(128)^(1/8),type='o',pch=18,lty=3,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("m*L^(1/8)"),main=c("Magnetización colapsada alfaud=-1.2"),sub=c("Tc=0.2267"))
#lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$magnetAnti*(512)^(1/8),type='o',pch=20,lty=5,col='green')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$magnetAnti*(256)^(1/8),type='o',pch=19,lty=4,col='blue')
#lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$magnetAnti*(32)^(1/8),type='o',pch=16,lty=1,col='cyan')
#lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$magnetAnti*(64)^(1/8),type='o',pch=17,lty=2,col='magenta')
plot(8*(dataL8$Temperatura-Tc)/Tc,dataL8$magnetAnti*8^(1/8),type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim))
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$magnetAnti*16^(1/8),type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

#Susceptibilidad Collapse
pdf('Graphs/Alfa=-12.0/SusceptibilityCollapse.pdf')
#plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$susceptAnti*128^(-7/4),type='o',pch=18,lty=3,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("Susceptibilidad*L^(-7/4)"),main=c("Susceptibilidad colapsada alfaud=-1.2"),sub=c("Tc=0.2267"))
#lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$susceptAnti*512^(-7/4),type='o',pch=20,lty=5,col='green')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$susceptAnti*256^(-7/4),type='o',pch=19,lty=4,col='blue')
#lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$susceptAnti*32^(-7/4),type='o',pch=16,lty=1,col='cyan')
#lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$susceptAnti*64^(-7/4),type='o',pch=17,lty=2,col='magenta')
plot(8*(dataL8$Temperatura-Tc)/Tc,dataL8$susceptAnti*8^(-7/4),type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim))
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$susceptAnti*16^(-7/4),type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

#Cap calorifica Collapse
exte <- 1.5
pdf('Graphs/Alfa=-12.0/CapCalorificaCollapse.pdf')
#plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$Cv*128^(exte),ylim=c(0,0.4),type='o',pch=18,lty=3,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("Cap Calorifica"),main=c("Cap Calorifica colapsada alfaud=-1.2"),sub=c("Tc=0.2267"))
#lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$Cv*512^(exte),type='o',pch=20,lty=5,col='green')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$Cv*256^(exte),type='o',pch=19,lty=4,col='blue')
#lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$Cv*32^(exte),type='o',pch=16,lty=1,col='cyan')
#lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$Cv*64^(exte),type='o',pch=17,lty=2,col='magenta')
plot(8*(dataL8$Temperatura-Tc)/Tc,dataL8$Cv*8^(exte),type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim))
lines(16*(dataL16$Temperatura-Tc)/Tc,dataL16$Cv*16^(exte),type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()
