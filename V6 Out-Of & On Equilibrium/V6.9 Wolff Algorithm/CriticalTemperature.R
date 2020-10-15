file  <- 'Output0.5.dat.csv'

data <- read.csv(file,stringsAsFactors=F,header=T,sep=',')

temps <- unique(data$Temperatura)

alpha <- unique(data$Alfa)

sizes <- unique(data$Size)

folder <- 'Graphs/Alfa=0.5/'
Tc <- 1.7#2.269185

# ***************************************************
# ALFA = 1 -> ISING USUAL
# ***************************************************
elemAlfa <- 1	
rep <- 2

elemSize <- 1
dataL32 <- data[data$Alfa==alpha[elemAlfa] & data$Size == sizes[elemSize],]
	dataL32 <- dataL32[with(dataL32,order(Temperatura)),]
elemSize <- 2
dataL64 <- data[data$Alfa==alpha[elemAlfa] & data$Size == sizes[elemSize],]
	dataL64 <- dataL64[with(dataL64,order(Temperatura)),]
elemSize <- 3
dataL128 <- data[data$Alfa==alpha[elemAlfa] & data$Size == sizes[elemSize],]
	dataL128 <- dataL128[with(dataL128,order(Temperatura)),]
elemSize <- 4
dataL256 <- data[data$Alfa==alpha[elemAlfa] & data$Size == sizes[elemSize],]
	dataL256 <- dataL256[with(dataL256,order(Temperatura)),]
elemSize <- 5
dataL512 <- data[data$Alfa==alpha[elemAlfa] & data$Size == sizes[elemSize],]
	dataL512 <- dataL512[with(dataL512,order(Temperatura)),]
elemSize <- 6
dataL1024 <- data[data$Alfa==alpha[elemAlfa] & data$Size == sizes[elemSize],]
	dataL1024 <- dataL1024[with(dataL1024,order(Temperatura)),]
elemSize <- 7
dataL2048 <- data[data$Alfa==alpha[elemAlfa] & data$Size == sizes[elemSize],]
	dataL2048 <- dataL2048[with(dataL2048,order(Temperatura)),]

# ***************************************************
# CUMULANTE
# ***************************************************

pdf(paste(folder,'Cumulant.pdf',sep=''))
plot(dataL128$Temperatura,dataL128$Cumulant,type='o',pch=18,lty=3,col='red',ylim=c(0.1,0.7),xlab=c("Temperatura [J/kB]"),ylab=c("Cumulante U_4"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$Cumulant,type='o',pch=19,lty=4,col='blue')
lines(dataL512$Temperatura,dataL512$Cumulant,type='o',pch=20,lty=5,col='green')
#lines(dataL1024$Temperatura,dataL1024$Cumulant,type='o',pch=19,lty=4,col='purple')
#lines(dataL2048$Temperatura,dataL2048$Cumulant,type='o',pch=20,lty=5,col='seagreen1')
lines(dataL32$Temperatura,dataL32$Cumulant,type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$Cumulant,type='o',pch=17,lty=2,col='magenta')
#legend(x=0.225,y=0.55,legend=c("L=032","L=064","L=128","L=256","L=512","L=1024","L=2048"),col=c('cyan','magenta','red','blue','green','purple','seagreen1'),pch=c(16,17,18,19,20,19,20),lty=c(1,2,3,4,5,4,5))
legend(x=0.7,y=0.55,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()

pdf(paste(folder,'closeCumulant.pdf',sep='')) # 2.25,2.28
plot(dataL128$Temperatura,dataL128$Cumulant,type='o',pch=18,lty=3,col='red',xlim=c(1.5,1.8),ylim=c(0.2,0.72),xlab=c("Temperatura [J/kB]"),ylab=c("Cumulante U_4"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$Cumulant,type='o',pch=19,lty=4,col='blue')
lines(dataL512$Temperatura,dataL512$Cumulant,type='o',pch=20,lty=5,col='green')
#lines(dataL1024$Temperatura,dataL1024$Cumulant,type='o',pch=19,lty=4,col='purple')
#lines(dataL2048$Temperatura,dataL2048$Cumulant,type='o',pch=20,lty=5,col='seagreen1')
lines(dataL32$Temperatura,dataL32$Cumulant,type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$Cumulant,type='o',pch=17,lty=2,col='magenta')
#legend(x=0.225,y=0.55,legend=c("L=032","L=064","L=128","L=256","L=512","L=1024","L=2048"),col=c('cyan','magenta','red','blue','green','purple','seagreen1'),pch=c(16,17,18,19,20,19,20),lty=c(1,2,3,4,5,4,5))
legend(x=2.260,y=0.5,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()

#Checking Tc with FSS Tc=0.22755
lim <- 2#xlim=c(-limite,limite)
pdf(paste(folder,'CumulantCollapse.pdf',sep=''))
plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$Cumulant,type='p',pch=18,col='red',ylim=c(0.2,0.72),xlim=c(-lim,lim),xlab=c("L*(T-Tc)/Tc"),ylab=c("U_4"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$Cumulant,type='p',pch=16,col='cyan')
lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$Cumulant,type='p',pch=19,col='blue')
lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$Cumulant,type='p',pch=20,col='green')
#lines(1024*(dataL1024$Temperatura-Tc)/Tc,dataL1024$Cumulant,type='p',pch=19,col='purple')
#lines(2048*(dataL2048$Temperatura-Tc)/Tc,dataL2048$Cumulant,type='p',pch=20,col='seagreen1')
lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$Cumulant,type='p',pch=17,col='magenta')
#legend(x=0.225,y=0.55,legend=c("L=032","L=064","L=128","L=256","L=512","L=1024","L=2048"),col=c('cyan','magenta','red','blue','green','purple','seagreen1'),pch=c(16,17,18,19,20,19,20),lty=c(1,2,3,4,5,4,5))
legend(x=-1.5,y=0.6,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()


# ***************************************************
# MAGNETIZACION
# ***************************************************

#Magnetization Collapse
pdf(paste(folder,'MagnetizationCollapse.pdf',sep=''))
plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$Mabs*(128)^(1/8),xlim=c(-lim,lim),type='p',pch=18,lty=3,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("m*L^(1/8)"))
lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$Mabs*(512)^(1/8),type='p',pch=20,lty=5,col='green')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$Mabs*(256)^(1/8),type='p',pch=19,lty=4,col='blue')
#lines(1024*(dataL1024$Temperatura-Tc)/Tc,dataL1024$Mabs*(1024)^(1/8),type='p',pch=19,lty=4,col='purple')
#lines(2048*(dataL2048$Temperatura-Tc)/Tc,dataL2048$Mabs*(2048)^(1/8),type='p',pch=20,lty=5,col='seagreen1')
lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$Mabs*(32)^(1/8),type='p',pch=16,lty=1,col='cyan')
lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$Mabs*(64)^(1/8),type='p',pch=17,lty=2,col='magenta')
#legend(x=0.225,y=0.55,legend=c("L=032","L=064","L=128","L=256","L=512","L=1024","L=2048"),col=c('cyan','magenta','red','blue','green','purple','seagreen1'),pch=c(16,17,18,19,20,19,20),lty=c(1,2,3,4,5,4,5))
legend(x=-2,y=1.0,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()



pdf(paste(folder,'Magnetzation.pdf',sep=''))
plot(dataL128$Temperatura,dataL128$Mabs,type='o',pch=18,lty=3,col='red',ylim=c(0,1),xlab=c("Temperatura [J/kB]"),ylab=c("Magnetización por espín"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$Mabs,type='o',pch=19,lty=4,col='blue')
lines(dataL512$Temperatura,dataL512$Mabs,type='o',pch=20,lty=5,col='green')
#lines(dataL1024$Temperatura,dataL1024$Mabs,type='o',pch=19,lty=4,col='purple')
#lines(dataL2048$Temperatura,dataL2048$Mabs,type='o',pch=20,lty=5,col='seagreen1')
lines(dataL32$Temperatura,dataL32$Mabs,type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$Mabs,type='o',pch=17,lty=2,col='magenta')
#legend(x=0.225,y=0.55,legend=c("L=032","L=064","L=128","L=256","L=512","L=1024","L=2048"),col=c('cyan','magenta','red','blue','green','purple','seagreen1'),pch=c(16,17,18,19,20,19,20),lty=c(1,2,3,4,5,4,5))
legend(x=0.5,y=0.55,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()



# ***************************************************
# ENERGIA
# ***************************************************


pdf(paste(folder,'Energia.pdf',sep=''))
plot(dataL128$Temperatura,dataL128$Energy,type='o',pch=18,lty=3,col='red',xlab=c("Temperatura [J/kB]"),ylab=c("Energia por espín"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$Energy,type='o',pch=19,lty=4,col='blue')
lines(dataL512$Temperatura,dataL512$Energy,type='o',pch=20,lty=5,col='green')
#lines(dataL1024$Temperatura,dataL1024$Energy,type='o',pch=19,lty=4,col='purple')
#lines(dataL2048$Temperatura,dataL2048$Energy,type='o',pch=20,lty=5,col='seagreen1')
lines(dataL32$Temperatura,dataL32$Energy,type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$Energy,type='o',pch=17,lty=2,col='magenta')
#legend(x=0.225,y=0.55,legend=c("L=032","L=064","L=128","L=256","L=512","L=1024","L=2048"),col=c('cyan','magenta','red','blue','green','purple','seagreen1'),pch=c(16,17,18,19,20,19,20),lty=c(1,2,3,4,5,4,5))
legend(x=0.5,y=-1.0,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()


# ***************************************************
# SUSCEPTIBILIDAD
# ***************************************************

pdf(paste(folder,'SusceptibilityAbs.pdf',sep=''))# 2,2.5
plot(dataL512$Temperatura,dataL512$SusceptAbs/(512^2),xlim=c(1.5,1.8),ylim=c(0,0.02),type='o',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad por espín /L^2"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$SusceptAbs/(256^2),type='o',pch=20,lty=4,col='blue')
lines(dataL128$Temperatura,dataL128$SusceptAbs/(128^2),type='o',pch=18,lty=3,col='red')
#lines(dataL1024$Temperatura,dataL1024$SusceptAbs/(1024^2),type='o',pch=19,lty=4,col='purple')
#lines(dataL2048$Temperatura,dataL2048$SusceptAbs/(2048^2),type='o',pch=20,lty=5,col='seagreen1')
lines(dataL32$Temperatura,dataL32$SusceptAbs/(32^2),type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$SusceptAbs/(64^2),type='o',pch=17,lty=2,col='magenta')
#legend(x=0.225,y=0.55,legend=c("L=032","L=064","L=128","L=256","L=512","L=1024","L=2048"),col=c('cyan','magenta','red','blue','green','purple','seagreen1'),pch=c(16,17,18,19,20,19,20),lty=c(1,2,3,4,5,4,5))
legend(x=2,y=0.02,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,20,20),lty=c(1,2,3,4,5))
dev.off()


#Susceptibilidad Collapse
pdf(paste(folder,'SusceptibilityCollapse.pdf',sep=''))
plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$SusceptAbs*128^(-7/4),xlim=c(-lim,lim),type='p',pch=18,lty=3,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("X*L^(-7/4)"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$SusceptAbs*256^(-7/4),type='p',pch=19,lty=4,col='blue')
lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$SusceptAbs*512^(-7/4),type='p',pch=20,lty=5,col='green')
#lines(1024*(dataL1024$Temperatura-Tc)/Tc,dataL1024$SusceptAbs*1024^(-7/4),type='p',pch=19,lty=4,col='purple')
#lines(2048*(dataL2048$Temperatura-Tc)/Tc,dataL2048$SusceptAbs*2048^(-7/4),type='p',pch=20,lty=5,col='seagreen1')
lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$SusceptAbs*32^(-7/4),type='p',pch=16,lty=1,col='cyan')
lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$SusceptAbs*64^(-7/4),type='p',pch=17,lty=2,col='magenta')
#legend(x=0.225,y=0.55,legend=c("L=032","L=064","L=128","L=256","L=512","L=1024","L=2048"),col=c('cyan','magenta','red','blue','green','purple','seagreen1'),pch=c(16,17,18,19,20,19,20),lty=c(1,2,3,4,5,4,5))
legend(x=0.5,y=0.02,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()

pdf(paste(folder,'Susceptibility.pdf',sep=''))
plot(dataL512$Temperatura,dataL512$Susceptibility/(512^2),type='p',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad por espín /L^2"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$Susceptibility/(256^2),type='p',pch=19,lty=4,col='blue')
lines(dataL128$Temperatura,dataL128$Susceptibility/(128^2),type='p',pch=18,lty=3,col='red')
#lines(dataL1024$Temperatura,dataL1024$Susceptibility/(1024^2),type='p',pch=19,lty=4,col='purple')
#lines(dataL2048$Temperatura,dataL2048$Susceptibility/(2048^2),type='p',pch=20,lty=5,col='seagreen1')
lines(dataL32$Temperatura,dataL32$Susceptibility/(32^2),type='p',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$Susceptibility/(64^2),type='p',pch=17,lty=2,col='magenta')
#legend(x=0.225,y=0.55,legend=c("L=032","L=064","L=128","L=256","L=512","L=1024","L=2048"),col=c('cyan','magenta','red','blue','green','purple','seagreen1'),pch=c(16,17,18,19,20,19,20),lty=c(1,2,3,4,5,4,5))
legend(x=3,y=0.4,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()


# ***************************************************
# CAPACIDAD CALORIFICA
# ***************************************************

#Cap calorifica Collapse
exte <- -0.2
pdf(paste(folder,'CapCalorificaCollapse.pdf',sep=''))
plot(128*(dataL128$Temperatura-Tc)/Tc,dataL128$HeatCapacity*128^(exte),xlim=c(-4,4),type='p',pch=18,lty=3,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("c*L^(-0.2)"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(256*(dataL256$Temperatura-Tc)/Tc,dataL256$HeatCapacity*256^(exte),type='p',pch=19,lty=4,col='blue')
lines(512*(dataL512$Temperatura-Tc)/Tc,dataL512$HeatCapacity*512^(exte),type='p',pch=20,lty=5,col='green')
#lines(1024*(dataL1024$Temperatura-Tc)/Tc,dataL1024$HeatCapacity*1024^(exte),type='p',pch=19,lty=4,col='purple')
#lines(2048*(dataL2048$Temperatura-Tc)/Tc,dataL2048$HeatCapacity*2048^(exte),type='p',pch=20,lty=5,col='seagreen1')
lines(32*(dataL32$Temperatura-Tc)/Tc,dataL32$HeatCapacity*32^(exte),type='p',pch=16,lty=1,col='cyan')
lines(64*(dataL64$Temperatura-Tc)/Tc,dataL64$HeatCapacity*64^(exte),type='p',pch=17,lty=2,col='magenta')
#legend(x=0.225,y=0.55,legend=c("L=032","L=064","L=128","L=256","L=512","L=1024","L=2048"),col=c('cyan','magenta','red','blue','green','purple','seagreen1'),pch=c(16,17,18,19,20,19,20),lty=c(1,2,3,4,5,4,5))
legend(x=0,y=0.6,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()

pdf(paste(folder,'CapacidadCalorifica.pdf',sep=''))
plot(dataL512$Temperatura,dataL512$HeatCapacity,type='o',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Capacidad Calorifica por espín"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$HeatCapacity,type='o',pch=19,lty=4,col='blue')
lines(dataL128$Temperatura,dataL128$HeatCapacity,type='o',pch=18,lty=3,col='red')
#lines(dataL1024$Temperatura,dataL1024$HeatCapacity,type='o',pch=19,lty=4,col='purple')
#lines(dataL2048$Temperatura,dataL2048$HeatCapacity,type='o',pch=20,lty=5,col='seagreen1')
lines(dataL32$Temperatura,dataL32$HeatCapacity,type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$HeatCapacity,type='o',pch=17,lty=2,col='magenta')
#legend(x=0.225,y=0.55,legend=c("L=032","L=064","L=128","L=256","L=512","L=1024","L=2048"),col=c('cyan','magenta','red','blue','green','purple','seagreen1'),pch=c(16,17,18,19,20,19,20),lty=c(1,2,3,4,5,4,5))
legend(x=3,y=2.5,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()


pdf(paste(folder,'closeCapacidadCalorifica.pdf',sep=''))
plot(dataL512$Temperatura,dataL512$HeatCapacity,type='o',pch=20,lty=5,col='green',xlim=c(2,2.5),xlab=c("Temperatura [J/kB]"),ylab=c("Capacidad Calorifica por espín"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(dataL256$Temperatura,dataL256$HeatCapacity,type='o',pch=19,lty=4,col='blue')
lines(dataL128$Temperatura,dataL128$HeatCapacity,type='o',pch=18,lty=3,col='red')
#lines(dataL1024$Temperatura,dataL1024$HeatCapacity,type='o',pch=19,lty=4,col='purple')
#lines(dataL2048$Temperatura,dataL2048$HeatCapacity,type='o',pch=20,lty=5,col='seagreen1')
lines(dataL32$Temperatura,dataL32$HeatCapacity,type='o',pch=16,lty=1,col='cyan')
lines(dataL64$Temperatura,dataL64$HeatCapacity,type='o',pch=17,lty=2,col='magenta')
#legend(x=0.225,y=0.55,legend=c("L=032","L=064","L=128","L=256","L=512","L=1024","L=2048"),col=c('cyan','magenta','red','blue','green','purple','seagreen1'),pch=c(16,17,18,19,20,19,20),lty=c(1,2,3,4,5,4,5))
legend(x=2,y=3,legend=c("L=032","L=064","L=128","L=256","L=512"),col=c('cyan','magenta','red','blue','green'),pch=c(16,17,18,19,20),lty=c(1,2,3,4,5))
dev.off()







