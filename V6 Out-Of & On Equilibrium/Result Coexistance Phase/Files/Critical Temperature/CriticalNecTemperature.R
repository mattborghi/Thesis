data03 <- 'alfa=-3.0.csv'
data05 <- 'alfa=0.5.csv'
data10 <- 'alfa=1.0.csv'

folder8 <- 'L=008/'
folder16 <- 'L=016/'
folder32 <- 'L=032/'
folder64 <- 'L=064/'
folder128 <- 'L=128/'
folder256 <- 'L=256/'
folder512 <- 'L=512/'

subfolder <- 'FSS/'

# ************************************************************
# ************************************************************
# ************************************************************


file8 <- sprintf("%s%s",folder8,data05)
file16 <- sprintf("%s%s",folder16,data05)
file32 <- sprintf("%s%s",folder32,data05)
file64 <- sprintf("%s%s",folder64,data05)
file128 <- sprintf("%s%s",folder128,data05)
file256 <- sprintf("%s%s",folder256,data05)
file512 <- sprintf("%s%s",folder512,data05)



data8	<- read.csv(file8,stringsAsFactors=F,header=T,sep=',')
data16	<- read.csv(file16,stringsAsFactors=F,header=T,sep=',')
data32	<- read.csv(file32,stringsAsFactors=F,header=T,sep=',')
data64	<- read.csv(file64,stringsAsFactors=F,header=T,sep=',')
data128 <- read.csv(file128,stringsAsFactors=F,header=T,sep=',')
data256 <- read.csv(file256,stringsAsFactors=F,header=T,sep=',')
data512 <- read.csv(file512,stringsAsFactors=F,header=T,sep=',')

# ***************************************************
# ALFA = 0.5
# ***************************************************

pdf('Graphs/Alfa=0.5/Cumulant.pdf')
plot(data128$Temperatura,data128$cumulantFerro,type='o',pch=18,col='red',xlab=c("Temperatura [J/kB]"),ylab=c("Cumulante"),main=c("Cumulante vs Temp alfaud=0.5"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(data256$Temperatura,data256$cumulantFerro,type='o',pch=19,col='blue')
#lines(data512$Temperatura,data512$cumulantFerro,type='o',pch=20,col='green')
lines(data32$Temperatura,data32$cumulantFerro,type='o',pch=16,lty=1,col='cyan')
lines(data64$Temperatura,data64$cumulantFerro,type='o',pch=17,lty=2,col='magenta')
lines(data8$Temperatura,data8$cumulantFerro,type='o',pch=16,lty=1,col='purple')
lines(data16$Temperatura,data16$cumulantFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=1,y=0.4,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()#

pdf('Graphs/Alfa=0.5/Susceptibility.pdf')
plot(data512$Temperatura,data512$susceptFerro,type='o',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad (L^2/10^3)"),main=c("Susceptibilidad vs Temp alfaud=0.5"))
lines(data128$Temperatura,data128$susceptFerro*(128^2)/(10^3),type='o',pch=18,lty=3,col='red')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(data256$Temperatura,data256$susceptFerro*(256^2)/(10^3),type='o',pch=19,lty=4,col='blue')
lines(data32$Temperatura,data32$susceptFerro*(32^2)/(10^3),type='o',pch=16,lty=1,col='cyan')
lines(data64$Temperatura,data64$susceptFerro*(64^2)/(10^3),type='o',pch=17,lty=2,col='magenta')
plot(data8$Temperatura,data8$susceptFerro,type='o',pch=16,lty=1,col='purple')
lines(data16$Temperatura,data16$susceptFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()#

Tc <- 1.7
pdf('Graphs/Alfa=0.5/CumulantCollapse.pdf')
plot(128*(data128$Temperatura-Tc)/Tc,data128$cumulantFerro,type='p',pch=18,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("Colapso Cumulante"),main=c("Colapso Cumulante vs Temp alfaud=0.5"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(256*(data256$Temperatura-Tc)/Tc,data256$cumulantFerro,type='p',pch=19,col='blue')
#lines(512*(data512$Temperatura-Tc)/Tc,data512$cumulantFerro,type='p',pch=20,col='green')
lines(32*(data32$Temperatura-Tc)/Tc,data32$cumulantFerro,type='p',pch=16,col='cyan')
lines(64*(data64$Temperatura-Tc)/Tc,data64$cumulantFerro,type='p',pch=17,col='magenta')
lines(8*(data8$Temperatura-Tc)/Tc,data8$cumulantFerro,type='p',pch=16,lty=1,col='purple')
lines(16*(data16$Temperatura-Tc)/Tc,data16$cumulantFerro,type='p',pch=17,lty=2,col='seagreen1')
legend(x=-50,y=0.4,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()#


# ***************************************************
# ALFA = 1 -> ISING USUAL
# ***************************************************
# FILES
# ***************************************************
file8 <- sprintf("%s%s",folder8,data10)
file16 <- sprintf("%s%s",folder16,data10)
file32 <- sprintf("%s%s",folder32,data10)
file64 <- sprintf("%s%s",folder64,data10)
file128 <- sprintf("%s%s",folder128,data10)
file256 <- sprintf("%s%s",folder256,data10)
file512 <- sprintf("%s%s",folder512,data10)

subfile8 <- sprintf("%s%s",subfolder,file8)
	
rep <- 2

data8	<- read.csv(file8,stringsAsFactors=F,header=T,sep=',')
data16	<- read.csv(file16,stringsAsFactors=F,header=T,sep=',')
data32	<- read.csv(file32,stringsAsFactors=F,header=T,sep=',')
data64	<- read.csv(file64,stringsAsFactors=F,header=T,sep=',')
data128 <- read.csv(file128,stringsAsFactors=F,header=T,sep=',')
data256 <- read.csv(file256,stringsAsFactors=F,header=T,sep=',')
data512 <- read.csv(file512,stringsAsFactors=F,header=T,sep=',')

subdata8 <- read.csv(subfile8,stringsAsFactors=F,header=T,sep=',')

pdf('Graphs/Alfa=1.0/Cumulant.pdf')
plot(data128$Temperatura,data128$cumulantFerro,type='o',pch=18,lty=3,col='red',xlab=c("Temperatura [J/kB]"),ylab=c("Cumulante"),main=c("Cumulante vs Temp alfaud=-1.2"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(data256$Temperatura,data256$cumulantFerro,type='o',pch=19,lty=4,col='blue')
#lines(data512$Temperatura,data512$cumulantFerro,type='o',pch=20,lty=5,col='green')
lines(data32$Temperatura,data32$cumulantFerro,type='o',pch=16,lty=1,col='cyan')
lines(data64$Temperatura,data64$cumulantFerro,type='o',pch=17,lty=2,col='magenta')
lines(data8$Temperatura,data8$cumulantFerro,type='o',pch=16,lty=1,col='purple',xlab=c("Temperatura [J/kB]"),ylab=c("Cumulante"),ylim=c(0,0.65))
lines(data16$Temperatura,data16$cumulantFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0,y=0.3,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

pdf('Graphs/Alfa=1.0/Susceptibility.pdf')#xlim=c(0.225,0.23)
#plot(data512$Temperatura,data512$susceptAnti,type='o',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad (L^2/10^3)"),main=c("Susceptibilidad vs Temp alfaud=-1.2"))
#plot(data256$Temperatura,data256$susceptAnti,type='o',pch=c(19,rep(NA, rep)),lty=4,col='blue',xlab=c("Temperatura [J/kB]"),ylim=c(0,0.22),ylab=c("Susceptibilidad"),main=c("Susceptibilidad vs Temp alfaud=-1.2"))
#lines(data128$Temperatura,data128$susceptAnti,type='o',pch=c(18,rep(NA, rep)),lty=3,col='red')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(data32$Temperatura,data32$susceptAnti,type='o',pch=c(16,rep(NA, rep)),lty=1,col='cyan')
#lines(data64$Temperatura,data64$susceptAnti,type='o',pch=c(17,rep(NA, rep)),lty=2,col='magenta')
plot(data8$Temperatura,data8$susceptFerro,type='o',pch=16,lty=1,col='purple',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad"))
lines(data16$Temperatura,data16$susceptFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=4,y=0.025,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

pdf('Graphs/Alfa=1.0/Magnetization.pdf')#,xlim=c(0.225,0.23)
#plot(data512$Temperatura,data512$magnetAnti,type='o',pch=20,lty=5,col='green')
#plot(data128$Temperatura,data128$magnetAnti,type='o',pch=18,lty=3,col='red',ylim=c(0,1),xlab=c("Temperatura [J/kB]"),ylab=c("Magnetización por espín"),main=c("Magnetización vs Temp alfaud=-1.2"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(data256$Temperatura,data256$magnetAnti,type='o',pch=19,lty=4,col='blue')
#lines(data32$Temperatura,data32$magnetAnti,type='o',pch=16,lty=1,col='cyan')
#lines(data64$Temperatura,data64$magnetAnti,type='o',pch=17,lty=2,col='magenta')
plot(data8$Temperatura,data8$magnetFerro,type='o',pch=16,lty=1,col='purple',ylim=c(0,1),xlab=c("Temperatura [J/kB]"),ylab=c("Magnetización"))
lines(data16$Temperatura,data16$magnetFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

pdf('Graphs/Alfa=1.0/CapacidadCalorifica.pdf')#,xlim=c(0.225,0.23)ylim=c(0,1)
#plot(data512$Temperatura,data512$Cv,type='o',pch=20,lty=5,col='green')
#plot(data128$Temperatura,data128$Cv*128^2,type='o',ylim=c(0.5,3.5),pch=18,lty=3,col='red',xlab=c("Temperatura [J/kB]"),ylab=c("Capacidad Calorifica"),main=c("Cap Calorifica vs Temp alfaud=-1.2"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(data256$Temperatura,data256$Cv*256^2,type='o',pch=19,lty=4,col='blue')
#lines(data32$Temperatura,data32$Cv*32^2,type='o',pch=16,lty=1,col='cyan')
#lines(data64$Temperatura,data64$Cv*64^2,type='o',pch=17,lty=2,col='magenta')
plot(data8$Temperatura,data8$Cv,type='o',pch=16,lty=1,col='purple',xlab=c("Temperatura [J/kB]"),ylab=c("Capacidad calorífica"))
lines(data16$Temperatura,data16$Cv,type='o',pch=17,lty=2,col='seagreen1')
legend(x=4,y=0.015,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()


#Checking Tc with FSS Tc=0.22755
Tc <- 2.269185
lim <- 10#xlim=c(-limite,limite)
pdf("Graphs/Alfa=1.0/CumulantCollapse.pdf")
plot(128*(data128$Temperatura-Tc)/Tc,data128$cumulantFerro,type='p',pch=18,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("Cumulante U4"),main=c("Colapso U4 para alfa alfa=-1.2"),sub=c("Tc=0.2267"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(32*(data32$Temperatura-Tc)/Tc,data32$cumulantFerro,type='p',pch=16,col='cyan')
lines(256*(data256$Temperatura-Tc)/Tc,data256$cumulantFerro,type='p',pch=19,col='blue')
lines(512*(data512$Temperatura-Tc)/Tc,data512$cumulantFerro,type='p',pch=20,col='green')
lines(64*(data64$Temperatura-Tc)/Tc,data64$cumulantFerro,type='p',pch=17,col='magenta')
lines(8*(data8$Temperatura-Tc)/Tc,data8$cumulantFerro,type='o',pch=16,lty=1,col='purple',xlab=c("L*(T-Tc)/Tc"),ylab=c("Cumulante"),main=c("Cumulante colapsado"),ylim=c(0,0.65))
lines(16*(data16$Temperatura-Tc)/Tc,data16$cumulantFerro,type='o',pch=17,lty=2,col='seagreen1')
legend(x=-10,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

#Magnetization Collapse
pdf('Graphs/Alfa=1.0/MagnetizationCollapse.pdf')
#plot(128*(data128$Temperatura-Tc)/Tc,data128$magnetAnti*(128)^(1/8),type='o',pch=18,lty=3,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("m*L^(1/8)"),main=c("Magnetización colapsada alfaud=-1.2"),sub=c("Tc=0.2267"))
#lines(512*(data512$Temperatura-Tc)/Tc,data512$magnetAnti*(512)^(1/8),type='o',pch=20,lty=5,col='green')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(256*(data256$Temperatura-Tc)/Tc,data256$magnetAnti*(256)^(1/8),type='o',pch=19,lty=4,col='blue')
#lines(32*(data32$Temperatura-Tc)/Tc,data32$magnetAnti*(32)^(1/8),type='o',pch=16,lty=1,col='cyan')
#lines(64*(data64$Temperatura-Tc)/Tc,data64$magnetAnti*(64)^(1/8),type='o',pch=17,lty=2,col='magenta')
plot(8*(data8$Temperatura-Tc)/Tc,data8$magnetFerro*(8)^(1/8),type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim),yrange=c(0,1.5),xlab=c("L*(T-Tc)/Tc"),ylab=c("m*L^(1/8)"),main=c("Magnetización colapsada"))
lines(16*(data16$Temperatura-Tc)/Tc,data16$magnetFerro*(16)^(1/8),type='o',pch=17,lty=2,col='seagreen1')
legend(x=1,y=5,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()


#Susceptibilidad Collapse
pdf('Graphs/Alfa=1.0/SusceptibilityCollapse.pdf')
#plot(128*(data128$Temperatura-Tc)/Tc,data128$susceptAnti*128^(-7/4),type='o',pch=18,lty=3,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("Susceptibilidad*L^(-7/4)"),main=c("Susceptibilidad colapsada alfaud=-1.2"),sub=c("Tc=0.2267"))
#lines(512*(data512$Temperatura-Tc)/Tc,data512$susceptAnti*512^(-7/4),type='o',pch=20,lty=5,col='green')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(256*(data256$Temperatura-Tc)/Tc,data256$susceptAnti*256^(-7/4),type='o',pch=19,lty=4,col='blue')
#lines(32*(data32$Temperatura-Tc)/Tc,data32$susceptAnti*32^(-7/4),type='o',pch=16,lty=1,col='cyan')
#lines(64*(data64$Temperatura-Tc)/Tc,data64$susceptAnti*64^(-7/4),type='o',pch=17,lty=2,col='magenta')
exp <- 7/4
plot(8*(data8$Temperatura-Tc)/Tc,(data8$susceptFerro)*8^(-exp),type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim),xlab=c("L*(T-Tc)/Tc"),ylab=c("X*L^(-7/4)"),main=c("Susceptibilidad colapsada"))
lines(16*(data16$Temperatura-Tc)/Tc,(data16$susceptFerro)*16^(-exp),type='o',pch=17,lty=2,col='seagreen1')
legend(x=-lim,y=5e-4,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

#Cap calorifica Collapse
exte <- 1.5
pdf('Graphs/Alfa=1.0/CapCalorificaCollapse.pdf')
#plot(128*(data128$Temperatura-Tc)/Tc,data128$Cv*128^(exte),ylim=c(0,0.4),type='o',pch=18,lty=3,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("Cap Calorifica"),main=c("Cap Calorifica colapsada alfaud=-1.2"),sub=c("Tc=0.2267"))
#lines(512*(data512$Temperatura-Tc)/Tc,data512$Cv*512^(exte),type='o',pch=20,lty=5,col='green')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(256*(data256$Temperatura-Tc)/Tc,data256$Cv*256^(exte),type='o',pch=19,lty=4,col='blue')
#lines(32*(data32$Temperatura-Tc)/Tc,data32$Cv*32^(exte),type='o',pch=16,lty=1,col='cyan')
#lines(64*(data64$Temperatura-Tc)/Tc,data64$Cv*64^(exte),type='o',pch=17,lty=2,col='magenta')
plot(8*(data8$Temperatura-Tc)/Tc,data8$Cv*8^(exte),type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim),xlab=c("L*(T-Tc)/Tc"),ylab=c("c*L^(1.5)"),main=c("Capacidad calorífica reducida"))
lines(16*(data16$Temperatura-Tc)/Tc,data16$Cv*16^(exte),type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()


# ***************************************************
# ALFA = -3.0
# ***************************************************
# FILES
# ***************************************************
file8 <- sprintf("%s%s",folder8,data03)
file16 <- sprintf("%s%s",folder16,data03)
file32 <- sprintf("%s%s",folder32,data03)
file64 <- sprintf("%s%s",folder64,data03)
file128 <- sprintf("%s%s",folder128,data03)
file256 <- sprintf("%s%s",folder256,data03)
file512 <- sprintf("%s%s",folder512,data03)

data8	<- read.csv(file8,stringsAsFactors=F,header=T,sep=',')
data16	<- read.csv(file16,stringsAsFactors=F,header=T,sep=',')
data32	<- read.csv(file32,stringsAsFactors=F,header=T,sep=',')
data64	<- read.csv(file64,stringsAsFactors=F,header=T,sep=',')
data128 <- read.csv(file128,stringsAsFactors=F,header=T,sep=',')
data256 <- read.csv(file256,stringsAsFactors=F,header=T,sep=',')
data512 <- read.csv(file512,stringsAsFactors=F,header=T,sep=',')

pdf('Graphs/Alfa=-3.0/Cumulant.pdf')
#plot(data128$Temperatura,data128$cumulantAnti,type='o',pch=18,col='red',xlim=c(2.2,2.3),xlab=c("Temperatura [J/kB]"),ylab=c("Cumulante"),main=c("Cumulante vs Temp alfaud=-3.0"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(data256$Temperatura,data256$cumulantAnti,type='o',pch=19,col='blue')
#lines(data512$Temperatura,data512$cumulantAnti,type='o',pch=20,col='green')
#lines(data32$Temperatura,data32$cumulantAnti,type='o',pch=16,lty=1,col='cyan')
#lines(data64$Temperatura,data64$cumulantAnti,type='o',pch=17,lty=2,col='magenta')
plot(data8$Temperatura,data8$cumulantAnti,type='o',pch=16,lty=1,col='purple')
lines(data16$Temperatura,data16$cumulantAnti,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()#

pdf('Graphs/Alfa=-3.0/Susceptibility.pdf')
plot(data8$Temperatura,data8$susceptAnti,type='o',pch=16,lty=1,col='purple')
#plot(data512$Temperatura,data512$susceptAnti,xlim=c(2.2,2.3),type='o',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad (L^2/10^3)"),main=c("Susceptibilidad vs Temp alfaud=-3.0"))
#lines(data128$Temperatura,data128$susceptAnti,type='o',pch=18,lty=3,col='red')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(data256$Temperatura,data256$susceptAnti,type='o',pch=19,lty=4,col='blue')
lines(data32$Temperatura,data32$susceptAnti,type='o',pch=16,lty=1,col='cyan')
lines(data64$Temperatura,data64$susceptAnti,type='o',pch=17,lty=2,col='magenta')
lines(data16$Temperatura,data16$susceptAnti,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()#

Tc <- 2.269
pdf('Graphs/Alfa=-3.0/CumulantCollapse.pdf')
#plot(128*(data128$Temperatura-Tc)/Tc,data128$cumulantAnti,type='p',pch=18,col='red',xlab=c("L*(T-Tc)/Tc"),ylab=c("Colapso Cumulante"),main=c("Colapso Cumulante vs Temp alfaud=-3.0"))
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
#lines(256*(data256$Temperatura-Tc)/Tc,data256$cumulantAnti,type='p',pch=19,col='blue')
#lines(512*(data512$Temperatura-Tc)/Tc,data512$cumulantAnti,type='p',pch=20,col='green')
lines(32*(data32$Temperatura-Tc)/Tc,data32$cumulantAnti,type='p',pch=16,col='cyan')
lines(64*(data64$Temperatura-Tc)/Tc,data64$cumulantAnti,type='p',pch=17,col='magenta')
lines(8*(data8$Temperatura-Tc)/Tc,data8$cumulantAnti,type='o',pch=16,lty=1,col='purple')
lines(16*(data16$Temperatura-Tc)/Tc,data16$cumulantAnti,type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()#

#Magnetization Collapse
pdf('Graphs/Alfa=-3.0/MagnetizationCollapse.pdf')
plot(512*(data512$Temperatura-Tc)/Tc,data512$magnetAnti*(512)^(-1/8),type='o',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Magnetización Colapsada"),main=c("Magnetización vs Temp alfaud=-3.0"))
lines(128*(data128$Temperatura-Tc)/Tc,data128$magnetAnti*(128)^(-1/8),type='o',pch=18,lty=3,col='red')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(256*(data256$Temperatura-Tc)/Tc,data256$magnetAnti*(256)^(-1/8),type='o',pch=19,lty=4,col='blue')
lines(32*(data32$Temperatura-Tc)/Tc,data32$magnetAnti*(32)^(-1/8),type='o',pch=16,lty=1,col='cyan')
lines(64*(data64$Temperatura-Tc)/Tc,data64$magnetAnti*(64)^(-1/8),type='o',pch=17,lty=2,col='magenta')
plot(8*(data8$Temperatura-Tc)/Tc,data8$Cv*8^(exte),type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim))
lines(16*(data16$Temperatura-Tc)/Tc,data16$Cv*16^(exte),type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

#Susceptibilidad Collapse
pdf('Graphs/Alfa=-3.0/SusceptibilityCollapse.pdf')
plot(512*(data512$Temperatura-Tc)/Tc,data512$susceptAnti*512^(7/4),type='o',pch=20,lty=5,col='green',xlab=c("Temperatura [J/kB]"),ylab=c("Susceptibilidad Colapsada"),main=c("Susceptibilidad vs Temp alfaud=-3.0"))
lines(128*(data128$Temperatura-Tc)/Tc,data128$susceptAnti*128^(7/4),type='o',pch=18,lty=3,col='red')
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
lines(256*(data256$Temperatura-Tc)/Tc,data256$susceptAnti*256^(7/4),type='o',pch=19,lty=4,col='blue')
lines(32*(data32$Temperatura-Tc)/Tc,data32$susceptAnti*32^(7/4),type='o',pch=16,lty=1,col='cyan')
lines(64*(data64$Temperatura-Tc)/Tc,data64$susceptAnti*64^(7/4),type='o',pch=17,lty=2,col='magenta')
plot(8*(data8$Temperatura-Tc)/Tc,data8$Cv*8^(exte),type='o',pch=16,lty=1,col='purple',xlim=c(-lim,lim))
lines(16*(data16$Temperatura-Tc)/Tc,data16$Cv*16^(exte),type='o',pch=17,lty=2,col='seagreen1')
legend(x=0.225,y=0.55,legend=c("L=008","L=016","L=032","L=064","L=128","L=256","L=512"),col=c('purple','seagreen1','cyan','magenta','red','blue','green'),pch=c(16,17,16,17,18,19,20),lty=c(1,2,1,2,3,4,5))
dev.off()

