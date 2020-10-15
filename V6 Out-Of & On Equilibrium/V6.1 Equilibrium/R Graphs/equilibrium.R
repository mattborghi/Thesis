
file <- "Energy20L.dat"
file2 <- "Magnetization20L.dat"
dat = read.csv(file, header = TRUE,stringsAsFactors=FALSE,sep=",")
dat2 = read.csv(file2,header=TRUE,stringsAsFactors=FALSE,sep=",")

#EnergyFile

#<E>





file3 <- "TimeAver.dat"
dat3 = read.csv(file3,header=TRUE,stringsAsFactors=FALSE,sep=",")
#At temp T=5
datt5<- dat3[dat3$Temperature==5,]
name <- c("Time","Energy","Magnetization")
datt5<-datt5[name]
plot(datt5$Time,datt5$Magnetization)
#For other temps
Temps <- unique(dat3$Temperature)
#cast by
index <- 60
datt60<- dat3[dat3$Temperature==Temps[index],]
