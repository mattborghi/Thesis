library(ggplot2)
size <- '512'
folder <- paste('../L=',size,'/',sep='')
file <- 'Magnetization.dat'
FileOutput <- paste(folder,'fitmagnet.data',sep="")
FileLocation <- paste(folder,file,sep="")
data <- read.csv(FileLocation,sep=',',header=T,stringsAsFactors=F)
dim(data)
head(data)
temps <- unique(data$Temp)
temps
field <- unique(data$B.Field)
field
m<-cbind("Temperatura","MagneticField","Magnetization","Magnetization2")
write(t(m),FileOutput,ncolumns=4)
#Plot Magnet vs B-Field for different Temps
first = TRUE
#i = 17

for (temp in temps) {
	#interData = paste("dataT",temp,sep="")
	interData <- data[data$Temp==temp,]
	interData <- interData[with(interData,order(B.Field)),]
	m<-cbind(temp,interData[interData$B.Field<0.2,]$B.Field,interData[interData$B.Field<0.2,]$AverMagnetization,interData[interData$B.Field<0.2,]$AverMagnetization.2)
	# t(x) is the transpose function applied to a matrix x 
	write(t(m),FileOutput,ncolumns=4,append=TRUE)

	#if (first){
		#Just the first time
	#	first = FALSE
#		plot(interData$B.Field,interData$AverMagnetization,xlim=c(0,1),pch=i,type='o')
#	} else{
#		if( TRUE ){
#			lines(interData$B.Field,interData$AverMagnetization,pch=(i+1),type='o')
#		}
#	}
}

dataset <- subset(data,data$Temp<0.2 & data$Temp>0.05 & data$B.Field<=0.2,select=c("Temp","B.Field","AverMagnetization","Suceptibility"))
grafico <- ggplot(data=dataset,aes(x=B.Field,y=AverMagnetization,color=factor(round(Temp,digits=4)))) + geom_point() + geom_line()+ theme(legend.position = c(0.6, 0.2))
grafico + xlab("Campo Magnético Externo") + ylab("Magnetización") + labs(color='Temperatura') + ggtitle(paste("Magnetización L=",size)) 
#Change legend title
#grafico$labels$colour <- "Temperatura"
#grafico
ImageOutput <- paste(folder,'Magnetization.pdf',sep="") 
ggsave(ImageOutput)

grafico <- ggplot(data=dataset,aes(x=B.Field,y=Suceptibility,color=factor(round(Temp,digits=4)))) + geom_point() + geom_line()+ theme(legend.position = c(0.6, 0.6))
grafico + xlab("Campo Magnético Externo") + ylab("Susceptibilidad") + labs(color='Temperatura') + ggtitle(paste("Susceptibilidad L=",size)) 
ImageOutput <- paste(folder,'Susceptibilidad.pdf',sep="") 
ggsave(ImageOutput)

#dev.copy(pdf,ImageOutput)
#dev.off()

dataset <- subset(data,data$Temp<0.2 & data$Temp>0.05 & data$B.Field<=0.2,select=c("Temp","B.Field","AverEnergy","C_v"))
grafico <- ggplot(data=dataset,aes(x=B.Field,y=AverEnergy,color=factor(round(Temp,digits=4)))) + geom_point() + geom_line() + theme(legend.position = c(0.2, 0.2))
grafico + xlab("Campo Magnético Externo") + ylab("Energia por Espin") + labs(color='Temperatura') + ggtitle(paste("Energía por espin L=",size))
ImageOutput <- paste(folder,'Energia.pdf',sep="") 
ggsave(ImageOutput)

grafico <- ggplot(data=dataset,aes(x=B.Field,y=C_v,color=factor(round(Temp,digits=4)))) + geom_point() + geom_line() + theme(legend.position = c(0.5, 0.2))
grafico + xlab("Campo Magnético Externo") + ylab("C_v") + labs(color='Temperatura') + ggtitle(paste("C_v L=",size)) 
ImageOutput <- paste(folder,'Cv.pdf',sep="") 
ggsave(ImageOutput)



#Resultados a campo fijo externo pequeño

dataset <- subset(data,data$B.Field==field[40] | data$B.Field==field[20] | data$B.Field==field[30] ,select=c("Temp","B.Field","AverMagnetization","Suceptibility"))
grafico <- ggplot(data=dataset,aes(x=Temp,y=AverMagnetization,color=factor(round(B.Field,digits=4)))) + geom_point() + geom_line() + theme(legend.position = c(0.6, 0.5))
grafico + xlab("Temperatura") + ylab("Magnetización") + labs(color='Campo magnético externo') + ggtitle(paste("Magnetización L=",size)) 
#Change legend title
#grafico$labels$colour <- "Temperatura"
#grafico
ImageOutput <- paste(folder,'MagnetizationFixH.pdf',sep="") 
ggsave(ImageOutput)

grafico <- ggplot(data=dataset,aes(x=Temp,y=Suceptibility,color=factor(round(B.Field,digits=4)))) + geom_point() + geom_line() + theme(legend.position = c(0.6, 0.6))
grafico + xlab("Temperatura") + ylab("Susceptibilidad") + labs(color='Campo magnético externo') + ggtitle(paste("Susceptibilidad L=",size)) 
ImageOutput <- paste(folder,'SusceptibilidadFixH.pdf',sep="") 
ggsave(ImageOutput)

#--------Maximum values of Susceptibility
filtH <- unique(data$B.Field)
maxSuscept <- array(0,dim=length(filtH))
maxTemp <- array(0,dim=length(filtH))
cont <- 1
for (i in filtH) {
	filteredData <- data[data$B.Field==i,]
	maxSuscept[cont] <- max(filteredData$Suceptibility)
	ind_max <- which(filteredData$Suceptibility == maxSuscept[cont] )
	maxTemp[cont] <- filteredData$Temp[ind_max]
	cont <- cont + 1
}
maxDF <- data.frame('Temperatura'=maxTemp,'Susceptibilidad'=maxSuscept,'MagField'=filtH) 
grafico <- ggplot(data=maxDF,aes(x=filtH,y=Temperatura)) + geom_point() + geom_line() + xlim(c(0,0.102)) + ylim(c(0,0.102))
grafico + xlab("Campo magnético externo") + ylab("Temperatura") + ggtitle(paste("Max. Susceptibilidad L=",size)) + theme(legend.position = c(0.7,0.2))
ImageOutput <- paste(folder,'MaxSusceptibilidad.pdf',sep="") 
ggsave(ImageOutput)


#dev.copy(pdf,ImageOutput)
#dev.off()

dataset <- subset(data,data$B.Field==field[40] | data$B.Field==field[20] | data$B.Field==field[30] ,select=c("Temp","B.Field","AverEnergy","C_v"))
grafico <- ggplot(data=dataset,aes(x=Temp,y=AverEnergy,color=factor(round(B.Field,digits=4)))) + geom_point() + geom_line() + theme(legend.position = c(0.5, 0.2))
grafico + xlab("Temperatura") + ylab("Energia por Espin") + labs(color='Campo magnético externo') + ggtitle(paste("Energía por espin L=",size)) 
ImageOutput <- paste(folder,'EnergiaFixH.pdf',sep="") 
ggsave(ImageOutput)

grafico <- ggplot(data=dataset,aes(x=Temp,y=C_v,color=factor(round(B.Field,digits=4)))) + geom_point() + geom_line()+ theme(legend.position = c(0.6,0.5))
grafico + xlab("Temperatura") + ylab("C_v") + labs(color='Campo magnético externo') + ggtitle(paste("C_v L=",size)) 
ImageOutput <- paste(folder,'CvFixH.pdf',sep="") 
ggsave(ImageOutput)
