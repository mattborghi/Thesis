#Print intermediate steps R
file <- "spinSteps.dat"
data <- read.csv(file,sep=",",stringsAsFactors=F,header=F)
#Dimensions
ncols <- 20
nrows <- 20
#nsteps
# of images stored 
nsteps <- 10
average <- 1
npoints <- 1000

for(k in seq(average)){
	for(n in seq(nsteps)){
	index <- 1
	interData <- as.numeric(data[n*k,])
	pdf(paste('Graphs/Steps',n,'Aver',k,'.pdf'))
	#x11()
	plot(0,0,xlim=c(0,nrows+1),ylim=c(0,ncols+1))
		for(j in seq(nrows)){
			#Fix a row
			for(i in seq(ncols)){
				#Loops in cols
				points(i,j,col=(interData[index]+2),pch=15,cex=3)
				index <- index + 1
			}
		}
	legend(x=-0.5,y=0.8,c("s=1","s=-1"),col=c(3,1),pch=15,box.col="white")
	dev.off()
	}
}


