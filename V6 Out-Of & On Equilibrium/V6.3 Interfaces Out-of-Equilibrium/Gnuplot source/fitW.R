file <- "Inter20L.dat"
data <- read.csv(file,sep=",",stringsAsFactors=F,header=T)

outputFile <- "FitResult.dat"
write("Time,Roughness,Err",file = outputFile,append=F)
#Numero de ptos en W(t)
N <- 20
#Find the number of columns
Time <- data[data$indexTime == 1,]
ncols <- nrow(Time)

maxtime <- max(data$indexTime)
if(N>maxtime) stop("The number of points desired N can't be larger than the sampled points")
if(N<=1) stop("N can not be less or equal to 1")

for (t in 1:N){
	#each time creates a matrix of dim = ncols x 11
	datanew <- data[data$indexTime == ( ceiling( (maxtime-1)/(N-1) *t + ( (N - maxtime )/(N-1) ) ) ),]
	#Plot, pdf better for latex
	pdf(paste(sprintf("Graphs/f1upT%d.pdf",t)))
	plot(datanew$ncolIndex,datanew$f1up)
	dev.off()
	#Change the column values so, they are proper to fit
	datafit <- datanew[,c("ncolIndex","f1up")]
	datafit$ncolIndex <- datanew$ncolIndex - ncols/2
	datafit$f1up <- datanew$f1up *2 - 1  
	plot(datafit$ncolIndex,datafit$f1up)
}



nlsfit <- nls( f1up ~ A*erf( sqrt(pi)*( ncolIndex -x0)/(2*W)),datafit ,start=list(A=-1,x0=1,W=1))


