# ********************************************
# MY FUNCTION ERR VALS
# ********************************************
calculateErrorCapCalor <- function(Energy,Energy2,Temperatura,dim){

	numOfElements <- round( length(Energy)/2 ) 
	numofRepetitions <- 10
	cv <- array(0, dim=numofRepetitions)

	for (i in seq(numofRepetitions)) {

		#Pick numofelements elements with repetition
		pickedE <- sample(Energy,numOfElements,replace=T)
		pickedE2 <- sample(Energy2,numOfElements,replace=T)

		cv[i] <- ( mean(pickedE2) - mean(pickedE)^2 )/Temperatura^2/dim
	}
	error <- sqrt( mean(cv^2) - mean(cv)^2 ) #Eq 47 pag 40 Tesis Bootstrap method
	
	return(error)
}
calculateErrorSuscept <- function(Magnet,Magnet2,Temperatura,dim){

	numOfElements <- round( length(Magnet)/2 ) 
	numofRepetitions <- 10
	X <- array(0, dim=numofRepetitions)

	for (i in seq(numofRepetitions)) {

		#Pick numofelements elements with repetition
		pickedM <- sample(Magnet,numOfElements,replace=T)
		pickedM2 <- sample(Magnet2,numOfElements,replace=T)

		X[i] <- ( mean(pickedM2) - mean(pickedM)^2 )/Temperatura/dim
	}
	error <- sqrt( mean(X^2) - mean(X)^2 ) #Eq 47 pag 40 Tesis Bootstrap method
	return(error)
}
calculateErrorCumulant <- function(Magnet2,Magnet4){

	numOfElements <- round( length(Magnet2)/2 ) 
	numofRepetitions <- 10
	U4 <- array(0, dim=numofRepetitions)

	for (i in seq(numofRepetitions)) {

		#Pick numofelements elements with repetition
		pickedM2 <- sample(Magnet2,numOfElements,replace=T)
		pickedM4 <- sample(Magnet4,numOfElements,replace=T)

		U4[i] <- 1 - mean(pickedM4)/ ( 3*mean(pickedM2)^2 )
	}
	error <- sqrt( mean(U4^2) - mean(U4)^2 ) #Eq 47 pag 40 Tesis Bootstrap method
	return(error)
}