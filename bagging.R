#### Bootstrap #############

generate_samples <- function(datos, cantidad, rows){
	muestras = list()
	for (i in seq(cantidad)) {
		muestras[[i]] <- smote(dplyr::sample_n(datos, rows, replace=TRUE))
	}

	muestras
}

bootstrap <- function(datos, n=100, r=1000){
	muestras <- generate_samples(datos,n,r)
	modelos <- list()
	for (i in seq(n)) {
		modelos[[toString(i)]] <- ova_ovo_classifier(muestras[[i]])
	}

	modelos
}

my_append <- function(vieja, nueva){
	vieja[[length(vieja)+1]] <- nueva
	return(vieja)
}

my_append2 <- function(vieja,nueva){
	append(list(vieja),list(nueva))
}
my_append3 <- function(vieja,nueva){
	c(vieja,nueva)
}

bootstrap_parallel <- function(datos, n=100, r=1000){
	muestras <- generate_samples(datos,n,r)
	modelos <- list()

	cores=detectCores()
	cl <- makeCluster(cores[1]-1) #not to overload your computer
	registerDoParallel(cl)

	modelos <- foreach(i=seq(n), .export = c("ova_ovo_classifier","ovo_rpart","ova_rpart","ova_predict", "ovo_predict","sec_max") , .packages="rpart", .combine=my_append3) %dopar% {
	   tempMatrix <- ova_ovo_classifier(muestras[[i]])
	   tempMatrix
	   #do other things if you want

	    #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
	}
	#stop cluster
	stopCluster(cl)
	modelos_finales <- list()
	z<-1
	for (i in seq(n)) {
		modelos_finales[[i]] <- list(modelos[[z]], modelos[[z+1]])
		z=z+2
	}
	#modelos[[n]] <- list(modelos[[n]], modelos[[n+1]])
	#modelos[[n+1]] <- NULL

	modelos_finales
}

ova_bootstrap_parallel <- function(datos, n=100, r=1000){
	muestras <- generate_samples(datos,n,r)
	modelos <- list()

	cores=detectCores()
	cl <- makeCluster(cores[1]-1) #not to overload your computer
	registerDoParallel(cl)

	modelos <- foreach(i=seq(n), .export = c("ova_ovo_classifier","ovo_rpart","ova_rpart","ova_predict", "ovo_predict","sec_max") , .packages="rpart", .combine=my_append3) %dopar% {
	   tempMatrix <- ova_rpart(muestras[[i]])
	   tempMatrix
	   #do other things if you want

	    #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
	}
	#stop cluster
	stopCluster(cl)

	modelos
	modelos_finales <- list()
	z<-1
	for (i in seq(n)) {
		modelos_finales[[i]] <- list(modelos[[z]], modelos[[z+1]], modelos[[z+2]], modelos[[z+3]])
		z=z+4
	}
	#modelos[[n]] <- list(modelos[[n]], modelos[[n+1]])
	#modelos[[n+1]] <- NULL

	modelos_finales
}

mPred <- function(test, modelos){
	decisiones <- data.frame(matrix(0, ncol = length(modelos), nrow = nrow(test)))

	for (i in seq(length(modelos))) {
		decisiones[,i] <- (ova_ovo_predict(test, modelos[[i]]))
		rownames(decisiones) <- c()
	}

	decisiones <- apply(X=decisiones, MARGIN=1, FUN=function(x) mfv(as.numeric(x)))
	sapply(decisiones,function(x) x[[1]][[1]])
}


mPred_paralel <- function(test, modelos){
	decisiones <- data.frame(matrix(0, ncol = length(modelos), nrow = nrow(test)))

	cores=detectCores()
	cl <- makeCluster(cores[1]-1) #not to overload your computer
	registerDoParallel(cl)

	decisiones <- foreach(i=seq(length(modelos)), .export = c("ova_ovo_predict","ova_predict", "ovo_predict","sec_max"), .errorhandling="remove",.combine=cbind) %dopar% {
	   tempMatrix = (ova_ovo_predict(test, modelos[[i]])) #calling a function
	   rownames(tempMatrix) <- c()
	   #do other things if you want

	   tempMatrix #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
	}
	#stop cluster
	stopCluster(cl)

	decisiones <- apply(X=decisiones, MARGIN=1, FUN=function(x) mfv(as.numeric(x)))
	sapply(decisiones,function(x) x[[1]][[1]])
}

ova_mPred_paralel <- function(test, modelos){
	decisiones <- data.frame(matrix(0, ncol = length(modelos), nrow = nrow(test)))

	cores=detectCores()
	cl <- makeCluster(cores[1]-1) #not to overload your computer
	registerDoParallel(cl)

	decisiones <- foreach(i=seq(length(modelos)), .export = c("ova_ovo_predict","ova_predict", "ovo_predict","sec_max"), .errorhandling="remove",.combine=cbind) %dopar% {
	   tempMatrix = (ova_predict(test, modelos[[i]], 4,F)) #calling a function
	   rownames(tempMatrix) <- c()
	   #do other things if you want

	   tempMatrix #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
	}
	#stop cluster
	stopCluster(cl)

	decisiones <- apply(X=decisiones, MARGIN=1, FUN=function(x) mfv(as.numeric(x)))
	sapply(decisiones,function(x) x[[1]][[1]])
}
