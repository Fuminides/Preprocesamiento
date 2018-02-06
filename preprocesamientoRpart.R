require("rpart")
library(doParallel)
require(modeest)
require(DmwR)

##### Boosting #############
boosting_train <- function(datos, boosting=5, bagging=10,rows=1000){
	originales <- datos
	actuales <- originales
	modelos_finales <- list()

	for (i in seq(boosting)) {
		modelos <- bootstrap_parallel(actuales, bagging,row)
		predicciones <- mPred_paralel(actuales, modelos)

		indices <- predicciones!=actuales$y
		print(mean(indices))
		fallos <- actuales[indices,]
		actuales <- rbind(originales,fallos)

		modelos_finales[[i]] <- list(modelos)
	}

	modelos_finales

}

globalPred <- function(test, modelos){
	decisiones <- data.frame(matrix(0, ncol = length(modelos), nrow = nrow(test)))

	for (i in seq(length(modelos))) {
		decisiones[,i] <- (mPred_paralel(test, modelos[[i]][[1]]))
		rownames(decisiones) <- c()
	}

	decisiones <- apply(X=decisiones, MARGIN=1, FUN=function(x) mfv(as.numeric(x)))
	sapply(decisiones,function(x) x[[1]][[1]])
}

#### Bootstrap #############

generate_samples <- function(datos, cantidad, rows){
	muestras = list()
	for (i in seq(cantidad)) {
		muestras[[i]] <- dplyr::sample_n(datos, rows)
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


####Ova-OVO#################

ova_ovo_classifier <- function(datos, k=4){
	list(ovo_rpart(datos,k), ova_rpart(datos, k))
}

ova_ovo_predict <- function(test, modelos, k=4){
	modelos_ovo <- modelos[[1]]
	modelos_ova <- modelos[[2]]
	#POR ALGUN PUTO MOTIVO SI LO EJECUTO A MANO VA, Y SI NO PUES NO
	ova_preds <- ova_predict(test, modelos_ova, k, T)
	preds <- vector(mode = "list")

	sapply(X=seq(nrow(ova_preds)), FUN = function(x) ovo_predict(test, x, modelos_ovo, sec_max(ova_preds[x,])-1))
	#for (i in seq(nrow(ova_preds))) {
	#	preds[[i]] <- ovo_predict(test, i, modelos_ovo, sec_max(ova_preds[i,])-1)
	#}

	#preds
}

################# OVO ############################
ovo_rpart <- function(datas, k=4){
	clasificadores <- list()
	combinaciones <- combn(k,2)

	for (combinacion in seq(ncol(combinaciones))){
		u = combinaciones[1, combinacion]-1
		v = combinaciones[2, combinacion]-1

		indices1 <- datas$y==u
		indices2 <- datas$y==v

		data_dummy <- datas[indices1 | indices2,]

		fit <- rpart(y~., data=data_dummy, method="class")

		clasificadores[[paste("a",u,v, sep="")]] <- list(fit)
		clasificadores[[paste("a",v,u, sep="")]] <- list(fit)

	}
	clasificadores
}

ovo_predict <- function(datos_originales, i, modelos_ovo, indices){
	muestra <- datos_originales[i,]
	modelo <- modelos_ovo[[paste("a",indices[[1]],indices[[2]], sep="")]][[1]]
	f <- predict(modelo, muestra,type='class')
	as.numeric(levels(f))[f]
	#if(prediccion==1){
	#	indices[[1]]
	#}

	#indices[[2]]
}


############# OVA #################################
ova_rpart <- function(datas, k=4){
	modelos <- vector(mode = "list", length = k)

	for (i in seq(k)-1) {
		data_dummy <- datas
		data_dummy$y <- data_dummy$y==i

		fit <- rpart(y~., data=data_dummy, method="class")

		modelos[[i+1]] <-  fit
	}

	modelos
}

ova_predict <- function(test, fits, k=4, probs=F){
	predicciones <- as.data.frame(matrix(0, ncol = 4, nrow = nrow(test)))

	for (i in seq(length(fits))) {
		predicciones[,i] <- predict(fits[[i]], test, type='prob')[,2]
	}

	if (probs){
		predicciones
	}
	else{
		clases <- apply(predicciones, 1, function(x) which(x==max(x))-1)
		clases
	}
}



ova_predict_tree <- function(test, fits, datas=NULL, k=4, basic = T){
	predicciones <- as.data.frame(matrix(0, ncol = 4, nrow = nrow(test)))

	for (i in seq(k)-1) {
		predicciones[,i+1] <- predict(fits[[i+1]], test, type='prob')[,2]
	}

	predicciones <- apply(predicciones, 1, function(x) which(x==max(x))-1)

	if (basic){
		indices <- (predicciones==Inf)

		fit_basic <- rpart(y~., data=datas, method="class")
		predict_basic <- predict(fit_basic, test, type='class')
		pdb <- as.numeric(levels(predict_basic))[predict_basic]

		predicciones[indices] <- pdb[indices]
	}	

	predicciones
}


################################################################
normalize <- function(train){
	strain <- preProcess(train[, -76], method=c("center", "scale"))
	strain2 <- predict(strain, train[, -76])
	strain2$y <- train$y

	strain2
}

sec_max <- function(x){
	max1 <- which.max(x)
	x_dummy <- x
	x_dummy[max1] <- min(x)
	max2 <- which.max(x_dummy)

	c(max1,max2)
}

valores_imputados <- function(){
	train <- load_train()
	test <- load_test()
	y<- train[,76]
	joined <- rbind(train[,-76], test)
	prunned <- knnImputation(joined)

	train <- prunned[c(seq(nrow(train))), ]
	train$y <- y
	test <- prunned[c(seq(nrow(test)))+nrow(train), ]

	list(train, test)
}

nivelate <- function(datos){
	clases <- split(datos,datos$y)
	conteos <- sapply(X=clases,FUN=nrow)
	mas_comun <- max(conteos)
	resultado <- data.frame(matrix(0,nrow=0,ncol=ncol(datos)))

	for (i in seq(length(conteos))) {
		if(conteos[[i]] != mas_comun){
			niveladas <- dplyr::sample_n(clases[[i]], mas_comun, replace=TRUE)
			resultado <- rbind(resultado,niveladas)
		}
		else{
			resultado <- rbind(resultado,clases[[i]])
		}
	}

	resultado
}

dum <- valores_imputados()
train <- dum[[1]]
test <- dum[[2]]
rm(dum)
train <- dplyr::sample_frac(nivelate(train))
mini <- train[seq(100),]