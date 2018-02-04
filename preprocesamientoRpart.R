ova_ovo_classifier <- function(datos, k){
	ovo_class <- ovo_rpart(datos,k)
	ova_class <- ova_rpart(datos, k)

	list(ovo_class, ova_class)
}

ova_ovo_test <- function(test, modelos, k=4){
	ova_models = modelos[[1]]
	ovo_models = modelos[[2]]

	ova_preds <- ova_predict(test, ova_models)


}




################# OVO ############################
ovo_rpart <- function(datas, k=4){
	clasificadores <- data.frame(matrix(0,nrow = 15))
	combinaciones <- combn(k,2)

	for (combinacion in seq(ncol(combinaciones))){
		u = combinaciones[1, combinacion]-1
		v = combinaciones[2, combinacion]-1

		indices1 <- datas$y==u
		indices2 <- datas$y==v

		data_dummy <- datas[indices1 | indices2,]

		fit <- rpart(y~., data=data_dummy, method="class")

		clasificadores[[paste("a",u,v, sep="")]] <- fit
		clasificadores[[paste("a",v,u, sep="")]] <- fit

	}
	clasificadores
}

ovo_predict <- function(datos_originales, i, modelos_ovo, indices){
	muestra <- datos_originales[i,]
	modelo <- modelos_ovo[[paste("a",indices[[1]],indices[[2]], sep="")]]

	predict(modelo, muestra,type='class')

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

	for (i in seq(k)-1) {
		predicciones[,i+1] <- predict(fits[[i+1]], test, type='prob')[,2]
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