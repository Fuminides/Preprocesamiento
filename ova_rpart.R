####Ova-OVO#################

#Crea y entrena un clasificador OVO-OVA de rpart.
ova_ovo_rpart <- function(datos, k=4){
	list(ovo_rpart(datos,k), ova_rpart(datos, k))
}
#Genera una prediccion para el clasificador OVO-OVA
ova_ovo_predict <- function(test, modelos, k=4){
	modelos_ovo <- modelos[[1]]
	modelos_ova <- modelos[[2]]

	ova_preds <- ova_predict(test, modelos_ova, k, T)
	preds <- vector(mode = "list")

	sapply(X=seq(nrow(ova_preds)), FUN = function(x) ovo_predict(test, x, modelos_ovo, sec_max(ova_preds[x,])-1))
	#for (i in seq(nrow(ova_preds))) {
	#	preds[[i]] <- ovo_predict(test, i, modelos_ovo, sec_max(ova_preds[i,])-1)
	#}

	#preds
}

################# OVO ############################
#Entrena un conjunto de clasificadores OVO con Rpart.
ovo_rpart <- function(datas, k=4){
	clasificadores <- list()
	combinaciones <- combn(k,2)

	for (combinacion in seq(ncol(combinaciones))){
		u = combinaciones[1, combinacion]-1
		v = combinaciones[2, combinacion]-1

		indices1 <- datas$y==u
		indices2 <- datas$y==v

		data_dummy <- datas[indices1 | indices2,]
		#data_dummy <- smote(data_dummy)#ROSE(y ~ ., data=data_dummy)$data

		fit <- rpart(y~., data=data_dummy, method="class")

		clasificadores[[paste("a",u,v, sep="")]] <- list(fit)
		clasificadores[[paste("a",v,u, sep="")]] <- list(fit)

	}
	clasificadores
}

#Predice una muestra con un modelo OVO especificado.
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
#Entrena los modelos OVA de rpart.
ova_rpart <- function(datas, k=4){
	modelos <- vector(mode = "list", length = k)

	for (i in seq(k)-1) {
		data_dummy <- datas
		data_dummy$y <- as.factor(data_dummy$y==i)
		#data_dummy <- ROSE(y~.,data_dummy)$data

		fit <- rpart(y~., data=data_dummy, method="class")

		modelos[[i+1]] <-  fit
	}

	modelos
}
#Hace la prediccion de un modelo OVA con rpart.
ova_predict <- function(test, fits, k=4, probs=F){
	predicciones <- as.data.frame(matrix(0, ncol = 4, nrow = nrow(test)))

	for (i in seq(length(fits))) {
		predicciones[,i] <- predict(fits[[i]], test, type='prob')[,2]
	}

	if (probs){
		predicciones
	}
	else{
		#do.call(rbind,apply(predicciones, 1, function(x) which(x==max(x))))[,1]-1
		apply(predicciones, 1, function(x) which(x==max(x)))-1
	}
}


#Aplica el modelo OVA en forma de árbol, (de más frecuente a menos) según la clase.
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

