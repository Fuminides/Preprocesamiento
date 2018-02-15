#### Boosting #############
boosting_train <- function(datos, boosting=5, bagging=10,rows=1000){
	originales <- datos
	actuales <- originales
	modelos_finales <- list()

	for (i in seq(boosting)) {
		modelos <- bootstrap_parallel(actuales, bagging,rows)
		predicciones <- mPred_paralel(actuales, modelos)

		indices <- predicciones!=actuales$y
		print(mean(indices))
		fallos <- actuales[indices,]
		actuales <- rbind(originales,fallos)


		modelos_finales[[i]] <- list(modelos)
	}

	modelos_finales

}

ova_boosting_train <- function(datos, boosting=5, bagging=10,rows=1000){
	originales <- datos
	actuales <- originales
	modelos_finales <- list()

	for (i in seq(boosting)) {
		modelos <- ova_bootstrap_parallel(actuales, bagging,rows)
		predicciones <- ova_mPred_paralel(actuales, modelos)

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

ova_globalPred <- function(test, modelos){
	decisiones <- data.frame(matrix(0, ncol = length(modelos), nrow = nrow(test)))

	for (i in seq(length(modelos))) {
		decisiones[,i] <- (ova_mPred_paralel(test, modelos[[i]][[1]]))
		rownames(decisiones) <- c()
	}

	decisiones <- apply(X=decisiones, MARGIN=1, FUN=function(x) mfv(as.numeric(x)))
	sapply(decisiones,function(x) x[[1]][[1]])
}
