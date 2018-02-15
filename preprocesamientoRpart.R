require("rpart")
library(doParallel)
require(modeest)
require(DMwR)
require(ROSE)

#####k-fold#################
kfold_rpart <- function(datos, mifit, mipredict, k=10){
	flds <- createFolds(datos$y, k = 10, list = TRUE, returnTrain = FALSE)
	aciertos <- c(seq(k))

	for (i in seq(k)) {
		test <- datos[flds[[i]],]
		train <- datos[!(seq(length(datos)) %in% flds[[i]]),]
		model <- mifit(datos)
		preds <- mipredict(test, model)
		aciertos[[i]] <- mean(preds==test$y)
	}

	mean(aciertos)
}

#comitee filter
comitee <- function(train, k=3, strict=F){
	valido <- logical(length=nrow(train))
	rows <- 1000
	predicciones <- data.frame(matrix(0, ncol = k, nrow = nrow(train)))

	for (i in seq(k)) {
		muestra <- dplyr::sample_n(train, rows, replace=TRUE)
		muestra$y <- as.factor(muestra$y)
		fit <- C5.0(x = muestra[, -ncol(muestra)], y = muestra$y)#fit <- rpart(y~., data=muestra, method="class")
		predicciones[,i] <- predict(fit, train[, -ncol(muestra)], type="class")==train$y

	}
	if(!strict){
		apply(X=predicciones, MARGIN=1, FUN=sum)!=0	
	}
	else{
		apply(X=predicciones, MARGIN=1, FUN=sum)>k/2	
	}
	
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

smote <- function(datos){
	datos$y <- factor(datos$y)
	datos <- SMOTE(form=y~.,data=datos, perc.under = 500)
	datos$y <- as.numeric(levels(datos$y))[datos$y]
	datos
}

dum <- valores_imputados()
train <- dum[[1]]
test <- dum[[2]]
rm(dum)
levels(train$x0)[1] <- "missing"
levels(train$x14)[1] <- "missing"
levels(train$x17)[1] <- "missing"
levels(train$x51)[1] <- "missing"
levels(train$x61)[1] <- "missing"
levels(train$x63)[1] <- "missing"
train <- train[comitee(train, 12),]
train <- smote(train)
mini <- train[seq(100),]