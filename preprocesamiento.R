require("rpart")
library(doParallel)
require(modeest)
require(DMwR)
require(ROSE)
require(mice)
require(caret)
source("utils.R")

#####k-fold#################
#Realiza k-fold dado un conjunto de datos para entrenar, y una función para clasificar y predecir.
kfold <- function(datos, mifit, mipredict, k=10){
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

################################################################
# FUNCIONES PREPROCESAMIENTO
################################################################
#Hace un filtro de ruido por comitee:
# Crea varios clasificadores C5.0, a partes de muestras aleatorias del conjunto inicial. Si ninguno la clasifica bien,
#entonces la muestra se considera ruido.
comitee <- function(train, k=3, strict=F){
	valido <- logical(length=nrow(train))
	rows <- 1000
	predicciones <- data.frame(matrix(0, ncol = k, nrow = nrow(train)))

	for (i in seq(k)) {
		muestra <- dplyr::sample_n(train, rows, replace=TRUE)
		muestra$y <- as.factor(muestra$y)
		fit <- C5.0(x = muestra[, -ncol(muestra)], y = muestra$y)
		#fit <- rpart(y~., data=muestra, method="class")
		predicciones[,i] <- predict(fit, train[, -ncol(muestra)], type="class")==train$y

	}
	if(!strict){
		apply(X=predicciones, MARGIN=1, FUN=sum)!=0	
	}
	else{
		apply(X=predicciones, MARGIN=1, FUN=sum)>k/2	
	}
	
}

#Normaliza los campos numericos del conjunto de datos.
normalize <- function(train){
	strain <- preProcess(train[, -ncol(train)], method=c("center", "scale"))
	strain2 <- predict(strain, train[, -ncol(train)])
	strain2$y <- train$y

	strain2
}

#Devuelve los dos valores maximos de un array
#(Funcion auxiliar para la seleccion de variables)
sec_max <- function(x){
	max1 <- which.max(x)
	x_dummy <- x
	x_dummy[max1] <- min(x)
	max2 <- which.max(x_dummy)

	c(max1,max2)
}

#Funcion auxiliar que convierte los niveles "" de los factores en NA.
aux <- function(x) {if (is.factor(x)){x[(x == "")] = NA;droplevels(x)} else {x}}

#Imputa los valores NA del conjunto de datos con MICE.
valores_imputados <- function(){
	train <- load_train()
	test <- load_test()
	y<- train[,76]
	joined <- rbind(train[,-76], test)
	joined <- as.data.frame(sapply(joined, aux))
	prunned <- mice(joined, printFlag=F) #Le lleva un rato, si
	prunned <- complete(prunned)
	train <- prunned[c(seq(nrow(train))), ]
	train$y <- y
	test <- prunned[c(seq(nrow(test)))+nrow(train), ]

	list(train, test)
}

#Hace oversampling simple de las clases menos comunes para equilibrar el dataset.
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

#Aplica smote al conjunto de datos.
smote <- function(datos){
	datos$y <- factor(datos$y)
	datos <- SMOTE(form=y~.,data=datos, perc.under = 500)
	datos$y <- as.numeric(levels(datos$y))[datos$y]
	datos
}

#Al cargar el fichero, se carga el conjunto de datos con el siguiente preprocesamiento:
#MICE para imputar NAs
#Comitee filter para eliminar outliers multivariable.
#Smote para balancear el conjunto de datos.
dum <- valores_imputados()
train <- dum[[1]]
test <- dum[[2]]
rm(dum)
train <- train[comitee(train, 12),]
train <- smote(train)
mini <- train[seq(100),] #Se coge un pedazo más pequeño para hacer pruebas.



#kfold(train, ova_ovo_rpart, ova_ovo_predict) #Ejemplo de llamada a kfold para rpart
#kfold(train, ova_ovo_glm, ova_ovo_predict) #Ejemplo de llamada a kfold para glm