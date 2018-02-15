require(caret)
require(dplyr)

write_test_glm <- function(fit){
	write_prediction(ova_predict(test, fit))
}

modelo0 <- "y~.-x73-x0-x17-x14-x21-x51-x30-x44-x27-x61-x62-x72-x71-x42-x6-x52-x54-x55-x68-x57-x8-x12-x18-x49-x20-x24-x36-x41-x34-x28-x31-x69"
modelo1 <- "y~.-x61-x51-x32-x68-x54-x55-x63-x41-x36-x0-x40-x37-x58-x33-x29-x22-x60-x35-x1-x14-x7-x31-x4-x49-x13-x48-x8-x38-x64-x52-x57-x24-x16-x56-x30-x74-x9-x45-x3-x28-x66"
modelo2 <- "y~.-x61-x17-x37-x29-x44-x50-x0-x26-x13-x14-x38-x39-x52-x73-x72-x67-x36-x30-x34-x48-x66-x20-x25-x51-x31-x58-x68-x69-x49-x16-x57-x10-x45-x42-x8"
modelo3 <- "y~.-x61-x21-x0-x17-x47-x70-x10-x39-x1-x37-x34-x11-x4-x8-x12-x68-x31-x69-x51-x55-x63-x2-x74-x6-x44-x41-x56-x19-x29-x38-x30-x24-x57-x14"

modelo30 <- "y~.-x64-x61-x17-x57-x14-x51-x52-x44-x11-x35-x24-x7-x0-x39-x29-x8-x1-x15-x6-x63-x30-x12-x36-x37-x55-x66-x69-x41-x73-x4-x68-x58-x49-x56-x47-x38-x21-x67-x62-x40-x23-x46"

############# OVA #################################
ova_glm <- function(datas, k=4, formula = "y~."){
	modelos <- vector(mode = "list", length = k)

	for (i in seq(k)-1) {
		data_dummy <- datas
		data_dummy$y <- as.factor(data_dummy$y==i)
		#datos <- SMOTE(form=y~.,data=data_dummy, perc.over = 300)
		
		#data_dummy <- ROSE(y~.,data_dummy)$data
		if(i==0){
			formula = modelo0
		}
		else if(i==1){
			formula = modelo1
		}
		else if(i==2){
			formula = modelo2
		}
		else if(i==3){
			formula = modelo3
		}

		fit <-glm(formula, data=data_dummy, family="binomial")

		modelos[[i+1]] <-  fit
	}

	modelos
}

ova_predict <- function(test, fits, k=4, probs=F){
	predicciones <- as.data.frame(matrix(0, ncol = 4, nrow = nrow(test)))

	for (i in seq(length(fits))) {
		predicciones[,i] <- predict(fits[[i]], test, type='response')
	}

	if (probs){
		predicciones
	}
	else{
		#do.call(rbind,apply(predicciones, 1, function(x) which(x==max(x))))[,1]-1
		apply(predicciones, 1, function(x) min(which(x==max(x))))-1
	}
}

ova_predict_doubt <- function(test, fits, k=4, probs=F){
	predicciones <- as.data.frame(matrix(0, ncol = 4, nrow = nrow(test)))

	for (i in seq(length(fits))) {
		predicciones[,i] <- predict(fits[[i]], test, type='response')>0.5
	}

	if (probs){
		predicciones
	}
	else{
		#do.call(rbind,apply(predicciones, 1, function(x) which(x==max(x))))[,1]-1
		apply(predicciones, 1, function(x) min(which(x==T)))-1
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

		fit_basic <- glm(y~., data=data_dummy, family="binomial")
		predict_basic <- predict(fit_basic, test, type='class')
		pdb <- as.numeric(levels(predict_basic))[predict_basic]

		predicciones[indices] <- pdb[indices]
	}	

	predicciones
}

level2_check <- function(formula){
	m_indice <- 0
	mejor <- 0
	for (i in (seq(75))) {
		print(i)
		if(is.numeric(train[,i])){
			model <- ova_glm(train, formula=paste(formula,"+I(x",(i-1),"^2)", sep=""))
			preds <- ova_predict(train, model)
			print(mean(preds==train$y))

			if(mean(preds==train$y) > mejor){
				mejor <- mean(preds==train$y)
				m_indice <- i
			}
		}

	}	
	c(m_indice, mejor)		
}


level3_check <- function(formula){
	m_indice <- 0
	mejor <- 0
	for (i in (seq(75))) {
		print(i)
		if(is.numeric(train[,i])){
			model <- ova_glm(train, formula=paste(formula,"+I(x",(i-1),"^3)", sep=""))
			preds <- ova_predict(train, model)
			print(mean(preds==train$y))

			if(mean(preds==train$y) > mejor){
				mejor <- mean(preds==train$y)
				m_indice <- i
			}
		}

	}	
	c(m_indice, mejor)		
}

best_interact <- function(indice){
	m_indice <- 0
	mejor <- 0
	for (i in (seq(75))) {
		print(i)
		if(is.numeric(train[,i])){
			model <- ova_glm(train, formula=paste("y~.+x",(indice-1),"*x",(i-1), sep=""))
			preds <- ova_predict(train, model)
			print(mean(preds==train$y))

			if(mean(preds==train$y) > mejor){
				mejor <- mean(preds==train$y)
				m_indice <- i
			}
		}

	}	
	c(m_indice, mejor)		
}

get_best <- function(m){
	best <- rownames(data.frame(summary(m)$coef[summary(m)$coef[,4] <= .05, 4]))
	paste(best, collapse = '+')
}

evaluate_fit <- function(formula, data1){
	fit <-glm(formula, data=data1, family="binomial")
	preds <- predict(fit, data1, type='response')
	print(summary(fit))
	print(mean((preds>0.5)==data1$y))
	print(sort(summary(fit)$coef[,4], decreasing = T)[1])

}

evaluate_fit_ovo <- function(formula, data1, nivel){
	fit <-glm(formula, data=data1, family="binomial")
	preds <- predict(fit, data1, type='response')
	print(summary(fit))
	print(mean((preds>0.5)==(data1$y==nivel)))
	print(sort(summary(fit)$coef[,4], decreasing = T)[1])

}
####Ova-OVO#################

#ova_ovo_glm <- function(datos, k=4){
#	list(ovo_glm(datos,k), ova_glm(datos, k))
#}

#ova_ovo_predict <- function(test, modelos, k=4){
#	modelos_ovo <- modelos[[1]]
##	modelos_ova <- modelos[[2]]
#
#	ova_preds <- ova_predict(test, modelos_ova, k, T)
#	preds <- vector(mode = "list")

#	sapply(X=seq(nrow(ova_preds)), FUN = function(x) ovo_predict(test, x, modelos_ovo, sec_max(ova_preds[x,])-1))
	#for (i in seq(nrow(ova_preds))) {
	#	preds[[i]] <- ovo_predict(test, i, modelos_ovo, sec_max(ova_preds[i,])-1)
	#}

	#preds
#}

################# OVO ############################
#ovo_glm <- function(datas, k=4){
#	clasificadores <- list()
#	combinaciones <- combn(k,2)
#
#	for (combinacion in seq(ncol(combinaciones))){
#		u = combinaciones[1, combinacion]-1
#		v = combinaciones[2, combinacion]-1
#
#		indices1 <- datas$y==u
#		indices2 <- datas$y==v
#
#		data_dummy <- sample_frac(datas[indices1 | indices2,])
#		#data_dummy <- smote(data_dummy)#ROSE(y ~ ., data=data_dummy)$data
#		data_dummy$y <- as.factor(data_dummy$y==u)
#
#		#fit <- glm(y~., data=data_dummy, family="binomial")
#		fit <- penalized(data_dummy$y, penalized=~., data=data_dummy, lambda1 = 10, model="logistic")
#
#		clasificadores[[paste("a",u,v, sep="")]] <- list(fit)
#		clasificadores[[paste("a",v,u, sep="")]] <- list(fit)
#
#	}
#	clasificadores
#}


#ovo_predict <- function(datos_originales, i, modelos_ovo, indices){
#	muestra <- datos_originales[i,]
#	modelo <- modelos_ovo[[paste("a",indices[[1]],indices[[2]], sep="")]][[1]]
	#f <- predict(modelo, muestra,type='class')
#	f <- predict(fit, penalized=~.,data=muestra)>0.5
	#as.numeric(levels(f))[f]
#	if(f==1){
#		indices[[1]]
#	}
#	else{
#		indices[[2]]
#	}
	#f[f==1] <- indices[[1]]
	#f[f==0] <- indices[[2]]

	#f
#}
