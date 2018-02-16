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
modelo23 <- "y~.-x17-x61-x0-x41-x51-x6-x50-x29-x26-x68-x30-x45-x67-x69-x37-x39-x38-x4-x58-x71-x70-x73-x66-x8-x65-x32-x36-x5-x25-x11-x63-x55-x14-x16-x49+I(x10^2)+I(x7^2)+I(x31^2)+I(x34^2)+I(x52^2)+I(x59^2)+I(x74^2)-x40-x2-x52-x34-x31-x57-x24-x47-x56-x43"#"y~.-x17-x61-x0-x41-x51-x6-x50-x29-x26-x68-x30-x45-x67-x69-x37-x39-x38-x4-x58-x71-x70-x73-x66-x8-x65-x32-x36-x5-x25-x11-x63-x55-x14-x16-x49+I(x10^2)+I(x7^2)+I(x9^2)+I(x31^2)+I(x34^2)+I(x52^2)+I(x59^2)+I(x74^2)+I(x59^3)+I(x74^3)"#"y~.-x17-x61-x0-x41-x51-x6-x50-x29-x26-x68-x30-x45-x67-x69-x37-x39-x38-x4-x58-x71-x70-x73-x66-x8-x65-x32-x36-x5-x25-x11-x63-x55-x14-x16-x49"
modelo31 <- "y~.-x61-x70-x51-x74-x63-x0-x25-x59-x67-x37-x1-x3-x19-x68-x44-x38-x29-x28-x5-x30-x33-x24-x41-x55-x58-x8-x31-x50-x17-x69-x43-x66-x10-x11-x47-x14-x4-x56-x73-x57"
modelo12 <- "y~.-x63-x61-x23-x24-x17-x22-x7-x36-x19-x3-x0-x4-x29-x69-x49-x72-x21-x20-x5-x12-x54-x6-x30-x14-x51-x74-x38-x16-x73-x10-x47-x41-x50-x68-x57-x31-x8-x58"
modelo02 <- "y~.-x61-x14-x36-x57-x17-x53-x51-x38-x44-x41-x62-x15-x39-x7-x22-x25-x58-x24-x0-x20-x70-x67-x12-x50-x49-x8-x29-x64-x42-x63-x30-x21-x55-x73-x68"
modelo10 <- "y~.-x61-x17-x14-x28-x48-x63-x15-x60-x43-x30-x29-x6-x5-x24-x0-x10-x44-x20-x42-x41-x25-x49-x68-x55-x56-x73-x54-x57-x51-x4-x8-x58-x31-x50-x38-x37-x2-x16-x47-x69-x12-x34"

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
			model <- glm(data=train, formula=paste(formula,"+I(x",(i-1),"^2)", sep=""),family="binomial")
			preds <- predict(fit, data1, type='response')
			print(mean((preds>0.5)==(train$y==nivel)))

			if(mean((preds>0.5)==(train$y==nivel)) > mejor){
				mejor <- mean((preds>0.5)==(train$y==nivel))
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
			preds <- predict(model, train, type='response')
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
	print(sort(summary(fit)$coef[,4], decreasing = T)[c(1,2)])

}
####Ova-OVO#################

ova_ovo_glm <- function(datos, k=4){
	list(ovo_glm(datos,k), ova_glm(datos, k))
}

ova_ovo_predict <- function(test, modelos, k=4){
	modelos_ovo <- modelos[[1]]
	modelos_ova <- modelos[[2]]
#
	ova_preds <- ova_predict(test, modelos_ova, k, T)
	preds <- vector(mode = "list")

	sapply(X=seq(nrow(ova_preds)), FUN = function(x) ovo_predict(test, x, modelos_ovo, sec_max(ova_preds[x,])-1))

	for (i in seq(nrow(ova_preds))) {
		preds[[i]] <- ovo_predict(test, i, modelos_ovo, sec_max(ova_preds[i,])-1)
	}

	preds
}

################# OVO ############################
ovo_glm <- function(datas, k=4){
	clasificadores <- list()
	combinaciones <- combn(k,2)

	for (combinacion in seq(ncol(combinaciones))){
		u = combinaciones[1, combinacion]-1
		v = combinaciones[2, combinacion]-1

		if(u<v){
			aux<-u
			u <- v
			v <- aux
		} 

		indices1 <- datas$y==u
		indices2 <- datas$y==v
		data_dummy <- sample_frac(datas[indices1 | indices2,])
		#data_dummy <- smote(data_dummy)#ROSE(y ~ ., data=data_dummy)$data

		if(u == 3 & v == 2 ){
			formula = modelo23
			data_dummy$y <- as.factor(data_dummy$y==2)
		}
		else if(u == 3 & v == 1 ){
			formula = modelo31
			data_dummy$y <- as.factor(data_dummy$y==3)
		}
		else if(u == 3 & v == 0 ){
			formula = modelo30
			data_dummy$y <- as.factor(data_dummy$y==3)
		}
		else if(u == 2 & v == 1 ){
			formula = modelo12
			data_dummy$y <- as.factor(data_dummy$y==1)
		}
		else if(u == 2 & v == 0 ){
			formula = modelo02
			data_dummy$y <- as.factor(data_dummy$y==0)
		}
		else if(u == 1 & v == 0 ){
			formula = modelo10
			data_dummy$y <- as.factor(data_dummy$y==1)
		}

		fit <- glm(formula, data=data_dummy, family="binomial")
		#fit <- penalized(data_dummy$y, penalized=~., data=data_dummy, lambda1 = 10, model="logistic")

		clasificadores[[paste("a",u,v, sep="")]] <- list(fit)
		clasificadores[[paste("a",v,u, sep="")]] <- list(fit)

	}

	clasificadores
}


ovo_predict <- function(datos_originales, i, modelos_ovo, indices){
	muestra <- datos_originales[i,]
	modelo <- modelos_ovo[[paste("a",indices[[1]],indices[[2]], sep="")]][[1]]

	f <- predict(modelo, muestra, type='response')>0.5
	#f <- predict(fit, penalized=~.,data=muestra)>0.5
	u <- indices[[1]]
	v <- indices[[2]]

	if(u<v){
		aux <- u
		u <- v
		v <- aux
	}

	#as.numeric(levels(f))[f]
	if(u == 3 & v == 2 ){
		ifelse(f, 2, 3)
	}
	else if(u == 3 & v == 1 ){
		ifelse(f, 3, 1)
	}
	else if(u == 3 & v == 0 ){
		ifelse(f, 3, 0)
	}
	else if(u == 2 & v == 1 ){
		ifelse(f, 1, 2)
	}
	else if(u == 2 & v == 0 ){
		ifelse(f, 0, 2)
	}
	else if(u == 1 & v == 0 ){
		ifelse(f, 1, 0)
	}

	#if(f==1){
	#	indices[[1]]
	#}
	#else{
	#	indices[[2]]
	#}
	#f[f==1] <- indices[[1]]
	#f[f==0] <- indices[[2]]

	#f
}
