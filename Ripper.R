library (RWeka)
library(caret)
source("utils.R")

#####k-fold#################
#Realiza k-fold dado un conjunto de datos para entrenar, y una función para clasificar y predecir.
kfold <- function(datos, k=10){
	flds <- createFolds(datos$y, k = 10, list = TRUE, returnTrain = FALSE)
	aciertos <- c(seq(k))


	for (i in seq(k)) {
		test <- datos[flds[[i]],]
		labels <- as.numeric(levels(test$y))[test$y]
		train <- datos[!(seq(length(datos)) %in% flds[[i]]),]
		model <- JRip(y ~ ., data = train)
		preds <- predict(model, test)
		preds <- as.numeric(levels(preds))[preds]
		aciertos[[i]] <- mean(preds==labels)
	}

	mean(aciertos)
}

#source("preprocesamiento.R")

### read and explore the data
m1 = load_train()
str(m1)

###convert numeric class y to factor
m1$y <- as.factor(m1$y)

### Pre-Processing
### Normalize by class y
mydata <- Normalize(y ~., data = m1)
mydata

### Discretize by class y
mydata <- Discretize(y ~., data = m1)
mydata

### look at the distribution of the data y variable
### class variable
table(mydata$y)

### training a model on the data
### use the 1R implementation in the RWeka package called OneR()

### consider all possible features to predict $y
data_1R <- OneR(y ~ ., data = mydata)
data_1R

### evaluating model performance
summary(data_1R)

### improving model performance
### JRip() Java-based implementation of the Ripper
### rule learning algorithm
data_JRip <- JRip(y ~ ., data = mydata)

### RIPPER selected rules
data_JRip

### evaluating model performance
summary(data_JRip)
mean(mydata$y==predict(data_JRip, mydata))

kfold(mydata)

