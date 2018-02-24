
#Carga el conjunto de datos de acuerto a la ruta relativa especificada.
load_train <- function(){
	read.csv("./Datos/my_dataset_train.csv")
}
#Carga el conjunto de test de acuerdo a la ruta relativa especificada.
load_test <- function(){
	read.csv("./Datos/my_dataset_test.csv")
}

#Dadas las predicciones que salen con predict, escribe la predicción en un fichero con el formato que pide Kaggle.
write_prediction <- function(y_1){

	y <- data.frame(seq(length(y_1)))
	y$y <- y_1

	names(y) <- c("Id", "Prediction")

	write.table(y, file = "submit.csv",
      quote = FALSE, row.names = FALSE,
      append = FALSE, sep = ",")
}
