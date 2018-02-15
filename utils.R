
load_train <- function(){
	read.csv("./Datos/my_dataset_train.csv")
}

load_test <- function(){
	read.csv("./Datos/my_dataset_test.csv")
}

write_prediction <- function(y_1){

	y <- data.frame(seq(length(y_1)))
	y$y <- y_1

	names(y) <- c("Id", "Prediction")

	write.table(y, file = "submit.csv",
      quote = FALSE, row.names = FALSE,
      append = FALSE, sep = ",")
}

write_test <- function(fit){

}