require("rpart")
require(modeest)
require(DMwR)
require(ROSE)
require(mice)
require(caret)
require(dummies)

normalize <- function(train){
  strain <- preProcess(train[, -ncol(train)], method=c("center", "scale"))
  strain2 <- predict(strain, train[, -ncol(train)])
  strain2$y <- train$y
  
  strain2
}

normalize_test <- function(train){
  strain <- preProcess(train, method=c("center", "scale"))
  strain2 <- predict(strain, train)
  
  strain2
}

aux <- function(x) {if (is.factor(x)){x[(x == "")] = NA;droplevels(x)} else {x}}

valores_imputados <- function(train, test){
  y<- train[,ncol(train)]
  joined <- rbind(train[,-ncol(train)], test)
  joined <- as.data.frame(sapply(joined, aux))
  prunned <- mice(joined, printFlag=F) #Le lleva un rato, si
  prunned <- complete(prunned)
  train <- prunned[c(seq(nrow(train))), ]
  train$y <- y
  test <- prunned[c(seq(nrow(test)))+nrow(train), ]
  
  list(train, test)
}

write_prediction <- function(y_1){
  
  y <- data.frame(seq(length(y_1)))
  y$y <- y_1
  
  names(y) <- c("Id", "Prediction")
  
  write.table(y, file = "submit.csv",
              quote = FALSE, row.names = FALSE,
              append = FALSE, sep = ",")
}

train <- read.csv("~/my_dataset_train.csv")
test <- read.csv("~/my_dataset_test.csv")

train = load_train()
test = load_test()

u=train[,-76]
u=rbind(u,test)
class=as.factor(train[,70])

u <- dummy.data.frame(data=u)

train <- u[c(seq(nrow(train))), ]
train$y <- class
test <- u[c(seq(nrow(test)))+nrow(train), ]


train <- normalize(train)
test <- normalize_test(test)

l = valores_imputados(train, test)

train <- l[[0]]
test <- l[[1]]

u=rbind(train[,-ncol(train)], test)

results=knn.cv(u,class,k=1,1:length(class))
levels(results)=levels(class)
table <- table(results,class)
sum(diag(table))/sum(table)

pred <- knn(train =  train[,-ncol(train)], test = test, cl=train$y, k=1)
mean(train$y != pred)


