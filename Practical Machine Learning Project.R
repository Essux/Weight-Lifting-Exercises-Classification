## ----setup, include=FALSE-------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(reshape2)
library(caret)
library(randomForest)


## ----include=FALSE--------------------------------------------------------------------------------------
pml.training <- read.csv("C:/Users/juanj/Desktop/Data Science Specialization/Practical Machine Learning/Project/pml-training.csv", stringsAsFactors=TRUE, na.strings=c("", "NA","#DIV/0!"))


## ----clean_data-----------------------------------------------------------------------------------------
dim(pml.training)
pml.training <- pml.training[ , colSums(is.na(pml.training)) == 0]
pml.training$cvtd_timestamp <- as.Date(as.character(pml.training$cvtd_timestamp), format = "%d/%m/%Y %H:%M")
pml.training <- pml.training[, c(-1, -5)]
dim(pml.training)


## ----class_proportions_plot-----------------------------------------------------------------------------
ggplot(pml.training, aes(x = classe)) + geom_bar(fill="red") + ggtitle("Class Proportions")


## ---- echo=F--------------------------------------------------------------------------------------------
cor.matrix <- melt(cor(pml.training[,c(-1, -4, -58)]))
cor.matrix <- cor.matrix[cor.matrix$Var1 != cor.matrix$Var2, ]
colnames(cor.matrix)[3] <- "R"
cor.matrix$R2 <- (cor.matrix$R)^2
cor.matrix <- cor.matrix[order(cor.matrix$R2, decreasing = T), ]

cor.strings <- data.frame(Var1 = as.character(cor.matrix$Var1), Var2 = as.character(cor.matrix$Var2))
cor.strings$A <- apply(cor.strings, 1, min)
cor.strings$B <- apply(cor.strings, 1, max)
cor.matrix$A <- cor.strings$A
cor.matrix$B <- cor.strings$B
cor.matrix <- unique(cor.matrix[, c("A", "B", "R", "R2")])
head(cor.matrix)


## -------------------------------------------------------------------------------------------------------
ggplot(pml.training, aes(x=num_window, fill=user_name)) + geom_histogram(binwidth = 1) + ggtitle("Num window per user")


## -------------------------------------------------------------------------------------------------------
ggplot(pml.training, aes(x=num_window, fill=classe)) + geom_histogram(binwidth = 1) + ggtitle("Num window per class")


## ---- warning=F-----------------------------------------------------------------------------------------
ggplot(pml.training, aes(x=num_window, fill=classe)) + geom_histogram(binwidth = 1) + ggtitle("Num window per class") + xlim(400, 450)


## -------------------------------------------------------------------------------------------------------
set.seed(1234)
inTrain <- createDataPartition(pml.training$classe, p = 0.8)[[1]]
training <- pml.training[inTrain, ]
testing <- pml.training[-inTrain, ]


## -------------------------------------------------------------------------------------------------------
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 3)


## -------------------------------------------------------------------------------------------------------
modelTree <- train(
  classe ~ num_window,
  training,
  method = "rpart",
  trControl = fitControl,
  tuneGrid = data.frame(cp = c(0.001, 0.005, 0.01))
) 


## -------------------------------------------------------------------------------------------------------
modelTree


## -------------------------------------------------------------------------------------------------------
postResample(pred = predict(modelTree, newdata = testing), testing$classe)
table(predict(modelTree, newdata = testing), testing$classe)


## -------------------------------------------------------------------------------------------------------
training <- training[, c(-1, -2, -3, -4, -5)]
colnames(training)


## -------------------------------------------------------------------------------------------------------
set.seed(8877)
# 10-fold CV
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 3)

modelTree <- train(
  classe ~ .,
  training,
  method = "rpart",
  trControl = fitControl,
  tuneGrid = data.frame(cp = c(0.00001, 0.00005, 0.0001, 0.0005, 0.001, 0.005))
)


## -------------------------------------------------------------------------------------------------------
set.seed(9191)
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 1) 

tuneGrid <- expand.grid(.trials = c(5, 7, 9),
                       .model = c("tree"),
                       .winnow = c(F))

modelC5 <- train(
  classe ~ .,
  training,
  method = "C5.0",
  trControl = fitControl,
  verbose=T,
  tuneGrid = tuneGrid
)


## -------------------------------------------------------------------------------------------------------
set.seed(2424)
modelRandomForest <-  randomForest(classe ~ ., data = training, ntree = 100)
modelRandomForest


## -------------------------------------------------------------------------------------------------------
postResample(predict(modelRandomForest, newdata = testing), testing$classe)

