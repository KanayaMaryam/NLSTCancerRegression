library(caret)
library(dplyr)
filename <- "data.csv"
dataset <- read.csv(filename, header = TRUE)

dataset <- subset(dataset, LungCancerDiagnosis.de_stag %in% c("Stage IIA", "Stage IIIB", "Stage IA", "Stage IIB", "Stage IV", "Stage IB", "Stage IIIA"))

dataset$LungCancerDiagnosis.de_stag <- ifelse(dataset$LungCancerDiagnosis.de_stag=="Stage IIA",2,as.character(dataset$LungCancerDiagnosis.de_stag))
dataset$LungCancerDiagnosis.de_stag <- ifelse(dataset$LungCancerDiagnosis.de_stag=="Stage IIIB",3,as.character(dataset$LungCancerDiagnosis.de_stag))
dataset$LungCancerDiagnosis.de_stag <- ifelse(dataset$LungCancerDiagnosis.de_stag=="Stage IA",1,as.character(dataset$LungCancerDiagnosis.de_stag))
dataset$LungCancerDiagnosis.de_stag <- ifelse(dataset$LungCancerDiagnosis.de_stag=="Stage IIB",2,as.character(dataset$LungCancerDiagnosis.de_stag))
dataset$LungCancerDiagnosis.de_stag <- ifelse(dataset$LungCancerDiagnosis.de_stag=="Stage IV",4,as.character(dataset$LungCancerDiagnosis.de_stag))
dataset$LungCancerDiagnosis.de_stag <- ifelse(dataset$LungCancerDiagnosis.de_stag=="Stage IB",1,as.character(dataset$LungCancerDiagnosis.de_stag))
dataset$LungCancerDiagnosis.de_stag <- ifelse(dataset$LungCancerDiagnosis.de_stag=="Stage IIIA",3,as.character(dataset$LungCancerDiagnosis.de_stag))

dataset = dataset[, names(dataset) %in% c("LungCancerDiagnosis.de_stag", "LungCancerDiagnosis.locrup", "LungCancerDiagnosis.locrmid", "LungCancerDiagnosis.locrlow", "LungCancerDiagnosis.loclup", "LungCancerDiagnosis.loclin", "LungCancerDiagnosis.locllow", "LungCancerDiagnosis.locrhil", "LungCancerDiagnosis.loclhil", "LungCancerDiagnosis.locrmsb", "LungCancerDiagnosis.loclmsb", "LungCancerDiagnosis.loccar", "LungCancerDiagnosis.locmed", "LungCancerDiagnosis.locoth", "LungCancerDiagnosis.locunk")]
temp = as.matrix(dataset)
temp[which(temp=="Yes")] <- 1
temp[which(temp=="No")] <- 0
dataset <- as.data.frame(temp)

print(nrow(dataset))
validation_index <- createDataPartition(dataset$LungCancerDiagnosis.de_stag, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]

control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

# a) linear algorithms
set.seed(7)
fit.lda <- train(LungCancerDiagnosis.de_stag~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(LungCancerDiagnosis.de_stag~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(LungCancerDiagnosis.de_stag~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(LungCancerDiagnosis.de_stag~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(LungCancerDiagnosis.de_stag~., data=dataset, method="rf", metric=metric, trControl=control)

results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
print(dotplot(results))


# estimate skill of LDA on the validation dataset
predictions <- predict(fit.svm, validation)
confusionMatrix(predictions, validation$LungCancerDiagnosis.de_stag)

