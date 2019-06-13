library(caret)
library(dplyr)
library(readr)
library(tidyverse)
library(skimr)
library(broom)



trainRowNumbers<-createDataPartition(y=df2$OFFENSE_CODE_GROUP, p=0.50,
                                     list=FALSE)
trainingdf2<-df2[trainRowNumbers,] 
testingdf2<-df2[-trainRowNumbers,]
dim(trainingdf2)
dim(testingdf2)

preprocess<-preProcess(df2[,1:4], method = c("center", "scale"))

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


#train K-Nearest Neighbor (KNN) classification model 
knn_fit <- train(as.factor(OFFENSE_CODE_GROUP) ~., data = trainingdf2, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"))
knn_fit


#train Random Forest (RF) classification model
rf_fit <- train(as.factor(OFFENSE_CODE_GROUP) ~., data = trainingdf2, method = "rf",
                trControl=trctrl,
                preProcess = c("center", "scale"))
rf_fit

#train Radial Support Vector Machine (svmRadial) classification model
svm_fit <- train(as.factor(OFFENSE_CODE_GROUP) ~., data = trainingdf2, method = "svmRadial",
                 trControl=trctrl,
                 preProcess = c("center", "scale"))
svm_fit

###LDA

model_lda <- train(as.factor(OFFENSE_CODE_GROUP)~., data=trainingdf2, method="lda", trControl=trctrl,preProcess=c("center", "scale")) 
model_lda

##Classification and Regression Trees (CART) 

model_cart <- train(as.factor(OFFENSE_CODE_GROUP)~., data=trainingdf2, method="rpart", trControl=trctrl,preProcess=c("center", "scale"))
model_cart


#Validate models with test data and provide confusion matrices
test_pred <- predict(knn_fit, newdata = testingdf2)
confusionMatrix(as.factor(as.integer(test_pred)), as.factor(testingdf2$OFFENSE_CODE_GROUP) )


test_pred <- predict(rf_fit, newdata = testingdf2)
confusionMatrix(as.factor(as.integer(test_pred)), as.factor(testingdf2$OFFENSE_CODE_GROUP) )


test_pred <- predict(svm_fit, newdata = testingdf2)
confusionMatrix(as.factor(as.integer(test_pred)), as.factor(testingdf2$OFFENSE_CODE_GROUP) )

test_pred <- predict(model_lda, newdata = testingdf2)
confusionMatrix(as.factor(as.integer(test_pred)), as.factor(testingdf2$OFFENSE_CODE_GROUP) )

test_pred <- predict(model_cart, newdata = testingdf2)
confusionMatrix(as.factor(as.integer(test_pred)), as.factor(testingdf2$OFFENSE_CODE_GROUP) )

results <- resamples(list(knn=knn_fit, svm=svm_fit, rf=rf_fit,cart=model_cart))
summary(results)

dotplot(results)

scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)
