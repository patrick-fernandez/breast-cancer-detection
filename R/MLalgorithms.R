set.seed(2)
#train-test split
test_index <- createDataPartition(data$Diagnosis, times = 1, p = 0.30, list = FALSE)
train_set <- data %>% slice(-test_index)
test_set <- data %>% slice(test_index)
#five-fold cross-validation resampling
cv <- trainControl(method = "cv", number = 5, classProbs = TRUE,
                   savePredictions = TRUE,
                   summaryFunction = twoClassSummary,
                   selectionFunction = "oneSE")
#tune model
set.seed(2)
#normalize then apply PCA in preprocessing
preProcessing <- c("center", "scale", "pca")
##Use Specificity as metric to choose optimal model
#method is SVM Linear
svm <- train(Diagnosis ~ ., preProc = preProcessing, trControl = cv, 
             method = "nnet", metric = "Spec", data = train_set)
svm
svm$finalModel
y_hat_svm <- predict(svm, test_set)
confusionMatrix(y_hat_svm, test_set$Diagnosis, positive = "M")

res_svm <- evalm(svm, positive = "M")
## get ROC
res_svm$roc
## get calibration curve
res_svm$cc
## get precision recall gain curve
res_svm$prg

set.seed(2)
#method is Neural Network
nn <- train(Diagnosis ~ ., preProc = preProcessing, trControl = cv, 
                  method = "nnet", metric = "Spec", data = train_set)
nn
nn$finalModel
y_hat_nn <- predict(nn, test_set)
confusionMatrix(y_hat_nn, test_set$Diagnosis, positive = "M")
res_nn <- evalm(nn, positive = "M")
## get ROC
res_nn$roc
## get calibration curve
res_nn$cc
## get precision recall gain curve
res_nn$prg

set.seed(2)
#method is Boosted Logistic Regression
logit <- train(Diagnosis ~ ., preProc = preProcessing, trControl = cv, 
                 method = "LogitBoost", metric = "Spec", data = train_set)
logit
logit$finalModel
y_hat_logit <- predict(logit, test_set)
confusionMatrix(y_hat_logit, test_set$Diagnosis, positive = "M")

res_logit <- evalm(logit, positive = "M")
## get ROC
res_logit$roc
## get calibration curve
res_logit$cc
## get precision recall gain curve
res_logit$prg

set.seed(2)
#method is Random Forest
rf <- train(Diagnosis ~ ., preProc = preProcessing, trControl = cv, 
               method = "rf", metric = "Spec", data = train_set)
rf$results
rf$finalModel
y_hat_rf <- predict(rf, test_set)
confusionMatrix(y_hat_rf, test_set$Diagnosis, positive = "M")

res_rf <- evalm(rf, positive = "M")
## get ROC
res_rf$roc
## get calibration curve
res_rf$cc
## get precision recall gain curve
res_rf$prg

#method is Naive Bayes
naiveBayes <- train(Diagnosis ~ ., preProc = preProcessing, trControl = cv, 
            method = "naive_bayes", metric = "Spec", data = train_set)
naiveBayes
naiveBayes$finalModel
y_hat_naiveBayes <- predict(naiveBayes, test_set)
confusionMatrix(y_hat_naiveBayes, test_set$Diagnosis, positive = "M")

res_naiveBayes <- evalm(naiveBayes, positive = "M")
## get ROC
res_naiveBayes$roc
## get calibration curve
res_naiveBayes$cc
## get precision recall gain curve
res_naiveBayes$prg

#method is KNN
knn <- train(Diagnosis ~ ., preProc = preProcessing, trControl = cv, 
             method = "knn", metric = "Spec", data = train_set)
knn
knn$finalModel
y_hat_knn <- predict(knn, test_set)
confusionMatrix(y_hat_knn, test_set$Diagnosis, positive = "M")

res_knn <- evalm(knn, positive = "M")
## get ROC
res_knn$roc
## get calibration curve
res_knn$cc
## get precision recall gain curve
res_knn$prg

#############################################################

##Filter out highly correlated predictors 
#normalize then filter out highly correlated predictors
preProcessing <- c("center", "scale", "corr")
##Use Specificity as metric to choose optimal model
#method is SVM Linear
svm <- train(Diagnosis ~ ., preProc = preProcessing, trControl = cv, 
             method = "nnet", metric = "Spec", data = train_set)
svm
svm$finalModel
y_hat_svm <- predict(svm, test_set)
confusionMatrix(y_hat_svm, test_set$Diagnosis, positive = "M")

res_svm <- evalm(svm, positive = "M")
## get ROC
res_svm$roc
## get calibration curve
res_svm$cc
## get precision recall gain curve
res_svm$prg

set.seed(2)
#method is Neural Network
nn <- train(Diagnosis ~ ., preProc = preProcessing, trControl = cv, 
            method = "nnet", metric = "Spec", data = train_set)
nn
nn$finalModel
y_hat_nn <- predict(nn, test_set)
confusionMatrix(y_hat_nn, test_set$Diagnosis, positive = "M")
res_nn <- evalm(nn, positive = "M")
## get ROC
res_nn$roc
## get calibration curve
res_nn$cc
## get precision recall gain curve
res_nn$prg

set.seed(2)
#method is Boosted Logistic Regression
logit <- train(Diagnosis ~ ., preProc = preProcessing, trControl = cv, 
               method = "LogitBoost", metric = "Spec", data = train_set)
logit
logit$finalModel
y_hat_logit <- predict(logit, test_set)
confusionMatrix(y_hat_logit, test_set$Diagnosis, positive = "M")

res_logit <- evalm(logit, positive = "M")
## get ROC
res_logit$roc
## get calibration curve
res_logit$cc
## get precision recall gain curve
res_logit$prg

set.seed(2)
#method is Random Forest
rf <- train(Diagnosis ~ ., preProc = preProcessing, trControl = cv, 
            method = "rf", metric = "Spec", data = train_set)
rf$results
rf$finalModel
y_hat_rf <- predict(rf, test_set)
confusionMatrix(y_hat_rf, test_set$Diagnosis, positive = "M")

res_rf <- evalm(rf, positive = "M")
## get ROC
res_rf$roc
## get calibration curve
res_rf$cc
## get precision recall gain curve
res_rf$prg

#method is Naive Bayes
naiveBayes <- train(Diagnosis ~ ., preProc = preProcessing, trControl = cv, 
                    method = "naive_bayes", metric = "Spec", data = train_set)
naiveBayes
naiveBayes$finalModel
y_hat_naiveBayes <- predict(naiveBayes, test_set)
confusionMatrix(y_hat_naiveBayes, test_set$Diagnosis, positive = "M")

res_naiveBayes <- evalm(naiveBayes, positive = "M")
## get ROC
res_naiveBayes$roc
## get calibration curve
res_naiveBayes$cc
## get precision recall gain curve
res_naiveBayes$prg

#method is KNN
knn <- train(Diagnosis ~ ., preProc = preProcessing, trControl = cv, 
             method = "knn", metric = "Spec", data = train_set)
knn
knn$finalModel
y_hat_knn <- predict(knn, test_set)
confusionMatrix(y_hat_knn, test_set$Diagnosis, positive = "M")

res_knn <- evalm(knn, positive = "M")
## get ROC
res_knn$roc
## get calibration curve
res_knn$cc
## get precision recall gain curve
res_knn$prg