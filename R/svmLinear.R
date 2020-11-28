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
#Use Specificity to select the optimal model
data_fit <- train(Diagnosis ~ ., preProc=c("center", "scale"), trControl = cv, 
                  method = "svmLinear", metric = "Spec", data = train_set)

data_fit$results
data_fit$finalModel
y_hat <- predict(data_fit, test_set)
confusionMatrix(y_hat, test_set$Diagnosis, positive = "M")

res <- evalm(data_fit, positive = "M")
## get ROC
res$roc
## get calibration curve
res$cc
## get precision recall gain curve
res$prg
