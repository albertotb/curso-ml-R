customRF <- list(type = "Classification", library = "randomForest", loop = NULL)

customRF$parameters <- data.frame(parameter = c("nodesize", "ntree", "mtry", "maxnodes"), 
                                  class = rep("numeric", 4), 
                                  label = c("nodesize", "ntree", "mtry", "maxnodes"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  if (is.na(param$maxnodes)) {
    param$maxnodes <- NULL
  }
  randomForest(x, y, 
               ntree=param$ntree, 
               nodesize=param$nodesize,
               mtry=param$mtry,
               maxnodes=param$maxnodes,
               ...)
}

customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)

customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")

customRF$sort <- function(x) x[order(x[,1]),]

customRF$levels <- function(x) x$classes

nfolds <- 5
cvIndex <- createFolds(y, nfolds, returnTrain = T)

control <- trainControl(index = cvIndex,
                        method="cv", 
                        number=nfolds, 
                        summaryFunction=twoClassSummary, 
                        classProbs=TRUE)

grid <- expand.grid(.mtry=c(floor(sqrt(ncol(X))), floor(log(ncol(X))), ncol(X)), 
                    .ntree=c(500, 1000, 1500),
                    .nodesize=c(1, 2, 4),
                    .maxnodes=c(NA, 4, 8))

# grid <- expand.grid(.mtry=c(5), 
#                     .ntree=c(100, 200),
#                     .nodesize=c(1),
#                     .maxnodes=c(NA))

X <- as.data.frame(X)
levels(y) <- c("No", "Yes")
metric <- "ROC"

cv <- train(X, y, 
            method=customRF, 
            metric=metric, 
            tuneGrid=grid, 
            trControl=control,
            strata=y, 
            sampsize=c(500, 500))

print(cv$results[order(cv$results[, metric], decreasing=TRUE), ])
print(cv$bestTune)
print(cv$times)

ytrue <- test$Diabetes1
ypred <- predict(cv$finalModel, newdata=test)

levels(ytrue) <- c("No", "Yes")
print(confusionMatrix(ypred, ytrue, positive="Yes"))
saveRDS(cv, file='cv.rds')
