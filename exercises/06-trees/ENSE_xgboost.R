library(dplyr)
library(xgboost)

data <- readxl::read_xlsx('../../data/DatosENSE.xlsx')
data$target = data$edCNO_AS == data$rawCNO_AS
#data <- select(data, -c(edCNO_AS, rawCNO_AS, CNAE_AS, CNAE2_AS))
data <- select(data, -c(edCNO_AS))

data <- data %>% mutate_if(is.character,as.factor)

vars = colnames(train)[1:ncol(train)-1]
sparse_matrix <- sparse.model.matrix(~ .,data = data[,vars])[,-1]


id_train <- sample(1:nrow(data),size=0.8*nrow(data))



#dtrain <- xgb.DMatrix(label = data$target[id_train], data = as.matrix(data[id_train, vars]))
bstSparse <- xgboost(data = sparse_matrix[id_train,], label = data$target[id_train], 
                     nrounds = 100,
                     subsample = 0.5,
                     colsample_bytree = 0.9,
                     max_depth  = 4,
                     eta = 0.3,
                     nthread = 4,objective="binary:logistic")

preds <- predict(bstSparse, sparse_matrix[-id_train,])


results = data.frame("prob_false" = 1-preds, "label" = data$target[-id_train])
res_sort = results[order(-results$prob_false),]


precision = function(df, depth){
  tot = dim(df)[1]
  inspect = floor(depth*tot)
  return(sum(df[1:inspect,2] == FALSE)/inspect)
}

recall = function(df, depth){
  tot = dim(df)[1]
  totF = sum(df[,2] == FALSE)
  inspect = floor(depth*tot)
  return(sum(df[1:inspect,2] == FALSE)/totF)
}


precRecallPlot = function(df, min = 0.0001, max = 0.5, step = 0.001){
  Recall = c()
  Precision = c()
  Depth = c()
  for(i in seq(min, max, step)){
    Depth = c(Depth, i)
    Recall = c(Recall, recall(df, i))
    Precision = c(Precision, precision(df, i))
  }
  
  results = data.frame(Depth = Depth, recall = Recall, precision = Precision)
  meltedResults = melt(results, id = "Depth")
  p = ggplot(meltedResults, aes(x = Depth, y = value, color = variable))
  p = p + geom_line() + xlab("Depth") + ylab("Value")
  p
}

precRecallPlot(res_sort)

