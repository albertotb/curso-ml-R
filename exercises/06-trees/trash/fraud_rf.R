library(randomForest)
library(reshape2)
library(ggplot2)

train = read.csv("../06-trees/data/reduced_train.csv")
test = read.csv("data/reduced_test.csv")

# vars = read.csv("variables.txt", header = FALSE)
# to_train = train[, vars$V1 ]
#to_train$y= train$FRAUDE

train[, c("X", "ID_TARJETA", "FC_AUTORIZACION")] = NULL
# train_ind <- sample(seq_len(nrow(train)), size = 50000)

#def_train = train[train_ind,]


def_train$FRAUDE = as.factor(def_train$FRAUDE)
rf = randomForest(FRAUDE ~ ., data = def_train, ntree = 100)

vars = colnames(def_train)
#vars = vars[1:length(vars) -1 ]

to_test = test[, vars]

pred_proba = predict(rf, to_test, type = "prob")

pred_proba = pred_proba[,2]

y_true = test$FRAUDE


results = data.frame("prob_fraude" = pred_proba, "fraude" = y_true)

res_sort = results[order(-results$prob_fraude),]
varImpPlot(rf, type=2)
varImpPlot(rf, type=1)

precision = function(df, depth){
  tot = dim(df)[1]
  inspect = floor(depth*tot)
  return(sum(df[1:inspect,2] == 1)/inspect)
}

recall = function(df, depth){
  tot = dim(df)[1]
  totF = sum(totF = df[,2])
  inspect = floor(depth*tot)
  return(sum(df[1:inspect,2] == 1)/totF)
}


precRecallPlot = function(df, min = 0.0001, max = 0.01, step = 0.00001){
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
