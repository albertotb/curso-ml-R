library(dplyr)
library(randomForest)

# Cargamos los datos, creamos el target y tiramos una columna
data <- readxl::read_xlsx('../../data/DatosENSE.xlsx')
data$target = data$edCNO_AS == data$rawCNO_AS
data <- select(data, -c(edCNO_AS, rawCNO_AS, CNAE_AS, CNAE2_AS))
data <- data %>% mutate_if(is.character,as.factor)

data$target = as.factor(data$target)

id_train <- sample(1:nrow(data),size=0.8*nrow(data))
train = data[id_train, ]
test = data[-id_train,]

vars = colnames(train)[1:ncol(train)-1]

model <- tuneRF(data[,vars], data$target, ntreeTry=50, stepFactor=2, improve=0.05,
                trace=TRUE, plot=TRUE, doBest = TRUE)

vars = colnames(train)
to_test = test[, vars]
pred_proba = predict(model,test, type="prob")
pred_proba = pred_proba[,1]

results = data.frame("prob_false" = pred_proba, "label" = test$target)
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

