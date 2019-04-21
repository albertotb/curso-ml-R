#### LIBRARIES #################
library(readr)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)

#### LOAD FILES ################
data <- read.csv("data/train_titanic.csv")

dim(data)
summary(data)

#### DATA CLEANNING ############

# NA Age: 177
data$Age <- sapply(data$Age, FUN=function(x) {ifelse(is.na(x),median(data$Age, na.rm = TRUE),x)})


#Factor
factor_vars <- c('PassengerId','Pclass','Sex','Embarked','Cabin')
data[factor_vars] <- lapply(data[factor_vars], function(x) as.factor(x))

totrain = floor( nrow(data)*0.8  )
ind_train = sample(seq_len(nrow(data)), size = totrain)

train = data[ind_train, ]
test = data[-ind_train, ]
#### MODEL #####################

fit <- rpart(Survived ~ Pclass + Sex + Embarked + Cabin, train, method = "class", cp=0)
## method = "class" quiere decir que hacemos clasificaición
## cp = complexity parameter. Todo aquello que no baja la impuridad en cp no se intenta
summary(fit)
printcp(fit)
rpart.plot(fit, type=1, extra = 102)

preds = predict(fit, test, type = "class")
sum(preds == test$Survived)/nrow(test)
## Prune


pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
rpart.plot(pfit, type=1, extra = 102)

preds = predict(pfit, test, type = "class")
sum(preds == test$Survived)/nrow(test)
