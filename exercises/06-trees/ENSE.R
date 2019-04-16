library(dplyr)
library(gbm)

# Cargamos los datos, creamos el target y tiramos una columna
data <- readxl::read_xlsx('../../data/DatosENSE.xlsx')
data$target = data$edCNO_AS == data$rawCNO_AS
data <- select(data, -c(edCNO_AS))
data <- data %>% mutate_if(is.character,as.factor)

id_train <- sample(1:nrow(data),size=0.8*nrow(data))

model <- gbm(target ~ . ,data = data[id_train,],distribution = "bernoulli",n.trees = 10000,
    shrinkage = 0.01, interaction.depth = 4)

summary(model)


n.trees = seq(from=100, to=10000, by=100) 
#Generamos predicciones para cada número de árboles
predmatrix<-predict(model,data[-id_train,],n.trees = n.trees, type='response')
predmatrix[ predmatrix > 0.5] = 1
predmatrix[ predmatrix < 0.5] = 0
dim(predmatrix) 

#Calculamos la tasa de fallo
test.error<-colMeans(predmatrix != data$target[-id_train])
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged


plot(n.trees, test.error, pch=19, col="blue", xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

mean(data$target[-id_train])
