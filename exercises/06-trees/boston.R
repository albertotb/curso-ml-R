# Usaremos la librería gbm para ajustar un conjunto de árboles entrenados mediante boosting al
# dataset the casas de Boston
library (gbm)
set.seed(1)

# Cargamos el dataset Boston de la librería MASS
library(MASS)
Boston

# Dividimos en un 50% train y 50% test
train = sample (1:nrow(Boston), nrow(Boston)/2)


# Ajustamos un modelo para predecir el valor mediano en función del resto de variables,
# para 5000 iteraciones y 4 interacciones máximas en cada árbol
# Escoger el argumento distribution adecuado
model <- gbm(medv ~ .,data=Boston[train ,], distribution="gaussian",n.trees=5000, interaction.depth=4)


# Obtener la importancia de cada variable usando summary
summary(model)


# Obtener el gráfico de dependencia parcial para las dos variables más importantes,
# tanto de forma independiente como de forma conjunta las dos
plot(model, i="rm")
plot(model, i="lstat")

plot(model, i.var = c("rm","lstat"))


# Representar el RMSE sobre el conjunto de test en función del número de árboles utilizados
# en la predicción
num_iters <- seq(1,5000,10)
yhat.boost <- predict(model,newdata = Boston[-train ,], n.trees=num_iters)
rmses <- colMeans((yhat.boost - Boston$medv[-train])^2)
plot(num_iters, rmses)
