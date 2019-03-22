# Paquete para la función confusionMatrix
# También tiene funciones para otras métricas como el error cuadrático medio
library(ModelMetrics)

# Inicializamos la semilla del generador de números aleatorios
set.seed(1)
data <- read.csv("titanic.csv")

# Eliminamos las columnas PassengerId, Name, Cabin y Ticket
# Eliminamos missing values
mask <- colnames(data) %in% c("PassengerId", "Name", "Cabin", "Ticket")
data_clean <- na.omit(data[, !mask])

# Dividimos en 80% entrenamiento y 20% test
idx   <- sample(nrow(data_clean), 0.8*nrow(data_clean))
train <- data_clean[ idx, ]
test  <- data_clean[-idx, ]

# Ajustamos regresión logística sobre todas las variables
# R automaticamente codifica los factores usando variables "dummy"
fit <- glm(Survived ~., data=train, family=binomial)

# Obtenemos predicciones y valores reales
prob <- predict(fit, test, type="response")
yreal <- test[, "Survived"]

# Calculamos tasa de acierto y matriz de confusión
acc <- mean((prob > 0.5) == yreal)
cm  <- confusionMatrix(yreal, prob, cutoff=0.5)

print(acc)
print(cm)
