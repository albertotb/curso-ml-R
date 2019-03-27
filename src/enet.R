library(ModelMetrics)
# Interfaz del paquete glmnet para trabajar con fórmulas
library(glmnetUtils)

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

# Ajustamos modelo ElasticNet con alpha=0.5
# El lambda optimo se selecciona por validación cruzada
fit <- cv.glmnet(Survived ~ ., data=train, family="binomial", alpha=0.5)

# Resultados de la validación cruzada
plot(fit)

# Obtenemos predicciones para el lambda con menor error
prob <- predict(fit, test, type="response", s="lambda.min")
yreal <- test[, "Survived"]

# Calculamos tasa de acierto y matriz de confusión
acc <- mean((prob > 0.5) == yreal)
cm <- confusionMatrix(yreal, prob, cutoff=0.5)

print(acc)
print(cm)


