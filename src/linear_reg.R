data <- read.csv("diamonds.csv")

# Inicializamos la semilla del generador de números aleatorios
set.seed(1)

# Dividimos en 60% entrenamiento y 40% test
idx   <- sample(nrow(data), 0.6*nrow(data))
train <- data[ idx, ]
test  <- data[-idx, ]

# Ajustamos regresión lineal
fit <- lm(price ~ ., data=train)

# Calculamos error cuadrático medio de entrenamiento
err_train <- mean(residuals(fit)^2)
print(err_train)

# Calculamos error cuadrático medio de test
yreal <- test[, "price"]
ypred <- predict(fit, test)

err_test <- mean((yreal - ypred)^2)
print(err_test)