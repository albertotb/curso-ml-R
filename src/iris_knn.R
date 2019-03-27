library(class)

n <- nrow(iris)

# muestreo aleatorio
idx <- sample(n, n*0.75)

# partir en conjuntos de entrenamiento y test
iris_train <- iris[idx, ]
iris_test <- iris[-idx, ]

# separar variables indenpendientes de la clase (variable respuesta)
# entrenamiento
iris_train_cl <- iris_train[, 5]
iris_train <- iris_train[, -5]

# test
iris_test_cl <- iris_test[, 5]
iris_test <- iris_test[, -5]

# modelo knn
iris_pred <- knn(iris_train, iris_test, iris_train_cl, k=3)

# error
error <- sum(iris_test_cl != iris_pred)/length(iris_test_cl)
