# Cargamos primero los paquetes necesarios: keras y tensorflow
library(keras)
library(tensorflow)

# Para instalarlos:
# devtools::install_github("rstudio/keras")
# library(keras)
# install_keras()

# Cargamos después los datos que son las tasas de interés mensuales de USA de Enero 2007 
# hasta hoy, obtenidos  de https://stats.oecd.org/viewhtml.aspx?datasetcode=MEI_FIN&lang=en
df <- read.csv('data.csv')

# Mostramos las primeras  observaciones y dibujamos la serie temporal
Series <- df$Value
head(Series)
plot(Series, typ='l',main="Datos", 
     xlab="Mes", ylab="Tasa de interés USA")


# Preparamos los datos. Los diferenciamos
diffed = diff(Series, differences = 1)
head(diffed)

# Construimos los datos retardados
lag_transform <- function(x, k= 1){
  
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}
supervised = lag_transform(diffed, 1)
head(supervised)


# Partimos los datos en conjunto de entrenamiento (70%) y de test (30%)
N = nrow(supervised)
n = round(N *0.7, digits = 0)
train = supervised[1:n, ]
test  = supervised[(n+1):N,  ]


# Normalizamos los datos entre -1 y 1
scale_data = function(train, test, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  std_test  = ((test - min(x) ) / (max(x) - min(x)  ))
  
  scaled_train = std_train *(fr_max -fr_min) + fr_min
  scaled_test = std_test *(fr_max -fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
  
}
Scaled = scale_data(train, test, c(-1, 1))

y_train = Scaled$scaled_train[, 2]
x_train = Scaled$scaled_train[, 1]

y_test = Scaled$scaled_test[, 2]
x_test = Scaled$scaled_test[, 1]


# Transformación inversa para deshacer el reescalado anterior (hará falta para mostrar las predicciones
# de manera más entendible)
invert_scaling = function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  t = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(t)
  
  for( i in 1:t){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}



# Pasamos el input a 3-dim para hacerlo comaptible con la librería keras
dim(x_train) <- c(length(x_train), 1, 1)

# Especificamos algunos parámetros
X_shape2 = dim(x_train)[2]
X_shape3 = dim(x_train)[3]
batch_size = 1                # Tamaño de los minilotes
units = 1                     # Número de unidades en la lstm

#=========================================================================================

# Definimos el modelo
model <- keras_model_sequential() 
model%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
  layer_dense(units = 1)

# Compilamos y mostramos el modelo
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
  metrics = c('accuracy')
)

summary(model)

# Ajustamos el modelo
Epochs = 50   
for(i in 1:Epochs ){
  model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model %>% reset_states()
}



# Hacemos las predicciones
L = length(x_test)
scaler = Scaled$scaler
predictions = numeric(L)
for(i in 1:L){
  X = x_test[i]
  dim(X) = c(1,1,1)
  yhat = model %>% predict(X, batch_size=batch_size)
  # deshacemos el reescalado
  yhat = invert_scaling(yhat, scaler,  c(-1, 1))
  # deshacemos la diferenciacion
  yhat  = yhat + Series[(n+i)]
  # guardamos
  predictions[i] <- yhat
}

# Dibujamos los valores
lines(c(rep(NA,135-40), predictions), col='red')

