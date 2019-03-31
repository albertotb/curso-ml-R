# Cargamos el dataset COIL 20.
# install.packages("devtools")
# devtools::install_github("jlmelville/coil20")
library(coil20)
# coil20 <- download_coil20(verbose = TRUE)  # Tarda unos 3 min, solo hacerlo la primera vez
# Para que vaya más rápido, cargamos uno ya descargado antes
# saveRDS(coil20, file = "coil20.rds")
coil20 <- readRDS(file = 'coil20.rds')

#1440 imagenes en escala de gris a 128 x 128  
# (20 objetos con 72 poses cada), 
# con 16,384 features (128 x 128 px). 
dim(coil20)

# Algunos ejemplos
for (i in 0:72){
  show_object(coil20, object = 4, pose = i)
  Sys.sleep(0.05)
}

for (i in 0:72){
  show_object(coil20, object = 1, pose = i)
  Sys.sleep(0.05)
}

proy_pca <- prcomp(coil20[, 1:128^2], retx = TRUE)
# Representamos las dos primeras componentes
plot(proy_pca$x[, 1:2], type = 'n')
text(proy_pca$x[, 1:2], labels = coil20$Label, cex = 0.5,
     col = rainbow(length(levels(coil20$Label)))[coil20$Label])


#install.packages('Rtsne')
library(Rtsne)

proy_tsne <- Rtsne(coil20[, 1:128^2], num_threads = 16)

plot(proy_tsne$Y[, 1:2], type = 'n', xlab = 'TSNE1', ylab='TSNE2')
text(proy_tsne$Y[, 1:2], labels = coil20$Label, cex = 0.5,
     col = rainbow(length(levels(coil20$Label)))[coil20$Label])

