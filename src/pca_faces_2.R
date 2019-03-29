#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")
library(EBImage)

# Descargar datos desde https://drive.google.com/file/d/14f9gJ3SrT2zE8iokixzZBOalvZ4U7YGg/view?usp=sharing
# y descomprimir, luego poner ruta en path:
faces_files <- list.files(path = "data/thumbnails128x128", pattern = ".png", 
                        all.files = TRUE,full.names = TRUE,no.. = TRUE)

faces <- readImage(faces_files)
plot(faces[,,,10])

faces_flat <- t(array(faces, dim=c(128^2*3, 4466)))

# 2 mins con alrededor de 5000 caras
proy_faces <- prcomp(faces_flat[, 1:(128^2*3)], center = FALSE)

eigvals <- proy_faces$sdev^2
ratio <- eigvals / sum(eigvals)
ratio_acum <- cumsum(ratio)

M = which(ratio_acum >= 0.99)[1]
faces_recons_flat <- proy_faces$x[,1:M] %*% t(proy_faces$rotation[,1:M])
faces_recons_flat[faces_recons_flat<0] <- 0
faces_recons_flat[faces_recons_flat>1] <- 1

faces_recons <- array(t(faces_recons_flat), dim=dim(faces))
fs = Image(faces_recons, colormode = 'Color')
plot((fs[,,,10]))


