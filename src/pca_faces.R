# install.packages('RnavGraphImageData')
# install.packages("remotes")
# remotes::install_github("jlmelville/snedata")

library(snedata)
library(RnavGraphImageData)

# 400 images (with dimension 64 x 64) of 40 individual's faces, with ten different poses per person.
olivetti <- olivetti_faces()


# Show the second pose of the first face
plot_im <- function(sample) {
  #par(mar = rep(0, 4))
  m = (matrix(as.numeric(sample), 64, 64, byrow = TRUE))[, c(64:1)]
  image(m, axes = FALSE, col = grey(seq(0, 1, length = 256)))
}

face_id <- 2

plot_im(olivetti[face_id, 1:64^2])


proy_faces <- prcomp(olivetti[, 1:64^2], center = FALSE)

# Cálculo de la varianza explicada en función de la proyección
eigvals <- proy_faces$sdev^2
ratio <- eigvals / sum(eigvals)
ratio_acum <- cumsum(ratio)

M = 3
faces_recons <- proy_faces$x[,1:M] %*% t(proy_faces$rotation[,1:M])

plot_im(faces_recons[face_id, 1:64^2])



