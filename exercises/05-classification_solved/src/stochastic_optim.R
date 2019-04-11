#devtools::install_github("airoldilab/sgd")

library(sgd)
# Dimensions
N <- 1e2  # number of data points
d <- 2  # number of features

# Generate data.
X <- matrix(rnorm(N*d), ncol=d)
theta <- rep(5, d+1)
eps <- rnorm(N)
y <- cbind(1, X) %*% theta + eps
dat <- data.frame(y=y, x=X)

sgd.theta <- sgd(y ~ ., data=dat, model="lm")

lm.fit <- lm(y ~ ., data = dat)


# Compare sizes of batch-size

# Compare early stopping
