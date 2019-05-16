## Definición del modelo

# Representaremos el dado insesgado como I y el dado trucado como T
s <- c("I", "T")

# Definimos la matriz de transiciones A, representando las probabilidades de que el casino cambie de dado
# A es de tamaño 2 x 2
A <- t(matrix(data = log(c(0.95, 0.05, 0.1, 0.9)), nrow = 2, ncol = 2, dimnames = list(c("I", "T"), c("I", "T"))))

# Definimos la matriz de emisiones B, indicando las probabilidades de sacar cada cara de cada dado
# B es de tamaño 6 x 2
B <- matrix(data = log(c(rep(1/6, 6), c(rep(1/10, 5), 1/2))), nrow = 6, ncol = 2, dimnames = list(seq(1,6), c("I", "T")))

# Definimos el prior sobre el estado oculto.
pi <- matrix(data = log(c(0.5, 0.5)), nrow = 2, ncol = 1, dimnames = list(c("I", "T"), ""))


observaciones<-c(6,6,4,1,5,3,2,1,6,1,6,2,1,1,5,2,3,4,6,5,3,2,1,4,3,5,6,6,3,4,2,6,1,6,5,5,2,3,4,2,3,2,3,1,5,1,4,2,4,
                   6,4,1,5,6,6,6,3,2,4,6,2,6,6,5,6,5,6,3,6,5,6,4,6,6,3,1,5,6,3,4,6,3,6,4,5,6,5,1,4,3,1,3,2,6,1,6,3,3,
                   1,2,3,6,1,6,6,5,4,3,6,1,1,5,1,3,3,3,2,1,2,6,6,6,5,2,6,5,6,1,5,2,2,4,2,4,4,3,4,2,5,3,3,5,6,1,2,3,5,
                   6,6,6,3,6,3,2,6,3,6,1,6,6,2,3,3,5,3,2,3,2,6,2,4,6,4,6,6,5,5,6,6,3,1,2,1,3,1,4,3,6,6,4,2,4,6,3,4,2,
                   3,3,2,4,2,3,4,5,3,6,6,1,2,3,2,4,6,6,4,5,2,3,5,6,4,2,1,5,1,4,6,6,1,5,1,4,6,2,2,1,4,5,5,6,1,4,4,4,6,
                   1,5,5,4,3,5,1,3,4,5,1,2,3,6,6,6,2,6,6,4,6,4,6,5,6,6,1,2,4,6,4,2,6,5,6,2,6,5,1,5,2,1,1,1,6,6,6,6,6,
                   4,6,3,3,4,6)

#####

probs  <- data.frame()
estados <- data.frame()

# we start by calculating the probability of being in particular state given the first symbol and initial matrix
# notice a change in log space - every multiplication is converted to summation
probs  <- rbind(probs, unlist(lapply(s, function(k) {
  pi[k, 1] + B[observaciones[1], k]
})))

estados <- rbind(estados, s)

colnames(probs)  <- c("I", "T")
colnames(estados) <- c("I", "T")

###

V <- matrix( nrow = length(observaciones), ncol = 2)
Ptr <- matrix( nrow = length(observaciones), ncol = 2)
Ptr[1,] = c(1, 2)

V[1, ] <- B[observaciones[1],] + pi

for (t in 2:length(observaciones)) {
  V[t, ] <- apply(B[observaciones[t], ] + A[,] + V[t-1,], 2, max)
  Ptr[t, ] <- apply(B[observaciones[t], ] + A[,] + V[t-1,], 2, which.max)
}


x_max <- rep(0, length(observaciones))
x_max[length(observaciones)] <- which.max(V[length(observaciones), ])
for (t in (length(observaciones)-1):1) {
  x_max[t] <- Ptr[t, x_max[t+1]]
}

viterbi.path <- s[x_max]

#####
real.path<-c("T","T","T","T","T","T","T","T","T","T","T","T","T","T","I","I","I","I","I","I","T","T","T","T","T",
             "T","T","T","T","T","T","T","T","T","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I",
             "I","I","T","T","T","T","T","T","T","T","T","T","T","T","T","T","T","I","I","I","T","T","T","T","T",
             "I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I",
             "I","I","I","T","T","T","T","T","T","T","T","T","T","T","T","T","T","T","T","T","T","T","T","T","T",
             "T","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","T",
             "T","T","T","T","T","T","T","T","T","I","I","I","I","I","T","T","I","I","I","I","I","I","I","I","I",
             "I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I",
             "I","I","I","I","I","I","I","I","I","I","T","T","T","T","I","I","I","I","I","I","I","I","I","I","I",
             "T","T","T","T","T","T","T","T","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I",
             "I","I","I","I","I","I","I","I","T","T","T","T","I","I","I","I","I","I","I","I","T","T","T","T","T",
             "T","T","T","T","T","T","T","T","I","I","I","T","T","T","T","T","T","T","T","T","T","I","I","T","T")

# Calculamos la tasa de acierto
viterbi.table <- table(viterbi.path == real.path)
cat(paste(round(viterbi.table["TRUE"] / sum(viterbi.table) * 100, 2), "% accuracy\n", sep = ""))