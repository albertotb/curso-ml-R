# Instalando en Windows:
# install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)

# Instalando en Linux:
# sudo add-apt-repository -y "ppa:marutter/rrutter"
# sudo add-apt-repository -y "ppa:marutter/c2d4u"
# sudo apt-get update
# sudo apt-get install r-cran-rstan
# packageurl <- "http://cran.r-project.org/src/contrib/Archive/StanHeaders/StanHeaders_2.17.2.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

library(rstan)

# En función de los avisos, activamos opciones:
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Definimos el modelo en el archivo 8schools.stan
# Observa que hemos reparametrizado para que el sampler vaya más rápido

# Definimos los datos observados
schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

# Ajustamos el modelo, primero lo compilará a C y luego hará la inferencia (por defecto)
# con una variante de HMC, NUTS, ya que detecta que el modelo es diferenciable
# Los parámetros del sampler son todos por defecto de momento, pero se pueden especificar como
# argumentos adicionales
fit <- stan(file = '8schools.stan', data = schools_dat)

# Diagnósticos
print(fit)  # Como Rhat = 1, parece que ha convergido
plot(fit)
pairs(fit, pars = c("mu", "tau"))

la <- extract(fit, permuted = TRUE) # para obtener las muestras
mu <- la$mu 


# Algoritmos de inferencia alternativos: VI
s8 <- stan_model(file = '8schools.stan')  # vb no soporta leer el modelo desde fichero directamente
fit_vb <- vb(s8, data = schools_dat)

print(fit_vb)
plot(fit)
pairs(fit, pars = c("mu", "tau"))
