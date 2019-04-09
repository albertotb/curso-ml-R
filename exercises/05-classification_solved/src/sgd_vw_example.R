# Instalamos desde https://github.com/rvw-org/rvw-legacy
# remotes::install_github("rvw-org/rvw-legacy")
library(rvw)
library(dplyr)


## Recomendación de peliculas
recs_train <- read.csv('ml-100k/ua.base', sep = '\t',col.names = c('user', 'item', 'rating', 'time'))
recs_train <- select (recs_train,-c(time))


recs_test <- read.csv('ml-100k/ua.test', sep = '\t',col.names = c('user', 'item', 'rating', 'time'))
recs_test <-  select (recs_test,-c(time))

model_lin <- vw(training_data = recs_train, validation_data = recs_test, model = 'rec_in.vw',
                target = "rating", passes=3, b=18, loss='squared', link_function = "--link=identity" ,use_perf = FALSE, 
                l1=0.0001,learning_rate = 0.005,
                plot_roc = FALSE, do_evaluation = FALSE, extra = '')
rmse_lin <- sqrt(mean((recs_test$rating - model_lin$data)**2))

model_quad <- vw(training_data = recs_train, validation_data = recs_test, model = 'rec_quad.vw',
        target = "rating", passes=3, b=18, loss='squared', link_function = "--link=identity" ,use_perf = FALSE, 
        l1=0.0001,learning_rate = 0.005,
        plot_roc = FALSE, do_evaluation = FALSE, extra = '-q ::')
rmse_quad <- sqrt(mean((recs_test$rating - model_quad$data)**2))

model_lrq <- vw(training_data = recs_train, validation_data = recs_test, model = 'rec_lrq2.vw',
                   target = "rating", passes=3, b=18, loss='squared', link_function = "--link=identity" ,use_perf = FALSE, 
                l1=0.0001,learning_rate = 0.005,
                plot_roc = FALSE, do_evaluation = FALSE, extra = '--lrq ::10')
rmse_lrq <- sqrt(mean((recs_test$rating - model_lrq$data)**2))

print(rmse_lin)
print(rmse_quad)
print(rmse_lrq)

# install.packages('fastDummies')
library(fastDummies)
# install.packages('pryr')
library(pryr)

recs_dum <- fastDummies::dummy_cols(recs, select_columns = c('user', 'item'))

print(object_size(recs))
print(object_size(recs_dum))
