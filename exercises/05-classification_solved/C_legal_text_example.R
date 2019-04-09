# Document vectorization in R
# install.packages('tidyverse')
library(text2vec)
library(tidyverse)
library(plyr)

# Descargamos y leemos los datos
download.file('https://github.com/vicgalle/neural-classifier/blob/master/data/Quantum1.xlsx?raw=true', 'data.xlsx')
data <- readxl::read_xlsx('data.xlsx')

# Echamos un vistazo
count(data, 'Grupo')
data$Fallos[1]

# Creamos split train - test
ind_train <- sample(1:nrow(data), 0.8*nrow(data))
data_train <- data[ind_train,]
data_test <- data[-ind_train,]

# Definimos el preprocesado y tokenizado
it_train = itoken(data_train$Fallos, 
                  preprocessor = tolower, 
                  tokenizer = word_tokenizer, 
                  ids = data_train$idSentidosFallos, 
                  progressbar = TRUE)
vocab = create_vocabulary(it_train)
# nos quedams con palabras que al menos aparezcan 10 veces. 
# Cada palabra deberá estar al menos en el 0.1% de documentos
pruned_vocab = prune_vocabulary(vocab, 
                                term_count_min = 10, 
                                doc_proportion_min = 0.001)
vectorizer = vocab_vectorizer(pruned_vocab)

#dtm: document term matrix
dtm_train = create_dtm(it_train, vectorizer)
dim(dtm_train)

## Clasificador
library(glmnet)
NFOLDS = 4
glmnet_classifier = cv.glmnet(x = dtm_train, y = data_train$Grupo, 
                              family = 'multinomial', 
                              # L1 penalty
                              alpha = 1,
                              type.measure = "class",
                              # 4-fold cross-validation
                              nfolds = NFOLDS,
                              # high value is less accurate, but has faster training
                              thresh = 1e-3,
                              # again lower number of iterations for faster training
                              maxit = 1e3)
plot(glmnet_classifier)



