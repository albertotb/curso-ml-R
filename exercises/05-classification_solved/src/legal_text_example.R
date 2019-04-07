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

# define preprocessing function and tokenization function
it_train = itoken(data$Fallos, 
                  preprocessor = tolower, 
                  tokenizer = word_tokenizer(), 
                  ids = data$idSentidosFallos, 
                  progressbar = TRUE)
vocab = create_vocabulary(it_train)
# nos quedams con palabras que al menos aparezcan 10 veces. 
# Cada palabra deberá estar al menos en el 0.1% de documentos
pruned_vocab = prune_vocabulary(vocab, 
                                term_count_min = 10, 
                                doc_proportion_min = 0.001)
vectorizer = vocab_vectorizer(pruned_vocab)

#dtm: document term matrix
dtm_data = create_dtm(it_train, vectorizer)
dim(dtm_data)

library(e1071)
model <- naiveBayes(x = as.matrix(dtm_data), y=data$Grupo)


