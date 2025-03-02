
##############################################
############### SCRIPT 1 - TCC ############### 
##############################################


# 1. Carregamento de bibliotecas necessárias

library(tidyverse)
library(tm)
library(stringi)
library(tidytext)
library(caret)
library(randomForest)
library(xgboost)
library(neuralnet)
library(text2vec)
library(word2vec)
library(pROC)

# 2. Carregamento dos dados e seleção das variáveis utilizadas nesse primeiro momento

dados <- readRDS("treino_balanceado.rds")

# Criação de uma coluna de Identificador para cada Tweet
dados = mutate(dados, ID = c(1:length(dados$TWEET)))
dados<-dplyr::select(dados, ID, TWEET, TIPO)
dados = dados[-c(917, 4951, 5209),]


# 3. Pré-processamento dos textos


# Tratamento dos textos

dados$TWEET = gsub(",", " ", dados$TWEET)
dados$TWEET = gsub("“", " ",  dados$TWEET)
dados$TWEET = gsub("”", " ",  dados$TWEET)
dados$TWEET = gsub("@[^ ]*", " ", dados$TWEET)
dados$TWEET = gsub("[0-9]", " ", dados$TWEET)
dados$TWEET = gsub("[[:punct:]]", " ", dados$TWEET)
#dados$TWEET = dados$TWEET |> removeNumbers()                            # Remoção de números
#dados$TWEET = dados$TWEET |> removePunctuation()                        # Remoção de pontuações
dados$TWEET <- dados$TWEET |> stripWhitespace()                          # Remoção de espaços em branco
dados$TWEET <- dados$TWEET |> tolower()                                  # Conversão para letras minúsculas
dados$TWEET <- gsub("http\\S+|www\\S+", "", dados$TWEET)                 # Remoção de URLs
dados$TWEET = stri_trans_general(str = dados$TWEET, 
                                 id = "Latin-ASCII")                     # Remoção de acentos

dados$TWEET <- stri_replace_all_regex(dados$TWEET, "[^\\p{ASCII}]", "")  # Remoção de emojis
dados$TWEET = sub("^\\s+", "", dados$TWEET)


eliminar = which(nchar(dados$TWEET)==1)

dados = dados[-eliminar,]


saveRDS(dados, "dados_treino_tratados.rds")










