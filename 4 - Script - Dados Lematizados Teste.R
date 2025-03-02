

#####################################
###### BASE DE TESTE - PARTE 2 ######
#####################################

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


# Carregamento da base de teste tratada
dados_teste <- readRDS("dados_teste_tratados.rds")


# Tokenização na Base de Teste
dados_token_teste = tidytext::unnest_tokens(dados_teste,output=Palavras,input=TWEET)


## Lematização na Base de Teste

lemma_novo <- readRDS("lemma_novo.rds")

dados_lem_Teste = dplyr::left_join(dados_token_teste, lemma_novo, 
                                   by='Palavras')

dados_lem_Teste$Palavras = ifelse(is.na(dados_lem_Teste$stem),
                                  dados_lem_Teste$Palavras,dados_lem_Teste$stem)

dados_lem_Teste = dplyr::select(dados_lem_Teste,-stem)


#dim(dados_lem_Teste)

# Filtro para seleção dos mesmos termos utilizados no Treino

palavras_usadas <- readRDS("palavras_usadas.rds")

dados_lem_Teste2 = dados_lem_Teste %>% 
  inner_join(palavras_usadas, by = 'Palavras')


# Contagem de frequência

dados_lem_Teste2 = group_by(dados_lem_Teste2,Palavras)

freq = arrange(summarize(dados_lem_Teste2, frequencia = n()),
               desc(frequencia))

dados_lem_Teste2 = left_join(dados_lem_Teste2,freq,'Palavras')

dados_lem_Teste2 = ungroup(dados_lem_Teste2)


saveRDS(dados_lem_Teste2, "dados_lem_Teste.rds")



