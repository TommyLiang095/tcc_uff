

#############################################
############### SCRIPT 3 -TCC ############### 
#############################################

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


# Carregando os dados
dados_lem <- readRDS("dados_lem.rds")


# 4. Vetorização de Documentos

# 4.1 - Criação da Matriz Termo Documento(TF-IDF)


# Cálculo das frequências

dados_lem_tfidf <- dados_lem %>%
  bind_tf_idf(Palavras, ID, frequencia)


# Separação da variável resposta e transformando em dummies

TIPO_POR_ID <- dados_lem %>%
  select(ID, TIPO) %>%
  distinct() %>%
  mutate(TIPO = ifelse(TIPO == "NAO_OFENSIVO", 0,  # Atribui 0 para 'NAO_OFENSIVO'
                       ifelse(TIPO == "OFENSIVO", 1, # Atribui 1 para 'OFENSIVO'
                              ifelse(TIPO == "DISCURSO_DE_ODIO", 2, NA)))) # Atribui 2 para 'DISCURSO_DE_ODIO' e NA para outros valores

variavel_resposta <- TIPO_POR_ID$TIPO


# Matriz Termo Documento

mtd_treino_tf = 
  dados_lem_tfidf |> 
  cast_dtm(
    term = Palavras,
    document = ID,
    value = tf_idf)


# Conversão da MTD para um data.frame, para usarmos na modelagem
mtd_treino_tf_df <- as.data.frame(as.matrix(mtd_treino_tf))

mtd_treino_tf_df$TIPO <- variavel_resposta

#head(mtd_treino_tf_df)



# 5. Modelos de Classificação


# 5.1 - APLICAÇÃO DO RANDOM FOREST


# MODELO - TF-IDF + RANDOM FOREST


# Transformação da variável resposta em fator

class(mtd_treino_tf_df$TIPO)
mtd_treino_tf_df$TIPO <- as.factor(mtd_treino_tf_df$TIPO)

# Aplicação dO Set.Seed no modelo
set.seed(218054070)
modelo_rf <- randomForest(TIPO ~ ., data = mtd_treino_tf_df, ntree = 100)


# Previsão na base de treino
pred_rf <- predict(modelo_rf, mtd_treino_tf_df)
confusionMatrix(pred_rf, mtd_treino_tf_df$TIPO)




# 5.2 - APLICAÇÃO DO MODELO XGBOOST



# MODELO - TF-IDF + XGBOOST


# Matriz TF-IDF
mtd_treino_tf_df


# Definição dos Hiperparâmetros

xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = 3)

mat_treino = as.matrix(mtd_treino_tf_df %>% select(-TIPO)) # Todas as colunas exceto a última


# Fixando uma semente ao rodar

set.seed(218054070)
xgb_model = xgboost::xgboost(params = xgb_params, data = mat_treino, label = mtd_treino_tf_df$TIPO, nrounds = 100, verbose = 1)


# Fazendo previsões na base
pred_prob <- predict(xgb_model, mat_treino)


# Convertendo probabilidades em classes

num_classes <- 3  # Número de classes
pred_matrix <- matrix(pred_prob, ncol = num_classes, byrow = TRUE)
pred_class <- max.col(pred_matrix) - 1

# Calculando métricas de avaliação
confusion <- confusionMatrix(as.factor(pred_class), as.factor(mtd_treino_tf_df$TIPO))



# 5.3 - APLICAÇÃO DO MODELO DE REDES NEURAIS


# MODELO - TF-IDF + REDES NEURAIS

mtd_treino_tf_df
#dim(mtd_treino_tf_df)
#class(mtd_treino_tf_df)
#colnames(mtd_treino_tf_df)[1342]

mtd_treino_tf_df$TIPO = as.factor(mtd_treino_tf_df$TIPO)
#glimpse(mtd_treino_tf_df)

matriz_rn = model.matrix(~., data = mtd_treino_tf_df)
#colnames(matriz_rn)[c(1343,1342,1000:1005)]
matriz_rn = matriz_rn[,-1]

colnames(matriz_rn) <- make.names(colnames(matriz_rn), unique = TRUE)
#dim(matriz_rn)
class(matriz_rn)
tail(colnames(matriz_rn))
#epiDisplay::tab1(mtd_treino_tf_df$TIPO)


set.seed(218054070)
perceptron = neuralnet(formula = TIPO1 + TIPO2 ~ .,
                       data = matriz_rn,
                       linear.output = F,
                       hidden=c(2,2),
                       err.fct="ce",
                       lifesign = "full",
                       threshold = 6,
                       lifesign.step = 50)


# Previsão na base de treino
prev_treino_1 = perceptron$net.result[[1]]

# Inclusão da Terceira classe na previsão, como o complementar das outras probabilidades
prev_treino_1 = prev_treino_1 |> cbind(1-apply(prev_treino_1, MARGIN = 1, FUN="sum"))


# Conversão para fator, é usado na Matriz de Confusão
custo_real = mtd_treino_tf_df$TIPO
custo_real = as.factor(custo_real)
levels(custo_real) <- c("Não Ofensivo", "Ofensivo", "Discurso de ódio")

#epiDisplay::tab1(custo_estimado)
# Conversão das probabilidade estimadas em fator
custo_estimado = factor(
  apply(X = prev_treino_1, MARGIN = 1, FUN = "which.max"),
  levels = 1:3,
  labels = c("Não Ofensivo", "Ofensivo", "Discurso de ódio"))


CM_tf_idf = confusionMatrix(data = custo_estimado,reference = custo_real)


###############################################################################################


# Previsão na Base de Teste


# Carregamento da base de teste lematizada
dados_lem_Teste2 <- readRDS("dados_lem_Teste.rds")


# Utilização do IDF dos termos cálculados no Treino para o Teste
palavras_idf = dados_lem_tfidf %>% 
  select(Palavras,idf) %>% 
  unique()


# Cálculo das frequência TF-IDF
dados_lem_Teste_tfidf = dados_lem_Teste2 %>% left_join(palavras_idf)


aux = dados_lem_Teste_tfidf %>% 
  group_by(ID, Palavras) %>% 
  count() %>% 
  rename(numerador = n)


dados_lem_Teste_tfidf = dados_lem_Teste_tfidf %>% left_join(aux)


aux2 = dados_lem_Teste_tfidf %>% 
  group_by(ID) %>% 
  count() %>% 
  rename(denominador = n)


dados_lem_Teste_tfidf = dados_lem_Teste_tfidf %>% left_join(aux2)


dados_lem_Teste_tfidf = dados_lem_Teste_tfidf %>% mutate(tf = numerador/denominador)


dados_lem_Teste_tfidf = dados_lem_Teste_tfidf %>% mutate(tf_idf = tf*idf)


# Criação da MTD (TF-IDF) com a Base de Teste

mtd_teste_tf = 
  dados_lem_Teste_tfidf |> 
  cast_dtm(
    term = Palavras,
    document = ID,
    value = tf_idf)


# Transformação para matrix
mtd_teste_tf = as.matrix(mtd_teste_tf)


# Para utilizar a matriz no método predict com o modelo treinado, é essencial que ela contenha as mesmas colunas utilizadas durante o treinamento.

temp = as.data.frame(mtd_teste_tf)
temp = temp %>% select(colnames(mat_treino))
mtd_teste_tf = as.matrix(temp)

#colnames(temp)[1:10]
#colnames(mat_treino)[1:10]


#dim(mtd_teste_tf)


TIPO_POR_ID_TESTE <- dados_lem_Teste_tfidf %>%
  select(ID, TIPO) %>%
  unique() %>%
  mutate(TIPO = ifelse(TIPO == "NAO_OFENSIVO", 0, # NAO_OFENSIVO = 0
                       ifelse(TIPO == "OFENSIVO", 1, # OFENSIVO = 1
                              ifelse(TIPO == "DISCURSO_DE_ODIO", 2, NA)))) # DISCURSO_DE_ODIO = 2


temp = temp %>% mutate(TIPO = TIPO_POR_ID_TESTE$TIPO)

mat_teste = as.matrix(temp %>% select(-TIPO))



# Previsão do modelo TD-IDF + RANDOM FOREST na Base Teste

# Modelo RF
#modelo_rf

# Transformando variável resposta em fator
temp$TIPO <- as.factor(temp$TIPO)

# Previsão na base de treino
pred_rf <- predict(modelo_rf, temp)
confusionMatrix(pred_rf, temp$TIPO)




# Previsão do modelo TD-IDF + XGBOOST na Base Teste

# Modelo XGBOOST
#xgb_model

prev_teste_tf_xgboost = predict(xgb_model, newdata = mat_teste)

num_classes <- 3  # Número de classes
pred_matrix <- matrix(prev_teste_tf_xgboost, ncol = num_classes, byrow = TRUE)
pred_class <- max.col(pred_matrix) - 1

confusion <- confusionMatrix(as.factor(pred_class), as.factor(temp$TIPO))





# Previsão do modelo TF-IDF + REDES NEURAIS na Base Teste

# Modelo Rede Neural
#perceptron

prev_teste_1 = (perceptron |> neuralnet::compute(mat_teste))$net.result
prev_teste_1 = prev_teste_1 |> cbind(1-apply(prev_teste_1, MARGIN = 1, FUN="sum"))


# Variável resposta com fator e níveis

custo_real = temp$TIPO
custo_real = as.factor(custo_real)
levels(custo_real) <- c("Não Ofensivo", "Ofensivo", "Discurso de ódio")

custo_estimado = factor(
  apply(X = prev_teste_1, MARGIN = 1, FUN = "which.max"),
  levels = 1:3,
  labels = c("Não Ofensivo", "Ofensivo", "Discurso de ódio"))


# Matriz de confusão
CMteste1 = confusionMatrix(data = custo_estimado,reference = custo_real)








