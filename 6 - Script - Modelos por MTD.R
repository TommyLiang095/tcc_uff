
#############################################
############### SCRIPT 4 -TCC ############### 
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



# 4.3 - Criação da Matriz Termo Documento(BOW)


# Separação da variável resposta e transformando em dummies

TIPO_POR_ID <- dados_lem %>%
  select(ID, TIPO) %>%
  distinct() %>%
  mutate(TIPO = ifelse(TIPO == "NAO_OFENSIVO", 0,  # Atribui 0 para 'NAO_OFENSIVO'
                       ifelse(TIPO == "OFENSIVO", 1, # Atribui 1 para 'OFENSIVO'
                              ifelse(TIPO == "DISCURSO_DE_ODIO", 2, NA)))) # Atribui 2 para 'DISCURSO_DE_ODIO' e NA para outros valores

variavel_resposta <- TIPO_POR_ID$TIPO


# Matriz Termo Documento

mtd_treino_bow = 
  dados_lem |> 
  cast_dtm(
    term = Palavras,
    document = ID,
    value = frequencia)


# Conversão da MTD para um data.frame, para usarmos na modelagem
mtd_treino_bow_df <- as.data.frame(as.matrix(mtd_treino_bow))

mtd_treino_bow_df$TIPO <- variavel_resposta




# 5. Modelos de Classificação


# 5.1 - APLICAÇÃO DO RANDOM FOREST



# MODELO - MTD + RANDOM FOREST

# Matriz Termo Documento
mtd_treino_bow_df

# Transformação da variável resposta em fator
class(mtd_treino_bow_df$TIPO)
mtd_treino_bow_df$TIPO <- as.factor(mtd_treino_bow_df$TIPO)

# Aplicação da semente no modelo
set.seed(218054070)
modelo_rf_1 <- randomForest(TIPO ~ ., data = mtd_treino_bow_df, ntree = 100)


# Previsão na base de treino
pred_rf <- predict(modelo_rf_1, mtd_treino_bow_df)
confusionMatrix(pred_rf, mtd_treino_bow_df$TIPO)




# 5.2 - APLICAÇÃO DO MODELO XGBOOST


# MODELO - MTD + XGBOOST

mtd_treino_bow_df

xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = 3)

mat_treino_bow = as.matrix(mtd_treino_bow_df %>% select(-TIPO)) # Todas as colunas exceto a última


# Fixando uma semente ao rodar

set.seed(218054070)
xgb_model_1 = xgboost::xgboost(params = xgb_params, data = mat_treino_bow, label = mtd_treino_bow_df$TIPO, nrounds = 100, verbose = 1)


# Fazendo previsões na base
pred_prob <- predict(xgb_model_1, mat_treino_bow)


# Convertendo probabilidades em classes

num_classes <- 3  # Número de classes
pred_matrix <- matrix(pred_prob, ncol = num_classes, byrow = TRUE)
pred_class <- max.col(pred_matrix) - 1

# Calculando métricas de avaliação
confusion <- confusionMatrix(as.factor(pred_class), as.factor(mtd_treino_bow_df$TIPO))





# MODELO - MTD + REDES NEURAIS


mtd_treino_bow_df


f = function(x){
  m = mean(x)
  s = sd(x)
  return((x-m)/s)
}


emb = mtd_treino_bow_df |> select(-TIPO)
emb = apply(emb,2,FUN = f)
class(emb)

TIPO = as.factor(mtd_treino_bow_df$TIPO)
class(TIPO)
base = cbind(as.data.frame(emb),TIPO)
class(base$TIPO)


# Transformação da variável resposta em fator
#mtd_treino_bow_df$TIPO = as.factor(mtd_treino_bow_df$TIPO)


# Transformação da MTD em matriz númerica
#matriz_rn_bow = model.matrix(~., data = mtd_treino_bow_df)

# Retirando a interseção
#matriz_rn_bow = matriz_rn_bow[,-1]


colnames(matriz_rn_bow) <- make.names(colnames(matriz_rn_bow), unique = TRUE)


set.seed(218054070)
perceptron_rn_bow = neuralnet(formula = TIPO ~ .,
                              data = base,
                              linear.output = F,
                              hidden=c(2,2),
                              err.fct="ce",
                              lifesign = "full",
                              threshold = 6,
                              lifesign.step = 50)


# Previsão na base de treino
prev_treino_2 = perceptron_rn_bow$net.result[[1]]


# Inclusão da Terceira classe na previsão, como o complementar das outras probabilidades
#prev_treino_2 = prev_treino_2 |> cbind(1-apply(prev_treino_2, MARGIN = 1, FUN="sum"))


# Conversão para fator, é usado na Matriz de Confusão
custo_real = mtd_treino_bow_df$TIPO
custo_real = as.factor(custo_real)
levels(custo_real) <- c("Não Ofensivo", "Ofensivo", "Discurso de ódio")


# Conversão das probabilidade estimadas em fator
custo_estimado = factor(
  apply(X = prev_treino_2, MARGIN = 1, FUN = "which.max"),
  levels = 1:3,
  labels = c("Não Ofensivo", "Ofensivo", "Discurso de ódio"))


CM_bow = confusionMatrix(data = custo_estimado,reference = custo_real)





###############################################################################################


# Previsão na Base de Teste


# Carregamento da base de teste lematizada
dados_lem_Teste2 <- readRDS("dados_lem_Teste.rds")



# Criação da MTD com a Base de Teste

mtd_teste_bow = 
  dados_lem_Teste2 |> 
  cast_dtm(
    term = Palavras,
    document = ID,
    value = frequencia)


# Inclusão da variável resposta ao MTD

TIPO_POR_ID_TESTE_1 <- dados_lem_Teste2 %>%
  select(ID, TIPO) %>%
  unique() %>%
  mutate(TIPO = ifelse(TIPO == "NAO_OFENSIVO", 0, # NAO_OFENSIVO = 0
                       ifelse(TIPO == "OFENSIVO", 1, # OFENSIVO = 1
                              ifelse(TIPO == "DISCURSO_DE_ODIO", 2, NA)))) # DISCURSO_DE_ODIO = 2

mtd_teste_bow$TIPO <- TIPO_POR_ID_TESTE_1$TIPO

mtd_teste_bow = as.matrix(mtd_teste_bow)




# Para utilizar a matriz no método predict com o modelo treinado, é essencial que ela contenha as mesmas colunas utilizadas durante o treinamento.


mtd_teste_bow = as.data.frame(mtd_teste_bow)

#head(colnames(matrix_teste_mtd))
#head(colnames(mat_treino_bow))

mtd_teste_bow = mtd_teste_bow %>% select(colnames(mat_treino_bow))


mtd_teste_bow = mtd_teste_bow %>% mutate(TIPO = TIPO_POR_ID_TESTE_1$TIPO)
matrix_mtd_teste_bow = as.matrix(mtd_teste_bow %>% select(-TIPO))



# Previsão do modelo MTD + RANDOM FOREST na Base Teste

#Modelo
#modelo_rf_1

# Transformando variável resposta em fator
mtd_teste_bow$TIPO <- as.factor(mtd_teste_bow$TIPO)


# Previsão na base de treino
pred_rf <- predict(modelo_rf_1, mtd_teste_bow)
confusionMatrix(pred_rf, mtd_teste_bow$TIPO)




# Previsão do modelo MTD + XGBOOST na Base Teste

#Modelo
#xgb_model_1


# Previsão na base de teste
prev_teste_xgboost = predict(xgb_model_1, newdata = matrix_mtd_teste_bow)

num_classes <- 3  # Número de classes
pred_matrix <- matrix(prev_teste_xgboost, ncol = num_classes, byrow = TRUE)
pred_class <- max.col(pred_matrix) - 1


confusion <- confusionMatrix(as.factor(pred_class), as.factor(mtd_teste_bow$TIPO))




# Previsão do modelo MTD + REDES NEURAIS na Base Teste

# Modelo
perceptron_rn_bow


#Normalização da base de teste

f = function(x){
  m = mean(x)
  s = sd(x)
  return((x-m)/s)
}


matrix_mtd_teste_bow = apply(matrix_mtd_teste_bow,2,FUN = f)


# Previsão na base de Teste

prev_teste_2 = (perceptron_rn_bow |> neuralnet::compute(matrix_mtd_teste_bow))$net.result
#prev_teste_2 = prev_teste_2 |> cbind(1-apply(prev_teste_2, MARGIN = 1, FUN="sum"))


# Variável resposta com fator e níveis

custo_real = mtd_teste_bow$TIPO
custo_real = as.factor(custo_real)
levels(custo_real) <- c("Não Ofensivo", "Ofensivo", "Discurso de ódio")

custo_estimado = factor(
  apply(X = prev_teste_2, MARGIN = 1, FUN = "which.max"),
  levels = 1:3,
  labels = c("Não Ofensivo", "Ofensivo", "Discurso de ódio"))


# Matriz de confusão
CMteste1 = confusionMatrix(data = custo_estimado,reference = custo_real)













